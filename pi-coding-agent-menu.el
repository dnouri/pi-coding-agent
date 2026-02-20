;;; pi-coding-agent-menu.el --- Transient menu and session management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pi-coding-agent

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Transient menu, session management, model selection, and command
;; infrastructure for pi-coding-agent.
;;
;; Key entry points:
;;   `pi-coding-agent-menu'            Transient menu (C-c C-p)
;;   `pi-coding-agent-new-session'     Start fresh session
;;   `pi-coding-agent-resume-session'  Resume previous session
;;   `pi-coding-agent-reload'          Restart pi process
;;   `pi-coding-agent-select-model'    Choose model interactively
;;   `pi-coding-agent-cycle-thinking'  Cycle thinking levels
;;   `pi-coding-agent-compact'         Compact conversation context
;;   `pi-coding-agent-fork'            Fork from previous message

;;; Code:

(require 'pi-coding-agent-render)
(require 'transient)

(when (and (not (bound-and-true-p byte-compile-current-file))
           (or (not (boundp 'transient-version))
               (version< transient-version "0.9.0")))
  (display-warning 'pi-coding-agent
                   (format "pi-coding-agent requires transient >= 0.9.0, \
but %s is loaded.
  Fix: M-x package-install RET transient RET, then restart Emacs."
                           (if (boundp 'transient-version)
                               transient-version
                             "unknown"))
                   :error))

;;;; Slash Commands via RPC

(defun pi-coding-agent--fetch-commands (proc callback)
  "Fetch available commands via RPC, call CALLBACK with result.
PROC is the pi process.  CALLBACK receives the command list on success."
  (pi-coding-agent--rpc-async proc '(:type "get_commands")
    (lambda (response)
      (when (eq (plist-get response :success) t)
        (let* ((data (plist-get response :data))
               (commands-vec (plist-get data :commands))
               ;; Convert vector to list
               (commands (append commands-vec nil)))
          (funcall callback commands))))))

;;;; Session Management

(defun pi-coding-agent--menu-model-description ()
  "Return model description for transient menu."
  (let ((model (plist-get (plist-get pi-coding-agent--state :model) :name)))
    (format "Model: %s" (or model "unknown"))))

(defun pi-coding-agent--menu-thinking-description ()
  "Return thinking level description for transient menu."
  (let ((level (plist-get pi-coding-agent--state :thinking-level)))
    (format "Thinking: %s" (or level "off"))))

;;;###autoload
(defun pi-coding-agent-new-session ()
  "Start a new pi session (reset)."
  (interactive)
  (when-let ((proc (pi-coding-agent--get-process))
             (chat-buf (pi-coding-agent--get-chat-buffer)))
    (pi-coding-agent--rpc-async proc '(:type "new_session")
                   (lambda (response)
                     (let* ((data (plist-get response :data))
                            (cancelled (plist-get data :cancelled)))
                       (if (and (plist-get response :success)
                                (pi-coding-agent--json-false-p cancelled))
                           (when (buffer-live-p chat-buf)
                             (with-current-buffer chat-buf
                               (pi-coding-agent--clear-chat-buffer)
                               (pi-coding-agent--refresh-header))
                             ;; Refresh state to get new session-file
                             (pi-coding-agent--rpc-async proc '(:type "get_state")
                               (lambda (resp)
                                 (pi-coding-agent--apply-state-response chat-buf resp)))
                             (message "Pi: New session started"))
                         (message "Pi: New session cancelled")))))))

(defun pi-coding-agent--session-dir-name (dir)
  "Convert DIR to session directory name.
Matches pi's encoding: --path-with-dashes--.
Note: Handles both Unix and Windows path separators."
  (let* ((clean-dir (directory-file-name dir))  ; Remove trailing slash
         (safe-path (replace-regexp-in-string "[/\\\\:]" "-"
                                              (replace-regexp-in-string "^[/\\\\]" "" clean-dir))))
    (concat "--" safe-path "--")))

(defun pi-coding-agent--session-metadata (path)
  "Extract metadata from session file PATH.
Returns plist with :modified-time, :first-message, :message-count, and
:session-name, or nil on error.  Session name comes from the most recent
session_info entry if present."
  (condition-case nil
      (let* ((attrs (file-attributes path))
             (modified-time (file-attribute-modification-time attrs)))
        (with-temp-buffer
          (insert-file-contents path)
          (let ((first-message nil)
                (message-count 0)
                (session-name nil)
                (has-session-header nil))
            (goto-char (point-min))
            ;; Scan lines to find session header, first message, count messages, and session name
            (while (not (eobp))
              (let* ((line (buffer-substring-no-properties
                            (point) (line-end-position))))
                (when (and line (not (string-empty-p line)))
                  (let* ((data (json-parse-string line :object-type 'plist))
                         (type (plist-get data :type)))
                    (when (equal type "session")
                      (setq has-session-header t))
                    (when (equal type "message")
                      (setq message-count (1+ message-count))
                      ;; Extract text from first message only
                      (unless first-message
                        (let* ((message (plist-get data :message))
                               (content (plist-get message :content)))
                          (when (and content (vectorp content) (> (length content) 0))
                            (setq first-message (plist-get (aref content 0) :text))))))
                    ;; Extract session name (use latest one)
                    (when (equal type "session_info")
                      (setq session-name
                            (pi-coding-agent--normalize-string-or-null
                             (plist-get data :name)))))))
              (forward-line 1))
            ;; Only return metadata if we found a valid session header
            (when has-session-header
              (list :modified-time modified-time
                    :first-message first-message
                    :message-count message-count
                    :session-name session-name)))))
    (error nil)))

(defun pi-coding-agent--update-session-name-from-file (session-file)
  "Update `pi-coding-agent--session-name' from SESSION-FILE metadata.
Call this from the chat buffer after switching or loading a session."
  (when session-file
    (let ((metadata (pi-coding-agent--session-metadata session-file)))
      (setq pi-coding-agent--session-name (plist-get metadata :session-name)))))

(defun pi-coding-agent--list-sessions (dir)
  "List available session files for project DIR.
Returns list of absolute paths to .jsonl files, sorted by modification
time with most recently used first."
  (let* ((sessions-base (expand-file-name "~/.pi/agent/sessions/"))
         (session-dir (expand-file-name (pi-coding-agent--session-dir-name dir) sessions-base)))
    (when (file-directory-p session-dir)
      ;; Sort by modification time descending (most recently used first)
      (sort (directory-files session-dir t "\\.jsonl$")
            (lambda (a b)
              (time-less-p (file-attribute-modification-time (file-attributes b))
                           (file-attribute-modification-time (file-attributes a))))))))

(defun pi-coding-agent--format-session-choice (path)
  "Format session PATH for display in selector.
Returns (display-string . path) for `completing-read'.
Prefers session name over first message when available."
  (let ((metadata (pi-coding-agent--session-metadata path)))
    (if metadata
        (let* ((modified-time (plist-get metadata :modified-time))
               (session-name (plist-get metadata :session-name))
               (first-msg (plist-get metadata :first-message))
               (msg-count (plist-get metadata :message-count))
               (relative-time (pi-coding-agent--format-relative-time modified-time))
               ;; Prefer session name, fall back to first message preview
               (label (cond
                       (session-name (pi-coding-agent--truncate-string session-name 50))
                       (first-msg (pi-coding-agent--truncate-string first-msg 50))
                       (t nil)))
               (display (if label
                            (format "%s · %s (%d msgs)"
                                    label relative-time msg-count)
                          (format "[empty session] · %s" relative-time))))
          (cons display path))
      ;; Fallback to filename if metadata extraction fails
      (let ((filename (file-name-nondirectory path)))
        (cons filename path)))))

(defun pi-coding-agent--reset-session-state ()
  "Reset all session-specific state for a new session.
Call this when starting a new session to ensure no stale state persists."
  (dolist (marker (list pi-coding-agent--message-start-marker
                        pi-coding-agent--streaming-marker
                        pi-coding-agent--thinking-marker
                        pi-coding-agent--thinking-start-marker))
    (when (markerp marker)
      (set-marker marker nil)))
  (setq pi-coding-agent--session-name nil
        pi-coding-agent--cached-stats nil
        pi-coding-agent--assistant-header-shown nil
        pi-coding-agent--local-user-message nil
        pi-coding-agent--extension-status nil
        pi-coding-agent--working-message nil
        pi-coding-agent--in-code-block nil
        pi-coding-agent--in-thinking-block nil
        pi-coding-agent--thinking-marker nil
        pi-coding-agent--thinking-start-marker nil
        pi-coding-agent--thinking-raw nil
        pi-coding-agent--line-parse-state 'line-start
        pi-coding-agent--pending-tool-overlay nil
        pi-coding-agent--activity-phase "idle")
  ;; Use accessors for cross-module state
  (pi-coding-agent--set-last-usage nil)
  (pi-coding-agent--clear-followup-queue)
  (pi-coding-agent--set-aborted nil)
  (pi-coding-agent--set-message-start-marker nil)
  (pi-coding-agent--set-streaming-marker nil)
  (when pi-coding-agent--tool-args-cache
    (clrhash pi-coding-agent--tool-args-cache)))

(defun pi-coding-agent--clear-chat-buffer ()
  "Clear the chat buffer and display fresh startup header.
Used when starting a new session."
  (when-let ((chat-buf (pi-coding-agent--get-chat-buffer)))
    (with-current-buffer chat-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (pi-coding-agent--format-startup-header))
        (insert "\n")
        (pi-coding-agent--reset-session-state)
        (goto-char (point-max))))))

(defun pi-coding-agent--load-session-history (proc callback &optional chat-buf)
  "Load and display session history from PROC.
Calls CALLBACK with message count when done.
CHAT-BUF is the target buffer; if nil, uses `pi-coding-agent--get-chat-buffer'.
Note: When called from async callbacks, pass CHAT-BUF explicitly."
  (let ((chat-buf (or chat-buf (pi-coding-agent--get-chat-buffer))))
    (pi-coding-agent--rpc-async proc '(:type "get_messages")
                   (lambda (response)
                     (when (plist-get response :success)
                       (let* ((messages (plist-get (plist-get response :data) :messages))
                              (count (if (vectorp messages) (length messages) 0)))
                         (pi-coding-agent--display-session-history messages chat-buf)
                         ;; Restore context usage from last assistant message
                         ;; (ensures context % displays correctly after resume/fork)
                         (when (buffer-live-p chat-buf)
                           (with-current-buffer chat-buf
                             (pi-coding-agent--set-last-usage
                              (pi-coding-agent--extract-last-usage messages))
                             (pi-coding-agent--refresh-header)))
                         (when callback
                           (funcall callback count))))))))

;;;###autoload
(defun pi-coding-agent-reload ()
  "Reload the current session by restarting the pi process.
Useful for reloading extensions, skills, prompts, and themes after
editing them, or when the pi process has died or become unresponsive.
Kills any existing process and starts fresh, then resumes the session
using the cached session file."
  (interactive)
  (let* ((chat-buf (pi-coding-agent--get-chat-buffer))
         (session-file (and chat-buf
                            (buffer-local-value 'pi-coding-agent--state chat-buf)
                            (plist-get (buffer-local-value 'pi-coding-agent--state chat-buf)
                                       :session-file))))
    (cond
     ;; No chat buffer
     ((not chat-buf)
      (message "Pi: No session to reload"))
     ;; No session file cached
     ((not session-file)
      (message "Pi: No session file available - cannot reload"))
     ;; Recover
     (t
      (with-current-buffer chat-buf
        ;; Kill old process if it exists (alive or dead)
        (when pi-coding-agent--process
          (pi-coding-agent--unregister-display-handler pi-coding-agent--process)
          (when (process-live-p pi-coding-agent--process)
            (delete-process pi-coding-agent--process)))
        ;; Reset status to idle (in case we were stuck in streaming)
        (setq pi-coding-agent--status 'idle)
        ;; Start new process
        (let* ((dir (pi-coding-agent--session-directory))
               (new-proc (pi-coding-agent--start-process dir)))
          (pi-coding-agent--set-process new-proc)
          (when (processp new-proc)
            (set-process-buffer new-proc chat-buf)
            (process-put new-proc 'pi-coding-agent-chat-buffer chat-buf)
            (pi-coding-agent--register-display-handler new-proc)
            ;; Switch to the saved session
            (pi-coding-agent--rpc-async new-proc
                           (list :type "switch_session" :sessionPath session-file)
                           (lambda (response)
                             (if (plist-get response :success)
                                 (progn
                                   ;; Update session name cache
                                   (when (buffer-live-p chat-buf)
                                     (with-current-buffer chat-buf
                                       (pi-coding-agent--update-session-name-from-file session-file)))
                                   ;; Reload state
                                   (pi-coding-agent--rpc-async new-proc '(:type "get_state")
                                     (lambda (resp)
                                       (pi-coding-agent--apply-state-response chat-buf resp)))
                                   ;; Reload commands (extensions, templates, skills may have changed)
                                   (pi-coding-agent--fetch-commands new-proc
                                     (lambda (commands)
                                       (when (buffer-live-p chat-buf)
                                         (with-current-buffer chat-buf
                                           (pi-coding-agent--set-commands commands)
                                           (pi-coding-agent--rebuild-commands-menu)))))
                                   (message "Pi: Session reloaded"))
                               (message "Pi: Failed to reload - %s"
                                        (or (plist-get response :error) "unknown error"))))))))))))

(defun pi-coding-agent-resume-session ()
  "Resume a previous pi session from the current project."
  (interactive)
  (when-let ((proc (pi-coding-agent--get-process))
             (dir (pi-coding-agent--session-directory)))
    (let ((sessions (pi-coding-agent--list-sessions dir)))
      (if (null sessions)
          (message "Pi: No previous sessions found")
        (let* ((choices (mapcar #'pi-coding-agent--format-session-choice sessions))
               (choice-strings (mapcar #'car choices))
               ;; Use completion table with metadata to preserve our sort order
               ;; (completing-read normally re-sorts alphabetically)
               (choice (completing-read "Resume session: "
                                        (lambda (string pred action)
                                          (if (eq action 'metadata)
                                              '(metadata (display-sort-function . identity))
                                            (complete-with-action action choice-strings string pred)))
                                        nil t))
               (selected-path (cdr (assoc choice choices)))
               ;; Capture chat buffer before async call
               (chat-buf (pi-coding-agent--get-chat-buffer)))
          (when selected-path
            (pi-coding-agent--rpc-async proc (list :type "switch_session"
                                      :sessionPath selected-path)
                           (lambda (response)
                             (let* ((data (plist-get response :data))
                                    (cancelled (plist-get data :cancelled)))
                               (if (and (plist-get response :success)
                                        (pi-coding-agent--json-false-p cancelled))
                                   (progn
                                     ;; Update session name cache
                                     (when (buffer-live-p chat-buf)
                                       (with-current-buffer chat-buf
                                         (pi-coding-agent--update-session-name-from-file selected-path)))
                                     ;; Refresh state to get new session-file
                                     (pi-coding-agent--rpc-async proc '(:type "get_state")
                                       (lambda (resp)
                                         (pi-coding-agent--apply-state-response chat-buf resp)))
                                     (pi-coding-agent--load-session-history
                                      proc
                                      (lambda (count)
                                        (message "Pi: Resumed session (%d messages)" count))
                                      chat-buf))
                                 (message "Pi: Failed to resume session")))))))))))

;;;; Model and Thinking

(defun pi-coding-agent-set-session-name (name)
  "Set the session NAME for the current session.
The name is displayed in the resume picker and header-line."
  (interactive
   (let ((chat-buf (pi-coding-agent--get-chat-buffer)))
     (list (read-string "Session name: "
                        (or (and chat-buf
                                 (buffer-local-value 'pi-coding-agent--session-name chat-buf))
                            "")))))
  (let* ((trimmed-name (string-trim name))
         (chat-buf (pi-coding-agent--get-chat-buffer)))
    (if (string-empty-p trimmed-name)
        ;; Consistent with TUI /name behavior
        (let ((current-name (and chat-buf
                                 (buffer-local-value 'pi-coding-agent--session-name chat-buf))))
          (if current-name
              (message "Pi: Session name: %s" current-name)
            (message "Pi: No session name set")))
      (let ((proc (pi-coding-agent--get-process)))
        (unless proc
          (user-error "No pi process running"))
        (pi-coding-agent--rpc-async proc
            (list :type "set_session_name" :name trimmed-name)
            (lambda (response)
              (if (plist-get response :success)
                  (progn
                    (when (buffer-live-p chat-buf)
                      (with-current-buffer chat-buf
                        (setq pi-coding-agent--session-name trimmed-name)
                        (force-mode-line-update t)))
                    (message "Pi: Session name set to \"%s\"" trimmed-name))
                (message "Pi: Failed to set session name: %s"
                         (or (plist-get response :error) "unknown error")))))))))

(defun pi-coding-agent-select-model ()
  "Select a model interactively."
  (interactive)
  (let ((proc (pi-coding-agent--get-process))
        (chat-buf (pi-coding-agent--get-chat-buffer)))
    (unless proc
      (user-error "No pi process running"))
    (let* ((response (pi-coding-agent--rpc-sync proc '(:type "get_available_models") 5))
           (data (plist-get response :data))
           (models (plist-get data :models))
           (current-name (plist-get (plist-get pi-coding-agent--state :model) :name))
           ;; Build alist of (display-name . model-plist) for selection
           (model-alist (mapcar (lambda (m)
                                  (cons (plist-get m :name) m))
                                models))
           (names (mapcar #'car model-alist))
           (choice (completing-read
                    (format "Model (current: %s): " (or current-name "unknown"))
                    names nil t)))
      (when (and choice (not (equal choice current-name)))
        (let* ((selected-model (cdr (assoc choice model-alist)))
               (model-id (plist-get selected-model :id))
               (provider (plist-get selected-model :provider)))
          (pi-coding-agent--rpc-async proc (list :type "set_model"
                                    :provider provider
                                    :modelId model-id)
                         (lambda (resp)
                           (when (and (plist-get resp :success)
                                      (buffer-live-p chat-buf))
                             (with-current-buffer chat-buf
                               (pi-coding-agent--update-state-from-response resp)
                               (force-mode-line-update))
                             (message "Pi: Model set to %s" choice)))))))))

(defun pi-coding-agent-cycle-thinking ()
  "Cycle through thinking levels."
  (interactive)
  (when-let ((proc (pi-coding-agent--get-process))
             (chat-buf (pi-coding-agent--get-chat-buffer)))
    (pi-coding-agent--rpc-async proc '(:type "cycle_thinking_level")
                   (lambda (response)
                     (when (and (plist-get response :success)
                                (buffer-live-p chat-buf))
                       (with-current-buffer chat-buf
                         (pi-coding-agent--update-state-from-response response)
                         (force-mode-line-update)
                         (message "Pi: Thinking level: %s"
                                  (plist-get pi-coding-agent--state :thinking-level))))))))

;;;; Session Info and Actions

(defun pi-coding-agent--format-session-stats (stats)
  "Format STATS plist as human-readable string."
  (let* ((tokens (plist-get stats :tokens))
         (input (or (plist-get tokens :input) 0))
         (output (or (plist-get tokens :output) 0))
         (total (or (plist-get tokens :total) 0))
         (cache-read (or (plist-get tokens :cacheRead) 0))
         (cache-write (or (plist-get tokens :cacheWrite) 0))
         (cost (or (plist-get stats :cost) 0))
         (messages (or (plist-get stats :userMessages) 0))
         (tools (or (plist-get stats :toolCalls) 0)))
    (format "Tokens: %s in / %s out (%s total) | Cache: R%s / W%s | Cost: $%.2f | Messages: %d | Tools: %d"
            (pi-coding-agent--format-number input)
            (pi-coding-agent--format-number output)
            (pi-coding-agent--format-number total)
            (pi-coding-agent--format-number cache-read)
            (pi-coding-agent--format-number cache-write)
            cost messages tools)))

(defun pi-coding-agent-session-stats ()
  "Display session statistics in the echo area."
  (interactive)
  (when-let ((proc (pi-coding-agent--get-process)))
    (pi-coding-agent--rpc-async proc '(:type "get_session_stats")
                   (lambda (response)
                     (if (plist-get response :success)
                         (let ((data (plist-get response :data)))
                           (message "Pi: %s" (pi-coding-agent--format-session-stats data)))
                       (message "Pi: Failed to get session stats"))))))

(defun pi-coding-agent-process-info ()
  "Display process information for debugging.
Shows PID, status, and session file."
  (interactive)
  (let* ((chat-buf (pi-coding-agent--get-chat-buffer))
         (proc (and chat-buf (buffer-local-value 'pi-coding-agent--process chat-buf)))
         (state (and chat-buf (buffer-local-value 'pi-coding-agent--state chat-buf)))
         (status (and chat-buf (buffer-local-value 'pi-coding-agent--status chat-buf)))
         (session-file (and state (plist-get state :session-file))))
    (cond
     ((not chat-buf)
      (message "Pi: No session"))
     ((not proc)
      (message "Pi: No process (status: %s, session: %s)"
               status
               (or session-file "none")))
     (t
      (message "Pi: PID %s, %s (status: %s, session: %s)"
               (process-id proc)
               (if (process-live-p proc) "alive" "dead")
               status
               (or (and session-file (file-name-nondirectory session-file)) "none"))))))

(defun pi-coding-agent--handle-manual-compaction-response (chat-buf response)
  "Handle manual compaction RESPONSE for CHAT-BUF.
Restores idle state, renders success details, and drains queued follow-ups."
  (when (buffer-live-p chat-buf)
    (with-current-buffer chat-buf
      (setq pi-coding-agent--status 'idle)
      (pi-coding-agent--set-activity-phase "idle")
      (if (plist-get response :success)
          (let ((data (plist-get response :data)))
            (pi-coding-agent--handle-compaction-success
             (plist-get data :tokensBefore)
             (plist-get data :summary)
             (current-time)))
        (message "Pi: Compact failed%s"
                 (if-let ((error-text (plist-get response :error)))
                     (format ": %s" error-text)
                   "")))
      (pi-coding-agent--process-followup-queue))))

(defun pi-coding-agent-compact (&optional custom-instructions)
  "Compact conversation context to reduce token usage.
Optional CUSTOM-INSTRUCTIONS provide guidance for the compaction summary."
  (interactive)
  (when-let ((chat-buf (pi-coding-agent--get-chat-buffer)))
    (let ((proc (pi-coding-agent--get-process)))
      (cond
       ((null proc)
        (message "Pi: No process available - try M-x pi-coding-agent-reload or C-c C-p R"))
       ((not (process-live-p proc))
        (message "Pi: Process died - try M-x pi-coding-agent-reload or C-c C-p R"))
       (t
        (message "Pi: Compacting...")
        (with-current-buffer chat-buf
          (setq pi-coding-agent--status 'compacting)
          (pi-coding-agent--set-activity-phase "compact"))
        (pi-coding-agent--rpc-async
         proc
         (if custom-instructions
             (list :type "compact" :customInstructions custom-instructions)
           '(:type "compact"))
         (lambda (response)
           (pi-coding-agent--handle-manual-compaction-response chat-buf response))))))))

(defun pi-coding-agent-export-html ()
  "Export session to HTML file."
  (interactive)
  (when-let ((proc (pi-coding-agent--get-process)))
    (pi-coding-agent--rpc-async proc '(:type "export_html")
                   (lambda (response)
                     (if (plist-get response :success)
                         (let* ((data (plist-get response :data))
                                (path (plist-get data :path)))
                           (message "Pi: Exported to %s" path))
                       (message "Pi: Export failed"))))))

(defun pi-coding-agent-copy-last-message ()
  "Copy last assistant message to kill ring."
  (interactive)
  (when-let ((proc (pi-coding-agent--get-process)))
    (pi-coding-agent--rpc-async proc '(:type "get_last_assistant_text")
                   (lambda (response)
                     (if (plist-get response :success)
                         (let* ((data (plist-get response :data))
                                (text (plist-get data :text)))
                           (if text
                               (progn
                                 (kill-new text)
                                 (message "Pi: Copied to kill ring"))
                             (message "Pi: No assistant message to copy")))
                       (message "Pi: Failed to get message"))))))

;;;; Fork

(defun pi-coding-agent--flatten-tree (nodes)
  "Flatten tree NODES into a hash table mapping id to node plist.
NODES is a vector of tree node plists, each with `:children' vector.
Returns a hash table for O(1) lookup by id."
  (let ((index (make-hash-table :test 'equal)))
    (cl-labels ((walk (ns)
                  (seq-doseq (node ns)
                    (puthash (plist-get node :id) node index)
                    (let ((children (plist-get node :children)))
                      (when (and children (> (length children) 0))
                        (walk children))))))
      (walk nodes))
    index))

(defun pi-coding-agent--active-branch-user-ids (index leaf-id)
  "Return chronological list of user message IDs on the active branch.
INDEX is a hash table from `pi-coding-agent--flatten-tree'.
LEAF-ID is the current leaf node ID.  Walk from leaf to root via
`:parentId', collecting IDs of nodes with type \"message\" and role
\"user\".  Returns list in root-to-leaf (chronological) order."
  (when leaf-id
    (let ((user-ids nil)
          (current-id leaf-id))
      (while current-id
        (let ((node (gethash current-id index)))
          (when (and node
                     (equal (plist-get node :type) "message")
                     (equal (plist-get node :role) "user"))
            (push (plist-get node :id) user-ids))
          (setq current-id (and node (plist-get node :parentId)))))
      user-ids)))

(defun pi-coding-agent--format-fork-message (msg &optional index)
  "Format MSG for display in fork selector.
MSG is a plist with :entryId and :text.
INDEX is the display index (1-based) for the message."
  (let* ((text (or (plist-get msg :text) ""))
         (preview (truncate-string-to-width text 60 nil nil "...")))
    (if index
        (format "%d: %s" index preview)
      preview)))

(defun pi-coding-agent-fork ()
  "Fork conversation from a previous user message.
Shows a selector of user messages and creates a fork from the selected one."
  (interactive)
  (when-let ((proc (pi-coding-agent--get-process)))
    (pi-coding-agent--rpc-async proc '(:type "get_fork_messages")
                   (lambda (response)
                     (if (plist-get response :success)
                         (let* ((data (plist-get response :data))
                                (messages (plist-get data :messages)))
                           ;; Note: messages is a vector from JSON, use seq-empty-p not null
                           (if (seq-empty-p messages)
                               (message "Pi: No messages to fork from")
                             (pi-coding-agent--show-fork-selector proc messages)))
                       (message "Pi: Failed to get fork messages"))))))

(defun pi-coding-agent--resolve-fork-entry (response ordinal heading-count)
  "Resolve a fork entry ID from get_tree RESPONSE.
ORDINAL is the 0-based user turn index.  HEADING-COUNT is the number
of visible You headings in the buffer.  Returns (ENTRY-ID . PREVIEW)
or nil if the ordinal could not be mapped."
  (when (plist-get response :success)
    (let* ((data (plist-get response :data))
           (tree (plist-get data :tree))
           (leaf-id (plist-get data :leafId))
           (index (pi-coding-agent--flatten-tree tree))
           (all-user-ids (pi-coding-agent--active-branch-user-ids index leaf-id))
           ;; Take last N to handle compaction (compacted-away
           ;; user messages at start of path aren't rendered)
           (visible-ids (last all-user-ids heading-count))
           (entry-id (nth ordinal visible-ids))
           (node (and entry-id (gethash entry-id index))))
      (when entry-id
        (cons entry-id (plist-get node :preview))))))

(defun pi-coding-agent-fork-at-point ()
  "Fork conversation from the user turn at point.
Determines which user message point is in (or after), confirms with
a preview, then forks.  Only works when the session is idle."
  (interactive)
  (let ((chat-buf (pi-coding-agent--get-chat-buffer)))
    (unless chat-buf
      (user-error "Pi: No chat buffer"))
    (with-current-buffer chat-buf
      (let* ((headings (pi-coding-agent--collect-you-headings))
             (ordinal (pi-coding-agent--user-turn-index-at-point headings)))
        (cond
         ((not (eq pi-coding-agent--status 'idle))
          (message "Pi: Cannot fork while streaming"))
         ((not ordinal)
          (message "Pi: No user message at point"))
         (t
          (let ((heading-count (length headings))
                (proc (pi-coding-agent--get-process)))
            (unless proc
              (user-error "Pi: No active process"))
            (pi-coding-agent--rpc-async proc '(:type "get_tree")
              (lambda (response)
                (let ((result (pi-coding-agent--resolve-fork-entry
                               response ordinal heading-count)))
                  (cond
                   ((not result)
                    (message "Pi: Could not map turn to entry ID"))
                   ((with-current-buffer chat-buf
                      (y-or-n-p (format "Fork from: %s? " (or (cdr result) "?"))))
                    (with-current-buffer chat-buf
                      (pi-coding-agent--execute-fork proc (car result)))))))))))))))

(defun pi-coding-agent--execute-fork (proc entry-id)
  "Execute fork to ENTRY-ID via PROC.
Sends the fork RPC, then on success: refreshes state, reloads history,
and pre-fills the input buffer with the forked message text.
Captures chat and input buffers at call time (before the async RPC)."
  (let ((chat-buf (pi-coding-agent--get-chat-buffer))
        (input-buf (pi-coding-agent--get-input-buffer)))
    (pi-coding-agent--rpc-async proc (list :type "fork" :entryId entry-id)
      (lambda (response)
        (if (plist-get response :success)
            (let* ((data (plist-get response :data))
                   (text (plist-get data :text)))
              ;; Refresh state to get new session-file
              (pi-coding-agent--rpc-async proc '(:type "get_state")
                (lambda (resp)
                  (pi-coding-agent--apply-state-response chat-buf resp)))
              ;; Reload and display the forked session
              (pi-coding-agent--load-session-history
               proc
               (lambda (count)
                 (message "Pi: Branched to new session (%d messages)" count))
               chat-buf)
              ;; Pre-fill input with the forked message text
              (when (buffer-live-p input-buf)
                (with-current-buffer input-buf
                  (erase-buffer)
                  (when text (insert text)))))
          (message "Pi: Branch failed"))))))

(defun pi-coding-agent--show-fork-selector (proc messages)
  "Show selector for MESSAGES and fork on selection.
PROC is the pi process.
MESSAGES is a vector of plists from get_fork_messages."
  (let* ((index 0)
         ;; Reverse so most recent messages appear first (upstream sends chronological order)
         (reversed-messages (reverse (append messages nil)))
         (formatted (mapcar (lambda (msg)
                              (setq index (1+ index))
                              (cons (pi-coding-agent--format-fork-message msg index) msg))
                            reversed-messages))
         (choice-strings (mapcar #'car formatted))
         ;; Use completion table with metadata to preserve our sort order
         ;; (completing-read normally re-sorts alphabetically)
         (choice (completing-read "Branch from: "
                                  (lambda (string pred action)
                                    (if (eq action 'metadata)
                                        '(metadata (display-sort-function . identity))
                                      (complete-with-action action choice-strings string pred)))
                                  nil t))
         (selected (cdr (assoc choice formatted))))
    (when selected
      (pi-coding-agent--execute-fork proc (plist-get selected :entryId)))))

;;;; Custom Commands

(defun pi-coding-agent--run-custom-command (cmd)
  "Execute custom command CMD.
Always prompts for arguments - user can press Enter if none needed.
Sends the literal /command text to pi, which handles expansion."
  (when-let ((chat-buf (pi-coding-agent--get-chat-buffer)))
    (let* ((name (plist-get cmd :name))
           (args-string (read-string (format "/%s: " name)))
           (full-command (if (string-empty-p args-string)
                             (format "/%s" name)
                           (format "/%s %s" name args-string))))
      (with-current-buffer chat-buf
        (pi-coding-agent--prepare-and-send full-command)))))

(defun pi-coding-agent-run-custom-command ()
  "Select and run a custom command.
Uses commands from pi's `get_commands' RPC."
  (interactive)
  (if (null pi-coding-agent--commands)
      (message "Pi: No commands available")
    (let* ((choices (mapcar (lambda (cmd)
                              (cons (format "%s - %s"
                                            (plist-get cmd :name)
                                            (or (plist-get cmd :description) ""))
                                    cmd))
                            pi-coding-agent--commands))
           (choice (completing-read "Command: " choices nil t))
           (cmd (cdr (assoc choice choices))))
      (when cmd
        (pi-coding-agent--run-custom-command cmd)))))

;;;; Transient Menu

(transient-define-prefix pi-coding-agent-menu ()
  "Pi coding agent menu."
  [:description
   (lambda () (concat (pi-coding-agent--menu-model-description) " • "
                      (pi-coding-agent--menu-thinking-description)))
   :class transient-row]
  [["Session"
    ("n" "new" pi-coding-agent-new-session)
    ("r" "resume" pi-coding-agent-resume-session)
    ("R" "reload" pi-coding-agent-reload)
    ("N" "name" pi-coding-agent-set-session-name)
    ("e" "export" pi-coding-agent-export-html)
    ("Q" "quit" pi-coding-agent-quit)]
   ["Context"
    ("c" "compact" pi-coding-agent-compact)
    ("f" "fork" pi-coding-agent-fork)]]
  [["Model"
    ("m" "select" pi-coding-agent-select-model)
    ("t" "thinking" pi-coding-agent-cycle-thinking)]
   ["Info"
    ("i" "stats" pi-coding-agent-session-stats)
    ("y" "copy last" pi-coding-agent-copy-last-message)]]
  [["Actions"
    ("RET" "send" pi-coding-agent-send)
    ("s" "steer" pi-coding-agent-queue-steering)
    ("k" "abort" pi-coding-agent-abort)]])

(defun pi-coding-agent-refresh-commands ()
  "Refresh commands from pi via RPC."
  (interactive)
  (if-let ((proc (pi-coding-agent--get-process)))
      (pi-coding-agent--fetch-commands proc
        (lambda (commands)
          (pi-coding-agent--set-commands commands)
          (pi-coding-agent--rebuild-commands-menu)
          (message "Pi: Refreshed %d commands" (length commands))))
    (message "Pi: No active process")))

;;;; Command Submenus (Templates, Extensions, Skills)

(defun pi-coding-agent--commands-by-source (source)
  "Return commands filtered by SOURCE, sorted alphabetically."
  (sort (seq-filter (lambda (c) (equal (plist-get c :source) source))
                    pi-coding-agent--commands)
        (lambda (a b)
          (string< (plist-get a :name) (plist-get b :name)))))

(defun pi-coding-agent--commands-by-source-and-location (source location)
  "Return commands filtered by SOURCE and LOCATION, sorted alphabetically."
  (sort (seq-filter (lambda (c)
                      (and (equal (plist-get c :source) source)
                           (equal (plist-get c :location) location)))
                    pi-coding-agent--commands)
        (lambda (a b)
          (string< (plist-get a :name) (plist-get b :name)))))

(defun pi-coding-agent--submenu-commands-ordered (source)
  "Return commands for SOURCE ordered by location then name.
Location order: path, project, user, then commands without location.
Within each location group, commands are sorted alphabetically by name.
This ordering is shared by run keys (a-z) and edit keys (A-Z)."
  (let ((path-cmds (pi-coding-agent--commands-by-source-and-location source "path"))
        (project-cmds (pi-coding-agent--commands-by-source-and-location source "project"))
        (user-cmds (pi-coding-agent--commands-by-source-and-location source "user"))
        (no-location-cmds (seq-filter (lambda (c)
                                        (and (equal (plist-get c :source) source)
                                             (null (plist-get c :location))))
                                      pi-coding-agent--commands)))
    (append path-cmds project-cmds user-cmds no-location-cmds)))

(defun pi-coding-agent--make-submenu-children (source)
  "Build transient children for commands with SOURCE.
Returns a list suitable for `transient-parse-suffixes'.
Commands are grouped by location (path, project, user).
Descriptions are truncated to fit the current frame width."
  (let* ((path-cmds (pi-coding-agent--commands-by-source-and-location source "path"))
         (project-cmds (pi-coding-agent--commands-by-source-and-location source "project"))
         (user-cmds (pi-coding-agent--commands-by-source-and-location source "user"))
         ;; Extensions don't have location, get them separately
         (no-location-cmds (seq-filter (lambda (c)
                                          (and (equal (plist-get c :source) source)
                                               (null (plist-get c :location))))
                                        pi-coding-agent--commands))
         (key 0)
         ;; Calculate available width for descriptions
         (available-width (max 20 (- (frame-width) 28)))
         (children '()))
    ;; Build location groups in order: path, project, user (then no-location for extensions)
    (dolist (group `(("Path" . ,path-cmds)
                     ("Project" . ,project-cmds)
                     ("User" . ,user-cmds)
                     (nil . ,no-location-cmds)))
      (let ((label (car group))
            (cmds (cdr group)))
        (when cmds
          ;; Add section header if there's a label
          (when label
            (push label children))
          ;; Add commands
          (dolist (cmd cmds)
            (when (< key 26)
              (let* ((name (plist-get cmd :name))
                     (desc (or (plist-get cmd :description) "")))
                ;; Run command with letter key (a-z)
                (push (list (format "%c" (+ ?a key))
                            (format "%-20s  %s"
                                    (truncate-string-to-width name 20)
                                    (truncate-string-to-width desc available-width))
                            `(lambda ()
                               (interactive)
                               (pi-coding-agent--run-custom-command ',cmd)))
                      children)
                (cl-incf key)))))))
    (nreverse children)))

(defun pi-coding-agent--make-submenu-edit-children (source)
  "Build edit suffixes for commands with SOURCE.
Returns a list suitable for `transient-parse-suffixes'.
Edit keys use uppercase letters (A-Z), matching the run keys (a-z).
Keys are assigned from the full command list so that `a' and `A'
always refer to the same command.  Commands without a :path are
skipped but still consume a key position."
  (let* ((all-cmds (pi-coding-agent--submenu-commands-ordered source))
         (key 0)
         (children '()))
    (dolist (cmd all-cmds)
      (when (< key 26)
        (let ((path (plist-get cmd :path)))
          (when path
            (let ((name (plist-get cmd :name)))
              (push (list (format "%c" (+ ?A key))
                          (truncate-string-to-width name 12)
                          `(lambda ()
                             (interactive)
                             (find-file-other-window ,path)))
                    children)))
          (cl-incf key))))
    (nreverse children)))

(defun pi-coding-agent--make-edit-columns (prefix source)
  "Build edit section as columns for SOURCE.
PREFIX is the transient command symbol.
Returns children for `:setup-children' as column group vectors."
  (let* ((items (pi-coding-agent--make-submenu-edit-children source))
         (len (length items)))
    (when (> len 0)
      (let* ((num-cols (min 3 len))
             (per-col (ceiling len (float num-cols)))
             (columns '()))
        (dotimes (i num-cols)
          (let* ((start (* i per-col))
                 (col-items (seq-subseq items start (min (+ start per-col) len))))
            (when col-items
              (push (vector 'transient-column
                            nil
                            (transient-parse-suffixes prefix col-items))
                    columns))))
        (nreverse columns)))))

(transient-define-prefix pi-coding-agent-templates-menu ()
  "All prompt templates.
Press letter to run, Shift+letter to edit source file."
  [:class transient-column
   :setup-children
   (lambda (_)
     (when-let ((items (pi-coding-agent--make-submenu-children "prompt")))
       (transient-parse-suffixes 'pi-coding-agent-templates-menu items)))]
  [:class transient-columns
   :description "Edit"
   :setup-children
   (lambda (_)
     (pi-coding-agent--make-edit-columns
      'pi-coding-agent-templates-menu "prompt"))])

(transient-define-prefix pi-coding-agent-extensions-menu ()
  "All extension commands.
Press letter to run, Shift+letter to edit source file."
  [:class transient-column
   :setup-children
   (lambda (_)
     (when-let ((items (pi-coding-agent--make-submenu-children "extension")))
       (transient-parse-suffixes 'pi-coding-agent-extensions-menu items)))]
  [:class transient-columns
   :description "Edit"
   :setup-children
   (lambda (_)
     (pi-coding-agent--make-edit-columns
      'pi-coding-agent-extensions-menu "extension"))])

(transient-define-prefix pi-coding-agent-skills-menu ()
  "All available skills.
Press letter to run, Shift+letter to edit source file."
  [:class transient-column
   :setup-children
   (lambda (_)
     (when-let ((items (pi-coding-agent--make-submenu-children "skill")))
       (transient-parse-suffixes 'pi-coding-agent-skills-menu items)))]
  [:class transient-columns
   :description "Edit"
   :setup-children
   (lambda (_)
     (pi-coding-agent--make-edit-columns
      'pi-coding-agent-skills-menu "skill"))])

;;;; Main Menu Command Sections

(defun pi-coding-agent--rebuild-commands-menu ()
  "Rebuild command entries in transient menu.
Groups commands by source (extension, skill, template) with up to 3
quick-access commands per category and links to full submenus.
Sections are displayed side-by-side to use horizontal space."
  (let* ((extensions (pi-coding-agent--commands-by-source "extension"))
         (skills (pi-coding-agent--commands-by-source "skill"))
         (templates (pi-coding-agent--commands-by-source "prompt"))
         (columns '())
         (key 1))
    ;; Remove existing command group (index 4 if it exists)
    (ignore-errors (transient-remove-suffix 'pi-coding-agent-menu '(4)))
    ;; Build columns in display order: extensions, skills, templates
    ;; Keys are assigned sequentially across all categories
    (when extensions
      (push (pi-coding-agent--build-command-section
             "Extensions" extensions key 3 "E" 'pi-coding-agent-extensions-menu)
            columns)
      (setq key (+ key (min 3 (length extensions)))))
    (when skills
      (push (pi-coding-agent--build-command-section
             "Skills" skills key 3 "S" 'pi-coding-agent-skills-menu)
            columns)
      (setq key (+ key (min 3 (length skills)))))
    (when templates
      (push (pi-coding-agent--build-command-section
             "Templates" templates key 3 "T" 'pi-coding-agent-templates-menu)
            columns)
      (setq key (+ key (min 3 (length templates)))))
    ;; Add all columns as a single transient-columns group after Actions (index 3)
    (when columns
      (transient-append-suffix 'pi-coding-agent-menu '(3)
        (apply #'vector (nreverse columns))))))

(defun pi-coding-agent--build-command-section (title commands start-key max-shown more-key more-menu)
  "Build a transient section for TITLE with COMMANDS.
Shows up to MAX-SHOWN commands starting at START-KEY.
MORE-KEY and MORE-MENU provide access to the full list (shown first)."
  (let ((shown (seq-take commands max-shown))
        (suffixes (list title))
        (key start-key))
    ;; Add "all..." link first for discovery
    (push (list more-key "all..." more-menu) suffixes)
    ;; Add quick-access commands
    (dolist (cmd shown)
      (let ((name (plist-get cmd :name)))
        (push (list (number-to-string key)
                    (truncate-string-to-width name 18)
                    `(lambda () (interactive) (pi-coding-agent--run-custom-command ',cmd)))
              suffixes)
        (setq key (1+ key))))
    (apply #'vector (nreverse suffixes))))

(provide 'pi-coding-agent-menu)
;;; pi-coding-agent-menu.el ends here
