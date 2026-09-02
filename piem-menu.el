;;; piem-menu.el --- Transient menu and session management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/piem

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
;; infrastructure for piem.
;;
;; Key entry points:
;;   `piem-menu'            Transient menu (C-c C-p)
;;   `piem-new-session'     Start fresh session
;;   `piem-reload'          Restart pi process
;;   `piem-select-model'    Choose model interactively
;;   `piem-select-thinking' Choose thinking level interactively
;;   `piem-cycle-thinking'  Cycle thinking levels from header-line
;;   `piem-compact'         Compact conversation context
;;   `piem-fork'            Fork from previous message

;;; Code:

(require 'cl-lib)
(require 'piem-jsonl)
(require 'piem-render)
(require 'transient)

(declare-function piem-session-browser "piem-browse")
(declare-function piem-tree-browser "piem-browse")

(defconst piem--minimum-transient-version "0.9.0"
  "Minimum supported transient version.")

(defun piem--normalize-version (version)
  "Return the numeric prefix of VERSION, or nil when none is present."
  (when (and (stringp version)
             (string-match "[0-9]+\\(?:\\.[0-9]+\\)*" version))
    (match-string 0 version)))

(defun piem--version-at-least-p (version minimum)
  "Return non-nil when VERSION satisfies MINIMUM.
VERSION may include a leading prefix like `v' or extra suffix text."
  (let ((normalized (piem--normalize-version version)))
    (and normalized
         (not (version< normalized minimum)))))

(when (and (not (bound-and-true-p byte-compile-current-file))
           (or (not (boundp 'transient-version))
               (not (piem--version-at-least-p
                     transient-version
                     piem--minimum-transient-version))))
  (display-warning 'piem
                   (format "piem requires transient >= %s, \
but %s is loaded.
  Fix: upgrade transient from MELPA.  If Emacs is using an older built-in
  copy, set `package-install-upgrade-built-in' to t before running
  M-x package-install RET transient RET, then restart Emacs."
                           piem--minimum-transient-version
                           (if (boundp 'transient-version)
                               transient-version
                             "unknown"))
                   :error))

;;;; Slash Commands via RPC

(defun piem--normalize-command (cmd &optional anchor)
  "Normalize a command plist from the RPC wire format.
Lift `sourceInfo.scope' to `:location' and `sourceInfo.path' to
`:path' when present, mapping Pi's temporary scope to the menu's path bucket,
then drop the raw `:sourceInfo' key.  Path values from
Pi are normalized to Emacs paths using ANCHOR or `default-directory'.  Unsafe
passive backend path metadata is ignored rather than stored as navigable state.
Returns CMD (modified in place)."
  (when-let* ((info (plist-get cmd :sourceInfo)))
    (when-let* ((scope (plist-get info :scope)))
      (plist-put cmd :location
                 (if (equal scope "temporary") "path" scope)))
    (when-let* ((path (plist-get info :path)))
      (plist-put cmd :path path))
    (cl-remf cmd :sourceInfo))
  (when-let* ((path (plist-get cmd :path)))
    (if-let* ((emacs-path (piem--passive-emacs-path path anchor)))
        (plist-put cmd :path emacs-path)
      (cl-remf cmd :path)))
  cmd)

(defun piem--fetch-commands (proc callback &optional anchor)
  "Fetch available commands via RPC, call CALLBACK with result.
PROC is the pi process.  CALLBACK receives the command list on success.
ANCHOR is the session directory used to normalize command source paths."
  (piem--rpc-async proc '(:type "get_commands")
    (lambda (response)
      (when (eq (plist-get response :success) t)
        (let* ((data (plist-get response :data))
               (commands-vec (plist-get data :commands))
               (commands (mapcar (lambda (cmd)
                                   (piem--normalize-command
                                    cmd anchor))
                                 (append commands-vec nil))))
          (funcall callback commands))))))

(defun piem--refresh-commands-ignoring-errors
    (proc chat-buf generation anchor)
  "Refresh CHAT-BUF commands from PROC without owning transition completion.
Synchronous command-fetch errors are reported but do not finish GENERATION;
state/history latches remain responsible for unlocking session switches once
those refreshes have been scheduled."
  (condition-case err
      (piem--fetch-commands
       proc
       (lambda (commands)
         (when (piem--session-transition-current-p
                chat-buf proc generation)
           (with-current-buffer chat-buf
             (piem--set-commands commands)
             (piem--rebuild-commands-menu))))
       anchor)
    (error
     (message "Pi: Failed to refresh commands - %s"
              (error-message-string err)))))

;;;; Session Management

(defun piem--menu-state ()
  "Return session state from the chat buffer.
State is buffer-local in the chat buffer; this accessor works
from either chat or input buffer."
  (let ((chat-buf (piem--get-chat-buffer)))
    (and chat-buf (buffer-local-value 'piem--state chat-buf))))

(defun piem--menu-model-description ()
  "Return model description for transient menu."
  (let* ((state (piem--menu-state))
         (model (plist-get (plist-get state :model) :name))
         (short (and model (piem--shorten-model-name model))))
    (format "Model: %s" (or short "unknown"))))

(defun piem--menu-thinking-description ()
  "Return thinking level description for transient menu."
  (let* ((state (piem--menu-state))
         (level (plist-get state :thinking-level)))
    (format "Thinking: %s" (or level "off"))))

(defun piem--menu-description ()
  "Return the transient menu summary line."
  (concat (piem--menu-model-description) " • "
          (piem--menu-thinking-description)))

(defun piem--menu-default-thinking-display-mode ()
  "Return the completed-thinking display mode used for new chat buffers."
  piem-thinking-display)

(defun piem--menu-current-thinking-display-mode ()
  "Return the completed-thinking display mode for the linked chat buffer."
  (let ((chat-buf (piem--get-chat-buffer)))
    (if (and chat-buf (buffer-live-p chat-buf))
        (with-current-buffer chat-buf
          (piem--thinking-display-mode))
      (piem--menu-default-thinking-display-mode))))

(defun piem--next-thinking-display-mode (mode)
  "Return the thinking-display mode after MODE in the visible/hidden cycle."
  (if (eq mode 'hidden) 'visible 'hidden))

(defclass piem--thinking-display-setting (transient-variable)
  ((getter :initarg :getter)
   (setter :initarg :setter))
  "Transient row that shows and changes a thinking-display mode.")

(cl-defmethod transient-init-value ((obj piem--thinking-display-setting))
  "Initialize OBJ from its current thinking-display getter."
  (oset obj value (funcall (oref obj getter))))

(cl-defmethod transient-infix-read ((obj piem--thinking-display-setting))
  "Return the next visible/hidden thinking-display value for OBJ."
  (piem--next-thinking-display-mode (oref obj value)))

(cl-defmethod transient-infix-set ((obj piem--thinking-display-setting) value)
  "Set OBJ to VALUE using its configured thinking-display setter."
  (funcall (oref obj setter) value)
  (oset obj value value))

(cl-defmethod transient-format-value ((obj piem--thinking-display-setting))
  "Format OBJ's current thinking-display value for the transient menu."
  (propertize (symbol-name (oref obj value)) 'face 'transient-value))

(defun piem--new-session-ready-p (chat-buf)
  "Return non-nil when CHAT-BUF can safely start a fresh session.
Server-owned streaming may be reset deliberately; unresolved local ownership
and another transition may not be discarded."
  (with-current-buffer chat-buf
    (cond
     ((piem--session-transition-active-p)
      (message "Pi: Cannot start a new session while session is switching")
      nil)
     ((piem--prompt-start-wait-active-p)
      (message "Pi: Cannot start a new session while prompt acceptance is pending")
      nil)
     ((piem--model-change-pending-p)
      (message "Pi: Cannot start a new session while a model change is pending")
      nil)
     ((or piem--followup-queue
          (piem--followup-drain-pending-p))
      (message "Pi: Cannot start a new session with queued follow-ups")
      nil)
     (piem--local-user-message
      (message "Pi: Wait for pi to echo your prompt before starting a new session")
      nil)
     (t t))))

;;;###autoload
(defun piem-new-session ()
  "Start a new pi session (reset)."
  (interactive)
  (when-let* ((proc (piem--get-process))
             (chat-buf (piem--get-chat-buffer))
             ((piem--new-session-ready-p chat-buf)))
    (let ((generation
           (with-current-buffer chat-buf
             (piem--begin-session-transition proc))))
      (condition-case err
          (piem--rpc-async
           proc '(:type "new_session")
           (lambda (response)
             (when (piem--session-transition-current-p
                    chat-buf proc generation)
               (condition-case callback-error
                   (let* ((success (eq (plist-get response :success) t))
                          (data (plist-get response :data))
                          (cancelled (plist-get data :cancelled)))
                     (cond
                      ((and success
                            (piem--json-false-p cancelled))
                       (unwind-protect
                           (with-current-buffer chat-buf
                             (piem--clear-chat-buffer)
                             (piem--refresh-header))
                         (piem--refresh-session-state proc chat-buf))
                       (message "Pi: New session started"))
                      (t
                       (with-current-buffer chat-buf
                         (piem--finish-session-transition generation))
                       (if (and success cancelled
                                (not (piem--json-false-p cancelled)))
                           (message "Pi: New session cancelled")
                         (message "Pi: Failed to start new session: %s"
                                  (or (plist-get response :error)
                                      "unknown error"))))))
                 ((error quit)
                  (when (piem--session-transition-current-p
                         chat-buf proc generation)
                    (with-current-buffer chat-buf
                      (piem--finish-session-transition generation)))
                  (if (eq (car callback-error) 'quit)
                      (signal (car callback-error) (cdr callback-error))
                    (message "Pi: Failed to start new session: %s"
                             (error-message-string callback-error))))))))
        ((error quit)
         (when (buffer-live-p chat-buf)
           (with-current-buffer chat-buf
             (piem--finish-session-transition generation)))
         (signal (car err) (cdr err)))))))

(defun piem--session-list-directory (&optional chat-buf)
  "Return the directory containing CHAT-BUF's current JSONL session file.
Return nil when the current state has no usable session file.  Relative
session file names are resolved from the chat buffer's stable session
directory."
  (let ((chat-buf (or chat-buf (piem--get-chat-buffer))))
    (when (and chat-buf (buffer-live-p chat-buf))
      (with-current-buffer chat-buf
        (when-let* ((session-file (plist-get piem--state
                                             :session-file))
                    ((stringp session-file))
                    ((not (string-empty-p session-file))))
          (when-let* ((emacs-session-file
                       (piem--emacs-path
                        session-file
                        (piem--chat-session-directory chat-buf))))
            (piem--route-preserving-file-name-directory
             emacs-session-file)))))))

(defun piem--session-file-cwd-or-error (path)
  "Return the recorded cwd from session file PATH, or signal `user-error'.
The returned directory is an Emacs path with a trailing slash.  For remote
session files, the recorded process-local cwd is anchored to PATH's TRAMP
prefix.  PATH must be a readable pi session file whose session header contains
a non-empty absolute cwd that names an existing directory."
  (let ((session-file (piem--route-preserving-expand-file-name path)))
    (unless (file-readable-p session-file)
      (user-error "Session file is not readable: %s" session-file))
    (let ((session-info
           (piem-jsonl-read-session-info session-file)))
      (unless session-info
        (user-error "Not a pi session file: %s" session-file))
      (let ((cwd (plist-get session-info :cwd)))
        (unless (and (stringp cwd) (not (string-empty-p cwd)))
          (user-error "Session file has no usable cwd: %s" session-file))
        (when (file-remote-p cwd)
          (user-error "Session file cwd must be process-local, not remote: %s\nSession file: %s"
                      cwd session-file))
        (when (piem--remote-home-path-p cwd)
          (user-error "Session file cwd must be absolute, not home-relative: %s\nSession file: %s"
                      cwd session-file))
        (unless (file-name-absolute-p cwd)
          (user-error "Session file cwd is not absolute: %s\nSession file: %s"
                      cwd session-file))
        (let ((expanded-cwd (piem--emacs-directory cwd session-file)))
          (unless (file-directory-p expanded-cwd)
            (user-error "Stored session cwd is not an existing directory: %s\nSession file: %s"
                        expanded-cwd session-file))
          expanded-cwd)))))

(defun piem--update-session-name-from-file (session-file)
  "Update `piem--session-name' from SESSION-FILE metadata.
Call this from the chat buffer after switching or loading a session.
Return the parsed metadata, or nil when SESSION-FILE was not a pi session."
  (when session-file
    (let ((session-info
           (piem-jsonl-read-session-info session-file)))
      (setq piem--session-name (plist-get session-info :name))
      session-info)))

(defun piem--reset-session-state ()
  "Reset all session-specific state for a new session.
Call this when starting a new session to ensure no stale state persists."
  (dolist (marker (list piem--message-start-marker
                        piem--streaming-marker
                        piem--thinking-marker
                        piem--thinking-start-marker))
    (when (markerp marker)
      (set-marker marker nil)))
  (setq piem--session-name nil
        piem--cached-stats nil
        piem--assistant-header-shown nil
        piem--local-user-message nil
        piem--extension-status nil
        piem--working-message nil
        piem--pre-compaction-status nil
        piem--in-code-block nil
        piem--in-thinking-block nil
        piem--thinking-marker nil
        piem--thinking-start-marker nil
        piem--thinking-raw nil
        piem--line-parse-state 'line-start
        piem--pending-tool-overlay nil
        piem--tool-block-order-counter 0
        piem--thinking-block-order-counter 0)
  (piem--set-activity-phase "idle" 'reset t)
  (piem--clear-local-user-message-region)
  (piem--invalidate-model-change)
  (piem--clear-unsupported-extension-ui-warnings)
  (piem--invalidate-history-loads)
  (piem--finish-session-transition
   piem--session-transition-generation)
  ;; Use accessors for cross-module state
  (piem--cancel-followup-drain-timer)
  (piem--invalidate-prompt-start-wait)
  (piem--clear-followup-queue)
  (piem--set-aborted nil)
  (piem--set-canonical-messages nil)
  (piem--set-message-start-marker nil)
  (piem--set-streaming-marker nil)
  (when piem--tool-args-cache
    (clrhash piem--tool-args-cache))
  (when piem--live-tool-blocks
    (clrhash piem--live-tool-blocks)))

(defun piem--clear-chat-buffer ()
  "Clear the chat buffer and display fresh startup header.
Used when starting a new session."
  (when-let* ((chat-buf (piem--get-chat-buffer)))
    (with-current-buffer chat-buf
      (let ((inhibit-read-only t))
        (piem--clear-render-artifacts)
        (erase-buffer)
        (insert (piem--format-startup-header))
        (insert "\n")
        (piem--reset-session-state)
        (goto-char (point-max))))))

(defun piem--load-session-history
    (proc callback &optional chat-buf completion-callback)
  "Load and display session history from PROC.
Calls CALLBACK with message count when history is applied successfully.
CHAT-BUF is the target buffer; if nil, uses `piem--get-chat-buffer'.
Optional COMPLETION-CALLBACK is called after a current RPC response is handled,
even when the response failed or was not safe to render.  Note: When called
from async callbacks, pass CHAT-BUF explicitly."
  (let ((chat-buf (or chat-buf (piem--get-chat-buffer))))
    (when (and chat-buf (buffer-live-p chat-buf))
      (with-current-buffer chat-buf
        (let ((generation (piem--invalidate-history-loads)))
          (piem--rpc-async proc '(:type "get_messages")
                         (lambda (response)
                           (unwind-protect
                               (when (and (eq (plist-get response :success) t)
                                          (buffer-live-p chat-buf))
                                 (with-current-buffer chat-buf
                                   (when (and (eq piem--process proc)
                                              (= generation
                                                 piem--history-load-generation)
                                              (piem--canonical-rerender-safe-p))
                                     (let* ((messages (plist-get (plist-get response :data)
                                                                 :messages))
                                            (count (if (vectorp messages)
                                                       (length messages)
                                                     0)))
                                       (piem--display-session-history
                                        messages chat-buf)
                                       ;; Refresh header after loading history (resume/fork).
                                       (piem--refresh-header)
                                       (when callback
                                         (funcall callback count))))))
                             (when completion-callback
                               (funcall completion-callback response))))))))))

(defun piem--session-transition-ready-p (chat-buf action)
  "Return non-nil when CHAT-BUF may ACTION another session.
ACTION should be a short verb such as resume or fork for user messages."
  (with-current-buffer chat-buf
    (cond
     ((not (eq piem--status 'idle))
      (message "Pi: Cannot %s while streaming" action)
      nil)
     ((piem--session-busy-p)
      (message "Pi: Cannot %s while Pi is busy" action)
      nil)
     (piem--local-user-message
      (message "Pi: Wait for pi to echo your prompt before you %s" action)
      nil)
     (t t))))

(defun piem--make-session-transition-latch
    (chat-buf proc generation count)
  "Return a callback to finish transition GENERATION after COUNT invocations.
Only completions that still belong to CHAT-BUF, PROC, and GENERATION count, so
stale callbacks from older session switches cannot unlock newer transitions."
  (let ((remaining count)
        (finished nil))
    (lambda (&rest _)
      (when (and (not finished)
                 (piem--session-transition-current-p
                  chat-buf proc generation))
        (setq remaining (1- remaining))
        (when (<= remaining 0)
          (setq finished t)
          (when (buffer-live-p chat-buf)
            (with-current-buffer chat-buf
              (piem--finish-session-transition generation))))))))

(defun piem--refresh-transition-state-and-history
    (proc chat-buf generation &optional session-file history-callback)
  "Refresh state and history for CHAT-BUF through PROC in parallel.
The transition remains active until both the state and history RPC callbacks
for GENERATION have completed or failed.  Synchronous scheduling errors finish
the transition before being re-signaled so the UI cannot stay wedged."
  (let ((latch (piem--make-session-transition-latch
                chat-buf proc generation 2)))
    (condition-case err
        (progn
          (piem--refresh-session-state
           proc chat-buf session-file generation latch)
          (piem--load-session-history
           proc history-callback chat-buf latch))
      (error
       (when (piem--session-transition-current-p
              chat-buf proc generation)
         (with-current-buffer chat-buf
           (piem--finish-session-transition generation)))
       (signal (car err) (cdr err))))))

(defun piem--refresh-session-state
    (proc chat-buf &optional session-file generation completion-callback)
  "Refresh session state for CHAT-BUF from PROC.
SESSION-FILE seeds the session-name cache when the switching action already
knows the selected file.  Optional GENERATION ties the refresh to an existing
session transition; otherwise this function starts and finishes its own guard.
Optional COMPLETION-CALLBACK runs after a current get_state response is
handled, even when the response failed."
  (when (buffer-live-p chat-buf)
    (with-current-buffer chat-buf
      (piem--set-canonical-messages nil)
      (when session-file
        (piem--update-session-name-from-file session-file))
      (let* ((own-generation (null generation))
             (generation (or generation
                             (piem--begin-session-transition))))
        (condition-case err
            (piem--rpc-async proc '(:type "get_state")
              (lambda (response)
                (when (piem--session-transition-current-p
                       chat-buf proc generation)
                  (unwind-protect
                      (when (eq (plist-get response :success) t)
                        (piem--apply-state-response chat-buf response)
                        (when (buffer-live-p chat-buf)
                          (with-current-buffer chat-buf
                            (unless session-file
                              (when-let* ((current-session-file
                                           (plist-get piem--state
                                                      :session-file)))
                                (piem--update-session-name-from-file
                                 current-session-file)))
                            (force-mode-line-update t))))
                    (when completion-callback
                      (funcall completion-callback response))
                    (when (and own-generation (buffer-live-p chat-buf))
                      (with-current-buffer chat-buf
                        (piem--finish-session-transition
                         generation)))))))
          ((error quit)
           (when (and own-generation (buffer-live-p chat-buf))
             (with-current-buffer chat-buf
               (piem--finish-session-transition generation)))
           (signal (car err) (cdr err))))))))

;;;###autoload
(defun piem-reload ()
  "Reload the current session by restarting the pi process.
Useful for reloading extensions, skills, prompts, and themes after
editing them, or when the pi process has died or become unresponsive.
Starts a fresh process, switches it back to the cached session file, then
replaces the old process, refreshes state and commands, and rebuilds the chat
buffer from session history."
  (interactive)
  (let* ((chat-buf (piem--get-chat-buffer))
         (session-file (and chat-buf
                            (buffer-local-value 'piem--state chat-buf)
                            (plist-get (buffer-local-value 'piem--state chat-buf)
                                       :session-file))))
    (cond
     ((not chat-buf)
      (message "Pi: No session to reload"))
     ((not session-file)
      (message "Pi: No session file available - cannot reload"))
     (t
      (with-current-buffer chat-buf
        (piem--cancel-model-change-and-restore-followups))
      (message "Pi: Reloading...")
      (with-current-buffer chat-buf
        (let ((dir (piem--session-directory)))
          (unless (and (stringp dir)
                       (not (string-empty-p dir))
                       (file-name-absolute-p dir))
            (user-error "Pi: Cannot reload from invalid session directory: %s"
                        dir))
          (let ((session-path (piem--process-local-path
                               session-file dir))
                (old-proc piem--process))
            (setq piem--status 'idle)
            (let* ((new-proc (piem--start-process dir))
                   (generation (piem--begin-session-transition
                                new-proc)))
              (when (processp new-proc)
                (set-process-buffer new-proc chat-buf)
                (process-put new-proc 'piem-chat-buffer chat-buf)
                (piem--register-display-handler new-proc)
                (piem--rpc-async
                 new-proc
                 (list :type "switch_session" :sessionPath session-path)
                 (lambda (response)
                   (when (piem--session-transition-current-p
                          chat-buf new-proc generation)
                     (let* ((data (plist-get response :data))
                            (cancelled (plist-get data :cancelled)))
                       (if (and (eq (plist-get response :success) t)
                                (piem--json-false-p cancelled))
                           (let ((refresh-scheduled nil))
                             (condition-case err
                                 (progn
                                   (when (and (processp old-proc)
                                              (not (eq old-proc new-proc)))
                                     (piem--skip-process-kill-confirmation
                                      old-proc)
                                     (piem--unregister-display-handler
                                      old-proc)
                                     (when (process-live-p old-proc)
                                       (delete-process old-proc)))
                                   (when (buffer-live-p chat-buf)
                                     (with-current-buffer chat-buf
                                       (piem--set-process new-proc)))
                                   (piem--refresh-transition-state-and-history
                                    new-proc chat-buf generation session-file
                                    (lambda (_count)
                                      (message "Pi: Session reloaded")))
                                   (setq refresh-scheduled t))
                               (error
                                (when (piem--session-transition-current-p
                                       chat-buf new-proc generation)
                                  (with-current-buffer chat-buf
                                    (piem--finish-session-transition
                                     generation)))
                                (message "Pi: Failed to reload - %s"
                                         (error-message-string err))))
                             (when refresh-scheduled
                               (piem--refresh-commands-ignoring-errors
                                new-proc chat-buf generation dir)))
                         (piem--unregister-display-handler new-proc)
                         (when (process-live-p new-proc)
                           (delete-process new-proc))
                         (if (and cancelled
                                  (not (piem--json-false-p cancelled)))
                             (message "Pi: Reload cancelled")
                           (message "Pi: Failed to reload - %s"
                                    (or (plist-get response :error)
                                        "unknown error")))
                         (when (buffer-live-p chat-buf)
                           (with-current-buffer chat-buf
                             (piem--finish-session-transition
                              generation)))))))))))))))))

(defun piem--resume-selected-session (proc chat-buf selected-path)
  "Resume SELECTED-PATH using PROC and rebuild CHAT-BUF from its history."
  (let* ((target-dir (piem--session-file-cwd-or-error selected-path))
         (session (and (buffer-live-p chat-buf)
                       (piem--chat-session-name chat-buf)))
         (existing-target (piem--find-session target-dir session)))
    (when (and existing-target (not (eq existing-target chat-buf)))
      (user-error "Pi session already open for: %s" target-dir))
    (let* ((session-path (if (buffer-live-p chat-buf)
                             (with-current-buffer chat-buf
                               (piem--process-local-path
                                selected-path
                                (piem--chat-session-directory chat-buf)))
                           (piem--process-local-path selected-path)))
           (generation (when (buffer-live-p chat-buf)
                         (with-current-buffer chat-buf
                           (piem--begin-session-transition proc)))))
      (piem--rpc-async
       proc
       (list :type "switch_session" :sessionPath session-path)
       (lambda (response)
         (when (piem--session-transition-current-p
                chat-buf proc generation)
           (let* ((data (plist-get response :data))
                  (cancelled (plist-get data :cancelled)))
             (if (and (eq (plist-get response :success) t)
                      (piem--json-false-p cancelled))
                 (let ((refresh-scheduled nil))
                   (condition-case err
                       (progn
                         (when (buffer-live-p chat-buf)
                           (with-current-buffer chat-buf
                             (piem--retarget-session-buffers
                              target-dir)))
                         (piem--refresh-transition-state-and-history
                          proc chat-buf generation selected-path
                          (lambda (count)
                            (message "Pi: Resumed session (%d messages)" count)))
                         (setq refresh-scheduled t))
                     (error
                      (when (piem--session-transition-current-p
                             chat-buf proc generation)
                        (with-current-buffer chat-buf
                          (piem--finish-session-transition generation)))
                      (message "Pi: Failed to resume session - %s"
                               (error-message-string err))))
                   (when refresh-scheduled
                     (piem--refresh-commands-ignoring-errors
                      proc chat-buf generation target-dir)))
               (message "Pi: Failed to resume session")
               (when (buffer-live-p chat-buf)
                 (with-current-buffer chat-buf
                   (piem--finish-session-transition generation)))))))))))

;;;; Model and Thinking

(defun piem-set-session-name (name)
  "Set the session NAME for the current session.
The name is displayed in the session browser and header-line."
  (interactive
   (let ((chat-buf (piem--get-chat-buffer)))
     (list (read-string "Session name: "
                        (or (and chat-buf
                                 (buffer-local-value 'piem--session-name chat-buf))
                            "")))))
  (let* ((trimmed-name (string-trim name))
         (chat-buf (piem--get-chat-buffer)))
    (if (string-empty-p trimmed-name)
        ;; Consistent with TUI /name behavior
        (let ((current-name (and chat-buf
                                 (buffer-local-value 'piem--session-name chat-buf))))
          (if current-name
              (message "Pi: Session name: %s" current-name)
            (message "Pi: No session name set")))
      (let ((proc (piem--get-process)))
        (unless proc
          (user-error "No pi process running"))
        (piem--rpc-async proc
            (list :type "set_session_name" :name trimmed-name)
            (lambda (response)
              (if (eq (plist-get response :success) t)
                  (progn
                    (when (buffer-live-p chat-buf)
                      (with-current-buffer chat-buf
                        (setq piem--session-name trimmed-name)
                        (force-mode-line-update t)))
                    (message "Pi: Session name set to \"%s\"" trimmed-name))
                (message "Pi: Failed to set session name: %s"
                         (or (plist-get response :error) "unknown error")))))))))

(defun piem-select-model (&optional initial-input)
  "Select a model interactively.
Optional INITIAL-INPUT pre-fills the completion prompt for filtering."
  (interactive)
  (let ((proc (piem--get-process))
        (chat-buf (piem--get-chat-buffer)))
    (unless proc
      (user-error "No pi process running"))
    (when (piem--model-change-pending-p chat-buf)
      (user-error "A model change is already pending"))
    (when (piem--session-transition-ready-p
           chat-buf "change models")
      (let* ((state (piem--menu-state))
           (response (piem--rpc-sync proc '(:type "get_available_models") 5))
           (data (plist-get response :data))
           (models (plist-get data :models))
           (current-name (plist-get (plist-get state :model) :name))
           (current-provider (plist-get (plist-get state :model) :provider))
           (current-short (and current-name
                               (piem--shorten-model-name current-name)))
           (current-display (and current-short current-provider
                                (format "%s [%s]" current-short current-provider)))
           ;; Build alist of (display-string . model-plist) for selection
           ;; Display includes provider for clarity
           (model-alist (mapcar (lambda (m)
                                  (let ((short (piem--shorten-model-name
                                               (plist-get m :name)))
                                        (prov (plist-get m :provider)))
                                    (cons (format "%s [%s]" short (or prov "?"))
                                          m)))
                                models))
           (names (mapcar #'car model-alist))
           (choice (let ((completion-ignore-case t)
                         (completion-styles '(basic flex)))
                     (if initial-input
                         ;; Try auto-selecting on unique match
                         (let ((matches (completion-all-completions
                                         initial-input names nil
                                         (length initial-input))))
                           (when (consp matches)
                             (setcdr (last matches) nil))
                           (cond
                            ((= (length matches) 1) (car matches))
                            ((null matches)
                             (message "Pi: No model matching \"%s\"" initial-input)
                             nil)
                            (t (completing-read
                                (format "Model (current: %s): "
                                        (or current-display "unknown"))
                                names nil t initial-input))))
                       (completing-read
                        (format "Model (current: %s): "
                                (or current-display "unknown"))
                        names nil t)))))
      (when (and choice
                 (not (equal choice current-display))
                 (piem--session-transition-ready-p
                  chat-buf "change models"))
        (let* ((selected-model (cdr (assoc choice model-alist)))
               (model-id (plist-get selected-model :id))
               (provider (plist-get selected-model :provider))
               (token (piem--begin-model-change proc chat-buf)))
          (if (not token)
              (message "Pi: Process changed while selecting a model; try again")
            (condition-case err
                (piem--rpc-async
                 proc (list :type "set_model"
                            :provider provider
                            :modelId model-id)
                 (lambda (resp)
                   (when (piem--model-change-current-p token chat-buf)
                     (let ((success (eq (plist-get resp :success) t))
                           (applied nil))
                       (unwind-protect
                           (when success
                             (with-current-buffer chat-buf
                               (piem--update-state-from-response resp)
                               (force-mode-line-update))
                             (setq applied t))
                         (when (piem--finish-model-change
                                token chat-buf)
                           (when (buffer-live-p chat-buf)
                             (with-current-buffer chat-buf
                               (if applied
                                   (piem--schedule-followup-queue-processing)
                                 (piem--restore-followup-queue-to-input))))
                           (cond
                            (applied
                             (message "Pi: Model set to %s" choice))
                            ((not success)
                             (message "Pi: Failed to set model: %s"
                                      (or (plist-get resp :error)
                                          "unknown error"))))))))))
              ((error quit)
               (piem--finish-model-change token chat-buf)
               (signal (car err) (cdr err)))))))))))

(defun piem--thinking-level-effective-value (level model)
  "Return LEVEL's provider value for MODEL.
Return `:null' when MODEL explicitly marks LEVEL unsupported."
  (let* ((key (intern (concat ":" level)))
         (level-map (plist-get model :thinkingLevelMap)))
    (cond
     ((equal level "off") "off")
     ((and level-map (plist-member level-map key)) (plist-get level-map key))
     (t level))))

(defun piem--filter-thinking-level-aliases (levels model)
  "Remove unsupported and duplicate provider aliases from LEVELS for MODEL."
  (let (result seen)
    (dolist (level levels (nreverse result))
      (let ((effective (piem--thinking-level-effective-value level model)))
        (unless (or (eq effective :null)
                    (member effective seen)
                    (and (stringp effective)
                         (not (equal level effective))
                         (member effective levels)))
          (push effective seen)
          (push level result))))))

(defun piem--get-available-thinking-levels (proc &optional model)
  "Fetch thinking levels for the current MODEL from PROC via RPC.
Signal a user error when capability discovery is unavailable rather
than offering levels the current model may not support."
  (let ((response (piem--rpc-sync
                   proc '(:type "get_available_thinking_levels") 3)))
    (unless response
      (user-error "Timed out fetching thinking levels from Pi"))
    (unless (eq (plist-get response :success) t)
      (user-error "Failed to fetch thinking levels: %s"
                  (or (plist-get response :error) "unknown error")))
    (let* ((raw-levels (plist-get (plist-get response :data) :levels))
           (levels (cond
                    ((vectorp raw-levels) (append raw-levels nil))
                    (raw-levels raw-levels)
                    (t '("off")))))
      (piem--filter-thinking-level-aliases levels model))))

(defun piem-cycle-thinking ()
  "Cycle through thinking levels."
  (interactive)
  (when-let* ((proc (piem--get-process))
             (chat-buf (piem--get-chat-buffer)))
    (piem--rpc-async proc '(:type "cycle_thinking_level")
                   (lambda (response)
                     (when (and (eq (plist-get response :success) t)
                                (buffer-live-p chat-buf))
                       (with-current-buffer chat-buf
                         (piem--update-state-from-response response)
                         (force-mode-line-update)
                         (message "Pi: Thinking level: %s"
                                  (plist-get piem--state :thinking-level))))))))

(defun piem--refresh-thinking-level-state (proc chat-buf)
  "Refresh CHAT-BUF state from PROC after a thinking-level change.
Uses `get_state' so the UI reflects the server's actual level,
including any model-specific clamping."
  (piem--rpc-async
   proc '(:type "get_state")
   (lambda (response)
     (if (eq (plist-get response :success) t)
         (let* ((data (plist-get response :data))
                (level (or (plist-get data :thinkingLevel) "off")))
           (piem--apply-state-response chat-buf response)
           (message "Pi: Thinking level: %s" level))
       (message "Pi: Thinking level updated, but failed to refresh state%s"
                (if-let* ((error-text (plist-get response :error)))
                    (format ": %s" error-text)
                  ""))))))

(defun piem-select-thinking ()
  "Select a thinking level from the minibuffer."
  (interactive)
  (let ((proc (piem--get-process))
        (chat-buf (piem--get-chat-buffer)))
    (unless proc
      (user-error "No pi process running"))
    (unless chat-buf
      (user-error "No pi session buffer"))
    (let* ((state (piem--menu-state))
           (current (or (plist-get state :thinking-level) "off"))
           (available (piem--get-available-thinking-levels
                       proc (plist-get state :model)))
           (choice (completing-read
                    (format "Thinking level (current: %s): " current)
                    available
                    nil t)))
      (unless (equal choice current)
        (piem--rpc-async
         proc (list :type "set_thinking_level" :level choice)
         (lambda (response)
           (if (eq (plist-get response :success) t)
               (piem--refresh-thinking-level-state proc chat-buf)
             (message "Pi: Failed to set thinking level: %s"
                      (or (plist-get response :error) "unknown error")))))))))

(defun piem-toggle-thinking-display ()
  "Toggle completed-thinking display for the current chat buffer."
  (interactive)
  (piem--set-chat-thinking-display
   (piem--next-thinking-display-mode
    (piem--menu-current-thinking-display-mode))))

(defun piem--set-default-thinking-display (mode)
  "Set MODE as the default completed-thinking display for new chat buffers."
  (setq piem-thinking-display mode)
  (message "Pi: New chat buffers will %s completed thinking by default"
           (if (eq mode 'hidden) "hide" "show")))

(defun piem-toggle-default-thinking-display ()
  "Toggle the completed-thinking display default for new chat buffers.
This changes the live default for future chat buffers in the current Emacs
session.  Persist it with Customize or your init file if you want it to stick
across restarts."
  (interactive)
  (piem--set-default-thinking-display
   (piem--next-thinking-display-mode
    (piem--menu-default-thinking-display-mode))))

(transient-define-infix piem-menu-chat-thinking-display ()
  :class 'piem--thinking-display-setting
  :key "h"
  :description "This chat"
  :getter #'piem--menu-current-thinking-display-mode
  :setter #'piem--set-chat-thinking-display)

(transient-define-infix piem-menu-default-thinking-display ()
  :class 'piem--thinking-display-setting
  :key "H"
  :description "New chat default"
  :getter #'piem--menu-default-thinking-display-mode
  :setter #'piem--set-default-thinking-display)

;;;; Session Info and Actions

(defun piem--format-session-stats (stats)
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
            (piem--format-number input)
            (piem--format-number output)
            (piem--format-number total)
            (piem--format-number cache-read)
            (piem--format-number cache-write)
            cost messages tools)))

(defun piem-session-stats ()
  "Display session statistics in the echo area."
  (interactive)
  (when-let* ((proc (piem--get-process)))
    (piem--rpc-async proc '(:type "get_session_stats")
                   (lambda (response)
                     (if (eq (plist-get response :success) t)
                         (let ((data (plist-get response :data)))
                           (message "Pi: %s" (piem--format-session-stats data)))
                       (message "Pi: Failed to get session stats"))))))

(defun piem-process-info ()
  "Display process information for debugging.
Shows PID, status, and session file."
  (interactive)
  (let* ((chat-buf (piem--get-chat-buffer))
         (proc (and chat-buf (buffer-local-value 'piem--process chat-buf)))
         (state (and chat-buf (buffer-local-value 'piem--state chat-buf)))
         (status (and chat-buf (buffer-local-value 'piem--status chat-buf)))
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

(defun piem--handle-manual-compaction-response (chat-buf response)
  "Handle manual compact command RESPONSE for CHAT-BUF.
Canonical compaction events render success, failure, and queue effects.
This callback reports only command-level failure seen before any end event."
  (when (buffer-live-p chat-buf)
    (with-current-buffer chat-buf
      (unless (eq (plist-get response :success) t)
        (when (eq piem--status 'compacting)
          (setq piem--status 'idle)
          (piem--set-activity-phase "idle")
          (piem--restore-followup-queue-to-input)
          (message "Pi: Compact failed%s"
                   (if-let* ((error-text (plist-get response :error)))
                       (format ": %s" error-text)
                     "")))))))

(defun piem-compact (&optional custom-instructions)
  "Compact conversation context to reduce token usage.
Optional CUSTOM-INSTRUCTIONS provide guidance for the compaction summary."
  (interactive)
  (when-let* ((chat-buf (piem--get-chat-buffer)))
    (let ((proc (piem--get-process)))
      (cond
       ((null proc)
        (message "Pi: No process available - try M-x piem-reload or C-c C-p R"))
       ((not (process-live-p proc))
        (message "Pi: Process died - try M-x piem-reload or C-c C-p R"))
       (t
        (message "Pi: Compacting...")
        (with-current-buffer chat-buf
          (setq piem--status 'compacting)
          (piem--set-activity-phase "compact"))
        (piem--rpc-async
         proc
         (if custom-instructions
             (list :type "compact" :customInstructions custom-instructions)
           '(:type "compact"))
         (lambda (response)
           (piem--handle-manual-compaction-response chat-buf response))))))))

(defun piem-export-html (&optional output-path)
  "Export session to HTML file.
Optional OUTPUT-PATH specifies where to save; nil uses pi's default."
  (interactive
   (list (let ((path (read-string
                      "Export path on session host (RET for default): ")))
           (and (not (string-empty-p path)) path))))
  (when-let* ((proc (piem--get-process)))
    (let* ((chat-buf (piem--get-chat-buffer))
           (anchor (when (buffer-live-p chat-buf)
                     (with-current-buffer chat-buf
                       (piem--chat-session-directory chat-buf))))
           (process-output-path
            (piem--process-local-path output-path anchor)))
      (piem--rpc-async
       proc
       (if process-output-path
           (list :type "export_html" :outputPath process-output-path)
         '(:type "export_html"))
       (lambda (response)
         (if (eq (plist-get response :success) t)
             (let* ((data (plist-get response :data))
                    (path (plist-get data :path))
                    (emacs-path (piem--passive-emacs-path
                                 path anchor)))
               (if emacs-path
                   (message "Pi: Exported to %s" emacs-path)
                 (message "Pi: Exported, but Pi did not return a usable path")))
           (message "Pi: Export failed")))))))

(defun piem-copy-last-message ()
  "Copy last assistant message to kill ring."
  (interactive)
  (when-let* ((proc (piem--get-process)))
    (piem--rpc-async proc '(:type "get_last_assistant_text")
                   (lambda (response)
                     (if (eq (plist-get response :success) t)
                         (let* ((data (plist-get response :data))
                                (text (plist-get data :text)))
                           (if text
                               (progn
                                 (kill-new text)
                                 (message "Pi: Copied to kill ring"))
                             (message "Pi: No assistant message to copy")))
                       (message "Pi: Failed to get message"))))))

;;;; Fork

(defun piem--flatten-tree (nodes)
  "Flatten tree NODES into a hash table mapping id to node plist.
NODES is a vector of tree node plists, each with `:children' vector.
Returns a hash table for O(1) lookup by id.

Uses iterative traversal to avoid `max-lisp-eval-depth' errors on deep
session trees."
  (let ((index (make-hash-table :test 'equal))
        (stack nil))
    ;; Push roots in reverse so popping preserves original order.
    (let ((i (1- (length nodes))))
      (while (>= i 0)
        (push (aref nodes i) stack)
        (setq i (1- i))))
    (while stack
      (let* ((node (pop stack))
             (children (plist-get node :children)))
        (puthash (plist-get node :id) node index)
        (let ((i (1- (length children))))
          (while (>= i 0)
            (push (aref children i) stack)
            (setq i (1- i))))))
    index))

(defun piem--active-branch-user-ids (index leaf-id)
  "Return chronological list of user message IDs on the active branch.
INDEX is a hash table from `piem--flatten-tree'.
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

(defun piem--format-fork-message (msg &optional index)
  "Format MSG for display in fork selector.
MSG is a plist with :entryId and :text.
INDEX is the display index (1-based) for the message."
  (let* ((text (or (plist-get msg :text) ""))
         (preview (truncate-string-to-width text 60 nil nil "...")))
    (if index
        (format "%d: %s" index preview)
      preview)))

(defun piem-fork ()
  "Fork conversation from a previous user message.
Shows a selector of user messages and creates a fork from the selected one."
  (interactive)
  (when-let* ((proc (piem--get-process))
              (chat-buf (piem--get-chat-buffer)))
    (when (piem--session-transition-ready-p chat-buf "fork")
      (piem--rpc-async proc '(:type "get_fork_messages")
                     (lambda (response)
                       (if (eq (plist-get response :success) t)
                           (let* ((data (plist-get response :data))
                                  (messages (plist-get data :messages)))
                             ;; Note: messages is a vector from JSON, use seq-empty-p not null
                             (if (seq-empty-p messages)
                                 (message "Pi: No messages to fork from")
                               (piem--show-fork-selector proc messages)))
                         (message "Pi: Failed to get fork messages")))))))

(defun piem--resolve-fork-entry (response ordinal heading-count)
  "Resolve a fork entry ID from get_fork_messages RESPONSE.
ORDINAL is the 0-based user turn index.  HEADING-COUNT is the number
of visible You headings in the buffer.  Returns (ENTRY-ID . PREVIEW)
or nil if the ordinal could not be mapped."
  (when (eq (plist-get response :success) t)
    (let* ((data (plist-get response :data))
           (messages (append (plist-get data :messages) nil))
           ;; Use last N messages to align with visible headings in
           ;; compacted sessions.
           (visible-messages (last messages heading-count))
           (selected (nth ordinal visible-messages))
           (entry-id (plist-get selected :entryId)))
      (when entry-id
        (cons entry-id (piem--format-fork-message selected))))))

(defun piem-fork-at-point ()
  "Fork conversation from the user turn at point.
Determines which user message point is in (or after), confirms with
a preview, then forks.  Only works when the session is idle."
  (interactive)
  (let ((chat-buf (piem--get-chat-buffer)))
    (unless chat-buf
      (user-error "Pi: No chat buffer"))
    (with-current-buffer chat-buf
      (let* ((headings (piem--collect-you-headings))
             (ordinal (piem--user-turn-index-at-point headings)))
        (cond
         ((not (piem--session-transition-ready-p chat-buf "fork")))
         ((not ordinal)
          (message "Pi: No user message at point"))
         (t
          (let ((heading-count (length headings))
                (proc (piem--get-process)))
            (unless proc
              (user-error "Pi: No active process"))
            (piem--rpc-async proc '(:type "get_fork_messages")
              (lambda (response)
                (if (not (eq (plist-get response :success) t))
                    (if-let* ((error-text (plist-get response :error)))
                        (message "Pi: Failed to get fork messages: %s" error-text)
                      (message "Pi: Failed to get fork messages"))
                  (let ((result (piem--resolve-fork-entry
                                 response ordinal heading-count)))
                    (cond
                     ((not result)
                      (message "Pi: Could not map turn to entry ID"))
                     ((with-current-buffer chat-buf
                        (y-or-n-p (format "Fork from: %s? " (or (cdr result) "?"))))
                      (with-current-buffer chat-buf
                        (piem--execute-fork proc (car result))))))))))))))))

(defun piem--execute-fork (proc entry-id)
  "Execute fork to ENTRY-ID via PROC.
Sends the fork RPC, then on success: refreshes state, reloads history,
and pre-fills the input buffer with the forked message text.
Captures chat and input buffers at call time (before the async RPC)."
  (let* ((chat-buf (piem--get-chat-buffer))
         (input-buf (piem--get-input-buffer))
         (generation (when (buffer-live-p chat-buf)
                       (with-current-buffer chat-buf
                         (piem--begin-session-transition proc)))))
    (piem--rpc-async proc (list :type "fork" :entryId entry-id)
      (lambda (response)
        (when (piem--session-transition-current-p
               chat-buf proc generation)
          (if (eq (plist-get response :success) t)
              (let ((refresh-scheduled nil)
                    text)
                (condition-case err
                    (progn
                      (setq text (plist-get (plist-get response :data) :text))
                      (piem--refresh-transition-state-and-history
                       proc chat-buf generation nil
                       (lambda (count)
                         (message "Pi: Branched to new session (%d messages)" count)))
                      (setq refresh-scheduled t))
                  (error
                   (when (piem--session-transition-current-p
                          chat-buf proc generation)
                     (with-current-buffer chat-buf
                       (piem--finish-session-transition generation)))
                   (message "Pi: Branch failed - %s"
                            (error-message-string err))))
                ;; Pre-fill input with the forked message text.  Sending stays
                ;; blocked by the transition until state and history settle.
                (when refresh-scheduled
                  (condition-case err
                      (when (buffer-live-p input-buf)
                        (piem--replace-input-draft input-buf text))
                    (error
                     (message "Pi: Failed to prefill fork prompt - %s"
                              (error-message-string err))))))
            (message "Pi: Branch failed")
            (when (buffer-live-p chat-buf)
              (with-current-buffer chat-buf
                (piem--finish-session-transition generation)))))))))

(defun piem--show-fork-selector (proc messages)
  "Show selector for MESSAGES and fork on selection.
PROC is the pi process.
MESSAGES is a vector of plists from get_fork_messages."
  (let* ((index 0)
         ;; Reverse so most recent messages appear first (upstream sends chronological order)
         (reversed-messages (reverse (append messages nil)))
         (formatted (mapcar (lambda (msg)
                              (setq index (1+ index))
                              (cons (piem--format-fork-message msg index) msg))
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
      (piem--execute-fork proc (plist-get selected :entryId)))))

;;;; Custom Commands

(defun piem--command-chat-buffer-or-error ()
  "Return the current pi chat buffer for running a slash command.
Signal `user-error' when no live chat buffer is linked to the current buffer."
  (let ((chat-buf (piem--get-chat-buffer)))
    (unless (and chat-buf (buffer-live-p chat-buf))
      (user-error "No pi session in current buffer"))
    chat-buf))

(defun piem-run-command (name &optional args)
  "Run pi slash command NAME with optional ARGS in the current session.
NAME is the command name without the leading slash.  ARGS, when
non-nil and non-empty, is appended after one space.

This command sends through the pi session associated with the current
pi chat buffer or its linked input buffer.  Signal `user-error' when the
current buffer is not part of a pi session."
  (interactive
   (progn
     (piem--command-chat-buffer-or-error)
     (list (completing-read "Pi command: "
                            (mapcar (lambda (cmd) (plist-get cmd :name))
                                    piem--commands)
                            nil t)
           (read-string "Args: "))))
  (let ((chat-buf (piem--command-chat-buffer-or-error)))
    (let ((full-command (if (or (null args) (string-empty-p args))
                            (format "/%s" name)
                          (format "/%s %s" name args))))
      (with-current-buffer chat-buf
        (piem--prepare-and-send full-command)))))

(defun piem--run-custom-command (cmd)
  "Execute custom command CMD.
Always prompts for arguments - user can press Enter if none needed.
Sends the literal /command text to pi, which handles expansion."
  (let* ((name (plist-get cmd :name))
         (args-string (read-string (format "/%s: " name))))
    (piem-run-command name args-string)))

(defun piem-run-custom-command ()
  "Select and run a custom command.
Uses commands from pi's `get_commands' RPC."
  (interactive)
  (if (null piem--commands)
      (message "Pi: No commands available")
    (let* ((choices (mapcar (lambda (cmd)
                              (cons (format "%s - %s"
                                            (plist-get cmd :name)
                                            (or (plist-get cmd :description) ""))
                                    cmd))
                            piem--commands))
           (choice (completing-read "Command: " choices nil t))
           (cmd (cdr (assoc choice choices))))
      (when cmd
        (piem--run-custom-command cmd)))))

;;;; Transient Menu

(transient-define-prefix piem-menu ()
  "Pi coding agent menu."
  [:description #'piem--menu-description
   :class transient-row]
  [["Session"
    ("n" "new" piem-new-session)
    ("r" "sessions" piem-session-browser)
    ("R" "reload" piem-reload)
    ("N" "name" piem-set-session-name)
    ("e" "export" piem-export-html)
    ("Q" "quit" piem-quit)]
   ["Context"
    ("c" "compact" piem-compact)
    ("f" "fork" piem-fork)
    ("w" "tree" piem-tree-browser)]
   ["Actions"
    ("RET" "send" piem-send)
    ("s" "steer" piem-queue-steering)
    ("k" "abort" piem-abort)]]
  [["Model"
    ("m" "select" piem-select-model)
    ("t" "thinking" piem-select-thinking)]
   ["Completed thinking"
    (piem-menu-chat-thinking-display)
    (piem-menu-default-thinking-display)]
   ["Info"
    ("i" "stats" piem-session-stats)
    ("y" "copy last" piem-copy-last-message)]])

(defun piem-refresh-commands ()
  "Refresh commands from pi via RPC."
  (interactive)
  (if-let* ((proc (piem--get-process)))
      (let* ((chat-buf (piem--get-chat-buffer))
             (anchor (when (buffer-live-p chat-buf)
                       (with-current-buffer chat-buf
                         (piem--chat-session-directory chat-buf)))))
        (piem--fetch-commands proc
          (lambda (commands)
            (piem--set-commands commands)
            (piem--rebuild-commands-menu)
            (message "Pi: Refreshed %d commands" (length commands)))
          anchor))
    (message "Pi: No active process")))

;;;; Command Submenus (Templates, Extensions, Skills)

(defun piem--commands-by-source (source)
  "Return commands filtered by SOURCE, sorted alphabetically."
  (sort (seq-filter (lambda (c) (equal (plist-get c :source) source))
                    piem--commands)
        (lambda (a b)
          (string< (plist-get a :name) (plist-get b :name)))))

(defun piem--commands-by-source-and-location (source location)
  "Return commands filtered by SOURCE and LOCATION, sorted alphabetically."
  (sort (seq-filter (lambda (c)
                      (and (equal (plist-get c :source) source)
                           (equal (plist-get c :location) location)))
                    piem--commands)
        (lambda (a b)
          (string< (plist-get a :name) (plist-get b :name)))))

(defun piem--submenu-commands-ordered (source)
  "Return commands for SOURCE ordered by location then name.
Location order: path, project, user, then commands without location.
Within each location group, commands are sorted alphabetically by name.
This ordering is shared by run keys (a-z) and edit keys (A-Z)."
  (let ((path-cmds (piem--commands-by-source-and-location source "path"))
        (project-cmds (piem--commands-by-source-and-location source "project"))
        (user-cmds (piem--commands-by-source-and-location source "user"))
        (no-location-cmds (seq-filter (lambda (c)
                                        (and (equal (plist-get c :source) source)
                                             (null (plist-get c :location))))
                                      piem--commands)))
    (append path-cmds project-cmds user-cmds no-location-cmds)))

(defun piem--make-submenu-children (source)
  "Build transient children for commands with SOURCE.
Returns a list suitable for `transient-parse-suffixes'.
Commands are grouped by location (path, project, user).
Descriptions are truncated to fit the current frame width."
  (let* ((path-cmds (piem--commands-by-source-and-location source "path"))
         (project-cmds (piem--commands-by-source-and-location source "project"))
         (user-cmds (piem--commands-by-source-and-location source "user"))
         ;; Extensions don't have location, get them separately
         (no-location-cmds (seq-filter (lambda (c)
                                          (and (equal (plist-get c :source) source)
                                               (null (plist-get c :location))))
                                        piem--commands))
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
                               (piem--run-custom-command ',cmd)))
                      children)
                (cl-incf key)))))))
    (nreverse children)))

(defun piem--edit-command-source (path)
  "Visit command source PATH, normalizing Pi paths for the current session."
  (let* ((chat-buf (piem--command-chat-buffer-or-error))
         (anchor (with-current-buffer chat-buf
                   (piem--chat-session-directory chat-buf)))
         (emacs-path (or (piem--emacs-path path anchor)
                         path)))
    (find-file-other-window emacs-path)))

(defun piem--make-submenu-edit-children (source)
  "Build edit suffixes for commands with SOURCE.
Returns a list suitable for `transient-parse-suffixes'.
Edit keys use uppercase letters (A-Z), matching the run keys (a-z).
Keys are assigned from the full command list so that `a' and `A'
always refer to the same command.  Commands without a :path are
skipped but still consume a key position."
  (let* ((all-cmds (piem--submenu-commands-ordered source))
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
                             (piem--edit-command-source ,path)))
                    children)))
          (cl-incf key))))
    (nreverse children)))

(defun piem--make-edit-columns (prefix source)
  "Build edit section as columns for SOURCE.
PREFIX is the transient command symbol.
Returns children for `:setup-children' as column group vectors."
  (let* ((items (piem--make-submenu-edit-children source))
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

(transient-define-prefix piem-templates-menu ()
  "All prompt templates.
Press letter to run, Shift+letter to edit source file."
  [:class transient-column
   :setup-children
   (lambda (_)
     (when-let* ((items (piem--make-submenu-children "prompt")))
       (transient-parse-suffixes 'piem-templates-menu items)))]
  [:class transient-columns
   :description "Edit"
   :setup-children
   (lambda (_)
     (piem--make-edit-columns
      'piem-templates-menu "prompt"))])

(transient-define-prefix piem-extensions-menu ()
  "All extension commands.
Press letter to run, Shift+letter to edit source file."
  [:class transient-column
   :setup-children
   (lambda (_)
     (when-let* ((items (piem--make-submenu-children "extension")))
       (transient-parse-suffixes 'piem-extensions-menu items)))]
  [:class transient-columns
   :description "Edit"
   :setup-children
   (lambda (_)
     (piem--make-edit-columns
      'piem-extensions-menu "extension"))])

(transient-define-prefix piem-skills-menu ()
  "All available skills.
Press letter to run, Shift+letter to edit source file."
  [:class transient-column
   :setup-children
   (lambda (_)
     (when-let* ((items (piem--make-submenu-children "skill")))
       (transient-parse-suffixes 'piem-skills-menu items)))]
  [:class transient-columns
   :description "Edit"
   :setup-children
   (lambda (_)
     (piem--make-edit-columns
      'piem-skills-menu "skill"))])

;;;; Main Menu Command Sections

(defun piem--rebuild-commands-menu ()
  "Rebuild command entries in transient menu.
Groups commands by source (extension, skill, template) with up to 3
quick-access commands per category and links to full submenus.
Sections are displayed side-by-side to use horizontal space."
  (let* ((extensions (piem--commands-by-source "extension"))
         (skills (piem--commands-by-source "skill"))
         (templates (piem--commands-by-source "prompt"))
         (columns '())
         (key 1))
    ;; Remove existing command group (index 3 if it exists)
    (ignore-errors (transient-remove-suffix 'piem-menu '(3)))
    ;; Build columns in display order: extensions, skills, templates
    ;; Keys are assigned sequentially across all categories
    (when extensions
      (push (piem--build-command-section
             "Extensions" extensions key 3 "E" 'piem-extensions-menu)
            columns)
      (setq key (+ key (min 3 (length extensions)))))
    (when skills
      (push (piem--build-command-section
             "Skills" skills key 3 "S" 'piem-skills-menu)
            columns)
      (setq key (+ key (min 3 (length skills)))))
    (when templates
      (push (piem--build-command-section
             "Templates" templates key 3 "T" 'piem-templates-menu)
            columns)
      (setq key (+ key (min 3 (length templates)))))
    ;; Add all columns as a single transient-columns group after the static rows.
    (when columns
      (transient-append-suffix 'piem-menu '(2)
        (apply #'vector (nreverse columns))))))

(defun piem--build-command-section (title commands start-key max-shown more-key more-menu)
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
                    `(lambda () (interactive) (piem--run-custom-command ',cmd)))
              suffixes)
        (setq key (1+ key))))
    (apply #'vector (nreverse suffixes))))

(provide 'piem-menu)
;;; piem-menu.el ends here
