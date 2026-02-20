;;; pi-coding-agent-input.el --- Input buffer, history, and completion -*- lexical-binding: t; -*-

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

;; Input buffer features for pi-coding-agent: prompt composition,
;; history navigation (comint/eshell-style M-p/M-n), incremental
;; history search (readline-style C-r), file reference completion (@),
;; path completion (Tab), slash command completion, message queuing
;; (follow-up and steering), and send/abort commands.
;;
;; Key entry points:
;;   `pi-coding-agent-send'                  Send prompt (C-c C-c)
;;   `pi-coding-agent-abort'                 Abort streaming (C-c C-k)
;;   `pi-coding-agent-quit'                  Close session
;;   `pi-coding-agent-previous-input'        History backward (M-p)
;;   `pi-coding-agent-next-input'            History forward (M-n)
;;   `pi-coding-agent-history-isearch-backward'  History search (C-r)
;;   `pi-coding-agent-queue-steering'        Steering message (C-c C-s)

;;; Code:

(require 'pi-coding-agent-render)
(require 'ring)

;;;; Input History (comint/eshell style)

(defvar pi-coding-agent--input-ring-size 100
  "Size of the input history ring.")

(defvar-local pi-coding-agent--input-ring nil
  "Ring holding input history for this session.")

(defvar-local pi-coding-agent--input-ring-index nil
  "Current position in input ring, or nil if not navigating history.")

(defvar-local pi-coding-agent--input-saved nil
  "Saved input before starting history navigation.")

(defvar-local pi-coding-agent--history-isearch-active nil
  "Non-nil when history isearch is active.")

(defvar-local pi-coding-agent--history-isearch-saved-input nil
  "Saved input before starting history isearch.")

(defvar-local pi-coding-agent--history-isearch-index nil
  "Current history index during isearch.")

(defun pi-coding-agent--input-ring ()
  "Return the input ring, creating if necessary."
  (unless pi-coding-agent--input-ring
    (setq pi-coding-agent--input-ring (make-ring pi-coding-agent--input-ring-size)))
  pi-coding-agent--input-ring)

(defun pi-coding-agent--history-add (input)
  "Add INPUT to history ring if non-empty and different from last."
  (let ((ring (pi-coding-agent--input-ring))
        (trimmed (and input (string-trim input))))
    (when (and trimmed
               (not (string-empty-p trimmed))
               (or (ring-empty-p ring)
                   (not (string= input (ring-ref ring 0)))))
      (ring-insert ring input))))

(defun pi-coding-agent-previous-input ()
  "Cycle backwards through input history.
Saves current input before first navigation."
  (interactive)
  (let ((ring (pi-coding-agent--input-ring)))
    (when (ring-empty-p ring)
      (user-error "No history"))
    (unless pi-coding-agent--input-ring-index
      (setq pi-coding-agent--input-saved (buffer-string)))
    (let ((new-index (if pi-coding-agent--input-ring-index
                         (1+ pi-coding-agent--input-ring-index)
                       0)))
      (if (>= new-index (ring-length ring))
          (user-error "Beginning of history")
        (setq pi-coding-agent--input-ring-index new-index)
        (delete-region (point-min) (point-max))
        (insert (ring-ref ring new-index))))))

(defun pi-coding-agent-next-input ()
  "Cycle forwards through input history.
Restores saved input when moving past newest entry."
  (interactive)
  (unless pi-coding-agent--input-ring-index
    (user-error "End of history"))
  (let ((new-index (1- pi-coding-agent--input-ring-index)))
    (delete-region (point-min) (point-max))
    (if (< new-index 0)
        (progn
          (setq pi-coding-agent--input-ring-index nil)
          (when pi-coding-agent--input-saved
            (insert pi-coding-agent--input-saved)))
      (setq pi-coding-agent--input-ring-index new-index)
      (insert (ring-ref (pi-coding-agent--input-ring) new-index)))))

;;;; History Isearch

(defun pi-coding-agent-history-isearch-backward ()
  "Search input history backward using isearch.
Incrementally search through history with matches appearing
directly in the input buffer, like readline."
  (interactive)
  (let ((ring (pi-coding-agent--input-ring)))
    (when (ring-empty-p ring)
      (user-error "No history"))
    (setq pi-coding-agent--history-isearch-active t
          pi-coding-agent--history-isearch-saved-input (buffer-string)
          pi-coding-agent--history-isearch-index nil)
    (isearch-backward nil t)))

(defun pi-coding-agent--history-isearch-setup ()
  "Configure isearch for history searching."
  (when pi-coding-agent--history-isearch-active
    (setq isearch-message-prefix-add "history ")
    (setq-local isearch-search-fun-function
                #'pi-coding-agent--history-isearch-search-fun)
    (setq-local isearch-wrap-function
                #'pi-coding-agent--history-isearch-wrap)
    (setq-local isearch-push-state-function
                #'pi-coding-agent--history-isearch-push-state)
    (setq-local isearch-lazy-count nil)
    (add-hook 'isearch-mode-end-hook
              #'pi-coding-agent--history-isearch-end nil t)))

(defun pi-coding-agent--history-isearch-end ()
  "Clean up after history isearch ends.
Restore original input if isearch was quit, keep history item if accepted."
  (setq isearch-message-prefix-add nil)
  (setq-local isearch-search-fun-function #'isearch-search-fun-default)
  (setq-local isearch-wrap-function nil)
  (setq-local isearch-push-state-function nil)
  (kill-local-variable 'isearch-lazy-count)
  (remove-hook 'isearch-mode-end-hook #'pi-coding-agent--history-isearch-end t)
  (when isearch-mode-end-hook-quit
    (delete-region (point-min) (point-max))
    (insert (or pi-coding-agent--history-isearch-saved-input "")))
  (unless isearch-suspended
    (setq pi-coding-agent--history-isearch-active nil
          pi-coding-agent--history-isearch-saved-input nil
          pi-coding-agent--history-isearch-index nil)))

(defun pi-coding-agent--history-isearch-goto (index)
  "Load history item at INDEX into the buffer.
If INDEX is nil, restore saved input (current line content before search)."
  (setq pi-coding-agent--history-isearch-index index)
  (delete-region (point-min) (point-max))
  (if (and index (not (ring-empty-p (pi-coding-agent--input-ring))))
      (insert (ring-ref (pi-coding-agent--input-ring) index))
    (when (and pi-coding-agent--history-isearch-saved-input
               (> (length pi-coding-agent--history-isearch-saved-input) 0))
      (insert pi-coding-agent--history-isearch-saved-input))))

(defun pi-coding-agent--history-isearch-search-fun ()
  "Return search function for history isearch.
First searches current buffer text, then cycles through history."
  (lambda (string bound noerror)
    (let ((search-fun (isearch-search-fun-default))
          (ring (pi-coding-agent--input-ring))
          found)
      (or
       (funcall search-fun string bound noerror)
       (unless bound
         (condition-case nil
             (progn
               (while (not found)
                 (cond
                  (isearch-forward
                   (when (null pi-coding-agent--history-isearch-index)
                     (error "End of history; no next item"))
                   (let ((new-idx (1- pi-coding-agent--history-isearch-index)))
                     (if (< new-idx 0)
                         (pi-coding-agent--history-isearch-goto nil)
                       (pi-coding-agent--history-isearch-goto new-idx)))
                   (goto-char (point-min)))
                  (t
                   (let* ((cur-idx (or pi-coding-agent--history-isearch-index -1))
                          (new-idx (1+ cur-idx)))
                     (when (>= new-idx (ring-length ring))
                       (error "Beginning of history; no preceding item"))
                     (pi-coding-agent--history-isearch-goto new-idx))
                   (goto-char (point-max))))
                 (setq isearch-barrier (point)
                       isearch-opoint (point))
                 (setq found (funcall search-fun string nil noerror)))
               (point))
           (error nil)))))))

(defun pi-coding-agent--history-isearch-wrap ()
  "Wrap history isearch to beginning/end of history.
For forward search: go to oldest history item.
For backward search: go to current input (nil index)."
  (pi-coding-agent--history-isearch-goto
   (if isearch-forward
       (1- (ring-length (pi-coding-agent--input-ring)))
     nil))
  (goto-char (if isearch-forward (point-min) (point-max))))

(defun pi-coding-agent--history-isearch-push-state ()
  "Save history index for isearch state restoration."
  (let ((index pi-coding-agent--history-isearch-index))
    (lambda (_cmd)
      (pi-coding-agent--history-isearch-goto index))))

;;;; Input Mode

(defun pi-coding-agent--input-mode-strip-metadata-keywords ()
  "Remove markdown metadata font-lock keywords from the current buffer.
Strips YAML, declarative, and pandoc metadata matchers that would
otherwise highlight lines ending with `:' as metadata keys."
  (font-lock-remove-keywords nil
    (cl-remove-if-not
     (lambda (kw)
       (and (listp kw) (symbolp (car kw))
            (string-match-p "metadata" (symbol-name (car kw)))))
     markdown-mode-font-lock-keywords)))

(define-derived-mode pi-coding-agent-input-mode text-mode "Pi-Input"
  "Major mode for composing pi prompts.
Defaults to plain `text-mode'.  Set
`pi-coding-agent-input-markdown-highlighting' to non-nil for GFM
syntax highlighting while preserving mode identity and keybindings."
  :group 'pi-coding-agent
  (when pi-coding-agent-input-markdown-highlighting
    (gfm-mode)
    (setq major-mode 'pi-coding-agent-input-mode)
    (setq mode-name "Pi-Input")
    (use-local-map pi-coding-agent-input-mode-map)
    ;; Users see exactly what they type — never hide markup in input.
    (setq-local markdown-hide-markup nil)
    (pi-coding-agent--input-mode-strip-metadata-keywords))
  (setq-local header-line-format '(:eval (pi-coding-agent--header-line-string)))
  (add-hook 'completion-at-point-functions #'pi-coding-agent--command-capf nil t)
  (add-hook 'completion-at-point-functions #'pi-coding-agent--file-reference-capf nil t)
  (add-hook 'completion-at-point-functions #'pi-coding-agent--path-capf nil t)
  (add-hook 'post-self-insert-hook #'pi-coding-agent--maybe-complete-at nil t)
  (add-hook 'isearch-mode-hook #'pi-coding-agent--history-isearch-setup nil t)
  (add-hook 'kill-buffer-hook #'pi-coding-agent--cleanup-input-on-kill nil t))

;;;; Sending Prompts

(defun pi-coding-agent--accept-input-text (text)
  "Accept TEXT from input buffer state.
Adds TEXT to history, resets history navigation, and clears input."
  (pi-coding-agent--history-add text)
  (setq pi-coding-agent--input-ring-index nil
        pi-coding-agent--input-saved nil)
  (erase-buffer))

(defun pi-coding-agent--queue-followup-text (chat-buf text)
  "Accept TEXT and enqueue it as a follow-up in CHAT-BUF."
  (pi-coding-agent--accept-input-text text)
  (with-current-buffer chat-buf
    (pi-coding-agent--push-followup text)))

(defun pi-coding-agent-send ()
  "Send the current input buffer contents to pi.
Clears the input buffer after sending.  Does nothing if buffer is empty.
If pi is busy (sending, streaming, or compacting), queues a local follow-up.
The /compact command is handled locally; other slash commands sent to pi."
  (interactive)
  (let* ((text (string-trim (buffer-string)))
         (chat-buf (pi-coding-agent--get-chat-buffer))
         (status (and chat-buf (buffer-local-value 'pi-coding-agent--status chat-buf)))
         (busy (and status (memq status '(streaming sending compacting)))))
    (cond
     ((string-empty-p text) nil)
     (busy
      (pi-coding-agent--queue-followup-text chat-buf text)
      (message "Pi: Message queued (will send after current response)"))
     (t
      (pi-coding-agent--accept-input-text text)
      (with-current-buffer chat-buf
        (pi-coding-agent--prepare-and-send text))))))

(defun pi-coding-agent-abort ()
  "Abort the current pi operation.
Only works when streaming is in progress."
  (interactive)
  (when-let ((chat-buf (pi-coding-agent--get-chat-buffer)))
    (when (eq (buffer-local-value 'pi-coding-agent--status chat-buf) 'streaming)
      (with-current-buffer chat-buf
        (pi-coding-agent--set-aborted t))
      (when-let ((proc (pi-coding-agent--get-process)))
        (pi-coding-agent--rpc-async proc
                       (list :type "abort")
                       (lambda (_response)
                         (run-with-timer 2 nil (lambda () (message nil)))
                         (message "Pi: Aborted")))))))

(defun pi-coding-agent-quit ()
  "Close the current pi session.
Kills both chat and input buffers, terminates the process,
and removes the input window (merging its space with adjacent windows).

If a process is running, asks for confirmation first.  If the user
cancels, the session remains intact."
  (interactive)
  (let* ((chat-buf (pi-coding-agent--get-chat-buffer))
         (input-buf (pi-coding-agent--get-input-buffer))
         (proc (when (buffer-live-p chat-buf)
                 (buffer-local-value 'pi-coding-agent--process chat-buf)))
         (proc-live (and proc (process-live-p proc)))
         (input-windows nil))
    (when (and proc-live
               (process-query-on-exit-flag proc)
               (not (yes-or-no-p "Pi session has a running process; quit anyway? ")))
      (user-error "Quit cancelled"))
    ;; Disable query flag to prevent double-ask on buffer kill
    (when proc-live
      (set-process-query-on-exit-flag proc nil))
    (when (buffer-live-p input-buf)
      (setq input-windows (get-buffer-window-list input-buf nil t)))
    ;; Kill chat first — its cleanup hook cascades to input buffer
    (when (buffer-live-p chat-buf)
      (kill-buffer chat-buf))
    (when (buffer-live-p input-buf)
      (kill-buffer input-buf))
    (dolist (win input-windows)
      (when (window-live-p win)
        (ignore-errors (delete-window win))))))

;;;; Slash Command Completion

(defun pi-coding-agent--command-capf ()
  "Completion-at-point function for /commands in input buffer.
Returns completion data when point is after / at start of buffer.
Uses commands from pi's `get_commands' RPC."
  (when (and (eq (char-after (point-min)) ?/)
             (> (point) (point-min)))
    (let* ((start (1+ (point-min)))
           (end (point))
           (commands (mapcar (lambda (cmd) (plist-get cmd :name))
                             pi-coding-agent--commands)))
      (list start end commands :exclusive 'no))))

;;;; Editor Features: File Reference (@)

(defun pi-coding-agent--at-trigger-p ()
  "Return non-nil if @ at point should trigger file completion.
Returns nil when @ follows an alphanumeric character (like in emails).
Assumes point is right after the @."
  (or (< (point) 3)  ; @ at buffer start or position 2 (no char before @)
      (save-excursion
        (backward-char 2)  ; Move to char before @
        (looking-at-p "[^[:alnum:]]"))))

(defun pi-coding-agent--maybe-complete-at ()
  "Trigger completion after @ if at word boundary.
Called from `post-self-insert-hook'.
Does not trigger when @ follows alphanumeric (e.g., in email addresses)."
  (when (and (eq last-command-event ?@)
             (pi-coding-agent--at-trigger-p))
    (run-at-time 0 nil #'pi-coding-agent--complete-file-reference)))

(defun pi-coding-agent--complete-file-reference ()
  "Complete file reference after @."
  (let* ((files (pi-coding-agent--get-project-files))
         (choice (completing-read "File: " files nil nil)))
    (when (and choice (not (string-empty-p choice)))
      (insert choice))))

(defvar-local pi-coding-agent--project-files-cache nil
  "Cached list of project files for @ completion.")

(defvar-local pi-coding-agent--project-files-cache-time nil
  "Time when project files cache was last updated.")

(defconst pi-coding-agent--project-files-cache-ttl 30
  "Seconds before project files cache expires.")

(defconst pi-coding-agent--file-exclude-patterns
  '(".git" "node_modules" ".elpa" "target" "build" "__pycache__" ".venv" "dist")
  "Directory names to exclude when listing files with find.")

(defun pi-coding-agent--get-project-files ()
  "Get list of project files, respecting .gitignore.
Uses cache if available and not expired."
  (let ((now (float-time)))
    (when (or (null pi-coding-agent--project-files-cache)
              (null pi-coding-agent--project-files-cache-time)
              (> (- now pi-coding-agent--project-files-cache-time)
                 pi-coding-agent--project-files-cache-ttl))
      (setq pi-coding-agent--project-files-cache
            (pi-coding-agent--list-project-files))
      (setq pi-coding-agent--project-files-cache-time now))
    pi-coding-agent--project-files-cache))

(defun pi-coding-agent--list-project-files ()
  "List project files using git ls-files or find.
Respects .gitignore when in a git repository."
  (let* ((dir (pi-coding-agent--session-directory))
         (default-directory dir))
    (condition-case nil
        (let ((output (shell-command-to-string
                       "git ls-files --cached --others --exclude-standard 2>/dev/null")))
          (if (string-empty-p output)
              (pi-coding-agent--list-files-with-find dir)
            (split-string output "\n" t)))
      (error (pi-coding-agent--list-files-with-find dir)))))

(defun pi-coding-agent--list-files-with-find (dir)
  "List files in DIR using find.
Excludes directories listed in `pi-coding-agent--file-exclude-patterns'."
  (let* ((default-directory dir)
         (prune-expr (mapconcat (lambda (p) (format "-name '%s'" p))
                                pi-coding-agent--file-exclude-patterns
                                " -o "))
         (cmd (format "find . \\( %s \\) -prune -o -type f -print 2>/dev/null | sed 's|^\\./||'"
                      prune-expr)))
    (split-string (shell-command-to-string cmd) "\n" t)))

(defun pi-coding-agent--file-reference-capf ()
  "Completion-at-point function for @file references.
Triggers when @ is typed, provides completion of project files."
  (when-let* ((at-pos (save-excursion
                        (when (search-backward "@" (line-beginning-position) t)
                          (point)))))
    (let* ((start (1+ at-pos))
           (end (point))
           (prefix (buffer-substring-no-properties start end))
           (files (pi-coding-agent--get-project-files))
           (candidates (if (string-empty-p prefix)
                           files
                         (cl-remove-if-not
                          (lambda (f) (string-match-p (regexp-quote prefix) f))
                          files))))
      (when candidates
        (list start end candidates
              :exclusive 'no
              :annotation-function (lambda (_) " (file)")
              :company-kind (lambda (_) 'file))))))

;;;; Editor Features: Path Completion

(defun pi-coding-agent--path-prefix-p (path)
  "Check if PATH has a completable prefix (./, ../, ~/, or /)."
  (or (string-prefix-p "./" path)
      (string-prefix-p "../" path)
      (string-prefix-p "~/" path)
      (string-prefix-p "/" path)))

(defun pi-coding-agent--path-completions (path)
  "Return file completion candidates for PATH, or nil if directory invalid."
  (let* ((dir (file-name-directory path))
         (base (file-name-nondirectory path))
         (expanded-dir (expand-file-name (or dir "") (pi-coding-agent--session-directory))))
    (when (file-directory-p expanded-dir)
      (mapcar (lambda (f) (concat (or dir "") f))
              (cl-remove-if (lambda (f) (member f '("." ".." "./" "../")))
                            (file-name-all-completions base expanded-dir))))))

(defun pi-coding-agent--path-capf ()
  "Completion-at-point function for file paths.
Completes paths starting with ./, ../, ~/, or /.
Skips / at buffer start to allow slash command completion."
  (when-let* ((bounds (bounds-of-thing-at-point 'filename))
              (start (car bounds))
              (end (cdr bounds))
              (path (buffer-substring-no-properties start end))
              ((pi-coding-agent--path-prefix-p path))
              ((not (and (string-prefix-p "/" path)
                         (= start (point-min)))))
              (candidates (pi-coding-agent--path-completions path)))
    (list start end candidates
          :exclusive 'no
          :annotation-function
          (lambda (c)
            (if (file-directory-p (expand-file-name c (pi-coding-agent--session-directory)))
                " (dir)" " (file)")))))

;;;; Editor Features: Message Queuing

(defun pi-coding-agent--send-steer-message (text)
  "Send TEXT as a steering message via RPC.
Returns t if message was sent, nil if process unavailable.
Shows error message if RPC fails."
  (let ((proc (pi-coding-agent--get-process)))
    (if (and proc (process-live-p proc))
        (progn
          (pi-coding-agent--rpc-async proc
                                      (list :type "steer" :message text)
                                      (lambda (response)
                                        (unless (eq (plist-get response :success) t)
                                          (message "Pi: Steering failed: %s"
                                                   (or (plist-get response :error) "unknown error")))))
          t)
      (message "Pi: Cannot send steering - process unavailable")
      nil)))

(defun pi-coding-agent-queue-steering ()
  "Send current input as a steering message.
When pi is sending or streaming, steering interrupts remaining tools.
Unlike normal sends, steering is NOT displayed locally - pi will echo
it back via message_start at the correct position (after current
assistant output completes).

When compaction is in progress, steering text is queued as a local
follow-up and sent after compaction completes."
  (interactive)
  (let ((text (string-trim (buffer-string))))
    (unless (string-empty-p text)
      (let ((chat-buf (pi-coding-agent--get-chat-buffer)))
        (when chat-buf
          (let ((status (buffer-local-value 'pi-coding-agent--status chat-buf)))
            (cond
             ((eq status 'idle)
              (message "Pi: Nothing to interrupt - use C-c C-c to send"))
             ((eq status 'compacting)
              (pi-coding-agent--queue-followup-text chat-buf text)
              (message "Pi: Steering queued (will send after compaction)"))
             (t
              (when (pi-coding-agent--send-steer-message text)
                (pi-coding-agent--accept-input-text text)
                (message "Pi: Steering message sent"))))))))))

(defun pi-coding-agent-queue-followup ()
  "Queue current input as a follow-up message.
Obsolete: Use `pi-coding-agent-send' (C-c C-c) instead, which now
automatically queues as follow-up when the agent is busy."
  (interactive)
  (pi-coding-agent-send))
(make-obsolete 'pi-coding-agent-queue-followup 'pi-coding-agent-send "1.3.0")

(provide 'pi-coding-agent-input)
;;; pi-coding-agent-input.el ends here
