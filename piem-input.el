;;; piem-input.el --- Input buffer, history, and completion -*- lexical-binding: t; -*-

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

;; Input buffer features for piem: prompt composition,
;; history navigation (comint/eshell-style M-p/M-n), incremental
;; history search (readline-style C-r), file reference completion (@),
;; path completion (Tab), slash command completion, message queuing
;; (follow-up and steering), and send/abort commands.
;;
;; Key entry points:
;;   `piem-send'                  Send prompt (C-c C-c)
;;   `piem-attach-image'          Attach one prompt image (C-c C-a)
;;   `piem-abort'                 Abort current operation (C-c C-k)
;;   `piem-quit'                  Close session
;;   `piem-previous-input'        History backward (M-p)
;;   `piem-next-input'            History forward (M-n)
;;   `piem-input-previous-message' Navigate previous chat message
;;   `piem-input-next-message'    Navigate next chat message
;;   `piem-history-isearch-backward'  History search (C-r)
;;   `piem-queue-steering'        Steering message (C-c C-s)

;;; Code:

(require 'piem-render)
(require 'ring)

;;;; Input History (comint/eshell style)

(defvar piem--input-ring-size 100
  "Size of the input history ring.")

(defvar-local piem--input-ring nil
  "Ring holding input history for this session.")

(defvar-local piem--input-ring-index nil
  "Current position in input ring, or nil if not navigating history.")

(defvar-local piem--input-saved nil
  "Saved input before starting history navigation.")

(defvar-local piem--history-isearch-active nil
  "Non-nil when history isearch is active.")

(defvar-local piem--history-isearch-saved-input nil
  "Saved input before starting history isearch.")

(defvar-local piem--history-isearch-index nil
  "Current history index during isearch.")

(defun piem--input-ring ()
  "Return the input ring, creating if necessary."
  (unless piem--input-ring
    (setq piem--input-ring (make-ring piem--input-ring-size)))
  piem--input-ring)

(defun piem--history-add (input)
  "Add INPUT to history ring if non-empty and different from last."
  (let ((ring (piem--input-ring))
        (trimmed (and input (string-trim input))))
    (when (and trimmed
               (not (string-empty-p trimmed))
               (or (ring-empty-p ring)
                   (not (string= input (ring-ref ring 0)))))
      (ring-insert ring input))))

(defun piem-previous-input ()
  "Cycle backwards through input history.
Saves current input before first navigation."
  (interactive)
  (let ((ring (piem--input-ring)))
    (when (ring-empty-p ring)
      (user-error "No history"))
    (unless piem--input-ring-index
      (setq piem--input-saved (buffer-string)))
    (let ((new-index (if piem--input-ring-index
                         (1+ piem--input-ring-index)
                       0)))
      (if (>= new-index (ring-length ring))
          (user-error "Beginning of history")
        (setq piem--input-ring-index new-index)
        (delete-region (point-min) (point-max))
        (insert (ring-ref ring new-index))))))

(defun piem-next-input ()
  "Cycle forwards through input history.
Restores saved input when moving past newest entry."
  (interactive)
  (unless piem--input-ring-index
    (user-error "End of history"))
  (let ((new-index (1- piem--input-ring-index)))
    (delete-region (point-min) (point-max))
    (if (< new-index 0)
        (progn
          (setq piem--input-ring-index nil)
          (when piem--input-saved
            (insert piem--input-saved)))
      (setq piem--input-ring-index new-index)
      (insert (ring-ref (piem--input-ring) new-index)))))

;;;; History Isearch

(defun piem-history-isearch-backward ()
  "Search input history backward using isearch.
Incrementally search through history with matches appearing
directly in the input buffer, like readline."
  (interactive)
  (let ((ring (piem--input-ring)))
    (when (ring-empty-p ring)
      (user-error "No history"))
    (setq piem--history-isearch-active t
          piem--history-isearch-saved-input (buffer-string)
          piem--history-isearch-index nil)
    (isearch-backward nil t)))

(defun piem--history-isearch-setup ()
  "Configure isearch for history searching."
  (when piem--history-isearch-active
    (setq isearch-message-prefix-add "history ")
    (setq-local isearch-search-fun-function
                #'piem--history-isearch-search-fun)
    (setq-local isearch-wrap-function
                #'piem--history-isearch-wrap)
    (setq-local isearch-push-state-function
                #'piem--history-isearch-push-state)
    (setq-local isearch-lazy-count nil)
    (add-hook 'isearch-mode-end-hook
              #'piem--history-isearch-end nil t)))

(defun piem--history-isearch-end ()
  "Clean up after history isearch ends.
Restore original input if isearch was quit, keep history item if accepted."
  (setq isearch-message-prefix-add nil)
  (setq-local isearch-search-fun-function #'isearch-search-fun-default)
  (setq-local isearch-wrap-function nil)
  (setq-local isearch-push-state-function nil)
  (kill-local-variable 'isearch-lazy-count)
  (remove-hook 'isearch-mode-end-hook #'piem--history-isearch-end t)
  (when isearch-mode-end-hook-quit
    (delete-region (point-min) (point-max))
    (insert (or piem--history-isearch-saved-input "")))
  (unless isearch-suspended
    (setq piem--history-isearch-active nil
          piem--history-isearch-saved-input nil
          piem--history-isearch-index nil)))

(defun piem--history-isearch-goto (index)
  "Load history item at INDEX into the buffer.
If INDEX is nil, restore saved input (current line content before search)."
  (setq piem--history-isearch-index index)
  (delete-region (point-min) (point-max))
  (if (and index (not (ring-empty-p (piem--input-ring))))
      (insert (ring-ref (piem--input-ring) index))
    (when (and piem--history-isearch-saved-input
               (> (length piem--history-isearch-saved-input) 0))
      (insert piem--history-isearch-saved-input))))

(defun piem--history-isearch-search-fun ()
  "Return search function for history isearch.
First searches current buffer text, then cycles through history."
  (lambda (string bound noerror)
    (let ((search-fun (isearch-search-fun-default))
          (ring (piem--input-ring))
          found)
      (or
       (funcall search-fun string bound noerror)
       (unless bound
         (condition-case nil
             (progn
               (while (not found)
                 (cond
                  (isearch-forward
                   (when (null piem--history-isearch-index)
                     (error "End of history; no next item"))
                   (let ((new-idx (1- piem--history-isearch-index)))
                     (if (< new-idx 0)
                         (piem--history-isearch-goto nil)
                       (piem--history-isearch-goto new-idx)))
                   (goto-char (point-min)))
                  (t
                   (let* ((cur-idx (or piem--history-isearch-index -1))
                          (new-idx (1+ cur-idx)))
                     (when (>= new-idx (ring-length ring))
                       (error "Beginning of history; no preceding item"))
                     (piem--history-isearch-goto new-idx))
                   (goto-char (point-max))))
                 (setq isearch-barrier (point)
                       isearch-opoint (point))
                 (setq found (funcall search-fun string nil noerror)))
               (point))
           (error nil)))))))

(defun piem--history-isearch-wrap ()
  "Wrap history isearch to beginning/end of history.
For forward search: go to oldest history item.
For backward search: go to current input (nil index)."
  (piem--history-isearch-goto
   (if isearch-forward
       (1- (ring-length (piem--input-ring)))
     nil))
  (goto-char (if isearch-forward (point-min) (point-max))))

(defun piem--history-isearch-push-state ()
  "Save history index for isearch state restoration."
  (let ((index piem--history-isearch-index))
    (lambda (_cmd)
      (piem--history-isearch-goto index))))

;;;; Input Mode

(defun piem--input-kill-buffer-query ()
  "Ask before killing input when its linked chat owns a live process."
  (piem--session-kill-buffer-query))

(define-derived-mode piem-input-mode text-mode "Pi-Input"
  "Major mode for composing pi prompts.
Uses tree-sitter markdown highlighting by default while preserving raw
markup visibility, mode identity, and keybindings.  Set
`piem-input-markdown-highlighting' to nil for plain text."
  :group 'piem
  (when piem-input-markdown-highlighting
    (md-ts-mode)
    (setq major-mode 'piem-input-mode)
    (setq mode-name "Pi-Input")
    (use-local-map piem-input-mode-map)
    ;; Users see exactly what they type — never hide markup in input.
    (setq-local md-ts-hide-markup nil)
    (md-ts--set-hide-markup nil))
  (setq-local header-line-format '(:eval (piem--header-line-string)))
  ;; Reset inherited completions (text-mode adds ispell, etc.) — our
  ;; input buffer should only offer slash commands, file refs, and paths.
  (setq-local completion-at-point-functions nil)
  (add-hook 'completion-at-point-functions #'piem--command-capf nil t)
  (add-hook 'completion-at-point-functions #'piem--file-reference-capf nil t)
  (add-hook 'completion-at-point-functions #'piem--path-capf nil t)
  (add-hook 'post-self-insert-hook #'piem--maybe-complete-at nil t)
  (add-hook 'isearch-mode-hook #'piem--history-isearch-setup nil t)
  (add-hook 'kill-buffer-query-functions
            #'piem--input-kill-buffer-query nil t)
  (add-hook 'kill-buffer-hook #'piem--cleanup-input-on-kill nil t))

;;;; Input-Buffer Chat Navigation

(defun piem--call-in-visible-chat-window (fn)
  "Call FN in the visible linked chat window, preserving input focus."
  (let* ((chat-buf (piem--get-chat-buffer))
         (win (and (buffer-live-p chat-buf)
                   (get-buffer-window chat-buf))))
    (if (window-live-p win)
        (save-selected-window
          (select-window win)
          (funcall fn))
      (user-error "No chat window visible"))))

(defun piem-input-next-message ()
  "Move chat to the next user message, keeping focus in input."
  (interactive)
  (piem--call-in-visible-chat-window
   #'piem-next-message))

(defun piem-input-previous-message ()
  "Move chat to the previous user message, keeping focus in input."
  (interactive)
  (piem--call-in-visible-chat-window
   #'piem-previous-message))

;;;; Prompt Images

(defun piem--prompt-image-byte-limit ()
  "Return the configured nonnegative byte limit for a prompt image."
  (if (natnump piem-prompt-image-max-bytes)
      piem-prompt-image-max-bytes
    (* 3 1024 1024)))

(defun piem--sniff-prompt-image-mime-type (data)
  "Return the supported MIME type sniffed from unibyte DATA, or nil."
  (let ((length (length data)))
    (cond
     ((and (>= length 8)
           (= (aref data 0) #x89)
           (equal (substring data 1 8) "PNG\r\n\x1a\n"))
      "image/png")
     ((and (>= length 3)
           (= (aref data 0) #xff)
           (= (aref data 1) #xd8)
           (= (aref data 2) #xff))
      "image/jpeg")
     ((and (>= length 6)
           (member (substring data 0 6) '("GIF87a" "GIF89a")))
      "image/gif")
     ((and (>= length 12)
           (equal (substring data 0 4) "RIFF")
           (equal (substring data 8 12) "WEBP"))
      "image/webp"))))

(defun piem--read-prompt-image (path)
  "Read and materialize supported prompt image PATH.
The file is read literally through Emacs, including through file-name
handlers, and is never handed to the Pi process as a path."
  (let* ((path (piem--route-preserving-expand-file-name path))
         (limit (piem--prompt-image-byte-limit))
         (attributes (file-attributes path 'string))
         (reported-size (and attributes (file-attribute-size attributes))))
    (unless (and attributes (file-regular-p path) (file-readable-p path))
      (user-error "Prompt image is not a readable regular file: %s" path))
    (when (> reported-size limit)
      (user-error "Prompt image is too large (%s; limit %s)"
                  (file-size-human-readable reported-size 'iec " " "B")
                  (file-size-human-readable limit 'iec " " "B")))
    (let ((data (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (let ((coding-system-for-read 'no-conversion))
                    (insert-file-contents-literally
                     path nil 0
                     (and (< limit most-positive-fixnum) (1+ limit))))
                  (buffer-string))))
      (when (> (length data) limit)
        (user-error "Prompt image exceeds the %s byte limit"
                    (file-size-human-readable limit 'iec " " "B")))
      (let ((mime-type (piem--sniff-prompt-image-mime-type data)))
        (unless mime-type
          (user-error "Unsupported prompt image format: %s" path))
        (piem--make-prompt-image
         :name (file-name-nondirectory path)
         :mime-type mime-type
         :byte-size (length data)
         :data (base64-encode-string data t))))))

;;;###autoload
(defun piem-attach-image (&optional clear)
  "Attach one materialized image to the current prompt draft.
With prefix argument CLEAR, remove the attached image instead.  A new image
replaces the previous draft image."
  (interactive "P")
  (let ((input-buffer (piem--get-input-buffer)))
    (unless (buffer-live-p input-buffer)
      (user-error "No pi input buffer for this command"))
    (with-current-buffer input-buffer
      (let ((chat-buf (piem--get-chat-buffer)))
        (when (and (buffer-live-p chat-buf)
                   (with-current-buffer chat-buf
                     (piem--prompt-start-wait-active-p)))
          (user-error
           "Cannot change prompt image while prompt acceptance is pending"))
        (if clear
            (progn
              (piem--clear-prompt-image)
              (message "Pi: Prompt image cleared"))
          (let* ((path (read-file-name "Attach prompt image: " nil nil t))
                 (image (piem--read-prompt-image path)))
            (piem--set-prompt-image image)
            (message "Pi: Attached image %s"
                     (piem--prompt-image-name image))))))))

(defun piem--model-supports-image-input-p (chat-buffer)
  "Return non-nil only when CHAT-BUFFER's model advertises image input."
  (let* ((state (and (buffer-live-p chat-buffer)
                     (buffer-local-value 'piem--state chat-buffer)))
         (model (and (listp state) (plist-get state :model))))
    (condition-case nil
        (and (listp model)
             (plist-member model :input)
             (let ((input (plist-get model :input)))
               (and (or (vectorp input) (listp input))
                    (member "image" (if (vectorp input)
                                        (append input nil)
                                      input))
                    t)))
      (error nil))))

;;;; Sending Prompts

(defun piem--accept-input-text (text &optional prompt-image)
  "Accept TEXT from input buffer state, consuming optional PROMPT-IMAGE.
Adds only TEXT to history, resets history navigation, and clears input."
  (piem--history-add text)
  (setq piem--input-ring-index nil
        piem--input-saved nil)
  (when prompt-image
    (piem--clear-prompt-image))
  (erase-buffer))

(defun piem--queue-followup-text (chat-buf text)
  "Accept TEXT and enqueue it as a follow-up in CHAT-BUF."
  (piem--accept-input-text text)
  (with-current-buffer chat-buf
    (piem--push-followup text)))

(defun piem-send ()
  "Send the current input buffer contents to pi.
Clears the input buffer after sending.  Does nothing if buffer is empty.
If pi is busy (sending, streaming, or compacting), queues a local follow-up.
An attached image is accepted only with a direct, ordinary, idle prompt.
All built-in slash commands are handled locally; other slash commands are
sent to pi."
  (interactive)
  (let* ((text (string-trim (buffer-string)))
         (chat-buf (piem--get-chat-buffer))
         (prompt-image (piem--get-prompt-image))
         (transitioning (and chat-buf
                             (piem--session-transition-active-p
                              chat-buf)))
         (busy (and chat-buf (piem--session-busy-p chat-buf))))
    (cond
     ((string-empty-p text)
      (when prompt-image
        (message "Pi: Add prompt text before sending the attached image")))
     (transitioning
      (message "Pi: Cannot send while session is switching"))
     ((and prompt-image
           (piem--model-change-pending-p chat-buf))
      (message "Pi: Wait for the pending model change before sending an image"))
     ((and prompt-image busy)
      (message "Pi: Cannot send an attached image while Pi is busy"))
     ((and prompt-image (string-prefix-p "/" text))
      (message "Pi: Attached images cannot be sent with slash commands"))
     ((and prompt-image
           (not (piem--model-supports-image-input-p chat-buf)))
      (message "Pi: Current model does not support known image input"))
     ((and busy (piem--builtin-command-text-p text))
      (message "Pi: Cannot queue /%s while Pi is busy"
               (piem--builtin-command-name text)))
     (busy
      (piem--queue-followup-text chat-buf text)
      (piem--maybe-hide-input-window)
      (message "Pi: Message queued (will send when Pi is ready)"))
     (prompt-image
      (piem--accept-input-text text prompt-image)
      (piem--maybe-hide-input-window)
      (with-current-buffer chat-buf
        (piem--prepare-and-send text nil prompt-image)))
     (t
      (piem--accept-input-text text)
      (piem--maybe-hide-input-window)
      (with-current-buffer chat-buf
        (piem--prepare-and-send text))))))

(defun piem-abort ()
  "Abort the current pi operation.
Works while streaming or compacting."
  (interactive)
  (when-let* ((chat-buf (piem--get-chat-buffer)))
    (let ((status (buffer-local-value 'piem--status chat-buf)))
      (when (memq status '(streaming compacting))
        (when (eq status 'streaming)
          (with-current-buffer chat-buf
            (piem--set-aborted t)))
        (when-let* ((proc (piem--get-process)))
          (piem--rpc-async proc
                         (list :type "abort")
                         (lambda (_response)
                           (run-with-timer 2 nil (lambda () (message nil)))
                           (message "Pi: Aborted"))))))))

(defun piem-quit ()
  "Close the current pi session.
Kills both chat and input buffers, terminates the process,
and removes the input window (merging its space with adjacent windows).

If a process is running, asks for confirmation first unless
`piem-quit-without-confirmation' is non-nil.  If the user
cancels, the session remains intact."
  (interactive)
  (let* ((chat-buf (piem--get-chat-buffer))
         (input-buf (piem--get-input-buffer))
         (proc (when (buffer-live-p chat-buf)
                 (buffer-local-value 'piem--process chat-buf)))
         (proc-live (and proc (process-live-p proc)))
         (input-windows nil))
    (when (and (piem--process-kill-confirmation-required-p proc)
               (not (yes-or-no-p "Pi session has a running process; quit anyway? ")))
      (user-error "Quit cancelled"))
    ;; Suppress Emacs and pi buffer-kill prompts after explicit confirmation.
    (when proc-live
      (piem--skip-process-kill-confirmation proc)
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

(defun piem--command-capf ()
  "Completion-at-point function for /commands in input buffer.
Returns completion data when point is after / at start of buffer.
Includes both built-in commands and commands from pi's `get_commands' RPC."
  (when (and (eq (char-after (point-min)) ?/)
             (> (point) (point-min)))
    (let* ((start (1+ (point-min)))
           (end (point))
           (builtin-names (mapcar #'car piem--builtin-commands))
           (rpc-names (mapcar (lambda (cmd) (plist-get cmd :name))
                              piem--commands))
           (commands (delete-dups (append builtin-names rpc-names))))
      (list start end commands :exclusive 'no))))

;;;; Editor Features: File Reference (@)

(defun piem--at-trigger-p ()
  "Return non-nil if @ at point should trigger file completion.
Returns nil when @ follows an alphanumeric character (like in emails).
Assumes point is right after the @."
  (or (< (point) 3)  ; @ at buffer start or position 2 (no char before @)
      (save-excursion
        (backward-char 2)  ; Move to char before @
        (looking-at-p "[^[:alnum:]]"))))

(defun piem--maybe-complete-at ()
  "Trigger completion after @ if at word boundary.
Called from `post-self-insert-hook'.
Does not trigger when @ follows alphanumeric (e.g., in email addresses)."
  (when (and (eq last-command-event ?@)
             (piem--at-trigger-p))
    (run-at-time 0 nil #'piem--complete-file-reference)))

(defun piem--complete-file-reference ()
  "Complete file reference after @."
  (let* ((files (piem--get-project-files))
         (choice (completing-read "File: " files nil nil)))
    (when (and choice (not (string-empty-p choice)))
      (insert choice))))

(defvar-local piem--project-files-cache nil
  "Cached list of project files for @ completion.")

(defvar-local piem--project-files-cache-time nil
  "Time when project files cache was last updated.")

(defconst piem--project-files-cache-ttl 30
  "Seconds before project files cache expires.")

(defconst piem--file-exclude-patterns
  '(".git" "node_modules" ".elpa" "target" "build" "__pycache__" ".venv" "dist")
  "Directory names to exclude when listing files with find.")

(defun piem--get-project-files ()
  "Get list of project files, respecting .gitignore.
Uses cache if available and not expired."
  (let ((now (float-time)))
    (when (or (null piem--project-files-cache)
              (null piem--project-files-cache-time)
              (> (- now piem--project-files-cache-time)
                 piem--project-files-cache-ttl))
      (setq piem--project-files-cache
            (piem--list-project-files))
      (setq piem--project-files-cache-time now))
    piem--project-files-cache))

(defun piem--list-project-files ()
  "List project files using git ls-files or find.
Respects .gitignore when in a git repository."
  (let* ((dir (piem--session-directory))
         (default-directory dir))
    (condition-case nil
        (let ((output (shell-command-to-string
                       "git ls-files --cached --others --exclude-standard 2>/dev/null")))
          (if (string-empty-p output)
              (piem--list-files-with-find dir)
            (split-string output "\n" t)))
      (error (piem--list-files-with-find dir)))))

(defun piem--list-files-with-find (dir)
  "List files in DIR using find.
Excludes directories listed in `piem--file-exclude-patterns'."
  (let* ((default-directory dir)
         (prune-expr (mapconcat (lambda (p) (format "-name '%s'" p))
                                piem--file-exclude-patterns
                                " -o "))
         (cmd (format "find . \\( %s \\) -prune -o -type f -print 2>/dev/null | sed 's|^\\./||'"
                      prune-expr)))
    (split-string (shell-command-to-string cmd) "\n" t)))

(defun piem--file-reference-capf ()
  "Completion-at-point function for @file references.
Triggers when @ is typed, provides completion of project files."
  (when-let* ((at-pos (save-excursion
                        (when (search-backward "@" (line-beginning-position) t)
                          (point)))))
    (let* ((start (1+ at-pos))
           (end (point))
           (prefix (buffer-substring-no-properties start end))
           (files (piem--get-project-files))
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

(defun piem--path-prefix-p (path)
  "Check if PATH has a completable prefix (./, ../, ~/, or /)."
  (or (string-prefix-p "./" path)
      (string-prefix-p "../" path)
      (string-prefix-p "~/" path)
      (string-prefix-p "/" path)))

(defun piem--path-completions (path)
  "Return file completion candidates for PATH, or nil if directory invalid."
  (condition-case nil
      (let* ((dir (piem--route-preserving-file-name-directory path))
             (base (file-name-nondirectory path))
             (session-dir (piem--session-directory))
             (expanded-dir (if dir
                               (piem--route-preserving-file-name-as-directory
                                (piem--emacs-path dir session-dir))
                             session-dir)))
        (when (file-directory-p expanded-dir)
          (mapcar (lambda (f) (concat (or dir "") f))
                  (cl-remove-if (lambda (f) (member f '("." ".." "./" "../")))
                                (file-name-all-completions base expanded-dir)))))
    (error nil)))

(defun piem--path-capf ()
  "Completion-at-point function for file paths.
Completes paths starting with ./, ../, ~/, or /.
Skips / at buffer start to allow slash command completion."
  (when-let* ((bounds (bounds-of-thing-at-point 'filename))
              (start (car bounds))
              (end (cdr bounds))
              (path (buffer-substring-no-properties start end))
              ((piem--path-prefix-p path))
              ((not (and (string-prefix-p "/" path)
                         (= start (point-min)))))
              (candidates (piem--path-completions path)))
    (list start end candidates
          :exclusive 'no
          :annotation-function
          (lambda (c)
            (if (string-suffix-p "/" c) " (dir)" " (file)")))))

;;;; Editor Features: Message Queuing

(defun piem--send-steer-message (text)
  "Send TEXT as a steering message via RPC.
Returns t if message was sent, nil if process unavailable.
Shows error message if RPC fails."
  (let ((proc (piem--get-process)))
    (if (and proc (process-live-p proc))
        (progn
          (piem--rpc-async proc
                                      (list :type "steer" :message text)
                                      (lambda (response)
                                        (unless (eq (plist-get response :success) t)
                                          (message "Pi: Steering failed: %s"
                                                   (or (plist-get response :error) "unknown error")))))
          t)
      (message "Pi: Cannot send steering - process unavailable")
      nil)))

(defun piem-queue-steering ()
  "Send current input as a steering message.
When pi is sending or streaming, steering interrupts remaining tools.
Unlike normal sends, steering is NOT displayed locally - pi will echo
it back via message_start at the correct position (after current
assistant output completes).

When compaction is in progress, steering text is queued as a local
follow-up.  It is sent after non-retry compaction, or after Pi's
automatic overflow retry turn finishes.  Steering refuses a draft image."
  (interactive)
  (let ((text (string-trim (buffer-string))))
    (if (piem--get-prompt-image)
        (message "Pi: Cannot steer with an attached image")
      (unless (string-empty-p text)
        (let ((chat-buf (piem--get-chat-buffer)))
          (when chat-buf
            (let ((status (buffer-local-value 'piem--status chat-buf)))
              (cond
               ((piem--session-transition-active-p chat-buf)
                (message "Pi: Cannot send steering while session is switching"))
               ((and (eq status 'idle)
                     (not (piem--session-busy-p chat-buf)))
                (message "Pi: Nothing to interrupt - use C-c C-c to send"))
               ((or (eq status 'compacting)
                    (and (eq status 'idle)
                         (piem--session-busy-p chat-buf)))
                (piem--queue-followup-text chat-buf text)
                (message "Pi: Steering queued (will send when Pi is ready)"))
               ((memq status '(sending streaming))
                (when (piem--send-steer-message text)
                  (piem--accept-input-text text)
                  (message "Pi: Steering message sent")))
               (t
                (message "Pi: Cannot steer while session status is %s"
                         status))))))))))

(defun piem-queue-followup ()
  "Queue current input as a follow-up message.
Obsolete: Use `piem-send' (C-c C-c) instead, which now
automatically queues as follow-up when the agent is busy."
  (interactive)
  (piem-send))
(make-obsolete 'piem-queue-followup 'piem-send "1.3.0")

(provide 'piem-input)
;;; piem-input.el ends here
