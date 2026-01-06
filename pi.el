;;; pi.el --- Emacs frontend for pi coding agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pi.el
;; Keywords: ai llm ai-pair-programming tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (markdown-mode "2.6"))

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

;; Emacs frontend for the pi coding agent (https://shittycodingagent.ai/).
;; Provides a two-window interface for AI-assisted coding: chat history
;; with rendered markdown, and a separate prompt composition buffer.
;;
;; Requirements:
;;   - pi coding agent installed and in PATH
;;
;; Usage:
;;   M-x pi           Start a session in current project
;;   C-u M-x pi       Start a named session
;;
;; Key Bindings:
;;   Input buffer:
;;     C-c C-c        Send prompt
;;     C-c C-k        Abort streaming
;;     C-c C-p        Open menu
;;     C-c C-r        Resume session
;;     M-p / M-n      History navigation
;;     C-r            Search history
;;
;;   Chat buffer:
;;     n / p          Navigate messages
;;     TAB            Toggle tool output
;;     C-c C-p        Open menu
;;
;; Press C-c C-p for the full transient menu with model selection,
;; thinking level, session management, and custom commands.
;;
;; See README.org for more documentation.

;;; Code:

(require 'pi-core)
(require 'project)
(require 'markdown-mode)

;;;; Customization Group

(defgroup pi nil
  "Emacs frontend for pi coding agent."
  :group 'tools
  :prefix "pi-")

;;;; Customization

(defcustom pi-rpc-timeout 30
  "Default timeout in seconds for synchronous RPC calls.
Some operations like model loading may need more time."
  :type 'natnum
  :group 'pi)

(defcustom pi-input-window-height 10
  "Height of the input window in lines."
  :type 'natnum
  :group 'pi)

(defcustom pi-separator-width 72
  "Total width of section separators in chat buffer."
  :type 'natnum
  :group 'pi)

(defcustom pi-tool-preview-lines 10
  "Number of lines to show before collapsing tool output."
  :type 'natnum
  :group 'pi)

(defcustom pi-bash-preview-lines 5
  "Number of lines to show for bash output before collapsing.
Bash output is typically more verbose, so fewer lines are shown."
  :type 'natnum
  :group 'pi)

(defcustom pi-context-warning-threshold 70
  "Context usage percentage at which to show warning color."
  :type 'natnum
  :group 'pi)

(defcustom pi-context-error-threshold 90
  "Context usage percentage at which to show error color."
  :type 'natnum
  :group 'pi)

;;;; Faces

(defface pi-separator
  '((t :inherit font-lock-comment-face))
  "Face for section separators in pi chat."
  :group 'pi)

(defface pi-user-label
  '((t :inherit bold :foreground "dodger blue"))
  "Face for the You label in pi chat."
  :group 'pi)

(defface pi-timestamp
  '((t :inherit shadow))
  "Face for timestamps in message headers."
  :group 'pi)

(defface pi-assistant-label
  '((t :inherit bold :foreground "sea green"))
  "Face for the Assistant label in pi chat."
  :group 'pi)

(defface pi-compaction-label
  '((t :inherit bold :foreground "medium purple"))
  "Face for the Compaction label in pi chat."
  :group 'pi)

(defface pi-tool-name
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for tool names (BASH, READ, etc.) in pi chat."
  :group 'pi)

(defface pi-tool-command
  '((t :inherit font-lock-function-name-face))
  "Face for tool commands and arguments."
  :group 'pi)

(defface pi-tool-output
  '((t :inherit shadow))
  "Face for tool output text."
  :group 'pi)

(defface pi-tool-error
  '((t :inherit error))
  "Face for tool error indicators."
  :group 'pi)

(defface pi-tool-block-pending
  '((((class color) (min-colors 88) (background dark))
     :background "#2a2a35" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#f0f0f5" :extend t)
    (t :inherit secondary-selection :extend t))
  "Face for tool blocks during execution."
  :group 'pi)

(defface pi-tool-block-success
  '((((class color) (min-colors 88) (background dark))
     :background "#2a352a" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#f0f5f0" :extend t)
    (t :inherit secondary-selection :extend t))
  "Face for tool blocks after successful completion."
  :group 'pi)

(defface pi-tool-block-error
  '((((class color) (min-colors 88) (background dark))
     :background "#352a2a" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#f5f0f0" :extend t)
    (t :inherit secondary-selection :extend t))
  "Face for tool blocks after failed completion."
  :group 'pi)

(defface pi-collapsed-indicator
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for collapsed content indicators."
  :group 'pi)

(defface pi-model-name
  '((t :inherit font-lock-type-face))
  "Face for model name in header line."
  :group 'pi)

(defface pi-thinking
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for thinking/reasoning block content."
  :group 'pi)

(defface pi-retry-notice
  '((t :inherit warning :slant italic))
  "Face for retry notifications (rate limit, overloaded, etc.)."
  :group 'pi)

(defface pi-error-notice
  '((t :inherit error))
  "Face for error notifications from the server."
  :group 'pi)

;;;; Language Detection

(defconst pi--extension-language-alist
  '(("ts" . "typescript") ("tsx" . "typescript")
    ("js" . "javascript") ("jsx" . "javascript") ("mjs" . "javascript")
    ("py" . "python") ("pyw" . "python")
    ("rb" . "ruby") ("rake" . "ruby")
    ("rs" . "rust")
    ("go" . "go")
    ("el" . "emacs-lisp") ("lisp" . "lisp") ("cl" . "lisp")
    ("sh" . "bash") ("bash" . "bash") ("zsh" . "zsh")
    ("c" . "c") ("h" . "c")
    ("cpp" . "cpp") ("cc" . "cpp") ("cxx" . "cpp") ("hpp" . "cpp")
    ("java" . "java")
    ("kt" . "kotlin") ("kts" . "kotlin")
    ("swift" . "swift")
    ("cs" . "csharp")
    ("php" . "php")
    ("json" . "json")
    ("yaml" . "yaml") ("yml" . "yaml")
    ("toml" . "toml")
    ("xml" . "xml")
    ("html" . "html") ("htm" . "html")
    ("css" . "css") ("scss" . "scss") ("sass" . "sass")
    ("sql" . "sql")
    ("md" . "markdown")
    ("org" . "org")
    ("lua" . "lua")
    ("r" . "r") ("R" . "r")
    ("pl" . "perl") ("pm" . "perl")
    ("hs" . "haskell")
    ("ml" . "ocaml") ("mli" . "ocaml")
    ("ex" . "elixir") ("exs" . "elixir")
    ("erl" . "erlang")
    ("clj" . "clojure") ("cljs" . "clojure")
    ("scala" . "scala")
    ("vim" . "vim")
    ("dockerfile" . "dockerfile")
    ("makefile" . "makefile") ("mk" . "makefile"))
  "Alist mapping file extensions to language names for syntax highlighting.")

(defun pi--path-to-language (path)
  "Return language name for PATH based on file extension.
Returns nil if the extension is not recognized."
  (when path
    (let ((ext (downcase (or (file-name-extension path) ""))))
      (cdr (assoc ext pi--extension-language-alist)))))

;;;; Major Modes

(defvar pi-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'pi-quit)
    (define-key map (kbd "C-c C-p") #'pi-menu)
    (define-key map (kbd "n") #'pi-next-message)
    (define-key map (kbd "p") #'pi-previous-message)
    (define-key map (kbd "TAB") #'pi-toggle-tool-section)
    (define-key map (kbd "<tab>") #'pi-toggle-tool-section)
    map)
  "Keymap for `pi-chat-mode'.")

(defun pi-next-message ()
  "Move to the next user message in the chat buffer."
  (interactive)
  (let ((pos (save-excursion
               (forward-line 1)
               (re-search-forward "^You:" nil t))))
    (if pos
        (progn
          (goto-char pos)
          (beginning-of-line))
      (message "No more messages"))))

(defun pi-previous-message ()
  "Move to the previous user message in the chat buffer."
  (interactive)
  (let ((pos (save-excursion
               (beginning-of-line)
               (re-search-backward "^You:" nil t))))
    (if pos
        (goto-char pos)
      (message "No previous message"))))

(defconst pi--thinking-block-regexp
  "^```thinking\n\\(\\(?:.*\n\\)*?\\)```"
  "Regexp matching thinking blocks for font-lock.")

(define-derived-mode pi-chat-mode gfm-mode "Pi-Chat"
  "Major mode for displaying pi conversation.
Derives from `gfm-mode' for syntax highlighting of code blocks.
This is a read-only buffer showing the conversation history."
  :group 'pi
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local markdown-fontify-code-blocks-natively t)
  ;; Hide markdown markup (**, `, ```) for cleaner display
  (setq-local markdown-hide-markup t)
  (add-to-invisibility-spec 'markdown-markup)
  (setq-local pi--tool-args-cache (make-hash-table :test 'equal))
  ;; Make window-point follow inserted text (like comint does).
  ;; This is key for natural scroll behavior during streaming.
  (setq-local window-point-insertion-type t)

  ;; Apply pi-thinking face to thinking block content
  (font-lock-add-keywords
   nil
   `((,pi--thinking-block-regexp 1 'pi-thinking append))
   'append)

  (add-hook 'kill-buffer-query-functions #'pi--kill-buffer-query nil t)
  (add-hook 'kill-buffer-hook #'pi--cleanup-on-kill nil t))

(defvar pi-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pi-send)
    (define-key map (kbd "TAB") #'completion-at-point)
    (define-key map (kbd "C-c C-k") #'pi-abort)
    (define-key map (kbd "C-c C-p") #'pi-menu)
    (define-key map (kbd "C-c C-r") #'pi-resume-session)
    (define-key map (kbd "M-p") #'pi-previous-input)
    (define-key map (kbd "M-n") #'pi-next-input)
    (define-key map (kbd "<C-up>") #'pi-previous-input)
    (define-key map (kbd "<C-down>") #'pi-next-input)
    (define-key map (kbd "C-r") #'pi-history-search)
    map)
  "Keymap for `pi-input-mode'.")

;;;; Input History (comint/eshell style)

(require 'ring)

(defvar pi--input-ring-size 100
  "Size of the input history ring.")

(defvar pi--input-ring nil
  "Ring holding input history.  Shared across all pi sessions.")

(defvar-local pi--input-ring-index nil
  "Current position in input ring, or nil if not navigating history.")

(defvar-local pi--input-saved nil
  "Saved input before starting history navigation.")

(defun pi--input-ring ()
  "Return the input ring, creating if necessary."
  (unless pi--input-ring
    (setq pi--input-ring (make-ring pi--input-ring-size)))
  pi--input-ring)

(defun pi--history-add (input)
  "Add INPUT to history ring if non-empty and different from last."
  (let ((ring (pi--input-ring))
        (trimmed (and input (string-trim input))))
    (when (and trimmed
               (not (string-empty-p trimmed))
               (or (ring-empty-p ring)
                   (not (string= input (ring-ref ring 0)))))
      (ring-insert ring input))))

(defun pi-previous-input ()
  "Cycle backwards through input history.
Saves current input before first navigation."
  (interactive)
  (let ((ring (pi--input-ring)))
    (when (ring-empty-p ring)
      (user-error "No history"))
    ;; Save current input on first navigation
    (unless pi--input-ring-index
      (setq pi--input-saved (buffer-string)))
    ;; Calculate new index
    (let ((new-index (if pi--input-ring-index
                         (1+ pi--input-ring-index)
                       0)))
      (if (>= new-index (ring-length ring))
          (user-error "Beginning of history")
        (setq pi--input-ring-index new-index)
        (delete-region (point-min) (point-max))
        (insert (ring-ref ring new-index))))))

(defun pi-next-input ()
  "Cycle forwards through input history.
Restores saved input when moving past newest entry."
  (interactive)
  (unless pi--input-ring-index
    (user-error "End of history"))
  (let ((new-index (1- pi--input-ring-index)))
    (delete-region (point-min) (point-max))
    (if (< new-index 0)
        ;; Restore saved input
        (progn
          (setq pi--input-ring-index nil)
          (when pi--input-saved
            (insert pi--input-saved)))
      ;; Show history entry
      (setq pi--input-ring-index new-index)
      (insert (ring-ref (pi--input-ring) new-index)))))

(defun pi-history-search ()
  "Search input history with completion."
  (interactive)
  (let* ((ring (pi--input-ring))
         (entries (when (not (ring-empty-p ring))
                    (ring-elements ring)))
         (choice (completing-read "History: " entries nil t)))
    (when (and choice (not (string-empty-p choice)))
      (delete-region (point-min) (point-max))
      (insert choice)
      ;; Reset history navigation state
      (setq pi--input-ring-index nil
            pi--input-saved nil))))

(define-derived-mode pi-input-mode text-mode "Pi-Input"
  "Major mode for composing pi prompts."
  :group 'pi
  (setq-local header-line-format '(:eval (pi--header-line-string)))
  (add-hook 'completion-at-point-functions #'pi--slash-capf nil t)
  (add-hook 'kill-buffer-query-functions #'pi--kill-buffer-query nil t)
  (add-hook 'kill-buffer-hook #'pi--cleanup-input-on-kill nil t))

;;;; Session Directory Detection

(defun pi--session-directory ()
  "Determine directory for pi session.
Uses project root if available, otherwise `default-directory'.
Always returns an expanded absolute path (no ~ abbreviation)."
  (expand-file-name
   (or (when-let ((proj (project-current)))
         (project-root proj))
       default-directory)))

;;;; Buffer Naming & Creation

(defun pi--buffer-name (type dir &optional session)
  "Generate buffer name for TYPE (:chat or :input) in DIR.
Optional SESSION name creates a named session.
Uses abbreviated directory for readability in buffer lists."
  (let ((type-str (pcase type
                    (:chat "chat")
                    (:input "input")))
        (abbrev-dir (abbreviate-file-name dir)))
    (if (and session (not (string-empty-p session)))
        (format "*pi-%s:%s<%s>*" type-str abbrev-dir session)
      (format "*pi-%s:%s*" type-str abbrev-dir))))

(defun pi--find-session (dir &optional session)
  "Find existing chat buffer for DIR and SESSION.
Returns the chat buffer or nil if not found."
  (get-buffer (pi--buffer-name :chat dir session)))

(defun pi--get-or-create-buffer (type dir &optional session)
  "Get or create buffer of TYPE for DIR and optional SESSION.
TYPE is :chat or :input.  Returns the buffer."
  (let* ((name (pi--buffer-name type dir session))
         (existing (get-buffer name)))
    (if existing
        existing
      (let ((buf (generate-new-buffer name)))
        (with-current-buffer buf
          (pcase type
            (:chat (pi-chat-mode))
            (:input (pi-input-mode))))
        buf))))

;;;; Buffer-Local Session Variables

(defvar-local pi--process nil
  "The pi RPC subprocess for this session.")

(defvar-local pi--chat-buffer nil
  "Reference to the chat buffer for this session.")

(defvar-local pi--input-buffer nil
  "Reference to the input buffer for this session.")

(defvar-local pi--streaming-marker nil
  "Marker for current streaming insertion point.")

;; pi--status is defined in pi-core.el as the single source of truth
;; for session activity state (idle, sending, streaming, compacting)

(defvar-local pi--cached-stats nil
  "Cached session statistics for header-line display.
Updated after each agent turn completes.")

(defvar-local pi--last-usage nil
  "Usage from last assistant message for context percentage.
This is the per-turn usage, not cumulative - used to calculate
how much of the context window was used in the last turn.")

(defvar-local pi--aborted nil
  "Non-nil if the current/last request was aborted.")

(defvar-local pi--message-start-marker nil
  "Marker for start of current message content.
Used to replace raw markdown with rendered Org on message completion.")

(defvar-local pi--tool-args-cache nil
  "Hash table mapping toolCallId to args.
Needed because tool_execution_end events don't include args.")

(defvar-local pi--pending-tool-overlay nil
  "Overlay for tool block currently being executed.
Set by `pi--display-tool-start', used by `pi--display-tool-end'.")

(defvar-local pi--assistant-header-shown nil
  "Non-nil if Assistant header has been shown for current prompt.
Used to avoid duplicate headers during retry sequences.")

;;;; Buffer Navigation

(defun pi--get-chat-buffer ()
  "Get the chat buffer for the current session.
Works from either chat or input buffer."
  (if (derived-mode-p 'pi-chat-mode)
      (current-buffer)
    pi--chat-buffer))

(defun pi--get-input-buffer ()
  "Get the input buffer for the current session.
Works from either chat or input buffer."
  (if (derived-mode-p 'pi-input-mode)
      (current-buffer)
    pi--input-buffer))

(defun pi--get-process ()
  "Get the pi process for the current session.
Works from either chat or input buffer."
  (if (derived-mode-p 'pi-chat-mode)
      pi--process
    (and pi--chat-buffer
         (buffer-local-value 'pi--process pi--chat-buffer))))

;;;; Display

(defun pi--display-buffers (chat-buf input-buf)
  "Display CHAT-BUF and INPUT-BUF in current window, split vertically.
Does not affect other windows in the frame."
  ;; Use current window for chat, split for input
  (switch-to-buffer chat-buf)
  (with-current-buffer chat-buf
    (goto-char (point-max)))
  (let ((input-win (split-window nil (- pi-input-window-height) 'below)))
    (set-window-buffer input-win input-buf)
    (select-window input-win)))

;;;; Response Display

;;; Scroll Behavior
;;
;; During streaming, windows "following" output (window-point at buffer end)
;; scroll to show new content. Windows where the user scrolled up stay put.
;;
;; Key mechanism: `window-point-insertion-type' is set to t in pi-chat-mode,
;; making window-point move with inserted text. We track which windows are
;; following before each insert, then restore point for non-following windows
;; afterward. Emacs naturally scrolls to keep point visible.

(defun pi--window-following-p (window)
  "Return non-nil if WINDOW is following output (point at end of buffer)."
  (>= (window-point window) (1- (point-max))))

(defmacro pi--with-scroll-preservation (&rest body)
  "Execute BODY preserving scroll for windows not following output.
Windows at buffer end will scroll to show new content.
Windows where user scrolled up stay in place."
  (declare (indent 0) (debug t))
  `(let* ((windows (get-buffer-window-list (current-buffer) nil t))
          (following (cl-remove-if-not #'pi--window-following-p windows))
          (saved-points (mapcar (lambda (w) (cons w (window-point w)))
                                (cl-remove-if #'pi--window-following-p windows))))
     ,@body
     ;; Restore point for non-following windows
     (dolist (pair saved-points)
       (when (window-live-p (car pair))
         (set-window-point (car pair) (cdr pair))))
     ;; Move following windows to new end
     (dolist (win following)
       (when (window-live-p win)
         (set-window-point win (point-max))))))

(defun pi--append-to-chat (text)
  "Append TEXT to the chat buffer.
Windows following the output (point at end) will scroll to show new text.
Windows where user scrolled up (point earlier) stay in place."
  (let ((inhibit-read-only t))
    (pi--with-scroll-preservation
      (save-excursion
        (goto-char (point-max))
        (insert text)))))

(defun pi--make-separator (label face &optional timestamp)
  "Create a separator line with LABEL styled with FACE.
If TIMESTAMP (Emacs time value) is provided, display it right-aligned.
The label stays centered; timestamp eats into the right dashes.
Returns a decorated separator line with centered label."
  (let* ((label-str (concat " " label " "))
         (total-width (- pi-separator-width 2))  ; Account for "* " prefix
         (label-len (length label-str))
         ;; Calculate symmetric dash count (same for both sides without timestamp)
         (dash-count (max 4 (/ (- total-width label-len) 2)))
         (left-dashes (make-string dash-count ?─)))
    (if timestamp
        ;; With timestamp: label stays centered, timestamp eats into right dashes
        (let* ((timestamp-str (pi--format-message-timestamp timestamp))
               (timestamp-len (length timestamp-str))
               ;; Right dashes reduced by timestamp length, keep at least 1 trailing dash
               (right-dash-count (max 1 (- dash-count timestamp-len 1)))
               (right-dashes (make-string right-dash-count ?─))
               (decorated (concat (propertize left-dashes 'face 'pi-separator)
                                  (propertize label-str 'face face)
                                  (propertize right-dashes 'face 'pi-separator)
                                  (propertize timestamp-str 'face 'pi-timestamp)
                                  (propertize "─" 'face 'pi-separator))))
          (concat "* " decorated))
      ;; Without timestamp: symmetric dashes around label
      (let* ((right-dashes (make-string dash-count ?─))
             (decorated (concat (propertize left-dashes 'face 'pi-separator)
                                (propertize label-str 'face face)
                                (propertize right-dashes 'face 'pi-separator))))
        (concat "* " decorated)))))

(defun pi--display-user-message (text &optional timestamp)
  "Display user message TEXT in the chat buffer.
If TIMESTAMP (Emacs time value) is provided, display it in the header."
  (pi--append-to-chat
   (concat "\n" (pi--make-separator "You" 'pi-user-label timestamp) "\n\n"
           text "\n")))

(defun pi--display-agent-start ()
  "Display separator for new agent turn.
Only shows the Assistant header once per prompt, even during retries.
Note: `pi--status' is set to `streaming' by `pi--update-state-from-event'."
  (setq pi--aborted nil)  ; Reset abort flag for new turn
  ;; Only show header if not already shown for this prompt
  (unless pi--assistant-header-shown
    (pi--append-to-chat
     (concat "\n" (pi--make-separator "Assistant" 'pi-assistant-label) "\n\n"))
    (setq pi--assistant-header-shown t))
  ;; Create markers at current end position
  ;; message-start-marker: where content begins (for later replacement)
  ;; streaming-marker: where new deltas are inserted
  (setq pi--message-start-marker (copy-marker (point-max) nil))
  (setq pi--streaming-marker (copy-marker (point-max) t))
  (pi--spinner-start)
  (force-mode-line-update))

(defun pi--display-message-delta (delta)
  "Display streaming message DELTA at the streaming marker."
  (when (and delta pi--streaming-marker)
    (let ((inhibit-read-only t))
      (pi--with-scroll-preservation
        (save-excursion
          (goto-char (marker-position pi--streaming-marker))
          (insert delta)
          (set-marker pi--streaming-marker (point)))))))

(defun pi--display-thinking-start ()
  "Insert opening marker for thinking block."
  (when pi--streaming-marker
    (let ((inhibit-read-only t))
      (pi--with-scroll-preservation
        (save-excursion
          (goto-char (marker-position pi--streaming-marker))
          (insert "```thinking\n")
          (set-marker pi--streaming-marker (point)))))))

(defun pi--display-thinking-delta (delta)
  "Display streaming thinking DELTA at the streaming marker."
  (when (and delta pi--streaming-marker)
    (let ((inhibit-read-only t))
      (pi--with-scroll-preservation
        (save-excursion
          (goto-char (marker-position pi--streaming-marker))
          (insert delta)
          (set-marker pi--streaming-marker (point)))))))

(defun pi--display-thinking-end (_content)
  "Insert closing marker for thinking block.
CONTENT is ignored - we use what was already streamed."
  (when pi--streaming-marker
    (let ((inhibit-read-only t))
      (pi--with-scroll-preservation
        (save-excursion
          (goto-char (marker-position pi--streaming-marker))
          (insert "\n```\n\n")
          (set-marker pi--streaming-marker (point)))))))

(defun pi--display-agent-end ()
  "Display end of agent turn.
Note: `pi--status' is set to `idle' by `pi--update-state-from-event'."
  (let ((inhibit-read-only t))
    ;; Show abort indicator if aborted
    (when pi--aborted
      (save-excursion
        (goto-char (point-max))
        ;; Remove trailing whitespace before adding indicator
        (skip-chars-backward " \t\n")
        (delete-region (point) (point-max))
        (insert "\n\n" (propertize "[Aborted]" 'face 'error) "\n"))
      (setq pi--aborted nil))
    ;; Add spacing for next turn, avoiding excess blank lines
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (delete-region (point) (point-max))
      (insert "\n\n")))
  (pi--spinner-stop)
  (pi--refresh-header))

(defun pi--display-retry-start (event)
  "Display retry notice from auto_retry_start EVENT.
Shows attempt number, delay, and raw error message."
  (let* ((attempt (plist-get event :attempt))
         (max-attempts (plist-get event :maxAttempts))
         (delay-ms (plist-get event :delayMs))
         (error-msg (or (plist-get event :errorMessage) "transient error"))
         (delay-sec (/ (or delay-ms 0) 1000.0))
         (notice (format "⟳ Retry %d/%d in %.0fs — %s"
                         (or attempt 1)
                         (or max-attempts 3)
                         delay-sec
                         error-msg)))
    (pi--append-to-chat
     (concat (propertize notice 'face 'pi-retry-notice) "\n"))))

(defun pi--display-retry-end (event)
  "Display retry result from auto_retry_end EVENT.
Shows success or final failure with raw error."
  (let* ((success (plist-get event :success))
         (attempt (plist-get event :attempt))
         (final-error (or (plist-get event :finalError) "unknown error")))
    (if (eq success t)
        (pi--append-to-chat
         (concat (propertize (format "✓ Retry succeeded on attempt %d"
                                     (or attempt 1))
                             'face 'pi-retry-notice)
                 "\n\n"))
      ;; Final failure
      (pi--append-to-chat
       (concat (propertize (format "✗ Retry failed after %d attempts — %s"
                                   (or attempt 1)
                                   final-error)
                           'face 'pi-error-notice)
               "\n\n")))))

(defun pi--display-error (error-msg)
  "Display ERROR-MSG from the server."
  (pi--append-to-chat
   (concat "\n" (propertize (format "[Error: %s]" (or error-msg "unknown"))
                            'face 'pi-error-notice)
           "\n")))

(defun pi--display-hook-error (event)
  "Display hook error from hook_error EVENT."
  (let* ((hook-path (plist-get event :hookPath))
         (hook-event (plist-get event :event))
         (error-msg (plist-get event :error))
         (hook-name (if hook-path (file-name-nondirectory hook-path) "unknown")))
    (pi--append-to-chat
     (concat "\n"
             (propertize (format "[Hook error in %s (%s): %s]"
                                 hook-name
                                 (or hook-event "unknown")
                                 (or error-msg "unknown error"))
                         'face 'pi-error-notice)
             "\n"))))

(defun pi--display-no-model-warning ()
  "Display warning when no model is available.
Shown when the session starts without a configured model/API key."
  (pi--append-to-chat
   (concat "\n"
           (propertize "⚠ No models available"
                       'face 'pi-error-notice)
           "\n\n"
           (propertize "To get started, either:\n"
                       'face 'pi-retry-notice)
           (propertize "  • Set an API key: "
                       'face 'pi-retry-notice)
           "ANTHROPIC_API_KEY, OPENAI_API_KEY, GEMINI_API_KEY, etc.\n"
           (propertize "  • Or run "
                       'face 'pi-retry-notice)
           (propertize "pi --login"
                       'face 'pi-tool-command)
           (propertize " in a terminal to authenticate via OAuth\n"
                       'face 'pi-retry-notice)
           "\n")))

(defun pi--kill-buffer-query ()
  "Query function for killing pi buffers.
Prompts for confirmation if streaming is in progress."
  (cond
   ;; Chat buffer during streaming - confirm
   ((and (derived-mode-p 'pi-chat-mode)
         (eq pi--status 'streaming))
    (yes-or-no-p "Pi is streaming. Kill session? "))
   ;; Input buffer during streaming - confirm
   ((and (derived-mode-p 'pi-input-mode)
         pi--chat-buffer
         (buffer-live-p pi--chat-buffer)
         (eq (buffer-local-value 'pi--status pi--chat-buffer) 'streaming))
    (yes-or-no-p "Pi is streaming. Kill session? "))
   ;; Otherwise allow
   (t t)))

(defun pi--cleanup-on-kill ()
  "Clean up resources when chat buffer is killed.
Also kills the linked input buffer."
  (when (derived-mode-p 'pi-chat-mode)
    ;; Kill linked input buffer
    (when (and pi--input-buffer (buffer-live-p pi--input-buffer))
      (let ((input-buf pi--input-buffer))
        ;; Clear reference to avoid infinite recursion
        (setq pi--input-buffer nil)
        (kill-buffer input-buf)))
    ;; Clean up process
    (when pi--process
      (pi--unregister-display-handler pi--process)
      (when (process-live-p pi--process)
        (delete-process pi--process)))))

(defun pi--cleanup-input-on-kill ()
  "Clean up when input buffer is killed.
Also kills the linked chat buffer (which handles process cleanup)."
  (when (derived-mode-p 'pi-input-mode)
    (when (and pi--chat-buffer (buffer-live-p pi--chat-buffer))
      (let ((chat-buf pi--chat-buffer))
        ;; Clear reference to avoid infinite recursion
        (setq pi--chat-buffer nil)
        (kill-buffer chat-buf)))))

(defun pi--register-display-handler (process)
  "Register display event handler for PROCESS."
  (let ((handler (pi--make-display-handler process)))
    (process-put process 'pi-display-handler handler)))

(defun pi--unregister-display-handler (process)
  "Unregister display event handler for PROCESS."
  (process-put process 'pi-display-handler nil))

(defun pi--make-display-handler (process)
  "Create a display event handler for PROCESS."
  (lambda (event)
    (when-let ((chat-buf (process-get process 'pi-chat-buffer)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (pi--handle-display-event event))))))

(defun pi--handle-display-event (event)
  "Handle EVENT for display purposes.
Updates buffer-local state and renders display updates."
  ;; Update state first (now buffer-local)
  (pi--update-state-from-event event)
  ;; Then handle display
  (pcase (plist-get event :type)
    ("agent_start"
     (pi--display-agent-start))
    ("message_start"
     ;; Reset markers for each new message to avoid clobbering tool output
     (setq pi--message-start-marker (copy-marker (point-max) nil))
     (setq pi--streaming-marker (copy-marker (point-max) t)))
    ("message_update"
     (when-let* ((msg-event (plist-get event :assistantMessageEvent))
                 (event-type (plist-get msg-event :type)))
       (pcase event-type
         ("text_delta"
          (pi--display-message-delta (plist-get msg-event :delta)))
         ("thinking_start"
          (pi--display-thinking-start))
         ("thinking_delta"
          (pi--display-thinking-delta (plist-get msg-event :delta)))
         ("thinking_end"
          (pi--display-thinking-end (plist-get msg-event :content)))
         ("error"
          ;; Error during streaming (e.g., API error)
          (pi--display-error (plist-get msg-event :reason))))))
    ("message_end"
     (let ((message (plist-get event :message)))
       ;; Display error if message ended with error (e.g., API error)
       (when (equal (plist-get message :stopReason) "error")
         (pi--display-error (plist-get message :errorMessage)))
       ;; Capture usage from assistant messages for context % calculation.
       ;; Skip aborted messages - they may have incomplete usage data and
       ;; would reset context percentage to 0%.  Matches TUI footer.ts behavior.
       ;; Note: error messages DO have valid usage data (tokens were consumed).
       (when (and (equal (plist-get message :role) "assistant")
                  (not (equal (plist-get message :stopReason) "aborted"))
                  (plist-get message :usage))
         (setq pi--last-usage (plist-get message :usage))))
     (pi--render-complete-message))
    ("tool_execution_start"
     (let ((tool-call-id (plist-get event :toolCallId))
           (args (plist-get event :args)))
       ;; Cache args for tool_execution_end (which doesn't include args)
       (when (and tool-call-id pi--tool-args-cache)
         (puthash tool-call-id args pi--tool-args-cache))
       (pi--display-tool-start (plist-get event :toolName) args)))
    ("tool_execution_end"
     (let* ((tool-call-id (plist-get event :toolCallId))
            (result (plist-get event :result))
            ;; Retrieve cached args since tool_execution_end doesn't include them
            (args (when (and tool-call-id pi--tool-args-cache)
                    (prog1 (gethash tool-call-id pi--tool-args-cache)
                      (remhash tool-call-id pi--tool-args-cache)))))
       (pi--display-tool-end (plist-get event :toolName)
                             args
                             (plist-get result :content)
                             (plist-get result :details)
                             (plist-get event :isError))))
    ("auto_compaction_start"
     (setq pi--status 'compacting)
     (pi--spinner-start)
     (let ((reason (plist-get event :reason)))
       (message "Pi: %sAuto-compacting... (C-c C-k to cancel)"
                (if (equal reason "overflow") "Context overflow, " ""))))
    ("auto_compaction_end"
     (pi--spinner-stop)
     (setq pi--status 'idle)
     (if (plist-get event :aborted)
         (message "Pi: Auto-compaction cancelled")
       (when-let ((result (plist-get event :result)))
         (pi--handle-compaction-success
          (plist-get result :tokensBefore)
          (plist-get result :summary)
          (pi--ms-to-time (plist-get result :timestamp))))))
    ("agent_end"
     (pi--display-agent-end))
    ("auto_retry_start"
     (pi--display-retry-start event))
    ("auto_retry_end"
     (pi--display-retry-end event))
    ("hook_error"
     (pi--display-hook-error event))))

;;;; Sending Prompts

(defun pi-send ()
  "Send the current input buffer contents to pi.
Clears the input buffer after sending.  Does nothing if buffer is empty.
If text starts with /, tries to expand as a custom slash command.
If pi is currently streaming, shows a message and preserves input."
  (interactive)
  (let* ((text (string-trim (buffer-string)))
         (expanded (pi--expand-slash-command text))
         (chat-buf (pi--get-chat-buffer))
         (streaming (and chat-buf
                         (buffer-local-value 'pi--status chat-buf)
                         (memq (buffer-local-value 'pi--status chat-buf)
                               '(streaming sending)))))
    (cond
     ;; Empty input - do nothing
     ((string-empty-p text) nil)
     ;; Streaming - block send, preserve input
     (streaming
      (message "Pi: Please wait for response to complete"))
     ;; Normal send
     (t
      ;; Add to history and reset navigation state
      (pi--history-add text)
      (setq pi--input-ring-index nil
            pi--input-saved nil)
      (erase-buffer)
      (with-current-buffer chat-buf
        (pi--display-user-message text (current-time))
        (setq pi--status 'sending)
        (setq pi--assistant-header-shown nil)  ; Reset for new prompt
        (pi--spinner-start)
        (force-mode-line-update))
      (pi--send-prompt expanded)))))

(defun pi--send-prompt (text)
  "Send TEXT as a prompt to the pi process.
Shows an error message if process is unavailable."
  (let ((proc (pi--get-process)))
    (cond
     ((null proc)
      (message "Pi: No process available - try M-x pi to restart"))
     ((not (process-live-p proc))
      (message "Pi: Process died - try M-x pi to restart"))
     (t
      (pi--rpc-async proc
                     (list :type "prompt" :message text)
                     #'ignore)))))

(defun pi-abort ()
  "Abort the current pi operation.
Only works when streaming is in progress."
  (interactive)
  (when-let ((chat-buf (pi--get-chat-buffer)))
    ;; Check pi--status in chat buffer (it's updated by agent_start/agent_end)
    (when (eq (buffer-local-value 'pi--status chat-buf) 'streaming)
      ;; Set aborted flag in chat buffer (where agent-end will check it)
      (with-current-buffer chat-buf
        (setq pi--aborted t))
      (when-let ((proc (pi--get-process)))
        (pi--rpc-async proc
                       (list :type "abort")
                       (lambda (_response)
                         (run-with-timer 2 nil (lambda () (message nil)))
                         (message "Pi: Aborted")))))))

(defun pi-quit ()
  "Close the current pi session.
Kills both chat and input buffers, terminates the process,
and removes the input window (merging its space with adjacent windows)."
  (interactive)
  (let ((chat-buf (pi--get-chat-buffer))
        (input-buf (pi--get-input-buffer))
        (input-windows nil))
    ;; Collect windows showing the input buffer (we'll delete these)
    (when (buffer-live-p input-buf)
      (setq input-windows (get-buffer-window-list input-buf nil t)))
    ;; Kill buffers (this also cleans up process via hooks)
    (when (buffer-live-p input-buf)
      (kill-buffer input-buf))
    (when (buffer-live-p chat-buf)
      (kill-buffer chat-buf))
    ;; Delete input windows only (merges space with neighbors)
    (dolist (win input-windows)
      (when (window-live-p win)
        (ignore-errors (delete-window win))))))

(defun pi--align-tables-in-region (start end)
  "Align all markdown tables between START and END."
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (re-search-forward "^|" end t))
      (when (markdown-table-at-point-p)
        (markdown-table-align)))))

(defun pi--render-complete-message ()
  "Finalize completed message by applying font-lock and aligning tables.
Uses pi--message-start-marker and pi--streaming-marker to find content.
Markdown stays as-is; gfm-mode handles highlighting and markup hiding.
Ensures message ends with newline for proper spacing."
  (when (and pi--message-start-marker pi--streaming-marker)
    (let ((start (marker-position pi--message-start-marker))
          (end (marker-position pi--streaming-marker)))
      (when (< start end)
        ;; Ensure trailing newline (messages should end with newline)
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char end)
            (unless (eq (char-before) ?\n)
              (insert "\n")
              (set-marker pi--streaming-marker (point))))
          ;; Align any markdown tables in the message
          (pi--align-tables-in-region start (marker-position pi--streaming-marker)))
        (font-lock-ensure start (marker-position pi--streaming-marker))))))

;;;; Tool Output

(defsubst pi--tool-path (args)
  "Extract file path from tool ARGS.
Checks both :path and :file_path keys for compatibility."
  (or (plist-get args :path)
      (plist-get args :file_path)))

(defun pi--tool-overlay-create (tool-name)
  "Create overlay for tool block TOOL-NAME at point.
Returns the overlay.  The overlay uses rear-advance so it
automatically extends when content is inserted at its end."
  (let ((ov (make-overlay (point) (point) nil nil t)))
    (overlay-put ov 'pi-tool-block t)
    (overlay-put ov 'pi-tool-name tool-name)
    (overlay-put ov 'face 'pi-tool-block-pending)
    ov))

(defun pi--display-tool-start (tool-name args)
  "Display header for tool TOOL-NAME with ARGS and create overlay."
  (let* ((header (pcase tool-name
                   ("bash" (format "$ %s" (or (plist-get args :command) "...")))
                   ("read" (format "read %s" (or (pi--tool-path args) "...")))
                   ("write" (format "write %s" (or (pi--tool-path args) "...")))
                   ("edit" (format "edit %s" (or (pi--tool-path args) "...")))
                   (_ tool-name)))
         (header-display (propertize header 'face 'pi-tool-command))
         (inhibit-read-only t))
    (pi--with-scroll-preservation
      (save-excursion
        (goto-char (point-max))
        ;; Add blank line before tool if previous line has content
        (unless (save-excursion
                  (forward-line -1)
                  (looking-at-p "^$"))
          (insert "\n"))
        ;; Create overlay at start of tool block
        (setq pi--pending-tool-overlay (pi--tool-overlay-create tool-name))
        (insert header-display "\n")))))

(defun pi--wrap-in-src-block (content lang)
  "Wrap CONTENT in a markdown fenced code block with LANG.
Returns markdown string for syntax highlighting."
  (format "```%s\n%s\n```" (or lang "") content))

(defun pi--render-tool-content (content lang)
  "Render CONTENT with optional syntax highlighting for LANG.
If LANG is non-nil, wraps in markdown code fence.
Returns the rendered string (markdown code block or plain text)."
  (if lang
      (pi--wrap-in-src-block content lang)
    ;; No language - return plain text with face
    (propertize content 'face 'pi-tool-output)))

(defun pi--display-tool-end (tool-name args content details is-error)
  "Display result for TOOL-NAME and update overlay face.
ARGS contains tool arguments, CONTENT is a list of content blocks.
DETAILS contains tool-specific data (e.g., diff for edit tool).
IS-ERROR indicates failure.
Shows preview lines with expandable toggle for long output."
  (let* ((is-error (eq t is-error))
         (text-blocks (seq-filter (lambda (c) (equal (plist-get c :type) "text"))
                                  content))
         (raw-output (mapconcat (lambda (c) (or (plist-get c :text) ""))
                                text-blocks "\n"))
         ;; Determine language for syntax highlighting
         (lang (pcase tool-name
                 ("edit" (when (and (not is-error)
                                    (plist-get details :diff))
                           "diff"))
                 ((or "read" "write")
                  (pi--path-to-language (pi--tool-path args)))
                 ("bash" "text")  ; wrap in fence for visual consistency
                 (_ nil)))
         ;; For edit with diff, use diff from details
         ;; For write, use content from args (result is just success message)
         (display-content (pcase tool-name
                            ("edit" (or (plist-get details :diff) raw-output))
                            ("write" (or (plist-get args :content) raw-output))
                            (_ raw-output)))
         (lines (split-string display-content "\n" t))
         (total-lines (length lines))
         (preview-limit (pcase tool-name
                          ("bash" pi-bash-preview-lines)
                          (_ pi-tool-preview-lines)))
         (needs-collapse (> total-lines preview-limit))
         (inhibit-read-only t))
    (pi--with-scroll-preservation
      (goto-char (point-max))
      (if needs-collapse
          ;; Long output: show preview + toggle button
          (let* ((preview-lines (seq-take lines preview-limit))
                 (preview-content (string-join preview-lines "\n"))
                 (full-content display-content)
                 (hidden-count (- total-lines preview-limit))
                 ;; Render preview with syntax highlighting
                 (rendered-preview (pi--render-tool-content preview-content lang)))
            (insert rendered-preview "\n")
            ;; Insert toggle button
            (insert-text-button
             (propertize (format "... (%d more lines)" hidden-count)
                         'face 'pi-collapsed-indicator)
             'action #'pi--toggle-tool-output
             'follow-link t
             'pi-full-content full-content
             'pi-preview-content preview-content
             'pi-lang lang
             'pi-expanded nil
             'hidden-count hidden-count)
            (insert "\n"))
        ;; Short output: show all with syntax highlighting
        (let ((rendered (pi--render-tool-content display-content lang)))
          (insert rendered "\n")))
      ;; Error indicator
      (when is-error
        (insert (propertize "[error]" 'face 'pi-tool-error) "\n"))
      ;; Update overlay face and finalize bounds
      (when pi--pending-tool-overlay
        (overlay-put pi--pending-tool-overlay 'face
                     (if is-error 'pi-tool-block-error 'pi-tool-block-success))
        ;; Fix overlay end to prevent extension to subsequent content.
        ;; The overlay has rear-advance=t (needed during streaming), so we
        ;; must explicitly set the end AFTER inserting the trailing newline,
        ;; then shrink it back to exclude the newline.
        (insert "\n")
        (move-overlay pi--pending-tool-overlay
                      (overlay-start pi--pending-tool-overlay)
                      (1- (point)))
        (setq pi--pending-tool-overlay nil)))))

(defun pi--toggle-tool-output (button)
  "Toggle between preview and full content for BUTTON."
  (let* ((inhibit-read-only t)
         (expanded (button-get button 'pi-expanded))
         (full-content (button-get button 'pi-full-content))
         (preview-content (button-get button 'pi-preview-content))
         (lang (button-get button 'pi-lang))
         (hidden-count (button-get button 'hidden-count))
         (btn-start (button-start button))
         (btn-end (button-end button)))
    (save-excursion
      ;; Find the tool overlay
      (goto-char btn-start)
      (when-let ((bounds (pi--find-tool-block-bounds)))
        ;; Content starts after first line (header)
        (goto-char (car bounds))
        (forward-line 1)
        (let ((content-start (point)))
          ;; Delete from content start to after button
          (delete-region content-start (1+ btn-end))
          (goto-char content-start)
          (if expanded
              (pi--insert-collapsed-content preview-content full-content lang hidden-count)
            (pi--insert-expanded-content preview-content full-content lang hidden-count)))))))

(defun pi--insert-collapsed-content (preview-content full-content lang hidden-count)
  "Insert PREVIEW-CONTENT with toggle button.
FULL-CONTENT is stored for expansion.  LANG is for syntax highlighting.
HIDDEN-COUNT shows lines remaining."
  (let ((rendered (pi--render-tool-content preview-content lang)))
    (insert rendered "\n"))
  (insert-text-button
   (propertize (format "... (%d more lines)" hidden-count) 'face 'pi-collapsed-indicator)
   'action #'pi--toggle-tool-output
   'follow-link t
   'pi-full-content full-content
   'pi-preview-content preview-content
   'pi-lang lang
   'pi-expanded nil
   'hidden-count hidden-count)
  (insert "\n"))

(defun pi--insert-expanded-content (preview-content full-content lang hidden-count)
  "Insert FULL-CONTENT with collapse button.
PREVIEW-CONTENT, LANG and HIDDEN-COUNT are stored for collapsing."
  (let ((rendered (pi--render-tool-content full-content lang)))
    (insert rendered "\n"))
  (insert-text-button
   (propertize "[-]" 'face 'pi-collapsed-indicator)
   'action #'pi--toggle-tool-output
   'follow-link t
   'pi-full-content full-content
   'pi-preview-content preview-content
   'pi-lang lang
   'pi-expanded t
   'hidden-count hidden-count)
  (insert "\n"))

(defun pi--find-tool-block-bounds ()
  "Find the bounds of the tool block at point.
Returns (START . END) if inside a tool block, nil otherwise."
  (let ((overlays (overlays-at (point))))
    (when-let ((ov (seq-find (lambda (o) (overlay-get o 'pi-tool-block)) overlays)))
      (cons (overlay-start ov) (overlay-end ov)))))

(defun pi--find-toggle-button-in-region (start end)
  "Find a toggle button between START and END."
  (save-excursion
    (goto-char start)
    (let ((found nil))
      (while (and (not found) (< (point) end))
        (let ((btn (button-at (point))))
          (if (and btn (button-get btn 'pi-full-content))
              (setq found btn)
            (forward-char 1))))
      found)))

(defun pi-toggle-tool-section ()
  "Toggle the tool output section at point.
Works anywhere inside a tool block overlay."
  (interactive)
  (let ((original-pos (point)))
    (if-let ((bounds (pi--find-tool-block-bounds)))
        (if-let ((btn (pi--find-toggle-button-in-region (car bounds) (cdr bounds))))
            (progn
              (pi--toggle-tool-output btn)
              ;; Try to restore position, clamped to new block bounds
              (when-let ((new-bounds (pi--find-tool-block-bounds)))
                (goto-char (min original-pos (cdr new-bounds)))))
          ;; No button found - short output, use markdown-cycle
          (markdown-cycle))
      ;; Not in a tool block
      (markdown-cycle))))

;;;; Compaction Display

(defun pi--display-compaction-result (tokens-before summary &optional timestamp)
  "Display a compaction result block in the chat buffer.
TOKENS-BEFORE is the token count before compaction.
SUMMARY is the compaction summary text (markdown).
TIMESTAMP is optional time when compaction occurred."
  (pi--append-to-chat
   (concat "\n" (pi--make-separator "Compaction" 'pi-compaction-label timestamp) "\n\n"
           (propertize (format "Compacted from %s tokens\n\n"
                               (pi--format-number (or tokens-before 0)))
                       'face 'pi-tool-name)
           (or summary "") "\n")))

(defun pi--handle-compaction-success (tokens-before summary &optional timestamp)
  "Handle successful compaction: display result, reset state, notify user.
TOKENS-BEFORE is the pre-compaction token count.
SUMMARY is the compaction summary text.
TIMESTAMP is optional time when compaction occurred."
  (pi--display-compaction-result tokens-before summary timestamp)
  (setq pi--last-usage nil)
  (pi--refresh-header)
  (message "Pi: Compacted from %s tokens" (pi--format-number (or tokens-before 0))))

;;;; Dependency Checking

(defun pi--check-pi ()
  "Check if pi binary is available.
Returns t if available, nil otherwise."
  (and (executable-find "pi") t))

(defun pi--check-dependencies ()
  "Check all required dependencies.
Displays warnings for missing dependencies."
  (unless (pi--check-pi)
    (display-warning 'pi "pi binary not found. Install with: npm install -g @mariozechner/pi-coding-agent"
                     :error)))

;;;; Startup Header

(defconst pi-version "0.1.0"
  "Version of pi.el.")

(defun pi--get-pi-version ()
  "Get pi CLI version by running `pi --version'."
  (condition-case nil
      (string-trim (shell-command-to-string "pi --version"))
    (error "Unknown")))

(defun pi--format-startup-header ()
  "Format the startup header string with styled separator."
  (let* ((pi-cli-version (pi--get-pi-version))
         (separator (pi--make-separator (format "Pi %s" pi-cli-version)
                                        'pi-assistant-label)))
    (concat
     separator "\n"
     "C-c C-c   send prompt\n"
     "C-c C-k   abort\n"
     "C-c C-r   resume session\n"
     "C-c C-p   menu\n")))

(defun pi--display-startup-header ()
  "Display the startup header in the chat buffer."
  (pi--append-to-chat (pi--format-startup-header)))

;;;; Header Line

(defun pi--format-tokens-compact (n)
  "Format token count N compactly (e.g., 50k, 1.2M)."
  (cond
   ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
   ((>= n 1000) (format "%.0fk" (/ n 1000.0)))
   (t (number-to-string n))))

(defun pi--shorten-model-name (name)
  "Shorten model NAME for display.
Removes common prefixes like \"Claude \" and suffixes like \" (latest)\"."
  (thread-last name
    (replace-regexp-in-string "^[Cc]laude " "")
    (replace-regexp-in-string " (latest)$" "")
    (replace-regexp-in-string "^claude-" "")))

(defvar pi--spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Frames for the busy spinner animation.")

(defvar pi--spinner-index 0
  "Current frame index in the spinner animation (shared for sync).")

(defvar pi--spinner-timer nil
  "Timer for animating spinners (shared across sessions).")

(defvar pi--spinning-sessions nil
  "List of chat buffers currently spinning.")

(defun pi--spinner-start ()
  "Start the spinner for current session."
  (let ((chat-buf (pi--get-chat-buffer)))
    (when (and chat-buf (not (memq chat-buf pi--spinning-sessions)))
      (push chat-buf pi--spinning-sessions)
      ;; Start global timer if not running
      (unless pi--spinner-timer
        (setq pi--spinner-index 0)
        (setq pi--spinner-timer
              (run-with-timer 0 0.1 #'pi--spinner-tick))))))

(defun pi--spinner-stop ()
  "Stop the spinner for current session."
  (let ((chat-buf (pi--get-chat-buffer)))
    (setq pi--spinning-sessions (delq chat-buf pi--spinning-sessions))
    ;; Stop global timer if no sessions spinning
    (when (and pi--spinner-timer (null pi--spinning-sessions))
      (cancel-timer pi--spinner-timer)
      (setq pi--spinner-timer nil))))

(defun pi--spinner-tick ()
  "Advance spinner to next frame and update spinning sessions."
  (setq pi--spinner-index
        (mod (1+ pi--spinner-index) (length pi--spinner-frames)))
  ;; Only update windows showing spinning sessions
  (dolist (buf pi--spinning-sessions)
    (when (buffer-live-p buf)
      (let ((input-buf (buffer-local-value 'pi--input-buffer buf)))
        ;; Update input buffer's header line (where spinner shows)
        (when (and input-buf (buffer-live-p input-buf))
          (dolist (win (get-buffer-window-list input-buf nil t))
            (with-selected-window win
              (force-mode-line-update))))))))

(defun pi--spinner-current ()
  "Return current spinner frame if this session is spinning."
  (let ((chat-buf (cond
                   ((derived-mode-p 'pi-chat-mode) (current-buffer))
                   ((derived-mode-p 'pi-input-mode) pi--chat-buffer)
                   (t nil))))
    (when (and chat-buf (memq chat-buf pi--spinning-sessions))
      (aref pi--spinner-frames pi--spinner-index))))

(defvar pi--header-model-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'pi-select-model)
    (define-key map [header-line mouse-2] #'pi-select-model)
    map)
  "Keymap for clicking model name in header-line.")

(defvar pi--header-thinking-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'pi-cycle-thinking)
    (define-key map [header-line mouse-2] #'pi-cycle-thinking)
    map)
  "Keymap for clicking thinking level in header-line.")

(defun pi--header-format-context (context-tokens context-window)
  "Format context usage as percentage with color coding.
CONTEXT-TOKENS is the tokens used, CONTEXT-WINDOW is the max.
Returns nil if CONTEXT-WINDOW is 0."
  (when (> context-window 0)
    (let* ((pct (* (/ (float context-tokens) context-window) 100))
           ;; Note: %% needed because % has special meaning in header-line-format
           (pct-str (format " %.1f%%%%/%s" pct
                            (pi--format-tokens-compact context-window))))
      (propertize pct-str
                  'face (cond
                         ((> pct pi-context-error-threshold) 'error)
                         ((> pct pi-context-warning-threshold) 'warning)
                         (t nil))))))

(defun pi--header-format-stats (stats last-usage model-obj)
  "Format session STATS for header line.
LAST-USAGE is the most recent message's token usage.
MODEL-OBJ contains model info including context window.
Returns nil if STATS is nil."
  (when stats
    (let* ((tokens (plist-get stats :tokens))
           (input (or (plist-get tokens :input) 0))
           (output (or (plist-get tokens :output) 0))
           (cache-read (or (plist-get tokens :cacheRead) 0))
           (cache-write (or (plist-get tokens :cacheWrite) 0))
           (cost (or (plist-get stats :cost) 0))
           ;; Context percentage from LAST message usage, not cumulative totals.
           (last-input (or (plist-get last-usage :input) 0))
           (last-output (or (plist-get last-usage :output) 0))
           (last-cache-read (or (plist-get last-usage :cacheRead) 0))
           (last-cache-write (or (plist-get last-usage :cacheWrite) 0))
           (context-tokens (+ last-input last-output last-cache-read last-cache-write))
           (context-window (or (plist-get model-obj :contextWindow) 0)))
      (concat
       " │ ↑" (pi--format-tokens-compact input)
       " ↓" (pi--format-tokens-compact output)
       " R" (pi--format-tokens-compact cache-read)
       " W" (pi--format-tokens-compact cache-write)
       (format " $%.2f" cost)
       (pi--header-format-context context-tokens context-window)))))

(defun pi--header-line-string ()
  "Return formatted header-line string for input buffer.
Accesses state from the linked chat buffer."
  (let* ((chat-buf (cond
                    ;; In input buffer with valid link to chat
                    ((and pi--chat-buffer (buffer-live-p pi--chat-buffer))
                     pi--chat-buffer)
                    ;; In chat buffer itself
                    ((derived-mode-p 'pi-chat-mode)
                     (current-buffer))
                    ;; No valid chat buffer yet
                    (t nil)))
         (state (and chat-buf (buffer-local-value 'pi--state chat-buf)))
         (stats (and chat-buf (buffer-local-value 'pi--cached-stats chat-buf)))
         (last-usage (and chat-buf (buffer-local-value 'pi--last-usage chat-buf)))
         (model-obj (plist-get state :model))
         (model-name (cond
                      ((stringp model-obj) model-obj)
                      ((plist-get model-obj :name))
                      (t "")))
         (model-short (if (string-empty-p model-name) "..."
                        (pi--shorten-model-name model-name)))
         (thinking (or (plist-get state :thinking-level) ""))
         (status-str (if-let ((spinner (pi--spinner-current)))
                         (concat " " spinner)
                       "  ")))  ; Same width as " ⠋" to prevent jumping
    (concat
     ;; Model (clickable)
     (propertize model-short
                 'face 'pi-model-name
                 'mouse-face 'highlight
                 'help-echo "mouse-1: Select model"
                 'local-map pi--header-model-map)
     ;; Thinking level (clickable)
     (unless (string-empty-p thinking)
       (concat " • "
               (propertize thinking
                           'mouse-face 'highlight
                           'help-echo "mouse-1: Cycle thinking level"
                           'local-map pi--header-thinking-map)))
     status-str
     ;; Stats (if available)
     (pi--header-format-stats stats last-usage model-obj))))

(defun pi--refresh-header ()
  "Refresh header-line by fetching and caching session stats."
  (when-let ((proc (pi--get-process))
             (chat-buf (pi--get-chat-buffer)))
    (let ((input-buf (buffer-local-value 'pi--input-buffer chat-buf)))
      (pi--rpc-async proc '(:type "get_session_stats")
                     (lambda (response)
                       (when (plist-get response :success)
                         (when (buffer-live-p chat-buf)
                           (with-current-buffer chat-buf
                             (setq pi--cached-stats (plist-get response :data))))
                         ;; Update the input buffer's header line
                         (when (buffer-live-p input-buf)
                           (dolist (win (get-buffer-window-list input-buf nil t))
                             (with-selected-window win
                               (force-mode-line-update))))))))))

;;;; Slash Command Discovery

(defun pi--parse-frontmatter (content)
  "Parse YAML frontmatter from CONTENT.
Returns plist with :description and :content keys."
  (if (not (string-prefix-p "---" content))
      (list :description nil :content content)
    (let ((end-index (string-match "\n---" content 3)))
      (if (not end-index)
          (list :description nil :content content)
        (let* ((frontmatter-block (substring content 4 end-index))
               (remaining-content (string-trim (substring content (+ end-index 4))))
               (description nil))
          ;; Simple YAML parsing - just key: value pairs
          (dolist (line (split-string frontmatter-block "\n"))
            (when (string-match "^description:\\s-*\\(.*\\)$" line)
              (setq description (string-trim (match-string 1 line)))))
          (list :description description :content remaining-content))))))

(defun pi--load-commands-from-dir (dir source)
  "Load slash commands from DIR.
SOURCE is \"user\" or \"project\" for display purposes.
Returns list of plists with :name, :description, :content, :source.
Silently skips unreadable files - this is expected behavior for
optional command files that may not exist or may have permission issues."
  (when (file-directory-p dir)
    (let ((commands '()))
      (dolist (file (directory-files dir t "\\.md$"))
        (when (file-regular-p file)
          ;; Silently skip files that can't be read - they're optional
          (condition-case nil
              (let* ((raw-content (with-temp-buffer
                                    (insert-file-contents file)
                                    (buffer-string)))
                     (parsed (pi--parse-frontmatter raw-content))
                     (name (file-name-base file))
                     (content (plist-get parsed :content))
                     (description (plist-get parsed :description)))
                ;; If no description in frontmatter, use first line
                (unless (and description (not (string-empty-p description)))
                  (let ((first-line (car (split-string content "\n" t))))
                    (when first-line
                      (setq description
                            (if (> (length first-line) 60)
                                (concat (substring first-line 0 60) "...")
                              first-line)))))
                ;; Append source to description
                (let ((source-str (format "(%s)" source)))
                  (setq description
                        (if description
                            (concat description " " source-str)
                          source-str)))
                (push (list :name name
                            :description description
                            :content content
                            :source source)
                      commands))
            (error nil))))
      (nreverse commands))))

(defvar pi--file-commands nil
  "Cached list of discovered file-based slash commands.")

(defun pi--discover-file-commands (dir)
  "Discover file-based slash commands for session in DIR.
Searches ~/.pi/agent/commands/ and DIR/.pi/commands/.
Returns list of command plists."
  (let ((user-dir (expand-file-name "~/.pi/agent/commands/"))
        (project-dir (expand-file-name ".pi/commands/" dir)))
    (append (pi--load-commands-from-dir user-dir "user")
            (pi--load-commands-from-dir project-dir "project"))))

;;;; Slash Command Argument Handling

(defun pi--parse-command-args (args-string)
  "Parse ARGS-STRING into a list of arguments.
Uses shell-like quoting rules: double and single quotes preserve spaces,
backslash escapes characters.  Returns a list of argument strings,
or nil for empty/whitespace-only input."
  (let ((result (split-string-shell-command args-string)))
    (if (equal result '("")) nil result)))

(defun pi--substitute-args (content args)
  "Substitute argument placeholders in CONTENT with ARGS.
Replaces $1, $2, etc. with positional arguments.
Replaces $@ with all arguments joined by spaces."
  (let ((result content))
    ;; Replace $@ with all args joined
    (setq result (replace-regexp-in-string "\\$@" (string-join args " ") result t t))
    ;; Replace $1, $2, etc. with positional args
    (setq result (replace-regexp-in-string
                  "\\$\\([0-9]+\\)"
                  (lambda (match)
                    (let* ((num (string-to-number (match-string 1 match)))
                           (index (1- num)))
                      (or (nth index args) "")))
                  result t))
    result))

;;;; Transient Menu

(require 'transient)

(defun pi--menu-model-description ()
  "Return model description for transient menu."
  (let ((model (plist-get (plist-get pi--state :model) :name)))
    (format "Model: %s" (or model "unknown"))))

(defun pi--menu-thinking-description ()
  "Return thinking level description for transient menu."
  (let ((level (plist-get pi--state :thinking-level)))
    (format "Thinking: %s" (or level "off"))))

(defun pi-new-session ()
  "Start a new pi session (reset)."
  (interactive)
  (when-let ((proc (pi--get-process)))
    (pi--rpc-async proc '(:type "reset")
                   (lambda (response)
                     (let* ((data (plist-get response :data))
                            (cancelled (plist-get data :cancelled)))
                       (if (and (plist-get response :success)
                                (pi--json-false-p cancelled))
                           (progn
                             (pi--clear-chat-buffer)
                             (pi--refresh-header)
                             (message "Pi: New session started"))
                         (message "Pi: New session cancelled")))))))

(defun pi--session-dir-name (dir)
  "Convert DIR to session directory name.
Matches pi's encoding: --path-with-dashes--.
Note: Handles both Unix and Windows path separators."
  (let* ((clean-dir (directory-file-name dir))  ; Remove trailing slash
         (safe-path (replace-regexp-in-string "[/\\\\:]" "-"
                                              (replace-regexp-in-string "^[/\\\\]" "" clean-dir))))
    (concat "--" safe-path "--")))

(defun pi--session-metadata (path)
  "Extract metadata from session file PATH.
Returns plist (:modified-time :first-message :message-count) or nil on error."
  (condition-case nil
      (let* ((attrs (file-attributes path))
             (modified-time (file-attribute-modification-time attrs)))
        (with-temp-buffer
          (insert-file-contents path)
          (let* ((line-count (count-lines (point-min) (point-max)))
                 ;; Get first line (session header)
                 (session-json (progn
                                 (goto-char (point-min))
                                 (buffer-substring-no-properties
                                  (point) (line-end-position))))
                 ;; Get second line (first message) if it exists
                 (first-msg-json (progn
                                   (forward-line 1)
                                   (unless (eobp)
                                     (buffer-substring-no-properties
                                      (point) (line-end-position))))))
            ;; Need at least the session header
            (when (and session-json (not (string-empty-p session-json)))
              (let* ((first-message
                      (when (and first-msg-json (not (string-empty-p first-msg-json)))
                        (let* ((msg-data (json-parse-string first-msg-json :object-type 'plist))
                               (message (plist-get msg-data :message))
                               (content (plist-get message :content)))
                          (when (and content (vectorp content) (> (length content) 0))
                            (plist-get (aref content 0) :text))))))
                (list :modified-time modified-time
                      :first-message first-message
                      :message-count (max 0 (1- line-count))))))))
    (error nil)))

(defun pi--ms-to-time (ms)
  "Convert milliseconds MS to Emacs time value.
Returns nil if MS is nil."
  (and ms (seconds-to-time (/ ms 1000.0))))

(defun pi--format-relative-time (time)
  "Format TIME (Emacs time value) as relative time string."
  (condition-case nil
      (let* ((now (current-time))
             (diff (float-time (time-subtract now time)))
             (minutes (/ diff 60))
             (hours (/ diff 3600))
             (days (/ diff 86400)))
        (cond
         ((< minutes 1) "just now")
         ((< minutes 60) (format "%d min ago" (floor minutes)))
         ((< hours 24) (format "%d hr ago" (floor hours)))
         ((< days 7) (format "%d days ago" (floor days)))
         (t (format-time-string "%b %d" time))))
    (error "Unknown time format")))

(defun pi--format-message-timestamp (time)
  "Format TIME for message headers.
Shows HH:MM if today, otherwise YYYY-MM-DD HH:MM."
  (let* ((time-day (format-time-string "%Y-%m-%d" time))
         (today (format-time-string "%Y-%m-%d" (current-time))))
    (if (string= time-day today)
        (format-time-string "%H:%M" time)
      (format-time-string "%Y-%m-%d %H:%M" time))))

(defun pi--truncate-string (str max-len)
  "Truncate STR to MAX-LEN chars, adding ellipsis if needed."
  (if (and str (> (length str) max-len))
      (concat (substring str 0 (- max-len 1)) "…")
    str))

(defun pi--list-sessions (dir)
  "List available session files for project DIR.
Returns list of absolute paths to .jsonl files, sorted newest first."
  (let* ((sessions-base (expand-file-name "~/.pi/agent/sessions/"))
         (session-dir (expand-file-name (pi--session-dir-name dir) sessions-base)))
    (when (file-directory-p session-dir)
      ;; Sort by filename descending (timestamps sort lexicographically)
      (sort (directory-files session-dir t "\\.jsonl$")
            (lambda (a b)
              (string> (file-name-nondirectory a)
                       (file-name-nondirectory b)))))))

(defun pi--format-session-choice (path)
  "Format session PATH for display in selector.
Returns (display-string . path) for `completing-read'."
  (let ((metadata (pi--session-metadata path)))
    (if metadata
        (let* ((modified-time (plist-get metadata :modified-time))
               (first-msg (plist-get metadata :first-message))
               (msg-count (plist-get metadata :message-count))
               (relative-time (pi--format-relative-time modified-time))
               (preview (pi--truncate-string first-msg 50))
               (display (if preview
                            (format "%s · %s (%d msgs)"
                                    preview relative-time msg-count)
                          (format "[empty session] · %s" relative-time))))
          (cons display path))
      ;; Fallback to filename if metadata extraction fails
      (let ((filename (file-name-nondirectory path)))
        (cons filename path)))))

(defun pi--extract-message-text (message)
  "Extract plain text content from MESSAGE.
MESSAGE is a plist with :role and :content.
Returns concatenated text from all text blocks."
  (let* ((content (plist-get message :content))
         (texts '()))
    (when (vectorp content)
      (dotimes (i (length content))
        (let* ((block (aref content i))
               (block-type (plist-get block :type)))
          (when (equal block-type "text")
            (push (plist-get block :text) texts)))))
    (string-join (nreverse texts) "")))

(defun pi--count-tool-calls (message)
  "Count the number of tool call blocks in assistant MESSAGE."
  (let* ((content (plist-get message :content))
         (count 0))
    (when (vectorp content)
      (dotimes (i (length content))
        (let* ((block (aref content i))
               (block-type (plist-get block :type)))
          (when (equal block-type "toolCall")
            (setq count (1+ count))))))
    count))

(defun pi--render-history-text (text)
  "Render TEXT as markdown content with proper isolation.
Ensures markdown structures don't leak to subsequent content."
  (when (and text (not (string-empty-p text)))
    (let ((start (with-current-buffer (pi--get-chat-buffer) (point-max))))
      (pi--append-to-chat text)
      (with-current-buffer (pi--get-chat-buffer)
        (font-lock-ensure start (point-max)))
      ;; Ensure we end with newlines to reset markdown context
      ;; Two newlines ends any list/paragraph context
      (pi--append-to-chat "\n\n"))))

(defun pi--display-history-messages (messages)
  "Display MESSAGES from session history with smart grouping.
Consecutive assistant messages are grouped under one header.
Tool calls are accumulated and shown as a single summary per group.
Each text block is rendered independently for proper formatting."
  (let ((prev-role nil)
        (pending-tool-count 0))
    ;; Helper to flush tool count summary
    (cl-flet ((flush-tools ()
                (when (> pending-tool-count 0)
                  (pi--append-to-chat
                   (concat (propertize (format "[%d tool call%s]"
                                               pending-tool-count
                                               (if (= pending-tool-count 1) "" "s"))
                                       'face 'pi-tool-name)
                           "\n\n"))
                  (setq pending-tool-count 0))))
      ;; Process each message
      (dotimes (i (length messages))
        (let* ((message (aref messages i))
               (role (plist-get message :role)))
          (pcase role
            ("user"
             ;; Flush any pending tool count
             (flush-tools)
             ;; Show user message with blank line after header
             (let* ((text (pi--extract-message-text message))
                    (timestamp (pi--ms-to-time (plist-get message :timestamp))))
               (when (and text (not (string-empty-p text)))
                 (pi--append-to-chat
                  (concat "\n" (pi--make-separator "You" 'pi-user-label timestamp) "\n\n"
                          text "\n"))))
             (setq prev-role "user"))
            ("assistant"
             ;; Show header only on transition from user
             (when (not (equal prev-role "assistant"))
               (flush-tools)
               (pi--append-to-chat
                (concat "\n" (pi--make-separator "Assistant" 'pi-assistant-label) "\n\n")))
             ;; Render text immediately (isolated)
             (let ((text (pi--extract-message-text message))
                   (tool-count (pi--count-tool-calls message)))
               (when (and text (not (string-empty-p text)))
                 (pi--render-history-text text))
               (setq pending-tool-count (+ pending-tool-count tool-count)))
             (setq prev-role "assistant"))
            ("compactionSummary"
             ;; Show compaction with header, tokens, and summary
             (flush-tools)
             (let* ((summary (plist-get message :summary))
                    (tokens-before (plist-get message :tokensBefore))
                    (timestamp (pi--ms-to-time (plist-get message :timestamp))))
               (pi--display-compaction-result tokens-before summary timestamp))
             (setq prev-role "compactionSummary"))
            ;; Skip toolResult - already counted tool calls in assistant message
            ("toolResult"
             nil))))
      ;; Flush any remaining tool count
      (flush-tools))))

(defun pi--clear-chat-buffer ()
  "Clear the chat buffer and display fresh startup header.
Used when starting a new session."
  (when-let ((chat-buf (pi--get-chat-buffer)))
    (with-current-buffer chat-buf
      (let ((inhibit-read-only t))
        ;; Clear buffer
        (erase-buffer)
        ;; Show startup header
        (insert (pi--format-startup-header))
        (insert "\n")
        ;; Reset markers
        (setq pi--message-start-marker nil)
        (setq pi--streaming-marker nil)
        ;; Reset usage (context % will show 0% until next message)
        (setq pi--last-usage nil)
        ;; Clear tool args cache
        (clrhash pi--tool-args-cache)
        ;; Position at end
        (goto-char (point-max))))))

(defun pi--display-session-history (messages)
  "Display session history MESSAGES in the chat buffer.
MESSAGES is a vector of message plists from get_messages RPC."
  (when-let ((chat-buf (pi--get-chat-buffer)))
    (with-current-buffer chat-buf
      (let ((inhibit-read-only t))
        ;; Clear buffer
        (erase-buffer)
        ;; Show startup header
        (insert (pi--format-startup-header))
        (insert "\n")
        ;; Render messages with smart grouping
        (when (vectorp messages)
          (pi--display-history-messages messages))
        ;; Ensure we end with proper spacing
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        ;; Reset markers for future streaming
        (setq pi--message-start-marker nil)
        (setq pi--streaming-marker nil)
        ;; Scroll to end
        (goto-char (point-max))))))

(defun pi--load-session-history (proc callback)
  "Load and display session history from PROC.
Calls CALLBACK with message count when done."
  (pi--rpc-async proc '(:type "get_messages")
                 (lambda (response)
                   (when (plist-get response :success)
                     (let* ((messages (plist-get (plist-get response :data) :messages))
                            (count (if (vectorp messages) (length messages) 0)))
                       (pi--display-session-history messages)
                       ;; Update header with new session info
                       (when-let ((chat-buf (pi--get-chat-buffer)))
                         (with-current-buffer chat-buf
                           (pi--refresh-header)))
                       (when callback
                         (funcall callback count)))))))

(defun pi-resume-session ()
  "Resume a previous pi session from the current project."
  (interactive)
  (when-let ((proc (pi--get-process))
             (dir (pi--session-directory)))
    (let ((sessions (pi--list-sessions dir)))
      (if (null sessions)
          (message "Pi: No previous sessions found")
        (let* ((choices (mapcar #'pi--format-session-choice sessions))
               (choice (completing-read "Resume session: "
                                        (mapcar #'car choices)
                                        nil t))
               (selected-path (cdr (assoc choice choices))))
          (when selected-path
            (pi--rpc-async proc (list :type "switch_session"
                                      :sessionPath selected-path)
                           (lambda (response)
                             (let* ((data (plist-get response :data))
                                    (cancelled (plist-get data :cancelled)))
                               (if (and (plist-get response :success)
                                        (pi--json-false-p cancelled))
                                   (pi--load-session-history
                                    proc
                                    (lambda (count)
                                      (message "Pi: Resumed session (%d messages)" count)))
                                 (message "Pi: Failed to resume session")))))))))))

(defun pi-select-model ()
  "Select a model interactively."
  (interactive)
  (let ((proc (pi--get-process))
        (chat-buf (pi--get-chat-buffer)))
    (unless proc
      (user-error "No pi process running"))
    (let* ((response (pi--rpc-sync proc '(:type "get_available_models") 5))
           (data (plist-get response :data))
           (models (plist-get data :models))
           (current-name (plist-get (plist-get pi--state :model) :name))
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
          (pi--rpc-async proc (list :type "set_model"
                                    :provider provider
                                    :modelId model-id)
                         (lambda (resp)
                           (when (and (plist-get resp :success)
                                      (buffer-live-p chat-buf))
                             (with-current-buffer chat-buf
                               (pi--update-state-from-response resp)
                               (force-mode-line-update))
                             (message "Pi: Model set to %s" choice)))))))))

(defun pi-cycle-thinking ()
  "Cycle through thinking levels."
  (interactive)
  (when-let ((proc (pi--get-process))
             (chat-buf (pi--get-chat-buffer)))
    (pi--rpc-async proc '(:type "cycle_thinking_level")
                   (lambda (response)
                     (when (and (plist-get response :success)
                                (buffer-live-p chat-buf))
                       (with-current-buffer chat-buf
                         (pi--update-state-from-response response)
                         (force-mode-line-update)
                         (message "Pi: Thinking level: %s"
                                  (plist-get pi--state :thinking-level))))))))

(defun pi--format-number (n)
  "Format number N with thousands separators."
  (let ((str (number-to-string n)))
    (replace-regexp-in-string
     "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\([^0-9]\\|$\\)"
     "\\1,\\2\\3"
     (replace-regexp-in-string
      "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\([0-9]\\{3\\}\\)\\([^0-9]\\|$\\)"
      "\\1,\\2,\\3\\4" str))))

(defun pi--format-session-stats (stats)
  "Format STATS plist as human-readable string."
  (let* ((tokens (plist-get stats :tokens))
         (input (or (plist-get tokens :input) 0))
         (output (or (plist-get tokens :output) 0))
         (total (or (plist-get tokens :total) 0))
         (cost (or (plist-get stats :cost) 0))
         (messages (or (plist-get stats :userMessages) 0))
         (tools (or (plist-get stats :toolCalls) 0)))
    (format "Tokens: %s in / %s out (%s total) | Cost: $%.2f | Messages: %d | Tools: %d"
            (pi--format-number input)
            (pi--format-number output)
            (pi--format-number total)
            cost messages tools)))

(defun pi-session-stats ()
  "Display session statistics in the echo area."
  (interactive)
  (when-let ((proc (pi--get-process)))
    (pi--rpc-async proc '(:type "get_session_stats")
                   (lambda (response)
                     (if (plist-get response :success)
                         (let ((data (plist-get response :data)))
                           (message "Pi: %s" (pi--format-session-stats data)))
                       (message "Pi: Failed to get session stats"))))))

(defun pi-compact ()
  "Compact conversation context to reduce token usage."
  (interactive)
  (when-let ((proc (pi--get-process))
             (chat-buf (pi--get-chat-buffer)))
    (message "Pi: Compacting...")
    (pi--spinner-start)
    (pi--rpc-async proc '(:type "compact")
                   (lambda (response)
                     (pi--spinner-stop)
                     (if (plist-get response :success)
                         (when (buffer-live-p chat-buf)
                           (with-current-buffer chat-buf
                             (let ((data (plist-get response :data)))
                               (pi--handle-compaction-success
                                (plist-get data :tokensBefore)
                                (plist-get data :summary)
                                (current-time)))))
                       (message "Pi: Compact failed"))))))

(defun pi-export-html ()
  "Export session to HTML file."
  (interactive)
  (when-let ((proc (pi--get-process)))
    (pi--rpc-async proc '(:type "export_html")
                   (lambda (response)
                     (if (plist-get response :success)
                         (let* ((data (plist-get response :data))
                                (path (plist-get data :path)))
                           (message "Pi: Exported to %s" path))
                       (message "Pi: Export failed"))))))

(defun pi-copy-last-message ()
  "Copy last assistant message to kill ring."
  (interactive)
  (when-let ((proc (pi--get-process)))
    (pi--rpc-async proc '(:type "get_last_assistant_text")
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

(defun pi--format-branch-message (msg)
  "Format MSG for display in branch selector.
MSG is a plist with :entryIndex and :text."
  (let* ((index (plist-get msg :entryIndex))
         (text (plist-get msg :text))
         (preview (truncate-string-to-width text 60 nil nil "...")))
    (format "%d: %s" index preview)))

(defun pi-branch ()
  "Branch conversation from a previous user message.
Shows a selector of user messages and creates a branch from the selected one."
  (interactive)
  (when-let ((proc (pi--get-process)))
    (pi--rpc-async proc '(:type "get_branch_messages")
                   (lambda (response)
                     (if (plist-get response :success)
                         (let* ((data (plist-get response :data))
                                (messages (plist-get data :messages)))
                           (if (null messages)
                               (message "Pi: No messages to branch from")
                             (pi--show-branch-selector proc messages)))
                       (message "Pi: Failed to get branch messages"))))))

(defun pi--show-branch-selector (proc messages)
  "Show selector for MESSAGES and branch on selection.
PROC is the pi process.  MESSAGES is a list of plists from get_branch_messages."
  (let* ((formatted (mapcar (lambda (msg)
                              (cons (pi--format-branch-message msg) msg))
                            messages))
         (choice (completing-read "Branch from: "
                                  (mapcar #'car formatted)
                                  nil t))
         (selected (cdr (assoc choice formatted))))
    (when selected
      (let ((entry-index (plist-get selected :entryIndex)))
        (pi--rpc-async proc (list :type "branch" :entryIndex entry-index)
                       (lambda (response)
                         (if (plist-get response :success)
                             (let* ((data (plist-get response :data))
                                    (text (plist-get data :text)))
                               ;; Reload and display the branched session
                               (pi--load-session-history
                                proc
                                (lambda (count)
                                  (message "Pi: Branched to new session (%d messages)" count)))
                               ;; Pre-fill input with the selected message text
                               (when-let ((input-buf (pi--get-input-buffer)))
                                 (with-current-buffer input-buf
                                   (erase-buffer)
                                   (insert text))))
                           (message "Pi: Branch failed"))))))))

(defun pi--run-custom-command (cmd)
  "Execute custom slash command CMD.
Prompts for arguments if the command content contains placeholders."
  (when-let ((chat-buf (pi--get-chat-buffer)))
    (let* ((content (plist-get cmd :content))
           (name (plist-get cmd :name))
           (needs-args (string-match-p "\\$[0-9@]" content))
           (args-string (if needs-args
                            (read-string (format "/%s: " name))
                          ""))
           (args (pi--parse-command-args args-string))
           (expanded (pi--substitute-args content args)))
      (with-current-buffer chat-buf
        (pi--display-user-message (format "/%s %s" name args-string) (current-time))
        (setq pi--status 'sending)
        (pi--spinner-start)
        (force-mode-line-update))
      (pi--send-prompt expanded))))

(defun pi--ensure-file-commands ()
  "Ensure pi--file-commands is populated for current session."
  (unless pi--file-commands
    (setq pi--file-commands (pi--discover-file-commands (pi--session-directory)))))

(defun pi--slash-capf ()
  "Completion-at-point function for /commands in input buffer.
Returns completion data when point is after / at start of line."
  (when (and (eq (char-after (line-beginning-position)) ?/)
             (not (bolp)))
    (pi--ensure-file-commands)
    (let* ((start (1+ (line-beginning-position)))
           (end (point))
           (commands (mapcar (lambda (cmd) (plist-get cmd :name))
                             pi--file-commands)))
      (list start end commands :exclusive 'no))))

(defun pi--expand-slash-command (text)
  "Expand TEXT if it's a known slash command.
If TEXT starts with / and matches a known command, expand it
with argument substitution.  Otherwise return TEXT unchanged."
  (pi--ensure-file-commands)
  (if (not (string-prefix-p "/" text))
      text
    (let* ((space-index (string-match " " text))
           (command-name (if space-index
                             (substring text 1 space-index)
                           (substring text 1)))
           (args-string (if space-index
                            (substring text (1+ space-index))
                          ""))
           (cmd (seq-find (lambda (c)
                            (equal (plist-get c :name) command-name))
                          pi--file-commands)))
      (if cmd
          (let ((args (pi--parse-command-args args-string)))
            (pi--substitute-args (plist-get cmd :content) args))
        text))))

(defun pi-run-custom-command ()
  "Select and run a custom slash command."
  (interactive)
  (pi--ensure-file-commands)
  (if (null pi--file-commands)
      (message "Pi: No custom commands found")
    (let* ((choices (mapcar (lambda (cmd)
                              (cons (format "%s - %s"
                                            (plist-get cmd :name)
                                            (or (plist-get cmd :description) ""))
                                    cmd))
                            pi--file-commands))
           (choice (completing-read "Custom command: " choices nil t))
           (cmd (cdr (assoc choice choices))))
      (when cmd
        (pi--run-custom-command cmd)))))

(transient-define-prefix pi-menu ()
  "Pi coding agent menu."
  [:description
   (lambda () (concat (pi--menu-model-description) " • "
                      (pi--menu-thinking-description)))
   :class transient-row]
  [["Session"
    ("n" "new" pi-new-session)
    ("r" "resume" pi-resume-session)
    ("e" "export" pi-export-html)
    ("q" "quit" pi-quit)]
   ["Context"
    ("c" "compact" pi-compact)
    ("b" "branch" pi-branch)]]
  [["Model"
    ("m" "select" pi-select-model)
    ("t" "thinking" pi-cycle-thinking)]
   ["Info"
    ("s" "stats" pi-session-stats)
    ("y" "copy last" pi-copy-last-message)]]
  [["Actions"
    ("RET" "send" pi-send)
    ("k" "abort" pi-abort)]])

(defun pi-refresh-commands ()
  "Refresh custom commands in the transient menu."
  (interactive)
  (pi--rebuild-custom-commands (pi--session-directory))
  (message "Pi: Refreshed %d custom commands" (length pi--file-commands)))

(defun pi--rebuild-custom-commands (dir)
  "Rebuild custom command entries in transient menu for DIR."
  ;; Discover and sort commands alphabetically
  (setq pi--file-commands
        (sort (pi--discover-file-commands dir)
              (lambda (a b)
                (string< (plist-get a :name) (plist-get b :name)))))
  ;; Remove existing custom group (index 4 if it exists)
  (ignore-errors (transient-remove-suffix 'pi-menu '(4)))
  ;; Add custom commands as a new two-column group after Actions (index 3)
  (when pi--file-commands
    (let* ((cmds (seq-take pi--file-commands 9))
           (mid (ceiling (length cmds) 2))
           (col1-cmds (seq-take cmds mid))
           (col2-cmds (seq-drop cmds mid))
           (col1 (pi--build-command-column "Custom" col1-cmds 1))
           (col2 (pi--build-command-column "" col2-cmds (1+ (length col1-cmds)))))
      (if col2-cmds
          ;; Two columns
          (transient-append-suffix 'pi-menu '(3) (vector col1 col2))
        ;; Single column
        (transient-append-suffix 'pi-menu '(3) col1)))))

(defun pi--build-command-column (title cmds start-key)
  "Build a transient column vector with TITLE and CMDS starting at START-KEY."
  (let ((key start-key)
        (suffixes (list title)))
    (dolist (cmd cmds)
      (let* ((name (plist-get cmd :name))
             (desc (truncate-string-to-width name 18)))
        (push (list (number-to-string key) desc
                    `(lambda () (interactive) (pi--run-custom-command ',cmd)))
              suffixes)
        (setq key (1+ key))))
    (apply #'vector (nreverse suffixes))))

;;;; Main Entry Point

(defun pi--setup-session (dir &optional session)
  "Set up a new or existing session for DIR with optional SESSION name.
Returns the chat buffer."
  (let* ((chat-buf (pi--get-or-create-buffer :chat dir session))
         (input-buf (pi--get-or-create-buffer :input dir session))
         (new-session nil))
    ;; Link buffers to each other
    (with-current-buffer chat-buf
      (setq pi--input-buffer input-buf)
      ;; Start process if not already running
      (unless (and pi--process (process-live-p pi--process))
        (setq pi--process (pi--start-process dir))
        (setq new-session t)
        ;; Store chat buffer reference in process for event handling
        (when (processp pi--process)
          (process-put pi--process 'pi-chat-buffer chat-buf)
          ;; Register event handler
          (pi--register-display-handler pi--process)
          ;; Initialize state from server
          (let ((buf chat-buf))  ; Capture for closure
            (pi--rpc-async pi--process '(:type "get_state")
                           (lambda (response)
                             (when (and (plist-get response :success)
                                        (buffer-live-p buf))
                               (with-current-buffer buf
                                 (let ((new-state (pi--extract-state-from-response response)))
                                   (setq pi--status (plist-get new-state :status)
                                         pi--state new-state))
                                 ;; Check if no model available and warn user
                                 (unless (plist-get pi--state :model)
                                   (pi--display-no-model-warning))
                                 (force-mode-line-update t))))))))
      ;; Build custom commands in transient for this session
      (when new-session
        (pi--rebuild-custom-commands dir))
      ;; Display startup header for new sessions
      (when new-session
        (pi--display-startup-header)))
    (with-current-buffer input-buf
      (setq pi--chat-buffer chat-buf))
    chat-buf))

(defun pi (&optional session)
  "Start or switch to pi session in current project.
With prefix arg, prompt for SESSION name to allow multiple sessions.
If already in a pi buffer and no SESSION specified, redisplays current session."
  (interactive
   (list (when current-prefix-arg
           (read-string "Session name: "))))
  (pi--check-dependencies)
  (let (chat-buf input-buf)
    (if (and (derived-mode-p 'pi-chat-mode 'pi-input-mode)
             (not session))
        ;; Already in pi buffer with no new session requested - use current session
        (setq chat-buf (pi--get-chat-buffer)
              input-buf (pi--get-input-buffer))
      ;; Find or create session for current directory
      (let ((dir (pi--session-directory)))
        (setq chat-buf (pi--setup-session dir session))
        (setq input-buf (buffer-local-value 'pi--input-buffer chat-buf))))
    ;; Display and focus
    (pi--display-buffers chat-buf input-buf)))

(provide 'pi)
;;; pi.el ends here
