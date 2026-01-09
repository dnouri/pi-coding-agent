;;; pi-coding-agent.el --- Emacs frontend for pi coding agent -*- lexical-binding: t; -*-

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

(require 'pi-coding-agent-core)
(require 'project)
(require 'markdown-mode)
(require 'ansi-color)

;;;; Customization Group

(defgroup pi-coding-agent nil
  "Emacs frontend for pi coding agent."
  :group 'tools
  :prefix "pi-coding-agent-")

;;;; Customization

(defcustom pi-coding-agent-rpc-timeout 30
  "Default timeout in seconds for synchronous RPC calls.
Some operations like model loading may need more time."
  :type 'natnum
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-input-window-height 10
  "Height of the input window in lines."
  :type 'natnum
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-separator-width 72
  "Total width of section separators in chat buffer."
  :type 'natnum
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-tool-preview-lines 10
  "Number of lines to show before collapsing tool output."
  :type 'natnum
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-bash-preview-lines 5
  "Number of lines to show for bash output before collapsing.
Bash output is typically more verbose, so fewer lines are shown."
  :type 'natnum
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-preview-max-bytes 51200
  "Maximum bytes for tool output preview (50KB default).
Prevents huge single-line outputs from blowing up the chat buffer."
  :type 'natnum
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-context-warning-threshold 70
  "Context usage percentage at which to show warning color."
  :type 'natnum
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-context-error-threshold 90
  "Context usage percentage at which to show error color."
  :type 'natnum
  :group 'pi-coding-agent)

;;;; Faces

(defface pi-coding-agent-separator
  '((t :inherit font-lock-comment-face))
  "Face for section separators in pi chat."
  :group 'pi-coding-agent)

(defface pi-coding-agent-user-label
  '((t :inherit bold :foreground "dodger blue"))
  "Face for the You label in pi chat."
  :group 'pi-coding-agent)

(defface pi-coding-agent-timestamp
  '((t :inherit shadow))
  "Face for timestamps in message headers."
  :group 'pi-coding-agent)

(defface pi-coding-agent-assistant-label
  '((t :inherit bold :foreground "sea green"))
  "Face for the Assistant label in pi chat."
  :group 'pi-coding-agent)

(defface pi-coding-agent-compaction-label
  '((t :inherit bold :foreground "medium purple"))
  "Face for the Compaction label in pi chat."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tool-name
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for tool names (BASH, READ, etc.) in pi chat."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tool-command
  '((t :inherit font-lock-function-name-face))
  "Face for tool commands and arguments."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tool-output
  '((t :inherit shadow))
  "Face for tool output text."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tool-error
  '((t :inherit error))
  "Face for tool error indicators."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tool-block-pending
  '((((class color) (min-colors 88) (background dark))
     :background "#2a2a35" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#f0f0f5" :extend t)
    (t :inherit secondary-selection :extend t))
  "Face for tool blocks during execution."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tool-block-success
  '((((class color) (min-colors 88) (background dark))
     :background "#2a352a" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#f0f5f0" :extend t)
    (t :inherit secondary-selection :extend t))
  "Face for tool blocks after successful completion."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tool-block-error
  '((((class color) (min-colors 88) (background dark))
     :background "#352a2a" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#f5f0f0" :extend t)
    (t :inherit secondary-selection :extend t))
  "Face for tool blocks after failed completion."
  :group 'pi-coding-agent)

(defface pi-coding-agent-collapsed-indicator
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for collapsed content indicators."
  :group 'pi-coding-agent)

(defface pi-coding-agent-model-name
  '((t :inherit font-lock-type-face))
  "Face for model name in header line."
  :group 'pi-coding-agent)

(defface pi-coding-agent-retry-notice
  '((t :inherit warning :slant italic))
  "Face for retry notifications (rate limit, overloaded, etc.)."
  :group 'pi-coding-agent)

(defface pi-coding-agent-error-notice
  '((t :inherit error))
  "Face for error notifications from the server."
  :group 'pi-coding-agent)

;;;; Language Detection

(defconst pi-coding-agent--extension-language-alist
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

(defun pi-coding-agent--path-to-language (path)
  "Return language name for PATH based on file extension.
Returns nil if the extension is not recognized."
  (when path
    (let ((ext (downcase (or (file-name-extension path) ""))))
      (cdr (assoc ext pi-coding-agent--extension-language-alist)))))

;;;; Major Modes

(defvar pi-coding-agent-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'pi-coding-agent-quit)
    (define-key map (kbd "C-c C-p") #'pi-coding-agent-menu)
    (define-key map (kbd "n") #'pi-coding-agent-next-message)
    (define-key map (kbd "p") #'pi-coding-agent-previous-message)
    (define-key map (kbd "TAB") #'pi-coding-agent-toggle-tool-section)
    (define-key map (kbd "<tab>") #'pi-coding-agent-toggle-tool-section)
    map)
  "Keymap for `pi-coding-agent-chat-mode'.")

(defun pi-coding-agent-next-message ()
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

(defun pi-coding-agent-previous-message ()
  "Move to the previous user message in the chat buffer."
  (interactive)
  (let ((pos (save-excursion
               (beginning-of-line)
               (re-search-backward "^You:" nil t))))
    (if pos
        (goto-char pos)
      (message "No previous message"))))

(defconst pi-coding-agent--blockquote-wrap-prefix
  (propertize "▌ " 'face 'markdown-blockquote-face)
  "String for continuation lines in blockquotes.
Matches `markdown-blockquote-display-char' with same face.")

(defun pi-coding-agent--fontify-blockquote-wrap-prefix (last)
  "Add `wrap-prefix' to blockquotes from point to LAST.
This makes wrapped lines show the blockquote indicator."
  (when (re-search-forward markdown-regex-blockquote last t)
    (put-text-property (match-beginning 0) (match-end 0)
                       'wrap-prefix pi-coding-agent--blockquote-wrap-prefix)
    t))

(define-derived-mode pi-coding-agent-chat-mode gfm-mode "Pi-Chat"
  "Major mode for displaying pi conversation.
Derives from `gfm-mode' for syntax highlighting of code blocks.
This is a read-only buffer showing the conversation history."
  :group 'pi-coding-agent
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local markdown-fontify-code-blocks-natively t)
  ;; Hide markdown markup (**, `, ```) for cleaner display
  (setq-local markdown-hide-markup t)
  (add-to-invisibility-spec 'markdown-markup)
  (setq-local pi-coding-agent--tool-args-cache (make-hash-table :test 'equal))
  ;; Make window-point follow inserted text (like comint does).
  ;; This is key for natural scroll behavior during streaming.
  (setq-local window-point-insertion-type t)

  ;; Add wrap-prefix to blockquotes so wrapped lines show the indicator
  (font-lock-add-keywords nil '((pi-coding-agent--fontify-blockquote-wrap-prefix)) 'append)

  (add-hook 'kill-buffer-hook #'pi-coding-agent--cleanup-on-kill nil t))

(defvar pi-coding-agent-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pi-coding-agent-send)
    (define-key map (kbd "TAB") #'completion-at-point)
    (define-key map (kbd "C-c C-k") #'pi-coding-agent-abort)
    (define-key map (kbd "C-c C-p") #'pi-coding-agent-menu)
    (define-key map (kbd "C-c C-r") #'pi-coding-agent-resume-session)
    (define-key map (kbd "M-p") #'pi-coding-agent-previous-input)
    (define-key map (kbd "M-n") #'pi-coding-agent-next-input)
    (define-key map (kbd "<C-up>") #'pi-coding-agent-previous-input)
    (define-key map (kbd "<C-down>") #'pi-coding-agent-next-input)
    (define-key map (kbd "C-r") #'pi-coding-agent-history-search)
    map)
  "Keymap for `pi-coding-agent-input-mode'.")

;;;; Input History (comint/eshell style)

(require 'ring)

(defvar pi-coding-agent--input-ring-size 100
  "Size of the input history ring.")

(defvar-local pi-coding-agent--input-ring nil
  "Ring holding input history for this session.")

(defvar-local pi-coding-agent--input-ring-index nil
  "Current position in input ring, or nil if not navigating history.")

(defvar-local pi-coding-agent--input-saved nil
  "Saved input before starting history navigation.")

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
    ;; Save current input on first navigation
    (unless pi-coding-agent--input-ring-index
      (setq pi-coding-agent--input-saved (buffer-string)))
    ;; Calculate new index
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
        ;; Restore saved input
        (progn
          (setq pi-coding-agent--input-ring-index nil)
          (when pi-coding-agent--input-saved
            (insert pi-coding-agent--input-saved)))
      ;; Show history entry
      (setq pi-coding-agent--input-ring-index new-index)
      (insert (ring-ref (pi-coding-agent--input-ring) new-index)))))

(defun pi-coding-agent-history-search ()
  "Search input history with completion."
  (interactive)
  (let* ((ring (pi-coding-agent--input-ring))
         (entries (when (not (ring-empty-p ring))
                    (ring-elements ring)))
         (choice (completing-read "History: " entries nil t)))
    (when (and choice (not (string-empty-p choice)))
      (delete-region (point-min) (point-max))
      (insert choice)
      ;; Reset history navigation state
      (setq pi-coding-agent--input-ring-index nil
            pi-coding-agent--input-saved nil))))

(define-derived-mode pi-coding-agent-input-mode text-mode "Pi-Input"
  "Major mode for composing pi prompts."
  :group 'pi-coding-agent
  (setq-local header-line-format '(:eval (pi-coding-agent--header-line-string)))
  (add-hook 'completion-at-point-functions #'pi-coding-agent--slash-capf nil t)
  (add-hook 'kill-buffer-hook #'pi-coding-agent--cleanup-input-on-kill nil t))

;;;; Session Directory Detection

(defun pi-coding-agent--session-directory ()
  "Determine directory for pi session.
Uses project root if available, otherwise `default-directory'.
Always returns an expanded absolute path (no ~ abbreviation)."
  (expand-file-name
   (or (when-let ((proj (project-current)))
         (project-root proj))
       default-directory)))

;;;; Buffer Naming & Creation

(defun pi-coding-agent--buffer-name (type dir &optional session)
  "Generate buffer name for TYPE (:chat or :input) in DIR.
Optional SESSION name creates a named session.
Uses abbreviated directory for readability in buffer lists."
  (let ((type-str (pcase type
                    (:chat "chat")
                    (:input "input")))
        (abbrev-dir (abbreviate-file-name dir)))
    (if (and session (not (string-empty-p session)))
        (format "*pi-coding-agent-%s:%s<%s>*" type-str abbrev-dir session)
      (format "*pi-coding-agent-%s:%s*" type-str abbrev-dir))))

(defun pi-coding-agent--find-session (dir &optional session)
  "Find existing chat buffer for DIR and SESSION.
Returns the chat buffer or nil if not found."
  (get-buffer (pi-coding-agent--buffer-name :chat dir session)))

(defun pi-coding-agent--get-or-create-buffer (type dir &optional session)
  "Get or create buffer of TYPE for DIR and optional SESSION.
TYPE is :chat or :input.  Returns the buffer."
  (let* ((name (pi-coding-agent--buffer-name type dir session))
         (existing (get-buffer name)))
    (if existing
        existing
      (let ((buf (generate-new-buffer name)))
        (with-current-buffer buf
          (pcase type
            (:chat (pi-coding-agent-chat-mode))
            (:input (pi-coding-agent-input-mode))))
        buf))))

;;;; Buffer-Local Session Variables

(defvar-local pi-coding-agent--process nil
  "The pi RPC subprocess for this session.")

(defvar-local pi-coding-agent--chat-buffer nil
  "Reference to the chat buffer for this session.")

(defvar-local pi-coding-agent--input-buffer nil
  "Reference to the input buffer for this session.")

(defvar-local pi-coding-agent--streaming-marker nil
  "Marker for current streaming insertion point.")

(defvar-local pi-coding-agent--in-code-block nil
  "Non-nil when streaming inside a fenced code block.
Used to suppress ATX heading transforms inside code.")

(defvar-local pi-coding-agent--in-thinking-block nil
  "Non-nil when streaming inside a thinking block.
Used to add blockquote prefix to each line.")

(defvar-local pi-coding-agent--line-parse-state 'line-start
  "Parsing state for current line during streaming.
Values:
  `line-start' - at beginning of line, ready for heading or fence
  `fence-1'    - seen one backtick at line start
  `fence-2'    - seen two backticks at line start
  `mid-line'   - somewhere in middle of line

Starts as `line-start' because content begins after separator newline.")

;; pi-coding-agent--status is defined in pi-coding-agent-core.el as the single source of truth
;; for session activity state (idle, sending, streaming, compacting)

(defvar-local pi-coding-agent--cached-stats nil
  "Cached session statistics for header-line display.
Updated after each agent turn completes.")

(defvar-local pi-coding-agent--last-usage nil
  "Usage from last assistant message for context percentage.
This is the per-turn usage, not cumulative - used to calculate
how much of the context window was used in the last turn.")

(defvar-local pi-coding-agent--aborted nil
  "Non-nil if the current/last request was aborted.")

(defvar-local pi-coding-agent--message-start-marker nil
  "Marker for start of current message content.
Used to replace raw markdown with rendered Org on message completion.")

(defvar-local pi-coding-agent--tool-args-cache nil
  "Hash table mapping toolCallId to args.
Needed because tool_execution_end events don't include args.")

(defvar-local pi-coding-agent--pending-tool-overlay nil
  "Overlay for tool block currently being executed.
Set by display-tool-start, used by display-tool-end.")

(defvar-local pi-coding-agent--assistant-header-shown nil
  "Non-nil if Assistant header has been shown for current prompt.
Used to avoid duplicate headers during retry sequences.")

(defvar-local pi-coding-agent--fontify-timer nil
  "Idle timer for periodic fontification during streaming.
Started on agent_start, stopped on agent_end.")

(defvar-local pi-coding-agent--last-fontified-pos nil
  "Position up to which we've fontified during streaming.
Used to avoid re-fontifying already-fontified text.")

;;;; Buffer Navigation

(defun pi-coding-agent--get-chat-buffer ()
  "Get the chat buffer for the current session.
Works from either chat or input buffer."
  (if (derived-mode-p 'pi-coding-agent-chat-mode)
      (current-buffer)
    pi-coding-agent--chat-buffer))

(defun pi-coding-agent--get-input-buffer ()
  "Get the input buffer for the current session.
Works from either chat or input buffer."
  (if (derived-mode-p 'pi-coding-agent-input-mode)
      (current-buffer)
    pi-coding-agent--input-buffer))

(defun pi-coding-agent--get-process ()
  "Get the pi process for the current session.
Works from either chat or input buffer."
  (if (derived-mode-p 'pi-coding-agent-chat-mode)
      pi-coding-agent--process
    (and pi-coding-agent--chat-buffer
         (buffer-local-value 'pi-coding-agent--process pi-coding-agent--chat-buffer))))

;;;; Display

(defun pi-coding-agent--display-buffers (chat-buf input-buf)
  "Display CHAT-BUF and INPUT-BUF in current window, split vertically.
Does not affect other windows in the frame."
  ;; Use current window for chat, split for input
  (switch-to-buffer chat-buf)
  (with-current-buffer chat-buf
    (goto-char (point-max)))
  (let ((input-win (split-window nil (- pi-coding-agent-input-window-height) 'below)))
    (set-window-buffer input-win input-buf)
    (select-window input-win)))

;;;; Response Display

;;; Scroll Behavior
;;
;; During streaming, windows "following" output (window-point at buffer end)
;; scroll to show new content. Windows where the user scrolled up stay put.
;;
;; Key mechanism: `window-point-insertion-type' is set to t in pi-coding-agent-chat-mode,
;; making window-point move with inserted text. We track which windows are
;; following before each insert, then restore point for non-following windows
;; afterward. Emacs naturally scrolls to keep point visible.

(defun pi-coding-agent--window-following-p (window)
  "Return non-nil if WINDOW is following output (point at end of buffer)."
  (>= (window-point window) (1- (point-max))))

(defmacro pi-coding-agent--with-scroll-preservation (&rest body)
  "Execute BODY preserving scroll for windows not following output.
Windows at buffer end will scroll to show new content.
Windows where user scrolled up stay in place."
  (declare (indent 0) (debug t))
  `(let* ((windows (get-buffer-window-list (current-buffer) nil t))
          (following (cl-remove-if-not #'pi-coding-agent--window-following-p windows))
          (saved-points (mapcar (lambda (w) (cons w (window-point w)))
                                (cl-remove-if #'pi-coding-agent--window-following-p windows))))
     ,@body
     ;; Restore point for non-following windows
     (dolist (pair saved-points)
       (when (window-live-p (car pair))
         (set-window-point (car pair) (cdr pair))))
     ;; Move following windows to new end
     (dolist (win following)
       (when (window-live-p win)
         (set-window-point win (point-max))))))

(defun pi-coding-agent--append-to-chat (text)
  "Append TEXT to the chat buffer.
Windows following the output (point at end) will scroll to show new text.
Windows where user scrolled up (point earlier) stay in place."
  (let ((inhibit-read-only t))
    (pi-coding-agent--with-scroll-preservation
      (save-excursion
        (goto-char (point-max))
        (insert text)))))

(defun pi-coding-agent--make-separator (label face &optional timestamp)
  "Create a setext-style H1 heading separator with LABEL styled with FACE.
If TIMESTAMP (Emacs time value) is provided, append it after \" · \".
Returns a markdown setext heading: label line followed by === underline.

Using setext headings enables outline/imenu navigation and keeps our
turn markers as H1 while LLM ATX headings are leveled down to H2+."
  (let* ((timestamp-str (when timestamp
                          (pi-coding-agent--format-message-timestamp timestamp)))
         (header-line (if timestamp-str
                          (concat label " · " timestamp-str)
                        label))
         ;; Underline must be at least 3 chars, and at least as long as header
         (underline-len (max 3 (length header-line)))
         (underline (make-string underline-len ?=)))
    (concat (propertize header-line 'face face) "\n"
            (propertize underline 'face 'pi-coding-agent-separator))))

(defun pi-coding-agent--display-user-message (text &optional timestamp)
  "Display user message TEXT in the chat buffer.
If TIMESTAMP (Emacs time value) is provided, display it in the header.
Note: No blank line after setext underline - the hidden === provides
visual spacing when `markdown-hide-markup' is enabled."
  (pi-coding-agent--append-to-chat
   (concat "\n" (pi-coding-agent--make-separator "You" 'pi-coding-agent-user-label timestamp) "\n"
           text "\n")))

(defun pi-coding-agent--display-agent-start ()
  "Display separator for new agent turn.
Only shows the Assistant header once per prompt, even during retries.
Note: status is set to `streaming' by the event handler.
Note: No blank line after setext underline - the hidden === provides
visual spacing when `markdown-hide-markup' is enabled."
  (setq pi-coding-agent--aborted nil)  ; Reset abort flag for new turn
  ;; Only show header if not already shown for this prompt
  (unless pi-coding-agent--assistant-header-shown
    (pi-coding-agent--append-to-chat
     (concat "\n" (pi-coding-agent--make-separator "Assistant" 'pi-coding-agent-assistant-label) "\n"))
    (setq pi-coding-agent--assistant-header-shown t))
  ;; Create markers at current end position
  ;; message-start-marker: where content begins (for later replacement)
  ;; streaming-marker: where new deltas are inserted
  (setq pi-coding-agent--message-start-marker (copy-marker (point-max) nil))
  (setq pi-coding-agent--streaming-marker (copy-marker (point-max) t))
  ;; Reset streaming parse state - content starts at line beginning, outside code/thinking block
  (setq pi-coding-agent--line-parse-state 'line-start)
  (setq pi-coding-agent--in-code-block nil)
  (setq pi-coding-agent--in-thinking-block nil)
  (pi-coding-agent--spinner-start)
  (pi-coding-agent--fontify-timer-start)
  (force-mode-line-update))

(defun pi-coding-agent--process-streaming-char (char state in-block)
  "Process CHAR with current STATE and IN-BLOCK flag.
Returns (NEW-STATE . NEW-IN-BLOCK).
STATE is one of: `line-start', `fence-1', `fence-2', `mid-line'."
  (pcase state
    ('line-start
     (cond
      ((eq char ?`) (cons 'fence-1 in-block))
      ((eq char ?\n) (cons 'line-start in-block))
      (t (cons 'mid-line in-block))))
    ('fence-1
     (cond
      ((eq char ?`) (cons 'fence-2 in-block))
      ((eq char ?\n) (cons 'line-start in-block))
      (t (cons 'mid-line in-block))))
    ('fence-2
     (cond
      ((eq char ?`) (cons 'mid-line (not in-block)))  ; Toggle code block!
      ((eq char ?\n) (cons 'line-start in-block))     ; Was just ``
      (t (cons 'mid-line in-block))))                 ; Was inline ``x
    ('mid-line
     (if (eq char ?\n)
         (cons 'line-start in-block)
       (cons 'mid-line in-block)))))

(defun pi-coding-agent--transform-delta (delta)
  "Transform DELTA for display, handling code blocks and heading levels.
Uses and updates buffer-local state variables for parse state.
Returns the transformed string.

Performance: Uses a two-pass approach.  First checks if transformation
is needed (rare), then only does the work when necessary.  The common
case of no headings is O(n) with no allocations."
  (let ((state pi-coding-agent--line-parse-state)
        (in-block pi-coding-agent--in-code-block)
        (len (length delta))
        (needs-transform nil)
        (i 0))
    ;; First pass: check if any transformation is needed and track state
    ;; Also collect positions where we need to insert extra #
    (let ((insert-positions nil))
      (while (< i len)
        (let ((char (aref delta i)))
          ;; Check if we need to add # at this position
          (when (and (eq state 'line-start)
                     (not in-block)
                     (eq char ?#))
            (push i insert-positions)
            (setq needs-transform t))
          ;; Update state
          (let ((new-state (pi-coding-agent--process-streaming-char char state in-block)))
            (setq state (car new-state))
            (setq in-block (cdr new-state)))
          (setq i (1+ i))))
      ;; Save final state
      (setq pi-coding-agent--line-parse-state state)
      (setq pi-coding-agent--in-code-block in-block)
      ;; Fast path: no transformation needed
      (if (not needs-transform)
          delta
        ;; Slow path: build result with extra # at marked positions
        ;; insert-positions is in reverse order (last position first)
        (let ((positions (nreverse insert-positions))
              (result nil)
              (prev-pos 0))
          (dolist (pos positions)
            ;; Add content before this position
            (when (< prev-pos pos)
              (push (substring delta prev-pos pos) result))
            ;; Add the extra #
            (push "#" result)
            (setq prev-pos pos))
          ;; Add remaining content
          (when (< prev-pos len)
            (push (substring delta prev-pos) result))
          (apply #'concat (nreverse result)))))))

(defun pi-coding-agent--display-message-delta (delta)
  "Display streaming message DELTA at the streaming marker.
Transforms ATX headings (outside code blocks) by adding one # level
to keep our setext H1 separators as the top-level document structure.
Inhibits modification hooks to prevent expensive jit-lock fontification
on each delta - fontification happens at message end instead."
  (when (and delta pi-coding-agent--streaming-marker)
    (let* ((inhibit-read-only t)
           (inhibit-modification-hooks t)
           (transformed (pi-coding-agent--transform-delta delta)))
      (pi-coding-agent--with-scroll-preservation
        (save-excursion
          (goto-char (marker-position pi-coding-agent--streaming-marker))
          (insert transformed)
          (set-marker pi-coding-agent--streaming-marker (point)))))))

(defun pi-coding-agent--display-thinking-start ()
  "Insert opening marker for thinking block (blockquote)."
  (when pi-coding-agent--streaming-marker
    (setq pi-coding-agent--in-thinking-block t)
    (let ((inhibit-read-only t))
      (pi-coding-agent--with-scroll-preservation
        (save-excursion
          (goto-char (marker-position pi-coding-agent--streaming-marker))
          (insert "> ")
          (set-marker pi-coding-agent--streaming-marker (point)))))))

(defun pi-coding-agent--display-thinking-delta (delta)
  "Display streaming thinking DELTA at the streaming marker.
Transforms newlines to include blockquote prefix.
Inhibits modification hooks to prevent expensive jit-lock fontification
on each delta - fontification happens at message end instead."
  (when (and delta pi-coding-agent--streaming-marker)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          ;; Transform newlines to include blockquote prefix on next line
          (transformed (replace-regexp-in-string "\n" "\n> " delta)))
      (pi-coding-agent--with-scroll-preservation
        (save-excursion
          (goto-char (marker-position pi-coding-agent--streaming-marker))
          (insert transformed)
          (set-marker pi-coding-agent--streaming-marker (point)))))))

(defun pi-coding-agent--display-thinking-end (_content)
  "End thinking block (blockquote).
CONTENT is ignored - we use what was already streamed."
  (when pi-coding-agent--streaming-marker
    (setq pi-coding-agent--in-thinking-block nil)
    (let ((inhibit-read-only t))
      (pi-coding-agent--with-scroll-preservation
        (save-excursion
          (goto-char (marker-position pi-coding-agent--streaming-marker))
          ;; End blockquote with blank line
          (insert "\n\n")
          (set-marker pi-coding-agent--streaming-marker (point)))))))

(defun pi-coding-agent--display-agent-end ()
  "Display end of agent turn.
Note: status is set to `idle' by the event handler."
  (let ((inhibit-read-only t))
    ;; Clean up pending tool overlay if abort happened mid-tool
    (pi-coding-agent--tool-overlay-finalize 'pi-coding-agent-tool-block-error)
    ;; Show abort indicator if aborted
    ;; Use scroll preservation so following windows stay at end
    (when pi-coding-agent--aborted
      (pi-coding-agent--with-scroll-preservation
        (save-excursion
          (goto-char (point-max))
          ;; Remove trailing whitespace before adding indicator
          (skip-chars-backward " \t\n")
          (delete-region (point) (point-max))
          (insert "\n\n" (propertize "[Aborted]" 'face 'error) "\n")))
      (setq pi-coding-agent--aborted nil))
    ;; Add spacing for next turn, avoiding excess blank lines
    ;; Use scroll preservation so following windows stay at end
    (pi-coding-agent--with-scroll-preservation
      (save-excursion
        (goto-char (point-max))
        (skip-chars-backward "\n")
        (delete-region (point) (point-max))
        (insert "\n\n"))))
  (pi-coding-agent--spinner-stop)
  (pi-coding-agent--fontify-timer-stop)
  (pi-coding-agent--refresh-header))

(defun pi-coding-agent--display-retry-start (event)
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
    (pi-coding-agent--append-to-chat
     (concat (propertize notice 'face 'pi-coding-agent-retry-notice) "\n"))))

(defun pi-coding-agent--display-retry-end (event)
  "Display retry result from auto_retry_end EVENT.
Shows success or final failure with raw error."
  (let* ((success (plist-get event :success))
         (attempt (plist-get event :attempt))
         (final-error (or (plist-get event :finalError) "unknown error")))
    (if (eq success t)
        (pi-coding-agent--append-to-chat
         (concat (propertize (format "✓ Retry succeeded on attempt %d"
                                     (or attempt 1))
                             'face 'pi-coding-agent-retry-notice)
                 "\n\n"))
      ;; Final failure
      (pi-coding-agent--append-to-chat
       (concat (propertize (format "✗ Retry failed after %d attempts — %s"
                                   (or attempt 1)
                                   final-error)
                           'face 'pi-coding-agent-error-notice)
               "\n\n")))))

(defun pi-coding-agent--display-error (error-msg)
  "Display ERROR-MSG from the server."
  (pi-coding-agent--append-to-chat
   (concat "\n" (propertize (format "[Error: %s]" (or error-msg "unknown"))
                            'face 'pi-coding-agent-error-notice)
           "\n")))

(defun pi-coding-agent--display-extension-error (event)
  "Display extension error from extension_error EVENT."
  (let* ((extension-path (plist-get event :extensionPath))
         (extension-event (plist-get event :event))
         (error-msg (plist-get event :error))
         (extension-name (if extension-path (file-name-nondirectory extension-path) "unknown")))
    (pi-coding-agent--append-to-chat
     (concat "\n"
             (propertize (format "[Extension error in %s (%s): %s]"
                                 extension-name
                                 (or extension-event "unknown")
                                 (or error-msg "unknown error"))
                         'face 'pi-coding-agent-error-notice)
             "\n"))))

(defun pi-coding-agent--display-no-model-warning ()
  "Display warning when no model is available.
Shown when the session starts without a configured model/API key."
  (pi-coding-agent--append-to-chat
   (concat "\n"
           (propertize "⚠ No models available"
                       'face 'pi-coding-agent-error-notice)
           "\n\n"
           (propertize "To get started, either:\n"
                       'face 'pi-coding-agent-retry-notice)
           (propertize "  • Set an API key: "
                       'face 'pi-coding-agent-retry-notice)
           "ANTHROPIC_API_KEY, OPENAI_API_KEY, GEMINI_API_KEY, etc.\n"
           (propertize "  • Or run "
                       'face 'pi-coding-agent-retry-notice)
           (propertize "pi --login"
                       'face 'pi-coding-agent-tool-command)
           (propertize " in a terminal to authenticate via OAuth\n"
                       'face 'pi-coding-agent-retry-notice)
           "\n")))

(defun pi-coding-agent--cleanup-on-kill ()
  "Clean up resources when chat buffer is killed.
Also kills the linked input buffer."
  (when (derived-mode-p 'pi-coding-agent-chat-mode)
    ;; Kill linked input buffer
    (when (and pi-coding-agent--input-buffer (buffer-live-p pi-coding-agent--input-buffer))
      (let ((input-buf pi-coding-agent--input-buffer))
        ;; Clear reference to avoid infinite recursion
        (setq pi-coding-agent--input-buffer nil)
        (kill-buffer input-buf)))
    ;; Clean up process
    (when pi-coding-agent--process
      (pi-coding-agent--unregister-display-handler pi-coding-agent--process)
      (when (process-live-p pi-coding-agent--process)
        (delete-process pi-coding-agent--process)))))

(defun pi-coding-agent--cleanup-input-on-kill ()
  "Clean up when input buffer is killed.
Also kills the linked chat buffer (which handles process cleanup)."
  (when (derived-mode-p 'pi-coding-agent-input-mode)
    (when (and pi-coding-agent--chat-buffer (buffer-live-p pi-coding-agent--chat-buffer))
      (let ((chat-buf pi-coding-agent--chat-buffer))
        ;; Clear reference to avoid infinite recursion
        (setq pi-coding-agent--chat-buffer nil)
        (kill-buffer chat-buf)))))

(defun pi-coding-agent--register-display-handler (process)
  "Register display event handler for PROCESS."
  (let ((handler (pi-coding-agent--make-display-handler process)))
    (process-put process 'pi-coding-agent-display-handler handler)))

(defun pi-coding-agent--unregister-display-handler (process)
  "Unregister display event handler for PROCESS."
  (process-put process 'pi-coding-agent-display-handler nil))

(defun pi-coding-agent--make-display-handler (process)
  "Create a display event handler for PROCESS."
  (lambda (event)
    (when-let ((chat-buf (process-get process 'pi-coding-agent-chat-buffer)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (pi-coding-agent--handle-display-event event))))))

(defun pi-coding-agent--handle-display-event (event)
  "Handle EVENT for display purposes.
Updates buffer-local state and renders display updates."
  ;; Update state first (now buffer-local)
  (pi-coding-agent--update-state-from-event event)
  ;; Then handle display
  (pcase (plist-get event :type)
    ("agent_start"
     (pi-coding-agent--display-agent-start))
    ("message_start"
     ;; Reset markers for each new message to avoid clobbering tool output
     (setq pi-coding-agent--message-start-marker (copy-marker (point-max) nil))
     (setq pi-coding-agent--streaming-marker (copy-marker (point-max) t)))
    ("message_update"
     (when-let* ((msg-event (plist-get event :assistantMessageEvent))
                 (event-type (plist-get msg-event :type)))
       (pcase event-type
         ("text_delta"
          (pi-coding-agent--display-message-delta (plist-get msg-event :delta)))
         ("thinking_start"
          (pi-coding-agent--display-thinking-start))
         ("thinking_delta"
          (pi-coding-agent--display-thinking-delta (plist-get msg-event :delta)))
         ("thinking_end"
          (pi-coding-agent--display-thinking-end (plist-get msg-event :content)))
         ("error"
          ;; Error during streaming (e.g., API error)
          (pi-coding-agent--display-error (plist-get msg-event :reason))))))
    ("message_end"
     (let ((message (plist-get event :message)))
       ;; Display error if message ended with error (e.g., API error)
       (when (equal (plist-get message :stopReason) "error")
         (pi-coding-agent--display-error (plist-get message :errorMessage)))
       ;; Capture usage from assistant messages for context % calculation.
       ;; Skip aborted messages - they may have incomplete usage data and
       ;; would reset context percentage to 0%.  Matches TUI footer.ts behavior.
       ;; Note: error messages DO have valid usage data (tokens were consumed).
       (when (and (equal (plist-get message :role) "assistant")
                  (not (equal (plist-get message :stopReason) "aborted"))
                  (plist-get message :usage))
         (setq pi-coding-agent--last-usage (plist-get message :usage))))
     (pi-coding-agent--render-complete-message))
    ("tool_execution_start"
     (let ((tool-call-id (plist-get event :toolCallId))
           (args (plist-get event :args)))
       ;; Cache args for tool_execution_end (which doesn't include args)
       (when (and tool-call-id pi-coding-agent--tool-args-cache)
         (puthash tool-call-id args pi-coding-agent--tool-args-cache))
       (pi-coding-agent--display-tool-start (plist-get event :toolName) args)))
    ("tool_execution_end"
     (let* ((tool-call-id (plist-get event :toolCallId))
            (result (plist-get event :result))
            ;; Retrieve cached args since tool_execution_end doesn't include them
            (args (when (and tool-call-id pi-coding-agent--tool-args-cache)
                    (prog1 (gethash tool-call-id pi-coding-agent--tool-args-cache)
                      (remhash tool-call-id pi-coding-agent--tool-args-cache)))))
       (pi-coding-agent--display-tool-end (plist-get event :toolName)
                             args
                             (plist-get result :content)
                             (plist-get result :details)
                             (plist-get event :isError))))
    ("tool_execution_update"
     (pi-coding-agent--display-tool-update (plist-get event :partialResult)))
    ("auto_compaction_start"
     (setq pi-coding-agent--status 'compacting)
     (pi-coding-agent--spinner-start)
     (let ((reason (plist-get event :reason)))
       (message "Pi: %sAuto-compacting... (C-c C-k to cancel)"
                (if (equal reason "overflow") "Context overflow, " ""))))
    ("auto_compaction_end"
     (pi-coding-agent--spinner-stop)
     (setq pi-coding-agent--status 'idle)
     (if (plist-get event :aborted)
         (message "Pi: Auto-compaction cancelled")
       (when-let ((result (plist-get event :result)))
         (pi-coding-agent--handle-compaction-success
          (plist-get result :tokensBefore)
          (plist-get result :summary)
          (pi-coding-agent--ms-to-time (plist-get result :timestamp))))))
    ("agent_end"
     (pi-coding-agent--display-agent-end))
    ("auto_retry_start"
     (pi-coding-agent--display-retry-start event))
    ("auto_retry_end"
     (pi-coding-agent--display-retry-end event))
    ("extension_error"
     (pi-coding-agent--display-extension-error event))))

;;;; Sending Prompts

(defun pi-coding-agent-send ()
  "Send the current input buffer contents to pi.
Clears the input buffer after sending.  Does nothing if buffer is empty.
If pi is currently streaming, shows a message and preserves input.
Slash commands are expanded before display and sending."
  (interactive)
  (let* ((text (string-trim (buffer-string)))
         (expanded (pi-coding-agent--expand-slash-command text))
         (chat-buf (pi-coding-agent--get-chat-buffer))
         (streaming (and chat-buf
                         (buffer-local-value 'pi-coding-agent--status chat-buf)
                         (memq (buffer-local-value 'pi-coding-agent--status chat-buf)
                               '(streaming sending)))))
    (cond
     ;; Empty input - do nothing
     ((string-empty-p text) nil)
     ;; Streaming - block send, preserve input
     (streaming
      (message "Pi: Please wait for response to complete"))
     ;; Normal send
     (t
      ;; Add to history and reset navigation state (save original for recall)
      (pi-coding-agent--history-add text)
      (setq pi-coding-agent--input-ring-index nil
            pi-coding-agent--input-saved nil)
      (erase-buffer)
      (with-current-buffer chat-buf
        (pi-coding-agent--display-user-message expanded (current-time))
        (setq pi-coding-agent--status 'sending)
        (setq pi-coding-agent--assistant-header-shown nil)  ; Reset for new prompt
        (pi-coding-agent--spinner-start)
        (force-mode-line-update))
      (pi-coding-agent--send-prompt expanded)))))

(defun pi-coding-agent--send-prompt (text)
  "Send TEXT as a prompt to the pi process.
If TEXT starts with /, tries to expand as a custom slash command.
Shows an error message if process is unavailable."
  (let ((proc (pi-coding-agent--get-process))
        (chat-buf (pi-coding-agent--get-chat-buffer))
        (expanded (pi-coding-agent--expand-slash-command text)))
    (cond
     ((null proc)
      (pi-coding-agent--abort-send chat-buf)
      (message "Pi: No process available - try M-x pi-coding-agent-recover or C-c C-p R"))
     ((not (process-live-p proc))
      (pi-coding-agent--abort-send chat-buf)
      (message "Pi: Process died - try M-x pi-coding-agent-recover or C-c C-p R"))
     (t
      (pi-coding-agent--rpc-async proc
                     (list :type "prompt" :message expanded)
                     #'ignore)))))

(defun pi-coding-agent--abort-send (chat-buf)
  "Clean up after a failed send attempt in CHAT-BUF.
Stops spinner and resets status to idle."
  (when (buffer-live-p chat-buf)
    (with-current-buffer chat-buf
      (pi-coding-agent--spinner-stop)
      (setq pi-coding-agent--status 'idle)
      (force-mode-line-update))))

(defun pi-coding-agent-abort ()
  "Abort the current pi operation.
Only works when streaming is in progress."
  (interactive)
  (when-let ((chat-buf (pi-coding-agent--get-chat-buffer)))
    ;; Check pi-coding-agent--status in chat buffer (it's updated by agent_start/agent_end)
    (when (eq (buffer-local-value 'pi-coding-agent--status chat-buf) 'streaming)
      ;; Set aborted flag in chat buffer (where agent-end will check it)
      (with-current-buffer chat-buf
        (setq pi-coding-agent--aborted t))
      (when-let ((proc (pi-coding-agent--get-process)))
        (pi-coding-agent--rpc-async proc
                       (list :type "abort")
                       (lambda (_response)
                         (run-with-timer 2 nil (lambda () (message nil)))
                         (message "Pi: Aborted")))))))

(defun pi-coding-agent-quit ()
  "Close the current pi session.
Kills both chat and input buffers, terminates the process,
and removes the input window (merging its space with adjacent windows)."
  (interactive)
  (let ((chat-buf (pi-coding-agent--get-chat-buffer))
        (input-buf (pi-coding-agent--get-input-buffer))
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

(defun pi-coding-agent--align-tables-in-region (start end)
  "Align all markdown tables between START and END."
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (re-search-forward "^|" end t))
      (when (markdown-table-at-point-p)
        (markdown-table-align)))))

(defun pi-coding-agent--render-complete-message ()
  "Finalize completed message by applying font-lock and aligning tables.
Uses message-start-marker and streaming-marker to find content.
Markdown stays as-is; `gfm-mode' handles highlighting and markup hiding.
Ensures message ends with newline for proper spacing."
  (when (and pi-coding-agent--message-start-marker pi-coding-agent--streaming-marker)
    (let ((start (marker-position pi-coding-agent--message-start-marker))
          (end (marker-position pi-coding-agent--streaming-marker)))
      (when (< start end)
        ;; Ensure trailing newline (messages should end with newline)
        ;; Use scroll preservation to keep following windows at end
        (let ((inhibit-read-only t))
          (pi-coding-agent--with-scroll-preservation
            (save-excursion
              (goto-char end)
              (unless (eq (char-before) ?\n)
                (insert "\n")
                (set-marker pi-coding-agent--streaming-marker (point)))))
          ;; Align any markdown tables in the message
          (pi-coding-agent--align-tables-in-region start (marker-position pi-coding-agent--streaming-marker)))
        (font-lock-ensure start (marker-position pi-coding-agent--streaming-marker))))))

;;;; Tool Output

(defsubst pi-coding-agent--tool-path (args)
  "Extract file path from tool ARGS.
Checks both :path and :file_path keys for compatibility."
  (or (plist-get args :path)
      (plist-get args :file_path)))

(defun pi-coding-agent--truncate-to-visual-lines (content max-lines width)
  "Truncate CONTENT to fit within MAX-LINES visual lines at WIDTH.
Also respects `pi-coding-agent-preview-max-bytes'.

Returns a plist with:
  :content      - the truncated content (or original if no truncation)
  :visual-lines - number of visual lines in result
  :hidden-lines - number of raw lines that were hidden"
  (let* ((lines (split-string content "\n" t))  ; omit empty strings from trailing newlines
         (total-raw-lines (length lines))
         (visual-count 0)
         (byte-count 0)
         (max-bytes pi-coding-agent-preview-max-bytes)
         (result-lines nil)
         (truncated-first-line nil))
    ;; Accumulate lines until we'd exceed limits
    (catch 'done
      (dolist (line lines)
        (let* ((line-len (length line))
               ;; Visual lines: ceiling(length / width), minimum 1
               (line-visual-lines (max 1 (ceiling (float line-len) width)))
               (new-visual-count (+ visual-count line-visual-lines))
               ;; +1 for newline between lines
               (new-byte-count (+ byte-count line-len (if result-lines 1 0))))
          ;; Check if adding this line would exceed limits
          (cond
           ;; Not first line and exceeds limits: stop
           ((and result-lines
                 (or (> new-visual-count max-lines)
                     (> new-byte-count max-bytes)))
            (throw 'done nil))
           ;; First line exceeds limits: truncate it to fit
           ((and (null result-lines)
                 (or (> new-visual-count max-lines)
                     (> new-byte-count max-bytes)))
            (let* ((max-chars-by-visual (* max-lines width))
                   (max-chars (min max-chars-by-visual max-bytes)))
              (setq line (substring line 0 (min line-len max-chars)))
              (setq line-len (length line))
              (setq line-visual-lines (max 1 (ceiling (float line-len) width)))
              (setq new-visual-count line-visual-lines)
              (setq new-byte-count line-len)
              (setq truncated-first-line t))))
          (setq visual-count new-visual-count)
          (setq byte-count new-byte-count)
          (push line result-lines))))
    (let* ((kept-lines (nreverse result-lines))
           ;; Count hidden lines - if first line was truncated, that's conceptually hidden content
           (hidden (- total-raw-lines (length kept-lines))))
      (list :content (string-join kept-lines "\n")
            :visual-lines visual-count
            ;; Report hidden lines; truncated first line means there's hidden content even with 1 line
            :hidden-lines (if (and truncated-first-line (= hidden 0)) 1 hidden)))))

(defun pi-coding-agent--tool-overlay-create (tool-name)
  "Create overlay for tool block TOOL-NAME at point.
Returns the overlay.  The overlay uses rear-advance so it
automatically extends when content is inserted at its end."
  (let ((ov (make-overlay (point) (point) nil nil t)))
    (overlay-put ov 'pi-coding-agent-tool-block t)
    (overlay-put ov 'pi-coding-agent-tool-name tool-name)
    (overlay-put ov 'face 'pi-coding-agent-tool-block-pending)
    ov))

(defun pi-coding-agent--tool-overlay-finalize (face)
  "Finalize pending tool overlay with FACE.
Replaces the overlay with a new one without rear-advance to prevent
it from extending to subsequent content.  Sets pending overlay to nil."
  (when pi-coding-agent--pending-tool-overlay
    (let ((start (overlay-start pi-coding-agent--pending-tool-overlay))
          (end (overlay-end pi-coding-agent--pending-tool-overlay))
          (tool-name (overlay-get pi-coding-agent--pending-tool-overlay
                                  'pi-coding-agent-tool-name))
          (header-end (overlay-get pi-coding-agent--pending-tool-overlay
                                   'pi-coding-agent-header-end)))
      (delete-overlay pi-coding-agent--pending-tool-overlay)
      (let ((ov (make-overlay start end nil nil nil)))  ; rear-advance=nil
        (overlay-put ov 'pi-coding-agent-tool-block t)
        (overlay-put ov 'pi-coding-agent-tool-name tool-name)
        (overlay-put ov 'pi-coding-agent-header-end header-end)
        (overlay-put ov 'face face)))
    (setq pi-coding-agent--pending-tool-overlay nil)))

(defun pi-coding-agent--display-tool-start (tool-name args)
  "Display header for tool TOOL-NAME with ARGS and create overlay."
  (let* ((header (pcase tool-name
                   ("bash" (format "$ %s" (or (plist-get args :command) "...")))
                   ("read" (format "read %s" (or (pi-coding-agent--tool-path args) "...")))
                   ("write" (format "write %s" (or (pi-coding-agent--tool-path args) "...")))
                   ("edit" (format "edit %s" (or (pi-coding-agent--tool-path args) "...")))
                   (_ tool-name)))
         (header-display (propertize header 'face 'pi-coding-agent-tool-command))
         (inhibit-read-only t))
    (pi-coding-agent--with-scroll-preservation
      (save-excursion
        (goto-char (point-max))
        ;; Add blank line before tool if previous line has content
        (unless (save-excursion
                  (forward-line -1)
                  (looking-at-p "^$"))
          (insert "\n"))
        ;; Create overlay at start of tool block
        (setq pi-coding-agent--pending-tool-overlay (pi-coding-agent--tool-overlay-create tool-name))
        (insert header-display "\n")
        ;; Store header end position for correct deletion in updates
        ;; (header may span multiple lines if command contains newlines)
        (overlay-put pi-coding-agent--pending-tool-overlay
                     'pi-coding-agent-header-end (point-marker))))))

(defun pi-coding-agent--extract-text-from-content (content-blocks)
  "Extract text from CONTENT-BLOCKS vector efficiently.
Returns the concatenated text from all text blocks.
Optimized for the common case of a single text block."
  (if (and (vectorp content-blocks) (> (length content-blocks) 0))
      (let ((first-block (aref content-blocks 0)))
        (if (and (= (length content-blocks) 1)
                 (equal (plist-get first-block :type) "text"))
            ;; Fast path: single text block (common case)
            (or (plist-get first-block :text) "")
          ;; Slow path: multiple blocks, need to filter and concat
          (mapconcat (lambda (c)
                       (if (equal (plist-get c :type) "text")
                           (or (plist-get c :text) "")
                         ""))
                     content-blocks "")))
    ""))

(defun pi-coding-agent--get-tail-lines (content n)
  "Get last N lines from CONTENT by scanning backward.
Returns (TAIL-CONTENT . HAS-HIDDEN) where HAS-HIDDEN is non-nil
if there are earlier lines not included in TAIL-CONTENT.
This is O(k) where k is the size of the tail, not O(n) like `split-string'."
  (let* ((len (length content))
         (pos len)
         (newlines-found 0))
    (if (= len 0)
        (cons "" nil)
      ;; Skip trailing newlines
      (while (and (> pos 0) (eq (aref content (1- pos)) ?\n))
        (setq pos (1- pos)))
      ;; Find N newlines from the end
      (while (and (> pos 0) (< newlines-found n))
        (setq pos (1- pos))
        (when (eq (aref content pos) ?\n)
          (setq newlines-found (1+ newlines-found))))
      ;; Adjust pos to start after the Nth newline
      (when (and (> pos 0) (eq (aref content pos) ?\n))
        (setq pos (1+ pos)))
      ;; Return tail and whether there's hidden content
      (cons (substring content pos) (> pos 0)))))

(defun pi-coding-agent--display-tool-update (partial-result)
  "Display PARTIAL-RESULT as streaming output in pending tool overlay.
PARTIAL-RESULT has same structure as tool result: plist with :content.
Shows rolling tail of output, truncated to visual lines.
Previous streaming content is replaced.
Inhibits modification hooks to prevent expensive jit-lock fontification
on each update - fontification happens at tool end instead.

Performance: Uses backward scanning to extract tail lines in O(k) time
where k is the tail size, rather than O(n) for the full content."
  (when (and pi-coding-agent--pending-tool-overlay partial-result)
    (let* ((content-blocks (plist-get partial-result :content))
           (raw-output (pi-coding-agent--extract-text-from-content content-blocks)))
      (when (not (string-empty-p raw-output))
        (let* ((max-lines pi-coding-agent-bash-preview-lines)
               ;; Get tail lines efficiently via backward scan
               (tail-result (pi-coding-agent--get-tail-lines raw-output max-lines))
               (tail-content (car tail-result))
               (has-hidden (cdr tail-result))
               ;; Apply visual line truncation to the tail
               (width (or (window-width) 80))
               (truncation (pi-coding-agent--truncate-to-visual-lines
                            tail-content max-lines width))
               (display-content (plist-get truncation :content))
               ;; Show indicator if either: earlier lines hidden OR single line truncated
               (show-hidden-indicator (or has-hidden
                                          (> (plist-get truncation :hidden-lines) 0)))
               (inhibit-read-only t)
               (inhibit-modification-hooks t))
          (pi-coding-agent--with-scroll-preservation
            (save-excursion
              (let* ((ov-end (overlay-end pi-coding-agent--pending-tool-overlay))
                     (header-end (overlay-get pi-coding-agent--pending-tool-overlay
                                              'pi-coding-agent-header-end)))
                ;; Delete previous streaming content (everything after header)
                ;; Header may span multiple lines if command contains newlines
                (when (and header-end (< header-end ov-end))
                  (delete-region header-end ov-end))
                ;; Insert new streaming content
                (goto-char (overlay-end pi-coding-agent--pending-tool-overlay))
                (when show-hidden-indicator
                  (insert (propertize "... (earlier output)\n"
                                      'face 'pi-coding-agent-collapsed-indicator)))
                (insert (propertize display-content 'face 'pi-coding-agent-tool-output))
                (insert "\n")))))))))

(defun pi-coding-agent--wrap-in-src-block (content lang)
  "Wrap CONTENT in a markdown fenced code block with LANG.
Returns markdown string for syntax highlighting."
  (format "```%s\n%s\n```" (or lang "") content))

(defun pi-coding-agent--render-tool-content (content lang)
  "Render CONTENT with optional syntax highlighting for LANG.
If LANG is non-nil, wraps in markdown code fence.
Returns the rendered string."
  (if lang
      (pi-coding-agent--wrap-in-src-block content lang)
    (propertize content 'face 'pi-coding-agent-tool-output)))

(defun pi-coding-agent--display-tool-end (tool-name args content details is-error)
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
                 ((or "edit" "read" "write")
                  (pi-coding-agent--path-to-language (pi-coding-agent--tool-path args)))
                 ("bash" "text")  ; wrap in fence for visual consistency
                 (_ nil)))
         ;; For edit tool with diff, we'll apply diff overlays after insertion
         (is-edit-diff (and (equal tool-name "edit")
                            (not is-error)
                            (plist-get details :diff)))
         ;; For edit with diff, use diff from details
         ;; For write, use content from args (result is just success message)
         ;; Strip ANSI escape codes - CLI tools often output colors
         (display-content (ansi-color-filter-apply
                           (pcase tool-name
                             ("edit" (or (plist-get details :diff) raw-output))
                             ("write" (or (plist-get args :content) raw-output))
                             (_ raw-output))))
         (preview-limit (pcase tool-name
                          ("bash" pi-coding-agent-bash-preview-lines)
                          (_ pi-coding-agent-tool-preview-lines)))
         ;; Use visual line truncation with byte limit
         (width (or (window-width) 80))
         (truncation (pi-coding-agent--truncate-to-visual-lines
                      display-content preview-limit width))
         (hidden-count (plist-get truncation :hidden-lines))
         (needs-collapse (> hidden-count 0))
         (inhibit-read-only t))
    (pi-coding-agent--with-scroll-preservation
      ;; Clear any streaming content from tool_execution_update
      (when pi-coding-agent--pending-tool-overlay
        (let ((header-end (overlay-get pi-coding-agent--pending-tool-overlay
                                       'pi-coding-agent-header-end))
              (ov-end (overlay-end pi-coding-agent--pending-tool-overlay)))
          ;; Header may span multiple lines if command contains newlines
          (when (and header-end (< header-end ov-end))
            (delete-region header-end ov-end))))
      (goto-char (point-max))
      (if needs-collapse
          ;; Long output: show preview with toggle button
          (let ((preview-content (plist-get truncation :content)))
            (pi-coding-agent--insert-tool-content-with-toggle
             preview-content display-content lang is-edit-diff hidden-count nil))
        ;; Short output: show all without toggle
        (let* ((rendered (pi-coding-agent--render-tool-content
                          (string-trim-right display-content "\n+") lang))
               (content-start (point)))
          (insert rendered "\n")
          (when is-edit-diff
            (pi-coding-agent--apply-diff-overlays content-start (point)))))
      ;; Error indicator
      (when is-error
        (insert (propertize "[error]" 'face 'pi-coding-agent-tool-error) "\n"))
      ;; Finalize overlay - replace with non-rear-advance version
      (pi-coding-agent--tool-overlay-finalize
       (if is-error 'pi-coding-agent-tool-block-error 'pi-coding-agent-tool-block-success))
      ;; Add trailing newline for spacing after tool block
      (insert "\n"))))

(defun pi-coding-agent--toggle-tool-output (button)
  "Toggle between preview and full content for BUTTON."
  (let* ((inhibit-read-only t)
         (expanded (button-get button 'pi-coding-agent-expanded))
         (full-content (button-get button 'pi-coding-agent-full-content))
         (preview-content (button-get button 'pi-coding-agent-preview-content))
         (lang (button-get button 'pi-coding-agent-lang))
         (is-edit-diff (button-get button 'pi-coding-agent-is-edit-diff))
         (hidden-count (button-get button 'hidden-count))
         (btn-start (button-start button))
         (btn-end (button-end button)))
    (save-excursion
      ;; Find the tool overlay
      (goto-char btn-start)
      (when-let* ((bounds (pi-coding-agent--find-tool-block-bounds))
                  (ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block))
                                (overlays-at (point))))
                  (header-end (overlay-get ov 'pi-coding-agent-header-end)))
        ;; Content starts after header (which may span multiple lines)
        (let ((content-start header-end))
          ;; Delete from content start to after button
          (delete-region content-start (1+ btn-end))
          (goto-char content-start)
          ;; Toggle: if currently expanded, show collapsed (and vice versa)
          (pi-coding-agent--insert-tool-content-with-toggle
           preview-content full-content lang is-edit-diff hidden-count (not expanded))
          ;; Ensure fontification of inserted content (JIT font-lock is lazy)
          (font-lock-ensure content-start (point))
          ;; Update overlay to include new content (overlay no longer has rear-advance)
          (move-overlay ov (car bounds) (point)))))))

(defun pi-coding-agent--insert-tool-content-with-toggle
    (preview-content full-content lang is-edit-diff hidden-count expanded)
  "Insert tool content with a toggle button.
When EXPANDED is nil, shows PREVIEW-CONTENT with expand button.
When EXPANDED is non-nil, shows FULL-CONTENT with collapse button.
LANG is for syntax highlighting.  IS-EDIT-DIFF applies diff overlays.
HIDDEN-COUNT is stored for the button label."
  (let* ((display-content (if expanded
                              (string-trim-right full-content "\n+")
                            preview-content))
         (rendered (pi-coding-agent--render-tool-content display-content lang))
         (content-start (point))
         (button-label (if expanded
                           "[-]"
                         (format "... (%d more lines)" hidden-count))))
    (insert rendered "\n")
    (when is-edit-diff
      (pi-coding-agent--apply-diff-overlays content-start (point)))
    (insert-text-button
     (propertize button-label 'face 'pi-coding-agent-collapsed-indicator)
     'action #'pi-coding-agent--toggle-tool-output
     'follow-link t
     'pi-coding-agent-full-content full-content
     'pi-coding-agent-preview-content preview-content
     'pi-coding-agent-lang lang
     'pi-coding-agent-is-edit-diff is-edit-diff
     'pi-coding-agent-expanded expanded
     'hidden-count hidden-count)
    (insert "\n")))

(defun pi-coding-agent--find-tool-block-bounds ()
  "Find the bounds of the tool block at point.
Returns (START . END) if inside a tool block, nil otherwise."
  (let ((overlays (overlays-at (point))))
    (when-let ((ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block)) overlays)))
      (cons (overlay-start ov) (overlay-end ov)))))

(defun pi-coding-agent--find-toggle-button-in-region (start end)
  "Find a toggle button between START and END."
  (save-excursion
    (goto-char start)
    (let ((found nil))
      (while (and (not found) (< (point) end))
        (let ((btn (button-at (point))))
          (if (and btn (button-get btn 'pi-coding-agent-full-content))
              (setq found btn)
            (forward-char 1))))
      found)))

(defun pi-coding-agent-toggle-tool-section ()
  "Toggle the tool output section at point.
Works anywhere inside a tool block overlay."
  (interactive)
  (let ((original-pos (point)))
    (if-let ((bounds (pi-coding-agent--find-tool-block-bounds)))
        (if-let ((btn (pi-coding-agent--find-toggle-button-in-region (car bounds) (cdr bounds))))
            (progn
              (pi-coding-agent--toggle-tool-output btn)
              ;; Try to restore position, clamped to new block bounds
              (when-let ((new-bounds (pi-coding-agent--find-tool-block-bounds)))
                (goto-char (min original-pos (cdr new-bounds)))))
          ;; No button found - short output, use markdown-cycle
          (markdown-cycle))
      ;; Not in a tool block
      (markdown-cycle))))

;;;; Diff Overlay Highlighting

;; Overlay priorities determine stacking order (higher = on top)
;; Tool-block overlay has no priority (defaults to 0)
(defconst pi-coding-agent--diff-line-priority 10
  "Priority for diff line background overlays.
Higher than tool-block (0) so diff colors show through.")

(defconst pi-coding-agent--diff-indicator-priority 20
  "Priority for diff indicator (+/-) overlays.
Higher than line background so indicator face isn't obscured.")

(defun pi-coding-agent--apply-diff-overlays (start end)
  "Apply diff highlighting overlays to region from START to END.
Scans for lines starting with +/- and applies diff faces via overlays.
Overlays survive font-lock refontification, unlike text properties.
The diff format from pi is: [+-]<space><padded-line-number><space><code>
For example: '+ 7     code' or '-12     code'"
  (save-excursion
    (goto-char start)
    (while (re-search-forward "^\\([+-]\\) *\\([0-9]+\\)" end t)
      (let* ((indicator (match-string 1))
             (is-added (string= indicator "+"))
             (indicator-start (match-beginning 1))
             (line-end (line-end-position))
             ;; Overlay for the indicator character
             (ind-ov (make-overlay indicator-start (match-end 1)))
             ;; Overlay for the rest of the line (background color)
             (line-ov (make-overlay (match-beginning 1) line-end)))
        ;; Indicator face (+/-) - highest priority to show on top
        (overlay-put ind-ov 'face (if is-added
                                      'diff-indicator-added
                                    'diff-indicator-removed))
        (overlay-put ind-ov 'priority pi-coding-agent--diff-indicator-priority)
        (overlay-put ind-ov 'pi-coding-agent-diff-overlay t)
        ;; Line background face - higher than tool-block but lower than indicator
        (overlay-put line-ov 'face (if is-added 'diff-added 'diff-removed))
        (overlay-put line-ov 'priority pi-coding-agent--diff-line-priority)
        (overlay-put line-ov 'pi-coding-agent-diff-overlay t)))))

;;;; Compaction Display

(defun pi-coding-agent--display-compaction-result (tokens-before summary &optional timestamp)
  "Display a compaction result block in the chat buffer.
TOKENS-BEFORE is the token count before compaction.
SUMMARY is the compaction summary text (markdown).
TIMESTAMP is optional time when compaction occurred."
  (pi-coding-agent--append-to-chat
   (concat "\n" (pi-coding-agent--make-separator "Compaction" 'pi-coding-agent-compaction-label timestamp) "\n"
           (propertize (format "Compacted from %s tokens\n\n"
                               (pi-coding-agent--format-number (or tokens-before 0)))
                       'face 'pi-coding-agent-tool-name)
           (or summary "") "\n")))

(defun pi-coding-agent--handle-compaction-success (tokens-before summary &optional timestamp)
  "Handle successful compaction: display result, reset state, notify user.
TOKENS-BEFORE is the pre-compaction token count.
SUMMARY is the compaction summary text.
TIMESTAMP is optional time when compaction occurred."
  (pi-coding-agent--display-compaction-result tokens-before summary timestamp)
  (setq pi-coding-agent--last-usage nil)
  (pi-coding-agent--refresh-header)
  (message "Pi: Compacted from %s tokens" (pi-coding-agent--format-number (or tokens-before 0))))

;;;; Dependency Checking

(defun pi-coding-agent--check-pi ()
  "Check if pi binary is available.
Returns t if available, nil otherwise."
  (and (executable-find "pi") t))

(defun pi-coding-agent--check-dependencies ()
  "Check all required dependencies.
Displays warnings for missing dependencies."
  (unless (pi-coding-agent--check-pi)
    (display-warning 'pi "pi binary not found. Install with: npm install -g @mariozechner/pi-coding-agent"
                     :error)))

;;;; Startup Header

(defconst pi-coding-agent-version "0.1.0"
  "Version of pi-coding-agent.")

(defun pi-coding-agent--get-pi-coding-agent-version ()
  "Get pi CLI version by running `pi --version'."
  (condition-case nil
      (string-trim (shell-command-to-string "pi --version"))
    (error "Unknown")))

(defun pi-coding-agent--format-startup-header ()
  "Format the startup header string with styled separator."
  (let* ((pi-coding-agent-cli-version (pi-coding-agent--get-pi-coding-agent-version))
         (separator (pi-coding-agent--make-separator (format "Pi %s" pi-coding-agent-cli-version)
                                        'pi-coding-agent-assistant-label)))
    (concat
     separator "\n"
     "C-c C-c   send prompt\n"
     "C-c C-k   abort\n"
     "C-c C-r   resume session\n"
     "C-c C-p   menu\n")))

(defun pi-coding-agent--display-startup-header ()
  "Display the startup header in the chat buffer."
  (pi-coding-agent--append-to-chat (pi-coding-agent--format-startup-header)))

;;;; Header Line

(defun pi-coding-agent--format-tokens-compact (n)
  "Format token count N compactly (e.g., 50k, 1.2M)."
  (cond
   ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
   ((>= n 1000) (format "%.0fk" (/ n 1000.0)))
   (t (number-to-string n))))

(defun pi-coding-agent--shorten-model-name (name)
  "Shorten model NAME for display.
Removes common prefixes like \"Claude \" and suffixes like \" (latest)\"."
  (thread-last name
    (replace-regexp-in-string "^[Cc]laude " "")
    (replace-regexp-in-string " (latest)$" "")
    (replace-regexp-in-string "^claude-" "")))

(defvar pi-coding-agent--spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Frames for the busy spinner animation.")

(defvar pi-coding-agent--spinner-index 0
  "Current frame index in the spinner animation (shared for sync).")

(defvar pi-coding-agent--spinner-timer nil
  "Timer for animating spinners (shared across sessions).")

(defvar pi-coding-agent--spinning-sessions nil
  "List of chat buffers currently spinning.")

(defun pi-coding-agent--spinner-start ()
  "Start the spinner for current session."
  (let ((chat-buf (pi-coding-agent--get-chat-buffer)))
    (when (and chat-buf (not (memq chat-buf pi-coding-agent--spinning-sessions)))
      (push chat-buf pi-coding-agent--spinning-sessions)
      ;; Start global timer if not running
      (unless pi-coding-agent--spinner-timer
        (setq pi-coding-agent--spinner-index 0)
        (setq pi-coding-agent--spinner-timer
              (run-with-timer 0 0.1 #'pi-coding-agent--spinner-tick))))))

(defun pi-coding-agent--spinner-stop (&optional chat-buf)
  "Stop the spinner for current session.
CHAT-BUF is the buffer to stop spinning; if nil, uses current context.
Note: When called from async callbacks, pass CHAT-BUF explicitly."
  (let ((chat-buf (or chat-buf (pi-coding-agent--get-chat-buffer))))
    (setq pi-coding-agent--spinning-sessions (delq chat-buf pi-coding-agent--spinning-sessions))
    ;; Stop global timer if no sessions spinning
    (when (and pi-coding-agent--spinner-timer (null pi-coding-agent--spinning-sessions))
      (cancel-timer pi-coding-agent--spinner-timer)
      (setq pi-coding-agent--spinner-timer nil))))

(defun pi-coding-agent--spinner-tick ()
  "Advance spinner to next frame and update spinning sessions."
  (setq pi-coding-agent--spinner-index
        (mod (1+ pi-coding-agent--spinner-index) (length pi-coding-agent--spinner-frames)))
  ;; Only update windows showing spinning sessions
  (dolist (buf pi-coding-agent--spinning-sessions)
    (when (buffer-live-p buf)
      (let ((input-buf (buffer-local-value 'pi-coding-agent--input-buffer buf)))
        ;; Update input buffer's header line (where spinner shows)
        (when (and input-buf (buffer-live-p input-buf))
          (dolist (win (get-buffer-window-list input-buf nil t))
            (with-selected-window win
              (force-mode-line-update))))))))

(defun pi-coding-agent--spinner-current ()
  "Return current spinner frame if this session is spinning."
  (let ((chat-buf (cond
                   ((derived-mode-p 'pi-coding-agent-chat-mode) (current-buffer))
                   ((derived-mode-p 'pi-coding-agent-input-mode) pi-coding-agent--chat-buffer)
                   (t nil))))
    (when (and chat-buf (memq chat-buf pi-coding-agent--spinning-sessions))
      (aref pi-coding-agent--spinner-frames pi-coding-agent--spinner-index))))

;;;; Streaming Fontification

(defcustom pi-coding-agent-fontify-idle-delay 0.2
  "Seconds of idle time before fontifying streamed content.
Lower values give more responsive highlighting but may cause stuttering."
  :type 'number
  :group 'pi-coding-agent)

(defun pi-coding-agent--fontify-streaming-region ()
  "Fontify newly streamed content incrementally.
Called by idle timer during streaming.  Only fontifies content
that hasn't been fontified yet."
  (when (and pi-coding-agent--message-start-marker
             pi-coding-agent--streaming-marker
             (marker-position pi-coding-agent--message-start-marker)
             (marker-position pi-coding-agent--streaming-marker))
    (let* ((start (or pi-coding-agent--last-fontified-pos
                      (marker-position pi-coding-agent--message-start-marker)))
           (end (marker-position pi-coding-agent--streaming-marker)))
      (when (< start end)
        (font-lock-ensure start end)
        (setq pi-coding-agent--last-fontified-pos end)))))

(defun pi-coding-agent--fontify-timer-start ()
  "Start idle timer for periodic fontification during streaming."
  (unless pi-coding-agent--fontify-timer
    (setq pi-coding-agent--last-fontified-pos nil)
    (setq pi-coding-agent--fontify-timer
          (run-with-idle-timer pi-coding-agent-fontify-idle-delay t
                               #'pi-coding-agent--fontify-timer-callback
                               (current-buffer)))))

(defun pi-coding-agent--fontify-timer-stop ()
  "Stop the fontification idle timer."
  (when pi-coding-agent--fontify-timer
    (cancel-timer pi-coding-agent--fontify-timer)
    (setq pi-coding-agent--fontify-timer nil)
    (setq pi-coding-agent--last-fontified-pos nil)))

(defun pi-coding-agent--fontify-timer-callback (buffer)
  "Fontify streaming region in BUFFER if it's still live and streaming."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq pi-coding-agent--status 'streaming)
        (pi-coding-agent--fontify-streaming-region)))))

(defvar pi-coding-agent--header-model-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'pi-coding-agent-select-model)
    (define-key map [header-line mouse-2] #'pi-coding-agent-select-model)
    map)
  "Keymap for clicking model name in header-line.")

(defvar pi-coding-agent--header-thinking-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'pi-coding-agent-cycle-thinking)
    (define-key map [header-line mouse-2] #'pi-coding-agent-cycle-thinking)
    map)
  "Keymap for clicking thinking level in header-line.")

(defun pi-coding-agent--header-format-context (context-tokens context-window)
  "Format context usage as percentage with color coding.
CONTEXT-TOKENS is the tokens used, CONTEXT-WINDOW is the max.
Returns nil if CONTEXT-WINDOW is 0."
  (when (> context-window 0)
    (let* ((pct (* (/ (float context-tokens) context-window) 100))
           ;; Note: %% needed because % has special meaning in header-line-format
           (pct-str (format " %.1f%%%%/%s" pct
                            (pi-coding-agent--format-tokens-compact context-window))))
      (propertize pct-str
                  'face (cond
                         ((> pct pi-coding-agent-context-error-threshold) 'error)
                         ((> pct pi-coding-agent-context-warning-threshold) 'warning)
                         (t nil))))))

(defun pi-coding-agent--header-format-stats (stats last-usage model-obj)
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
       " │ ↑" (pi-coding-agent--format-tokens-compact input)
       " ↓" (pi-coding-agent--format-tokens-compact output)
       " R" (pi-coding-agent--format-tokens-compact cache-read)
       " W" (pi-coding-agent--format-tokens-compact cache-write)
       (format " $%.2f" cost)
       (pi-coding-agent--header-format-context context-tokens context-window)))))

(defun pi-coding-agent--header-line-string ()
  "Return formatted header-line string for input buffer.
Accesses state from the linked chat buffer."
  (let* ((chat-buf (cond
                    ;; In input buffer with valid link to chat
                    ((and pi-coding-agent--chat-buffer (buffer-live-p pi-coding-agent--chat-buffer))
                     pi-coding-agent--chat-buffer)
                    ;; In chat buffer itself
                    ((derived-mode-p 'pi-coding-agent-chat-mode)
                     (current-buffer))
                    ;; No valid chat buffer yet
                    (t nil)))
         (state (and chat-buf (buffer-local-value 'pi-coding-agent--state chat-buf)))
         (stats (and chat-buf (buffer-local-value 'pi-coding-agent--cached-stats chat-buf)))
         (last-usage (and chat-buf (buffer-local-value 'pi-coding-agent--last-usage chat-buf)))
         (model-obj (plist-get state :model))
         (model-name (cond
                      ((stringp model-obj) model-obj)
                      ((plist-get model-obj :name))
                      (t "")))
         (model-short (if (string-empty-p model-name) "..."
                        (pi-coding-agent--shorten-model-name model-name)))
         (thinking (or (plist-get state :thinking-level) ""))
         (status-str (if-let ((spinner (pi-coding-agent--spinner-current)))
                         (concat " " spinner)
                       "  ")))  ; Same width as " ⠋" to prevent jumping
    (concat
     ;; Model (clickable)
     (propertize model-short
                 'face 'pi-coding-agent-model-name
                 'mouse-face 'highlight
                 'help-echo "mouse-1: Select model"
                 'local-map pi-coding-agent--header-model-map)
     ;; Thinking level (clickable)
     (unless (string-empty-p thinking)
       (concat " • "
               (propertize thinking
                           'mouse-face 'highlight
                           'help-echo "mouse-1: Cycle thinking level"
                           'local-map pi-coding-agent--header-thinking-map)))
     status-str
     ;; Stats (if available)
     (pi-coding-agent--header-format-stats stats last-usage model-obj))))

(defun pi-coding-agent--refresh-header ()
  "Refresh header-line by fetching and caching session stats."
  (when-let ((proc (pi-coding-agent--get-process))
             (chat-buf (pi-coding-agent--get-chat-buffer)))
    (let ((input-buf (buffer-local-value 'pi-coding-agent--input-buffer chat-buf)))
      (pi-coding-agent--rpc-async proc '(:type "get_session_stats")
                     (lambda (response)
                       (when (plist-get response :success)
                         (when (buffer-live-p chat-buf)
                           (with-current-buffer chat-buf
                             (setq pi-coding-agent--cached-stats (plist-get response :data))))
                         ;; Update the input buffer's header line
                         (when (buffer-live-p input-buf)
                           (dolist (win (get-buffer-window-list input-buf nil t))
                             (with-selected-window win
                               (force-mode-line-update))))))))))

;;;; Slash Command Discovery

(defun pi-coding-agent--parse-frontmatter (content)
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

(defun pi-coding-agent--load-commands-from-dir (dir source)
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
                     (parsed (pi-coding-agent--parse-frontmatter raw-content))
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

(defvar pi-coding-agent--file-commands nil
  "Cached list of discovered file-based slash commands.")

(defun pi-coding-agent--discover-file-commands (dir)
  "Discover file-based slash commands for session in DIR.
Searches ~/.pi/agent/commands/ and DIR/.pi/commands/.
Returns list of command plists."
  (let ((user-dir (expand-file-name "~/.pi/agent/commands/"))
        (project-dir (expand-file-name ".pi/commands/" dir)))
    (append (pi-coding-agent--load-commands-from-dir user-dir "user")
            (pi-coding-agent--load-commands-from-dir project-dir "project"))))

;;;; Slash Command Argument Handling

(defun pi-coding-agent--parse-command-args (args-string)
  "Parse ARGS-STRING into a list of arguments.
Uses shell-like quoting rules: double and single quotes preserve spaces,
backslash escapes characters.  Returns a list of argument strings,
or nil for empty/whitespace-only input."
  (let ((result (split-string-shell-command args-string)))
    (if (equal result '("")) nil result)))

(defun pi-coding-agent--substitute-args (content args)
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
Returns plist (:modified-time :first-message :message-count) or nil on error."
  (condition-case nil
      (let* ((attrs (file-attributes path))
             (modified-time (file-attribute-modification-time attrs)))
        (with-temp-buffer
          (insert-file-contents path)
          (let ((first-message nil)
                (message-count 0)
                (has-session-header nil))
            (goto-char (point-min))
            ;; Scan lines to find session header, first message, and count messages
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
                            (setq first-message (plist-get (aref content 0) :text)))))))))
              (forward-line 1))
            ;; Only return metadata if we found a valid session header
            (when has-session-header
              (list :modified-time modified-time
                    :first-message first-message
                    :message-count message-count)))))
    (error nil)))

(defun pi-coding-agent--ms-to-time (ms)
  "Convert milliseconds MS to Emacs time value.
Returns nil if MS is nil."
  (and ms (seconds-to-time (/ ms 1000.0))))

(defun pi-coding-agent--format-relative-time (time)
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

(defun pi-coding-agent--format-message-timestamp (time)
  "Format TIME for message headers.
Shows HH:MM if today, otherwise YYYY-MM-DD HH:MM."
  (let* ((time-day (format-time-string "%Y-%m-%d" time))
         (today (format-time-string "%Y-%m-%d" (current-time))))
    (if (string= time-day today)
        (format-time-string "%H:%M" time)
      (format-time-string "%Y-%m-%d %H:%M" time))))

(defun pi-coding-agent--truncate-string (str max-len)
  "Truncate STR to MAX-LEN chars, adding ellipsis if needed."
  (if (and str (> (length str) max-len))
      (concat (substring str 0 (- max-len 1)) "…")
    str))

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
Returns (display-string . path) for `completing-read'."
  (let ((metadata (pi-coding-agent--session-metadata path)))
    (if metadata
        (let* ((modified-time (plist-get metadata :modified-time))
               (first-msg (plist-get metadata :first-message))
               (msg-count (plist-get metadata :message-count))
               (relative-time (pi-coding-agent--format-relative-time modified-time))
               (preview (pi-coding-agent--truncate-string first-msg 50))
               (display (if preview
                            (format "%s · %s (%d msgs)"
                                    preview relative-time msg-count)
                          (format "[empty session] · %s" relative-time))))
          (cons display path))
      ;; Fallback to filename if metadata extraction fails
      (let ((filename (file-name-nondirectory path)))
        (cons filename path)))))

(defun pi-coding-agent--extract-message-text (message)
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

(defun pi-coding-agent--count-tool-calls (message)
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

(defun pi-coding-agent--render-history-text (text)
  "Render TEXT as markdown content with proper isolation.
Ensures markdown structures don't leak to subsequent content."
  (when (and text (not (string-empty-p text)))
    (let ((start (with-current-buffer (pi-coding-agent--get-chat-buffer) (point-max))))
      (pi-coding-agent--append-to-chat text)
      (with-current-buffer (pi-coding-agent--get-chat-buffer)
        (let ((inhibit-read-only t))
          (pi-coding-agent--align-tables-in-region start (point-max)))
        (font-lock-ensure start (point-max)))
      ;; Ensure we end with newlines to reset markdown context
      ;; Two newlines ends any list/paragraph context
      (pi-coding-agent--append-to-chat "\n\n"))))

(defun pi-coding-agent--display-history-messages (messages)
  "Display MESSAGES from session history with smart grouping.
Consecutive assistant messages are grouped under one header.
Tool calls are accumulated and shown as a single summary per group.
Each text block is rendered independently for proper formatting."
  (let ((prev-role nil)
        (pending-tool-count 0))
    ;; Helper to flush tool count summary
    (cl-flet ((flush-tools ()
                (when (> pending-tool-count 0)
                  (pi-coding-agent--append-to-chat
                   (concat (propertize (format "[%d tool call%s]"
                                               pending-tool-count
                                               (if (= pending-tool-count 1) "" "s"))
                                       'face 'pi-coding-agent-tool-name)
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
             ;; Show user message (no blank line after setext - hidden === provides spacing)
             (let* ((text (pi-coding-agent--extract-message-text message))
                    (timestamp (pi-coding-agent--ms-to-time (plist-get message :timestamp))))
               (when (and text (not (string-empty-p text)))
                 (pi-coding-agent--append-to-chat
                  (concat "\n" (pi-coding-agent--make-separator "You" 'pi-coding-agent-user-label timestamp) "\n"
                          text "\n"))))
             (setq prev-role "user"))
            ("assistant"
             ;; Show header only on transition from user
             (when (not (equal prev-role "assistant"))
               (flush-tools)
               (pi-coding-agent--append-to-chat
                (concat "\n" (pi-coding-agent--make-separator "Assistant" 'pi-coding-agent-assistant-label) "\n")))
             ;; Render text immediately (isolated)
             (let ((text (pi-coding-agent--extract-message-text message))
                   (tool-count (pi-coding-agent--count-tool-calls message)))
               (when (and text (not (string-empty-p text)))
                 (pi-coding-agent--render-history-text text))
               (setq pending-tool-count (+ pending-tool-count tool-count)))
             (setq prev-role "assistant"))
            ("compactionSummary"
             ;; Show compaction with header, tokens, and summary
             (flush-tools)
             (let* ((summary (plist-get message :summary))
                    (tokens-before (plist-get message :tokensBefore))
                    (timestamp (pi-coding-agent--ms-to-time (plist-get message :timestamp))))
               (pi-coding-agent--display-compaction-result tokens-before summary timestamp))
             (setq prev-role "compactionSummary"))
            ;; Skip toolResult - already counted tool calls in assistant message
            ("toolResult"
             nil))))
      ;; Flush any remaining tool count
      (flush-tools))))

(defun pi-coding-agent--clear-chat-buffer ()
  "Clear the chat buffer and display fresh startup header.
Used when starting a new session."
  (when-let ((chat-buf (pi-coding-agent--get-chat-buffer)))
    (with-current-buffer chat-buf
      (let ((inhibit-read-only t))
        ;; Clear buffer
        (erase-buffer)
        ;; Show startup header
        (insert (pi-coding-agent--format-startup-header))
        (insert "\n")
        ;; Reset markers
        (setq pi-coding-agent--message-start-marker nil)
        (setq pi-coding-agent--streaming-marker nil)
        ;; Reset usage (context % will show 0% until next message)
        (setq pi-coding-agent--last-usage nil)
        ;; Clear tool args cache
        (clrhash pi-coding-agent--tool-args-cache)
        ;; Position at end
        (goto-char (point-max))))))

(defun pi-coding-agent--display-session-history (messages &optional chat-buf)
  "Display session history MESSAGES in the chat buffer.
MESSAGES is a vector of message plists from get_messages RPC.
CHAT-BUF is the target buffer; if nil, uses `pi-coding-agent--get-chat-buffer'.
Note: When called from async callbacks, pass CHAT-BUF explicitly."
  (setq chat-buf (or chat-buf (pi-coding-agent--get-chat-buffer)))
  (when (and chat-buf (buffer-live-p chat-buf))
    (with-current-buffer chat-buf
      (let ((inhibit-read-only t))
        ;; Clear buffer
        (erase-buffer)
        ;; Show startup header
        (insert (pi-coding-agent--format-startup-header))
        (insert "\n")
        ;; Render messages with smart grouping
        (when (vectorp messages)
          (pi-coding-agent--display-history-messages messages))
        ;; Ensure we end with proper spacing
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        ;; Reset markers for future streaming
        (setq pi-coding-agent--message-start-marker nil)
        (setq pi-coding-agent--streaming-marker nil)
        ;; Scroll to end
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
                         ;; Update header with new session info
                         (when (buffer-live-p chat-buf)
                           (with-current-buffer chat-buf
                             (pi-coding-agent--refresh-header)))
                         (when callback
                           (funcall callback count))))))))

;;;###autoload
(defun pi-coding-agent-recover ()
  "Recover the current session by restarting the process.
Useful when the pi process has died or become unresponsive.
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
      (message "Pi: No session to recover"))
     ;; No session file cached
     ((not session-file)
      (message "Pi: No session file available - cannot recover"))
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
          (setq pi-coding-agent--process new-proc)
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
                                   ;; Reload state
                                   (pi-coding-agent--rpc-async new-proc '(:type "get_state")
                                                  (lambda (state-response)
                                                    (when (plist-get state-response :success)
                                                      (let ((new-state (pi-coding-agent--extract-state-from-response state-response)))
                                                        (setq pi-coding-agent--status (plist-get new-state :status)
                                                              pi-coding-agent--state new-state))
                                                      (force-mode-line-update t))))
                                   (message "Pi: Session recovered"))
                               (message "Pi: Failed to recover - %s"
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
                                   (pi-coding-agent--load-session-history
                                    proc
                                    (lambda (count)
                                      (message "Pi: Resumed session (%d messages)" count))
                                    chat-buf)
                                 (message "Pi: Failed to resume session")))))))))))

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

(defun pi-coding-agent--format-number (n)
  "Format number N with thousands separators."
  (let ((str (number-to-string n)))
    (replace-regexp-in-string
     "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\([^0-9]\\|$\\)"
     "\\1,\\2\\3"
     (replace-regexp-in-string
      "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\([0-9]\\{3\\}\\)\\([^0-9]\\|$\\)"
      "\\1,\\2,\\3\\4" str))))

(defun pi-coding-agent--format-session-stats (stats)
  "Format STATS plist as human-readable string."
  (let* ((tokens (plist-get stats :tokens))
         (input (or (plist-get tokens :input) 0))
         (output (or (plist-get tokens :output) 0))
         (total (or (plist-get tokens :total) 0))
         (cost (or (plist-get stats :cost) 0))
         (messages (or (plist-get stats :userMessages) 0))
         (tools (or (plist-get stats :toolCalls) 0)))
    (format "Tokens: %s in / %s out (%s total) | Cost: $%.2f | Messages: %d | Tools: %d"
            (pi-coding-agent--format-number input)
            (pi-coding-agent--format-number output)
            (pi-coding-agent--format-number total)
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

(defun pi-coding-agent-compact ()
  "Compact conversation context to reduce token usage."
  (interactive)
  (when-let ((proc (pi-coding-agent--get-process))
             (chat-buf (pi-coding-agent--get-chat-buffer)))
    (message "Pi: Compacting...")
    (pi-coding-agent--spinner-start)
    (pi-coding-agent--rpc-async proc '(:type "compact")
                   (lambda (response)
                     ;; Pass chat-buf explicitly (callback may run in arbitrary context)
                     (pi-coding-agent--spinner-stop chat-buf)
                     (if (plist-get response :success)
                         (when (buffer-live-p chat-buf)
                           (with-current-buffer chat-buf
                             (let ((data (plist-get response :data)))
                               (pi-coding-agent--handle-compaction-success
                                (plist-get data :tokensBefore)
                                (plist-get data :summary)
                                (current-time)))))
                       (message "Pi: Compact failed"))))))

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

(defun pi-coding-agent--format-branch-message (msg &optional index)
  "Format MSG for display in branch selector.
MSG is a plist with :entryId and :text.
INDEX is the display index (1-based) for the message."
  (let* ((text (or (plist-get msg :text) ""))
         (preview (truncate-string-to-width text 60 nil nil "...")))
    (if index
        (format "%d: %s" index preview)
      preview)))

(defun pi-coding-agent-branch ()
  "Branch conversation from a previous user message.
Shows a selector of user messages and creates a branch from the selected one."
  (interactive)
  (when-let ((proc (pi-coding-agent--get-process)))
    (pi-coding-agent--rpc-async proc '(:type "get_branch_messages")
                   (lambda (response)
                     (if (plist-get response :success)
                         (let* ((data (plist-get response :data))
                                (messages (plist-get data :messages)))
                           ;; Note: messages is a vector from JSON, use seq-empty-p not null
                           (if (seq-empty-p messages)
                               (message "Pi: No messages to branch from")
                             (pi-coding-agent--show-branch-selector proc messages)))
                       (message "Pi: Failed to get branch messages"))))))

(defun pi-coding-agent--show-branch-selector (proc messages)
  "Show selector for MESSAGES and branch on selection.
PROC is the pi process.
MESSAGES is a vector of plists from get_branch_messages."
  (let* ((index 0)
         ;; Reverse so most recent messages appear first (upstream sends chronological order)
         (reversed-messages (reverse (append messages nil)))
         (formatted (mapcar (lambda (msg)
                              (setq index (1+ index))
                              (cons (pi-coding-agent--format-branch-message msg index) msg))
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
         (selected (cdr (assoc choice formatted)))
         ;; Capture buffers before async call (callback runs in arbitrary context)
         (chat-buf (pi-coding-agent--get-chat-buffer))
         (input-buf (pi-coding-agent--get-input-buffer)))
    (when selected
      (let ((entry-id (plist-get selected :entryId)))
        (pi-coding-agent--rpc-async proc (list :type "branch" :entryId entry-id)
                       (lambda (response)
                         (if (plist-get response :success)
                             (let* ((data (plist-get response :data))
                                    (text (plist-get data :text)))
                               ;; Reload and display the branched session
                               (pi-coding-agent--load-session-history
                                proc
                                (lambda (count)
                                  (message "Pi: Branched to new session (%d messages)" count))
                                chat-buf)
                               ;; Pre-fill input with the selected message text
                               (when (buffer-live-p input-buf)
                                 (with-current-buffer input-buf
                                   (erase-buffer)
                                   ;; text may be nil if RPC returns null
                                   (when text (insert text)))))
                           (message "Pi: Branch failed"))))))))

(defun pi-coding-agent--run-custom-command (cmd)
  "Execute custom slash command CMD.
Prompts for arguments if the command content contains placeholders."
  (when-let ((chat-buf (pi-coding-agent--get-chat-buffer)))
    (let* ((content (plist-get cmd :content))
           (name (plist-get cmd :name))
           (needs-args (string-match-p "\\$[0-9@]" content))
           (args-string (if needs-args
                            (read-string (format "/%s: " name))
                          ""))
           (args (pi-coding-agent--parse-command-args args-string))
           (expanded (pi-coding-agent--substitute-args content args)))
      (with-current-buffer chat-buf
        (pi-coding-agent--display-user-message (format "/%s %s" name args-string) (current-time))
        (setq pi-coding-agent--status 'sending)
        (pi-coding-agent--spinner-start)
        (force-mode-line-update))
      (pi-coding-agent--send-prompt expanded))))

(defun pi-coding-agent--ensure-file-commands ()
  "Ensure pi-coding-agent--file-commands is populated for current session."
  (unless pi-coding-agent--file-commands
    (setq pi-coding-agent--file-commands (pi-coding-agent--discover-file-commands (pi-coding-agent--session-directory)))))

(defun pi-coding-agent--slash-capf ()
  "Completion-at-point function for /commands in input buffer.
Returns completion data when point is after / at start of line."
  (when (and (eq (char-after (line-beginning-position)) ?/)
             (not (bolp)))
    (pi-coding-agent--ensure-file-commands)
    (let* ((start (1+ (line-beginning-position)))
           (end (point))
           (commands (mapcar (lambda (cmd) (plist-get cmd :name))
                             pi-coding-agent--file-commands)))
      (list start end commands :exclusive 'no))))

(defun pi-coding-agent--expand-slash-command (text)
  "Expand TEXT if it's a known slash command.
If TEXT starts with / and matches a known command, expand it
with argument substitution.  Otherwise return TEXT unchanged."
  (pi-coding-agent--ensure-file-commands)
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
                          pi-coding-agent--file-commands)))
      (if cmd
          (let ((args (pi-coding-agent--parse-command-args args-string)))
            (pi-coding-agent--substitute-args (plist-get cmd :content) args))
        text))))

(defun pi-coding-agent-run-custom-command ()
  "Select and run a custom slash command."
  (interactive)
  (pi-coding-agent--ensure-file-commands)
  (if (null pi-coding-agent--file-commands)
      (message "Pi: No custom commands found")
    (let* ((choices (mapcar (lambda (cmd)
                              (cons (format "%s - %s"
                                            (plist-get cmd :name)
                                            (or (plist-get cmd :description) ""))
                                    cmd))
                            pi-coding-agent--file-commands))
           (choice (completing-read "Custom command: " choices nil t))
           (cmd (cdr (assoc choice choices))))
      (when cmd
        (pi-coding-agent--run-custom-command cmd)))))

(transient-define-prefix pi-coding-agent-menu ()
  "Pi coding agent menu."
  [:description
   (lambda () (concat (pi-coding-agent--menu-model-description) " • "
                      (pi-coding-agent--menu-thinking-description)))
   :class transient-row]
  [["Session"
    ("n" "new" pi-coding-agent-new-session)
    ("r" "resume" pi-coding-agent-resume-session)
    ("R" "recover" pi-coding-agent-recover)
    ("e" "export" pi-coding-agent-export-html)
    ("q" "quit" pi-coding-agent-quit)]
   ["Context"
    ("c" "compact" pi-coding-agent-compact)
    ("b" "branch" pi-coding-agent-branch)]]
  [["Model"
    ("m" "select" pi-coding-agent-select-model)
    ("t" "thinking" pi-coding-agent-cycle-thinking)]
   ["Info"
    ("s" "stats" pi-coding-agent-session-stats)
    ("y" "copy last" pi-coding-agent-copy-last-message)]]
  [["Actions"
    ("RET" "send" pi-coding-agent-send)
    ("k" "abort" pi-coding-agent-abort)]])

(defun pi-coding-agent-refresh-commands ()
  "Refresh custom commands in the transient menu."
  (interactive)
  (pi-coding-agent--rebuild-custom-commands (pi-coding-agent--session-directory))
  (message "Pi: Refreshed %d custom commands" (length pi-coding-agent--file-commands)))

(defun pi-coding-agent--rebuild-custom-commands (dir)
  "Rebuild custom command entries in transient menu for DIR."
  ;; Discover and sort commands alphabetically
  (setq pi-coding-agent--file-commands
        (sort (pi-coding-agent--discover-file-commands dir)
              (lambda (a b)
                (string< (plist-get a :name) (plist-get b :name)))))
  ;; Remove existing custom group (index 4 if it exists)
  (ignore-errors (transient-remove-suffix 'pi-coding-agent-menu '(4)))
  ;; Add custom commands as a new two-column group after Actions (index 3)
  (when pi-coding-agent--file-commands
    (let* ((cmds (seq-take pi-coding-agent--file-commands 9))
           (mid (ceiling (length cmds) 2))
           (col1-cmds (seq-take cmds mid))
           (col2-cmds (seq-drop cmds mid))
           (col1 (pi-coding-agent--build-command-column "Custom" col1-cmds 1))
           (col2 (pi-coding-agent--build-command-column "" col2-cmds (1+ (length col1-cmds)))))
      (if col2-cmds
          ;; Two columns
          (transient-append-suffix 'pi-coding-agent-menu '(3) (vector col1 col2))
        ;; Single column
        (transient-append-suffix 'pi-coding-agent-menu '(3) col1)))))

(defun pi-coding-agent--build-command-column (title cmds start-key)
  "Build a transient column vector with TITLE and CMDS starting at START-KEY."
  (let ((key start-key)
        (suffixes (list title)))
    (dolist (cmd cmds)
      (let* ((name (plist-get cmd :name))
             (desc (truncate-string-to-width name 18)))
        (push (list (number-to-string key) desc
                    `(lambda () (interactive) (pi-coding-agent--run-custom-command ',cmd)))
              suffixes)
        (setq key (1+ key))))
    (apply #'vector (nreverse suffixes))))

;;;; Main Entry Point

(defun pi-coding-agent--setup-session (dir &optional session)
  "Set up a new or existing session for DIR with optional SESSION name.
Returns the chat buffer."
  (let* ((chat-buf (pi-coding-agent--get-or-create-buffer :chat dir session))
         (input-buf (pi-coding-agent--get-or-create-buffer :input dir session))
         (new-session nil))
    ;; Link buffers to each other
    (with-current-buffer chat-buf
      (setq pi-coding-agent--input-buffer input-buf)
      ;; Start process if not already running
      (unless (and pi-coding-agent--process (process-live-p pi-coding-agent--process))
        (setq pi-coding-agent--process (pi-coding-agent--start-process dir))
        (setq new-session t)
        ;; Associate process with chat buffer for built-in kill confirmation
        (when (processp pi-coding-agent--process)
          (set-process-buffer pi-coding-agent--process chat-buf)
          (process-put pi-coding-agent--process 'pi-coding-agent-chat-buffer chat-buf)
          ;; Register event handler
          (pi-coding-agent--register-display-handler pi-coding-agent--process)
          ;; Initialize state from server
          (let ((buf chat-buf))  ; Capture for closure
            (pi-coding-agent--rpc-async pi-coding-agent--process '(:type "get_state")
                           (lambda (response)
                             (when (and (plist-get response :success)
                                        (buffer-live-p buf))
                               (with-current-buffer buf
                                 (let ((new-state (pi-coding-agent--extract-state-from-response response)))
                                   (setq pi-coding-agent--status (plist-get new-state :status)
                                         pi-coding-agent--state new-state))
                                 ;; Check if no model available and warn user
                                 (unless (plist-get pi-coding-agent--state :model)
                                   (pi-coding-agent--display-no-model-warning))
                                 (force-mode-line-update t))))))))
      ;; Build custom commands in transient for this session
      (when new-session
        (pi-coding-agent--rebuild-custom-commands dir))
      ;; Display startup header for new sessions
      (when new-session
        (pi-coding-agent--display-startup-header)))
    (with-current-buffer input-buf
      (setq pi-coding-agent--chat-buffer chat-buf))
    chat-buf))

;;;###autoload
(defun pi-coding-agent (&optional session)
  "Start or switch to pi coding agent session in current project.
With prefix arg, prompt for SESSION name to allow multiple sessions.
If already in a pi buffer and no SESSION specified, redisplays current session."
  (interactive
   (list (when current-prefix-arg
           (read-string "Session name: "))))
  (pi-coding-agent--check-dependencies)
  (let (chat-buf input-buf)
    (if (and (derived-mode-p 'pi-coding-agent-chat-mode 'pi-coding-agent-input-mode)
             (not session))
        ;; Already in pi buffer with no new session requested - use current session
        (setq chat-buf (pi-coding-agent--get-chat-buffer)
              input-buf (pi-coding-agent--get-input-buffer))
      ;; Find or create session for current directory
      (let ((dir (pi-coding-agent--session-directory)))
        (setq chat-buf (pi-coding-agent--setup-session dir session))
        (setq input-buf (buffer-local-value 'pi-coding-agent--input-buffer chat-buf))))
    ;; Display and focus
    (pi-coding-agent--display-buffers chat-buf input-buf)))

(provide 'pi-coding-agent)
;;; pi-coding-agent.el ends here
