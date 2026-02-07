;;; pi-coding-agent.el --- Emacs frontend for pi coding agent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pi-coding-agent
;; Keywords: ai llm ai-pair-programming tools
;; Version: 1.2.6
;; Package-Requires: ((emacs "28.1") (markdown-mode "2.6") (transient "0.9.0"))

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
;;   - pi coding agent 0.51.3 or later, installed and in PATH
;;
;; Optional Dependencies:
;;   - phscroll: Markdown tables that exceed the window width wrap awkwardly.
;;     phscroll enables horizontal scrolling so tables stay readable.
;;     Install from: https://github.com/misohena/phscroll
;;
;; Usage:
;;   M-x pi           Start a session in current project
;;   C-u M-x pi       Start a named session
;;
;; Key Bindings:
;;   Input buffer:
;;     C-c C-c        Send prompt (queues as follow-up if busy)
;;     C-c C-s        Queue steering (interrupts after current tool; busy only)
;;     C-c C-k        Abort streaming
;;     C-c C-p        Open menu
;;     C-c C-r        Resume session
;;     M-p / M-n      History navigation
;;     C-r            Incremental history search (like readline)
;;     TAB            Path/file completion
;;     @              File reference (search project files)
;;
;;   Chat buffer:
;;     n / p          Navigate messages
;;     TAB            Toggle tool output
;;     RET            Visit file at point (from tool blocks)
;;     C-c C-p        Open menu
;;
;; Editor Features:
;;   - File reference (@): Type @ to search project files (respects .gitignore)
;;   - Path completion (Tab): Complete relative paths, ../, ~/, etc.
;;   - Message queuing: Submit messages while agent is working:
;;       C-c C-c  queues follow-up (delivered after agent completes)
;;       C-c C-s  queues steering (interrupts after current tool)
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

;; Optional: phscroll for horizontal table scrolling
(require 'phscroll nil t)
(declare-function phscroll-mode "phscroll" (&optional arg))
(declare-function phscroll-region "phscroll" (beg end))

(defcustom pi-coding-agent-phscroll-offer-install t
  "Whether to offer installing `phscroll' for horizontal table scrolling.
When non-nil and phscroll is not installed, pi-coding-agent will
prompt once on first session start.  Set to nil to suppress."
  :type 'boolean
  :group 'pi-coding-agent)

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

(defcustom pi-coding-agent-visit-file-other-window t
  "Whether to open files in other window when visiting from tool blocks.
When non-nil, RET on a line in tool output opens in other window.
When nil, RET opens in the same window.
Prefix arg toggles the behavior."
  :type 'boolean
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-table-horizontal-scroll t
  "Whether to enable horizontal scrolling for wide tables.
When non-nil and `phscroll' is available, wide tables scroll
horizontally instead of wrapping awkwardly.

Requires the `phscroll' package (not on MELPA).
See URL `https://github.com/misohena/phscroll' for installation.

When phscroll is not available, tables wrap like other content."
  :type 'boolean
  :group 'pi-coding-agent)

;;;; Faces

(defface pi-coding-agent-timestamp
  '((t :inherit shadow))
  "Face for timestamps in message headers."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tool-name
  '((t :inherit font-lock-function-name-face :weight bold :slant italic))
  "Face for tool names (BASH, READ, etc.) in pi chat."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tool-command
  '((t :inherit font-lock-function-name-face :slant italic))
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

(defface pi-coding-agent-tool-block
  '((t :extend t))
  "Face for tool blocks.
Subtle blue-tinted background derived from the current theme."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tool-block-error
  '((t :inherit diff-removed :extend t))
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

;;;; Dynamic Face Computation

(require 'color)

(defun pi-coding-agent--blend-color (base target amount)
  "Blend BASE color toward TARGET by AMOUNT (0.0–1.0).
Returns a hex color string.  AMOUNT of 0.0 returns BASE unchanged;
1.0 returns TARGET."
  (apply #'color-rgb-to-hex
         (cl-mapcar (lambda (b tgt)
                      (+ (* (- 1.0 amount) b) (* amount tgt)))
                    (color-name-to-rgb base)
                    (color-name-to-rgb target))))

(defun pi-coding-agent--update-tool-block-face (&rest _)
  "Set `pi-coding-agent-tool-block' background from theme.
Blends the default background slightly toward blue, producing a
subtle tint that works with any theme.  Called from mode setup and
on theme changes."
  (condition-case nil
      (let ((bg (face-background 'default nil t)))
        (when (and bg (color-defined-p bg))
          (let* ((dark-p (< (nth 2 (apply #'color-rgb-to-hsl
                                          (color-name-to-rgb bg)))
                            0.5))
                 (tint (if dark-p "#5555cc" "#3333aa"))
                 (amount (if dark-p 0.12 0.08)))
            (set-face-attribute
             'pi-coding-agent-tool-block nil
             :background
             (pi-coding-agent--blend-color bg tint amount)))))
    (error nil)))

;; Recompute when theme changes (Emacs 29+)
(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions
            #'pi-coding-agent--update-tool-block-face))

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

(defsubst pi-coding-agent--tool-path (args)
  "Extract file path from tool ARGS.
Checks both :path and :file_path keys for compatibility."
  (or (plist-get args :path)
      (plist-get args :file_path)))

(defun pi-coding-agent--path-to-language (path)
  "Return language name for PATH based on file extension.
Returns \"text\" for unrecognized extensions to ensure consistent fencing."
  (when path
    (let ((ext (downcase (or (file-name-extension path) ""))))
      (or (cdr (assoc ext pi-coding-agent--extension-language-alist))
          "text"))))

;;;; Major Modes

(defvar pi-coding-agent-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'pi-coding-agent-quit)
    (define-key map (kbd "C-c C-p") #'pi-coding-agent-menu)
    (define-key map (kbd "n") #'pi-coding-agent-next-message)
    (define-key map (kbd "p") #'pi-coding-agent-previous-message)
    (define-key map (kbd "TAB") #'pi-coding-agent-toggle-tool-section)
    (define-key map (kbd "<tab>") #'pi-coding-agent-toggle-tool-section)
    (define-key map (kbd "RET") #'pi-coding-agent-visit-file)
    (define-key map (kbd "<return>") #'pi-coding-agent-visit-file)
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
  ;; Disable hl-line-mode: its post-command-hook overlay update causes
  ;; scroll oscillation in buffers with invisible text + variable heights.
  (setq-local global-hl-line-mode nil)
  (hl-line-mode -1)
  ;; Make window-point follow inserted text (like comint does).
  ;; This is key for natural scroll behavior during streaming.
  (setq-local window-point-insertion-type t)

  ;; Add wrap-prefix to blockquotes so wrapped lines show the indicator
  (font-lock-add-keywords nil '((pi-coding-agent--fontify-blockquote-wrap-prefix)) 'append)

  ;; Enable phscroll for horizontal table scrolling, offer install if missing
  (pi-coding-agent--maybe-install-phscroll)
  (when (pi-coding-agent--phscroll-available-p)
    (phscroll-mode 1))

  ;; Compute tool-block face from current theme
  (pi-coding-agent--update-tool-block-face)

  (add-hook 'kill-buffer-hook #'pi-coding-agent--cleanup-on-kill nil t))

(defun pi-coding-agent-complete ()
  "Complete at point, suppressing help text in the *Completions* buffer.
This wraps `completion-at-point' with `completion-show-help' bound to nil,
removing the instructional header that would otherwise appear."
  (interactive)
  (let ((completion-show-help nil))
    (completion-at-point)))

(defvar pi-coding-agent-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'pi-coding-agent-send)
    (define-key map (kbd "TAB") #'pi-coding-agent-complete)
    (define-key map (kbd "C-c C-k") #'pi-coding-agent-abort)
    (define-key map (kbd "C-c C-p") #'pi-coding-agent-menu)
    (define-key map (kbd "C-c C-r") #'pi-coding-agent-resume-session)
    (define-key map (kbd "M-p") #'pi-coding-agent-previous-input)
    (define-key map (kbd "M-n") #'pi-coding-agent-next-input)
    (define-key map (kbd "<C-up>") #'pi-coding-agent-previous-input)
    (define-key map (kbd "<C-down>") #'pi-coding-agent-next-input)
    (define-key map (kbd "C-r") #'pi-coding-agent-history-isearch-backward)
    ;; Message queuing (steering only - follow-up handled by C-c C-c)
    (define-key map (kbd "C-c C-s") #'pi-coding-agent-queue-steering)
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

;; History isearch state (for C-r incremental search)
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

;;;; History Isearch

(defun pi-coding-agent-history-isearch-backward ()
  "Search input history backward using isearch.
Incrementally search through history with matches appearing
directly in the input buffer, like readline."
  (interactive)
  (let ((ring (pi-coding-agent--input-ring)))
    (when (ring-empty-p ring)
      (user-error "No history"))
    ;; Save current input before starting (like comint-stored-incomplete-input)
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
  ;; Clean up isearch customizations
  (setq isearch-message-prefix-add nil)
  (setq-local isearch-search-fun-function #'isearch-search-fun-default)
  (setq-local isearch-wrap-function nil)
  (setq-local isearch-push-state-function nil)
  (kill-local-variable 'isearch-lazy-count)
  (remove-hook 'isearch-mode-end-hook #'pi-coding-agent--history-isearch-end t)
  ;; Restore original input if quit (C-g), keep history item if accepted
  (when isearch-mode-end-hook-quit
    (delete-region (point-min) (point-max))
    (insert (or pi-coding-agent--history-isearch-saved-input "")))
  ;; Reset state (unless suspended for later resume)
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
    ;; Restore saved input when index is nil
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

(define-derived-mode pi-coding-agent-input-mode text-mode "Pi-Input"
  "Major mode for composing pi prompts."
  :group 'pi-coding-agent
  (setq-local header-line-format '(:eval (pi-coding-agent--header-line-string)))
  ;; Completion-at-point functions (order matters: first match wins)
  (add-hook 'completion-at-point-functions #'pi-coding-agent--command-capf nil t)
  (add-hook 'completion-at-point-functions #'pi-coding-agent--file-reference-capf nil t)
  (add-hook 'completion-at-point-functions #'pi-coding-agent--path-capf nil t)
  ;; Auto-trigger completion after @ is typed
  (add-hook 'post-self-insert-hook #'pi-coding-agent--maybe-complete-at nil t)
  ;; History isearch setup (C-r incremental search)
  (add-hook 'isearch-mode-hook #'pi-coding-agent--history-isearch-setup nil t)
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

(defun pi-coding-agent--extract-last-usage (messages)
  "Extract usage from the last non-aborted assistant message in MESSAGES.
MESSAGES is a vector of message plists from get_messages RPC.
Returns the usage plist, or nil if no valid assistant message found.
Skips aborted messages as they may have incomplete usage data."
  (when (vectorp messages)
    (let ((i (1- (length messages)))
          (result nil))
      (while (and (>= i 0) (not result))
        (let ((msg (aref messages i)))
          (when (and (equal (plist-get msg :role) "assistant")
                     (not (equal (plist-get msg :stopReason) "aborted"))
                     (plist-get msg :usage))
            (setq result (plist-get msg :usage))))
        (setq i (1- i)))
      result)))

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

(defvar-local pi-coding-agent--streaming-tool-id nil
  "Tool call ID of overlay created via toolcall_start during LLM streaming.
Enables dedup guard in tool_execution_start to skip overlay creation
when the overlay was already created by the streaming event path.
Set at toolcall_start, consumed and cleared at tool_execution_start.")

(defvar-local pi-coding-agent--assistant-header-shown nil
  "Non-nil if Assistant header has been shown for current prompt.
Used to avoid duplicate headers during retry sequences.")

(defvar-local pi-coding-agent--followup-queue nil
  "List of follow-up messages queued while agent is busy.
Messages are added when user sends while streaming.
On agent_end, the first message is popped and sent as a normal prompt.
This is simpler than using pi's RPC follow_up command.")

(defvar-local pi-coding-agent--local-user-message nil
  "Text of user message we displayed locally, awaiting pi's echo.
Set when displaying a user message (normal send, follow-up).
Cleared when we receive message_start role=user from pi.
When nil and we receive message_start role=user, we display it.
When set but different from pi's message, we display pi's version
\(e.g., expanded template).")

(defvar-local pi-coding-agent--fontify-timer nil
  "Idle timer for periodic fontification during streaming.
Started on agent_start, stopped on agent_end.")

(defvar-local pi-coding-agent--last-fontified-pos nil
  "Position up to which we've fontified during streaming.
Used to avoid re-fontifying already-fontified text.")

(defvar-local pi-coding-agent--extension-status nil
  "Alist of extension status messages for header-line display.
Keys are extension identifiers (strings), values are status text.
Displayed in header-line after model/thinking info with | separator.")

(defvar-local pi-coding-agent--session-name nil
  "Cached session name for header-line display.
Extracted from session_info entries when session is loaded or switched.")

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

(defun pi-coding-agent--make-separator (label &optional timestamp)
  "Create a setext-style H1 heading separator with LABEL.
If TIMESTAMP (Emacs time value) is provided, append it after \" · \".
Returns a markdown setext heading: label line followed by === underline.
Fontification is handled by `gfm-mode' (inherits `markdown-header-face-1').

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
    (concat header-line "\n" underline)))

(defun pi-coding-agent--display-user-message (text &optional timestamp)
  "Display user message TEXT in the chat buffer.
If TIMESTAMP (Emacs time value) is provided, display it in the header.
Note: No blank line after setext underline - the hidden === provides
visual spacing when `markdown-hide-markup' is enabled."
  (pi-coding-agent--append-to-chat
   (concat "\n" (pi-coding-agent--make-separator "You" timestamp) "\n"
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
     (concat "\n" (pi-coding-agent--make-separator "Assistant") "\n"))
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
           ;; Strip leading newlines from first content after header
           (delta (if (and pi-coding-agent--message-start-marker
                          (= (marker-position pi-coding-agent--message-start-marker)
                             (marker-position pi-coding-agent--streaming-marker)))
                     (string-trim-left delta "\n+")
                   delta))
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
  "Finalize agent turn: normalize whitespace, handle abort, process queue.
Note: status is set to `idle' by the event handler."
  ;; Reset per-turn state for clean next turn.
  (setq pi-coding-agent--local-user-message nil)
  (setq pi-coding-agent--streaming-tool-id nil)
  ;; Capture abort state before clearing it - we need to know if we should skip queue
  (let ((was-aborted pi-coding-agent--aborted))
    (let ((inhibit-read-only t))
      ;; Clean up pending tool overlay if abort happened mid-tool
      (pi-coding-agent--tool-overlay-finalize 'pi-coding-agent-tool-block-error)
      ;; Show abort indicator if aborted, and clear queued messages
      ;; User abort means "stop everything" including queued follow-ups
      (when pi-coding-agent--aborted
        (pi-coding-agent--with-scroll-preservation
          (save-excursion
            (goto-char (point-max))
            ;; Remove trailing whitespace before adding indicator
            (skip-chars-backward " \t\n")
            (delete-region (point) (point-max))
            (insert "\n\n" (propertize "[Aborted]" 'face 'error) "\n")))
        (setq pi-coding-agent--aborted nil)
        (setq pi-coding-agent--followup-queue nil))
      ;; Normalize trailing newlines - ensure content ends with single newline.
      (pi-coding-agent--with-scroll-preservation
        (save-excursion
          (goto-char (point-max))
          (skip-chars-backward "\n")
          (delete-region (point) (point-max))
          (insert "\n"))))
    (pi-coding-agent--spinner-stop)
    (pi-coding-agent--fontify-timer-stop)
    (pi-coding-agent--refresh-header)
    ;; Check follow-up queue and send next message if any (unless aborted)
    (unless was-aborted
      (pi-coding-agent--process-followup-queue))))

(defun pi-coding-agent--prepare-and-send (text)
  "Prepare chat buffer state and send TEXT to pi.
For slash commands: don't display locally, let pi send expanded content.
For regular text: display locally for responsiveness.
The /compact command is handled specially by calling `pi-coding-agent-compact'.
Must be called with chat buffer current.
Status transitions are handled by pi events (agent_start, agent_end)."
  (cond
   ;; /compact is handled locally, invoking `pi-coding-agent-compact' directly
   ((or (string= text "/compact")
        (string-prefix-p "/compact " text))
    (let ((args (when (string-prefix-p "/compact " text)
                  (string-trim (substring text (length "/compact "))))))
      (pi-coding-agent-compact (and args (not (string-empty-p args)) args))))
   ;; Other slash commands: don't display locally, send to pi
   ((string-prefix-p "/" text)
    (pi-coding-agent--send-prompt text))
   ;; Regular text: display locally for responsiveness, then send
   (t
    (pi-coding-agent--display-user-message text (current-time))
    (setq pi-coding-agent--local-user-message text)
    (setq pi-coding-agent--assistant-header-shown nil)
    (pi-coding-agent--send-prompt text))))

(defun pi-coding-agent--process-followup-queue ()
  "Pop first message from follow-up queue and send it.
Does nothing if queue is empty.  Messages are processed in FIFO order."
  (when pi-coding-agent--followup-queue
    (let ((text (car (last pi-coding-agent--followup-queue))))
      (setq pi-coding-agent--followup-queue (butlast pi-coding-agent--followup-queue))
      (pi-coding-agent--prepare-and-send text))))

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

(defun pi-coding-agent--extension-ui-notify (event)
  "Handle notify method from EVENT."
  (let ((msg (plist-get event :message))
        (notify-type (plist-get event :notifyType)))
    (message "Pi: %s%s"
             (pcase notify-type
               ("warning" "⚠ ")
               ("error" "✗ ")
               (_ ""))
             msg)))

(defun pi-coding-agent--extension-ui-confirm (event proc)
  "Handle confirm method from EVENT, responding via PROC."
  (let* ((id (plist-get event :id))
         (title (plist-get event :title))
         (msg (plist-get event :message))
         ;; Don't add colon if title already ends with one
         (separator (if (string-suffix-p ":" title) " " ": "))
         (prompt (format "%s%s%s " title separator msg))
         (confirmed (yes-or-no-p prompt)))
    (when proc
      (pi-coding-agent--send-extension-ui-response proc
                     (list :type "extension_ui_response"
                           :id id
                           :confirmed (if confirmed t :json-false))))))

(defun pi-coding-agent--extension-ui-select (event proc)
  "Handle select method from EVENT, responding via PROC."
  (let* ((id (plist-get event :id))
         (title (plist-get event :title))
         (options (append (plist-get event :options) nil))
         (selected (completing-read (concat title " ") options nil t)))
    (when proc
      (pi-coding-agent--send-extension-ui-response proc
                     (list :type "extension_ui_response"
                           :id id
                           :value selected)))))

(defun pi-coding-agent--extension-ui-input (event proc)
  "Handle input method from EVENT, responding via PROC."
  (let* ((id (plist-get event :id))
         (title (plist-get event :title))
         (placeholder (plist-get event :placeholder))
         (value (read-string (concat title " ") placeholder)))
    (when proc
      (pi-coding-agent--send-extension-ui-response proc
                     (list :type "extension_ui_response"
                           :id id
                           :value value)))))

(defun pi-coding-agent--extension-ui-set-editor-text (event)
  "Handle set_editor_text method from EVENT."
  (let ((text (plist-get event :text)))
    (when-let ((input-buf pi-coding-agent--input-buffer))
      (when (buffer-live-p input-buf)
        (with-current-buffer input-buf
          (erase-buffer)
          (insert text))))))

(defun pi-coding-agent--extension-ui-set-status (event)
  "Handle setStatus method from EVENT."
  (let ((key (plist-get event :statusKey))
        (text (plist-get event :statusText)))
    (if text
        (setq pi-coding-agent--extension-status
              (cons (cons key text)
                    (assoc-delete-all key pi-coding-agent--extension-status)))
      (setq pi-coding-agent--extension-status
            (assoc-delete-all key pi-coding-agent--extension-status)))
    (force-mode-line-update t)))

(defun pi-coding-agent--extension-ui-unsupported (event proc)
  "Handle unsupported method from EVENT by sending cancelled via PROC."
  (when proc
    (pi-coding-agent--rpc-async proc
                   (list :type "extension_ui_response"
                         :id (plist-get event :id)
                         :cancelled t)
                   #'ignore)))

(defun pi-coding-agent--handle-extension-ui-request (event)
  "Handle extension_ui_request EVENT from pi.
Dispatches to appropriate handler based on method."
  (let ((method (plist-get event :method))
        (proc pi-coding-agent--process))
    (pcase method
      ("notify"         (pi-coding-agent--extension-ui-notify event))
      ("confirm"        (pi-coding-agent--extension-ui-confirm event proc))
      ("select"         (pi-coding-agent--extension-ui-select event proc))
      ("input"          (pi-coding-agent--extension-ui-input event proc))
      ("set_editor_text" (pi-coding-agent--extension-ui-set-editor-text event))
      ("setStatus"      (pi-coding-agent--extension-ui-set-status event))
      (_                (pi-coding-agent--extension-ui-unsupported event proc)))))

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
Also kills the linked input buffer.

Note: This runs from `kill-buffer-hook', which executes AFTER the kill
decision is made.  For proper cancellation support, use `pi-coding-agent-quit'
which asks upfront before any buffers are touched."
  (when (derived-mode-p 'pi-coding-agent-chat-mode)
    ;; Clean up process first (before killing input buffer)
    (when pi-coding-agent--process
      (pi-coding-agent--unregister-display-handler pi-coding-agent--process)
      (when (process-live-p pi-coding-agent--process)
        (delete-process pi-coding-agent--process)))
    ;; Kill linked input buffer
    (when (and pi-coding-agent--input-buffer (buffer-live-p pi-coding-agent--input-buffer))
      (let ((input-buf pi-coding-agent--input-buffer))
        ;; Clear reference to avoid infinite recursion
        (setq pi-coding-agent--input-buffer nil)
        (kill-buffer input-buf)))))

(defun pi-coding-agent--cleanup-input-on-kill ()
  "Clean up when input buffer is killed.
Also kills the linked chat buffer (which handles process cleanup).

Note: This runs from `kill-buffer-hook', which executes AFTER the kill
decision is made.  For proper cancellation support, use `pi-coding-agent-quit'
which asks upfront before any buffers are touched."
  (when (derived-mode-p 'pi-coding-agent-input-mode)
    (when (and pi-coding-agent--chat-buffer (buffer-live-p pi-coding-agent--chat-buffer))
      (let* ((chat-buf pi-coding-agent--chat-buffer)
             (proc (buffer-local-value 'pi-coding-agent--process chat-buf)))
        ;; Clear reference to avoid infinite recursion
        (setq pi-coding-agent--chat-buffer nil)
        ;; Disable process query to prevent prompting (we're already committed)
        (when (and proc (process-live-p proc))
          (set-process-query-on-exit-flag proc nil))
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
     (let* ((message (plist-get event :message))
            (role (plist-get message :role)))
       (pcase role
         ("user"
          ;; User message from pi - check if we displayed it locally
          (let* ((content (plist-get message :content))
                 (timestamp (plist-get message :timestamp))
                 (text (when content
                         (pi-coding-agent--extract-user-message-text content)))
                 (local-msg pi-coding-agent--local-user-message))
            ;; Clear local tracking
            (setq pi-coding-agent--local-user-message nil)
            ;; Display if: no local message, OR pi's message differs (expanded template)
            (when (and text
                       (or (null local-msg)
                           (not (string= text local-msg))))
              (pi-coding-agent--display-user-message
               text
               (pi-coding-agent--ms-to-time timestamp))
              ;; Reset so next assistant message shows its header
              (setq pi-coding-agent--assistant-header-shown nil))))
         ("custom"
          ;; Custom message from extension (e.g., /pisay)
          ;; Display content directly if display flag is set
          (when (plist-get message :display)
            (let ((content (plist-get message :content)))
              (when (and content (stringp content) (> (length content) 0))
                (pi-coding-agent--append-to-chat (concat "\n" content "\n"))
                ;; Reset so next assistant message shows its header
                (setq pi-coding-agent--assistant-header-shown nil)))))
         (_
          ;; Assistant message - show header if needed, reset markers
          (unless pi-coding-agent--assistant-header-shown
            (pi-coding-agent--append-to-chat
             (concat "\n" (pi-coding-agent--make-separator "Assistant") "\n"))
            (setq pi-coding-agent--assistant-header-shown t))
          (setq pi-coding-agent--message-start-marker (copy-marker (point-max) nil))
          (setq pi-coding-agent--streaming-marker (copy-marker (point-max) t))))))
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
         ("toolcall_start"
          ;; LLM began generating a tool call — create overlay immediately.
          ;; Guard: only one streaming tool at a time (sequential execution).
          (unless pi-coding-agent--streaming-tool-id
            (when-let* ((tool-call (pi-coding-agent--extract-tool-call
                                    event msg-event)))
              ;; Clear fontification buffer so incremental sync starts
              ;; fresh for each tool call
              (pi-coding-agent--fontify-reset
               (plist-get tool-call :arguments))
              (pi-coding-agent--display-tool-start
               (plist-get tool-call :name)
               (plist-get tool-call :arguments))
              (setq pi-coding-agent--streaming-tool-id
                    (plist-get tool-call :id)))))
         ("toolcall_delta"
          ;; LLM streaming tool call args — update header and stream content.
          ;; Header updates here for responsiveness (path appears as soon as
          ;; the LLM generates it).  Overlay path for navigation is only set
          ;; at tool_execution_start with authoritative args.
          (when pi-coding-agent--streaming-tool-id
            (when-let* ((tool-call (pi-coding-agent--extract-tool-call
                                    event msg-event)))
              ;; Update header when path/command becomes available
              (pi-coding-agent--display-tool-update-header
               (plist-get tool-call :name)
               (plist-get tool-call :arguments))
              ;; For write: stream growing file content with syntax highlighting
              (when (equal (plist-get tool-call :name) "write")
                (when-let* ((args (plist-get tool-call :arguments))
                            (file-content (plist-get args :content)))
                  (pi-coding-agent--display-tool-streaming-text
                   file-content pi-coding-agent-tool-preview-lines
                   (pi-coding-agent--path-to-language
                    (pi-coding-agent--tool-path args))))))))
         ("toolcall_end")  ; No-op: ID must survive until tool_execution_start
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
       ;; Skip overlay creation if toolcall_start already created it
       (if (and pi-coding-agent--streaming-tool-id
                (equal tool-call-id pi-coding-agent--streaming-tool-id))
           (progn
             (setq pi-coding-agent--streaming-tool-id nil)
             ;; Update header and path from authoritative args.
             ;; During streaming, header shows placeholder ("...") since
             ;; delta args may be partial.  Now we have the real args.
             (let ((tool-name (plist-get event :toolName)))
               (pi-coding-agent--display-tool-update-header tool-name args))
             (when-let* ((path (pi-coding-agent--tool-path args))
                         (ov pi-coding-agent--pending-tool-overlay))
               (overlay-put ov 'pi-coding-agent-tool-path path)))
         (pi-coding-agent--display-tool-start (plist-get event :toolName) args))))
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
     (if (pi-coding-agent--normalize-boolean (plist-get event :aborted))
         (progn
           (message "Pi: Auto-compaction cancelled")
           ;; Clear queue on abort (user wanted to stop)
           (setq pi-coding-agent--followup-queue nil))
       (when-let ((result (plist-get event :result)))
         (pi-coding-agent--handle-compaction-success
          (plist-get result :tokensBefore)
          (plist-get result :summary)
          (pi-coding-agent--ms-to-time (plist-get result :timestamp))))
       ;; Process followup queue after successful compaction
       (pi-coding-agent--process-followup-queue)))
    ("agent_end"
     (pi-coding-agent--display-agent-end))
    ("auto_retry_start"
     (pi-coding-agent--display-retry-start event))
    ("auto_retry_end"
     (pi-coding-agent--display-retry-end event))
    ("extension_error"
     (pi-coding-agent--display-extension-error event))
    ("extension_ui_request"
     (pi-coding-agent--handle-extension-ui-request event))))

;;;; Sending Prompts

(defun pi-coding-agent-send ()
  "Send the current input buffer contents to pi.
Clears the input buffer after sending.  Does nothing if buffer is empty.
If pi is busy (streaming or compacting), adds to local follow-up queue.
The /compact command is handled locally; other slash commands sent to pi."
  (interactive)
  (let* ((text (string-trim (buffer-string)))
         (chat-buf (pi-coding-agent--get-chat-buffer))
         (status (and chat-buf (buffer-local-value 'pi-coding-agent--status chat-buf)))
         (busy (and status (memq status '(streaming sending compacting)))))
    (cond
     ;; Empty input - do nothing
     ((string-empty-p text) nil)
     ;; Busy (streaming or compacting) - add to local follow-up queue
     (busy
      (pi-coding-agent--history-add text)
      (setq pi-coding-agent--input-ring-index nil
            pi-coding-agent--input-saved nil)
      (erase-buffer)
      ;; Add to queue in chat buffer
      (with-current-buffer chat-buf
        (push text pi-coding-agent--followup-queue))
      (message "Pi: Message queued (will send after current response)"))
     ;; Normal send
     (t
      (pi-coding-agent--history-add text)
      (setq pi-coding-agent--input-ring-index nil
            pi-coding-agent--input-saved nil)
      (erase-buffer)
      (with-current-buffer chat-buf
        (pi-coding-agent--prepare-and-send text))))))

(defun pi-coding-agent--send-prompt (text)
  "Send TEXT as a prompt to the pi process.
Slash commands are sent literally - pi handles expansion.
Shows an error message if process is unavailable."
  (let ((proc (pi-coding-agent--get-process))
        (chat-buf (pi-coding-agent--get-chat-buffer)))
    (cond
     ((null proc)
      (pi-coding-agent--abort-send chat-buf)
      (message "Pi: No process available - try M-x pi-coding-agent-reload or C-c C-p R"))
     ((not (process-live-p proc))
      (pi-coding-agent--abort-send chat-buf)
      (message "Pi: Process died - try M-x pi-coding-agent-reload or C-c C-p R"))
     (t
      (pi-coding-agent--rpc-async proc
                     (list :type "prompt" :message text)
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
    ;; Ask user ONCE if there's a live process - abort entirely if cancelled
    (when (and proc-live
               (process-query-on-exit-flag proc)
               (not (yes-or-no-p "Pi session has a running process; quit anyway? ")))
      (user-error "Quit cancelled"))
    ;; User confirmed (or no process) - disable query flag to prevent double-ask
    (when proc-live
      (set-process-query-on-exit-flag proc nil))
    ;; Collect windows showing the input buffer (we'll delete these)
    (when (buffer-live-p input-buf)
      (setq input-windows (get-buffer-window-list input-buf nil t)))
    ;; Kill chat buffer first - its cleanup hook will kill input buffer
    ;; This avoids the input->chat->confirm->fail->orphaned-input problem
    (when (buffer-live-p chat-buf)
      (kill-buffer chat-buf))
    ;; Input buffer should already be dead from cleanup hook, but ensure it
    (when (buffer-live-p input-buf)
      (kill-buffer input-buf))
    ;; Delete input windows only (merges space with neighbors)
    (dolist (win input-windows)
      (when (window-live-p win)
        (ignore-errors (delete-window win))))))

(defun pi-coding-agent--markdown-visible-width (s)
  "Return display width of S with markdown markup removed.
Strips markdown syntax that `markdown-hide-markup' would hide:
- Images: ![alt](url) -> alt
- Links: [text](url) -> text
- Bold: **text** -> text
- Italic: *text* -> text
- Code: `text` -> text
- Strikethrough: ~~text~~ -> text

Order matters for overlapping patterns: images before links (both
use brackets), bold before italic (both use asterisks)."
  (let ((result s))
    (setq result (replace-regexp-in-string "!\\[\\([^]]*\\)\\]([^)]*)" "\\1" result))
    (setq result (replace-regexp-in-string "\\[\\([^]]*\\)\\]([^)]*)" "\\1" result))
    (setq result (replace-regexp-in-string "\\*\\*\\([^*]+\\)\\*\\*" "\\1" result))
    (setq result (replace-regexp-in-string "\\*\\([^* \t\n]+\\)\\*" "\\1" result))
    (setq result (replace-regexp-in-string "`\\([^`]+\\)`" "\\1" result))
    (setq result (replace-regexp-in-string "~~\\([^~]+\\)~~" "\\1" result))
    (string-width result)))

(defun pi-coding-agent--table-pad-cell (cell width fmt)
  "Pad CELL to WIDTH using format FMT, accounting for hidden markup.
FMT is one of l (left), r (right), c (center), or nil (left).
Unlike `format`, this pads based on visible width, not raw length."
  (let* ((visible-width (pi-coding-agent--markdown-visible-width cell))
         (padding-needed (max 0 (- width visible-width))))
    (pcase fmt
      ('r (concat (make-string padding-needed ?\s) cell))
      ('c (let ((left-pad (/ padding-needed 2)))
            (concat (make-string left-pad ?\s)
                    cell
                    (make-string (- padding-needed left-pad) ?\s))))
      (_ (concat cell (make-string padding-needed ?\s))))))

(defun pi-coding-agent--table-align-raw (cells fmtspec widths)
  "Format CELLS according to FMTSPEC and WIDTHS, using visible width for padding.
This replaces `markdown-table-align-raw' to handle hidden markdown markup."
  (string-join
   (cl-mapcar (lambda (cell fmt width)
                (concat " " (pi-coding-agent--table-pad-cell cell width fmt) " "))
              cells fmtspec widths)
   "|"))

(defun pi-coding-agent--align-tables-in-region (start end)
  "Align all markdown tables between START and END.
Uses visible text width for column sizing, accounting for hidden markup."
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (re-search-forward "^|" end t))
      (when (markdown-table-at-point-p)
        ;; Override markdown's functions to use visible width
        (cl-letf (((symbol-function 'markdown--string-width)
                   #'pi-coding-agent--markdown-visible-width)
                  ((symbol-function 'markdown-table-align-raw)
                   #'pi-coding-agent--table-align-raw))
          (markdown-table-align))))))

(defun pi-coding-agent--phscroll-available-p ()
  "Return non-nil if phscroll is available and enabled."
  (and pi-coding-agent-table-horizontal-scroll
       (featurep 'phscroll)))

(defun pi-coding-agent--maybe-install-phscroll ()
  "Offer to install phscroll when horizontal scroll is wanted but missing.
On Emacs 29+, offer to install via `package-vc-install'.
On Emacs 28, show the URL.  If declined, suppress future prompts
by saving `pi-coding-agent-phscroll-offer-install' to nil."
  (when (and pi-coding-agent-table-horizontal-scroll
             pi-coding-agent-phscroll-offer-install
             (not (featurep 'phscroll))
             (not noninteractive))
    (if (fboundp 'package-vc-install)
        (if (y-or-n-p "Install `phscroll' for horizontal table scrolling? ")
            (progn
              (package-vc-install "https://github.com/misohena/phscroll")
              (require 'phscroll))
          (customize-save-variable 'pi-coding-agent-phscroll-offer-install nil))
      (message "pi-coding-agent: horizontal table scrolling requires `phscroll': \
https://github.com/misohena/phscroll")
      (customize-save-variable 'pi-coding-agent-phscroll-offer-install nil))))

(defun pi-coding-agent--apply-phscroll-to-tables (start end)
  "Apply horizontal scrolling to markdown tables between START and END.
Does nothing if phscroll is not available or not enabled.

Must be called AFTER `font-lock-ensure' so that invisible text
properties are set on hidden markup.  Phscroll caches character
widths when regions are created, so markup must already be hidden."
  (when (pi-coding-agent--phscroll-available-p)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^|" end t)
        (let ((table-start (line-beginning-position))
              table-end)
          ;; Find end of table (consecutive lines starting with |)
          ;; Must go to line start since re-search left point after the |
          (goto-char table-start)
          (while (and (not (eobp))
                      (looking-at "^|"))
            (forward-line 1))
          (setq table-end (point))
          ;; Apply phscroll if table has multiple lines
          (when (> table-end table-start)
            (phscroll-region table-start table-end)))))))

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
        (font-lock-ensure start (marker-position pi-coding-agent--streaming-marker))
        (let ((inhibit-read-only t))
          (pi-coding-agent--apply-phscroll-to-tables start (marker-position pi-coding-agent--streaming-marker)))))))

;;;; Tool Output

(defun pi-coding-agent--truncate-to-visual-lines (content max-lines width)
  "Truncate CONTENT to fit within MAX-LINES visual lines at WIDTH.
Also respects `pi-coding-agent-preview-max-bytes'.
Strips blank lines for compact display but tracks original line numbers.

Returns a plist with:
  :content      - the truncated content (or original if no truncation)
  :visual-lines - number of visual lines in result
  :hidden-lines - raw lines hidden (including stripped blanks)
  :line-map     - vector mapping displayed line to original line number"
  (let* ((all-lines (split-string (string-trim-right content "\n+") "\n"))
         (total-raw-lines (length all-lines))
         (visual-count 0)
         (byte-count 0)
         (max-bytes pi-coding-agent-preview-max-bytes)
         (result-lines nil)
         (line-map nil)  ; list of original line numbers for kept lines
         (truncated-first-line nil)
         (original-line-num 0))
    ;; Accumulate non-blank lines until we'd exceed limits
    (catch 'done
      (dolist (line all-lines)
        (setq original-line-num (1+ original-line-num))
        ;; Skip blank lines (they don't count toward visual limit)
        (unless (string-empty-p line)
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
            (push line result-lines)
            (push original-line-num line-map)))))
    (let* ((kept-lines (nreverse result-lines))
           (line-map-vec (vconcat (nreverse line-map)))
           (last-displayed (if (> (length line-map-vec) 0)
                               (aref line-map-vec (1- (length line-map-vec)))
                             0))
           (hidden (- total-raw-lines last-displayed)))
      (list :content (string-join kept-lines "\n")
            :visual-lines visual-count
            ;; Report hidden lines; truncated first line means there's hidden content even with 1 line
            :hidden-lines (if (and truncated-first-line (= hidden 0)) 1 hidden)
            :line-map line-map-vec))))

(defun pi-coding-agent--tool-overlay-create (tool-name &optional path)
  "Create overlay for tool block TOOL-NAME at point.
Optional PATH stores the file path for navigation.
Returns the overlay.  The overlay uses rear-advance so it
automatically extends when content is inserted at its end."
  (let ((ov (make-overlay (point) (point) nil nil t)))
    (overlay-put ov 'pi-coding-agent-tool-block t)
    (overlay-put ov 'pi-coding-agent-tool-name tool-name)
    (overlay-put ov 'face 'pi-coding-agent-tool-block)
    (when path
      (overlay-put ov 'pi-coding-agent-tool-path path))
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
                                   'pi-coding-agent-header-end))
          (path (overlay-get pi-coding-agent--pending-tool-overlay
                             'pi-coding-agent-tool-path))
          (offset (overlay-get pi-coding-agent--pending-tool-overlay
                               'pi-coding-agent-tool-offset))
          (line-map (overlay-get pi-coding-agent--pending-tool-overlay
                                 'pi-coding-agent-line-map)))
      (delete-overlay pi-coding-agent--pending-tool-overlay)
      (let ((ov (make-overlay start end nil nil nil)))  ; rear-advance=nil
        (overlay-put ov 'pi-coding-agent-tool-block t)
        (overlay-put ov 'pi-coding-agent-tool-name tool-name)
        (overlay-put ov 'pi-coding-agent-header-end header-end)
        (when path
          (overlay-put ov 'pi-coding-agent-tool-path path))
        (when offset
          (overlay-put ov 'pi-coding-agent-tool-offset offset))
        (when line-map
          (overlay-put ov 'pi-coding-agent-line-map line-map))
        (overlay-put ov 'face face)))
    (setq pi-coding-agent--pending-tool-overlay nil)))

(defun pi-coding-agent--tool-header (tool-name args)
  "Return propertized header for tool TOOL-NAME with ARGS.
The tool name prefix uses `pi-coding-agent-tool-name' face and
the arguments use `pi-coding-agent-tool-command' face.
Uses `font-lock-face' to survive gfm-mode refontification."
  (let ((path (pi-coding-agent--tool-path args)))
    (pcase tool-name
      ("bash"
       (let ((cmd (or (plist-get args :command) "...")))
         (concat (propertize "$" 'font-lock-face 'pi-coding-agent-tool-name)
                 (propertize (concat " " cmd) 'font-lock-face 'pi-coding-agent-tool-command))))
      ((or "read" "write" "edit")
       (concat (propertize tool-name 'font-lock-face 'pi-coding-agent-tool-name)
               (propertize (concat " " (or path "...")) 'font-lock-face 'pi-coding-agent-tool-command)))
      (_ (propertize tool-name 'font-lock-face 'pi-coding-agent-tool-name)))))

(defun pi-coding-agent--display-tool-start (tool-name args)
  "Display header for tool TOOL-NAME with ARGS and create overlay."
  (let* ((header-display (pi-coding-agent--tool-header tool-name args))
         (path (pi-coding-agent--tool-path args))
         (inhibit-read-only t))
    (pi-coding-agent--with-scroll-preservation
      (save-excursion
        (goto-char (point-max))
        ;; Terminate current line if mid-line (e.g., text delta without
        ;; trailing newline before toolcall_start)
        (unless (bolp)
          (insert "\n"))
        ;; Add blank line before tool if previous line has content
        (unless (save-excursion
                  (forward-line -1)
                  (looking-at-p "^$"))
          (insert "\n"))
        ;; Create overlay at start of tool block, storing path for navigation
        (setq pi-coding-agent--pending-tool-overlay
              (pi-coding-agent--tool-overlay-create tool-name path))
        (insert header-display "\n")
        ;; Store header end position for correct deletion in updates
        ;; (header may span multiple lines if command contains newlines)
        (overlay-put pi-coding-agent--pending-tool-overlay
                     'pi-coding-agent-header-end (point-marker))))))

(defun pi-coding-agent--display-tool-update-header (tool-name args)
  "Update the header of the pending tool overlay for TOOL-NAME with ARGS.
Replaces the header text when it has changed (e.g., when authoritative
args arrive at tool_execution_start after streaming placeholder)."
  (when pi-coding-agent--pending-tool-overlay
    (let* ((new-header (pi-coding-agent--tool-header tool-name args))
           (ov pi-coding-agent--pending-tool-overlay)
           (ov-start (overlay-start ov))
           (header-end (overlay-get ov 'pi-coding-agent-header-end)))
      (when (and ov-start header-end)
        (let ((old-header (buffer-substring-no-properties
                           ov-start (1- (marker-position header-end)))))
          (unless (string= old-header (substring-no-properties new-header))
            (let ((inhibit-read-only t)
                  (inhibit-modification-hooks t))
              (pi-coding-agent--with-scroll-preservation
                (save-excursion
                  (goto-char ov-start)
                  (delete-region ov-start (1- (marker-position header-end)))
                  (insert new-header))))))))))

(defun pi-coding-agent--extract-tool-call (event msg-event)
  "Extract toolCall from EVENT using contentIndex in MSG-EVENT.
Returns the toolCall plist from message.content, or nil if not found."
  (let* ((content-index (plist-get msg-event :contentIndex))
         (content-vec (plist-get (plist-get event :message) :content))
         (tool-call (and (vectorp content-vec)
                         (< content-index (length content-vec))
                         (aref content-vec content-index))))
    (when (and tool-call (equal (plist-get tool-call :type) "toolCall"))
      tool-call)))

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

(defun pi-coding-agent--extract-user-message-text (content)
  "Extract text from user message CONTENT.
CONTENT is a vector of content blocks from a user message.
Returns the concatenated text, or nil if empty."
  (let ((text (pi-coding-agent--extract-text-from-content content)))
    (unless (string-empty-p text) text)))

(defun pi-coding-agent--get-tail-lines (content n)
  "Get last N non-blank lines from CONTENT by scanning backward.
Blank lines are included in the returned content but do not count
toward N, so downstream consumers that strip blanks still get N
content lines.
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
      ;; Find N newlines from the end, skipping blank-line boundaries.
      ;; A newline at `pos' leads to a blank line when content[pos+1]
      ;; is also a newline — that boundary doesn't add a content line.
      (while (and (> pos 0) (< newlines-found n))
        (setq pos (1- pos))
        (when (and (eq (aref content pos) ?\n)
                   (not (eq (aref content (1+ pos)) ?\n)))
          (setq newlines-found (1+ newlines-found))))
      ;; Adjust pos to start after the Nth newline
      (when (and (> pos 0) (eq (aref content pos) ?\n))
        (setq pos (1+ pos)))
      ;; Return tail and whether there's hidden content
      (cons (substring content pos) (> pos 0)))))

(defun pi-coding-agent--display-tool-streaming-text (raw-text max-lines &optional lang)
  "Display RAW-TEXT as streaming content in pending tool overlay.
Shows rolling tail of output, truncated to MAX-LINES visual lines.
Previous streaming content is replaced.

When LANG is non-nil, uses incremental fontification: the full
content is synced into a cached buffer where the language's major
mode provides syntax context.  The visible tail is then extracted
with text properties preserved, giving correct highlighting even
when multi-line constructs (docstrings, block comments) start
above the visible window.

Inhibits modification hooks to prevent jit-lock from scanning the
full buffer on each delta."
  (when (and pi-coding-agent--pending-tool-overlay
             raw-text
             (not (string-empty-p raw-text)))
    (when lang
      (pi-coding-agent--fontify-sync raw-text lang))
    (let* ((tail-result
            (if lang
                (pi-coding-agent--fontify-buffer-tail lang max-lines)
              (pi-coding-agent--get-tail-lines raw-text max-lines)))
           (tail-content (car tail-result))
           (has-hidden (cdr tail-result))
           (truncation (unless lang
                         (pi-coding-agent--truncate-to-visual-lines
                          tail-content max-lines (or (window-width) 80))))
           ;; Normalize: fontify-buffer-tail returns content without
           ;; trailing newline, get-tail-lines/truncate may include one.
           (display-content
            (string-trim-right
             (or (if lang tail-content (plist-get truncation :content)) "")
             "\n+"))
           (show-hidden-indicator
            (or has-hidden
                (and truncation
                     (> (plist-get truncation :hidden-lines) 0))))
           (inhibit-read-only t)
           (inhibit-modification-hooks t))
      ;; Nothing to display yet (e.g., first delta before any newline)
      (unless (string-empty-p display-content)
        (pi-coding-agent--with-scroll-preservation
          (save-excursion
            (let* ((ov-end (overlay-end pi-coding-agent--pending-tool-overlay))
                   (header-end (overlay-get pi-coding-agent--pending-tool-overlay
                                            'pi-coding-agent-header-end)))
              ;; Delete previous streaming content (everything after header)
              (when (and header-end (< header-end ov-end))
                (delete-region header-end ov-end))
              ;; Insert new streaming content
              (goto-char (overlay-end pi-coding-agent--pending-tool-overlay))
              (when show-hidden-indicator
                (insert (propertize "... (earlier output)\n"
                                    'face 'pi-coding-agent-collapsed-indicator)))
              (let ((content-start (point)))
                (insert (if lang
                            display-content
                          (pi-coding-agent--render-tool-content
                           display-content nil)))
                (insert "\n")
                ;; Mark pre-fontified content so jit-lock won't override
                ;; our syntax faces with gfm-mode faces on redisplay.
                ;; Also layer markdown-code-face underneath so the text
                ;; uses fixed-pitch font, matching completed code blocks.
                (when lang
                  (put-text-property content-start (point)
                                     'fontified t)
                  (add-face-text-property content-start (point)
                                          'markdown-code-face t))))))))))

(defun pi-coding-agent--display-tool-update (partial-result)
  "Display PARTIAL-RESULT as streaming output in pending tool overlay.
PARTIAL-RESULT has same structure as tool result: plist with :content.
Extracts text from content blocks and delegates to
`pi-coding-agent--display-tool-streaming-text'."
  (when (and pi-coding-agent--pending-tool-overlay partial-result)
    (let* ((content-blocks (plist-get partial-result :content))
           (raw-output (pi-coding-agent--extract-text-from-content content-blocks)))
      (pi-coding-agent--display-tool-streaming-text
       raw-output pi-coding-agent-bash-preview-lines))))

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

(defsubst pi-coding-agent--fontify-buffer-name (lang)
  "Return the name of the fontification cache buffer for LANG."
  (format " *pi-fontify:%s*" lang))

(defun pi-coding-agent--fontify-reset (args)
  "Clear the fontification buffer for the language implied by ARGS.
Called at `toolcall_start' so each tool call starts with a fresh
buffer, preventing stale content from a previous call from being
treated as a matching prefix during incremental sync."
  (when-let* ((lang (pi-coding-agent--path-to-language
                     (pi-coding-agent--tool-path args)))
              (buf (get-buffer (pi-coding-agent--fontify-buffer-name lang))))
    (with-current-buffer buf
      (erase-buffer))))

(defun pi-coding-agent--fontify-sync (content lang)
  "Sync CONTENT into the fontification buffer for LANG.
Appends only the new text into a persistent buffer.  Fontification
runs only on complete lines (up to the last newline) to avoid
incorrect keyword matching on partial tokens.

The buffer always accumulates content regardless of whether the
language mode is available, so `pi-coding-agent--fontify-buffer-tail'
can extract the tail even for languages without an installed mode."
  (condition-case nil
      (let ((buf (get-buffer-create
                  (pi-coding-agent--fontify-buffer-name lang))))
        (with-current-buffer buf
          ;; Activate language mode once (best-effort for fontification).
          (let ((mode (and lang (markdown-get-lang-mode lang))))
            (when (and mode (fboundp mode) (not (eq major-mode mode)))
              (let ((inhibit-message t))
                (ignore-errors
                  (delay-mode-hooks (funcall mode))))
              (font-lock-set-defaults)))
          (let ((buf-size (buffer-size))
                (new-size (length content)))
            (cond
             ((> new-size buf-size)
              ;; Common case: content grew — append delta.
              (goto-char (point-max))
              (let ((start (point)))
                (insert (substring content buf-size))
                ;; Fontify only up to the last complete line so partial
                ;; tokens don't confuse font-lock keyword regexps.
                (let ((fontify-end (save-excursion
                                     (goto-char (point-max))
                                     (line-beginning-position))))
                  (when (> fontify-end start)
                    (ignore-errors
                      (font-lock-default-fontify-region
                       start fontify-end nil))))))
             ;; (= new-size buf-size): content unchanged — nothing to do.
             ((< new-size buf-size)
              ;; Content shrank (shouldn't happen) — full reset.
              (erase-buffer)
              (insert content)
              (ignore-errors
                (font-lock-default-fontify-region
                 (point-min) (point-max) nil)))))))
    (error nil)))

(defun pi-coding-agent--fontify-buffer-tail (lang n)
  "Extract last N non-blank complete lines from the LANG fontification buffer.
Only includes lines terminated by a newline — the trailing partial
line (if any) is excluded so the visible preview has a stable line
count that doesn't fluctuate as partial tokens stream in.

Blank lines are excluded from the returned content entirely — they
don't count toward N and are not included in the result.  This
ensures the display height is always exactly min(N, non-blank-lines),
preventing cursor jumping when the tail window moves over regions
with varying numbers of blank lines.

Returns (CONTENT . HAS-HIDDEN) where CONTENT is a string with text
properties preserved.  HAS-HIDDEN is non-nil when earlier lines
exist above the returned tail.
Returns nil if the buffer doesn't exist or has no complete lines."
  (let ((buf (get-buffer (pi-coding-agent--fontify-buffer-name lang))))
    (when (and buf (> (buffer-size buf) 0))
      (with-current-buffer buf
        ;; Find end of last complete line (just before the trailing
        ;; partial line, if any).  This is the last newline position.
        (goto-char (point-max))
        (when (re-search-backward "\n" nil t)
          (let ((end (point))
                (lines-found 0)
                (line-strings nil))
            ;; Collect the last complete line (the one terminated by end).
            (forward-line 0)
            (unless (looking-at-p "^$")
              (push (buffer-substring (point) end) line-strings)
              (setq lines-found (1+ lines-found)))
            ;; Walk backward collecting non-blank lines.
            (while (and (> (point) (point-min))
                        (< lines-found n))
              (forward-line -1)
              (unless (looking-at-p "^$")
                (let ((line-end (save-excursion (end-of-line) (point))))
                  (push (buffer-substring (point) line-end) line-strings)
                  (setq lines-found (1+ lines-found)))))
            (when line-strings
              (cons (mapconcat #'identity line-strings "\n")
                    (> (point) (point-min))))))))))

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
      ;; Store offset for read tool (used for line number calculation)
      (when-let ((offset (plist-get args :offset)))
        (when pi-coding-agent--pending-tool-overlay
          (overlay-put pi-coding-agent--pending-tool-overlay
                       'pi-coding-agent-tool-offset offset)))
      ;; Store line map for navigation (maps displayed line to original line)
      (when-let ((line-map (plist-get truncation :line-map)))
        (when pi-coding-agent--pending-tool-overlay
          (overlay-put pi-coding-agent--pending-tool-overlay
                       'pi-coding-agent-line-map line-map)))
      ;; Finalize overlay - replace with non-rear-advance version
      (pi-coding-agent--tool-overlay-finalize
       (if is-error 'pi-coding-agent-tool-block-error 'pi-coding-agent-tool-block))
      ;; Add trailing newline for spacing after tool block
      (insert "\n"))))

(defun pi-coding-agent--toggle-tool-output (button)
  "Toggle between preview and full content for BUTTON.
Preserves window scroll position during the toggle."
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
        ;; Save window positions relative to content-start
        ;; Windows before the tool block: save absolute position
        ;; Windows inside tool block: will use header position after toggle
        (let* ((content-start header-end)
               (block-start (car bounds))
               (saved-windows
                (mapcar (lambda (w)
                          (let ((ws (window-start w)))
                            (list w ws (window-point w)
                                  ;; Flag: was window-start before content area?
                                  (< ws content-start))))
                        (get-buffer-window-list (current-buffer) nil t))))
          ;; Delete from content start to after button
          (delete-region content-start (1+ btn-end))
          (goto-char content-start)
          ;; Toggle: if currently expanded, show collapsed (and vice versa)
          (pi-coding-agent--insert-tool-content-with-toggle
           preview-content full-content lang is-edit-diff hidden-count (not expanded))
          ;; Ensure fontification of inserted content (JIT font-lock is lazy)
          (font-lock-ensure content-start (point))
          ;; Update overlay to include new content
          (move-overlay ov block-start (point))
          ;; Restore window positions
          (dolist (win-state saved-windows)
            (let ((win (nth 0 win-state))
                  (old-start (nth 1 win-state))
                  (old-point (nth 2 win-state))
                  (was-before-content (nth 3 win-state)))
              (when (window-live-p win)
                (if was-before-content
                    ;; Window was before tool content - restore exactly
                    (progn
                      (set-window-start win old-start t)
                      (set-window-point win (min old-point (point-max))))
                  ;; Window was inside tool content - show from block start
                  (set-window-start win block-start t)
                  (set-window-point win block-start))))))))))

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

;;;; File Navigation

(defun pi-coding-agent--diff-line-at-point ()
  "Extract line number from diff line at point.
Returns the line number if point is on a +/- diff line, nil otherwise.
Diff format: [+-] LINENUM content (e.g., '+ 7     code' or '-12     code')."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[+-] *\\([0-9]+\\)")
      (string-to-number (match-string 1)))))

(defun pi-coding-agent--code-block-line-at-point ()
  "Return line number within code block content at point.
Searches backward for opening fence (```), counts lines from there.
Returns nil if not inside a code block or if on the fence line itself."
  (save-excursion
    (let ((orig-line (line-number-at-pos)))
      (beginning-of-line)
      (when (re-search-backward "^```" nil t)
        (let ((fence-line (line-number-at-pos)))
          ;; Only return line number if we're after the fence line
          (when (> orig-line fence-line)
            (- orig-line fence-line)))))))

(defun pi-coding-agent--tool-line-at-point (overlay)
  "Calculate file line number at point for tool OVERLAY.
For edit diffs: parse line from +/- format.
For read/write: count lines from header + apply line-map for stripped blanks."
  (let ((tool-name (overlay-get overlay 'pi-coding-agent-tool-name))
        (offset (or (overlay-get overlay 'pi-coding-agent-tool-offset) 1))
        (line-map (overlay-get overlay 'pi-coding-agent-line-map))
        (header-end (overlay-get overlay 'pi-coding-agent-header-end)))
    (or
     ;; Edit diff format: explicit line number
     (and (equal tool-name "edit") (pi-coding-agent--diff-line-at-point))
     ;; Use line-map if available (handles stripped blank lines)
     (when (and line-map header-end)
       (save-excursion
         (let* ((current-line (line-number-at-pos))
                (header-line (line-number-at-pos header-end))
                (lines-from-header (- current-line header-line))
                (map-index (1- lines-from-header)))
           (when (and (>= map-index 0) (< map-index (length line-map)))
             (+ (aref line-map map-index) (1- offset))))))
     ;; Fallback: code block position + offset (for expanded view or no line-map)
     (when-let ((block-line (pi-coding-agent--code-block-line-at-point)))
       (+ block-line (1- offset)))
     ;; Last fallback to line 1
     1)))

(defun pi-coding-agent-visit-file (&optional toggle)
  "Visit the file associated with the tool block at point.
If on a diff line, go to the corresponding line number.
For read/write, go to the line within the displayed content.
By default, uses `pi-coding-agent-visit-file-other-window' to decide
whether to open in another window.  With prefix arg TOGGLE, invert
that behavior."
  (interactive "P")
  (if-let* ((ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block))
                          (overlays-at (point))))
            (path (overlay-get ov 'pi-coding-agent-tool-path)))
      (let* ((line (pi-coding-agent--tool-line-at-point ov))
             (use-other-window (if toggle
                                   (not pi-coding-agent-visit-file-other-window)
                                 pi-coding-agent-visit-file-other-window)))
        (funcall (if use-other-window #'find-file-other-window #'find-file) path)
        (goto-char (point-min))
        (forward-line (1- line)))
    (user-error "No file at point")))

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
   (concat "\n" (pi-coding-agent--make-separator "Compaction" timestamp) "\n"
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

(defconst pi-coding-agent-version "1.2.6"
  "Version of pi-coding-agent.")

(defun pi-coding-agent--get-pi-coding-agent-version ()
  "Get pi CLI version by running `pi --version'."
  (condition-case nil
      (string-trim (shell-command-to-string "pi --version"))
    (error "Unknown")))

(defun pi-coding-agent--format-startup-header ()
  "Format the startup header string with styled separator."
  (let* ((pi-coding-agent-cli-version (pi-coding-agent--get-pi-coding-agent-version))
         (separator (pi-coding-agent--make-separator (format "Pi %s" pi-coding-agent-cli-version))))
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

(defcustom pi-coding-agent-markdown-search-limit 30000
  "Maximum bytes to search backward for markdown code block context.
Markdown-mode's `markdown-find-previous-block' scans backward to find
enclosing code blocks for syntax highlighting.  In large buffers with
many code blocks, this O(n) scan causes severe performance issues.

This setting limits the backward search, improving performance by 7-25x
in typical chat buffers (100-200KB with 100+ code blocks).

Set to nil to disable the limit (not recommended for large buffers)."
  :type '(choice (integer :tag "Limit in bytes")
                 (const :tag "No limit (slow)" nil))
  :group 'pi-coding-agent)

(defun pi-coding-agent--limit-markdown-backward-search (orig-fun prop &optional lim)
  "Advice to limit `markdown-find-previous-prop' backward search.
ORIG-FUN is the original function, PROP is the property to find,
LIM is an optional limit which we strengthen based on
`pi-coding-agent-markdown-search-limit'.

Only applies in `pi-coding-agent-chat-mode' buffers to avoid affecting
other markdown buffers.  This optimization is safe because markdown
syntax highlighting only needs the nearest enclosing code block for
correct context, not blocks from earlier in the buffer."
  (if (and pi-coding-agent-markdown-search-limit
           (derived-mode-p 'pi-coding-agent-chat-mode))
      (let ((limit (max (point-min)
                        (- (point) pi-coding-agent-markdown-search-limit))))
        (funcall orig-fun prop (if lim (max lim limit) limit)))
    (funcall orig-fun prop lim)))

(defun pi-coding-agent--fontify-streaming-region ()
  "Fontify newly streamed message text incrementally.
Called by idle timer during streaming.  Only fontifies message text
that hasn't been fontified yet, tracked via the variable
`pi-coding-agent--last-fontified-pos'.  Skips the pending tool
overlay region — tool content is pre-fontified in a temp buffer
and marked with the `fontified' property to prevent jit-lock from
overriding it with gfm-mode faces."
  (when (and pi-coding-agent--message-start-marker
             pi-coding-agent--streaming-marker
             (marker-position pi-coding-agent--message-start-marker)
             (marker-position pi-coding-agent--streaming-marker))
    (let* ((start (or pi-coding-agent--last-fontified-pos
                      (marker-position pi-coding-agent--message-start-marker)))
           (end (marker-position pi-coding-agent--streaming-marker))
           ;; Skip the pending tool overlay to avoid gfm-mode overwriting
           ;; pre-fontified syntax faces (e.g., __init__ → markdown bold)
           (ov pi-coding-agent--pending-tool-overlay)
           (ov-start (and ov (overlay-start ov)))
           (ov-end (and ov (overlay-end ov))))
      (when (< start end)
        (if (and ov-start ov-end (< ov-start end) (< start ov-end))
            ;; Tool overlay intersects — fontify only the region before it
            (when (< start ov-start)
              (font-lock-ensure start ov-start))
          ;; No tool overlay in range — fontify everything
          (font-lock-ensure start end))
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

(defun pi-coding-agent--header-format-extension-status (ext-status)
  "Format EXT-STATUS alist for header-line display.
Returns string with | separator and all extension statuses, or empty string."
  (if (null ext-status)
      ""
    (concat " │ "
            (mapconcat (lambda (pair)
                         (propertize (cdr pair) 'face 'pi-coding-agent-retry-notice))
                       ext-status
                       " · "))))

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
         (ext-status (and chat-buf (buffer-local-value 'pi-coding-agent--extension-status chat-buf)))
         (session-name (and chat-buf (buffer-local-value 'pi-coding-agent--session-name chat-buf)))
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
     ;; Spinner/status (right after model/thinking)
     status-str
     ;; Stats (if available)
     (pi-coding-agent--header-format-stats stats last-usage model-obj)
     ;; Extension status (if any)
     (pi-coding-agent--header-format-extension-status ext-status)
     ;; Session name at end (truncated) - like a title
     (when session-name
       (concat " │ " (pi-coding-agent--truncate-string session-name 30))))))

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

(defun pi-coding-agent--apply-state-response (chat-buf response)
  "Apply get_state RESPONSE to CHAT-BUF.
Updates buffer-local state variables and refreshes mode-line.
Safely handles dead buffers by checking liveness first."
  (when (and (plist-get response :success)
             (buffer-live-p chat-buf))
    (with-current-buffer chat-buf
      (let ((new-state (pi-coding-agent--extract-state-from-response response)))
        (setq pi-coding-agent--status (plist-get new-state :status)
              pi-coding-agent--state new-state))
      (force-mode-line-update t))))

;;;; Slash Commands via RPC

(defvar-local pi-coding-agent--commands nil
  "List of available commands from pi.
Each entry is a plist with :name, :description, :source.
Source is \"prompt\", \"extension\", or \"skill\".")

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

(defun pi-coding-agent--set-commands (commands)
  "Set COMMANDS for the current session buffers.
COMMANDS is a list of plists with :name, :description, :source."
  (setq pi-coding-agent--commands commands)
  (let ((chat-buf (pi-coding-agent--get-chat-buffer))
        (input-buf (pi-coding-agent--get-input-buffer)))
    (dolist (buf (list chat-buf input-buf))
      (when (and (buffer-live-p buf)
                 (not (eq buf (current-buffer))))
        (with-current-buffer buf
          (setq pi-coding-agent--commands commands))))))

;;;; Transient Menu

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
        (font-lock-ensure start (point-max))
        (let ((inhibit-read-only t))
          (pi-coding-agent--apply-phscroll-to-tables start (point-max))))
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
                  (concat "\n" (pi-coding-agent--make-separator "You" timestamp) "\n"
                          text "\n"))))
             (setq prev-role "user"))
            ("assistant"
             ;; Show header only on transition from user
             (when (not (equal prev-role "assistant"))
               (flush-tools)
               (pi-coding-agent--append-to-chat
                (concat "\n" (pi-coding-agent--make-separator "Assistant") "\n")))
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

(defun pi-coding-agent--reset-session-state ()
  "Reset all session-specific state for a new session.
Call this when starting a new session to ensure no stale state persists."
  (setq pi-coding-agent--session-name nil
        pi-coding-agent--cached-stats nil
        pi-coding-agent--last-usage nil
        pi-coding-agent--assistant-header-shown nil
        pi-coding-agent--followup-queue nil
        pi-coding-agent--local-user-message nil
        pi-coding-agent--aborted nil
        pi-coding-agent--extension-status nil
        pi-coding-agent--message-start-marker nil
        pi-coding-agent--streaming-marker nil
        pi-coding-agent--in-code-block nil
        pi-coding-agent--in-thinking-block nil
        pi-coding-agent--line-parse-state 'line-start
        pi-coding-agent--pending-tool-overlay nil)
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
                         ;; Restore context usage from last assistant message
                         ;; (ensures context % displays correctly after resume/fork)
                         (when (buffer-live-p chat-buf)
                           (with-current-buffer chat-buf
                             (setq pi-coding-agent--last-usage
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

(defun pi-coding-agent-compact (&optional custom-instructions)
  "Compact conversation context to reduce token usage.
Optional CUSTOM-INSTRUCTIONS provide guidance for the compaction summary."
  (interactive)
  (when-let ((proc (pi-coding-agent--get-process))
             (chat-buf (pi-coding-agent--get-chat-buffer)))
    (message "Pi: Compacting...")
    (pi-coding-agent--spinner-start)
    (pi-coding-agent--rpc-async proc
                   (if custom-instructions
                       (list :type "compact" :customInstructions custom-instructions)
                     '(:type "compact"))
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
         (selected (cdr (assoc choice formatted)))
         ;; Capture buffers before async call (callback runs in arbitrary context)
         (chat-buf (pi-coding-agent--get-chat-buffer))
         (input-buf (pi-coding-agent--get-input-buffer)))
    (when selected
      (let ((entry-id (plist-get selected :entryId)))
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
                               ;; Pre-fill input with the selected message text
                               (when (buffer-live-p input-buf)
                                 (with-current-buffer input-buf
                                   (erase-buffer)
                                   ;; text may be nil if RPC returns null
                                   (when text (insert text)))))
                           (message "Pi: Branch failed"))))))))

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
        ;; Try git ls-files first (respects .gitignore)
        (let ((output (shell-command-to-string
                       "git ls-files --cached --others --exclude-standard 2>/dev/null")))
          (if (string-empty-p output)
              ;; Fallback to find if git fails or no files
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
           ;; Filter files containing prefix as substring
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
              ;; Exclude current/parent dir entries (with and without trailing slash)
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
;;
;; Message queuing has two paths:
;;
;; 1. Follow-up (C-c C-c while busy): Uses local queue in Emacs.
;;    - Message added to `pi-coding-agent--followup-queue'
;;    - On agent_end, first message is popped and sent as normal prompt
;;    - Displayed locally immediately (like normal sends)
;;    - Pi's echo via message_start is ignored (flag tracks this)
;;
;; 2. Steering (C-c C-s while busy): Uses pi's RPC `steer` command.
;;    - Pi intercepts between tool calls to deliver steering
;;    - NOT displayed locally - waits for pi's echo via message_start
;;    - This ensures steering appears at correct position (after current
;;      assistant output completes, avoiding display corruption)

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
Steering messages are delivered after the current tool execution,
interrupting any remaining tools.  Only works when agent is busy.
Unlike normal sends, steering is NOT displayed locally - pi will echo
it back via message_start at the correct position (after current
assistant output completes)."
  (interactive)
  (let ((text (string-trim (buffer-string))))
    (unless (string-empty-p text)
      (let ((chat-buf (pi-coding-agent--get-chat-buffer)))
        (when chat-buf
          (let ((status (buffer-local-value 'pi-coding-agent--status chat-buf)))
            (if (eq status 'idle)
                ;; Idle - refuse (nothing to interrupt)
                (message "Pi: Nothing to interrupt - use C-c C-c to send")
              ;; Busy - send via RPC (don't display locally)
              ;; Note: pi handles command expansion
              ;; Add to history
              (pi-coding-agent--history-add text)
              (setq pi-coding-agent--input-ring-index nil
                    pi-coding-agent--input-saved nil)
              (erase-buffer)
              ;; Don't display locally - pi will echo back via message_start
              ;; at the correct position (after current assistant turn)
              ;; Send via RPC (steering requires pi to intercept between tools)
              (pi-coding-agent--send-steer-message text)
              (message "Pi: Steering message sent"))))))))

(defun pi-coding-agent-queue-followup ()
  "Queue current input as a follow-up message.
Obsolete: Use `pi-coding-agent-send' (C-c C-c) instead, which now
automatically queues as follow-up when the agent is busy."
  (interactive)
  (pi-coding-agent-send))

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
    ("S" "stats" pi-coding-agent-session-stats)
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
            (let* ((name (plist-get cmd :name))
                   (desc (or (plist-get cmd :description) "")))
              ;; Run command with number key
              (push (list (format "%d" (cl-incf key))
                          (format "%-20s  %s"
                                  (truncate-string-to-width name 20)
                                  (truncate-string-to-width desc available-width))
                          `(lambda ()
                             (interactive)
                             (pi-coding-agent--run-custom-command ',cmd)))
                    children))))))
    (nreverse children)))

(defun pi-coding-agent--make-submenu-edit-children (source)
  "Build edit suffixes for commands with SOURCE.
Returns a list suitable for `transient-parse-suffixes'.
Edit keys use Shift+number (!@#$%^&*() for 1-9)."
  (let* ((all-cmds (seq-filter (lambda (c)
                                 (and (equal (plist-get c :source) source)
                                      (plist-get c :path)))
                               pi-coding-agent--commands))
         ;; Sort by location: path, project, user
         (sorted-cmds (seq-sort-by
                       (lambda (c)
                         (pcase (plist-get c :location)
                           ("path" 0)
                           ("project" 1)
                           ("user" 2)
                           (_ 3)))
                       #'<
                       all-cmds))
         (key 0)
         (children '())
         ;; Shift+number symbols for edit bindings
         (shift-keys ["!" "@" "#" "$" "%" "^" "&" "*" "("]))
    (dolist (cmd sorted-cmds)
      (when (< key 9)
        (let ((name (plist-get cmd :name))
              (path (plist-get cmd :path)))
          (push (list (aref shift-keys key)
                      (truncate-string-to-width name 12)
                      `(lambda ()
                         (interactive)
                         (find-file-other-window ,path)))
                children)
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
Press number to run, Shift+number to edit source file."
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
Press number to run, Shift+number to edit source file."
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
Press number to run, Shift+number to edit source file."
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
          (let ((buf chat-buf)
                (proc pi-coding-agent--process))  ; Capture for closures
            (pi-coding-agent--rpc-async proc '(:type "get_state")
              (lambda (response)
                (pi-coding-agent--apply-state-response buf response)
                ;; Check if no model available and warn user
                (when (and (plist-get response :success)
                           (buffer-live-p buf))
                  (with-current-buffer buf
                    (unless (plist-get pi-coding-agent--state :model)
                      (pi-coding-agent--display-no-model-warning))))))
            ;; Fetch commands via RPC (independent of get_state)
            (pi-coding-agent--fetch-commands proc
              (lambda (commands)
                (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (pi-coding-agent--set-commands commands)
                    (pi-coding-agent--rebuild-commands-menu))))))))
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

;;;; Performance Optimizations

;; Limit markdown backward search to prevent O(n) scanning in large buffers.
;; See `pi-coding-agent-markdown-search-limit' for details.
(advice-add 'markdown-find-previous-prop :around
            #'pi-coding-agent--limit-markdown-backward-search)

(defun pi-coding-agent-unload-function ()
  "Clean up when `pi-coding-agent' is unloaded."
  (advice-remove 'markdown-find-previous-prop
                 #'pi-coding-agent--limit-markdown-backward-search)
  nil)  ;; Return nil to continue standard unloading

(provide 'pi-coding-agent)
;;; pi-coding-agent.el ends here
