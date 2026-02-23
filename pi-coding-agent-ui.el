;;; pi-coding-agent-ui.el --- Shared state, faces, and UI primitives -*- lexical-binding: t; -*-

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

;; Foundation module for pi-coding-agent: shared state, faces, customization,
;; buffer management, display primitives, header-line, and major modes.
;;
;; This is the base layer that all other pi-coding-agent modules require.
;; It provides:
;; - Customization options and face definitions
;; - Buffer-local session variables (the shared mutable state)
;; - Buffer creation, naming, and navigation
;; - Display primitives (append-to-chat, scroll preservation, separators)
;; - Header-line formatting and activity phases
;; - Sending infrastructure (send-prompt, abort-send)
;; - Major mode definitions (chat-mode, input-mode)

;;; Code:

(require 'pi-coding-agent-core)
(require 'cl-lib)
(require 'project)
(require 'markdown-mode)
(require 'color)


;; Forward declarations: keymaps bind functions defined in other modules.
;; Grouped by target module for easy cross-referencing.

;; pi-coding-agent-render.el (chat buffer commands)
(declare-function pi-coding-agent-toggle-tool-section "pi-coding-agent-render")
(declare-function pi-coding-agent-visit-file "pi-coding-agent-render")
(declare-function pi-coding-agent--cleanup-on-kill "pi-coding-agent-render")
(declare-function pi-coding-agent--restore-tool-properties "pi-coding-agent-render")

;; pi-coding-agent-input.el (input buffer commands)
(declare-function pi-coding-agent-quit "pi-coding-agent-input")
(declare-function pi-coding-agent-send "pi-coding-agent-input")
(declare-function pi-coding-agent-abort "pi-coding-agent-input")
(declare-function pi-coding-agent-previous-input "pi-coding-agent-input")
(declare-function pi-coding-agent-next-input "pi-coding-agent-input")
(declare-function pi-coding-agent-history-isearch-backward "pi-coding-agent-input")
(declare-function pi-coding-agent-queue-steering "pi-coding-agent-input")
(declare-function pi-coding-agent-input-mode "pi-coding-agent-input")

;; pi-coding-agent-menu.el (menu and session commands)
(declare-function pi-coding-agent-menu "pi-coding-agent-menu")
(declare-function pi-coding-agent-resume-session "pi-coding-agent-menu")
(declare-function pi-coding-agent-select-model "pi-coding-agent-menu")
(declare-function pi-coding-agent-cycle-thinking "pi-coding-agent-menu")
(declare-function pi-coding-agent-fork-at-point "pi-coding-agent-menu")

;; Optional: phscroll for horizontal table scrolling
(require 'phscroll nil t)
(declare-function phscroll-mode "phscroll" (&optional arg))

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

(defcustom pi-coding-agent-executable '("pi")
  "Command to invoke the pi binary, as a list of strings.
The first element is the program; remaining elements are passed
before \"--mode rpc\" and `pi-coding-agent-extra-args'.

For npx users:
  (setq pi-coding-agent-executable \\='(\"npx\" \"pi\"))"
  :type '(repeat string)
  :group 'pi-coding-agent)

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
  "Maximum visual lines to show before collapsing tool output."
  :type 'natnum
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-bash-preview-lines 5
  "Maximum visual lines to show for bash output before collapsing.
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

(defcustom pi-coding-agent-input-markdown-highlighting nil
  "Whether to enable GFM syntax highlighting in the input buffer.
When non-nil, the input buffer gets GitHub Flavored Markdown
highlighting (bold, italic, code spans, fenced blocks).  When nil,
the input buffer uses plain `text-mode'.

Takes effect for new sessions; existing input buffers keep their mode."
  :type 'boolean
  :group 'pi-coding-agent)

(defcustom pi-coding-agent-copy-raw-markdown nil
  "Whether to copy raw markdown from the chat buffer.
When non-nil, copy commands (`kill-ring-save', `kill-region') preserve
raw markdown — bold markers (**), backticks, code fences, and setext
underlines are kept.  Useful for pasting into docs, Slack, or other
markdown-aware contexts.

When nil (the default), only the visible text is copied."
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

(defface pi-coding-agent-activity-phase
  '((t :inherit shadow))
  "Face for activity phase label in header line."
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
    (define-key map (kbd "f") #'pi-coding-agent-fork-at-point)
    (define-key map (kbd "TAB") #'pi-coding-agent-toggle-tool-section)
    (define-key map (kbd "<tab>") #'pi-coding-agent-toggle-tool-section)
    (define-key map (kbd "RET") #'pi-coding-agent-visit-file)
    (define-key map (kbd "<return>") #'pi-coding-agent-visit-file)
    map)
  "Keymap for `pi-coding-agent-chat-mode'.")

;;;; You Heading Detection

(defconst pi-coding-agent--you-heading-re
  "^You\\( · .*\\)?$"
  "Regex matching the first line of a user turn setext heading.
Matches `You' at line start, optionally followed by ` · <timestamp>'.
Must be verified with `pi-coding-agent--at-you-heading-p' to confirm
the next line is a setext underline (===), avoiding false matches on
user message text starting with \"You\".")

(defun pi-coding-agent--at-you-heading-p ()
  "Return non-nil if current line is a You setext heading.
Checks that the current line matches `pi-coding-agent--you-heading-re'
and the next line is a setext underline (three or more `=' characters)."
  (and (save-excursion
         (beginning-of-line)
         (looking-at pi-coding-agent--you-heading-re))
       (save-excursion
         (forward-line 1)
         (looking-at "^=\\{3,\\}$"))))

;;;; Turn Detection

(defun pi-coding-agent--collect-you-headings ()
  "Return list of buffer positions of all You setext headings.
Scans from `point-min', returns positions in chronological order."
  (let (headings)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pi-coding-agent--you-heading-re nil t)
        (let ((pos (match-beginning 0)))
          (save-excursion
            (goto-char pos)
            (when (pi-coding-agent--at-you-heading-p)
              (push pos headings))))))
    (nreverse headings)))

(defun pi-coding-agent--user-turn-index-at-point (&optional headings)
  "Return 0-based index of the user turn at or before point.
HEADINGS is an optional pre-computed list from
`pi-coding-agent--collect-you-headings'; when nil, the buffer is scanned.
Returns nil if point is before the first You heading."
  (let ((headings (or headings (pi-coding-agent--collect-you-headings)))
        (limit (point))
        (index 0)
        (result nil))
    (dolist (h headings)
      (when (<= h limit)
        (setq result index))
      (setq index (1+ index)))
    result))

;;;; Chat Navigation

(defun pi-coding-agent--find-you-heading (search-fn)
  "Find the next You setext heading using SEARCH-FN.
SEARCH-FN is `re-search-forward' or `re-search-backward'.
Returns the position of the heading line start, or nil if not found."
  (save-excursion
    (let ((found nil))
      (while (and (not found)
                  (funcall search-fn pi-coding-agent--you-heading-re nil t))
        (let ((candidate (match-beginning 0)))
          (save-excursion
            (goto-char candidate)
            (when (pi-coding-agent--at-you-heading-p)
              (setq found candidate)))))
      found)))

(defun pi-coding-agent-next-message ()
  "Move to the next user message in the chat buffer."
  (interactive)
  (let ((pos (save-excursion
               (forward-line 1)
               (pi-coding-agent--find-you-heading #'re-search-forward))))
    (if pos
        (progn
          (goto-char pos)
          (when (get-buffer-window) (recenter 0)))
      (message "No more messages"))))

(defun pi-coding-agent-previous-message ()
  "Move to the previous user message in the chat buffer."
  (interactive)
  (let ((pos (save-excursion
               (beginning-of-line)
               (pi-coding-agent--find-you-heading #'re-search-backward))))
    (if pos
        (progn
          (goto-char pos)
          (when (get-buffer-window) (recenter 0)))
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

;;;; Copy Visible Text

(defun pi-coding-agent--visible-text (beg end)
  "Return visible text between BEG and END, stripping hidden markup.
Skips characters with `invisible' property matching `buffer-invisibility-spec'
and characters with `display' property equal to the empty string."
  (let ((result nil)
        (pos beg))
    (while (< pos end)
      (let* ((inv (get-text-property pos 'invisible))
             (disp (get-text-property pos 'display))
             (next (min (next-single-char-property-change pos 'invisible nil end)
                        (next-single-char-property-change pos 'display nil end))))
        (cond
         ((and inv (invisible-p inv)) nil)
         ((equal disp "") nil)
         (t (push (buffer-substring-no-properties pos next) result)))
        (setq pos next)))
    (apply #'concat (nreverse result))))

(defun pi-coding-agent--filter-buffer-substring (beg end &optional delete)
  "Filter function for `filter-buffer-substring-function' in chat buffers.
When `pi-coding-agent-copy-raw-markdown' is nil, returns only visible
text between BEG and END.  If DELETE is non-nil, also removes the region.
Otherwise delegates to the default filter."
  (if pi-coding-agent-copy-raw-markdown
      (buffer-substring--filter beg end delete)
    (prog1 (pi-coding-agent--visible-text beg end)
      (when delete (delete-region beg end)))))

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
  ;; Strip hidden markup from copy operations (M-w, C-w)
  (setq-local filter-buffer-substring-function
              #'pi-coding-agent--filter-buffer-substring)
  (setq-local pi-coding-agent--tool-args-cache (make-hash-table :test 'equal))
  (setq-local pi-coding-agent--fontify-buffers (make-hash-table :test 'equal))
  ;; Disable hl-line-mode: its post-command-hook overlay update causes
  ;; scroll oscillation in buffers with invisible text + variable heights.
  (setq-local global-hl-line-mode nil)
  (hl-line-mode -1)
  ;; Make window-point follow inserted text (like comint does).
  ;; This is key for natural scroll behavior during streaming.
  (setq-local window-point-insertion-type t)

  ;; Add wrap-prefix to blockquotes so wrapped lines show the indicator
  (font-lock-add-keywords nil '((pi-coding-agent--fontify-blockquote-wrap-prefix)) 'append)

  ;; Run after font-lock to undo markdown damage in tool overlays.
  (jit-lock-register #'pi-coding-agent--restore-tool-properties)

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
          ;; Keep canonical session directory for exact matching.
          (setq default-directory dir)
          (pcase type
            (:chat (pi-coding-agent-chat-mode))
            (:input (pi-coding-agent-input-mode))))
        buf))))

;;;; Project Buffer Discovery

(defun pi-coding-agent--normalize-directory (dir)
  "Normalize DIR for exact path comparisons.
Returns an expanded absolute path with a trailing slash."
  (file-name-as-directory (expand-file-name dir)))

(defun pi-coding-agent-project-buffers ()
  "Return pi chat buffers for the current project directory.
Matches buffers by exact `default-directory', not by `buffer-name' prefix.
Returns a list ordered by `buffer-list' recency (most recent first)."
  (let ((target-dir (pi-coding-agent--normalize-directory
                     (pi-coding-agent--session-directory))))
    (cl-remove-if-not
     (lambda (buf)
       (and (buffer-live-p buf)
            (with-current-buffer buf
              (and (derived-mode-p 'pi-coding-agent-chat-mode)
                   (stringp default-directory)
                   (string=
                    (pi-coding-agent--normalize-directory default-directory)
                    target-dir)))))
     (buffer-list))))

;;;; Window Hiding

(defun pi-coding-agent--hide-session-windows ()
  "Hide the current pi session in the selected frame.
Preserves this frame's window layout by deleting input windows (the
child splits created by `pi-coding-agent--display-buffers') and
replacing chat windows with their previous buffers via `bury-buffer'.

Must be called from a pi chat or input buffer.  Only affects windows
of the current session in the selected frame."
  (let ((chat-buf (pi-coding-agent--get-chat-buffer))
        (input-buf (pi-coding-agent--get-input-buffer)))
    (when (buffer-live-p input-buf)
      (dolist (win (get-buffer-window-list input-buf nil))
        (ignore-errors (delete-window win))))
    (when (buffer-live-p chat-buf)
      (dolist (win (get-buffer-window-list chat-buf nil))
        (with-selected-window win
          (bury-buffer))))))

;;;; Buffer-Local Session Variables

(defvar-local pi-coding-agent--process nil
  "The pi RPC subprocess for this session.")

(defvar-local pi-coding-agent--process-version nil
  "Detected pi CLI version for the current process.")

(defun pi-coding-agent--set-process (process)
  "Set the pi RPC subprocess PROCESS for this session.
Resets cached process version and starts a delayed version probe for
new live processes in interactive sessions."
  (setq pi-coding-agent--process process
        pi-coding-agent--process-version nil)
  (when (and (processp process)
             (process-live-p process)
             (not noninteractive))
    (pi-coding-agent--probe-process-version-async (current-buffer))))

(defvar-local pi-coding-agent--chat-buffer nil
  "Reference to the chat buffer for this session.")

(defun pi-coding-agent--set-chat-buffer (buffer)
  "Set the chat BUFFER reference for this session."
  (setq pi-coding-agent--chat-buffer buffer))

(defvar-local pi-coding-agent--input-buffer nil
  "Reference to the input buffer for this session.")

(defun pi-coding-agent--set-input-buffer (buffer)
  "Set the input BUFFER reference for this session."
  (setq pi-coding-agent--input-buffer buffer))

(defvar-local pi-coding-agent--streaming-marker nil
  "Marker for current streaming insertion point.")

(defun pi-coding-agent--set-streaming-marker (marker)
  "Set the streaming insertion point MARKER."
  (setq pi-coding-agent--streaming-marker marker))

(defvar-local pi-coding-agent--in-code-block nil
  "Non-nil when streaming inside a fenced code block.
Used to suppress ATX heading transforms inside code.")

(defvar-local pi-coding-agent--in-thinking-block nil
  "Non-nil while processing a thinking block for the current message.
Used for lifecycle resets when new messages or turns begin.")

(defvar-local pi-coding-agent--thinking-marker nil
  "Marker for insertion point inside the current thinking block.
Unlike `pi-coding-agent--streaming-marker', this marker stays anchored
in thinking text when other content blocks (for example, tool headers)
interleave during streaming.")

(defvar-local pi-coding-agent--thinking-start-marker nil
  "Marker for the start of the current thinking block.
Used to rewrite thinking content in place after whitespace normalization.")

(defvar-local pi-coding-agent--thinking-raw nil
  "Accumulated raw thinking deltas for the current thinking block.
Normalized and re-rendered incrementally to avoid excess whitespace.")

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

(defvar-local pi-coding-agent--activity-phase "idle"
  "Fine-grained activity phase for header-line display.
One of \"thinking\", \"replying\", \"running\",
\"compact\", or \"idle\".
Always populated and rendered in a fixed-width slot.")

(defun pi-coding-agent--set-activity-phase (phase)
  "Set activity PHASE for header-line display in current chat buffer.
PHASE should be one of \"thinking\", \"replying\",
\"running\", \"compact\", \"idle\".
Returns non-nil when the phase changed."
  (unless (equal pi-coding-agent--activity-phase phase)
    (setq pi-coding-agent--activity-phase phase)
    (force-mode-line-update t)
    t))

(defvar-local pi-coding-agent--cached-stats nil
  "Cached session statistics for header-line display.
Updated after each agent turn completes.")

(defvar-local pi-coding-agent--last-usage nil
  "Usage from last assistant message for context percentage.
This is the per-turn usage, not cumulative - used to calculate
how much of the context window was used in the last turn.")

(defun pi-coding-agent--set-last-usage (usage)
  "Set the last assistant message USAGE for context percentage."
  (setq pi-coding-agent--last-usage usage))

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

(defun pi-coding-agent--set-aborted (value)
  "Set the aborted flag to VALUE."
  (setq pi-coding-agent--aborted value))

(defvar-local pi-coding-agent--message-start-marker nil
  "Marker for start of current message content.
Used to replace raw markdown with rendered Org on message completion.")

(defun pi-coding-agent--set-message-start-marker (marker)
  "Set the message start MARKER."
  (setq pi-coding-agent--message-start-marker marker))

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

(defvar-local pi-coding-agent--fontify-buffers nil
  "Hash table mapping language strings to fontification cache buffers.
Each chat buffer tracks its own fontify buffers so parallel sessions
writing the same language don't corrupt each other's syntax state.
Initialized in `pi-coding-agent-chat-mode'; cleaned up by
`pi-coding-agent--kill-fontify-buffers' when the session ends.")

(defvar-local pi-coding-agent--assistant-header-shown nil
  "Non-nil if Assistant header has been shown for current prompt.
Used to avoid duplicate headers during retry sequences.")

(defvar-local pi-coding-agent--followup-queue nil
  "List of follow-up messages queued while agent is busy.
Messages are added when user sends while streaming.
On agent_end, the first message is popped and sent as a normal prompt.
This is simpler than using pi's RPC follow_up command.")

(defun pi-coding-agent--push-followup (message)
  "Push MESSAGE onto the follow-up queue."
  (push message pi-coding-agent--followup-queue))

(defun pi-coding-agent--dequeue-followup ()
  "Dequeue and return the oldest follow-up message, or nil if empty.
Follow-ups are processed in FIFO order: first pushed, first sent."
  (when pi-coding-agent--followup-queue
    (let ((text (car (last pi-coding-agent--followup-queue))))
      (setq pi-coding-agent--followup-queue
            (butlast pi-coding-agent--followup-queue))
      text)))

(defun pi-coding-agent--clear-followup-queue ()
  "Clear all pending follow-up messages."
  (setq pi-coding-agent--followup-queue nil))

(defvar-local pi-coding-agent--local-user-message nil
  "Text of user message we displayed locally, awaiting pi's echo.
Set when displaying a user message (normal send, follow-up).
Cleared when we receive message_start role=user from pi.
When nil and we receive message_start role=user, we display it.
When set but different from pi's message, we display pi's version
\(e.g., expanded template).")

(defvar-local pi-coding-agent--extension-status nil
  "Alist of extension status messages for header-line display.
Keys are extension identifiers (strings), values are status text.")

(defvar-local pi-coding-agent--working-message nil
  "Transient extension working message for header-line display.")

(defvar-local pi-coding-agent--session-name nil
  "Cached session name for header-line display.
Extracted from session_info entries when session is loaded or switched.")

(defvar-local pi-coding-agent--commands nil
  "List of available commands from pi.
Each entry is a plist with :name, :description, :source.
Source is \"prompt\", \"extension\", or \"skill\".")

(defun pi-coding-agent--set-commands (commands)
  "Set COMMANDS in current buffer and propagate to sibling session buffers.
COMMANDS is a list of plists with :name, :description, :source.
Both chat and input buffers share the same commands list, so this
setter updates all of them to keep them in sync."
  (setq pi-coding-agent--commands commands)
  (let ((chat-buf (pi-coding-agent--get-chat-buffer))
        (input-buf (pi-coding-agent--get-input-buffer)))
    (dolist (buf (list chat-buf input-buf))
      (when (and (buffer-live-p buf)
                 (not (eq buf (current-buffer))))
        (with-current-buffer buf
          (setq pi-coding-agent--commands commands))))))

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

(defun pi-coding-agent--window-can-split-for-input-p (window)
  "Return non-nil if WINDOW can be split into chat and input windows."
  (>= (window-total-height window)
      (* 2 window-min-height)))

(defun pi-coding-agent--input-height-for-window (window)
  "Return input pane height to use when splitting WINDOW.
Clamps `pi-coding-agent-input-window-height' to the maximum that still
leaves at least `window-min-height' lines for chat."
  (let* ((window-height (window-total-height window))
         (max-input-height (- window-height window-min-height)))
    (max window-min-height
         (min pi-coding-agent-input-window-height
              max-input-height))))

(defun pi-coding-agent--windows-by-height (&optional windows)
  "Return live WINDOWS sorted by descending height.
If WINDOWS is nil, use all non-minibuffer windows in the selected frame."
  (sort (cl-remove-if-not #'window-live-p
                          (copy-sequence (or windows (window-list nil 'no-mini))))
        (lambda (a b)
          (> (window-total-height a)
             (window-total-height b)))))

(defun pi-coding-agent--window-with-most-height (&optional windows)
  "Return the tallest window from WINDOWS.
If WINDOWS is nil, use all non-minibuffer windows in the selected frame."
  (car (pi-coding-agent--windows-by-height windows)))

(defun pi-coding-agent--best-display-window (&optional preferred)
  "Return best window for displaying chat+input.
Use PREFERRED when it can be split, else pick the tallest splittable
window in the frame.  Falls back to PREFERRED or selected window."
  (or (and preferred
           (window-live-p preferred)
           (pi-coding-agent--window-can-split-for-input-p preferred)
           preferred)
      (cl-find-if #'pi-coding-agent--window-can-split-for-input-p
                  (pi-coding-agent--windows-by-height))
      preferred
      (selected-window)))

(defun pi-coding-agent--preferred-display-window (chat-wins input-wins selected)
  "Return preferred base window for displaying chat+input.
CHAT-WINS and INPUT-WINS are existing session windows.  SELECTED is the
currently selected window."
  (cond
   ;; Input-only visible: prefer selected non-input window so we can
   ;; replace it cleanly and avoid duplicate input windows.
   ((and input-wins (not chat-wins)
         (not (memq selected input-wins))
         (pi-coding-agent--window-can-split-for-input-p selected))
    selected)
   (chat-wins (pi-coding-agent--window-with-most-height chat-wins))
   (input-wins (pi-coding-agent--window-with-most-height input-wins))
   (t selected)))

(defun pi-coding-agent--delete-extra-input-windows (input-wins target)
  "Delete windows in INPUT-WINS except TARGET."
  (dolist (win input-wins)
    (unless (eq win target)
      (ignore-errors (delete-window win)))))

(defun pi-coding-agent--paired-input-window (chat-win input-buf)
  "Return input window below CHAT-WIN showing INPUT-BUF, or nil."
  (when (window-live-p chat-win)
    (let ((below (window-in-direction 'below chat-win)))
      (and below
           (eq (window-buffer below) input-buf)
           below))))

(defun pi-coding-agent--best-input-window (chat-buf input-buf)
  "Return best visible window for INPUT-BUF in current frame.
Prefer the input window below the selected CHAT-BUF window, then the
selected input window, then the tallest input window."
  (let* ((input-wins (get-buffer-window-list input-buf nil))
         (selected (selected-window))
         (selected-chat-win (and (eq (window-buffer selected) chat-buf)
                                 selected)))
    (or (pi-coding-agent--paired-input-window selected-chat-win input-buf)
        (and (memq selected input-wins)
             selected)
        (pi-coding-agent--window-with-most-height input-wins))))

(defun pi-coding-agent--focus-input-window (chat-buf input-buf)
  "Select a visible INPUT-BUF window for the CHAT-BUF session."
  (when-let ((win (pi-coding-agent--best-input-window chat-buf input-buf)))
    (select-window win)))

(defun pi-coding-agent--display-buffers (chat-buf input-buf)
  "Ensure CHAT-BUF and INPUT-BUF are visible.
Uses a split window with chat above and input below.  Falls back to a
larger window when the selected one cannot be split."
  (let* ((chat-wins (get-buffer-window-list chat-buf nil))
         (input-wins (get-buffer-window-list input-buf nil))
         (selected (selected-window))
         (preferred (pi-coding-agent--preferred-display-window
                     chat-wins input-wins selected))
         (target (pi-coding-agent--best-display-window preferred))
         (input-win nil))
    ;; Remove stale input windows when restoring from an input-only view.
    (when (and input-wins (not chat-wins))
      (pi-coding-agent--delete-extra-input-windows input-wins target))
    (with-selected-window target
      (unless (pi-coding-agent--window-can-split-for-input-p target)
        (delete-other-windows target))
      (unless (pi-coding-agent--window-can-split-for-input-p target)
        (user-error "Window too small for chat + input layout"))
      (switch-to-buffer chat-buf)
      (with-current-buffer chat-buf
        (goto-char (point-max)))
      (let ((input-height (pi-coding-agent--input-height-for-window target)))
        (setq input-win (split-window nil (- input-height) 'below))
        (set-window-buffer input-win input-buf)))
    (when (window-live-p input-win)
      (select-window input-win))))

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

;;;; Formatting Utilities

(defun pi-coding-agent--format-number (n)
  "Format number N with thousands separators."
  (let ((str (number-to-string n)))
    (replace-regexp-in-string
     "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\([^0-9]\\|$\\)"
     "\\1,\\2\\3"
     (replace-regexp-in-string
      "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\([0-9]\\{3\\}\\)\\([^0-9]\\|$\\)"
      "\\1,\\2,\\3\\4" str))))

(defun pi-coding-agent--truncate-string (str max-len)
  "Truncate STR to MAX-LEN chars, adding ellipsis if needed."
  (if (and str (> (length str) max-len))
      (concat (substring str 0 (- max-len 1)) "…")
    str))

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

;;;; Phscroll Availability

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

;;;; Dependency Checking

(defun pi-coding-agent--check-pi ()
  "Check if pi binary is available.
Returns t if available, nil otherwise."
  (and (executable-find (car pi-coding-agent-executable)) t))

(defun pi-coding-agent--check-dependencies ()
  "Check all required dependencies.
Displays warnings for missing dependencies."
  (unless (pi-coding-agent--check-pi)
    (display-warning 'pi (format "%s not found in PATH. Install with: npm install -g @mariozechner/pi-coding-agent"
                                 (car pi-coding-agent-executable))
                     :error)))

;;;; Startup Header

(defconst pi-coding-agent-version "1.3.4"
  "Version of pi-coding-agent.")

(defconst pi-coding-agent--version-probe-delay 0.1
  "Seconds to wait before probing `pi --version' for a new process.")

(defun pi-coding-agent--finish-pi-version-process (proc)
  "Collect `pi --version' output from PROC and invoke its callback."
  (let ((callback (process-get proc 'pi-coding-agent-version-callback))
        (stdout-buf (process-get proc 'pi-coding-agent-version-stdout-buf))
        (stderr-buf (process-get proc 'pi-coding-agent-version-stderr-buf)))
    (unwind-protect
        (let ((stdout (when (buffer-live-p stdout-buf)
                        (string-trim
                         (with-current-buffer stdout-buf
                           (buffer-string))))))
          (when callback
            (funcall callback
                     (and stdout
                          (not (string-empty-p stdout))
                          stdout))))
      (when (buffer-live-p stdout-buf)
        (kill-buffer stdout-buf))
      (when (buffer-live-p stderr-buf)
        (kill-buffer stderr-buf)))))

(defun pi-coding-agent--run-pi-version-once-async (callback)
  "Run `pi --version' asynchronously and call CALLBACK with version or nil."
  (let ((stdout-buf (generate-new-buffer " *pi-coding-agent-version-stdout*"))
        (stderr-buf (generate-new-buffer " *pi-coding-agent-version-stderr*")))
    (condition-case nil
        (let ((proc (make-process
                     :name "pi-version"
                     :command `(,@pi-coding-agent-executable "--version")
                     :connection-type 'pipe
                     :buffer stdout-buf
                     :stderr stderr-buf
                     :noquery t
                     :sentinel
                     (lambda (proc _event)
                       (when (memq (process-status proc) '(exit signal))
                         (pi-coding-agent--finish-pi-version-process proc))))))
          (process-put proc 'pi-coding-agent-version-callback callback)
          (process-put proc 'pi-coding-agent-version-stdout-buf stdout-buf)
          (process-put proc 'pi-coding-agent-version-stderr-buf stderr-buf)
          proc)
      (error
       (when (buffer-live-p stdout-buf)
         (kill-buffer stdout-buf))
       (when (buffer-live-p stderr-buf)
         (kill-buffer stderr-buf))
       (funcall callback nil)))))

(defun pi-coding-agent--request-pi-version-async (callback)
  "Resolve pi CLI version asynchronously and call CALLBACK with string or nil."
  (run-at-time pi-coding-agent--version-probe-delay nil
               #'pi-coding-agent--run-pi-version-once-async
               callback))

(defun pi-coding-agent--probe-process-version-async (chat-buf)
  "Probe and cache CLI version for CHAT-BUF's process.
Stores the result in CHAT-BUF and emits a minibuffer notice when available."
  (pi-coding-agent--request-pi-version-async
   (lambda (version)
     (when (and version (buffer-live-p chat-buf))
       (with-current-buffer chat-buf
         (setq pi-coding-agent--process-version version)
         (message "Pi: version %s" version))))))

(defun pi-coding-agent--format-startup-header ()
  "Format the startup header string with styled separator."
  (let ((separator (pi-coding-agent--make-separator "Pi Coding Agent for Emacs")))
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

;;; Header-Line Formatting

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
When CONTEXT-TOKENS is nil, usage is unknown and rendered as \"?\".
Returns nil if CONTEXT-WINDOW is 0."
  (when (> context-window 0)
    (if (null context-tokens)
        (format " ?/%s" (pi-coding-agent--format-tokens-compact context-window))
      (let* ((pct (* (/ (float context-tokens) context-window) 100))
             ;; Note: %% needed because % has special meaning in header-line-format
             (pct-str (format " %.1f%%%%/%s" pct
                              (pi-coding-agent--format-tokens-compact context-window))))
        (propertize pct-str
                    'face (cond
                           ((> pct pi-coding-agent-context-error-threshold) 'error)
                           ((> pct pi-coding-agent-context-warning-threshold) 'warning)
                           (t nil)))))))

(defun pi-coding-agent--header-format-stats (stats last-usage model-obj)
  "Format compact header stats from STATS.
Shows only cumulative session cost and last-turn context usage.
LAST-USAGE is the most recent message's token usage.
MODEL-OBJ contains model info including context window.
Returns nil if STATS is nil."
  (when stats
    (let* ((cost (or (plist-get stats :cost) 0))
           ;; Context percentage from LAST message usage, not cumulative totals.
           ;; After compaction, usage is unknown until the next assistant message.
           (context-tokens (when last-usage
                             (+ (or (plist-get last-usage :input) 0)
                                (or (plist-get last-usage :output) 0)
                                (or (plist-get last-usage :cacheRead) 0)
                                (or (plist-get last-usage :cacheWrite) 0))))
           (context-window (or (plist-get model-obj :contextWindow) 0)))
      (concat
       " │"
       (format " $%.2f" cost)
       (pi-coding-agent--header-format-context context-tokens context-window)))))

(defun pi-coding-agent--header-format-extension-status (ext-status)
  "Format EXT-STATUS alist for header-line display.
Returns extension statuses joined with \" · \", or empty string."
  (if (null ext-status)
      ""
    (mapconcat (lambda (pair)
                 (propertize (cdr pair) 'face 'pi-coding-agent-retry-notice))
               ext-status
               " · ")))

(defun pi-coding-agent--header-format-identity (model-short thinking activity-phase-str)
  "Format identity group from MODEL-SHORT, THINKING, and ACTIVITY-PHASE-STR."
  (concat
   (propertize model-short
               'face 'pi-coding-agent-model-name
               'mouse-face 'highlight
               'help-echo "mouse-1: Select model"
               'local-map pi-coding-agent--header-model-map)
   (if (string-empty-p thinking)
       ""
     (concat " • "
             (propertize thinking
                         'mouse-face 'highlight
                         'help-echo "mouse-1: Cycle thinking level"
                         'local-map pi-coding-agent--header-thinking-map)))
   " " activity-phase-str))

(defun pi-coding-agent--header-format-context-group (session-name)
  "Format context group from SESSION-NAME.
Returns a leading-pipe group string or empty string
when no session name exists."
  (if (and session-name (not (string-empty-p session-name)))
      (concat " │ " (pi-coding-agent--truncate-string session-name 30))
    ""))

(defun pi-coding-agent--header-format-extension-group (ext-status working-message)
  "Format extension group from EXT-STATUS and WORKING-MESSAGE.
Returns a leading-pipe group string or empty string
when no extension info exists."
  (let* ((status-str (pi-coding-agent--header-format-extension-status ext-status))
         (working-str (if (and working-message (not (string-empty-p working-message)))
                          (propertize working-message 'face 'shadow)
                        ""))
         (parts nil))
    (unless (string-empty-p status-str)
      (push status-str parts))
    (unless (string-empty-p working-str)
      (push working-str parts))
    (if parts
        (concat " │ " (mapconcat #'identity (nreverse parts) " · "))
      "")))

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
         (working-message (and chat-buf (buffer-local-value 'pi-coding-agent--working-message chat-buf)))
         (session-name (and chat-buf (buffer-local-value 'pi-coding-agent--session-name chat-buf)))
         (model-obj (plist-get state :model))
         (model-name (cond
                      ((stringp model-obj) model-obj)
                      ((plist-get model-obj :name))
                      (t "")))
         (model-short (if (string-empty-p model-name) "..."
                        (pi-coding-agent--shorten-model-name model-name)))
         (thinking (or (plist-get state :thinking-level) ""))
         (activity-phase (or (and chat-buf
                                  (buffer-local-value 'pi-coding-agent--activity-phase chat-buf))
                             "idle"))
         (activity-phase-str
          (propertize (format "%-8s" activity-phase)
                      'face 'pi-coding-agent-activity-phase)))
    (concat
     (pi-coding-agent--header-format-identity model-short thinking activity-phase-str)
     (pi-coding-agent--header-format-stats stats last-usage model-obj)
     (pi-coding-agent--header-format-context-group session-name)
     (pi-coding-agent--header-format-extension-group ext-status working-message))))

;;; State Management

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

;;;; Sending Infrastructure

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
Resets activity phase and status to idle."
  (when (buffer-live-p chat-buf)
    (with-current-buffer chat-buf
      (setq pi-coding-agent--status 'idle)
      (pi-coding-agent--set-activity-phase "idle"))))


(provide 'pi-coding-agent-ui)
;;; pi-coding-agent-ui.el ends here
