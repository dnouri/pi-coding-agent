;;; piem-ui.el --- Shared state, faces, and UI primitives -*- lexical-binding: t; -*-

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

;; Foundation module for piem: shared state, faces, customization,
;; buffer management, display primitives, header-line, and major modes.
;;
;; This is the base layer that all other piem modules require.
;; It provides:
;; - Customization options and face definitions
;; - Buffer-local session variables (the shared mutable state)
;; - Buffer creation, naming, and navigation
;; - Display primitives (append-to-chat, scroll preservation, separators)
;; - Header-line formatting and activity phases
;; - Sending infrastructure (send-prompt, abort-send)
;; - Major mode definitions (chat-mode, input-mode)

;;; Code:

(require 'piem-core)
(require 'cl-lib)
(require 'project)
(require 'md-ts-mode)
(require 'piem-grammars)
(require 'color)
(require 'diff-mode)


;; Forward declarations: keymaps bind functions defined in other modules.
;; Grouped by target module for easy cross-referencing.

;; piem-render.el (chat buffer commands)
(declare-function piem-toggle-tool-section "piem-render")
(declare-function piem-shell-command-at-point "piem-render")
(declare-function piem-visit-file "piem-render")
(declare-function piem--dispatch-button "piem-render")
(declare-function piem--cleanup-on-kill "piem-render")
(declare-function piem--restore-tool-properties "piem-render")
(declare-function piem--maybe-refresh-hot-tail-tables "piem-table")

;; piem-input.el (input buffer commands)
(declare-function piem-quit "piem-input")
(declare-function piem-send "piem-input")
(declare-function piem-attach-image "piem-input")
(declare-function piem-abort "piem-input")
(declare-function piem-previous-input "piem-input")
(declare-function piem-next-input "piem-input")
(declare-function piem-history-isearch-backward "piem-input")
(declare-function piem-queue-steering "piem-input")
(declare-function piem-input-mode "piem-input")

;; piem-browse.el (session browser)
(declare-function piem-session-browser "piem-browse")

;; piem-menu.el (menu and session commands)
(declare-function piem-menu "piem-menu")
(declare-function piem-new-session "piem-menu")
(declare-function piem-export-html "piem-menu")
(declare-function piem-compact "piem-menu")
(declare-function piem-select-model "piem-menu")
(declare-function piem-cycle-thinking "piem-menu")
(declare-function piem-fork-at-point "piem-menu")
(declare-function piem-copy-last-message "piem-menu")

;;;; Customization Group

(defgroup piem nil
  "Emacs frontend for pi coding agent."
  :group 'tools
  :prefix "piem-")

;;;; Customization

(defcustom piem-executable '("pi")
  "Command to invoke the pi binary, as a list of strings.
The first element is the program; remaining elements are passed
before \"--mode rpc\", `piem-extra-args', and the project
trust flag selected by `piem-project-trust-policy'.

For npx users:
  (setq piem-executable
        \\='(\"npx\" \"-y\" \"@earendil-works/pi-coding-agent@latest\"))"
  :type '(repeat string)
  :group 'piem)

(defcustom piem-project-trust-policy 'approve
  "How to pass Pi project trust flags when starting RPC sessions.
Pi does not show its built-in project trust prompt in RPC mode.  The
Emacs frontend therefore approves project-local Pi inputs by default so
`.pi' prompts, skills, settings, themes, and extensions are available.

Allowed values are:
- `approve'     Pass --approve and trust project-local files for this run.
- `default'     Pass no trust flag and let Pi use trust.json and
                defaultProjectTrust.
- `no-approve'  Pass --no-approve and ignore project-local files for this run."
  :type '(choice (const :tag "Approve project-local files" approve)
                 (const :tag "Use Pi's trust default" default)
                 (const :tag "Ignore project-local files" no-approve))
  :group 'piem)

(defcustom piem-rpc-timeout 30
  "Default timeout in seconds for synchronous RPC calls.
Some operations like model loading may need more time."
  :type 'natnum
  :group 'piem)

(defcustom piem-input-window-height 10
  "Height of the input window.
An integer specifies an absolute number of lines.
A float between 0.0 and 1.0 (exclusive) specifies a fraction of the
total window height, e.g. 0.3 means 30% for input."
  :type '(choice (natnum :tag "Lines")
                 (float :tag "Fraction (0.0–1.0)"))
  :group 'piem)

(defcustom piem-input-window-display 'always
  "How the input window is displayed alongside the chat window.
When `always' (the default), the input window is shown whenever the
session is displayed.

When `on-demand', the input window is shown when a session is first
launched, hidden after each send, and reopened with
\[piem-open-input].  Redisplaying an existing session
\(e.g. with `piem-toggle') shows only the chat window.

When `hidden', a session launches with only the chat window visible.
In every other respect this is like `on-demand': the input is hidden
after each send, reopened with `piem-open-input', and
redisplaying an existing session shows only the chat window.  This
suits a chat-centric workflow where you compose in the input only
when needed."
  :type '(choice (const :tag "Always visible" always)
                 (const :tag "On demand (hide after send)" on-demand)
                 (const :tag "Hidden at launch (chat only; open on demand)" hidden))
  :group 'piem)

(defcustom piem-activity-phase-functions nil
  "Functions called after a session activity phase is applied.
Each function is called with five arguments:

  CHAT-BUFFER INPUT-BUFFER OLD-PHASE NEW-PHASE REASON

NEW-PHASE is one of \"thinking\", \"replying\", \"running\",
\"compact\", or \"idle\".  INPUT-BUFFER may be nil or dead during
session teardown.

REASON is one of `phase-change', `reset', `teardown',
`input-link', or `input-unlink'.  This lets handlers distinguish a
real session phase change from buffer lifecycle events that merely
reapply or clean up buffer-local UI.

This is an abnormal hook.  Functions should be idempotent because
piem may call them again with the same OLD-PHASE and
NEW-PHASE when session buffers are relinked or reset."
  :type 'hook
  :group 'piem)

(defcustom piem-separator-width 72
  "Total width of section separators in chat buffer."
  :type 'natnum
  :group 'piem)

(defcustom piem-tool-preview-lines 10
  "Maximum visual lines to show before collapsing tool output."
  :type 'natnum
  :group 'piem)

(defcustom piem-bash-preview-lines 5
  "Maximum visual lines to show for bash output before collapsing.
Bash output is typically more verbose, so fewer lines are shown."
  :type 'natnum
  :group 'piem)

(defcustom piem-preview-max-bytes 51200
  "Maximum bytes for tool output preview (50KB default).
Prevents huge single-line outputs from blowing up the chat buffer."
  :type 'natnum
  :group 'piem)

(defcustom piem-image-preview-max-width 900
  "Maximum pixel width for inline image previews.
Previews are also constrained to the visible chat window."
  :type 'natnum
  :group 'piem)

(defcustom piem-image-preview-max-bytes (* 10 1024 1024)
  "Maximum source bytes decoded for one inline image preview.
Larger user-message or tool-result images use a textual placeholder."
  :type 'natnum
  :group 'piem)

(defcustom piem-prompt-image-max-bytes (* 3 1024 1024)
  "Maximum source bytes for the image attached to a prompt draft."
  :type 'natnum
  :group 'piem)

(defcustom piem-context-warning-threshold 70
  "Context usage percentage at which to show warning color."
  :type 'natnum
  :group 'piem)

(defcustom piem-context-error-threshold 90
  "Context usage percentage at which to show error color."
  :type 'natnum
  :group 'piem)

(defcustom piem-visit-file-other-window t
  "Whether RET requests the native opener for another window.
When non-nil, RET on a strict tool row, plain path reference, or local Markdown
link label calls `find-file-other-window'; when nil, it calls `find-file'.  A
prefix argument inverts that request.  Emacs display policy may redirect the
final window placement."
  :type 'boolean
  :group 'piem)

(defcustom piem-input-markdown-highlighting t
  "Whether to enable markdown syntax highlighting in the input buffer.
When non-nil, the input buffer gets tree-sitter markdown highlighting
\(bold, italic, code spans, fenced blocks) while keeping raw markdown
markup visible.  When nil, the input buffer uses plain `text-mode'.

Takes effect for new sessions; existing input buffers keep their mode."
  :type 'boolean
  :group 'piem)

(defcustom piem-copy-raw-markdown nil
  "Whether to copy raw markdown from the chat buffer.
When non-nil, copy commands (`kill-ring-save', `kill-region') preserve
raw markdown — bold markers (**), backticks, code fences, and setext
underlines are kept.  Useful for pasting into docs or other markdown-aware
contexts.

When nil (the default), only the visible text is copied."
  :type 'boolean
  :group 'piem)

(defcustom piem-extension-status-faces nil
  "Alist mapping extension status keys to faces in the header line.
Keys are exact `statusKey' strings sent by extension `setStatus' requests,
not necessarily extension package names.  Hovering header status text shows
the key to use here.  Values are face symbols or face attribute plists
accepted by `propertize'.

For example:
  \='((\"sub-status:usage\" . (:foreground \"#c6a0f6\"))
    (\"solveit-mode\" . warning))"
  :type '(alist :key-type string
                :value-type (choice (face :tag "Face")
                                     (plist :tag "Face attributes"
                                            :key-type symbol
                                            :value-type sexp)))
  :group 'piem)

(defcustom piem-quit-without-confirmation nil
  "Whether quitting skips confirmation for a live process.
When non-nil, closing a session never asks whether a running pi process
should be terminated.  When nil, `piem-quit', direct buffer
kills, and exiting Emacs all prompt before killing a live process."
  :type 'boolean
  :group 'piem)

(defcustom piem-hot-tail-turn-count 3
  "How many recent headed chat turns stay hot for redisplay refreshes.
The hot tail is the suffix of the chat buffer beginning at the Nth newest
`You' or `Assistant' setext heading.  Resize-sensitive features refresh only
inside that suffix; older history stays frozen until explicitly rebuilt."
  :type 'natnum
  :group 'piem)

(defcustom piem-thinking-display 'visible
  "Default display mode for completed assistant thinking in new chat buffers.
New chat buffers copy this user preference into a buffer-local session value.
Later per-buffer toggles affect only that chat buffer; they do not change this
user option.

Allowed values are:
- `visible'  Keep completed thinking expanded as blockquote markdown.
- `hidden'   Collapse completed thinking to a short stub line.

Live streaming thinking is always shown while the assistant is still working.
Per-block TAB toggles are temporary local overrides and are cleared by buffer
rebuilds, reloads, or whole-chat display-mode changes."
  :type '(choice (const :tag "Visible" visible)
                 (const :tag "Hidden" hidden))
  :group 'piem)

(defcustom piem-thinking-hidden-preview t
  "Whether hidden completed thinking should preview its first line.
When non-nil, collapsed completed thinking shows the first non-empty trimmed
line when the normalized thinking spans more than one line, is at least
3 characters long, and shorter than 72 characters. Otherwise the hidden block
falls back to a generic line-count label."
  :type 'boolean
  :group 'piem)

(defcustom piem-prettify-tables t
  "Whether display-only markdown tables use prettier visible separators.
When non-nil, table overlays replace raw markdown pipes and separator rows
with Unicode box-drawing characters in the visible display.  The underlying
buffer text stays canonical markdown, so copy, search, and session history
still operate on the raw table source."
  :type 'boolean
  :group 'piem)

;;;; Faces

(defface piem-timestamp
  '((t :inherit shadow))
  "Face for timestamps in message headers."
  :group 'piem)

(defface piem-tool-name
  '((t :inherit font-lock-function-name-face :weight bold :slant italic))
  "Face for tool names (BASH, READ, etc.) in pi chat."
  :group 'piem)

(defface piem-tool-command
  '((t :inherit font-lock-function-name-face :slant italic))
  "Face for tool commands and arguments."
  :group 'piem)

(defface piem-tool-output
  '((t :inherit shadow))
  "Face for tool output text."
  :group 'piem)

(defface piem-tool-block
  '((t :extend t))
  "Face for tool blocks.
Subtle blue-tinted background derived from the current theme."
  :group 'piem)

(defface piem-tool-block-error
  '((t :extend t))
  "Face for tool blocks after failed completion.
Background is derived from the current theme so syntax faces stay visible."
  :group 'piem)

(defface piem-diff-line-added
  '((t :extend t))
  "Face for added edit-diff lines.
Background is derived from the current theme so syntax faces stay visible."
  :group 'piem)

(defface piem-diff-line-removed
  '((t :extend t))
  "Face for removed edit-diff lines.
Background is derived from the current theme so syntax faces stay visible."
  :group 'piem)

(defface piem-collapsed-indicator
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for collapsed content indicators."
  :group 'piem)

(defface piem-model-name
  '((t :inherit font-lock-type-face))
  "Face for model name in header line."
  :group 'piem)

(defface piem-activity-phase
  '((t :inherit shadow))
  "Face for activity phase label in header line."
  :group 'piem)

(defface piem-retry-notice
  '((t :inherit warning :slant italic))
  "Face for retry notifications (rate limit, overloaded, etc.)."
  :group 'piem)

(defface piem-error-notice
  '((t :inherit error))
  "Face for error notifications from the server."
  :group 'piem)

;;;; Dynamic Face Computation

(defun piem--blend-color (base target amount)
  "Blend BASE color toward TARGET by AMOUNT (0.0–1.0).
Returns a hex color string.  AMOUNT of 0.0 returns BASE unchanged;
1.0 returns TARGET."
  (apply #'color-rgb-to-hex
         (cl-mapcar (lambda (b tgt)
                      (+ (* (- 1.0 amount) b) (* amount tgt)))
                    (color-name-to-rgb base)
                    (color-name-to-rgb target))))

(defun piem--dark-color-p (color)
  "Return non-nil when COLOR has low lightness."
  (< (nth 2 (apply #'color-rgb-to-hsl (color-name-to-rgb color))) 0.5))

(defun piem--theme-face-background (face)
  "Return FACE background color from the current theme, or nil."
  (let ((bg (face-background face nil t)))
    (and bg (color-defined-p bg) bg)))

(defun piem--theme-face-foreground (face)
  "Return FACE foreground color from the current theme, or nil."
  (let ((fg (face-foreground face nil t)))
    (and fg (color-defined-p fg) fg)))

(defun piem--theme-diff-background (diff-face indicator-face)
  "Return a syntax-friendly line background derived from DIFF-FACE.
Prefer DIFF-FACE's own background.  If the theme only colors diff
foregrounds, blend the default background toward DIFF-FACE's foreground,
falling back to INDICATOR-FACE when needed."
  (or (piem--theme-face-background diff-face)
      (when-let* ((bg (piem--theme-face-background 'default))
                  (tint (or (piem--theme-face-foreground diff-face)
                            (piem--theme-face-foreground indicator-face))))
        (piem--blend-color
         bg tint (if (piem--dark-color-p bg) 0.20 0.10)))))

(defun piem--set-face-background-only (face background)
  "Set FACE to contribute only BACKGROUND so syntax foregrounds stay visible."
  (set-face-attribute face nil
                      :inherit nil
                      :foreground 'unspecified
                      :background (or background 'unspecified)
                      :extend t))

(defun piem--update-tool-block-face ()
  "Set `piem-tool-block' background from theme."
  (when-let* ((bg (piem--theme-face-background 'default)))
    (let* ((dark-p (piem--dark-color-p bg))
           (tint (if dark-p "#5555cc" "#3333aa"))
           (amount (if dark-p 0.12 0.08)))
      (set-face-attribute
       'piem-tool-block nil
       :background
       (piem--blend-color bg tint amount)))))

(defun piem--update-tool-block-error-face ()
  "Set `piem-tool-block-error' background from theme."
  (piem--set-face-background-only
   'piem-tool-block-error
   (piem--theme-diff-background
    'diff-removed 'diff-indicator-removed)))

(defun piem--update-edit-diff-faces ()
  "Set edit-diff line faces from the current theme."
  (piem--set-face-background-only
   'piem-diff-line-added
   (piem--theme-diff-background
    'diff-added 'diff-indicator-added))
  (piem--set-face-background-only
   'piem-diff-line-removed
   (piem--theme-diff-background
    'diff-removed 'diff-indicator-removed)))

(defun piem--update-theme-derived-faces (&rest _)
  "Set internal faces derived from the current theme.
Updates tool blocks plus edit-diff overlays.  Called from mode setup and
on theme changes."
  (dolist (update '(piem--update-tool-block-face
                    piem--update-tool-block-error-face
                    piem--update-edit-diff-faces))
    (condition-case-unless-debug nil
        (funcall update)
      (error nil))))

;; Recompute when theme changes (Emacs 29+)
(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions
            #'piem--update-theme-derived-faces))

;;;; Language Detection

(defconst piem--extension-language-alist
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

(defsubst piem--tool-path (args)
  "Extract file path from tool ARGS.
Checks both :path and :file_path keys for compatibility."
  (or (plist-get args :path)
      (plist-get args :file_path)))

(defun piem--path-to-language (path)
  "Return language name for PATH based on file extension.
Returns \"text\" for unrecognized extensions to ensure consistent fencing.
Return nil when PATH is not a string."
  (when (stringp path)
    (let ((ext (downcase (or (file-name-extension path) ""))))
      (or (cdr (assoc ext piem--extension-language-alist))
          "text"))))

;;;; Major Modes

(defvar piem-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'piem-quit)
    (define-key map (kbd "C-c C-p") #'piem-menu)
    (define-key map (kbd "C-c C-k") #'piem-abort)
    (define-key map (kbd "C-c C-n") #'piem-new-session)
    (define-key map (kbd "C-c C-r") #'piem-session-browser)
    (define-key map (kbd "C-c C-e") #'piem-export-html)
    (define-key map (kbd "C-c C-c") #'piem-compact)
    (define-key map (kbd "C-c C-m") #'piem-select-model)
    (define-key map (kbd "C-c C-t") #'piem-cycle-thinking)
    (define-key map (kbd "C-c C-y") #'piem-copy-last-message)
    (define-key map (kbd "n") #'piem-next-message)
    (define-key map (kbd "p") #'piem-previous-message)
    (define-key map (kbd "f") #'piem-fork-at-point)
    (define-key map (kbd "TAB") #'piem-toggle-tool-section)
    (define-key map (kbd "<tab>") #'piem-toggle-tool-section)
    (define-key map (kbd "!") #'piem-shell-command-at-point)
    (define-key map (kbd "RET") #'piem-visit-file)
    (define-key map (kbd "<return>") #'piem-visit-file)
    (define-key map [remap push-button] #'piem--dispatch-button)
    map)
  "Keymap for `piem-chat-mode'.")

;;;; You Heading Detection

(defconst piem--you-heading-re
  "^You\\( · .*\\)?$"
  "Regex matching the first line of a user turn setext heading.
Matches `You' at line start, optionally followed by ` · <timestamp>'.
Must be verified with `piem--at-you-heading-p' to confirm
the next line is a setext underline (===), avoiding false matches on
user message text starting with \"You\".")

(defun piem--at-you-heading-p ()
  "Return non-nil if current line is a You setext heading.
Checks that the current line matches `piem--you-heading-re'
and the next line is a setext underline (three or more `=' characters)."
  (and (save-excursion
         (beginning-of-line)
         (looking-at piem--you-heading-re))
       (save-excursion
         (forward-line 1)
         (looking-at "^=\\{3,\\}$"))))

(defvar-local piem--hot-tail-start nil
  "Marker at the start of the recent hot-tail suffix.
Tables and future redisplay-sensitive subsystems refresh only at or after
this boundary.")

(defconst piem--turn-heading-re
  "^\\(?:You\\(?: · .*\\)?\\|Assistant\\)$"
  "Regex matching headed chat turns that define the hot-tail boundary.")

(defun piem--at-turn-heading-p ()
  "Return non-nil if current line is a hot-tail turn heading.
A turn heading is a `You' or `Assistant' setext heading whose next line is
an underline of three or more `=' characters."
  (and (save-excursion
         (beginning-of-line)
         (looking-at piem--turn-heading-re))
       (save-excursion
         (forward-line 1)
         (looking-at "^=\\{3,\\}$"))))

;;;; Turn Detection

(defun piem--collect-you-headings ()
  "Return list of buffer positions of all You setext headings.
Scans from `point-min', returns positions in chronological order."
  (let (headings)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward piem--you-heading-re nil t)
        (let ((pos (match-beginning 0)))
          (save-excursion
            (goto-char pos)
            (when (piem--at-you-heading-p)
              (push pos headings))))))
    (nreverse headings)))

(defun piem--user-turn-index-at-point (&optional headings)
  "Return 0-based index of the user turn at or before point.
HEADINGS is an optional pre-computed list from
`piem--collect-you-headings'; when nil, the buffer is scanned.
Returns nil if point is before the first You heading."
  (let ((headings (or headings (piem--collect-you-headings)))
        (limit (point))
        (index 0)
        (result nil))
    (dolist (h headings)
      (when (<= h limit)
        (setq result index))
      (setq index (1+ index)))
    result))

(defun piem--update-hot-tail-boundary ()
  "Move `piem--hot-tail-start' to the recent headed-turn suffix.
The marker lands on the Nth newest `You' or `Assistant' heading, where N is
`piem-hot-tail-turn-count'.  If there are at most N headed turns,
all content stays hot and the marker moves to `point-min'.  A count of 0
makes the hot region empty by moving the marker to `point-max'."
  (let ((headings nil)
        (count piem-hot-tail-turn-count))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward piem--turn-heading-re nil t)
        (let ((candidate (match-beginning 0)))
          (save-excursion
            (goto-char candidate)
            (when (piem--at-turn-heading-p)
              (push candidate headings))))))
    (setq headings (nreverse headings))
    (move-marker
     piem--hot-tail-start
     (cond
      ((zerop count) (point-max))
      ((<= (length headings) count) (point-min))
      (t (nth (- (length headings) count) headings)))
     (current-buffer))))

(defun piem--in-hot-tail-p (pos)
  "Return non-nil when POS is inside the hot tail."
  (>= pos (marker-position piem--hot-tail-start)))

;;;; Chat Navigation

(defun piem--find-you-heading (search-fn)
  "Find the next You setext heading using SEARCH-FN.
SEARCH-FN is `re-search-forward' or `re-search-backward'.
Returns the position of the heading line start, or nil if not found."
  (save-excursion
    (let ((found nil))
      (while (and (not found)
                  (funcall search-fn piem--you-heading-re nil t))
        (let ((candidate (match-beginning 0)))
          (save-excursion
            (goto-char candidate)
            (when (piem--at-you-heading-p)
              (setq found candidate)))))
      found)))

(defun piem-next-message ()
  "Move to the next user message in the chat buffer."
  (interactive)
  (let ((pos (save-excursion
               (forward-line 1)
               (piem--find-you-heading #'re-search-forward))))
    (if pos
        (progn
          (goto-char pos)
          (when (get-buffer-window) (recenter 0)))
      (message "No more messages"))))

(defun piem-previous-message ()
  "Move to the previous user message in the chat buffer."
  (interactive)
  (let ((pos (save-excursion
               (beginning-of-line)
               (piem--find-you-heading #'re-search-backward))))
    (if pos
        (progn
          (goto-char pos)
          (when (get-buffer-window) (recenter 0)))
      (message "No previous message"))))

;;;; Copy Visible Text

(defun piem--visible-text-span-p (position)
  "Return non-nil when buffer text at POSITION contributes visible text.
This deliberately follows the package's existing visible-copy semantics:
active `invisible' text and text whose `display' property is the empty string
are omitted; nonempty display replacements and overlay display strings are not
expanded into synthetic buffer characters."
  (let ((invisible (get-text-property position 'invisible))
        (display (get-text-property position 'display)))
    (and (not (and invisible (invisible-p invisible)))
         (not (equal display "")))))

(defun piem--position-inside-omitted-text-p (position beg end)
  "Return non-nil when POSITION has no visible boundary in BEG..END.
A position strictly inside one omitted run, or between adjacent omitted property
runs, is hidden because neither neighboring character contributes visible text.
The outer run boundaries remain usable as adjacent visible positions."
  (and (< position end)
       (> position beg)
       (not (piem--visible-text-span-p position))
       (not (piem--visible-text-span-p (1- position)))))

(defun piem--visible-text (beg end)
  "Return visible text between BEG and END, preserving text properties.
Skips characters with `invisible' property matching `buffer-invisibility-spec'
and characters with `display' property equal to the empty string.
The returned string carries face properties from font-lock, which
display overlay strings render faithfully (bold, italic, code, etc.)."
  (let ((result nil)
        (pos beg))
    (while (< pos end)
      (let ((next (min
                   (next-single-char-property-change pos 'invisible nil end)
                   (next-single-char-property-change pos 'display nil end))))
        (when (piem--visible-text-span-p pos)
          (push (buffer-substring pos next) result))
        (setq pos next)))
    (apply #'concat (nreverse result))))

(defun piem--visible-text-with-position-map (beg end position)
  "Project visible buffer text from BEG to END and map POSITION into it.
Return a plist with `:text', `:positions', and `:index'.
`:positions' is a vector parallel to `:text': element N is the exact buffer
position of visible character N.  `:index' is the visible boundary at POSITION,
namely the number of projected characters whose source positions precede it.
A nonempty visible half-open range [A,B) maps back to the real buffer envelope
from `(aref POSITIONS A)' through one past `(aref POSITIONS (1- B))'.  This
preserves hidden inline spans inside a visible candidate while excluding hidden
prefixes and suffixes from its bounds.

The caller owns bounding BEG and END; this helper never widens or fontifies the
buffer.  Its visibility rule is exactly `piem--visible-text''s and,
like that function, does not interpret overlay display replacement strings."
  (unless (and (<= beg position) (<= position end))
    (error "Position %s is outside visible input range %s..%s"
           position beg end))
  (let ((chunks nil)
        (source-positions (make-vector (- end beg) nil))
        (visible-count 0)
        (pos beg)
        index index-set)
    (while (< pos end)
      (let ((next (min
                   (next-single-char-property-change pos 'invisible nil end)
                   (next-single-char-property-change pos 'display nil end))))
        (if (piem--visible-text-span-p pos)
            (progn
              (push (buffer-substring-no-properties pos next) chunks)
              (unless index-set
                (when (<= position next)
                  (setq index (+ visible-count
                                 (max 0 (min (- position pos)
                                             (- next pos)))))
                  (setq index-set t)))
              (let ((source pos))
                (while (< source next)
                  (aset source-positions visible-count source)
                  (setq source (1+ source)
                        visible-count (1+ visible-count)))))
          (unless index-set
            (when (<= position next)
              (setq index visible-count
                    index-set t))))
        (setq pos next)))
    (list :text (apply #'concat (nreverse chunks))
          :positions (cl-subseq source-positions 0 visible-count)
          :index (or index visible-count))))

(defun piem--filter-buffer-substring (beg end &optional delete)
  "Filter function for `filter-buffer-substring-function' in chat buffers.
When `piem-copy-raw-markdown' is nil, returns only visible
text between BEG and END.  If DELETE is non-nil, also removes the region.
Raw copying keeps Markdown characters but strips internal render properties."
  (if piem-copy-raw-markdown
      (substring-no-properties (buffer-substring--filter beg end delete))
    (prog1 (substring-no-properties (piem--visible-text beg end))
      (when delete (delete-region beg end)))))

(defvar-local piem--canonical-buffer-name nil
  "Stable session buffer name for this chat buffer.
A chat buffer may also be backed by a transcript file, but session lookup
still uses this name to find the live conversation.")

(defvar-local piem--canonical-session-directory nil
  "Stable session directory for this chat buffer.
Project lookup, window toggling, and path completion use this directory even
when the buffer is also backed by a transcript file elsewhere.")

(defvar-local piem--canonical-session-name nil
  "Optional named-session suffix for this chat buffer.")

(defvar piem--chat-buffer)

(defun piem--chat-session-buffer-name (&optional buffer)
  "Return the stable session buffer name for chat BUFFER.
Falls back to the live `buffer-name' when BUFFER has no canonical name yet."
  (with-current-buffer (or buffer (current-buffer))
    (or piem--canonical-buffer-name
        (buffer-name))))

(defun piem--chat-session-directory (&optional buffer)
  "Return the stable session directory for chat BUFFER.
Falls back to BUFFER's `default-directory' when no canonical directory is
recorded yet."
  (with-current-buffer (or buffer (current-buffer))
    (or piem--canonical-session-directory
        default-directory)))

(defun piem--chat-session-name (&optional buffer)
  "Return the optional named-session suffix for chat BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    piem--canonical-session-name))

(defun piem--set-chat-session-identity (dir &optional session)
  "Record the stable session identity for the current chat buffer.
DIR is the session directory and SESSION is the optional named-session suffix."
  (setq piem--canonical-buffer-name
        (piem--buffer-name :chat dir session)
        piem--canonical-session-directory dir
        piem--canonical-session-name session
        default-directory dir))

(defun piem--restore-chat-buffer-read-only ()
  "Restore the normal read-only contract for chat buffers after saving."
  (setq buffer-read-only t))

(define-derived-mode piem-chat-mode md-ts-mode "Pi-Chat"
  "Major mode for displaying pi conversation.
Derives from `md-ts-mode' for tree-sitter syntax highlighting.
This is a read-only buffer showing the conversation history."
  :group 'piem
  (setq-local buffer-read-only t)
  ;; Chat buffers are generated read-only views.  Recording every incremental
  ;; streaming and rendering update retains large undo trees for content the
  ;; user cannot edit, so keep undo disabled for the lifetime of the buffer.
  (buffer-disable-undo)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  ;; Hide markdown markup (**, `, ```) for cleaner display
  (setq-local md-ts-hide-markup t)
  (md-ts--set-hide-markup t)
  ;; Strip hidden markup from copy operations (M-w, C-w)
  (setq-local filter-buffer-substring-function
              #'piem--filter-buffer-substring)
  (setq-local piem--thinking-display piem-thinking-display)
  (setq-local piem--tool-args-cache (make-hash-table :test 'equal))
  (setq-local piem--live-tool-blocks (make-hash-table :test 'equal))
  (setq-local piem--tool-block-order-counter 0)
  (setq-local piem--thinking-block-order-counter 0)
  (setq-local piem--history-load-generation 0)
  (setq-local piem--session-transition-generation 0)
  (setq-local piem--session-transition-active nil)
  (setq-local piem--model-change-generation 0)
  (setq-local piem--model-change-active-token nil)
  (setq-local piem--local-user-message-region nil)
  ;; Disable hl-line-mode: its post-command-hook overlay update causes
  ;; scroll oscillation in buffers with invisible text + variable heights.
  (setq-local global-hl-line-mode nil)
  (hl-line-mode -1)
  ;; Make window-point follow inserted text (like comint does).
  ;; This is key for natural scroll behavior during streaming.
  (setq-local window-point-insertion-type t)
  ;; Recent content is hot by default in a fresh chat buffer.
  (setq-local piem--hot-tail-start (copy-marker (point-min) nil))

  ;; Run after font-lock to undo markdown damage in tool overlays.
  (jit-lock-register #'piem--restore-tool-properties)

  ;; Compute theme-derived faces used by chat overlays.
  (piem--update-theme-derived-faces)

  ;; Saving a transcript should not make the live chat editable.
  (add-hook 'after-save-hook #'piem--restore-chat-buffer-read-only nil t)
  (add-hook 'window-configuration-change-hook
            #'piem--maybe-refresh-hot-tail-tables nil t)
  (add-hook 'window-size-change-functions
            #'piem--maybe-rebalance-windows)
  (add-hook 'kill-buffer-query-functions
            #'piem--session-kill-buffer-query nil t)
  (add-hook 'kill-buffer-hook #'piem--cleanup-on-kill nil t))

(put 'piem-chat-mode 'mode-class 'special)

(defun piem-complete ()
  "Complete at point, suppressing help text in the *Completions* buffer.
This wraps `completion-at-point' with `completion-show-help' bound to nil,
removing the instructional header that would otherwise appear."
  (interactive)
  (let ((completion-show-help nil))
    (completion-at-point)))

(defvar piem-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'piem-send)
    (define-key map (kbd "C-c C-a") #'piem-attach-image)
    (define-key map (kbd "TAB") #'piem-complete)
    (define-key map (kbd "C-c C-k") #'piem-abort)
    (define-key map (kbd "C-c C-p") #'piem-menu)
    (define-key map (kbd "C-c C-r") #'piem-session-browser)
    (define-key map (kbd "M-p") #'piem-previous-input)
    (define-key map (kbd "M-n") #'piem-next-input)
    (define-key map (kbd "<C-up>") #'piem-previous-input)
    (define-key map (kbd "<C-down>") #'piem-next-input)
    (define-key map (kbd "C-r") #'piem-history-isearch-backward)
    ;; Message queuing (steering only - follow-up handled by C-c C-c)
    (define-key map (kbd "C-c C-s") #'piem-queue-steering)
    map)
  "Keymap for `piem-input-mode'.")

;;;; Session Directory Detection

(defun piem--session-directory ()
  "Determine directory for the current pi session context.
Inside pi buffers, uses the chat buffer's stable session directory so manual
transcript saves do not retarget the live session.  Elsewhere, uses the
current project root when available, falling back to `default-directory'.
Always returns an expanded absolute path; remote TRAMP home text is preserved."
  (piem--route-preserving-expand-file-name
   (cond
    ((derived-mode-p 'piem-chat-mode)
     (piem--chat-session-directory))
    ((derived-mode-p 'piem-input-mode)
     (if (buffer-live-p piem--chat-buffer)
         (with-current-buffer piem--chat-buffer
           (piem--chat-session-directory))
       default-directory))
    (t
     (or (when-let* ((proj (project-current)))
           ;; `project-current' may return an instance whose backend
           ;; never defined a `project-root' method (older projectile
           ;; returns (projectile . DIR)).  Recover the root from that
           ;; cons shape, else degrade to `default-directory' instead
           ;; of crashing session startup on `cl-no-applicable-method'.
           (condition-case nil
               (project-root proj)
             (cl-no-applicable-method
              (when (and (consp proj) (stringp (cdr proj)))
                (cdr proj)))))
         default-directory)))))

;;;; Buffer Naming & Creation

(defun piem--buffer-name (type dir &optional session)
  "Generate buffer name for TYPE (:chat or :input) in DIR.
Optional SESSION name creates a named session.
Uses abbreviated directory for readability in buffer lists."
  (let ((type-str (pcase type
                    (:chat "chat")
                    (:input "input")))
        (abbrev-dir (piem--route-preserving-abbreviate-file-name
                     dir)))
    (if (and session (not (string-empty-p session)))
        (format "*piem-%s:%s<%s>*" type-str abbrev-dir session)
      (format "*piem-%s:%s*" type-str abbrev-dir))))

(defun piem--find-session (dir &optional session)
  "Find existing chat buffer for DIR and SESSION.
Matches the chat buffer's stable session identity, even when the buffer is
also visiting a transcript file and therefore has a different live name."
  (let ((target-name (piem--buffer-name :chat dir session)))
    (cl-find-if
     (lambda (buf)
       (and (buffer-live-p buf)
            (with-current-buffer buf
              (and (derived-mode-p 'piem-chat-mode)
                   (equal (piem--chat-session-buffer-name)
                          target-name)))))
     (buffer-list))))

(defun piem--get-or-create-buffer (type dir &optional session)
  "Get or create buffer of TYPE for DIR and optional SESSION.
TYPE is :chat or :input.  Returns the buffer.
Existing buffers keep their state; session metadata is refreshed explicitly
by session setup code."
  (let* ((name (piem--buffer-name type dir session))
         (existing (if (eq type :chat)
                       (piem--find-session dir session)
                     (get-buffer name)))
         (buf (or existing (generate-new-buffer name))))
    (unless existing
      (with-current-buffer buf
        (pcase type
          (:chat
           (piem-chat-mode)
           (piem--set-chat-session-identity dir session))
          (:input
           (piem-input-mode)
           (setq default-directory dir)))))
    buf))

;;;; Project Buffer Discovery

(defun piem--normalize-directory (dir)
  "Normalize DIR for exact path comparisons.
Returns an expanded absolute path with a trailing slash."
  (piem--route-preserving-file-name-as-directory
   (piem--route-preserving-expand-file-name dir)))

(defun piem-project-buffers ()
  "Return pi chat buffers for the current project directory.
Matches buffers by their stable session directory, not by the live buffer name
or transcript file location.  Returns a list ordered by `buffer-list'
recency, with the most recent buffer first."
  (let ((target-dir (piem--normalize-directory
                     (piem--session-directory))))
    (cl-remove-if-not
     (lambda (buf)
       (and (buffer-live-p buf)
            (with-current-buffer buf
              (and (derived-mode-p 'piem-chat-mode)
                   (stringp (piem--chat-session-directory))
                   (string=
                    (piem--normalize-directory
                     (piem--chat-session-directory))
                    target-dir)))))
     (buffer-list))))

;;;; Window Hiding

(defun piem--hide-session-windows ()
  "Hide the current pi session in the selected frame.
Preserves this frame's window layout by deleting input windows (the
child splits created by `piem--display-buffers') and
replacing chat windows with their previous buffers via `bury-buffer'.

Must be called from a pi chat or input buffer.  Only affects windows
of the current session in the selected frame."
  (let ((chat-buf (piem--get-chat-buffer))
        (input-buf (piem--get-input-buffer)))
    (when (buffer-live-p input-buf)
      (dolist (win (get-buffer-window-list input-buf nil))
        (ignore-errors (delete-window win))))
    (when (buffer-live-p chat-buf)
      (dolist (win (get-buffer-window-list chat-buf nil))
        (with-selected-window win
          (bury-buffer))))))

;;;; Buffer-Local Session Variables

(defvar-local piem--process nil
  "The pi RPC subprocess for this session.")

(defvar-local piem--process-version nil
  "Detected pi CLI version for the current process.")

(defvar-local piem--model-change-generation 0
  "Monotonic generation for asynchronous model-change callbacks.")

(defvar-local piem--model-change-active-token nil
  "Process-bound token owned by the active model change, or nil.")

(defun piem--begin-model-change (process &optional chat-buffer)
  "Begin a model change through PROCESS in CHAT-BUFFER and return its token.
Return nil if PROCESS is no longer current.  CHAT-BUFFER defaults to the
current buffer."
  (let ((buffer (or chat-buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq process piem--process)
          (setq piem--model-change-generation
                (1+ (or piem--model-change-generation 0)))
          (setq piem--model-change-active-token
                (cons piem--model-change-generation process)))))))

(defun piem--model-change-owned-p (token &optional chat-buffer)
  "Return non-nil when TOKEN owns CHAT-BUFFER's model-change gate.
CHAT-BUFFER defaults to the current buffer."
  (let ((buffer (or chat-buffer (current-buffer))))
    (and token
         (buffer-live-p buffer)
         (with-current-buffer buffer
           (and (eq token piem--model-change-active-token)
                (eql (car token) piem--model-change-generation))))))

(defun piem--model-change-current-p (token &optional chat-buffer)
  "Return non-nil when TOKEN owns CHAT-BUFFER's current-process model change.
CHAT-BUFFER defaults to the current buffer."
  (let ((buffer (or chat-buffer (current-buffer))))
    (and (piem--model-change-owned-p token buffer)
         (with-current-buffer buffer
           (eq (cdr token) piem--process)))))

(defun piem--finish-model-change (token &optional chat-buffer)
  "Finish CHAT-BUFFER's model change only when TOKEN still owns it.
Unlike applying its response, cleanup does not require TOKEN's process to
remain current.  CHAT-BUFFER defaults to the current buffer."
  (let ((buffer (or chat-buffer (current-buffer))))
    (when (piem--model-change-owned-p token buffer)
      (with-current-buffer buffer
        (setq piem--model-change-active-token nil))
      t)))

(defun piem--invalidate-model-change (&optional chat-buffer)
  "Invalidate any model change in CHAT-BUFFER and return the new generation.
CHAT-BUFFER defaults to the current buffer."
  (let ((buffer (or chat-buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq piem--model-change-generation
              (1+ (or piem--model-change-generation 0))
              piem--model-change-active-token nil)
        piem--model-change-generation))))

(defun piem--model-change-pending-p (&optional chat-buffer)
  "Return whether CHAT-BUFFER has an active model change.
CHAT-BUFFER defaults to the current buffer."
  (let ((buffer (or chat-buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (buffer-local-value 'piem--model-change-active-token buffer)
         t)))

(defun piem--cancel-model-change-and-restore-followups
    (&optional chat-buffer)
  "Cancel CHAT-BUFFER's model change and restore text queued behind it."
  (let ((buffer (or chat-buffer (current-buffer))))
    (when (and (buffer-live-p buffer)
               (piem--model-change-pending-p buffer))
      (with-current-buffer buffer
        (piem--invalidate-model-change)
        (piem--restore-followup-queue-to-input))
      t)))

(defun piem--set-process (process)
  "Set the pi RPC subprocess PROCESS for this session.
Resets cached process version and starts a delayed version probe for
new live processes in interactive sessions."
  (unless (eq process piem--process)
    (piem--invalidate-model-change))
  (setq piem--process process
        piem--process-version nil)
  (when (and (processp process)
             (process-live-p process)
             (not noninteractive))
    (piem--probe-process-version-async (current-buffer))))

(defvar-local piem--chat-buffer nil
  "Reference to the chat buffer for this session.")

(defun piem--set-chat-buffer (buffer)
  "Set the chat BUFFER reference for this session.
In input buffers, also store BUFFER in `other-window-scroll-buffer'
so built-in other-window scrolling commands target the linked chat."
  (setq piem--chat-buffer buffer)
  (when (derived-mode-p 'piem-input-mode)
    (setq-local other-window-scroll-buffer buffer)))

(defvar-local piem--input-buffer nil
  "Reference to the input buffer for this session.")

(defvar piem--activity-phase)

(defun piem--set-input-buffer (buffer)
  "Set the input BUFFER reference for this session."
  (let ((old-buffer piem--input-buffer)
        (phase piem--activity-phase))
    (unless (eq old-buffer buffer)
      (when (and old-buffer (buffer-live-p old-buffer))
        (piem--run-activity-phase-functions
         (current-buffer) old-buffer phase "idle" 'input-unlink))
      (setq piem--input-buffer buffer)
      (when (and buffer (buffer-live-p buffer))
        (piem--set-activity-phase phase 'input-link t)))))

(defvar-local piem--thinking-display nil
  "Completed-thinking display mode for this chat buffer.
One of the symbols `visible' or `hidden'. Live streaming thinking is always
shown while the assistant is still working; this mode is applied when a
thinking block completes and whenever completed thinking is redisplayed later.
Temporary per-block TAB toggles do not change this buffer-local preference.")

(defun piem--set-thinking-display (mode)
  "Set completed-thinking display MODE for the current chat buffer."
  (setq piem--thinking-display mode))

(defun piem--thinking-display-mode ()
  "Return the active completed-thinking display mode for this chat buffer."
  (or piem--thinking-display
      piem-thinking-display
      'visible))

(defvar-local piem--canonical-messages nil
  "Canonical session messages cached for idle history rebuilds.
This is updated from successful history loads and completed agent turns.  It is
used when the buffer needs a canonical transcript again, such as reload,
resume, fork, or explicit history rerenders, so the buffer does not have to
parse rendered text back into message structure.")

(defun piem--set-canonical-messages (messages)
  "Set canonical session MESSAGES for the current chat buffer."
  (setq piem--canonical-messages messages))

(defvar-local piem--history-load-generation 0
  "Monotonic generation number for in-flight canonical history loads.
Each new history request or local outbound send bumps this counter so stale
callbacks cannot rebuild the chat buffer over newer session state.")

(defun piem--set-history-load-generation (generation)
  "Set canonical history-load GENERATION for the current chat buffer."
  (setq piem--history-load-generation generation))

(defun piem--invalidate-history-loads ()
  "Invalidate pending canonical history requests and return the new generation."
  (let ((next (1+ (or piem--history-load-generation 0))))
    (piem--set-history-load-generation next)
    next))

(defvar-local piem--session-transition-generation 0
  "Monotonic generation for async session-transition callbacks.
Each session switch, fork, or reset bumps this counter so stale callbacks
cannot apply older session identity or header state over a newer session view.")

(defvar-local piem--session-transition-active nil
  "Non-nil while a session switch or fork RPC is in flight.")

(defvar-local piem--session-transition-process nil
  "Process allowed to complete the active session transition.")

(defun piem--set-session-transition-generation (generation)
  "Set session-transition GENERATION for the current chat buffer."
  (setq piem--session-transition-generation generation))

(defun piem--begin-session-transition (&optional proc)
  "Invalidate pending session-transition callbacks and return the new generation.
Optional PROC may complete the transition before it becomes the current process."
  (let ((next (1+ (or piem--session-transition-generation 0))))
    (piem--set-session-transition-generation next)
    (setq piem--session-transition-active t
          piem--session-transition-process proc)
    next))

(defun piem--finish-session-transition (generation)
  "Mark session transition GENERATION finished when it is still current."
  (when (= generation piem--session-transition-generation)
    (setq piem--session-transition-active nil
          piem--session-transition-process nil)))

(defun piem--session-transition-active-p (&optional chat-buf)
  "Return non-nil when CHAT-BUF is switching sessions or forking."
  (with-current-buffer (or chat-buf (current-buffer))
    (and piem--session-transition-active t)))

(defun piem--session-transition-current-p (chat-buf proc generation)
  "Return non-nil when CHAT-BUF still expects PROC at GENERATION.
This keeps async session-transition callbacks from older switches, forks, or
resets from overwriting the current chat buffer state."
  (and (buffer-live-p chat-buf)
       (with-current-buffer chat-buf
         (and (or (eq piem--process proc)
                  (eq piem--session-transition-process proc))
              (= generation piem--session-transition-generation)))))

(defvar-local piem--streaming-marker nil
  "Marker for current streaming insertion point.")

(defun piem--set-streaming-marker (marker)
  "Set the streaming insertion point MARKER."
  (setq piem--streaming-marker marker))

(defvar-local piem--in-code-block nil
  "Non-nil when streaming inside a fenced code block.
Used to suppress ATX heading transforms inside code.")

(defvar-local piem--in-thinking-block nil
  "Non-nil while processing a thinking block for the current message.
Used for lifecycle resets when new messages or turns begin.")

(defvar-local piem--thinking-marker nil
  "Marker for insertion point inside the current thinking block.
Unlike `piem--streaming-marker', this marker stays anchored
in thinking text when other content blocks (for example, tool headers)
interleave during streaming.")

(defvar-local piem--thinking-start-marker nil
  "Marker for the start of the current thinking block.
Used to rewrite thinking content in place after whitespace normalization.")

(defvar-local piem--thinking-raw nil
  "Accumulated raw thinking deltas for the current thinking block.
Normalized and re-rendered incrementally to avoid excess whitespace.")

(defvar-local piem--thinking-prev-rendered nil
  "Previously rendered blockquote text for the current thinking block.
Used for incremental rendering: when the new rendered text extends the
previous text, only the suffix is inserted instead of replacing the
entire region.  Reset by `piem--reset-thinking-state'.")

(defvar-local piem--line-parse-state 'line-start
  "Parsing state for current line during streaming.
Values:
  `line-start' - at beginning of line, ready for heading or fence
  `fence-1'    - seen one backtick at line start
  `fence-2'    - seen two backticks at line start
  `mid-line'   - somewhere in middle of line

Starts as `line-start' because content begins after separator newline.")

;; piem--status is defined in piem-core.el as the single source of truth
;; for session activity state (idle, sending, streaming, compacting)

(defvar-local piem--activity-phase "idle"
  "Fine-grained activity phase for header-line display.
One of \"thinking\", \"replying\", \"running\",
\"compact\", or \"idle\".
Always populated and rendered in a fixed-width slot.")

(defun piem--run-activity-phase-functions
    (chat-buf input-buf old-phase new-phase reason)
  "Run activity phase functions for CHAT-BUF and INPUT-BUF.
OLD-PHASE is the previously applied phase.  NEW-PHASE is the phase that
is now applied.  REASON explains why the application happened.  User functions
are isolated so a customization error cannot break rendering or state
transitions."
  (dolist (fn piem-activity-phase-functions)
    (condition-case-unless-debug err
        (funcall fn chat-buf input-buf old-phase new-phase reason)
      (error
       (display-warning
        'piem
        (format "Activity phase function %S failed: %s"
                fn (error-message-string err))
        :error)))))

(defun piem--set-activity-phase (phase &optional reason force)
  "Set activity PHASE for header-line display in current chat buffer.
PHASE should be one of \"thinking\", \"replying\",
\"running\", \"compact\", or \"idle\".  REASON defaults to
`phase-change'.  When FORCE is non-nil, rerun
`piem-activity-phase-functions' even if PHASE did not change.
Returns non-nil when the phase changed."
  (let ((chat-buf (piem--get-chat-buffer))
        (reason (or reason 'phase-change)))
    (if (and chat-buf
             (buffer-live-p chat-buf)
             (not (eq chat-buf (current-buffer))))
        (with-current-buffer chat-buf
          (piem--set-activity-phase phase reason force))
      (let* ((old-phase piem--activity-phase)
             (changed (not (equal old-phase phase))))
        (when (or changed force)
          (setq piem--activity-phase phase)
          (when changed
            (force-mode-line-update t))
          (piem--run-activity-phase-functions
           (current-buffer) piem--input-buffer old-phase phase reason))
        changed))))

(defvar-local piem--cached-stats nil
  "Cached session statistics for header-line display.
Updated after each agent turn completes.")

(defvar-local piem--aborted nil
  "Non-nil if the current/last request was aborted.")

(defun piem--set-aborted (value)
  "Set the aborted flag to VALUE."
  (setq piem--aborted value))

(defvar-local piem--message-start-marker nil
  "Marker for start of current message content.
Used to replace raw markdown with rendered Org on message completion.")

(defun piem--set-message-start-marker (marker)
  "Set the message start MARKER."
  (setq piem--message-start-marker marker))

(defvar-local piem--tool-args-cache nil
  "Hash table mapping toolCallId to authoritative execution args.
Needed because `tool_execution_end' events do not include args.  This is
per-turn state and is cleared on turn end, history rebuild, and session reset.")

(defvar-local piem--live-tool-blocks nil
  "Hash table mapping toolCallId to live tool block records.
Concurrent preview and execution lifecycle work is keyed through this
registry so each live block keeps its own output and metadata.")

(defvar-local piem--tool-block-order-counter 0
  "Monotonic counter used to stamp tool block ordering metadata.")

(defvar-local piem--thinking-block-order-counter 0
  "Monotonic counter used to stamp completed thinking block metadata.")

(defvar-local piem--pending-tool-overlay nil
  "Compatibility overlay slot for legacy non-keyed helper paths.
Keyed live block helpers are authoritative for concurrent preview and
execution; this slot remains only for older single-tool flows.")

(defvar-local piem--assistant-header-shown nil
  "Non-nil if Assistant header has been shown for current prompt.
Used to avoid duplicate headers during retry sequences.")

(cl-defstruct (piem--prompt-image
               (:constructor piem--make-prompt-image))
  "Materialized image attached to one input-buffer prompt draft."
  name
  mime-type
  byte-size
  data)

(defvar-local piem--draft-prompt-image nil
  "Materialized prompt image attached to the current input draft.")

(defun piem--get-prompt-image (&optional input-buffer)
  "Return the draft prompt image in INPUT-BUFFER or the current buffer."
  (let ((buffer (or input-buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (buffer-local-value 'piem--draft-prompt-image buffer))))

(defun piem--set-prompt-image (image &optional input-buffer)
  "Set IMAGE as the draft prompt image in INPUT-BUFFER or current buffer."
  (let ((buffer (or input-buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq piem--draft-prompt-image image)
        (force-mode-line-update t)))
    image))

(defun piem--clear-prompt-image (&optional input-buffer)
  "Clear the draft prompt image in INPUT-BUFFER or the current buffer."
  (piem--set-prompt-image nil input-buffer))

(defun piem--prompt-image-content-block (image)
  "Return the RPC image content block for prompt IMAGE."
  (list :type "image"
        :data (piem--prompt-image-data image)
        :mimeType (piem--prompt-image-mime-type image)))

(defun piem--replace-input-draft (input-buffer text)
  "Replace INPUT-BUFFER's draft with TEXT and clear its prompt image."
  (when (buffer-live-p input-buffer)
    (with-current-buffer input-buffer
      (erase-buffer)
      (when text
        (insert text))
      (piem--clear-prompt-image)
      (goto-char (point-max)))))

(defvar-local piem--followup-queue nil
  "List of follow-up messages queued while agent is busy.
Messages are added when the user sends while streaming, compacting, or
waiting for local prompt preflight, post-run drain, or automatic retry.  The
oldest message is sent after the session settles, and dropped only after
prompt preflight accepts it.  This is simpler than using pi's RPC follow_up
command.")

(defun piem--push-followup (message)
  "Push MESSAGE onto the follow-up queue."
  (push message piem--followup-queue))

(defun piem--dequeue-followup ()
  "Dequeue and return the oldest follow-up message, or nil if empty.
Follow-ups are processed in FIFO order: first pushed, first sent."
  (when piem--followup-queue
    (let ((text (car (last piem--followup-queue))))
      (setq piem--followup-queue
            (butlast piem--followup-queue))
      text)))

(defun piem--clear-followup-queue ()
  "Clear all pending follow-up messages."
  (setq piem--followup-queue nil))

(defun piem--followups-in-fifo-order ()
  "Return queued follow-up messages in the order they would be sent."
  (reverse piem--followup-queue))

(defun piem--restore-input-text (text &optional prompt-image)
  "Restore TEXT and optional PROMPT-IMAGE to the linked input buffer.
Recovered text is older than any draft currently in the input buffer, so it is
placed first and separated from the draft by a blank line."
  (when-let* ((input-buf piem--input-buffer)
              ((buffer-live-p input-buf)))
    (with-current-buffer input-buf
      (let ((draft (buffer-string)))
        (erase-buffer)
        (insert text)
        (unless (string-empty-p draft)
          (insert "\n\n" draft))
        (when prompt-image
          (piem--set-prompt-image prompt-image))
        (goto-char (point-max))))))

(defun piem--restore-followup-queue-to-input ()
  "Move all queued follow-ups back to the input buffer and clear the queue.
If the linked input buffer is gone, leave the queue intact rather than losing
user text."
  (when-let* (((buffer-live-p piem--input-buffer))
              ((consp piem--followup-queue)))
    (let ((text (mapconcat #'identity
                           (piem--followups-in-fifo-order)
                           "\n\n")))
      (piem--clear-followup-queue)
      (piem--restore-input-text text))))

(defun piem--peek-followup ()
  "Return the oldest queued follow-up message without removing it."
  (car (last piem--followup-queue)))

(defun piem--drop-followup (message)
  "Remove MESSAGE when it is the oldest queued follow-up.
Return non-nil when a message was removed.  Follow-ups are acknowledged after
prompt preflight succeeds, so rejected queued prompts remain available."
  (when (and piem--followup-queue
             (equal (piem--peek-followup) message))
    (piem--dequeue-followup)
    t))

(defvar-local piem--followup-drain-timer nil
  "Timer waiting to drain the local follow-up queue after Pi settles.")

(defun piem--followup-drain-pending-p ()
  "Return non-nil when a local follow-up drain is pending."
  (and piem--followup-drain-timer t))

(defvar-local piem--local-user-message nil
  "Locally displayed user turn awaiting pi's authoritative echo.
A string records an existing text-only turn.  An image turn stores its full
normalized content vector so text and image blocks must both match.  Nil means
there is no local echo to suppress.  The value is cleared on message_start;
when the authoritative turn differs, pi's version is also displayed (for
example, after prompt or image transformation).")

(defvar-local piem--local-user-message-region nil
  "Marker pair bounding the locally displayed user turn awaiting pi's echo.")

(defun piem--clear-local-user-message-region ()
  "Detach and clear markers for the locally displayed pending user turn."
  (when (consp piem--local-user-message-region)
    (set-marker (car piem--local-user-message-region) nil)
    (set-marker (cdr piem--local-user-message-region) nil))
  (setq piem--local-user-message-region nil))

(defvar-local piem--prompt-start-wait-active nil
  "Non-nil while a prompt is waiting for response, agent_start, or fallback.")

(defun piem--prompt-start-wait-active-p ()
  "Return non-nil when local prompt preflight still owns the next turn."
  (and piem--prompt-start-wait-active t))

(defun piem--session-busy-p (&optional chat-buf)
  "Return non-nil when CHAT-BUF has active or locally pending work.
When CHAT-BUF is nil, inspect the current buffer.  This includes Pi-owned
activity from `piem--status' plus model changes, session
transitions, prompt preflight, and follow-up drain waits."
  (with-current-buffer (or chat-buf (current-buffer))
    (or (memq piem--status '(sending streaming compacting))
        (piem--model-change-pending-p)
        (piem--session-transition-active-p)
        (piem--prompt-start-wait-active-p)
        (piem--followup-drain-pending-p))))

(defun piem--canonical-rerender-safe-p ()
  "Return non-nil when the chat buffer may rebuild from canonical messages.
A locally displayed user prompt awaiting pi's echo is newer than the cached
canonical history, so rebuilding now would erase that visible turn."
  (and (eq piem--status 'idle)
       (not (piem--prompt-start-wait-active-p))
       (not (piem--followup-drain-pending-p))
       (null piem--local-user-message)))

(defvar-local piem--extension-status nil
  "Alist of extension status messages for header-line display.
Keys are extension identifiers (strings), values are status text.")

(defvar-local piem--working-message nil
  "Transient extension working message for header-line display.")

(defvar-local piem--unsupported-extension-ui-methods-warned nil
  "Unsupported extension UI method names already warned for this pi session.")

(defun piem--record-unsupported-extension-ui-warning (method)
  "Record an unsupported extension UI warning for METHOD.
Return non-nil when METHOD had not already been warned for this pi session."
  (unless (member method piem--unsupported-extension-ui-methods-warned)
    (push method piem--unsupported-extension-ui-methods-warned)
    t))

(defun piem--clear-unsupported-extension-ui-warnings ()
  "Forget unsupported extension UI warnings for the current pi session."
  (setq piem--unsupported-extension-ui-methods-warned nil))

(defvar-local piem--session-name nil
  "Cached session name for header-line display.
Extracted from session_info entries when session is loaded or switched.")

(defvar-local piem--commands nil
  "List of available commands from pi.
Each entry is a plist with :name, :source, and :description.
Optional :location (\"user\" or \"project\") and :path may be present.
Source is \"prompt\", \"extension\", or \"skill\".")

(defvar piem--builtin-commands
  '(("compact" :handler piem-compact       :args optional)
    ("new"     :handler piem-new-session)
    ("model"   :handler piem-select-model  :args optional)
    ("session" :handler piem-session-stats)
    ("name"    :handler piem-set-session-name :args required)
    ("fork"    :handler piem-fork)
    ("resume"  :handler piem-session-browser)
    ("reload"  :handler piem-reload)
    ("export"  :handler piem-export-html  :args optional)
    ("copy"    :handler piem-copy-last-message)
    ("quit"    :handler piem-quit))
  "Built-in slash commands dispatched client-side.
Each entry is (NAME . PLIST) where PLIST has:
  :handler  Function to call (symbol)
  :args     nil (no args), `optional', or `required'

Commands with :args `optional' pass the trailing text (or nil) to the
handler.  Commands with :args `required' prompt interactively when no
argument is given (the handler's `interactive' spec handles this).
Descriptions come from the handler's docstring.")

(defun piem--builtin-command-name (text)
  "Return the built-in slash command name in TEXT, or nil."
  (when (and (stringp text)
             (string-prefix-p "/" text))
    (let* ((without-slash (substring text 1))
           (words (split-string without-slash))
           (name (car words)))
      (and (assoc name piem--builtin-commands)
           name))))

(defun piem--builtin-command-text-p (text)
  "Return non-nil when TEXT names a client-side built-in command."
  (and (piem--builtin-command-name text) t))

(defun piem--set-commands (commands)
  "Set COMMANDS in current buffer and propagate to sibling session buffers.
COMMANDS is a list of plists with :name, :description, :source.
Both chat and input buffers share the same commands list, so this
setter updates all of them to keep them in sync."
  (setq piem--commands commands)
  (let ((chat-buf (piem--get-chat-buffer))
        (input-buf (piem--get-input-buffer)))
    (dolist (buf (list chat-buf input-buf))
      (when (and (buffer-live-p buf)
                 (not (eq buf (current-buffer))))
        (with-current-buffer buf
          (setq piem--commands commands))))))

;;;; Buffer Navigation

(defun piem--get-chat-buffer ()
  "Get the chat buffer for the current session.
Works from either chat or input buffer."
  (if (derived-mode-p 'piem-chat-mode)
      (current-buffer)
    piem--chat-buffer))

(defun piem--get-input-buffer ()
  "Get the input buffer for the current session.
Works from either chat or input buffer."
  (if (derived-mode-p 'piem-input-mode)
      (current-buffer)
    piem--input-buffer))

(defun piem--get-process ()
  "Get the pi process for the current session.
Works from either chat or input buffer."
  (if (derived-mode-p 'piem-chat-mode)
      piem--process
    (and piem--chat-buffer
         (buffer-local-value 'piem--process piem--chat-buffer))))

(defun piem--session-live-process-p (proc)
  "Return non-nil when PROC is a live process object."
  (and (processp proc) (process-live-p proc)))

(defun piem--process-kill-confirmation-required-p (proc)
  "Return non-nil when killing PROC should ask the user first."
  (and (piem--session-live-process-p proc)
       (not piem-quit-without-confirmation)
       (not (process-get proc 'piem-skip-kill-confirmation))))

(defun piem--skip-process-kill-confirmation (proc)
  "Suppress pi's own kill confirmation for PROC during intentional teardown."
  (when (processp proc)
    (process-put proc 'piem-skip-kill-confirmation t)))

(defun piem--session-kill-buffer-query ()
  "Ask before killing a session buffer would terminate a live pi process."
  (let ((proc (piem--get-process)))
    (or (not (piem--process-kill-confirmation-required-p proc))
        (yes-or-no-p "Pi session has a running process; kill it? "))))

(defun piem--session-kill-emacs-query ()
  "Ask before exiting Emacs terminates a live pi session process.
Session processes are started with `:noquery', so Emacs' own exit query
does not see them; this function replaces it with pi's confirmation.
Return nil to abort the exit."
  (or (not (cl-some (lambda (proc)
                      (and (process-get proc 'piem-chat-buffer)
                           (piem--process-kill-confirmation-required-p
                            proc)))
                    (process-list)))
      (yes-or-no-p "Pi session has a running process; exit anyway? ")))

;; Closing the last frame kills Emacs without killing session buffers, so
;; `kill-buffer-query-functions' never runs.  Guard that path explicitly.
(add-hook 'kill-emacs-query-functions
          #'piem--session-kill-emacs-query)

(defun piem--retarget-session-buffers (dir)
  "Retarget the current chat/input session buffers to DIR."
  (let* ((chat-buf (piem--get-chat-buffer))
         (session (and (buffer-live-p chat-buf)
                       (piem--chat-session-name chat-buf)))
         (input-buf (and (buffer-live-p chat-buf)
                         (buffer-local-value 'piem--input-buffer
                                             chat-buf)))
         (existing (piem--find-session dir session)))
    (unless (buffer-live-p chat-buf)
      (user-error "No pi session buffer"))
    (when (and existing (not (eq existing chat-buf)))
      (user-error "Pi session already open for: %s" dir))
    (with-current-buffer chat-buf
      (piem--set-chat-session-identity dir session)
      (rename-buffer piem--canonical-buffer-name))
    (when (buffer-live-p input-buf)
      (with-current-buffer input-buf
        (setq default-directory dir)
        (rename-buffer (piem--buffer-name :input dir session))
        (piem--set-chat-buffer chat-buf)))))

;;;; Display

(defun piem--window-can-split-for-input-p (window)
  "Return non-nil if WINDOW can be split into chat and input windows."
  (>= (window-total-height window)
      (* 2 window-min-height)))

(defun piem--input-height-for-window-height (total)
  "Compute input pane height for a container of TOTAL lines.
When `piem-input-window-height' is an integer, use it directly.
When it is a float, compute the height as that fraction of TOTAL.
In both cases, clamp the result to the range
\[`window-min-height', TOTAL - `window-min-height']."
  (let* ((max-input-height (- total window-min-height))
         (raw (if (floatp piem-input-window-height)
                  (round (* piem-input-window-height total))
                piem-input-window-height)))
    (max window-min-height
         (min raw max-input-height))))

(defun piem--input-height-for-window (window)
  "Return input pane height to use when splitting WINDOW."
  (piem--input-height-for-window-height
   (window-total-height window)))

(defun piem--rebalance-input-window (chat-win input-win)
  "Adjust INPUT-WIN height to match the configured ratio.
CHAT-WIN and INPUT-WIN must be a vertically stacked pair.
Only resizes when `piem-input-window-height' is a float."
  (when (and (floatp piem-input-window-height)
             (window-live-p chat-win)
             (window-live-p input-win))
    (let* ((total (+ (window-total-height chat-win)
                     (window-total-height input-win)))
           (target (piem--input-height-for-window-height total))
           (current (window-total-height input-win))
           (delta (- target current)))
      (unless (zerop delta)
        (window-resize input-win delta nil t)))))

(defun piem--maybe-rebalance-windows (_frame)
  "Rebalance pi chat/input window pairs after a frame size change.
Intended for `window-size-change-functions'."
  (when (floatp piem-input-window-height)
    (dolist (win (window-list nil 'no-mini))
      (when-let* ((input-buf (buffer-local-value
                              'piem--input-buffer
                              (window-buffer win)))
                  (input-win (get-buffer-window input-buf)))
        (unless (eq win input-win)
          (piem--rebalance-input-window win input-win))))))

(defun piem--windows-by-height (&optional windows)
  "Return live WINDOWS sorted by descending height.
If WINDOWS is nil, use all non-minibuffer windows in the selected frame."
  (sort (cl-remove-if-not #'window-live-p
                          (copy-sequence (or windows (window-list nil 'no-mini))))
        (lambda (a b)
          (> (window-total-height a)
             (window-total-height b)))))

(defun piem--window-with-most-height (&optional windows)
  "Return the tallest window from WINDOWS.
If WINDOWS is nil, use all non-minibuffer windows in the selected frame."
  (car (piem--windows-by-height windows)))

(defun piem--best-display-window (&optional preferred)
  "Return best window for displaying chat+input.
Use PREFERRED when it can be split, else pick the tallest splittable
window in the frame.  Falls back to PREFERRED or selected window."
  (or (and preferred
           (window-live-p preferred)
           (piem--window-can-split-for-input-p preferred)
           preferred)
      (cl-find-if #'piem--window-can-split-for-input-p
                  (piem--windows-by-height))
      preferred
      (selected-window)))

(defun piem--preferred-display-window (chat-wins input-wins selected)
  "Return preferred base window for displaying chat+input.
CHAT-WINS and INPUT-WINS are existing session windows.  SELECTED is the
currently selected window."
  (cond
   ;; Input-only visible: prefer selected non-input window so we can
   ;; replace it cleanly and avoid duplicate input windows.
   ((and input-wins (not chat-wins)
         (not (memq selected input-wins))
         (piem--window-can-split-for-input-p selected))
    selected)
   (chat-wins (piem--window-with-most-height chat-wins))
   (input-wins (piem--window-with-most-height input-wins))
   (t selected)))

(defun piem--delete-extra-input-windows (input-wins target)
  "Delete windows in INPUT-WINS except TARGET."
  (dolist (win input-wins)
    (unless (eq win target)
      (ignore-errors (delete-window win)))))

(defun piem--paired-input-window (chat-win input-buf)
  "Return input window below CHAT-WIN showing INPUT-BUF, or nil."
  (when (window-live-p chat-win)
    (let ((below (window-in-direction 'below chat-win)))
      (and below
           (eq (window-buffer below) input-buf)
           below))))

(defun piem--best-input-window (chat-buf input-buf)
  "Return best visible window for INPUT-BUF in current frame, or nil.
Prefer the input window below the selected CHAT-BUF window, then the
selected input window, then the tallest input window."
  (when-let* ((input-wins (get-buffer-window-list input-buf nil)))
    (let* ((selected (selected-window))
           (selected-chat-win (and (eq (window-buffer selected) chat-buf)
                                   selected)))
      (or (piem--paired-input-window selected-chat-win input-buf)
          (and (memq selected input-wins)
               selected)
          (piem--window-with-most-height input-wins)))))

(defun piem--focus-input-window (chat-buf input-buf)
  "Select a visible INPUT-BUF window for the CHAT-BUF session."
  (when-let* ((win (piem--best-input-window chat-buf input-buf)))
    (select-window win)))

(defun piem--split-input-below-chat (chat-buf input-buf)
  "Show INPUT-BUF in a new window below the best visible CHAT-BUF window.
The new window is soft-dedicated so `display-buffer' never targets it.
Return the new input window, or nil when no chat window can be split."
  (when-let* ((chat-win
               (or (and (eq (window-buffer (selected-window)) chat-buf)
                        (piem--window-can-split-for-input-p
                         (selected-window))
                        (selected-window))
                   (cl-find-if #'piem--window-can-split-for-input-p
                               (piem--windows-by-height
                                (get-buffer-window-list chat-buf nil))))))
    (let ((input-win
           (split-window
            chat-win (- (piem--input-height-for-window chat-win))
            'below)))
      (set-window-buffer input-win input-buf)
      ;; Soft-dedicate the input window so `display-buffer' never
      ;; targets it (magit, help, compilation, etc.).  The 'side
      ;; value still allows `switch-to-buffer' and `C-x o'.
      (set-window-dedicated-p input-win 'side)
      input-win)))

(defun piem-open-input ()
  "Open the input window below the chat window and select it.
If an input window is already visible, select it instead.  If no chat
window is visible either, restore the full session layout."
  (interactive)
  (let* ((chat-buf (piem--get-chat-buffer))
         (input-buf (piem--get-input-buffer)))
    (unless (and (buffer-live-p chat-buf) (buffer-live-p input-buf))
      (user-error "No pi session for this buffer"))
    (cond
     ((piem--focus-input-window chat-buf input-buf))
     ((when-let* ((input-win
                   (piem--split-input-below-chat chat-buf input-buf)))
        (select-window input-win)))
     (t (piem--display-buffers chat-buf input-buf)))))

(defun piem--input-window-on-demand-p ()
  "Return non-nil when the input window is shown on demand.
True for the `on-demand' and `hidden' values of
`piem-input-window-display', which both hide the input
after each send and reopen it with `piem-open-input'."
  (memq piem-input-window-display '(on-demand hidden)))

(defun piem--maybe-hide-input-window ()
  "Hide the input window when it is shown on demand.
Intended to run after `piem-send' accepts input.  When the
selected window is deleted, select a chat window instead."
  (when (piem--input-window-on-demand-p)
    (let* ((input-buf (piem--get-input-buffer))
           (input-wins (and (buffer-live-p input-buf)
                            (get-buffer-window-list input-buf nil)))
           (selected (selected-window)))
      (dolist (win input-wins)
        (when (window-parent win)
          (delete-window win)))
      (when (and (memq selected input-wins)
                 (not (window-live-p selected)))
        (when-let* ((chat-buf (piem--get-chat-buffer))
                    (chat-win (get-buffer-window chat-buf)))
          (select-window chat-win))))))

(defun piem--display-buffers (chat-buf input-buf &optional chat-only)
  "Ensure CHAT-BUF and INPUT-BUF are visible.
When CHAT-ONLY is non-nil, show only the chat window.
Uses a split window with chat above and input below.  Falls back to a
larger window when the selected one cannot be split."
  (let* ((chat-wins (get-buffer-window-list chat-buf nil))
         (input-wins (get-buffer-window-list input-buf nil))
         (selected (selected-window))
         (preferred (piem--preferred-display-window
                     chat-wins input-wins selected))
         (target (piem--best-display-window preferred))
         (input-win nil))
    ;; Remove stale input windows when restoring from an input-only view.
    (when (and input-wins (not chat-wins))
      (piem--delete-extra-input-windows input-wins target))
    (with-selected-window target
      (unless chat-only
        (unless (piem--window-can-split-for-input-p target)
          (delete-other-windows target))
        (unless (piem--window-can-split-for-input-p target)
          (user-error "Window too small for chat + input layout")))
      (switch-to-buffer chat-buf)
      (with-current-buffer chat-buf
        (goto-char (point-max)))
      (unless chat-only
        (setq input-win
              (piem--split-input-below-chat chat-buf input-buf))))
    (when (window-live-p input-win)
      (select-window input-win))))

;;; Scroll Behavior
;;
;; During streaming, windows "following" output (window-point at buffer end)
;; scroll to show new content. Windows where the user scrolled up stay put.
;;
;; Key mechanism: `window-point-insertion-type' is set to t in piem-chat-mode,
;; making window-point move with inserted text. We track which windows are
;; following before each insert, then restore point for non-following windows
;; afterward. Emacs naturally scrolls to keep point visible.

(defun piem--window-following-p (window)
  "Return non-nil if WINDOW is following output (point at end of buffer)."
  (>= (window-point window) (1- (point-max))))

(defmacro piem--with-scroll-preservation (&rest body)
  "Execute BODY preserving scroll for windows not following output.
Windows at buffer end will scroll to show new content.
Windows where user scrolled up stay in place.
Valid for append-only inserts; for mid-buffer rewrites like tool cooling, see
`piem--capture-tool-cooling-view' in piem-render.el."
  (declare (indent 0) (debug t))
  `(let* ((windows (get-buffer-window-list (current-buffer) nil t))
          (following (cl-remove-if-not #'piem--window-following-p windows))
          (saved-points (mapcar (lambda (w) (cons w (window-point w)))
                                (cl-remove-if #'piem--window-following-p windows))))
     ,@body
     ;; Restore point for non-following windows
     (dolist (pair saved-points)
       (when (window-live-p (car pair))
         (set-window-point (car pair) (cdr pair))))
     ;; Move following windows to new end
     (dolist (win following)
       (when (window-live-p win)
         (set-window-point win (point-max))))))

(defun piem--append-to-chat (text)
  "Append TEXT to the chat buffer.
Windows following the output (point at end) will scroll to show new text.
Windows where user scrolled up (point earlier) stay in place."
  (let ((inhibit-read-only t))
    (piem--with-scroll-preservation
      (save-excursion
        (goto-char (point-max))
        (insert text)))))

(defun piem--make-separator (label &optional timestamp)
  "Create a setext-style H1 heading separator with LABEL.
If TIMESTAMP (Emacs time value) is provided, append it after \" · \".
Returns a markdown setext heading: label line followed by === underline.
Fontification is handled by `md-ts-mode'.

Using setext headings enables outline/imenu navigation and keeps our
turn markers as H1 while LLM ATX headings are leveled down to H2+."
  (let* ((timestamp-str (when timestamp
                          (piem--format-message-timestamp timestamp)))
         (header-line (if timestamp-str
                          (concat label " · " timestamp-str)
                        label))
         ;; Underline must be at least 3 chars, and at least as long as header
         (underline-len (max 3 (length header-line)))
         (underline (make-string underline-len ?=)))
    (concat header-line "\n" underline "\n")))

;;;; Formatting Utilities

(defun piem--format-number (n)
  "Format number N with thousands separators."
  (let ((str (number-to-string n)))
    (replace-regexp-in-string
     "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\([^0-9]\\|$\\)"
     "\\1,\\2\\3"
     (replace-regexp-in-string
      "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\([0-9]\\{3\\}\\)\\([^0-9]\\|$\\)"
      "\\1,\\2,\\3\\4" str))))

(defun piem--truncate-string (str max-len)
  "Truncate STR to MAX-LEN chars, adding ellipsis if needed."
  (if (and str (> (length str) max-len))
      (concat (substring str 0 (- max-len 1)) "…")
    str))

(defun piem--ms-to-time (ms)
  "Convert milliseconds MS to Emacs time value.
Returns nil if MS is nil."
  (and ms (seconds-to-time (/ ms 1000.0))))

(defun piem--format-message-timestamp (time)
  "Format TIME for message headers as YYYY-MM-DD HH:MM."
  (format-time-string "%Y-%m-%d %H:%M" time))

;;;; Dependency Checking

(defconst piem--pi-package "@earendil-works/pi-coding-agent"
  "Npm package name for the pi CLI supported by piem.")

(defconst piem--minimum-pi-version "0.84.2"
  "Minimum supported pi CLI version.")

(defun piem--pi-install-command ()
  "Return the npm command to install the supported pi CLI."
  (format "npm install -g %s" piem--pi-package))

(defun piem--dependency-directory (&optional directory)
  "Return the directory where process dependencies should be checked.
Use DIRECTORY when non-nil.  In pi buffers, prefer the active session
directory; otherwise use `default-directory'."
  (or directory
      (if (derived-mode-p 'piem-chat-mode
                          'piem-input-mode)
          (piem--session-directory)
        default-directory)))

(defun piem--multi-hop-remote-prefix-p (prefix)
  "Return non-nil when PREFIX is a TRAMP multi-hop route."
  (and (stringp prefix)
       (string-search "|" prefix)))

(defun piem--remote-exec-path-directory
    (entry directory remote-prefix)
  "Return ENTRY from the function `exec-path' under remote DIRECTORY.
REMOTE-PREFIX is DIRECTORY's full TRAMP prefix.  Nil and empty entries mean the
remote DIRECTORY itself.  Process-local absolute entries are re-prefixed with
REMOTE-PREFIX so multi-hop routes are not collapsed by generic file helpers."
  (cond
   ((or (null entry) (equal entry ""))
    (piem--route-preserving-file-name-as-directory directory))
   ((not (stringp entry))
    nil)
   ((piem--remote-prefix-for-path entry)
    (when (equal (piem--remote-prefix-for-path entry)
                 remote-prefix)
      (piem--route-preserving-file-name-as-directory entry)))
   ((file-name-absolute-p entry)
    (piem--route-preserving-file-name-as-directory
     (concat remote-prefix entry)))
   (t
    (piem--route-preserving-file-name-as-directory
     (piem--route-preserving-expand-file-name entry directory)))))

(defun piem--remote-executable-path
    (program directory remote-prefix)
  "Return PROGRAM as an Emacs path in remote DIRECTORY.
REMOTE-PREFIX is DIRECTORY's full TRAMP prefix."
  (cond
   ((piem--remote-prefix-for-path program)
    (and (equal (piem--remote-prefix-for-path program)
                remote-prefix)
         program))
   ((file-name-absolute-p program)
    (concat remote-prefix program))
   (t
    (piem--route-preserving-expand-file-name program directory))))

(defun piem--remote-executable-file-p (path)
  "Return non-nil when remote PATH names an executable file.
This intentionally uses ordinary file predicates so TRAMP performs real I/O in
normal operation; tests should stub this predicate or `file-executable-p' for
fake hosts."
  (ignore-errors (file-executable-p path)))

(defun piem--remote-executable-find (program directory)
  "Find PROGRAM on remote DIRECTORY while preserving its full TRAMP route.
This is a focused replacement for `executable-find' on multi-hop remotes,
where generic file-name operations can collapse `/ssh:bastion|sudo:host:' to
`/sudo:host:'.  It binds `default-directory' to DIRECTORY, asks the function
`exec-path' for the process-local remote PATH, and returns the first executable
candidate re-prefixed with DIRECTORY's full TRAMP route."
  (let* ((remote-prefix (piem--remote-prefix directory))
         (directory (piem--route-preserving-file-name-as-directory
                     (piem--route-preserving-expand-file-name
                      directory)))
         (path-entries (let ((default-directory directory))
                         (exec-path)))
         (suffixes (or exec-suffixes '(""))))
    (when (and (stringp program)
               (not (string-empty-p program))
               remote-prefix)
      (catch 'found
        (if (string-search "/" program)
            (dolist (suffix suffixes)
              (when-let* ((candidate
                           (piem--remote-executable-path
                            (concat program suffix)
                            directory remote-prefix))
                          ((piem--remote-executable-file-p
                            candidate)))
                (throw 'found candidate)))
          (dolist (entry path-entries)
            (when-let* ((dir (piem--remote-exec-path-directory
                              entry directory remote-prefix)))
              (dolist (suffix suffixes)
                (let ((candidate
                       (piem--route-preserving-expand-file-name
                        (concat program suffix) dir)))
                  (when (piem--remote-executable-file-p candidate)
                    (throw 'found candidate)))))))))))

(defun piem--check-pi (&optional directory)
  "Check if pi binary is available in DIRECTORY's execution context.
Bind `default-directory' to DIRECTORY and use that execution context.  For
multi-hop remote directories, ask the function `exec-path' for remote PATH
entries and re-prefix candidates; otherwise delegate to `executable-find'.
Returns t if available, nil otherwise."
  (let* ((directory (piem--dependency-directory directory))
         (default-directory directory)
         (program (car piem-executable))
         (remote-prefix (piem--remote-prefix directory)))
    (and program
         (if (piem--multi-hop-remote-prefix-p remote-prefix)
             (piem--remote-executable-find program directory)
           (executable-find program t))
         t)))

(defun piem--check-dependencies (&optional directory)
  "Check all required dependencies.
When DIRECTORY is non-nil, perform process dependency checks there.  Displays
warnings for missing dependencies."
  (let ((directory (piem--dependency-directory directory)))
    (unless (piem--check-pi directory)
      (display-warning 'pi (format "%s not found in %s. Install with: %s"
                                   (car piem-executable)
                                   (if-let* ((remote-prefix (piem--remote-prefix directory)))
                                       (format "remote PATH (%s)" remote-prefix)
                                     "PATH")
                                   (piem--pi-install-command))
                       :error)))
  (piem--maybe-install-essential-grammars)
  (piem--maybe-warn-incompatible-markdown-grammar)
  (piem--maybe-install-optional-grammars))

;;;; Startup Header

(defconst piem-version "3.0.0"
  "Version of piem.")

(defconst piem--version-probe-delay 0.1
  "Seconds to wait before probing `pi --version' for a new process.")

(defun piem--extract-pi-version (output)
  "Extract a standalone semantic pi version from OUTPUT, or nil."
  (when (stringp output)
    (catch 'version
      (dolist (line (split-string output "[\r\n]+" t))
        (let ((trimmed (string-trim line)))
          (when (string-match
                 "\\`v?\\([0-9]+\\.[0-9]+\\.[0-9]+\\)\\'"
                 trimmed)
            (throw 'version (match-string 1 trimmed))))))))

(defun piem--pi-version-outdated-p (version)
  "Return non-nil when VERSION is older than supported pi."
  (and (stringp version)
       (condition-case nil
           (version< version piem--minimum-pi-version)
         (error nil))))

(defun piem--warn-if-pi-version-outdated (version)
  "Warn when VERSION is older than `piem--minimum-pi-version'."
  (when (piem--pi-version-outdated-p version)
    (display-warning
     'pi
     (format "Pi CLI version %s is older than the supported minimum %s. Upgrade with: %s"
             version
             piem--minimum-pi-version
             (piem--pi-install-command))
     :warning)))

(defun piem--finish-pi-version-process (proc)
  "Collect `pi --version' output from PROC and invoke its callback."
  (let ((callback (process-get proc 'piem-version-callback))
        (stdout-buf (process-get proc 'piem-version-stdout-buf))
        (stderr-buf (process-get proc 'piem-version-stderr-buf)))
    (unwind-protect
        (let* ((stdout (when (buffer-live-p stdout-buf)
                         (with-current-buffer stdout-buf
                           (buffer-string))))
               (stderr (when (buffer-live-p stderr-buf)
                         (with-current-buffer stderr-buf
                           (buffer-string))))
               (output (concat (or stdout "") "\n" (or stderr ""))))
          (when callback
            (funcall callback (piem--extract-pi-version output))))
      (when (buffer-live-p stdout-buf)
        (kill-buffer stdout-buf))
      (when (buffer-live-p stderr-buf)
        (kill-buffer stderr-buf)))))

(defun piem--run-pi-version-once-async (callback &optional directory)
  "Run `pi --version' asynchronously and call CALLBACK with version or nil.
Run in DIRECTORY, defaulting to `default-directory'.  Process creation uses
`default-directory' file handlers when present, with separate stdout and stderr
buffers."
  (let ((stdout-buf (generate-new-buffer " *piem-version-stdout*"))
        (stderr-buf (generate-new-buffer " *piem-version-stderr*"))
        (directory (or directory default-directory)))
    (condition-case nil
        (let* ((default-directory directory)
               (proc (make-process
                      :name "pi-version"
                      :command `(,@piem-executable "--version")
                      :connection-type 'pipe
                      :file-handler t
                      :buffer stdout-buf
                      :stderr stderr-buf
                      :noquery t
                      :sentinel
                      (lambda (proc _event)
                        (when (memq (process-status proc) '(exit signal))
                          (piem--finish-pi-version-process proc))))))
          (process-put proc 'piem-version-callback callback)
          (process-put proc 'piem-version-stdout-buf stdout-buf)
          (process-put proc 'piem-version-stderr-buf stderr-buf)
          proc)
      (error
       (when (buffer-live-p stdout-buf)
         (kill-buffer stdout-buf))
       (when (buffer-live-p stderr-buf)
         (kill-buffer stderr-buf))
       (funcall callback nil)))))

(defun piem--request-pi-version-async (callback)
  "Resolve pi CLI version asynchronously and call CALLBACK with string or nil."
  (let ((directory default-directory))
    (run-at-time piem--version-probe-delay nil
                 #'piem--run-pi-version-once-async
                 callback directory)))

(defun piem--probe-process-version-async (chat-buf)
  "Probe and cache CLI version for CHAT-BUF's process.
Stores the result in CHAT-BUF and emits a minibuffer notice when available."
  (when (buffer-live-p chat-buf)
    (with-current-buffer chat-buf
      (let ((default-directory (piem--chat-session-directory chat-buf)))
        (piem--request-pi-version-async
         (lambda (version)
           (when (and version (buffer-live-p chat-buf))
             (with-current-buffer chat-buf
               (setq piem--process-version version)
               (message "Pi: version %s" version)
               (piem--warn-if-pi-version-outdated version)))))))))

(defun piem--format-startup-header ()
  "Format the startup header string with styled separator."
  (let ((separator (piem--make-separator "Pi Coding Agent for Emacs")))
    (concat
     separator "\n"
     "C-c C-c   send prompt\n"
     "C-c C-a   attach image (C-u clears)\n"
     "C-c C-k   abort\n"
     "C-c C-r   sessions\n"
     "C-c C-p   menu\n")))

(defun piem--display-startup-header ()
  "Display the startup header in the chat buffer."
  (piem--append-to-chat (piem--format-startup-header)))

;;;; Header Line

(defun piem--format-tokens-compact (n)
  "Format token count N compactly (e.g., 50k, 1.2M)."
  (cond
   ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
   ((>= n 1000) (format "%.0fk" (/ n 1000.0)))
   (t (number-to-string n))))

(defun piem--shorten-model-name (name)
  "Shorten model NAME for display.
Removes common prefixes like \"Claude \" and suffixes like \" (latest)\"."
  (thread-last name
    (replace-regexp-in-string "^[Cc]laude " "")
    (replace-regexp-in-string " (latest)$" "")
    (replace-regexp-in-string "^claude-" "")))

;;; Header-Line Formatting

(defvar piem--header-model-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'piem-select-model)
    (define-key map [header-line mouse-2] #'piem-select-model)
    map)
  "Keymap for clicking model name in header-line.")

(defvar piem--header-thinking-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'piem-cycle-thinking)
    (define-key map [header-line mouse-2] #'piem-cycle-thinking)
    map)
  "Keymap for clicking thinking level in header-line.")

(defun piem--header-format-context (percent context-window)
  "Format context usage for header-line display.
PERCENT is context usage (0–100), CONTEXT-WINDOW is the max tokens.
When PERCENT is nil, usage is unknown and rendered as \"?\".
Returns nil if CONTEXT-WINDOW is 0."
  (when (> context-window 0)
    (if (null percent)
        (format " ?/%s" (piem--format-tokens-compact context-window))
      (let ((pct-str (piem--header-escape-text
                      (format " %.1f%%/%s" percent
                              (piem--format-tokens-compact context-window)))))
        (propertize pct-str
                    'face (cond
                           ((> percent piem-context-error-threshold) 'error)
                           ((> percent piem-context-warning-threshold) 'warning)
                           (t nil)))))))

(defun piem--header-format-stats (stats)
  "Format compact header stats from STATS.
Shows cumulative session cost and server-provided context percentage.
Returns nil if STATS is nil."
  (when stats
    (let* ((cost (or (plist-get stats :cost) 0))
           (ctx (plist-get stats :contextUsage))
           (raw-tokens (and ctx (plist-get ctx :tokens)))
           (percent (if (or (null raw-tokens)
                            (piem--json-null-p raw-tokens))
                        nil
                      (plist-get ctx :percent)))
           (context-window (or (and ctx (plist-get ctx :contextWindow)) 0)))
      (concat
       " │"
       (format " $%.2f" cost)
       (piem--header-format-context percent context-window)))))

(defun piem--header-escape-text (text)
  "Escape TEXT for use in `header-line-format'."
  (replace-regexp-in-string "%" "%%" text t t))

(defun piem--header-format-extension-status (ext-status)
  "Format EXT-STATUS alist for header-line display.
Returns extension statuses joined with \" · \", or empty string."
  (if (null ext-status)
      ""
    (mapconcat (lambda (pair)
                 (let* ((key (car pair))
                        (text (piem--header-escape-text (cdr pair)))
                        (face (cdr (assoc key piem-extension-status-faces)))
                        (properties (and (stringp key)
                                         (list 'help-echo key
                                               'mouse-face 'highlight))))
                   (when face
                     (setq properties (append properties (list 'face face))))
                   (if properties
                       (apply #'propertize text properties)
                     text)))
               ext-status
               " · ")))

(defun piem--header-format-identity (model-short thinking activity-phase-str)
  "Format identity group from MODEL-SHORT, THINKING, and ACTIVITY-PHASE-STR."
  (concat
   (propertize model-short
               'face 'piem-model-name
               'mouse-face 'highlight
               'help-echo "mouse-1: Select model"
               'local-map piem--header-model-map)
   (if (string-empty-p thinking)
       ""
     (concat " • "
             (propertize thinking
                         'mouse-face 'highlight
                         'help-echo "mouse-1: Cycle thinking level"
                         'local-map piem--header-thinking-map)))
   " " activity-phase-str))

(defun piem--header-format-context-group (session-name)
  "Format context group from SESSION-NAME.
Returns a leading-pipe group string or empty string
when no session name exists."
  (if (and session-name (not (string-empty-p session-name)))
      (concat " │ " (piem--truncate-string session-name 30))
    ""))

(defun piem--header-format-extension-group (ext-status working-message)
  "Format extension group from EXT-STATUS and WORKING-MESSAGE.
Returns a leading-pipe group string or empty string
when no extension info exists."
  (let* ((status-str (piem--header-format-extension-status ext-status))
         (working-str (if (and working-message (not (string-empty-p working-message)))
                          (propertize (piem--header-escape-text working-message)
                                      'face 'shadow)
                        ""))
         (parts nil))
    (unless (string-empty-p status-str)
      (push status-str parts))
    (unless (string-empty-p working-str)
      (push working-str parts))
    (if parts
        (concat " │ " (mapconcat #'identity (nreverse parts) " · "))
      "")))

(defun piem--header-format-prompt-image (image)
  "Format a leading-pipe header group for prompt IMAGE."
  (if (not image)
      ""
    (let ((name (piem--header-escape-text
                 (piem--prompt-image-name image)))
          (size (file-size-human-readable
                 (piem--prompt-image-byte-size image)
                 'iec " " "B")))
      (format " │ image: %s (%s)" name size))))

(defun piem--header-line-string ()
  "Return formatted header-line string for input buffer.
Accesses state from the linked chat buffer."
  (let* ((chat-buf (cond
                    ;; In input buffer with valid link to chat
                    ((and piem--chat-buffer (buffer-live-p piem--chat-buffer))
                     piem--chat-buffer)
                    ;; In chat buffer itself
                    ((derived-mode-p 'piem-chat-mode)
                     (current-buffer))
                    ;; No valid chat buffer yet
                    (t nil)))
         (state (and chat-buf (buffer-local-value 'piem--state chat-buf)))
         (stats (and chat-buf (buffer-local-value 'piem--cached-stats chat-buf)))
         (ext-status (and chat-buf (buffer-local-value 'piem--extension-status chat-buf)))
         (working-message (and chat-buf (buffer-local-value 'piem--working-message chat-buf)))
         (session-name (and chat-buf (buffer-local-value 'piem--session-name chat-buf)))
         (model-obj (plist-get state :model))
         (model-name (cond
                      ((stringp model-obj) model-obj)
                      ((plist-get model-obj :name))
                      (t "")))
         (model-short (if (string-empty-p model-name) "..."
                        (piem--shorten-model-name model-name)))
         (thinking (or (plist-get state :thinking-level) ""))
         (activity-phase (or (and chat-buf
                                  (buffer-local-value 'piem--activity-phase chat-buf))
                             "idle"))
         (activity-phase-str
          (propertize (format "%-8s" activity-phase)
                      'face 'piem-activity-phase)))
    (concat
     (piem--header-format-identity model-short thinking activity-phase-str)
     (piem--header-format-stats stats)
     (piem--header-format-context-group session-name)
     (piem--header-format-extension-group ext-status working-message)
     (piem--header-format-prompt-image
      (piem--get-prompt-image)))))

;;; State Management

(defun piem--refresh-header ()
  "Refresh header-line by fetching and caching session stats."
  (when-let* ((proc (piem--get-process))
             (chat-buf (piem--get-chat-buffer)))
    (let ((input-buf (buffer-local-value 'piem--input-buffer chat-buf)))
      (piem--rpc-async proc '(:type "get_session_stats")
                     (lambda (response)
                       (when (eq (plist-get response :success) t)
                         (when (buffer-live-p chat-buf)
                           (with-current-buffer chat-buf
                             (setq piem--cached-stats (plist-get response :data))))
                         ;; Update the input buffer's header line
                         (when (buffer-live-p input-buf)
                           (dolist (win (get-buffer-window-list input-buf nil t))
                             (with-selected-window win
                               (force-mode-line-update))))))))))

(defun piem--merge-state-response-status (remote-status)
  "Return status after merging REMOTE-STATUS with local pending work."
  (if (and (eq remote-status 'idle)
           (piem--prompt-start-wait-active-p)
           (memq piem--status '(sending streaming compacting)))
      piem--status
    remote-status))

(defun piem--apply-state-response (chat-buf response)
  "Apply get_state RESPONSE to CHAT-BUF.
Updates buffer-local state variables and refreshes mode-line.
Safely handles dead buffers by checking liveness first."
  (when (and (eq (plist-get response :success) t)
             (buffer-live-p chat-buf))
    (with-current-buffer chat-buf
      (let* ((old-session-id (plist-get piem--state :session-id))
             (new-state (piem--extract-state-from-response
                         response
                         (piem--chat-session-directory chat-buf)))
             (new-session-id (plist-get new-state :session-id)))
        (when (and old-session-id
                   new-session-id
                   (not (equal old-session-id new-session-id)))
          (piem--clear-unsupported-extension-ui-warnings))
        (let ((new-status
               (piem--merge-state-response-status
                (plist-get new-state :status))))
          (plist-put new-state :status new-status)
          (setq piem--status new-status
                piem--state new-state)))
      (force-mode-line-update t))))

;;;; Sending Infrastructure

(defconst piem--prompt-start-timeout 0.5
  "Seconds to wait for agent_start after a successful prompt response.
Some extension commands can complete without a visible agent turn; this timeout
returns the frontend to idle for that no-turn success path.")

(defvar-local piem--prompt-start-timer nil
  "Timer waiting for agent_start after prompt preflight success.")

(defvar-local piem--prompt-start-generation 0
  "Generation used to match prompt-start fallback timers to their prompt.")

(defun piem--cancel-prompt-start-timer ()
  "Cancel any pending prompt-start fallback timer."
  (when (timerp piem--prompt-start-timer)
    (cancel-timer piem--prompt-start-timer))
  (setq piem--prompt-start-timer nil))

(defun piem--invalidate-prompt-start-wait ()
  "Cancel and invalidate any pending wait for agent_start."
  (piem--cancel-prompt-start-timer)
  (setq piem--prompt-start-wait-active nil)
  (setq piem--prompt-start-generation
        (1+ piem--prompt-start-generation)))

(defun piem--begin-prompt-start-wait ()
  "Mark the current prompt as waiting for agent_start and return its token."
  (piem--invalidate-prompt-start-wait)
  (setq piem--prompt-start-wait-active t)
  piem--prompt-start-generation)

(defun piem--prompt-start-current-p (generation)
  "Return non-nil when GENERATION is still the active prompt-start wait."
  (and generation
       (piem--prompt-start-wait-active-p)
       (= generation piem--prompt-start-generation)))

(defun piem--finish-prompt-without-agent-start
    (chat-buf generation on-no-agent-start)
  "Finish CHAT-BUF prompt GENERATION after Pi confirms no agent turn.
Call ON-NO-AGENT-START after releasing local ownership."
  (when (and (buffer-live-p chat-buf)
             (with-current-buffer chat-buf
               (piem--prompt-start-current-p generation)))
    (with-current-buffer chat-buf
      (setq piem--prompt-start-wait-active nil)
      (setq piem--prompt-start-generation
            (1+ piem--prompt-start-generation))
      (when (eq piem--status 'sending)
        (setq piem--status 'idle)
        (piem--set-activity-phase "idle"))
      (when on-no-agent-start
        (funcall on-no-agent-start)))))

(defun piem--probe-prompt-start-state
    (chat-buf generation on-no-agent-start)
  "Ask Pi whether CHAT-BUF prompt GENERATION started an agent turn.
Call ON-NO-AGENT-START only after Pi authoritatively reports idle."
  (let ((proc (and (buffer-live-p chat-buf)
                   (with-current-buffer chat-buf
                     piem--process))))
    (when (and proc (process-live-p proc))
      (condition-case nil
          (piem--rpc-async
           proc '(:type "get_state")
           (lambda (response)
             (when (and (buffer-live-p chat-buf)
                        (with-current-buffer chat-buf
                          (piem--prompt-start-current-p generation)))
               (let* ((data (plist-get response :data))
                      (active
                       (and (eq (plist-get response :success) t)
                            (or (piem--normalize-boolean
                                 (plist-get data :isStreaming))
                                (piem--normalize-boolean
                                 (plist-get data :isCompacting))))))
                 (if (or active (not (eq (plist-get response :success) t)))
                     (piem--schedule-prompt-start-fallback
                      chat-buf generation on-no-agent-start)
                   (piem--finish-prompt-without-agent-start
                    chat-buf generation on-no-agent-start))))))
        (error
         (piem--schedule-prompt-start-fallback
          chat-buf generation on-no-agent-start))))))

(defun piem--clear-sending-if-no-agent-start
    (chat-buf generation &optional on-no-agent-start)
  "Check whether CHAT-BUF prompt GENERATION produced no agent_start.
Elapsed time alone is not authoritative: query Pi before releasing local prompt
ownership or invoking ON-NO-AGENT-START."
  (when (buffer-live-p chat-buf)
    (with-current-buffer chat-buf
      (when (piem--prompt-start-current-p generation)
        (setq piem--prompt-start-timer nil)
        (if (memq piem--status '(streaming compacting))
            (piem--schedule-prompt-start-fallback
             chat-buf generation on-no-agent-start)
          (piem--probe-prompt-start-state
           chat-buf generation on-no-agent-start))))))

(defun piem--schedule-prompt-start-fallback
    (chat-buf generation &optional on-no-agent-start)
  "Schedule idle fallback for CHAT-BUF after success with no agent_start.
GENERATION ties the fallback to the prompt response that scheduled it.
ON-NO-AGENT-START is called if the fallback actually fires."
  (when (buffer-live-p chat-buf)
    (with-current-buffer chat-buf
      (when (piem--prompt-start-current-p generation)
        (piem--cancel-prompt-start-timer)
        (setq piem--prompt-start-timer
              (run-at-time piem--prompt-start-timeout nil
                           #'piem--clear-sending-if-no-agent-start
                           chat-buf generation on-no-agent-start))))))

(defun piem--handle-prompt-send-failure
    (chat-buf generation on-failure &optional error-text)
  "Finish the current failed prompt send owned by GENERATION.
Restore user input through ON-FAILURE, reset CHAT-BUF, and report ERROR-TEXT.
Return non-nil only when GENERATION still owned the prompt-start wait."
  (let ((current-failure
         (and (buffer-live-p chat-buf)
              (with-current-buffer chat-buf
                (piem--prompt-start-current-p generation)))))
    (when current-failure
      (piem--abort-send chat-buf on-failure)
      (message "Pi: Send failed%s"
               (if error-text (format ": %s" error-text) "")))
    current-failure))

(defun piem--send-prompt
    (text &optional on-success on-failure on-no-agent-start prompt-image)
  "Send TEXT and optional PROMPT-IMAGE to the pi process.
Slash commands are sent literally - pi handles expansion.
Shows an error message if process is unavailable.
ON-SUCCESS is called in the chat buffer after prompt preflight accepts TEXT.
ON-FAILURE is called in the chat buffer if preflight rejects TEXT or scheduling
fails synchronously.  ON-NO-AGENT-START is called if success is not followed
by agent_start."
  (let ((proc (piem--get-process))
        (chat-buf (piem--get-chat-buffer))
        (prompt-generation nil))
    (cond
     ((null proc)
      (piem--abort-send chat-buf on-failure)
      (message "Pi: No process available - try M-x piem-reload or C-c C-p R"))
     ((not (process-live-p proc))
      (piem--abort-send chat-buf on-failure)
      (message "Pi: Process died - try M-x piem-reload or C-c C-p R"))
     (t
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (setq prompt-generation (piem--begin-prompt-start-wait))
          (setq piem--status 'sending)
          (piem--set-activity-phase "thinking")))
      (condition-case err
          (piem--rpc-async
           proc
           (append (list :type "prompt" :message text)
                   (when prompt-image
                     (list :images
                           (vector
                            (piem--prompt-image-content-block
                             prompt-image)))))
           (lambda (response)
             (if (eq (plist-get response :success) t)
                 (when (buffer-live-p chat-buf)
                   (with-current-buffer chat-buf
                     (when (piem--prompt-start-current-p
                            prompt-generation)
                       (when on-success
                         (funcall on-success))
                       (piem--schedule-prompt-start-fallback
                        chat-buf prompt-generation on-no-agent-start))))
               (piem--handle-prompt-send-failure
                chat-buf prompt-generation on-failure
                (plist-get response :error)))))
        ((error quit)
         (if (eq (car err) 'quit)
             (unwind-protect
                 (piem--handle-prompt-send-failure
                  chat-buf prompt-generation on-failure
                  (error-message-string err))
               (signal (car err) (cdr err)))
           (piem--handle-prompt-send-failure
            chat-buf prompt-generation on-failure
            (error-message-string err)))))))))

(defun piem--abort-send (chat-buf &optional on-failure)
  "Clean up after a failed send attempt in CHAT-BUF.
Call ON-FAILURE once after invalidating the prompt wait, then reset activity,
local echo state, and status to idle even if restoration signals."
  (when (buffer-live-p chat-buf)
    (with-current-buffer chat-buf
      (piem--invalidate-prompt-start-wait)
      (unwind-protect
          (when on-failure
            (funcall on-failure))
        (setq piem--local-user-message nil)
        (piem--clear-local-user-message-region)
        (setq piem--pre-compaction-status nil)
        (setq piem--status 'idle)
        (piem--set-activity-phase "idle")))))


(provide 'piem-ui)
;;; piem-ui.el ends here
