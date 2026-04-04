;;; pi-coding-agent-table.el --- Display-only table decoration -*- lexical-binding: t; -*-

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

;; Display-only markdown pipe table rendering for pi-coding-agent chat buffers.
;;
;; Detects pipe tables via tree-sitter, wraps them to the window width via
;; `markdown-table-wrap', and renders the wrapped output as per-line display
;; overlays.  The raw buffer text stays canonical — overlays are disposable.
;;
;; Key entry points for the render module:
;; - `pi-coding-agent--decorate-tables-in-region' — stable path
;; - `pi-coding-agent--maybe-decorate-streaming-table' — streaming path
;; - `pi-coding-agent--maybe-refresh-hot-tail-tables' — resize path
;;
;; Depends on `pi-coding-agent-ui' for visible-text extraction and
;; scroll preservation, and on `markdown-table-wrap' for the wrapping engine.

;;; Code:

(require 'pi-coding-agent-ui)
(require 'cl-lib)
(require 'markdown-table-wrap)

;; Persistent fontification buffer for visible-string extraction.  Global
;; (not buffer-local) because Emacs is single-threaded: the erase-insert-
;; fontify sequence always completes within one event cycle, so concurrent
;; chat sessions cannot race.
(defvar pi-coding-agent--visible-string-buffer nil)

;;;; Buffer-Local State

(defvar-local pi-coding-agent--last-table-display-width nil
  "Last visible chat width used for display-only table wrapping.")

(defvar-local pi-coding-agent--table-decoration-pending nil
  "Non-nil when table decoration was skipped because the buffer was hidden.
Set by streaming and message-completion paths when the chat buffer has
no visible window.  Cleared by the hot-tail refresh hook when the
buffer becomes visible again.")

(defvar-local pi-coding-agent--visible-string-cache
  nil
  "Session-level cache for `pi-coding-agent--markdown-visible-string'.
Maps markdown cell text to its visible rendering.  The mapping is purely
functional (input text → stripped text), so entries never go stale.
Survives across tables and resize operations, eliminating redundant
fontification when the same cell content reappears at different widths.")

;;;; Visibility and Width

(defun pi-coding-agent--chat-buffer-hidden-p ()
  "Return non-nil when the current chat buffer has no visible window.
Returns nil in batch mode so unit tests that use windowless temp
buffers are not affected by the visibility guard."
  (and (not noninteractive)
       (null (get-buffer-window (current-buffer)))))

(defun pi-coding-agent--chat-window-width ()
  "Return usable character columns for the chat window, or nil if hidden.
Excludes columns reserved by fringes such as line-number display."
  (when-let* ((window (get-buffer-window (current-buffer) nil)))
    (window-max-chars-per-line window)))

(defun pi-coding-agent--chat-display-width ()
  "Return the usable character width of the chat window, or 80 if hidden."
  (or (pi-coding-agent--chat-window-width) 80))

;;;; Hot-Tail Resize Refresh

(defun pi-coding-agent--refresh-hot-tail-tables (width)
  "Refresh display-only tables in the hot tail using WIDTH."
  (when (markerp pi-coding-agent--hot-tail-start)
    (pi-coding-agent--decorate-tables-in-region
     (marker-position pi-coding-agent--hot-tail-start)
     (point-max)
     width)))

(defun pi-coding-agent--maybe-refresh-hot-tail-tables ()
  "Refresh hot-tail tables after a visible chat-width change.
Also catches up on deferred decoration when the buffer becomes
visible after streaming while hidden."
  (when-let* ((width (pi-coding-agent--chat-window-width)))
    (when (or pi-coding-agent--table-decoration-pending
              (not (equal width pi-coding-agent--last-table-display-width)))
      (setq pi-coding-agent--table-decoration-pending nil)
      (setq pi-coding-agent--last-table-display-width width)
      (let ((gc-cons-threshold (max gc-cons-threshold (* 8 1024 1024))))
        (pi-coding-agent--with-scroll-preservation
          (save-excursion
            (pi-coding-agent--refresh-hot-tail-tables width)))))))

;;;; Tree-Sitter Detection

;; Tell the Emacs 29 byte compiler about the Emacs 30 signature so
;; calls with the LANGUAGE argument don't trigger arity warnings.
;; Same pattern as md-ts-mode.
(declare-function treesit-parser-list "treesit.c"
  (&optional buffer language))

(defun pi-coding-agent--markdown-parser ()
  "Return the first markdown tree-sitter parser for the current buffer.
On Emacs 30+ this uses the LANGUAGE argument of `treesit-parser-list'.
On Emacs 29, which lacks that parameter, we filter manually."
  (if (>= emacs-major-version 30)
      (car (treesit-parser-list (current-buffer) 'markdown))
    (cl-find 'markdown (treesit-parser-list)
             :key #'treesit-parser-language)))

(defvar pi-coding-agent--treesit-table-query nil
  "Pre-compiled tree-sitter query for pipe_table nodes.")

(defvar pi-coding-agent--treesit-data-row-query nil
  "Pre-compiled tree-sitter query for pipe_table data rows.")

(defun pi-coding-agent--ensure-treesit-queries ()
  "Ensure tree-sitter table queries are compiled.
Compiles both queries once against the current buffer's markdown
language, avoiding the per-call sexp→compiled compilation overhead
that thrashes tree-sitter's single-entry query cache."
  (unless pi-coding-agent--treesit-table-query
    (let ((lang (treesit-parser-language
                 (pi-coding-agent--markdown-parser))))
      (setq pi-coding-agent--treesit-table-query
            (treesit-query-compile lang '((pipe_table) @table)))
      (setq pi-coding-agent--treesit-data-row-query
            (treesit-query-compile lang '((pipe_table (pipe_table_row) @row)))))))

(defun pi-coding-agent--treesit-table-regions (beg end)
  "Find pipe-table regions between BEG and END using tree-sitter.
Returns a list of (START . END) pairs for each `pipe_table' node
whose start position falls within the range."
  (let ((regions nil))
    (when-let* ((parser (pi-coding-agent--markdown-parser)))
      (pi-coding-agent--ensure-treesit-queries)
      (let ((captures (treesit-query-capture
                       (treesit-parser-root-node parser)
                       pi-coding-agent--treesit-table-query
                       beg end)))
        (dolist (cap captures)
          (when (eq (car cap) 'table)
            (let ((node (cdr cap)))
              (push (cons (treesit-node-start node)
                          (treesit-node-end node))
                    regions))))))
    (nreverse regions)))

(defun pi-coding-agent--remove-table-overlays (beg end)
  "Remove display-only table overlays in the region BEG..END."
  (dolist (ov (overlays-in beg end))
    (when (overlay-get ov 'pi-coding-agent-table-display)
      (delete-overlay ov))))

(defun pi-coding-agent--table-has-data-row-p (beg end)
  "Return non-nil if the pipe_table between BEG and END has data rows.
A table needs at least one `pipe_table_row' child to be worth
decorating (header + separator alone is not enough)."
  (when-let* ((parser (pi-coding-agent--markdown-parser)))
    (pi-coding-agent--ensure-treesit-queries)
    (let ((captures (treesit-query-capture
                     (treesit-parser-root-node parser)
                     pi-coding-agent--treesit-data-row-query
                     beg end)))
      (cl-some (lambda (cap) (eq (car cap) 'row)) captures))))

;;;; Streaming Decoration

(defun pi-coding-agent--maybe-decorate-streaming-table ()
  "Decorate the active streaming table if complete enough.
Called after a newline-containing delta or at `text_end'.  Only the
last table in the current message is touched; earlier tables retain
their existing overlays.  Skips decoration when the buffer has no
visible window; sets `pi-coding-agent--table-decoration-pending'
so the hot-tail refresh hook can catch up on visibility."
  (when (and pi-coding-agent--message-start-marker
             pi-coding-agent--streaming-marker)
    (if (pi-coding-agent--chat-buffer-hidden-p)
        (setq pi-coding-agent--table-decoration-pending t)
      (let* ((start (marker-position pi-coding-agent--message-start-marker))
             (end (marker-position pi-coding-agent--streaming-marker))
             (regions (pi-coding-agent--treesit-table-regions start end)))
        (when-let* ((last-region (car (last regions))))
          (let ((width (pi-coding-agent--chat-display-width)))
            (setq pi-coding-agent--last-table-display-width width)
            (when (pi-coding-agent--table-has-data-row-p
                   (car last-region) (cdr last-region))
              (pi-coding-agent--remove-table-overlays
               (car last-region) (cdr last-region))
              (pi-coding-agent--decorate-table
               (car last-region) (cdr last-region) width))))))))

;;;; Row Helpers

(defun pi-coding-agent--table-spacer-line-p (line)
  "Return non-nil if LINE is a table spacer row (all cells whitespace)."
  (and (string-prefix-p "|" line)
       (string-blank-p (replace-regexp-in-string "|" "" line))))

(defun pi-coding-agent--table-separator-line-p (line)
  "Return non-nil if LINE is a table separator row."
  (and (string-match-p "-" line)
       (string-match-p "\\`|[-: |]+|?\\'" line)))

(defun pi-coding-agent--split-table-line-prefix (line)
  "Split table LINE into its container prefix and bare pipe-table text.
Returns (PREFIX . BARE), where PREFIX is everything before the first `|'."
  (if-let* ((pipe-index (string-match-p "|" line)))
      (cons (substring line 0 pipe-index)
            (substring line pipe-index))
    (cons "" line)))

;;;; Visible-String Extraction

(defun pi-coding-agent--visible-string-buffer ()
  "Return the persistent fontification buffer, creating it if needed."
  (unless (buffer-live-p pi-coding-agent--visible-string-buffer)
    (setq pi-coding-agent--visible-string-buffer
          (generate-new-buffer " *pi-visible-string*"))
    (with-current-buffer pi-coding-agent--visible-string-buffer
      (md-ts-mode)
      (setq-local md-ts-hide-markup t)
      (md-ts--set-hide-markup t)))
  pi-coding-agent--visible-string-buffer)

(defconst pi-coding-agent--markdown-inline-chars-re "[*_`~\\[]"
  "Regexp matching characters that can start markdown inline syntax.
Cells without any of these characters have identical visible rendering
and can skip the fontification buffer entirely.")

(defun pi-coding-agent--markdown-visible-string (markdown)
  "Return the visible chat-buffer rendering of MARKDOWN.
This hides markdown delimiters the same way `pi-coding-agent-chat-mode'
does, so wrapped table overlays match ordinary visible-copy semantics.
Uses a persistent fontification buffer to avoid per-call mode setup.
Cells with no inline syntax characters are returned as-is."
  (if (not (string-match-p pi-coding-agent--markdown-inline-chars-re markdown))
      markdown
    (with-current-buffer (pi-coding-agent--visible-string-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert markdown))
      (font-lock-ensure)
      (pi-coding-agent--visible-text (point-min) (point-max)))))

;;;; Cell Rendering

(defun pi-coding-agent--render-table-row-lines (cells col-widths aligns)
  "Render table CELLS into display lines using COL-WIDTHS and ALIGNS.
When `pi-coding-agent-prettify-tables' is non-nil, emits Unicode
box-drawing verticals instead of markdown pipes."
  (let* ((num-cols (length col-widths))
         (padded (append cells (make-list (max 0 (- num-cols (length cells))) "")))
         (wrapped-cells
          (cl-mapcar (lambda (cell column-width)
                       (markdown-table-wrap-cell (or cell "") column-width))
                     padded col-widths))
         (max-height (apply #'max (mapcar #'length wrapped-cells)))
         (pretty pi-coding-agent-prettify-tables)
         (delim-open  (if pretty "│ " "| "))
         (delim-mid   (if pretty " │ " " | "))
         (delim-close (if pretty " │" " |")))
    (cl-loop for line-index below max-height
             collect
             (let ((acc (list delim-open)))
               (cl-loop for cell-lines in wrapped-cells
                        for column-width in col-widths
                        for align in aligns
                        for first = t then nil
                        do
                        (unless first (push delim-mid acc))
                        (let* ((cell (or (nth line-index cell-lines) ""))
                               (empty (string-empty-p cell))
                               (pad (if empty
                                        column-width
                                      (max 0 (- column-width
                                                 (markdown-table-wrap-visible-width cell))))))
                          (cond
                           (empty
                            (push (make-string column-width ?\s) acc))
                           ((eq align 'right)
                            (push (make-string pad ?\s) acc)
                            (push cell acc))
                           ((eq align 'center)
                            (let ((left-pad (/ pad 2)))
                              (push (make-string left-pad ?\s) acc)
                              (push cell acc)
                              (push (make-string (- pad left-pad) ?\s) acc)))
                           (t
                            (push cell acc)
                            (push (make-string pad ?\s) acc)))))
               (push delim-close acc)
               (apply #'concat (nreverse acc))))))

(defun pi-coding-agent--render-table-separator-line (col-widths aligns)
  "Render the separator line for COL-WIDTHS and ALIGNS.
When `pi-coding-agent-prettify-tables' is non-nil, emits a box-drawing
rule (├─┼─┤) directly; otherwise emits standard markdown syntax."
  (if pi-coding-agent-prettify-tables
      (concat "├─" (mapconcat (lambda (w) (make-string (max 1 w) ?─))
                              col-widths "─┼─")
              "─┤")
    (let ((parts
           (cl-mapcar
            (lambda (column-width align)
              (let ((dashes (make-string (max 1 column-width) ?-)))
                (pcase align
                  ('left
                   (if (>= column-width 2)
                       (concat ":" (substring dashes 1))
                     ":"))
                  ('right
                   (if (>= column-width 2)
                       (concat (substring dashes 1) ":")
                     ":"))
                  ('center
                   (if (>= column-width 3)
                       (concat ":" (substring dashes 2) ":")
                     (if (>= column-width 2) "::" ":")))
                  (_ dashes))))
            col-widths aligns)))
      (concat "| " (mapconcat #'identity parts " | ") " |"))))

(defun pi-coding-agent--table-alignments (separator-line)
  "Return column alignment symbols parsed from SEPARATOR-LINE."
  (mapcar (lambda (cell)
            (let ((trimmed (string-trim cell)))
              (cond
               ((and (string-prefix-p ":" trimmed)
                     (string-suffix-p ":" trimmed)) 'center)
               ((string-suffix-p ":" trimmed) 'right)
               ((string-prefix-p ":" trimmed) 'left)
               (t nil))))
          (markdown-table-wrap--split-table-row (string-trim separator-line))))

;;;; Display Groups

(defun pi-coding-agent--table-display-groups (raw-lines width)
  "Return prefix-aware display groups for RAW-LINES at WIDTH.
Each result element corresponds to one raw source line and contains the
wrapped display lines for that logical table row.  Container prefixes such as
blockquotes or indentation are preserved on every visual continuation line.
Plain tables (no prefix) take a fast path that skips prefix splitting."
  (let* ((no-prefix (cl-every (lambda (l) (and (> (length l) 0)
                                               (= (aref l 0) ?|)))
                              raw-lines))
         (parts (unless no-prefix
                  (mapcar #'pi-coding-agent--split-table-line-prefix raw-lines)))
         (prefixes (unless no-prefix (mapcar #'car parts)))
         (bare-lines (if no-prefix raw-lines (mapcar #'cdr parts)))
         (prefix-width (if no-prefix 0
                         (apply #'max 0 (mapcar #'string-width prefixes)))))
    (when (>= (length bare-lines) 2)
      (let* ((headers (markdown-table-wrap--split-table-row
                       (string-trim (car bare-lines))))
             (aligns (pi-coding-agent--table-alignments (cadr bare-lines)))
             (rows (mapcar (lambda (line)
                             (markdown-table-wrap--split-table-row
                              (string-trim line)))
                           (nthcdr 2 bare-lines)))
             (visible-cache
              (or pi-coding-agent--visible-string-cache
                  (setq pi-coding-agent--visible-string-cache
                        (make-hash-table :test 'equal :size 256))))
             (display-cell
              (lambda (cell)
                (or (gethash cell visible-cache)
                    (puthash cell
                             (pi-coding-agent--markdown-visible-string cell)
                             visible-cache))))
             (display-headers (mapcar display-cell headers))
             (display-rows
              (mapcar (lambda (row)
                        (mapcar display-cell row))
                      rows))
             (num-cols (max (length display-headers)
                            (length aligns)
                            (if display-rows
                                (apply #'max (mapcar #'length display-rows))
                              0)))
             (content-width (max 1 (- width prefix-width)))
             (col-widths
              (markdown-table-wrap-compute-widths
               display-headers display-rows content-width num-cols))
             (header-lines
              (pi-coding-agent--render-table-row-lines
               display-headers col-widths aligns))
             (separator-line
              (pi-coding-agent--render-table-separator-line
               col-widths aligns))
             (row-groups
              (mapcar (lambda (row)
                        (pi-coding-agent--render-table-row-lines
                         row col-widths aligns))
                      display-rows)))
        (if no-prefix
            (append (list header-lines)
                    (list (list separator-line))
                    row-groups)
          (append
           (list (mapcar (lambda (line) (concat (car prefixes) line))
                         header-lines))
           (list (list (concat (nth 1 prefixes) separator-line)))
           (cl-mapcar (lambda (prefix row-lines)
                        (mapcar (lambda (line) (concat prefix line))
                                row-lines))
                      (nthcdr 2 prefixes)
                      row-groups)))))))

;;;; Line Mapping

(defun pi-coding-agent--table-line-mapping (raw-lines wrapped-lines)
  "Map RAW-LINES to groups of WRAPPED-LINES for per-line display.
Returns a list of string lists.  Element i contains the wrapped
display lines for raw line i, or nil if no separator was found.

The mapping uses the separator row as an anchor:
  - Lines before the separator belong to the header (raw line 0).
  - The separator itself belongs to raw line 1.
  - Data lines after the separator are partitioned by spacer rows
    (all-whitespace-cell rows inserted by `markdown-table-wrap'
    between logical data rows when wrapping occurs).
This intentionally depends on `markdown-table-wrap''s current wrapped
output shape; the regression tests in `pi-coding-agent-table-test.el'
guard that contract until upstream offers a stable structured mapping API."
  (when-let* ((sep-idx (cl-position-if
                         #'pi-coding-agent--table-separator-line-p
                         wrapped-lines)))
    (let* ((header-group (seq-subseq wrapped-lines 0 sep-idx))
           (sep-line (nth sep-idx wrapped-lines))
           (data-wrapped (seq-subseq wrapped-lines (1+ sep-idx)))
           ;; Raw table: 1 header line + 1 separator line + N data lines
           (n-data (- (length raw-lines) 2))
           (data-groups
            (cond
             ((= n-data 0) nil)
             ((= n-data 1) (list data-wrapped))
             (t
              ;; Split by spacer rows; spacers themselves are excluded.
              (let ((groups nil) (current nil))
                (dolist (line data-wrapped)
                  (if (pi-coding-agent--table-spacer-line-p line)
                      (when current
                        (push (nreverse current) groups)
                        (setq current nil))
                    (push line current)))
                (when current
                  (push (nreverse current) groups))
                (let ((result (nreverse groups)))
                  (if (= (length result) n-data)
                      result
                    ;; No spacers found → 1:1 mapping (table fits in width)
                    (mapcar #'list data-wrapped))))))))
      (append (list header-group)
              (list (list sep-line))
              data-groups))))

;;;; Font Neutralization

(defconst pi-coding-agent--visual-face-attrs
  '(:foreground :background :weight :slant :underline
    :overline :strike-through :box :inverse-video)
  "Face attributes preserved in table display strings.
Font-identity attributes (:family, :foundry, :width, :height) are
excluded so table columns align regardless of the theme's font choices.")

(defun pi-coding-agent--face-visual-attr (face-val attr)
  "Return effective ATTR from FACE-VAL, following inheritance.
FACE-VAL may be a face name, an anonymous attribute plist, or a list
mixing both forms.  Returns `unspecified' when the attribute is unset."
  (cond
   ((null face-val) 'unspecified)
   ((symbolp face-val)
    (if (facep face-val)
        (face-attribute face-val attr nil t)
      'unspecified))
   ((and (consp face-val) (keywordp (car face-val)))
    (or (plist-get face-val attr) 'unspecified))
   ((listp face-val)
    (cl-loop for f in face-val
             for v = (pi-coding-agent--face-visual-attr f attr)
             unless (eq v 'unspecified) return v
             finally return 'unspecified))
   (t 'unspecified)))

(defun pi-coding-agent--visual-face-spec (face-val)
  "Resolve FACE-VAL to an anonymous plist with only visual attributes.
Returns nil when the face contributes no visual attributes."
  (let (spec)
    (dolist (attr pi-coding-agent--visual-face-attrs)
      (let ((val (pi-coding-agent--face-visual-attr face-val attr)))
        (unless (eq val 'unspecified)
          (setq spec (plist-put spec attr val)))))
    spec))

(defun pi-coding-agent--neutralize-fonts (str)
  "Return copy of STR with font-identity face attributes removed.
Walks each `face' span, resolves the effective visual attributes
\(foreground, weight, slant, etc.), and replaces the face with an
anonymous spec.  Font-identity attributes (:family, :foundry, :width,
:height) are omitted so the buffer's default font governs rendering.
This prevents column misalignment in GUI Emacs when inline markdown
faces like `md-ts-code' inherit from `fixed-pitch'."
  (let ((result (copy-sequence str))
        (pos 0)
        (len (length str)))
    (while (< pos len)
      (let* ((next (next-single-property-change pos 'face result len))
             (face-val (get-text-property pos 'face result)))
        (when face-val
          (let ((spec (pi-coding-agent--visual-face-spec face-val)))
            (if spec
                (put-text-property pos next 'face spec result)
              (remove-text-properties pos next '(face nil) result))))
        (setq pos next)))
    result))

;;;; Overlay Creation

(defun pi-coding-agent--decorate-table (beg end width)
  "Create per-line display overlays for the raw table between BEG and END.
WIDTH is the target display width.  The raw buffer text is preserved;
each raw line gets its own overlay whose `display' property shows the
corresponding wrapped output (which may span multiple visual lines
when columns wrap).

Container prefixes (for example `> ' in blockquotes or list indentation)
are preserved on every visual continuation line.  Trailing newlines from
the tree-sitter node are preserved in the last overlay's display string so
blank-line spacing between a table and following text is not collapsed."
  (let* ((table-beg (save-excursion (goto-char beg) (line-beginning-position)))
         (raw (buffer-substring-no-properties table-beg end))
         (trimmed (string-trim-right raw "\n+"))
         (trailing (substring raw (length trimmed))))
    (when-let* ((raw-lines (split-string trimmed "\n"))
                (groups (pi-coding-agent--table-display-groups raw-lines width)))
      (unless (and (= (length groups) (length raw-lines))
                   (cl-every (lambda (raw-line group)
                               (and (= (length group) 1)
                                    (equal raw-line (car group))))
                             raw-lines groups))
        (let ((n (length raw-lines)))
          (save-excursion
            (goto-char table-beg)
            (dotimes (i n)
              (let* ((line-beg (line-beginning-position))
                     (line-end (if (= i (1- n))
                                   end ; last line: cover trailing newlines
                                 (min (1+ (line-end-position)) end)))
                     (group (nth i groups))
                     (display-str (concat
                                   (mapconcat #'identity group "\n")
                                   (if (= i (1- n)) trailing "\n")))
                     (ov (make-overlay line-beg line-end nil nil nil)))
                (overlay-put ov 'display
                             (pi-coding-agent--neutralize-fonts display-str))
                (overlay-put ov 'face 'default)
                (overlay-put ov 'pi-coding-agent-table-display t)
                (overlay-put ov 'evaporate t))
              (forward-line 1))))
        t))))

(defun pi-coding-agent--decorate-tables-in-region (beg end &optional width)
  "Decorate complete pipe tables in BEG..END with display overlays.
WIDTH is the target display width; defaults to the value from
`pi-coding-agent--chat-display-width'.  Tables inside fenced code
blocks are skipped (tree-sitter excludes them automatically), and
header-only tables are left undecorated until they gain a data row.
Idempotent: existing table overlays in the region are removed first."
  (let ((width (or width (pi-coding-agent--chat-display-width)))
         ;; Defer GC during batch decoration to reduce pause frequency.
         ;; 8 MB threshold prevents inter-call GC during consecutive
         ;; decoration passes (e.g., resize sweeps).  Standard pattern
         ;; used by org-mode and magit for batch operations.
         (gc-cons-threshold (max gc-cons-threshold (* 8 1024 1024))))
    (setq pi-coding-agent--last-table-display-width width)
    (pi-coding-agent--remove-table-overlays beg end)
    (dolist (region (pi-coding-agent--treesit-table-regions beg end))
      (when (pi-coding-agent--table-has-data-row-p (car region) (cdr region))
        (pi-coding-agent--decorate-table (car region) (cdr region) width)))))

;;;; Cleanup

(defun pi-coding-agent--cleanup-visible-string-buffer ()
  "Kill the persistent visible-string fontification buffer.
Called from `pi-coding-agent--cleanup-on-kill' when a chat buffer dies."
  (when (buffer-live-p pi-coding-agent--visible-string-buffer)
    (kill-buffer pi-coding-agent--visible-string-buffer)
    (setq pi-coding-agent--visible-string-buffer nil)))

(provide 'pi-coding-agent-table)
;;; pi-coding-agent-table.el ends here
