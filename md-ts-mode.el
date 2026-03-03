;;; md-ts-mode.el --- Major mode for Markdown using tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.
;; Copyright (C) 2025-2026 Daniel Nouri <daniel.nouri@gmail.com>

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/md-ts-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: markdown languages tree-sitter

;; Based on markdown-ts-mode from GNU Emacs 31 by Rahul Martim Juliato.

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A tree-sitter-based major mode for editing Markdown files.  Works
;; on Emacs 29, 30, and 31.  Features include syntax highlighting for
;; headings, emphasis, code spans, links, block quotes, and fenced
;; code blocks with embedded language highlighting.
;;
;; Requires tree-sitter grammars:
;; - tree-sitter-markdown v0.4.1+
;; - tree-sitter-markdown-inline v0.4.1+

;;; Code:

(require 'treesit)
(require 'seq)
(require 'subr-x)
(require 'outline)

;;; Compatibility
;;
;; Backport shims for Emacs 31 tree-sitter features.  On Emacs 31+
;; all guards are false and no shims are defined.  Delete this
;; section when minimum Emacs is 31.

;; Silence byte-compiler for variables defined only in Emacs 30/31.
(defvar treesit-enabled-modes)
(defvar treesit-outline-predicate)
;; Ensure `treesit-outline-predicate' is bound on Emacs 29 (where
;; treesit.el doesn't define it) so `setq-local' works in `md-ts-mode'.
(unless (boundp 'treesit-outline-predicate)
  (setq treesit-outline-predicate nil))

;; Forward-declare functions that are either native (Emacs 30/31) or
;; installed via fset below.  Needed because the byte-compiler cannot
;; see inside `unless' guards.
(declare-function derived-mode-add-parents "derived" (mode parents))
(declare-function treesit--cleanup-local-range-overlays "treesit")
(declare-function treesit-ensure-installed "treesit")
(declare-function treesit-local-parsers-on "treesit")
(declare-function treesit-merge-font-lock-feature-list "treesit")
(declare-function treesit-range-rules "treesit")
(declare-function treesit-update-ranges "treesit")

;; Emacs 30 widened the signatures of these C functions (added TAG and
;; LANGUAGE arguments).  Declare the Emacs 30+ arglists so the Emacs
;; 29 byte-compiler accepts multi-version calls without warnings.
(declare-function treesit-parser-create "treesit.c"
  (language &optional buffer no-reuse tag))
(declare-function treesit-parser-list "treesit.c"
  (&optional buffer language))

;; Emacs 29 shims — C-level functions and variables missing before
;; Emacs 30.  Defined with md-ts-- prefix; aliased to the real name
;; when the native version is absent.

(defun md-ts--treesit-node-children (node &optional named)
  "Return a list of NODE's children.
If NAMED is non-nil, return only named children."
  (let ((count (treesit-node-child-count node named))
        (children nil))
    (dotimes (i count)
      (push (treesit-node-child node i named) children))
    (nreverse children)))

(unless (fboundp 'treesit-node-children)
  (fset 'treesit-node-children (symbol-function 'md-ts--treesit-node-children)))

(defun md-ts--derived-mode-add-parents (_mode _parents)
  "No-op shim.  `derived-mode-add-parents' was added in Emacs 30."
  nil)

(unless (fboundp 'derived-mode-add-parents)
  (fset 'derived-mode-add-parents
        (symbol-function 'md-ts--derived-mode-add-parents)))

(defun md-ts--treesit--cleanup-local-range-overlays (modified-tick beg end)
  "Delete local-parser overlays between BEG and END older than MODIFIED-TICK."
  (dolist (ov (overlays-in beg end))
    (when-let* ((ov-timestamp
                 (overlay-get ov 'treesit-parser-ov-timestamp)))
      (when (< ov-timestamp modified-tick)
        (when-let* ((local-parser (overlay-get ov 'treesit-parser)))
          (treesit-parser-delete local-parser))
        (delete-overlay ov)))))

(unless (fboundp 'treesit--cleanup-local-range-overlays)
  (fset 'treesit--cleanup-local-range-overlays
        (symbol-function 'md-ts--treesit--cleanup-local-range-overlays)))

;; Always t after loading: on Emacs 31 the native functions exist;
;; on 29/30 the shims defined below provide them.  Initialized here
;; so that `defvar' is the sole declaration (no top-level `setq').
(defvar md-ts--range-shims-ready t
  "Non-nil when all range-related tree-sitter shims are available.")

(defvar md-ts--range-shims-installed nil
  "Non-nil when md-ts-mode installed its own range shims.
This is nil on Emacs 31+ where the native versions are used.")

;; Emacs 29/30 shims — range infrastructure, utility functions, and
;; C-function arity adapters.  Defined with md-ts-- prefix and
;; aliased to the real name when the native version is absent.
;; Delete this section when minimum Emacs is 31.

(defmacro md-ts--declare-unavailable-functions ()
  "Declare treesit C functions for the byte-compiler."
  '(progn
     (declare-function treesit-language-available-p "treesit.c")
     (declare-function treesit-parser-create "treesit.c")
     (declare-function treesit-node-parent "treesit.c")
     (declare-function treesit-node-child "treesit.c")
     (declare-function treesit-node-type "treesit.c")
     (declare-function treesit-node-start "treesit.c")
     (declare-function treesit-node-end "treesit.c")
     (declare-function treesit-node-string "treesit.c")
     (declare-function treesit-node-check "treesit.c")
     (declare-function treesit-query-capture "treesit.c")
     (declare-function treesit-search-subtree "treesit.c")
     (declare-function treesit-search-forward "treesit.c")
     (declare-function treesit-available-p "treesit.c")
     (defvar treesit-thing-settings)
     (defvar treesit-major-mode-remap-alist)
     (defvar treesit-extra-load-path)
     (defvar treesit-enabled-modes)))

(defun md-ts--treesit-ensure-installed (lang)
  "Ensure the grammar for LANG is available."
  (treesit-ready-p lang t))

(defun md-ts--treesit-merge-font-lock-feature-list (features-1 features-2)
  "Merge FEATURES-1 and FEATURES-2, removing duplicates per level."
  (let ((result nil))
    (while (or (car features-1) (car features-2))
      (cond
       ((and (car features-1) (not (car features-2)))
        (push (car features-1) result))
       ((and (not (car features-1)) (car features-2))
        (push (car features-2) result))
       (t (push (seq-uniq (append (car features-1) (car features-2)))
                result)))
      (setq features-1 (cdr features-1)
            features-2 (cdr features-2)))
    (nreverse result)))

;; Emacs 29/30 C-function arity adapters.
;;
;; Emacs 30 added a TAG argument to `treesit-parser-create' and
;; LANGUAGE/TAG filters to `treesit-parser-list'.  Emacs 29 has
;; narrower signatures.  These helpers abstract the difference.

(defun md-ts--parser-create (lang &optional buffer no-reuse tag)
  "Create a parser for LANG, like `treesit-parser-create'.
BUFFER, NO-REUSE, and TAG are passed through.
On Emacs 29, TAG is silently ignored."
  (if (>= emacs-major-version 30)
      (treesit-parser-create lang buffer no-reuse tag)
    (treesit-parser-create lang buffer no-reuse)))

(defun md-ts--parser-list (&optional buffer language)
  "Return parsers for BUFFER, optionally filtered by LANGUAGE.
On Emacs 29, applies the LANGUAGE filter in Lisp."
  (if (>= emacs-major-version 30)
      (treesit-parser-list buffer language)
    (let ((all (treesit-parser-list buffer)))
      (if language
          (seq-filter (lambda (p)
                        (eq (treesit-parser-language p) language))
                      all)
        all))))

;; Range functions.

(defun md-ts--treesit-range-fn-exclude-children (node offset)
  "Return ranges covering NODE but excluding its children.
OFFSET is a cons (START-OFFSET . END-OFFSET) added to the bounds."
  (let* ((start (+ (treesit-node-start node) (or (car offset) 0)))
         (end (+ (treesit-node-end node) (or (cdr offset) 0)))
         (prev-end start)
         (ranges nil))
    (dolist (child (treesit-node-children node))
      (let ((child-start (treesit-node-start child))
            (child-end (treesit-node-end child)))
        (push (cons prev-end child-start) ranges)
        (setq prev-end child-end)))
    (push (cons prev-end end) ranges)
    (nreverse ranges)))

(defun md-ts--treesit-query-range (node query &optional beg end offset
                                        range-fn)
  "Query NODE with QUERY and return a list of (START . END) ranges.
BEG, END restrict the query.  OFFSET is (START-OFFSET . END-OFFSET).
RANGE-FN, if non-nil, is called with (NODE OFFSET) to produce ranges
instead of simple offset arithmetic.  Captures starting with
underscore are ignored."
  (let ((offset-left (or (car offset) 0))
        (offset-right (or (cdr offset) 0))
        (result nil))
    (dolist (capture (treesit-query-capture node query beg end))
      (let ((name (car capture))
            (cap-node (cdr capture)))
        (unless (string-prefix-p "_" (symbol-name name))
          (if range-fn
              (dolist (r (funcall range-fn cap-node offset))
                (push r result))
            (push (cons (+ (treesit-node-start cap-node) offset-left)
                        (+ (treesit-node-end cap-node) offset-right))
                  result)))))
    (nreverse result)))

(defun md-ts--treesit-query-range-by-language
    (node query language-fn &optional beg end offset range-fn)
  "Like `treesit-query-range', but group ranges by language.
QUERY NODE and return an alist ((LANGUAGE . RANGES) ...).
Nodes captured as @language are passed to LANGUAGE-FN to determine
the language symbol; nil means skip.  Other captures produce ranges
for the most recently resolved language.  BEG, END, OFFSET, and
RANGE-FN have the same meaning as in `treesit-query-range'."
  (let ((offset-left (or (car offset) 0))
        (offset-right (or (cdr offset) 0))
        (current-lang nil)
        (ranges-by-language nil))
    (dolist (capture (treesit-query-capture node query beg end))
      (let ((name (car capture))
            (cap-node (cdr capture)))
        (cond
         ((eq name 'language)
          (setq current-lang (funcall language-fn cap-node)))
         ((string-prefix-p "_" (symbol-name name))
          nil)
         (current-lang
          (let ((ranges (if range-fn
                           (funcall range-fn cap-node offset)
                         (list (cons (+ (treesit-node-start cap-node)
                                        offset-left)
                                     (+ (treesit-node-end cap-node)
                                        offset-right)))))
                (entry (assq current-lang ranges-by-language)))
            (if entry
                (setcdr entry (append (cdr entry) ranges))
              (push (cons current-lang ranges) ranges-by-language)))))))
    (nreverse ranges-by-language)))

(defun md-ts--treesit-range-rules (&rest query-specs)
  "Produce settings for `treesit-range-settings'.
QUERY-SPECS are alternating :KEYWORD VALUE pairs followed by a
QUERY.  Accepts :embed, :host, :local, :offset, and :range-fn.
:embed can be a symbol (language) or a function (dynamic language).
Returns a list of 5-element tuples (QUERY EMBED LOCAL OFFSET RANGE-FN)."
  (let (host embed offset result local range-fn)
    (while query-specs
      (pcase (pop query-specs)
        (:local (when (eq t (pop query-specs))
                  (setq local t)))
        (:host
         (let ((host-lang (pop query-specs)))
           (unless (symbolp host-lang)
             (signal 'treesit-error
                     (list "Value of :host option should be a symbol"
                           host-lang)))
           (setq host host-lang)))
        (:embed
         (let ((embed-lang (pop query-specs)))
           (unless (or (symbolp embed-lang)
                       (functionp embed-lang))
             (signal 'treesit-error
                     (list "Value of :embed option should be a symbol or a function"
                           embed-lang)))
           (setq embed embed-lang)))
        (:offset
         (let ((range-offset (pop query-specs)))
           (unless (and (consp range-offset)
                        (numberp (car range-offset))
                        (numberp (cdr range-offset)))
             (signal 'treesit-error
                     (list "Value of :offset option should be a pair of numbers"
                           range-offset)))
           (setq offset range-offset)))
        (:range-fn
         (let ((fn (pop query-specs)))
           (unless (functionp fn)
             (signal 'treesit-error
                     (list "Value of :range-fn option should be a function"
                           fn)))
           (setq range-fn fn)))
        (query
         (if (functionp query)
             (push (list query nil nil) result)
           (when (null embed)
             (signal 'treesit-error
                     (list "Value of :embed option cannot be omitted")))
           (when (null host)
             (signal 'treesit-error
                     (list "Value of :host option cannot be omitted")))
           (when (treesit-available-p)
             (push (list (treesit-query-compile host query)
                         embed local offset range-fn)
                   result)))
         (setq host nil embed nil offset nil
               local nil range-fn nil))))
    (nreverse result)))

;; Range dispatch.

(defun md-ts--query-ranges-by-lang
    (parser query lang &optional beg end offset range-fn)
  "Query PARSER with QUERY, returning ranges grouped by language.
If LANG is a function, use `md-ts--treesit-query-range-by-language'.
If LANG is a symbol, use `md-ts--treesit-query-range' and wrap the result.
BEG, END, OFFSET, and RANGE-FN are passed through."
  (if (functionp lang)
      (md-ts--treesit-query-range-by-language
       parser query lang beg end offset range-fn)
    (list (cons lang
                (md-ts--treesit-query-range
                 parser query beg end offset range-fn)))))

(defun md-ts--treesit--update-ranges-local
    (query embedded-lang modified-tick &optional beg end offset range-fn)
  "Update ranges for local parsers between BEG and END.
Use QUERY to find ranges and ensure each has a local parser for
EMBEDDED-LANG, which can be a symbol or a function.  MODIFIED-TICK,
OFFSET, and RANGE-FN control overlay timestamps and range computation."
  (let* ((host-lang (treesit-query-language query))
         (host-parser (treesit-parser-create host-lang))
         (ranges-by-lang (md-ts--query-ranges-by-lang
                          host-parser query embedded-lang
                          beg end offset range-fn)))
    (dolist (lang-and-ranges ranges-by-lang)
      (let ((lang (car lang-and-ranges))
            (ranges (cdr lang-and-ranges)))
        (pcase-dolist (`(,beg . ,end) ranges)
          (let ((has-parser
                 (catch 'done
                   (dolist (ov (overlays-in beg end) nil)
                     (when-let* ((embedded-parser
                                  (overlay-get ov 'treesit-parser))
                                 (parser-lang (treesit-parser-language
                                               embedded-parser)))
                       (when (eq parser-lang lang)
                         (treesit-parser-set-included-ranges
                          embedded-parser `((,beg . ,end)))
                         (move-overlay ov beg end)
                         (overlay-put ov 'treesit-parser-ov-timestamp
                                      modified-tick)
                         (throw 'done t)))))))
            (when (not has-parser)
              (let ((embedded-parser
                     (condition-case nil
                         (md-ts--parser-create lang nil t 'embedded)
                       (treesit-load-language-error nil))))
                (when embedded-parser
                  (let ((ov (make-overlay beg end nil nil t)))
                    (overlay-put ov 'treesit-parser embedded-parser)
                    (overlay-put ov 'treesit-host-parser host-parser)
                    (overlay-put ov 'treesit-parser-ov-timestamp
                                 modified-tick)
                    (treesit-parser-set-included-ranges
                     embedded-parser `((,beg . ,end)))))))))))))

(defun md-ts--treesit-update-ranges (&optional beg end)
  "Update the ranges for each language in the current buffer.
If BEG and END are non-nil, only update ranges in that region."
  (let ((modified-tick (buffer-chars-modified-tick))
        (beg (or beg (point-min)))
        (end (or end (point-max))))
    (dolist (setting treesit-range-settings)
      (let ((query (nth 0 setting))
            (language (nth 1 setting))
            (local (nth 2 setting))
            (offset (nth 3 setting))
            (range-fn (nth 4 setting)))
        (cond
         ((functionp query) (funcall query beg end))
         (local
          (md-ts--treesit--update-ranges-local
           query language modified-tick beg end offset range-fn))
         (t
          (let* ((host-lang (treesit-query-language query))
                 (ranges-by-lang (md-ts--query-ranges-by-lang
                                  host-lang query language
                                  beg end offset range-fn)))
            (dolist (lang-and-ranges ranges-by-lang)
              (let* ((resolved-lang (car lang-and-ranges))
                     (new-ranges (cdr lang-and-ranges))
                     (parser (treesit-parser-create resolved-lang))
                     (old-ranges (treesit-parser-included-ranges parser))
                     (set-ranges (treesit--clip-ranges
                                  (treesit--merge-ranges
                                   old-ranges new-ranges beg end)
                                  (point-min) (point-max))))
                (dolist (p (md-ts--parser-list nil resolved-lang))
                  (treesit-parser-set-included-ranges
                   p (or set-ranges
                         `((,(point-min) . ,(point-min))))))))))))
    (treesit--cleanup-local-range-overlays modified-tick beg end))))

;; Alias all Emacs 31 range infrastructure shims.  The guard checks
;; for a function that only exists in Emacs 31; on 29/30 all shims
;; are installed.
(unless (fboundp 'treesit-range-fn-exclude-children)
  (dolist (pair `((treesit-ensure-installed
                   . md-ts--treesit-ensure-installed)
                  (treesit-merge-font-lock-feature-list
                   . md-ts--treesit-merge-font-lock-feature-list)
                  (treesit-range-fn-exclude-children
                   . md-ts--treesit-range-fn-exclude-children)
                  (treesit-query-range
                   . md-ts--treesit-query-range)
                  (treesit-query-range-by-language
                   . md-ts--treesit-query-range-by-language)
                  (treesit-range-rules
                   . md-ts--treesit-range-rules)
                  ;; Use intern to avoid triggering package-lint's
                  ;; private-symbol check for this Emacs-internal name.
                  (,(intern "treesit--update-ranges-local")
                   . md-ts--treesit--update-ranges-local)
                  (treesit-update-ranges
                   . md-ts--treesit-update-ranges)))
    (fset (car pair) (symbol-function (cdr pair))))
  (setq md-ts--range-shims-installed t))

;; Emacs 29 font-lock polyfill — local parser support.
;;
;; Emacs 29's `treesit-font-lock-fontify-region' calls
;; `treesit-buffer-root-node' which returns only the first parser for
;; each language.  When local parsers (per-node overlays) exist, a
;; local parser may shadow the non-local one and return a root node
;; that covers only a single inline range — dropping captures for all
;; other ranges.
;;
;; Worse, Emacs 29's `treesit-query-capture' silently returns nil
;; when a parser has disjoint included ranges, so even the non-local
;; parser with merged ranges cannot produce correct results.
;;
;; Emacs 30 added `treesit-local-parsers-on' and collects root nodes
;; from both local and global parsers.  However, it does NOT exclude
;; global parsers when local parsers exist for the same language —
;; so the global parser's cross-paragraph trees still get fontified.
;; We work around this by giving the global markdown-inline parser
;; empty ranges (see `md-ts-mode').  This polyfill goes further and
;; also excludes such global parsers from the root-node list.

(defun md-ts--treesit-local-parsers-on (&optional beg end _language
                                                  _with-host)
  "Return local parsers between BEG and END (Emacs 29 polyfill).
LANGUAGE and WITH-HOST are accepted for signature compatibility but
ignored."
  (let (result)
    (dolist (ov (overlays-in (or beg (point-min)) (or end (point-max))))
      (when-let* ((parser (overlay-get ov 'treesit-parser)))
        (push parser result)))
    (nreverse result)))

(unless (fboundp 'treesit-local-parsers-on)
  (fset 'treesit-local-parsers-on
        (symbol-function 'md-ts--treesit-local-parsers-on)))

(defun md-ts--treesit-font-lock-fontify-region (start end &optional loudly)
  "Fontify the region between START and END.
If LOUDLY is non-nil, display some debugging information.

This is an Emacs 29 polyfill that collects root nodes from both
local (per-overlay) parsers and global parsers, matching the
behavior of Emacs 30's `treesit-font-lock-fontify-region'."
  (when (or loudly treesit--font-lock-verbose)
    (message "Fontifying region: %s-%s" start end))
  (treesit-update-ranges start end)
  (font-lock-unfontify-region start end)
  (let* ((local-parsers (treesit-local-parsers-on start end))
         (local-langs (mapcar #'treesit-parser-language local-parsers))
         ;; Exclude global parsers whose language has local parsers.
         ;; Local parsers each have a single contiguous range and
         ;; produce correct query results; the global parser for the
         ;; same language has disjoint ranges and hits the Emacs 29
         ;; treesit-query-capture bug (silently returns nil).
         (global-parsers
          (seq-remove (lambda (p)
                        (memq (treesit-parser-language p) local-langs))
                      (treesit-parser-list)))
         (root-nodes
          (mapcar #'treesit-parser-root-node
                  (append local-parsers global-parsers))))
    (dolist (setting treesit-font-lock-settings)
      (let* ((query (nth 0 setting))
             (enable (nth 1 setting))
             (override (nth 3 setting))
             (language (treesit-query-language query))
             (root-nodes
              (seq-filter
               (lambda (node)
                 (eq (treesit-node-language node) language))
               root-nodes)))
        (when (eq treesit--font-lock-fast-mode 'unspecified)
          (pcase-let ((`(,max-depth ,max-width)
                       (treesit-subtree-stat
                        (treesit-buffer-root-node language))))
            (if (or (> max-depth 100) (> max-width 4000))
                (setq treesit--font-lock-fast-mode t)
              (setq treesit--font-lock-fast-mode nil))))
        (when-let*
            ((activate (eq t enable))
             (nodes (if (eq t treesit--font-lock-fast-mode)
                        (mapcan
                         (lambda (node)
                           (treesit--children-covering-range-recurse
                            node start end (* 4 jit-lock-chunk-size)))
                         root-nodes)
                      root-nodes)))
          (ignore activate) ; silence byte-compiler unused-variable warning
          (dolist (sub-node nodes)
            (let* ((delta-start
                    (car treesit--font-lock-query-expand-range))
                   (delta-end
                    (cdr treesit--font-lock-query-expand-range))
                   (captures
                    (treesit-query-capture
                     sub-node query
                     (max (- start delta-start) (point-min))
                     (min (+ end delta-end) (point-max)))))
              (with-silent-modifications
                (dolist (capture captures)
                  (let* ((face (car capture))
                         (node (cdr capture))
                         (node-start (treesit-node-start node))
                         (node-end (treesit-node-end node)))
                    (if (and (facep face)
                             (or (>= start node-end)
                                 (>= node-start end)))
                        (when (or loudly treesit--font-lock-verbose)
                          (message
                           "Captured node %s(%s-%s) but it is outside of fontifying region"
                           node node-start node-end))
                      (cond
                       ((facep face)
                        (treesit-fontify-with-override
                         (max node-start start) (min node-end end)
                         face override))
                       ((functionp face)
                        (funcall face node override start end)))
                      (when (or loudly treesit--font-lock-verbose)
                        (message
                         "Fontifying text from %d to %d, Face: %s, Node: %s"
                         (max node-start start) (min node-end end)
                         face
                         (treesit-node-type node)))))))))))))
  `(jit-lock-bounds ,start . ,end))

;; On Emacs 29, the native `treesit-font-lock-fontify-region' only
;; queries global parsers.  Our polyfill also collects local (overlay-
;; based) parsers, matching Emacs 30+ behavior.  We guard on the Emacs
;; version rather than `fboundp' because we already shimmed
;; `treesit-local-parsers-on' above.
(when (< emacs-major-version 30)
  (fset 'treesit-font-lock-fontify-region
        (symbol-function 'md-ts--treesit-font-lock-fontify-region)))

;; On Emacs 31 the macro is built-in.  Expand it unconditionally at
;; compile time so treesit C-function declarations are always visible.
(eval-when-compile
  (if (fboundp 'treesit-declare-unavailable-functions)
      (treesit-declare-unavailable-functions)
    (md-ts--declare-unavailable-functions)))

;;; Grammar recipes

(add-to-list
 'treesit-language-source-alist
 '(markdown
   "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
   "v0.4.1" "tree-sitter-markdown/src")
 t)
(add-to-list
 'treesit-language-source-alist
 '(markdown-inline
   "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
   "v0.4.1" "tree-sitter-markdown-inline/src")
 t)

;;; Variables

(defvar md-ts--code-block-language-map
  '(("c++" . cpp)
    ("c#" . c-sharp)
    ("sh" . bash))
  "Alist mapping code block language names to tree-sitter languages.

Keys should be strings, and values should be language symbols.

For example, \"c++\" in

    ```c++
    int main() {
        return 0;
    }
    ```

maps to tree-sitter language `cpp'.")

(defvar md-ts-code-block-source-mode-map
  '((bash . bash-ts-mode)
    (c . c-ts-mode)
    (c-sharp . csharp-ts-mode)
    (cmake . cmake-ts-mode)
    (cpp . c++-ts-mode)
    (css . css-ts-mode)
    (dockerfile . dockerfile-ts-mode)
    (elixir . elixir-ts-mode)
    (go . go-ts-mode)
    (gomod . go-mod-ts-mode)
    (gowork . go-work-ts-mode)
    (heex . heex-ts-mode)
    (html . html-ts-mode)
    (java . java-ts-mode)
    (javascript . js-ts-mode)
    (json . json-ts-mode)
    (lua . lua-ts-mode)
    (php . php-ts-mode)
    (python . python-ts-mode)
    (ruby . ruby-ts-mode)
    (rust . rust-ts-mode)
    (toml . toml-ts-mode)
    (tsx . tsx-ts-mode)
    (typescript . typescript-ts-mode)
    (yaml . yaml-ts-mode))
  "An alist of supported code block languages and their major mode.")

(defcustom md-ts-hide-markup nil
  "Non-nil means hide Markdown markup delimiters in this buffer."
  :type 'boolean
  :safe #'booleanp
  :group 'md-ts)

(defcustom md-ts-heading-scaling nil
  "Whether to use variable-height faces for headings.
When non-nil, the scaling values in `md-ts-heading-scaling-values'
will be applied to headings of levels one through six respectively."
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (md-ts-update-heading-faces))
  :group 'md-ts)

(defcustom md-ts-heading-scaling-values
  '(2.0 1.7 1.4 1.1 1.0 1.0)
  "List of scaling values for headings of level one through six.
Used when `md-ts-heading-scaling' is non-nil."
  :type '(repeat float)
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (md-ts-update-heading-faces))
  :group 'md-ts)

;;; Faces

(defgroup md-ts-faces nil
  "Faces used in Markdown TS Mode."
  :group 'md-ts-faces
  :group 'faces)

(defface md-ts-delimiter '((t (:inherit shadow)))
  "Face for Markdown structural delimiters.
Applied to heading markers (#), emphasis delimiters (* and **),
code backticks, setext underlines, thematic breaks, table
separators, and other structural markup characters.")

(defface md-ts-heading-1 '((t (:inherit outline-1 :weight bold)))
  "Face for first level Markdown headings.")

(defface md-ts-heading-2 '((t (:inherit outline-2 :weight bold)))
  "Face for second level Markdown headings.")

(defface md-ts-heading-3 '((t (:inherit outline-3 :weight bold)))
  "Face for third level Markdown headings.")

(defface md-ts-heading-4 '((t (:inherit outline-4 :weight bold)))
  "Face for fourth level Markdown headings.")

(defface md-ts-heading-5 '((t (:inherit outline-5 :weight bold)))
  "Face for fifth level Markdown headings.")

(defface md-ts-heading-6 '((t (:inherit outline-6 :weight bold)))
  "Face for sixth level Markdown headings.")

(defface md-ts-list-marker '((t (:inherit shadow)))
  "Face for Markdown list markers like - and *.")

(defface md-ts-block-quote '((t (:inherit font-lock-doc-face)))
  "Face for Markdown block quotes.")

(defface md-ts-strikethrough '((t (:strike-through t)))
  "Face for Markdown strikethrough text (~~deleted~~).")

(defface md-ts-language-keyword '((t (:inherit font-lock-keyword-face)))
  "Face for the language keyword for Markdown code blocks.")

(defface md-ts-task-list-marker '((t (:inherit font-lock-builtin-face)))
  "Face for task list markers ([ ] and [x]).")

(defface md-ts-code '((t (:inherit (fixed-pitch font-lock-constant-face))))
  "Face for inline code, indented code blocks, and fenced code blocks.
Inherits `fixed-pitch' for a monospace font and
`font-lock-constant-face' for a distinct foreground color.
Customize this face to add a contrasting background, for example:
\(set-face-attribute \\='md-ts-code nil :background \"gray20\").")

(defun md-ts-update-heading-faces ()
  "Update heading faces according to `md-ts-heading-scaling'.
When `md-ts-heading-scaling' is non-nil, apply the heights from
`md-ts-heading-scaling-values' to heading faces 1 through 6.
Otherwise reset heights to `unspecified' so themes can provide
their own scaling."
  (dotimes (num 6)
    (let* ((face (intern (format "md-ts-heading-%s" (1+ num))))
           (scale (if md-ts-heading-scaling
                      (float (nth num md-ts-heading-scaling-values))
                    'unspecified)))
      (unless (get face 'saved-face)
        (set-face-attribute face nil :height scale)))))

;;; Font-lock

(defun md-ts--fontify-delimiter (node override start end &rest _)
  "Fontify delimiter NODE and optionally hide its markup.
OVERRIDE, START, and END are passed to `treesit-fontify-with-override'."
  (treesit-fontify-with-override
   (treesit-node-start node) (treesit-node-end node)
   'md-ts-delimiter override start end)
  (when md-ts-hide-markup
    (let ((hide-end (treesit-node-end node))
          (type (treesit-node-type node)))
      ;; For ATX heading markers, also hide the trailing space.
      ;; For setext underlines, also hide the trailing newline.
      (when (or (string-prefix-p "atx_h" type)
                (string-prefix-p "setext_h" type))
        (setq hide-end (min (1+ hide-end) (point-max))))
      (put-text-property (treesit-node-start node) hide-end
                         'invisible 'md-ts--markup))))

(defun md-ts--fontify-thematic-break (node override start end &rest _)
  "Fontify thematic break NODE as a horizontal rule.
OVERRIDE, START, and END are passed to `treesit-fontify-with-override'.
Replaces the `---` (or `***` etc.) with a line of `─' characters
via the `display' text property."
  (let* ((beg (treesit-node-start node))
         (node-end (treesit-node-end node))
         (rule (make-string (max 3 (- (window-width) 1)) ?─)))
    (treesit-fontify-with-override beg node-end 'md-ts-delimiter
                                   override start end)
    (put-text-property beg node-end 'display rule)))

(defun md-ts--fontify-task-marker (node override start end &rest _)
  "Fontify task list marker NODE with face and checkbox display.
OVERRIDE, START, and END are passed to `treesit-fontify-with-override'.
Replaces `[ ]' with ☐ and `[x]' with ☑ via `display' property."
  (let* ((beg (treesit-node-start node))
         (node-end (treesit-node-end node))
         (checked (string= (treesit-node-type node) "task_list_marker_checked"))
         (glyph (if checked "☑" "☐")))
    (treesit-fontify-with-override beg node-end 'md-ts-task-list-marker
                                   override start end)
    (put-text-property beg node-end 'display glyph)))

(defun md-ts--fontify-link-node (node override start end &rest _)
  "Fontify inline link or image NODE, optionally hiding URL.
Applies `shadow' to bracket/paren delimiters.  When
`md-ts-hide-markup' is non-nil, hides the `[', `]', and the
entire `(URL)' tail so only the link text remains visible.
OVERRIDE, START, and END are passed to `treesit-fontify-with-override'."
  (dolist (child (treesit-node-children node))
    (let ((type (treesit-node-type child)))
      (when (member type '("[" "]" "(" ")" "!"))
        (treesit-fontify-with-override
         (treesit-node-start child) (treesit-node-end child)
         'shadow override start end)
        (when md-ts-hide-markup
          (cond
           ((member type '("[" "!"))
            (put-text-property (treesit-node-start child)
                               (treesit-node-end child)
                               'invisible 'md-ts--markup))
           ((string= type "]")
            (put-text-property (treesit-node-start child)
                               (treesit-node-end node)
                               'invisible 'md-ts--markup))))))))

(defun md-ts--fontify-fenced-code-block (node _override _start _end &rest _)
  "Fontify fenced code block NODE, hiding fence lines when appropriate.
When `md-ts-hide-markup' is non-nil, hides the opening fence line
\(delimiter + language tag + newline) and closing fence line
\(delimiter + newline) so no phantom blank lines remain.
The `md-ts-code' face is applied separately via a late-running
font-lock rule to avoid interfering with embedded language faces."
  (when md-ts-hide-markup
    (let ((block-start (treesit-node-start node))
          (block-end (treesit-node-end node)))
      ;; Find the code_fence_content child — it holds the actual code body.
      (let ((content-node
             (seq-find (lambda (c)
                         (equal (treesit-node-type c) "code_fence_content"))
                       (treesit-node-children node))))
        (if content-node
            (let ((content-start (treesit-node-start content-node))
                  (content-end (treesit-node-end content-node)))
              ;; Hide opening line: from block start through to content start
              ;; (covers ```, info_string, and the trailing newline)
              (put-text-property block-start content-start
                                 'invisible 'md-ts--markup)
              ;; Hide closing line: from content end through block-end.
              ;; The fenced_code_block node already includes the trailing
              ;; newline of the closing fence, so block-end is correct.
              (put-text-property content-end block-end
                                 'invisible 'md-ts--markup))
          ;; Empty code block (no content node): hide the entire block
          (put-text-property block-start block-end
                             'invisible 'md-ts--markup))))))

(defvar md-ts--treesit-settings
  (treesit-font-lock-rules
   :language 'markdown-inline
   :override t
   :feature 'delimiter
   '((inline_link) @md-ts--fontify-link-node
     (image) @md-ts--fontify-link-node
     (full_reference_link) @md-ts--fontify-link-node
     (collapsed_reference_link) @md-ts--fontify-link-node
     (shortcut_link) @md-ts--fontify-link-node)

   :language 'markdown
   :feature 'heading
   '((atx_heading (atx_h1_marker)) @md-ts-heading-1
     (atx_heading (atx_h2_marker)) @md-ts-heading-2
     (atx_heading (atx_h3_marker)) @md-ts-heading-3
     (atx_heading (atx_h4_marker)) @md-ts-heading-4
     (atx_heading (atx_h5_marker)) @md-ts-heading-5
     (atx_heading (atx_h6_marker)) @md-ts-heading-6
     (setext_heading heading_content: (_) @md-ts-heading-1 (setext_h1_underline))
     (setext_heading heading_content: (_) @md-ts-heading-2 (setext_h2_underline)))

   :language 'markdown
   :feature 'heading
   :override 'prepend
   '((atx_h1_marker) @md-ts--fontify-delimiter
     (atx_h2_marker) @md-ts--fontify-delimiter
     (atx_h3_marker) @md-ts--fontify-delimiter
     (atx_h4_marker) @md-ts--fontify-delimiter
     (atx_h5_marker) @md-ts--fontify-delimiter
     (atx_h6_marker) @md-ts--fontify-delimiter
     (setext_h1_underline) @md-ts--fontify-delimiter
     (setext_h2_underline) @md-ts--fontify-delimiter)

   :language 'markdown
   :feature 'paragraph
   '(((thematic_break) @md-ts--fontify-thematic-break)
     (list_item (list_marker_star) @md-ts-list-marker)
     (list_item (list_marker_plus) @md-ts-list-marker)
     (list_item (list_marker_minus) @md-ts-list-marker)
     (list_item (list_marker_dot) @md-ts-list-marker)
     (list_item (task_list_marker_unchecked) @md-ts--fontify-task-marker)
     (list_item (task_list_marker_checked) @md-ts--fontify-task-marker)
     ((pipe_table_delimiter_row) @md-ts-delimiter)
     ((pipe_table_header) @bold)
     ((html_block) @font-lock-doc-face))

   :language 'markdown
   :feature 'paragraph
   :override 'prepend
   '((block_quote) @md-ts-block-quote
     (block_quote_marker) @md-ts--fontify-delimiter
     ((block_continuation) @md-ts--fontify-delimiter
      (:match "^>" @md-ts--fontify-delimiter))
     (fenced_code_block) @md-ts--fontify-fenced-code-block)

   :language 'markdown-inline
   :override 'append
   :feature 'paragraph-inline
   '(((image_description) @link)
     ((link_destination) @font-lock-string-face)
     ((code_span) @md-ts-code)
     ((code_span_delimiter) @md-ts--fontify-delimiter)
     ((emphasis) @italic)
     ((strong_emphasis) @bold)
     ((strikethrough) @md-ts-strikethrough)
     (inline_link (link_text) @link)
     (inline_link (link_destination) @font-lock-string-face)
     (shortcut_link (link_text) @link)
     (full_reference_link (link_text) @link)
     (full_reference_link (link_label) @shadow)
     (collapsed_reference_link (link_text) @link))

   :language 'markdown-inline
   :feature 'paragraph-inline
   :override 'append
   '((emphasis_delimiter) @md-ts--fontify-delimiter)))

;;; Imenu

(defun md-ts-imenu-node-p (node)
  "Check if NODE is a valid entry to imenu."
  (equal (treesit-node-type (treesit-node-parent node))
         "atx_heading"))

(defun md-ts-imenu-name-function (node)
  "Return an imenu entry if NODE is a valid header."
  (let ((name (treesit-node-text node)))
    (if (md-ts-imenu-node-p node)
	(thread-first (treesit-node-parent node) (treesit-node-text))
      name)))

(defun md-ts-outline-predicate (node)
  "Return non-nil if NODE is a section with an ATX heading child."
  (and (equal (treesit-node-type node) "section")
       (when-let* ((child (treesit-node-child node 0)))
         (equal (treesit-node-type child) "atx_heading"))))

;;; Code blocks

(defvar-local md-ts--configured-languages nil
  "Languages whose font-lock and indent have been loaded in this buffer.")

(defvar-local md-ts--unavailable-languages nil
  "Languages whose mode failed to activate (missing grammars, etc.).")

(defun md-ts--harvest-treesit-configs (mode)
  "Harvest tree-sitter configs from MODE.
Return a plist with :font-lock, :simple-indent, and :range keys,
or nil if MODE fails to activate (e.g. missing grammar dependencies)."
  (condition-case err
      (with-temp-buffer
        (funcall mode)
        (list :font-lock treesit-font-lock-settings
              :simple-indent treesit-simple-indent-rules
              :range treesit-range-settings))
    (error
     (message "md-ts-mode: failed to harvest configs from %s: %s"
              mode (error-message-string err))
     nil)))

(defun md-ts--add-config-for-mode (language mode)
  "Add font-lock and indent configurations for LANGUAGE from MODE.
Re-appends `code' feature rules at the end so `md-ts-code' face
is applied after all embedded language fontification.
Return non-nil on success, nil if MODE could not be harvested."
  (let ((configs (md-ts--harvest-treesit-configs mode)))
    (when configs
      (ignore language)
      ;; Remove existing code-feature rules, append new lang, re-append code.
      ;; This keeps md-ts-code rules physically last in the settings list.
      (let ((code-rules (seq-filter (lambda (s) (eq (nth 2 s) 'code))
                                    treesit-font-lock-settings))
            (non-code (seq-remove (lambda (s) (eq (nth 2 s) 'code))
                                  treesit-font-lock-settings)))
        (setq treesit-font-lock-settings
              (append non-code
                      (plist-get configs :font-lock)
                      code-rules)))
      (setq treesit-simple-indent-rules
            (append treesit-simple-indent-rules
                    (plist-get configs :simple-indent)))
      (setq treesit-range-settings
            (append treesit-range-settings
                    (plist-get configs :range)))
      (setq-local indent-line-function #'treesit-indent)
      (setq-local indent-region-function #'treesit-indent-region)
      t)))

(defun md-ts--convert-code-block-language (node)
  "Convert NODE to a language symbol for the code block.
Return nil if no tree-sitter mode is available for the language."
  (let* ((lang-string (alist-get (treesit-node-text node)
                                 md-ts--code-block-language-map
                                 (treesit-node-text node) nil #'equal))
         (lang (if (symbolp lang-string)
                   lang-string
                 (intern (downcase lang-string)))))
    (let ((mode (alist-get lang md-ts-code-block-source-mode-map)))
      (cond
       ((not (and mode (fboundp mode))) nil)
       ((memq lang md-ts--unavailable-languages) nil)
       ((memq lang md-ts--configured-languages) lang)
       ((md-ts--add-config-for-mode lang mode)
        (push lang md-ts--configured-languages)
        lang)
       (t
        (push lang md-ts--unavailable-languages)
        nil)))))

;;; Range settings

(defun md-ts--range-settings ()
  "Return range settings for `md-ts-mode'.
Inline nodes get local parsers; code blocks get per-block parsers."
  (treesit-range-rules
   ;; Local: one parser per (inline) / (pipe_table_cell) node,
   ;; so delimiter matching cannot cross paragraph boundaries.
   :embed 'markdown-inline
   :host 'markdown
   :local t
   '((inline) @markdown-inline
     (pipe_table_cell) @markdown-inline)

   :embed #'md-ts--convert-code-block-language
   :host 'markdown
   :local t
   '((fenced_code_block (info_string (language) @language)
                        (code_fence_content) @content))))

;;; Hide markup

(defun md-ts--set-hide-markup (value)
  "Set hiding of Markdown markup delimiters in the current buffer.
VALUE non-nil hides markup, nil shows it."
  (if value
      (add-to-invisibility-spec 'md-ts--markup)
    (remove-from-invisibility-spec 'md-ts--markup))
  (font-lock-flush))

(defun md-ts-toggle-hide-markup ()
  "Toggle hiding of Markdown markup delimiters in the current buffer."
  (interactive)
  (setq md-ts-hide-markup (not md-ts-hide-markup))
  (md-ts--set-hide-markup md-ts-hide-markup))

;;; Major mode

(defun md-ts-setup ()
  "Setup treesit for `md-ts-mode'."
  (make-local-variable 'md-ts-hide-markup)
  (setq-local treesit-font-lock-settings md-ts--treesit-settings)
  (setq-local treesit-range-settings (md-ts--range-settings))
  (add-to-list 'font-lock-extra-managed-props 'invisible)

  (when (treesit-ready-p 'html t)
    (treesit-parser-create 'html)
    (when (require 'html-ts-mode nil t)
      (defvar html-ts-mode--font-lock-settings)
      (setq-local treesit-font-lock-settings
                  (append treesit-font-lock-settings
                          html-ts-mode--font-lock-settings))
      (setq-local treesit-font-lock-feature-list
                  (treesit-merge-font-lock-feature-list
                   treesit-font-lock-feature-list
                   (bound-and-true-p
                    html-ts-mode--treesit-font-lock-feature-list)))
      (setq-local treesit-range-settings
                  (append treesit-range-settings
                          (treesit-range-rules
                           :embed 'html
                           :host 'markdown
                           :local t
                           '((html_block) @html)

                           :embed 'html
                           :host 'markdown-inline
                           '((html_tag) @html))))))

  (when (treesit-ready-p 'yaml t)
    (require 'yaml-ts-mode)
    (defvar yaml-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings
                        yaml-ts-mode--font-lock-settings))
    (setq-local treesit-font-lock-feature-list
                (treesit-merge-font-lock-feature-list
                 treesit-font-lock-feature-list
                 (bound-and-true-p
                  yaml-ts-mode--font-lock-feature-list)))
    (setq-local treesit-range-settings
                (append treesit-range-settings
                        (treesit-range-rules
                         :embed 'yaml
                         :host 'markdown
                         :local t
                         '((minus_metadata) @yaml)))))

  (when (treesit-ready-p 'toml t)
    (require 'toml-ts-mode)
    (defvar toml-ts-mode--font-lock-settings)
    (setq treesit-font-lock-settings
          (append treesit-font-lock-settings
                  toml-ts-mode--font-lock-settings))
    (setq-local treesit-font-lock-feature-list
                (treesit-merge-font-lock-feature-list
                 treesit-font-lock-feature-list
                 (bound-and-true-p
                  toml-ts-mode--font-lock-feature-list)))
    (setq-local treesit-range-settings
                (append treesit-range-settings
                        (treesit-range-rules
                         :embed 'toml
                         :host 'markdown
                         :local t
                         '((plus_metadata) @toml)))))

  (treesit-major-mode-setup)
  (md-ts--set-hide-markup md-ts-hide-markup)

  ;; Append md-ts-code rules LAST so they run after all embedded
  ;; language fontification.  `md-ts--add-config-for-mode' ensures
  ;; code rules stay at the end even when new languages are added
  ;; dynamically.  This ordering is critical:
  ;;   1. Embedded lang rules run first with nil override (set face)
  ;;   2. Code rules run last with :override 'append (layer md-ts-code)
  (setq-local treesit-font-lock-settings
              (append treesit-font-lock-settings
                      (treesit-font-lock-rules
                       :language 'markdown
                       :feature 'code
                       :override 'append
                       '((fenced_code_block) @md-ts-code
                         (indented_code_block) @md-ts-code))))
  (treesit-font-lock-recompute-features '(code)))

;;;###autoload
(define-derived-mode md-ts-mode text-mode "Markdown"
  "Major mode for editing Markdown using tree-sitter grammar."

  (setq-local comment-start "<!-- ")
  (setq-local comment-end " -->")

  (setq-local font-lock-defaults nil
	      treesit-font-lock-feature-list '((delimiter heading)
					       (paragraph)
					       (paragraph-inline)))

  (setq-local treesit-simple-imenu-settings
              `(("Headings" ,#'md-ts-imenu-node-p
                 nil ,#'md-ts-imenu-name-function)))
  (setq-local treesit-outline-predicate #'md-ts-outline-predicate)

  (when (and (treesit-ensure-installed 'markdown)
             (treesit-ensure-installed 'markdown-inline))
    ;; Global markdown-inline parser with empty ranges: prevents
    ;; Emacs from auto-creating a full-buffer parser.  Actual
    ;; fontification uses local per-node parsers (see range settings).
    (let ((inline-parser (treesit-parser-create 'markdown-inline)))
      (treesit-parser-set-included-ranges
       inline-parser `((,(point-min) . ,(point-min)))))
    (treesit-parser-create 'markdown)
    (md-ts-setup)))

(derived-mode-add-parents 'md-ts-mode '(markdown-mode))

;;;###autoload
(defun md-ts-mode-maybe ()
  "Enable `md-ts-mode' when its grammar is available."
  (declare-function treesit-language-available-p "treesit.c")
  (if (or (treesit-language-available-p 'markdown)
          (eq treesit-enabled-modes t)
          (memq 'md-ts-mode treesit-enabled-modes))
      (md-ts-mode)
    (fundamental-mode)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.md\\'" . md-ts-mode-maybe))
;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'treesit-major-mode-remap-alist
               '(markdown-mode . md-ts-mode)))

(provide 'md-ts-mode)
;;; md-ts-mode.el ends here
