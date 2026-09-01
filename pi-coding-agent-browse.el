;;; pi-coding-agent-browse.el --- Session and tree browser -*- lexical-binding: t; -*-

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

;; Session and tree browsing for pi-coding-agent.
;;
;; Provides two read-only, refreshable, keyboard-driven buffers:
;;   - Session Browser: find, filter, switch sessions (like TUI /resume)
;;   - Tree Browser: navigate conversation tree, label nodes (like TUI /tree)
;;
;; Session data comes from time-sliced scans of JSONL files on disk,
;; and conversation trees are projected from the linked chat's JSONL
;; session file.  Browsing does not need a live pi process, but the tree
;; shows the last persisted turn, so it can lag an in-flight turn;
;; refresh manually with `g'.  Tree labels are appended out-of-band and
;; fold back into every disk read.
;;
;; Session switching uses menu.el's guarded resume flow.  Renaming the
;; linked current session asks pi to append session_info; renaming any
;; other session appends session_info out-of-band.  Tree navigation
;; guards the linked session, process, and loaded file; computes the
;; target from fresh JSONL data; atomically rewrites an ordinary local
;; session file so the target's ancestor chain ends it (write-temp +
;; rename — the rename is the only step that touches the file); resumes
;; the rewritten file through menu; prefills the input buffer with the
;; target's re-edit text; and dismisses the browser window once the
;; transition settles.  Navigation does not auto-reopen the browser
;; (refresh with `g'); rewinding before the first message points at the
;; chat's fork command instead.

;;; Code:

(require 'pi-coding-agent-core)
(require 'pi-coding-agent-ui)
(require 'pi-coding-agent-jsonl)
(require 'cl-lib)
(require 'magit-section)
(require 'transient)

;; Forward declarations for functions in other modules (avoid circular deps)
(declare-function pi-coding-agent-set-session-name "pi-coding-agent-menu" (name))
(declare-function pi-coding-agent--resume-selected-session "pi-coding-agent-menu"
                  (proc chat-buf selected-path))
(declare-function pi-coding-agent--session-transition-ready-p "pi-coding-agent-menu"
                  (chat-buf action))
(declare-function pi-coding-agent--session-file-cwd-or-error "pi-coding-agent-menu"
                  (path))
(declare-function pi-coding-agent--session-list-directory "pi-coding-agent-menu"
                  (&optional chat-buf))

;;;; Response Parsers

(defun pi-coding-agent--parse-tree (response)
  "Parse a `get_tree' RESPONSE into a tree data plist.
Returns plist with :tree (vector) and :leafId (string), or nil on failure.
Currently unused by the disk-based tree seam (`--browse-load-tree'
reads the session file directly); reserved for a future RPC fallback
and as the projected-shape fixture loader for tests."
  (when (eq (plist-get response :success) t)
    (plist-get response :data)))

;;;; Session Display Helpers

(defun pi-coding-agent--collapse-whitespace (str)
  "Collapse whitespace (including newlines) in STR to a single space."
  (replace-regexp-in-string "[\n\r\t ]+" " " str))

(defun pi-coding-agent--first-nonempty-line (str)
  "Return the first non-empty line from STR.
Skips leading blank lines.  Returns empty string if STR is empty
or contains only whitespace."
  (if (or (null str) (string-empty-p str))
      ""
    (let ((lines (split-string str "\n")))
      (or (cl-find-if (lambda (l) (not (string-empty-p (string-trim l)))) lines)
          ""))))

(defun pi-coding-agent--session-display-name (session)
  "Return display name for SESSION plist.
Prefers :name, falls back to :firstMessage, then \"[empty session]\".
Newlines and excess whitespace are collapsed to single spaces."
  (let ((raw (or (pi-coding-agent--normalize-string-or-null
                  (plist-get session :name))
                 (pi-coding-agent--normalize-string-or-null
                  (plist-get session :firstMessage)))))
    (if raw
        (pi-coding-agent--collapse-whitespace raw)
      "[empty session]")))

;;;; Margin Rendering Infrastructure

(defun pi-coding-agent--propertize-face (string face)
  "Propertize STRING with both `face' and `font-lock-face' set to FACE.
This follows Magit's convention to survive fontification."
  (propertize string 'face face 'font-lock-face face))

(defun pi-coding-agent--make-margin-overlay (string)
  "Create a right-margin overlay on the current line displaying STRING.
The overlay uses `evaporate' so it auto-removes when the buffer text
is deleted (e.g., during erase-and-rewrite refresh).
STRING defaults to a single space if nil."
  (save-excursion
    (forward-line (if (bolp) -1 0))
    (let ((o (make-overlay (1+ (point)) (line-end-position) nil t)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'before-string
                   (propertize "o" 'display
                               (list (list 'margin 'right-margin)
                                     (or string " ")))))))

(defconst pi-coding-agent--session-margin-width 20
  "Right margin width for the session browser.
Accommodates: count (4 digits + \" msgs \") + age (2 + 1 + 7) + padding.
4 + 5 + 10 = 19, plus 1 char left padding = 20.")

(defconst pi-coding-agent--tree-margin-width 16
  "Right margin width for the tree browser.
Accommodates: \"[\" + 12-char label + \"]\" + padding = 16.")

(defvar-local pi-coding-agent--browse-margin-width nil
  "Right margin width for the current browse buffer.
Set by the derived mode; used by the window-configuration hook.")

(defun pi-coding-agent--browse-set-window-margins (width &optional window)
  "Set right margin to WIDTH on WINDOW (default: selected window).
Preserves any existing left margin."
  (let ((win (or window (selected-window))))
    (when (window-live-p win)
      (set-window-margins win (car (window-margins win)) width))))

(defun pi-coding-agent--browse-apply-margins ()
  "Re-apply right margins for the current browse buffer.
Reads width from `pi-coding-agent--browse-margin-width'.
Intended as a `window-configuration-change-hook' callback."
  (when pi-coding-agent--browse-margin-width
    (pi-coding-agent--browse-set-window-margins
     pi-coding-agent--browse-margin-width)))

;;;; Margin Age Formatting

(defconst pi-coding-agent--age-spec
  '(("year"   31557600)
    ("month"   2629800)
    ("week"     604800)
    ("day"       86400)
    ("hour"       3600)
    ("minute"       60)
    ("second"        1))
  "Time units and their durations in seconds.
Used for margin age display in browse buffers.")

(defun pi-coding-agent--margin-age (seconds)
  "Convert SECONDS to a (COUNT . UNIT) pair.
Returns the largest unit where COUNT >= 1, or (0 . \"second\") for zero."
  (let ((result (cons 0 "second")))
    (cl-loop for (unit secs) in pi-coding-agent--age-spec
             when (>= seconds secs)
             do (setq result (cons (floor (/ (float seconds) secs)) unit))
             and return nil)
    result))

(defconst pi-coding-agent--margin-age-unit-width
  (apply #'max (mapcar (lambda (s) (length (concat (car s) "s")))
                       pi-coding-agent--age-spec))
  "Width of the longest pluralized unit name (\"minutes\" = 7).")

(defconst pi-coding-agent--margin-age-format
  (format "%%2d %%-%ds" pi-coding-agent--margin-age-unit-width)
  "Format string for margin age: \"%2d %-7s\".")

(defun pi-coding-agent--format-margin-age (seconds)
  "Format SECONDS as a magit-log–style aligned age string.
Format: \"%2d %-Ns\" where N is the longest pluralized unit width.
Example: \" 5 minutes\", \" 1 hour   \", \"10 days   \"."
  (let* ((pair (pi-coding-agent--margin-age seconds))
         (count (car pair))
         (unit (cdr pair))
         (unit-str (if (= count 1) unit (concat unit "s"))))
    (format pi-coding-agent--margin-age-format count unit-str)))

(defun pi-coding-agent--format-margin-age-from-iso (iso-timestamp)
  "Format ISO-TIMESTAMP as a margin age string.
Returns nil on invalid input."
  (condition-case nil
      (let* ((time (date-to-time iso-timestamp))
             (diff (floor (float-time (time-subtract (current-time) time)))))
        (pi-coding-agent--format-margin-age (max 0 diff)))
    (error nil)))

;;;; Tree Helpers

(defun pi-coding-agent--active-path-ids (tree leaf-id)
  "Compute the set of node IDs on the active path.
TREE is the root vector from get_tree.
LEAF-ID is the current leaf node ID.
Returns a hash table mapping active node IDs to t."
  (let ((result (make-hash-table :test 'equal)))
    (when leaf-id
      ;; Build parent-id lookup from tree
      (let ((parent-map (make-hash-table :test 'equal))
            (stack (append tree nil)))
        (while stack
          (let* ((node (pop stack))
                 (children (plist-get node :children)))
            (when (vectorp children)
              (dotimes (i (length children))
                (let ((child (aref children i)))
                  (puthash (plist-get child :id)
                           (plist-get node :id)
                           parent-map)
                  (push child stack))))))
        ;; Walk from leaf to root, marking the active path
        (let ((current leaf-id))
          (while current
            (puthash current t result)
            (setq current (gethash current parent-map))))))
    result))

;;;; Tree Filter Predicates

(defconst pi-coding-agent--empty-assistant-preview "(no content)"
  "Preview string the RPC projection sets for assistant messages with no text.
Used as a heuristic to detect tool-dispatch-only assistant messages.")

(defun pi-coding-agent--browse-node-empty-assistant-p (node)
  "Return non-nil if NODE is an empty assistant message.
Empty assistants have no text content — typically tool-dispatch messages
containing only toolCall blocks.  Detected via the preview string heuristic.
Aborted or errored messages are NOT considered empty."
  (let ((type (plist-get node :type))
        (role (plist-get node :role)))
    (and (equal type "message")
         (equal role "assistant")
         (let ((preview (or (plist-get node :preview) "")))
           (or (string-empty-p preview)
               (equal preview pi-coding-agent--empty-assistant-preview)))
         (not (equal (plist-get node :stopReason) "aborted"))
         (not (plist-get node :errorMessage)))))

(defun pi-coding-agent--browse-node-visible-p (node filter-mode)
  "Return non-nil if NODE should be visible under FILTER-MODE.
FILTER-MODE is one of: \"default\", \"no-tools\", \"user-only\",
\"labeled-only\", \"all\".
NODE is a tree node plist.

Filtering is two-phase (matching TUI tree-selector.ts:282-311):
  Phase 1 — universal pre-filter: empty assistant messages are always
            hidden regardless of mode (unless aborted or errored).
  Phase 2 — mode-specific filter: each mode defines additional rules."
  (if (pi-coding-agent--browse-node-empty-assistant-p node)
      ;; Phase 1: universal pre-filter — empty assistants always hidden
      nil
    ;; Phase 2: mode-specific filter
    (let ((type (plist-get node :type))
          (role (plist-get node :role)))
      (pcase filter-mode
        ("all" t)
        ("labeled-only"
         (and (plist-get node :label) t))
        ("user-only"
         (and (equal type "message") (equal role "user")))
        ("no-tools"
         (and (not (member type '("model_change" "thinking_level_change")))
              (not (equal type "tool_result"))))
        (_ ;; "default"
         (not (member type '("model_change" "thinking_level_change"))))))))

;;;; Tree Flattening for Display

(defun pi-coding-agent--flatten-tree-for-display (tree leaf-id filter-mode)
  "Flatten TREE into a display-ordered list of (NODE INDENT PREFIX) lists.
LEAF-ID identifies the current leaf for active-branch-first ordering.
FILTER-MODE controls which nodes are visible.
Each entry is (NODE INDENT-LEVEL PREFIX-STRING) where PREFIX-STRING
contains tree connectors and gutter characters for visual structure."
  (let ((active-ids (pi-coding-agent--active-path-ids tree leaf-id))
        (result nil))
    (pi-coding-agent--flatten-tree-walk
     (append tree nil) 0 active-ids filter-mode
     nil nil
     (lambda (node indent prefix) (push (list node indent prefix) result)))
    (nreverse result)))

(defun pi-coding-agent--flatten-tree-walk (nodes indent active-ids filter-mode
                                                 gutter-stack is-branch-children
                                                 emit)
  "Walk NODES at INDENT level, calling EMIT for visible nodes.
ACTIVE-IDS is the active path hash table.
FILTER-MODE controls visibility.
GUTTER-STACK is a list of strings (\"│  \" or \"   \") for ancestor levels.
IS-BRANCH-CHILDREN is non-nil if NODES are siblings at a branch point.
EMIT is called with (node indent prefix) for each visible node.
Active-branch children are shown first at branch points.
Uses an explicit stack to avoid overflow on deep trees."
  ;; Each stack frame: [siblings vis-count vis-index indent gutter is-branch]
  (let* ((vis-count (cl-count-if
                     (lambda (n)
                       (pi-coding-agent--browse-node-visible-p n filter-mode))
                     nodes))
         (stack (list (vector nodes vis-count 0
                              indent gutter-stack is-branch-children))))
    (while stack
      (let* ((frame (pop stack))
             (siblings (aref frame 0))
             (v-count  (aref frame 1))
             (v-index  (aref frame 2))
             (cur-indent (aref frame 3))
             (gutter   (aref frame 4))
             (is-branch-ch (aref frame 5)))
        (when siblings
          (let* ((node (car siblings))
                 (rest (cdr siblings))
                 (is-visible (pi-coding-agent--browse-node-visible-p
                              node filter-mode))
                 (children (plist-get node :children))
                 (child-list (and (vectorp children) (append children nil)))
                 (is-branch (> (length child-list) 1))
                 (child-indent (if is-branch (1+ cur-indent) cur-indent))
                 ;; Compute gutter and child frame for this node
                 (child-gutter gutter)
                 (next-v-index v-index))
            ;; Push continuation for remaining siblings (goes UNDER children)
            (when is-visible
              (let* ((last-visible-p (= v-index (1- v-count)))
                     (connector (when is-branch-ch
                                  (if last-visible-p "└─ " "├─ ")))
                     (prefix (concat (apply #'concat gutter)
                                     (or connector "")))
                     (new-gutter (when is-branch-ch
                                   (if last-visible-p "   " "│  "))))
                (funcall emit node cur-indent prefix)
                (when new-gutter
                  (setq child-gutter (append gutter (list new-gutter))))
                (setq next-v-index (1+ v-index))))
            ;; Push remaining siblings (continuation)
            (when rest
              (push (vector rest v-count next-v-index
                            cur-indent gutter is-branch-ch)
                    stack))
            ;; Push children ON TOP (processed before remaining siblings)
            (when child-list
              (let* ((sorted (if is-branch
                                 (pi-coding-agent--sort-active-first
                                  child-list active-ids)
                               child-list))
                     (child-v-count
                      (cl-count-if
                       (lambda (n)
                         (pi-coding-agent--browse-node-visible-p n filter-mode))
                       sorted)))
                (push (vector sorted child-v-count 0
                              child-indent child-gutter is-branch)
                      stack)))))))))

(defun pi-coding-agent--sort-active-first (children active-ids)
  "Sort CHILDREN so the subtree containing an active node comes first.
ACTIVE-IDS is the hash table of active path node IDs."
  (let ((active nil)
        (inactive nil))
    (dolist (child children)
      (if (pi-coding-agent--subtree-contains-active-p child active-ids)
          (push child active)
        (push child inactive)))
    (append (nreverse active) (nreverse inactive))))

(defun pi-coding-agent--subtree-contains-active-p (node active-ids)
  "Return non-nil if NODE or any descendant is in ACTIVE-IDS.
Uses iterative DFS to avoid stack overflow on deep trees."
  (let ((stack (list node)))
    (cl-block found
      (while stack
        (let* ((n (pop stack))
               (children (plist-get n :children)))
          (when (gethash (plist-get n :id) active-ids)
            (cl-return-from found t))
          (when (vectorp children)
            (dotimes (i (length children))
              (push (aref children i) stack)))))
      nil)))

;;;; Client-Side Search/Filter

(defun pi-coding-agent--matches-filter-p (text tokens)
  "Return non-nil if TEXT matches all regexp TOKENS.
Each whitespace-separated token is a regexp.
All tokens must match for the entry to be included."
  (or (null tokens)
      (cl-every (lambda (tok) (string-match-p tok text)) tokens)))

;;;; Session Sort/Filter/Threading

(defconst pi-coding-agent--session-sort-modes
  '("threaded" "recent" "relevance")
  "Available sort modes for the session browser.")

(defun pi-coding-agent--session-sort-next (current)
  "Return the sort mode after CURRENT in the cycle."
  (let ((modes pi-coding-agent--session-sort-modes))
    (or (cadr (member current modes))
        (car modes))))

(defun pi-coding-agent--session-sort-items (items sort-mode)
  "Sort session ITEMS by SORT-MODE.
\"recent\" sorts by modified time descending.
\"relevance\" sorts by message count descending.
\"threaded\" returns items as-is (threading is handled during rendering)."
  (pcase sort-mode
    ("recent"
     (sort (copy-sequence items)
           (lambda (a b)
             (string> (plist-get a :modified) (plist-get b :modified)))))
    ("relevance"
     (sort (copy-sequence items)
           (lambda (a b)
             (> (or (plist-get a :messageCount) 0)
                (or (plist-get b :messageCount) 0)))))
    (_ items)))

(defun pi-coding-agent--session-thread-items (items)
  "Arrange ITEMS into a flat list with threading depth.
Returns a list of (session . depth) cons cells.
Top-level items have depth 0, children have depth 1+."
  (let ((by-path (make-hash-table :test 'equal))
        (children-of (make-hash-table :test 'equal))
        (root-items nil))
    ;; Index by path
    (dolist (item items)
      (puthash (plist-get item :path) item by-path))
    ;; Group children under parents
    (dolist (item items)
      (let ((parent-path (plist-get item :parentSessionPath)))
        (if (and parent-path (gethash parent-path by-path))
            (puthash parent-path
                     (append (gethash parent-path children-of) (list item))
                     children-of)
          (push item root-items))))
    ;; Build threaded list with depth (DFS)
    (let ((result nil))
      (dolist (root (nreverse root-items))
        (setq result (pi-coding-agent--collect-threaded
                      root children-of 0 result)))
      (nreverse result))))

(defun pi-coding-agent--collect-threaded (item children-of depth result)
  "Collect ITEM and its children into RESULT at DEPTH.
CHILDREN-OF maps parent path to child items.
Returns the updated RESULT list."
  (push (cons item depth) result)
  (let ((kids (gethash (plist-get item :path) children-of)))
    (dolist (kid kids)
      (setq result (pi-coding-agent--collect-threaded
                    kid children-of (1+ depth) result))))
  result)

(defun pi-coding-agent--session-filter-named (items)
  "Filter ITEMS to only those with a name."
  (cl-remove-if-not (lambda (item)
                      (pi-coding-agent--normalize-string-or-null
                       (plist-get item :name)))
                    items))

(defun pi-coding-agent--session-filter-search (items tokens)
  "Filter ITEMS by search TOKENS.
Matches against session name, first message, and allMessagesText."
  (if (null tokens)
      items
    (cl-remove-if-not
     (lambda (item)
       (let ((text (concat
                    (or (plist-get item :name) "")
                    " "
                    (or (plist-get item :firstMessage) "")
                    " "
                    (or (plist-get item :allMessagesText) ""))))
         (pi-coding-agent--matches-filter-p text tokens)))
     items)))

;;;; Time-Based Section Headers

(defun pi-coding-agent--session-time-group (iso-timestamp)
  "Return time group label for ISO-TIMESTAMP.
Groups: \"Today\", \"Yesterday\", \"This Week\", \"Older\"."
  (condition-case nil
      (let* ((time (date-to-time iso-timestamp))
             (now (current-time))
             (diff-days (/ (float-time (time-subtract now time)) 86400.0)))
        (cond
         ((< diff-days 1) "Today")
         ((< diff-days 2) "Yesterday")
         ((< diff-days 7) "This Week")
         (t "Older")))
    (error "Older")))

;;;; Section Classes

(defclass pi-coding-agent-session-section (magit-section)
  ((keymap :initform 'pi-coding-agent-session-section-map))
  "Section class for a session entry in the session browser.")

;;;; Keymaps

(defvar pi-coding-agent-browse-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "g") #'pi-coding-agent-browse-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Base keymap for pi-coding-agent browse modes.")

(defvar pi-coding-agent-session-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map pi-coding-agent-browse-mode-map)
    (define-key map (kbd "s") #'pi-coding-agent-session-browser-cycle-sort)
    (define-key map (kbd "f") #'pi-coding-agent-session-browser-toggle-named)
    (define-key map (kbd "/") #'pi-coding-agent-session-browser-search)
    (define-key map (kbd "t") #'pi-coding-agent-session-browser-toggle-scope)
    (define-key map (kbd "r") #'pi-coding-agent-session-browser-rename)
    (define-key map (kbd "d") #'pi-coding-agent-session-browser-delete)
    (define-key map (kbd "RET") #'pi-coding-agent-session-browser-switch)
    (define-key map (kbd "?") #'pi-coding-agent-session-browser-dispatch)
    (define-key map (kbd "h") #'pi-coding-agent-session-browser-dispatch)
    map)
  "Keymap for the session browser.")

(defvar pi-coding-agent-session-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'pi-coding-agent-session-browser-switch)
    (define-key map (kbd "d") #'pi-coding-agent-session-browser-delete)
    map)
  "Keymap for session sections (text property on each session line).")

;;;; Buffer-Local State

(defvar-local pi-coding-agent--session-browser-scope "current"
  "Scope for session listing: \"current\" or \"all\".")

(defvar-local pi-coding-agent--session-browser-sort "threaded"
  "Sort mode: \"threaded\", \"recent\", or \"relevance\".")

(defvar-local pi-coding-agent--session-browser-named-only nil
  "When non-nil, show only named sessions.")

(defvar-local pi-coding-agent--session-browser-items nil
  "Session items from the last `--browse-load-sessions' callback.")

(defvar-local pi-coding-agent--session-browser-search-query nil
  "Current search query string, or nil.")

(defvar-local pi-coding-agent--session-browser-search-tokens nil
  "Parsed search tokens from `pi-coding-agent--session-browser-search-query'.")

(defvar-local pi-coding-agent--session-browser-loading nil
  "Non-nil while a fetch is in progress.")

(defvar-local pi-coding-agent--session-browser-fetch-anchor nil
  "Point anchor carried across the in-flight fetch cycle.
Set to the anchor captured at fetch start.  A refresh issued while
loading captures no anchor of its own (the loading render already
destroyed the sections), so it reuses this one instead.  Cleared when
the cycle's final render runs.")

(defvar-local pi-coding-agent--session-browser-error nil
  "Error message string from last fetch, or nil on success.")

(defvar-local pi-coding-agent--session-browser-fetch-token 0
  "Generation counter for session-browser fetches.
`pi-coding-agent--browse-load-sessions' bumps it per fetch; callbacks
from superseded fetches are dropped by comparing their captured token
against the buffer's current one.")

;;;; Session Browser Dispatch Transient

(defun pi-coding-agent--session-dispatch-heading ()
  "Return heading string for the session browser dispatch transient.
Shows current scope, sort mode, and named-only state — the same state
`pi-coding-agent--session-browser-header-line' formats for the
header-line.  Transient evaluates group descriptions in the invoking
browser buffer (`transient-with-shadowed-buffer' inside
`transient--insert-group'), so these buffer-local reads see the
browser's state on the real rendering path."
  (mapconcat #'identity
             (append (list (format "scope:%s"
                                   pi-coding-agent--session-browser-scope)
                           (format "sort:%s"
                                   pi-coding-agent--session-browser-sort))
                     (and pi-coding-agent--session-browser-named-only
                          '("named-only")))
             " │ "))

(transient-define-prefix pi-coding-agent-session-browser-dispatch ()
  "Session browser help."
  [:description pi-coding-agent--session-dispatch-heading
   ["Actions"
    ("RET" "switch" pi-coding-agent-session-browser-switch)
    ("r" "rename" pi-coding-agent-session-browser-rename)
    ("d" "delete" pi-coding-agent-session-browser-delete)
    ("g" "refresh" pi-coding-agent-browse-refresh)
    ("q" "quit" quit-window)]
   ["Filter & Sort"
    ("s" "sort" pi-coding-agent-session-browser-cycle-sort)
    ("f" "named only" pi-coding-agent-session-browser-toggle-named)
    ("t" "scope" pi-coding-agent-session-browser-toggle-scope)
    ("/" "search" pi-coding-agent-session-browser-search)]])

;;;; Faces

(defface pi-coding-agent-session-name
  '((t :weight bold))
  "Face for session names in the session browser."
  :group 'pi-coding-agent)

(defface pi-coding-agent-session-message-count
  '((t :inherit shadow))
  "Face for message counts in the session browser."
  :group 'pi-coding-agent)

(defface pi-coding-agent-session-age
  '((t :inherit shadow))
  "Face for relative age in the session browser margin."
  :group 'pi-coding-agent)

(defface pi-coding-agent-session-thread-connector
  '((t :inherit shadow))
  "Face for threading connectors (├─, └─) in the session browser."
  :group 'pi-coding-agent)

(defface pi-coding-agent-session-group-header
  '((t :inherit magit-section-heading))
  "Face for time-group headers (Today, Yesterday, etc.)."
  :group 'pi-coding-agent)

;;;; Major Modes

(define-derived-mode pi-coding-agent-browse-mode magit-section-mode
  "Pi-Browse"
  "Base mode for pi-coding-agent browse buffers.
Inherits section navigation from `magit-section-mode'."
  :group 'pi-coding-agent)

(define-derived-mode pi-coding-agent-session-browser-mode
  pi-coding-agent-browse-mode "Pi-Sessions"
  "Major mode for browsing pi sessions.
\\{pi-coding-agent-session-browser-mode-map}"
  :group 'pi-coding-agent
  (setq-local header-line-format
              '(:eval (pi-coding-agent--session-browser-header-line)))
  (setq pi-coding-agent--browse-margin-width
        pi-coding-agent--session-margin-width)
  (setq-local right-margin-width pi-coding-agent--session-margin-width)
  (add-hook 'window-configuration-change-hook
            #'pi-coding-agent--browse-apply-margins nil t))

;;;; Buffer Management

(defun pi-coding-agent--session-browser-buffer-name (dir)
  "Return session browser buffer name for DIR."
  (format "*pi-coding-agent-sessions:%s*"
          (pi-coding-agent--route-preserving-abbreviate-file-name dir)))

(defun pi-coding-agent--get-or-create-session-browser (dir)
  "Get or create session browser buffer for DIR."
  (let* ((name (pi-coding-agent--session-browser-buffer-name dir))
         (buf (get-buffer name)))
    (or buf
        (with-current-buffer (generate-new-buffer name)
          (setq default-directory dir)
          (pi-coding-agent-session-browser-mode)
          (current-buffer)))))

;;;; Rendering

(defun pi-coding-agent--session-browser-render (buf)
  "Render the session browser in BUF from its buffer-local state."
  (with-current-buffer buf
    (let* ((inhibit-read-only t)
           (items (or pi-coding-agent--session-browser-items '()))
           ;; Apply filters
           (filtered (if pi-coding-agent--session-browser-named-only
                        (pi-coding-agent--session-filter-named items)
                      items))
           (filtered (pi-coding-agent--session-filter-search
                      filtered
                      pi-coding-agent--session-browser-search-tokens)))
      (magit-insert-section (root)
        (cond
         (pi-coding-agent--session-browser-loading
          (insert (pi-coding-agent--propertize-face
                   "Loading sessions..."
                   'pi-coding-agent-activity-phase)
                  "\n"))
         (pi-coding-agent--session-browser-error
          (insert (pi-coding-agent--propertize-face
                   (format "Error: %s\n" pi-coding-agent--session-browser-error)
                   'error)))
         ((null items)
          (insert "No sessions found.\n"))
         ((null filtered)
          (insert "No matching sessions.\n"))
         ((equal pi-coding-agent--session-browser-sort "threaded")
          (pi-coding-agent--session-browser-render-threaded filtered))
         ((equal pi-coding-agent--session-browser-sort "recent")
          (pi-coding-agent--session-browser-render-recent filtered))
         (t
          (let ((sorted (pi-coding-agent--session-sort-items
                         filtered pi-coding-agent--session-browser-sort)))
            (pi-coding-agent--session-browser-render-flat sorted))))))))

(defun pi-coding-agent--session-browser-render-flat (items)
  "Render ITEMS as a flat list."
  (dolist (item items)
    (pi-coding-agent--session-browser-insert-session item 0 nil)))

(defun pi-coding-agent--session-browser-render-threaded (items)
  "Render ITEMS in threaded view with connectors."
  (let ((threaded (pi-coding-agent--session-thread-items items)))
    (dolist (entry threaded)
      (let ((item (car entry))
            (depth (cdr entry)))
        (pi-coding-agent--session-browser-insert-session item depth t)))))

(defun pi-coding-agent--session-browser-render-recent (items)
  "Render ITEMS sorted by recency with time-group headers."
  (let ((sorted (pi-coding-agent--session-sort-items items "recent"))
        (last-group nil))
    (dolist (item sorted)
      (let ((group (pi-coding-agent--session-time-group
                    (plist-get item :modified))))
        (unless (equal group last-group)
          (magit-insert-section (time-group group)
            (magit-insert-heading
              (pi-coding-agent--propertize-face
               group 'pi-coding-agent-session-group-header)))
          (setq last-group group)))
      (pi-coding-agent--session-browser-insert-session item 0 nil))))

(defun pi-coding-agent--session-browser-insert-session (session depth threaded)
  "Insert SESSION as a magit-section at DEPTH.
When THREADED is non-nil, prepend threading connector at DEPTH.
In non-threaded modes, forked sessions get a \"fork:\" prefix.
Message count and age are rendered as a right-margin overlay."
  (let* ((path (plist-get session :path))
         (name (pi-coding-agent--session-display-name session))
         (count (or (plist-get session :messageCount) 0))
         (modified (plist-get session :modified))
         (is-fork (plist-get session :parentSessionPath))
         (prefix (cond
                  ((and threaded (> depth 0))
                   (concat (make-string (* 2 (1- depth)) ?\s)
                           (pi-coding-agent--propertize-face
                            "└─ " 'pi-coding-agent-session-thread-connector)))
                  ((and is-fork (not threaded))
                   (pi-coding-agent--propertize-face
                    "fork: " 'pi-coding-agent-session-thread-connector))
                  (t "")))
         (heading (concat prefix
                          (pi-coding-agent--propertize-face
                           name 'pi-coding-agent-session-name)))
         (margin-str (concat
                      (pi-coding-agent--propertize-face
                       (format "%4d msgs " count)
                       'pi-coding-agent-session-message-count)
                      (pi-coding-agent--propertize-face
                       (or (pi-coding-agent--format-margin-age-from-iso modified)
                           (format (format "%%%ds"
                                           (+ 3 pi-coding-agent--margin-age-unit-width))
                                   "?"))
                       'pi-coding-agent-session-age))))
    (magit-insert-section (session path)
      (magit-insert-heading heading)
      (pi-coding-agent--make-margin-overlay margin-str))))

;;;; Header-Line

(defun pi-coding-agent--session-browser-header-line ()
  "Return header-line string for the session browser."
  (let* ((scope pi-coding-agent--session-browser-scope)
         (sort pi-coding-agent--session-browser-sort)
         (named pi-coding-agent--session-browser-named-only)
         (query pi-coding-agent--session-browser-search-query)
         (count (length (or pi-coding-agent--session-browser-items '()))))
    (mapconcat #'identity
               (append (list (format "Sessions [%s]" scope)
                             (format "sort:%s" sort))
                       (and named '("named-only"))
                       (and query (list (format "/%s" query)))
                       (list (format "(%d)" count)
                             (pi-coding-agent--propertize-face "?:help" 'shadow)))
               " │ ")))

;;;; Session Browser Interactive Commands

(defun pi-coding-agent-session-browser-cycle-sort ()
  "Cycle the session browser sort mode."
  (interactive)
  (setq pi-coding-agent--session-browser-sort
        (pi-coding-agent--session-sort-next
         pi-coding-agent--session-browser-sort))
  (pi-coding-agent--session-browser-rerender)
  (message "Pi: Sort: %s" pi-coding-agent--session-browser-sort))

(defun pi-coding-agent-session-browser-toggle-named ()
  "Toggle named-only filter in the session browser."
  (interactive)
  (setq pi-coding-agent--session-browser-named-only
        (not pi-coding-agent--session-browser-named-only))
  (pi-coding-agent--session-browser-rerender)
  (message "Pi: Named-only: %s"
           (if pi-coding-agent--session-browser-named-only "on" "off")))

(defun pi-coding-agent-session-browser-toggle-scope ()
  "Toggle scope between current and all projects."
  (interactive)
  (setq pi-coding-agent--session-browser-scope
        (if (equal pi-coding-agent--session-browser-scope "current")
            "all" "current"))
  (pi-coding-agent--session-browser-fetch-and-render)
  (message "Pi: Scope: %s" pi-coding-agent--session-browser-scope))

(defun pi-coding-agent-session-browser-search ()
  "Set or clear search filter in the session browser."
  (interactive)
  (let ((query (read-string "Filter (regexp tokens): "
                            pi-coding-agent--session-browser-search-query))
        (need-rerender t))
    (if (string-empty-p query)
        (setq pi-coding-agent--session-browser-search-query nil
              pi-coding-agent--session-browser-search-tokens nil)
      ;; Validate regexp tokens
      (condition-case err
          (let ((tokens (split-string query)))
            (dolist (tok tokens)
              (string-match-p tok ""))
            (setq pi-coding-agent--session-browser-search-query query
                  pi-coding-agent--session-browser-search-tokens tokens))
        (invalid-regexp
         (message "Pi: Invalid regexp: %s" (error-message-string err))
         (setq need-rerender nil))))
    (when need-rerender
      (pi-coding-agent--session-browser-rerender))))

(defun pi-coding-agent-session-browser-switch ()
  "Switch to the session at point."
  (interactive)
  (if-let* ((section (magit-current-section))
            (path (oref section value)))
      (pi-coding-agent--browse-switch-session path)
    (message "Pi: No session at point")))

(defun pi-coding-agent--browse-clean-session-name (name)
  "Return NAME cleaned for a session_info append.
CR/LF runs collapse to single spaces, then surrounding whitespace
trims (pi's appendSessionInfo order)."
  (string-trim (replace-regexp-in-string "[\r\n]+" " " name)))

(defun pi-coding-agent--browse-last-entry-id-in-buffer ()
  "Return the id of the last parseable non-header line in the current buffer.
Blank or malformed trailing lines are skipped: pi's loader ignores them,
so parenting an append to one would detach it from the conversation (the
leaf walk would stop at a null parent).  The header ends the search — a
header-only file reads as nil."
  (goto-char (point-max))
  (catch 'done
    (while t
      (skip-chars-backward " \t\r\n")
      (if (bobp)
          (throw 'done nil)
        (let* ((bol (line-beginning-position))
               (data (and (> (point) bol)
                          (pi-coding-agent--parse-json-line
                           (buffer-substring-no-properties bol (point))))))
          (cond
           ((and (consp data) (equal (plist-get data :type) "session"))
            (throw 'done nil))
           ((consp data)
            (throw 'done (pi-coding-agent--normalize-string-or-null
                          (plist-get data :id))))
           ;; Blank or malformed line: step over it and retry.
           (t (goto-char bol))))))))

(defun pi-coding-agent--browse-session-file-state (path)
  "Re-read session file PATH fresh and describe its tail for an append.
Return a plist (:last-id ID-OR-NIL :ids HASH :newline-p BOOL), or nil when
PATH is missing or unreadable.  :last-id is the id of the last parseable
non-header line (see `pi-coding-agent--browse-last-entry-id-in-buffer').
:ids holds every id-shaped string in the file — a superset of pi's entry
index — for fresh-id collision checks.  :newline-p reports whether the
file already ends in a newline.  The whole file is inserted once, fresh:
a live pi process appends concurrently, so the state must reflect the
physical file, never cached browser data."
  (condition-case nil
      (when (file-readable-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (let ((ids (make-hash-table :test #'equal)))
            (goto-char (point-min))
            (while (re-search-forward
                    "\"id\"[ \t]*:[ \t]*\"\\([^\"]+\\)\"" nil t)
              (puthash (match-string-no-properties 1) t ids))
            (list :last-id (pi-coding-agent--browse-last-entry-id-in-buffer)
                  :ids ids
                  :newline-p (and (> (point-max) (point-min))
                                  (eq (char-before (point-max)) ?\n))))))
    (error nil)))

(defun pi-coding-agent--browse-fresh-entry-id (ids)
  "Return a random 8-hex entry id absent from IDS, like pi's generateId.
Collision checking is not optional: pi keys entries by id and walks
parent chains without a cycle guard, so a colliding id can wedge its
loader on the next session load.  After 100 failed tries fall back to a
wider 16-hex id, mirroring pi's full-UUID fallback."
  (cl-loop repeat 100
           for id = (format "%08x" (random #x100000000))
           when (not (gethash id ids))
           return id
           finally return (format "%08x%08x"
                                  (random #x100000000)
                                  (random #x100000000))))

(defun pi-coding-agent--browse-append-session-entry (path type payload action)
  "Append one out-of-band entry of TYPE with PAYLOAD to session file PATH.
ACTION names the operation for messages (\"rename\", \"label\").
Mirrors pi's appenders without a live process.  The file is re-read
immediately before the append via
`pi-coding-agent--browse-session-file-state' so :parentId is the id of
the current last parseable line (never the browser's cached leaf),
which keeps the entry from orphaning the context on the next load, and
the fresh id is checked against every id already in the file (pi's
loader cannot tolerate duplicates — see
`pi-coding-agent--browse-fresh-entry-id').  When the file does not end
in a newline a separator is inserted first — bytes are never glued
onto a partial line.  PAYLOAD's pairs are encoded verbatim after the
shared :type/:id/:parentId/:timestamp head, so an omitted key stays
omitted (clearing a label relies on that: the load-time fold treats an
absent :label as cleared).

The file is NEVER created: pi's _persist appends per-entry once the
file exists, and its full-rewrite path is an exclusive create ('wx')
that only runs when the file never existed — creating it here would
crash pi's next flush with EEXIST.  So this helper only ever appends
to an existing readable file.  Races: a concurrent pi append between
the read and the write turns our line into a benign sibling (name
resolution is file-order latest-wins and projection filters these
bookkeeping entries), and stale-parent orphans are impossible by
construction.  A session live elsewhere sees the change on its next
state refresh.  Return non-nil when the line was appended; nil (with a
message) when PATH is unreadable."
  (if-let* ((state (pi-coding-agent--browse-session-file-state path)))
      (let ((line (json-encode
                   (append (list :type type
                                 :id (pi-coding-agent--browse-fresh-entry-id
                                      (plist-get state :ids))
                                 :parentId (plist-get state :last-id)
                                 :timestamp (format-time-string
                                             "%Y-%m-%dT%H:%M:%S.%3NZ"
                                             (current-time) t))
                           payload))))
        (let ((coding-system-for-write 'utf-8))
          (write-region
           (concat (unless (plist-get state :newline-p) "\n") line "\n")
           nil path 'append))
        t)
    (message "Pi: Cannot %s: session file is unreadable: %s" action path)
    nil))

(defun pi-coding-agent--browse-append-session-info (path name)
  "Append a session_info entry naming session file PATH to NAME, out-of-band.
Thin wrapper over `pi-coding-agent--browse-append-session-entry' with
the session_info payload (:name NAME); see there for the freshness,
race, and never-create contract.  Return non-nil when the line was
appended."
  (pi-coding-agent--browse-append-session-entry
   path "session_info" (list :name name) "rename"))

(defun pi-coding-agent-session-browser-rename ()
  "Rename the session at point.
Prompt once; empty or whitespace-only input cancels with a message
\(names cannot be cleared, matching the TUI).  Dispatch on
current-vs-other session (see
`pi-coding-agent--browse-session-file-matches-p'):
  - Current session: `pi-coding-agent-set-session-name' RPC, then
    refresh.  Known benign race: the refresh may beat pi's
    session_info flush, leaving a stale name visible until
    \\[pi-coding-agent-browse-refresh].
  - Other session: `pi-coding-agent--browse-append-session-info'
    appends out-of-band, then refreshes; an unreadable file cancels
    with a message and no refresh."
  (interactive)
  (if-let* ((section (magit-current-section))
            (path (oref section value)))
      (let* ((item (cl-find path pi-coding-agent--session-browser-items
                            :key (lambda (it) (plist-get it :path))
                            :test #'equal))
             (existing (and item
                            (pi-coding-agent--normalize-string-or-null
                             (plist-get item :name))))
             (input (read-string "Rename session: " (or existing "")))
             (clean (pi-coding-agent--browse-clean-session-name input)))
        (if (string-empty-p clean)
            (message "Pi: Rename cancelled")
          (if (pi-coding-agent--browse-session-file-matches-p
               (pi-coding-agent--get-chat-buffer) path)
              (progn
                (pi-coding-agent-set-session-name clean)
                (pi-coding-agent--session-browser-fetch-and-render))
            (when (pi-coding-agent--browse-append-session-info path clean)
              (pi-coding-agent--session-browser-fetch-and-render)))))
    (message "Pi: No session at point")))

(defun pi-coding-agent-session-browser-delete ()
  "Delete the session at point, trashing the file when configured.
Mirrors pi's /resume picker: refuse the currently active session
\(a live linked chat buffer whose pi process is running on this
session file), confirm with `y-or-n-p', then delete with the TRASH
argument of `delete-file' — the file moves to the trash when
`delete-by-moving-to-trash' is non-nil and is unlinked otherwise.

The live-session guard only sees Emacs-side processes — a pi
running in a terminal on the same file is invisible, exactly as the
TUI's own guard cannot see Emacs — and a dead linked chat does not
block: its session is stale history and deletable.  Afterwards the
browser refreshes and the message reports whether the file was
trashed or unlinked."
  (interactive)
  (if-let* ((section (magit-current-section))
            (path (oref section value))
            (item (cl-find path pi-coding-agent--session-browser-items
                           :key (lambda (it) (plist-get it :path))
                           :test #'equal)))
      (let* ((chat-buf pi-coding-agent--chat-buffer)
             (name (or (plist-get item :name)
                       (file-name-nondirectory path))))
        (when (and (pi-coding-agent--browse-session-file-matches-p chat-buf path)
                   (pi-coding-agent--session-live-process-p
                    (buffer-local-value 'pi-coding-agent--process chat-buf)))
          (user-error "Cannot delete the currently active session"))
        (if (not (y-or-n-p (format "Delete session %s? " name)))
            (message "Pi: Delete cancelled")
          (delete-file path t)
          (pi-coding-agent--session-browser-fetch-and-render)
          (message (if delete-by-moving-to-trash
                       "Pi: moved %s to trash"
                     "Pi: deleted %s")
                   name)))
    (message "Pi: No session at point")))

;;;; Point-Preserving Rerender

(defun pi-coding-agent--browse-capture-point-anchor ()
  "Return the point anchor (IDENT . OFFSET) for the section at point.
IDENT is the section's `magit-section-ident'; OFFSET is the distance
from the section start.  Return nil when point sits on no content
section: either there is no section at point, or only the root
section, which every render recreates over the whole buffer and which
carries no identity (a loading-state render produces nothing else).
Used to both capture point before a render and to carry a position
across a fetch cycle whose intermediate renders destroy sections."
  (let ((section (magit-current-section)))
    (when (and section (not (eq section magit-root-section)))
      (cons (magit-section-ident section)
            (- (point) (oref section start))))))

(defun pi-coding-agent--browse-rerender-preserving-point (buf render-fn
                                                            &optional fallback)
  "Erase BUF, render via RENDER-FN, restore point by section identity.
The section at point is captured as a `magit-section-ident' before the
erase; after rendering, point moves to that section's start (plus the
captured intra-section column offset).  Falls back to `point-min' when
the section no longer exists (e.g. filtered away or state change).

FALLBACK, when non-nil, is a `(IDENT . OFFSET)' anchor (from
`pi-coding-agent--browse-capture-point-anchor') captured before an
intermediate render destroyed the sections point sat on — the fetch
cycle renders a loading state before the final render.  It is used
only when the point-local capture yields no anchor, so a plain
rerender (no FALLBACK) behaves exactly as before.

After the restore, every live window displaying BUF is synced to the
restored position: `erase-buffer' clamps all displaying windows to
bob and `goto-char' moves only the buffer's own point, so without
`set-window-point' a pane whose window is not selected keeps showing
point-at-top (same idiom as `pi-coding-agent--with-scroll-preservation'
in ui.el).  The sync covers both restore paths — the point-min
fallback included."
  (with-current-buffer buf
    (let* ((anchor (or (pi-coding-agent--browse-capture-point-anchor)
                       fallback))
           (ident (car anchor))
           (offset (cdr anchor)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall render-fn buf)
        (let ((new (and ident (magit-get-section ident))))
          (if new
              (let ((start (oref new start))
                    (end (oref new end)))
                (goto-char start)
                (when (> offset 0)
                  (forward-char (min offset (1- (- (or end (point-max)) start))))))
            (goto-char (point-min)))
          ;; `erase-buffer' clamped every displaying window's point to
          ;; bob; the `goto-char' above moved only the buffer's own
          ;; point.  Sync all live windows displaying BUF — the list
          ;; spans live frames and yields only live windows (same
          ;; idiom as `pi-coding-agent--with-scroll-preservation').
          (dolist (w (get-buffer-window-list buf nil t))
            (set-window-point w (point)))
          (when-let* ((cur (magit-current-section)))
            (magit-section-show cur))
          (force-mode-line-update))))))

;;;; Fetch and Render

(defun pi-coding-agent--session-browser-fetch-and-render ()
  "Fetch sessions and re-render the session browser.
Sessions are read from disk, so no live pi process is required.

The point anchor is captured before the loading-state render — that
render destroys the session sections, so without carrying the anchor
across the cycle the final render would find nothing under point to
restore (E2E defect A4).  A refresh issued while another fetch is
still loading finds no sections to capture and reuses the in-flight
cycle's anchor (see `pi-coding-agent--session-browser-fetch-anchor')."
  (let* ((buf (current-buffer))
         (anchor (or (pi-coding-agent--browse-capture-point-anchor)
                     ;; Mid-flight refresh: the loading render already
                     ;; destroyed the sections under point, so carry
                     ;; the anchor the in-flight cycle captured.
                     (and pi-coding-agent--session-browser-loading
                          pi-coding-agent--session-browser-fetch-anchor))))
    (setq pi-coding-agent--session-browser-loading t
          pi-coding-agent--session-browser-fetch-anchor anchor)
    ;; Loading-state render: default point behavior (nothing to keep).
    (pi-coding-agent--session-browser-rerender)
    (pi-coding-agent--browse-load-sessions
     pi-coding-agent--session-browser-scope
     (lambda (items error)
       (when (buffer-live-p buf)
         ;; The scan calls back from a timer in whatever buffer is
         ;; current; render in the browser buffer, not that one.
         (with-current-buffer buf
           (setq pi-coding-agent--session-browser-loading nil
                 pi-coding-agent--session-browser-fetch-anchor nil
                 pi-coding-agent--session-browser-error error
                 pi-coding-agent--session-browser-items items)
           (pi-coding-agent--session-browser-rerender anchor)))))))

(defun pi-coding-agent--session-browser-rerender (&optional fallback)
  "Re-render the session browser from local state, preserving point.
FALLBACK is a pre-fetch `(IDENT . OFFSET)' anchor handed to
`pi-coding-agent--browse-rerender-preserving-point' for the fetch
cycle's final render."
  (pi-coding-agent--browse-rerender-preserving-point
   (current-buffer) #'pi-coding-agent--session-browser-render fallback))

;;;; Tree Browser Section Classes and Keymaps

(defclass pi-coding-agent-tree-node-section (magit-section)
  ((keymap :initform 'pi-coding-agent-tree-node-section-map))
  "Section class for a tree node in the tree browser.")

(defun pi-coding-agent--register-section-types ()
  "Register browse section classes in `magit--section-type-alist'.
Wrapped because that alist is a private Magit internal; if Magit ever
changes the mechanism, this is the single place to adapt.

Unregistered types (e.g. the `time-group' headings, which are not
interactive) silently fall back to the plain `magit-section' class;
that is fine for display-only sections."
  (setf (alist-get 'session magit--section-type-alist)
        'pi-coding-agent-session-section)
  (setf (alist-get 'tree-node magit--section-type-alist)
        'pi-coding-agent-tree-node-section))
(pi-coding-agent--register-section-types)

(defvar pi-coding-agent-tree-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map pi-coding-agent-browse-mode-map)
    (define-key map (kbd "f") #'pi-coding-agent-tree-browser-cycle-filter)
    (define-key map (kbd "l") #'pi-coding-agent-tree-browser-set-label)
    (define-key map (kbd "/") #'pi-coding-agent-tree-browser-search)
    (define-key map (kbd "RET") #'pi-coding-agent-tree-browser-navigate)
    (define-key map (kbd "?") #'pi-coding-agent-tree-browser-dispatch)
    (define-key map (kbd "h") #'pi-coding-agent-tree-browser-dispatch)
    map)
  "Keymap for the tree browser.")

(defvar pi-coding-agent-tree-node-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'pi-coding-agent-tree-browser-navigate)
    (define-key map (kbd "l") #'pi-coding-agent-tree-browser-set-label)
    map)
  "Keymap for tree node sections.")

;;;; Tree Browser State

(defvar-local pi-coding-agent--tree-browser-filter "no-tools"
  "Filter mode: \"no-tools\", \"default\", \"user-only\", \"labeled-only\", \"all\".")

(defvar-local pi-coding-agent--tree-browser-tree nil
  "Projected tree from the last `--browse-load-tree' callback.
Vector of root nodes in the browse node dialect.")

(defvar-local pi-coding-agent--tree-browser-leaf-id nil
  "Current leaf node ID from the tree response.")

(defvar-local pi-coding-agent--tree-browser-visible-count 0
  "Count of visible entries from the last render.
Cached to avoid re-flattening the tree in the header-line.")

(defvar-local pi-coding-agent--tree-browser-search-query nil
  "Current search query string, or nil.")

(defvar-local pi-coding-agent--tree-browser-search-tokens nil
  "Parsed search tokens.")

(defvar-local pi-coding-agent--tree-browser-loading nil
  "Non-nil while a fetch is in progress.")

(defvar-local pi-coding-agent--tree-browser-fetch-anchor nil
  "Point anchor carried across the in-flight fetch cycle.
Same lifecycle as `pi-coding-agent--session-browser-fetch-anchor': set
from the anchor captured at fetch start, reused by a refresh issued
while loading, cleared when the cycle's final render runs.")

(defvar-local pi-coding-agent--tree-browser-error nil
  "Error message string from the last tree fetch, or nil on success.
Rendered as an error state with a zero visible count.")

(defvar-local pi-coding-agent--tree-browser-fetch-token 0
  "Generation counter for tree-browser fetches.
`pi-coding-agent--browse-load-tree' bumps it per fetch; deferred reads
from superseded fetches drop themselves by comparing their captured
token against the buffer's current one (mirrors the session side).")

(defvar-local pi-coding-agent--tree-browser-loaded-file nil
  "Session file the current tree was loaded from, or nil on error states.
Set, when a fetch succeeds, to the file the FETCH read (resolved at
fetch start — a chat session switch mid-read cannot retarget the
labeler onto a tree the browser is not showing);
`pi-coding-agent--browse-set-label' compares against it to refuse
labeling a session the chat has since left.")

(defconst pi-coding-agent--tree-filter-modes
  '("no-tools" "default" "user-only" "labeled-only" "all")
  "Available filter modes for the tree browser.")

;;;; Tree Browser Dispatch Transient

(defun pi-coding-agent--tree-dispatch-heading ()
  "Return heading string for the tree browser dispatch transient.
Shows current filter mode.
Sibling of `pi-coding-agent--tree-browser-header-line' — both
format the same state variables for different contexts.  Transient
evaluates group descriptions in the invoking browser buffer (see
`transient-with-shadowed-buffer' inside `transient--insert-group'),
so this buffer-local read sees the browser's state on the real
rendering path."
  (format "filter:%s" pi-coding-agent--tree-browser-filter))

(transient-define-prefix pi-coding-agent-tree-browser-dispatch ()
  "Tree browser help."
  [:description pi-coding-agent--tree-dispatch-heading
   ["Actions"
    ("RET" "navigate" pi-coding-agent-tree-browser-navigate)
    ("l" "label" pi-coding-agent-tree-browser-set-label)
    ("g" "refresh" pi-coding-agent-browse-refresh)
    ("q" "quit" quit-window)]
   ["Filter"
    ("f" "filter" pi-coding-agent-tree-browser-cycle-filter)
    ("/" "search" pi-coding-agent-tree-browser-search)]])

;;;; Tree Browser Faces

(defface pi-coding-agent-tree-user
  '((t :inherit font-lock-keyword-face))
  "Face for user messages in the tree browser."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tree-assistant
  '((t :inherit font-lock-string-face))
  "Face for assistant messages in the tree browser."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tree-tool
  '((t :inherit shadow))
  "Face for tool results in the tree browser."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tree-compaction
  '((t :inherit shadow :slant italic))
  "Face for compaction entries in the tree browser."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tree-summary
  '((t :inherit warning))
  "Face for branch summaries in the tree browser."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tree-active
  '((t :weight bold))
  "Face for active-path marker in the tree browser."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tree-label
  '((t :inherit success :weight bold))
  "Face for node labels in the tree browser."
  :group 'pi-coding-agent)

(defface pi-coding-agent-tree-connector
  '((t :inherit shadow))
  "Face for tree connectors (├─, └─, │) in the tree browser."
  :group 'pi-coding-agent)

;;;; Tree Browser Mode

(define-derived-mode pi-coding-agent-tree-browser-mode
  pi-coding-agent-browse-mode "Pi-Tree"
  "Major mode for browsing pi conversation tree.
\\{pi-coding-agent-tree-browser-mode-map}"
  :group 'pi-coding-agent
  (setq-local header-line-format
              '(:eval (pi-coding-agent--tree-browser-header-line)))
  (setq pi-coding-agent--browse-margin-width
        pi-coding-agent--tree-margin-width)
  (setq-local right-margin-width pi-coding-agent--tree-margin-width)
  (add-hook 'window-configuration-change-hook
            #'pi-coding-agent--browse-apply-margins nil t))

;;;; Tree Browser Buffer Management

(defun pi-coding-agent--tree-browser-buffer-name (dir)
  "Return tree browser buffer name for DIR."
  (format "*pi-coding-agent-tree:%s*"
          (pi-coding-agent--route-preserving-abbreviate-file-name dir)))

(defun pi-coding-agent--get-or-create-tree-browser (dir)
  "Get or create tree browser buffer for DIR."
  (let* ((name (pi-coding-agent--tree-browser-buffer-name dir))
         (buf (get-buffer name)))
    (or buf
        (with-current-buffer (generate-new-buffer name)
          (setq default-directory dir)
          (pi-coding-agent-tree-browser-mode)
          (current-buffer)))))

;;;; Tree Node Formatting

(defun pi-coding-agent--tree-node-face (node)
  "Return the face for NODE based on its type and role."
  (let ((type (plist-get node :type))
        (role (plist-get node :role)))
    (pcase type
      ("message"
       (pcase role
         ("user" 'pi-coding-agent-tree-user)
         ("assistant" 'pi-coding-agent-tree-assistant)
         ("branchSummary" 'pi-coding-agent-tree-summary)
         ("compactionSummary" 'pi-coding-agent-tree-compaction)
         (_ 'default)))
      ("tool_result" 'pi-coding-agent-tree-tool)
      ("compaction" 'pi-coding-agent-tree-compaction)
      ("branch_summary" 'pi-coding-agent-tree-summary)
      ("model_change" 'shadow)
      ("thinking_level_change" 'shadow)
      (_ 'default))))

(defun pi-coding-agent--tree-node-type-label (node)
  "Return a short type label for NODE."
  (let ((type (plist-get node :type))
        (role (plist-get node :role)))
    (pcase type
      ("message"
       (pcase role
         ("user" "you")
         ("assistant" "ast")
         ("branchSummary" "sum")
         ("compactionSummary" "cmp")
         ("bashExecution" "sh")
         (_ role)))
      ("tool_result"
       (or (plist-get node :toolName) "tool"))
      ("compaction" "compact")
      ("branch_summary" "summary")
      ("model_change" "model")
      ("thinking_level_change" "think")
      (_ type))))

(defun pi-coding-agent--tree-strip-bracket-preview (node)
  "Return preview text for NODE with bracket wrappers stripped.
The upstream `formatToolCall' wraps previews as `[name: args]'.  Since
the type-label column already identifies the tool, the wrapper is
redundant.  Prefers `formattedToolCall' over `preview'."
  (let ((text (or (plist-get node :formattedToolCall)
                  (plist-get node :preview)
                  "")))
    (cond
     ;; [name: content] → content
     ((string-match "^\\[.+?: \\(.*\\)\\]$" text)
      (match-string 1 text))
     ;; [name] (no args) → empty
     ((string-match "^\\[.+\\]$" text)
      "")
     ;; Plain text → as-is
     (t text))))

(defun pi-coding-agent--tree-node-preview (node)
  "Return preview text for NODE."
  (let ((type (plist-get node :type)))
    (pcase type
      ("compaction"
       (format "compacted (%s tokens)"
               (pi-coding-agent--format-tokens-compact
                (or (plist-get node :tokensBefore) 0))))
      ("branch_summary"
       (pi-coding-agent--first-nonempty-line
        (or (plist-get node :summary) "")))
      ("model_change"
       (format "%s/%s" (plist-get node :provider) (plist-get node :modelId)))
      ("thinking_level_change"
       (or (plist-get node :thinkingLevel) ""))
      ("tool_result"
       (pi-coding-agent--tree-strip-bracket-preview node))
      ("message"
       (if (equal (plist-get node :role) "bashExecution")
           (pi-coding-agent--tree-strip-bracket-preview node)
         (or (plist-get node :preview) "")))
      (_ (or (plist-get node :preview) "")))))

(defun pi-coding-agent--tree-format-node-line (node is-active)
  "Format a single NODE into a display string.
IS-ACTIVE is non-nil if the node is on the active path.
Labels are rendered separately as right-margin overlays."
  (let* ((face (pi-coding-agent--tree-node-face node))
         (type-label (pi-coding-agent--tree-node-type-label node))
         (preview (pi-coding-agent--tree-node-preview node))
         (marker (if is-active
                     (pi-coding-agent--propertize-face
                      "• " 'pi-coding-agent-tree-active)
                   "  "))
         (type-str (pi-coding-agent--propertize-face
                    (format "%-7s" type-label) face))
         (preview-str (pi-coding-agent--propertize-face preview face)))
    (concat marker type-str " " preview-str)))

;;;; Tree Browser Rendering

(defun pi-coding-agent--tree-browser-render (buf)
  "Render the tree browser in BUF from its buffer-local state."
  (with-current-buffer buf
    (let* ((inhibit-read-only t)
           (tree pi-coding-agent--tree-browser-tree)
           (leaf-id pi-coding-agent--tree-browser-leaf-id)
           (filter pi-coding-agent--tree-browser-filter))
      (magit-insert-section (root)
        (cond
         (pi-coding-agent--tree-browser-loading
          (setq pi-coding-agent--tree-browser-visible-count 0)
          (insert (pi-coding-agent--propertize-face
                   "Loading tree..."
                   'pi-coding-agent-activity-phase)
                  "\n"))
         (pi-coding-agent--tree-browser-error
          (setq pi-coding-agent--tree-browser-visible-count 0)
          (insert (pi-coding-agent--propertize-face
                   (format "Error: %s\n" pi-coding-agent--tree-browser-error)
                   'error)))
         ((or (null tree) (= (length tree) 0))
          (insert "No conversation tree.\n"))
         (t
          (let* ((flat (pi-coding-agent--flatten-tree-for-display
                        tree leaf-id filter))
                 (active-ids (pi-coding-agent--active-path-ids tree leaf-id))
                 ;; Apply search filter if active
                 (visible (if pi-coding-agent--tree-browser-search-tokens
                              (cl-remove-if-not
                               (lambda (entry)
                                 (pi-coding-agent--matches-filter-p
                                  (pi-coding-agent--tree-node-preview
                                   (nth 0 entry))
                                  pi-coding-agent--tree-browser-search-tokens))
                               flat)
                            flat)))
            (setq pi-coding-agent--tree-browser-visible-count
                  (length visible))
            (if (null visible)
                (insert "No matching entries.\n")
              (dolist (entry visible)
                (let* ((node (nth 0 entry))
                       (prefix (nth 2 entry))
                       (node-id (plist-get node :id))
                       (is-active (gethash node-id active-ids))
                       (prefix-str (pi-coding-agent--propertize-face
                                    prefix
                                    'pi-coding-agent-tree-connector))
                       (line (pi-coding-agent--tree-format-node-line
                              node is-active)))
                  (magit-insert-section (tree-node node-id)
                    (magit-insert-heading
                      (concat prefix-str line))
                    (when-let* ((label (plist-get node :label)))
                      ;; 3 = "[" + "]" + 1 char padding
                      (let ((truncated
                             (pi-coding-agent--truncate-string
                              label
                              (- pi-coding-agent--tree-margin-width 3))))
                        (pi-coding-agent--make-margin-overlay
                         (pi-coding-agent--propertize-face
                          (format "[%s]" truncated)
                          'pi-coding-agent-tree-label)))))))))))))))

;;;; Tree Browser Header-Line

(defun pi-coding-agent--tree-browser-header-line ()
  "Return header-line string for the tree browser.
Uses cached visible count from the last render to avoid redundant
tree flattening on every redisplay cycle."
  (let* ((filter pi-coding-agent--tree-browser-filter)
         (query pi-coding-agent--tree-browser-search-query)
         (total pi-coding-agent--tree-browser-visible-count))
    (mapconcat #'identity
               (append (list (format "Tree [%s]" filter)
                             (format "(%d)" total))
                       (and query (list (format "/%s" query)))
                       (list (pi-coding-agent--propertize-face "?:help" 'shadow)))
               " │ ")))

;;;; Tree Browser Interactive Commands

(defun pi-coding-agent-tree-browser-cycle-filter ()
  "Cycle the tree browser filter mode."
  (interactive)
  (let* ((modes pi-coding-agent--tree-filter-modes)
         (current pi-coding-agent--tree-browser-filter)
         (next (or (cadr (member current modes)) (car modes))))
    (setq pi-coding-agent--tree-browser-filter next)
    (pi-coding-agent--tree-browser-rerender)
    (message "Pi: Filter: %s" next)))

(defun pi-coding-agent-tree-browser-search ()
  "Set or clear search filter in the tree browser."
  (interactive)
  (let ((query (read-string "Filter (regexp tokens): "
                            pi-coding-agent--tree-browser-search-query))
        (need-rerender t))
    (if (string-empty-p query)
        (setq pi-coding-agent--tree-browser-search-query nil
              pi-coding-agent--tree-browser-search-tokens nil)
      (condition-case err
          (let ((tokens (split-string query)))
            (dolist (tok tokens)
              (string-match-p tok ""))
            (setq pi-coding-agent--tree-browser-search-query query
                  pi-coding-agent--tree-browser-search-tokens tokens))
        (invalid-regexp
         (message "Pi: Invalid regexp: %s" (error-message-string err))
         (setq need-rerender nil))))
    (when need-rerender
      (pi-coding-agent--tree-browser-rerender))))

(defun pi-coding-agent-tree-browser-navigate ()
  "Navigate to the tree node at point."
  (interactive)
  (if-let* ((section (magit-current-section))
            (node-id (oref section value)))
      (pi-coding-agent--browse-navigate node-id)
    (message "Pi: No tree node at point")))

(defun pi-coding-agent-tree-browser-set-label ()
  "Set or clear a label on the tree node at point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (node-id (oref section value)))
    (let* ((current-label (when pi-coding-agent--tree-browser-tree
                            (pi-coding-agent--tree-find-label
                             pi-coding-agent--tree-browser-tree node-id)))
           (new-label (read-string
                       (if current-label
                           (format "Label (current: %s, empty to clear): "
                                   current-label)
                         "Label: ")
                       current-label))
           (label (if (string-empty-p (string-trim new-label)) nil new-label)))
      (pi-coding-agent--browse-set-label node-id label))))

(defun pi-coding-agent--tree-find-label (tree node-id)
  "Find the label for NODE-ID in TREE.
Returns the label string or nil."
  (let ((stack (append tree nil))
        (result nil))
    (while (and stack (not result))
      (let* ((node (pop stack))
             (children (plist-get node :children)))
        (when (equal (plist-get node :id) node-id)
          (setq result (plist-get node :label)))
        (when (vectorp children)
          (dotimes (i (length children))
            (push (aref children i) stack)))))
    result))

(defun pi-coding-agent--tree-find-node (tree node-id)
  "Find the projected node with :id NODE-ID in TREE; nil when absent.
Iterative pre-order walk; used by
`pi-coding-agent--browse-navigate-message' for the success preview."
  (let ((stack (append tree nil))
        (found nil))
    (while (and stack (not found))
      (let ((node (pop stack)))
        (if (equal (plist-get node :id) node-id)
            (setq found node)
          (setq stack (append (append (plist-get node :children) nil)
                              stack)))))
    found))

(defun pi-coding-agent--tree-node-with-label (node label)
  "Return a copy of projected NODE carrying :label LABEL, or no label.
LABEL nil removes the label pair entirely — the load-time fold treats
an absent label as cleared, and projection only emits the pair when a
label is set, so a patched node stays shape-identical to a fresh read.
Only NODE's own plist is copied; the :children vector is shared as-is,
keeping the copy O(1) in tree size — rebuilding the spine above NODE
is `pi-coding-agent--tree-apply-label's business.  A newly set :label
pair goes right after :timestamp, the canonical projected position."
  (let ((out nil)
        (replaced nil))
    (while (consp node)
      (let ((key (pop node)))
        (if (eq key :label)
            ;; Drop the old pair wherever it sits.
            (when (consp node) (pop node))
          (setq out (nconc out
                           (list key (if (consp node) (pop node) nil))))
          (when (and (eq key :timestamp) label)
            (setq out (nconc out (list :label label))
                  replaced t)))))
    (if (or replaced (not label))
        out
      ;; No :timestamp anchor (not a projected node): append at the end.
      (nconc out (list :label label)))))

(defun pi-coding-agent--tree-apply-label (tree node-id label)
  "Patch TREE so the projected node with NODE-ID carries LABEL, or none.
Return a fresh tree vector: the root-to-node spine is rebuilt with
fresh plists and child vectors, with the patched node
\\(`pi-coding-agent--tree-node-with-label'\\) `aset' into each container,
while every unvisited subtree is shared — the patch costs O(path
length), not O(tree size).  Return nil when NODE-ID is not in TREE.
Both the search and the rebuild are iterative, so deep chains cannot
overflow the Lisp stack."
  (when (vectorp tree)
    (let* ((root-frame (cons tree 0))
           (stack (list root-frame))
           ;; PATH holds the reversed (CONTAINER . INDEX) frames of the
           ;; current DFS chain; once the target is found it is the
           ;; deepest-first root-to-node path.
           (path nil)
           (found nil))
      (catch 'exited
        (while stack
          (let* ((frame (car stack))
                 (vec (car frame))
                 (idx (cdr frame)))
            (if (>= idx (length vec))
                (progn
                  (pop stack)
                  ;; This frame's owner node is done; leave its path
                  ;; entry too (the root frame has no owner above it).
                  (unless (eq frame root-frame) (pop path)))
              (setcdr frame (1+ idx))
              (let* ((node (aref vec idx))
                     (children (plist-get node :children))
                     (descend (and (vectorp children)
                                   (> (length children) 0))))
                (push (cons vec idx) path)
                (when (equal (plist-get node :id) node-id)
                  (setq found t)
                  (throw 'exited t))
                (if descend
                    (push (cons children 0) stack)
                  (pop path))))))
        nil)
      (when found
        (let* ((target-frame (car path))
               (target (aref (car target-frame) (cdr target-frame)))
               (patched (pi-coding-agent--tree-node-with-label target label))
               (result nil)
               (frames path))
          (while frames
            (let* ((frame (car frames))
                   (vec (copy-sequence (car frame))))
              (aset vec (cdr frame) patched)
              (if (cdr frames)
                  ;; The node at the next frame up owns VEC as its
                  ;; :children: hand it a fresh plist pointing there.
                  (let* ((owner-frame (cadr frames))
                         (owner (aref (car owner-frame)
                                      (cdr owner-frame))))
                    (setq patched (plist-put (copy-sequence owner)
                                             :children vec)))
                (setq result vec)))
            (setq frames (cdr frames)))
          result)))))

;;;; Disk-Backed Data Layer Seams

(defun pi-coding-agent--browse-current-session-directory ()
  "Return the session directory for the \"current\" scope, or nil.
Resolution order: the optional menu-supplied session list directory
when menu.el is loaded, then the munged stable session directory of the
linked chat buffer — rooted on that directory's own host when remote —
then the munged project directory; the last works with no session at
all.  Signals when resolution itself fails."
  (or (and (fboundp 'pi-coding-agent--session-list-directory)
           (pi-coding-agent--session-list-directory))
      (when-let* ((chat-buf pi-coding-agent--chat-buffer))
        (and (buffer-live-p chat-buf)
             (let ((cwd (pi-coding-agent--chat-session-directory chat-buf)))
               (pi-coding-agent-jsonl-session-dir-for-cwd
                cwd (pi-coding-agent-jsonl-sessions-root cwd)))))
      (pi-coding-agent-jsonl-session-dir-for-cwd
       (pi-coding-agent--session-directory))))

(defun pi-coding-agent--browse-session-directories (scope)
  "Return the list of session directories to scan for SCOPE.
\"current\" is the single current-project directory (see
`pi-coding-agent--browse-current-session-directory').  \"all\" is
every root-level munged --…-- directory under the sessions root —
remote-anchored when a current directory is known — so .subagents
sidecars and non-munged directories are excluded by construction.
Missing roots read as empty; signals when resolution itself fails."
  (if (not (equal scope "all"))
      (list (pi-coding-agent--browse-current-session-directory))
    (let* ((cur (pi-coding-agent--browse-current-session-directory))
           (root (if cur
                     (pi-coding-agent-jsonl-sessions-root
                      (file-name-as-directory cur))
                   (pi-coding-agent-jsonl-sessions-root))))
      (delq nil
            (mapcar (lambda (dir)
                      (and (file-directory-p dir) dir))
                    (condition-case nil
                        (directory-files root t "\\`--")
                      (error nil)))))))

(defun pi-coding-agent--browse-session-files (dirs)
  "Return every \\.jsonl file directly inside DIRS, in listing order.
Unreadable or missing directories are skipped silently (empty)."
  (apply #'append
         (mapcar (lambda (dir)
                   (condition-case nil
                       (directory-files dir t "\\.jsonl\\'")
                     (error nil)))
                 dirs)))

(defun pi-coding-agent--browse-scan-session-files (buf token files items callback)
  "Scan FILES for sessions in 25 ms time slices, then call CALLBACK once.
Each slice processes whole files until 25 ms elapse (a single huge file
is atomic — one ~0.2 s hiccup is possible, documented), then defers
the rest to the next slice via `(run-at-time 0 nil ...)`.  The FIRST
slice is scheduled the same way, so a superseding fetch drops an older
scan before any slice runs and the callback is uniformly asynchronous.
A slice is dropped when BUF died or a newer fetch bumped the fetch
TOKEN, and a dropped scan never calls CALLBACK.  ITEMS accumulates the
session plists (already in the browse dialect: the scan is an identity
mapping over `pi-coding-agent-jsonl-read-session-info' output).

The exactly-once contract also holds when a slice is interrupted: the
slice loop runs inside a `condition-case' with explicit `quit' and
`error' handlers, so a `quit' during a slice (C-g against a slow
scan) or an `error' abandons the scan and reports through CALLBACK
once with the failure string — `quit' is not an `error', so without
its own handler the callback would never run and the browser would
sit on its loading state forever (same contract as the deferred read
in `pi-coding-agent--browse-load-tree')."
  (if (and (buffer-live-p buf)
           (eq token (buffer-local-value
                      'pi-coding-agent--session-browser-fetch-token buf)))
      (let ((deadline (+ (float-time) 0.025))
            (failure nil)
            (finished nil))
        ;; CALLBACK is invoked only below, OUTSIDE the condition-case:
        ;; a signaling callback must not re-enter a handler and report
        ;; twice.
        (condition-case err
            (progn
              (while (and files (< (float-time) deadline))
                (let ((info (pi-coding-agent-jsonl-read-session-info
                             (car files))))
                  (when info (push info items)))
                (setq files (cdr files)))
              (if files
                  (run-at-time 0 nil #'pi-coding-agent--browse-scan-session-files
                               buf token files items callback)
                (setq finished t)))
          (quit
           (setq failure "Session scan was interrupted"))
          (error
           (setq failure (format "Session scan failed: %s"
                                 (error-message-string err)))))
        (cond (failure
               ;; Same one-shot report a failed fetch uses: nil items
               ;; plus the error string (see
               ;; `pi-coding-agent--browse-load-sessions').
               (funcall callback nil failure))
              (finished
               (funcall callback (nreverse items) nil))))
    ;; Stale or orphaned fetch: drop silently.
    nil))

(defun pi-coding-agent--browse-load-sessions (scope callback)
  "Load session items for SCOPE, then call CALLBACK with (ITEMS ERROR).
ITEMS is a list of session plists in the browse session dialect:
\(:path :id :cwd :name? :parentSessionPath? :created :modified
:messageCount :firstMessage) — the identity over
`pi-coding-agent-jsonl-read-session-info' output.  ERROR is an error
string or nil.  SCOPE is \"current\" (one project directory) or \"all\" (every
munged directory under the sessions root).  The scan is
chunked (see `pi-coding-agent--browse-scan-session-files'), shows a
loading state throughout, and reports exactly once; a superseded
fetch's callback is dropped by the fetch token — a superseding fetch
cancels an older one before any slice runs.  Directory resolution
failures surface synchronously as the ERROR string \"Cannot list
sessions: …\"."
  (let ((buf (current-buffer)))
    (setq pi-coding-agent--session-browser-fetch-token
          (1+ pi-coding-agent--session-browser-fetch-token))
    (let ((token pi-coding-agent--session-browser-fetch-token))
      (let ((dirs nil)
            (failure nil))
        (condition-case err
            (setq dirs (pi-coding-agent--browse-session-directories scope))
          (error
           (setq failure (format "Cannot list sessions: %s"
                                (error-message-string err)))))
        (if failure
            (funcall callback nil failure)
          (run-at-time 0 nil #'pi-coding-agent--browse-scan-session-files
                       buf token
                       (pi-coding-agent--browse-session-files dirs)
                       nil callback))))))

(defun pi-coding-agent--tree-browser-chat-session-file ()
  "Return the linked chat buffer's current session file, or nil.
A live `pi-coding-agent--chat-buffer' link supplies the normalized
:session-file from its state plist (populated at startup via get_state
--apply-state-response and TRAMP-prefixed for Emacs).  The file persists
after process death, so tree fetches and labels keep working with no
live pi process.  Nil covers both a dead link and a chat whose session
file does not exist yet (it is created on the first assistant reply)."
  (when-let* ((chat-buf pi-coding-agent--chat-buffer))
    (when (buffer-live-p chat-buf)
      (with-current-buffer chat-buf
        (when (plistp pi-coding-agent--state)
          (pi-coding-agent--normalize-string-or-null
           (plist-get pi-coding-agent--state :session-file)))))))

(defun pi-coding-agent--browse-load-tree (callback)
  "Load the conversation tree, then call CALLBACK with (TREE LEAF-ID MESSAGE).
TREE is a vector of projected root nodes in the browse node dialect,
LEAF-ID the projected leaf entry id, and MESSAGE an error string or
nil on success — the same shape as the session side's (ITEMS ERROR)
callback.  The third MESSAGE argument lets callers render precise
error states instead of a generic \"no tree\".

The tree comes from the linked chat's session file on DISK —
`pi-coding-agent-jsonl-project-session-file' — never an RPC get_tree.
Labels appended out-of-band fold back on every disk read, navigation
rewrites this same file, and no process is needed — the state
:session-file key persists after process death.  The tree shows
the last PERSISTED turn, so it lags an in-flight turn; refresh
manually with \\[pi-coding-agent-browse-refresh].

The buffer's fetch token is bumped here (mirroring
`pi-coding-agent--browse-load-sessions'); then a missing chat link
reports the link error and a link with no session file yet — or one
naming a path that does not exist — reports the not-yet error, both
synchronously.  Otherwise the read itself is deferred through
`(run-at-time 0 ...)' so the caller's forced redisplay can paint the
loading state first (see `pi-coding-agent--tree-browser-fetch-and-render':
Emacs runs due 0-timers before redisplaying, so without the forced
paint a single timer hop starves the loading render entirely); then
it runs as one blocking read (a 53 MB worst-case session reads in
~0.6 s, typical files in tens of milliseconds; chunking would
complicate the pure jsonl reader for no UI gain).  A deferred read
drops itself when a newer fetch has bumped the token, and is wrapped
in `condition-case': a nil or garbage read reports MESSAGE \"Session
file is unreadable or not a pi session file: PATH\" instead of
signaling, and a `quit' during the blocking read (C-g against a huge
file) reports \"Session file read was interrupted: PATH\" — `quit'
is not an `error', so without its own handler the callback would
never run and the browser would sit on its loading state forever."
  (let ((buf (current-buffer)))
    (setq pi-coding-agent--tree-browser-fetch-token
          (1+ pi-coding-agent--tree-browser-fetch-token))
    (let ((token pi-coding-agent--tree-browser-fetch-token)
          (linked (and pi-coding-agent--chat-buffer
                       (buffer-live-p pi-coding-agent--chat-buffer)))
          (path (pi-coding-agent--tree-browser-chat-session-file)))
      (cond
       ((not linked)
        (funcall callback nil nil
                 "No linked pi chat session (open the tree browser from a pi chat)"))
       ((or (not path) (not (file-exists-p path)))
        (funcall callback nil nil
                 "No session file yet — it is created on the first assistant reply"))
       (t
        (run-at-time
         0 nil
         (lambda ()
           (when (and (buffer-live-p buf)
                      (eq token (buffer-local-value
                                 'pi-coding-agent--tree-browser-fetch-token
                                 buf)))
             (let ((tree nil)
                   (leaf-id nil)
                   (failure nil))
               (condition-case nil
                   (let ((result (pi-coding-agent-jsonl-project-session-file
                                  path)))
                     (if result
                         (setq tree (plist-get result :tree)
                               leaf-id (plist-get result :leafId))
                       (setq failure
                             (format "Session file is unreadable or not a pi session file: %s"
                                     path))))
                 (quit
                  (setq failure
                        (format "Session file read was interrupted: %s" path)))
                 (error
                  (setq failure
                        (format "Session file is unreadable or not a pi session file: %s"
                                path))))
               (funcall callback tree leaf-id failure))))))))))

(defun pi-coding-agent--browse-session-file-matches-p (chat-buf path)
  "Return non-nil when CHAT-BUF's current session file is PATH.
Compares `expand-file-name' spellings (anchored at CHAT-BUF so relative
session files resolve like the chat does) with `file-equal-p' as the
symlink-aware fallback."
  (and (buffer-live-p chat-buf)
       (with-current-buffer chat-buf
         (let ((current (plist-get pi-coding-agent--state :session-file)))
           (and (stringp current)
                (or (equal (expand-file-name current)
                           (expand-file-name path))
                    (file-equal-p current path)))))))

(defun pi-coding-agent--browse-transition-refused-p (chat-buf action)
  "Return non-nil when CHAT-BUF must refuse to start ACTION now.
One gate for both halves of the transition guard, shared by
`pi-coding-agent--browse-switch-session' and
`pi-coding-agent--browse-navigate': an ACTIVE session transition
refuses with \"Pi: Cannot ACTION while switching sessions\" — the
status stays idle during the transition latch, so
`pi-coding-agent--session-transition-ready-p' cannot see it, and a
second concurrent switch or navigate would race the first — and
otherwise the ready guard runs with ACTION (reporting its own refusal
when it returns nil)."
  (if (pi-coding-agent--session-transition-active-p chat-buf)
      (progn
        (message "Pi: Cannot %s while switching sessions" action)
        t)
    (not (pi-coding-agent--session-transition-ready-p chat-buf action))))

(defun pi-coding-agent--browse-switch-session (path)
  "Switch the linked chat session to session file PATH.
Guards, in order: a live linked chat buffer (else `user-error'), a live
pi process (else `user-error'), and no session transition in flight —
`pi-coding-agent--browse-transition-refused-p' gates both an active
transition, which keeps the status idle (without the explicit check a
second RET would race the first switch), and the ready guard, which
reports its own refusal and returns quietly.  Delegation is
`pi-coding-agent--resume-selected-session' (PROC CHAT-BUF PATH); its
synchronous `user-error's (bad cwd, duplicate open) surface in the
browser.  Afterwards `pi-coding-agent--browse-quit-when-settled' waits
out the transition and dismisses the browser window only when the chat
landed on PATH; a failed switch leaves the browser open (menu already
messaged).  No extra refresh logic: \\[pi-coding-agent-browse-refresh]
re-derives the \"current\" scope live and the entry point opens a fresh
browser per project directory."
  (let ((chat-buf pi-coding-agent--chat-buffer))
    (unless (and chat-buf (buffer-live-p chat-buf))
      (user-error "No pi session to switch to"))
    (let ((proc (buffer-local-value 'pi-coding-agent--process chat-buf)))
      (unless (pi-coding-agent--session-live-process-p proc)
        (user-error "Pi process is not running"))
      (unless (pi-coding-agent--browse-transition-refused-p chat-buf "switch")
        (pi-coding-agent--resume-selected-session proc chat-buf path)
        (pi-coding-agent--browse-quit-when-settled
         chat-buf (selected-window) path)))))

(defun pi-coding-agent--browse-poll-settled (chat-buf win path deadline)
  "Polling body of `pi-coding-agent--browse-quit-when-settled'.
CHAT-BUF, WIN, and PATH are the parent's arguments; give up silently
once DEADLINE (an absolute time) has passed.  A dead CHAT-BUF also ends
the poll quietly — there is nothing left to wait for, and probing a
killed buffer would signal inside the timer."
  (if (not (buffer-live-p chat-buf))
      nil
    (if (pi-coding-agent--session-transition-active-p chat-buf)
        (when (time-less-p (current-time) deadline)
          (run-at-time 0.05 nil #'pi-coding-agent--browse-poll-settled
                       chat-buf win path deadline))
      (when (and (pi-coding-agent--browse-session-file-matches-p chat-buf path)
                 (window-live-p win)
                 (with-current-buffer (window-buffer win)
                   (derived-mode-p 'pi-coding-agent-session-browser-mode
                                   'pi-coding-agent-tree-browser-mode)))
        (quit-window nil win)))))

(defun pi-coding-agent--browse-quit-when-settled (chat-buf win path)
  "Wait out CHAT-BUF's session transition, then dismiss the browser window.
Polls `pi-coding-agent--session-transition-active-p' every 0.05 s with
a 30 s timeout.  Once settled, WIN is quit ONLY when the chat state's
:session-file matches PATH — a failed switch already messaged via the
menu, so a mismatch silently leaves the browser open — and only when
WIN still shows a pi browse buffer (session OR tree browser, via
`derived-mode-p'): a dead or repurposed window (the buffer was killed
mid-poll) is left alone so `quit-window' can never close whatever
replaced it."
  (pi-coding-agent--browse-poll-settled
   chat-buf win path (time-add (current-time) 30)))

(defun pi-coding-agent--browse-navigate (node-id)
  "Move the live conversation onto tree node NODE-ID.
Guard → rewrite → switch → reload → prefill, mirroring pi's
navigateTree without a navigate RPC:

 1. a live linked chat buffer, else `user-error' \"No pi session to
    navigate\";
 2. the loaded-file guard (as `pi-coding-agent--browse-set-label'):
    a fresh session-file resolution that is nil messages \"Pi: Cannot
    navigate: no session file\", one that disagrees with
    `pi-coding-agent--tree-browser-loaded-file' messages \"Pi: Session
    changed since the tree was loaded — refresh with g\" (the rewrite
    would have to pick one of two files);
 3. a live pi process, else `user-error' \"Pi process is not running\";
 4. no in-flight session transition (the first half of
    `pi-coding-agent--browse-transition-refused-p') —
    `--session-transition-ready-p' cannot see one (status stays idle
    during the latch), and a second RET during a switch would race it;
 5. `pi-coding-agent--session-transition-ready-p' with the action
    \"navigate\" (the second half; reports its own refusal);
 6. a FRESH `pi-coding-agent-jsonl-read-file' — the browser's cached
    tree can lag the file — else the unreadable message;
 7. a versioned header: version 1 files (no ids) refuse with the
    migrate hint;
 8. `pi-coding-agent-jsonl-navigation-target': unknown node ids refuse
    with the refresh hint; a nil :leaf-id (the root user message has
    no parent to rewind to) refuses with the fork hint — the chat's
    fork command does that job;
 9. :current-p is the two-way no-op: without :prefill just message
    \"Pi: Already at current position\"; with :prefill (re-editing a
    prompt the file already sits on) prefill, message, and settle —
    no write, no switch in either case;
10. the resume cwd pre-flight (`--session-file-cwd-or-error') runs
    BEFORE any write so its `user-error's surface before the file
    changes;
11. the local atomic rewrite (`--browse-rewrite-session-file') — the
    closing rename is the ONLY call that touches the session file;
    pre-commit local failure messages and stops byte-identically;
12. `pi-coding-agent--resume-selected-session' (PROC CHAT-BUF PATH) —
    a same-path switch is legal, so the switch rides the normal
    choreography including the transition latch and history reload;
13. the input prefill runs immediately after the resume RPC is
    scheduled (the latch blocks sending until the switch settles),
    against the input buffer captured from the chat BEFORE the RPC;
14. \"Pi: Navigated to PREVIEW\" from the cached tree
    (`--tree-find-node', `--tree-node-preview', truncated to 60;
    \"Pi: Navigated\" without a preview), then
    `pi-coding-agent--browse-quit-when-settled';
15. no auto-reopen of the browser — refresh with `g'.

On ordinary local files this guard/read/rewrite path is synchronous and
does not yield back to Emacs, so only an independent writer can stale
the file between checks.  The ready guard idles the linked pi process
but cannot exclude another pi instance or external writer; such a
writer between the authoritative line read and rename can lose its
change.  TRAMP file handlers may yield, and their rename need not be
atomic.  These residual risks are accepted here rather than inventing
cross-module writer coordination."
  (let ((chat-buf pi-coding-agent--chat-buffer))
    (unless (and chat-buf (buffer-live-p chat-buf))
      (user-error "No pi session to navigate"))
    ;; The input buffer is captured BEFORE the RPC: the chat may retarget
    ;; buffers during the switch (step 12).
    (let ((input-buf (buffer-local-value 'pi-coding-agent--input-buffer
                                         chat-buf))
          (path (pi-coding-agent--tree-browser-chat-session-file)))
      (cond
       ((null path)
        (message "Pi: Cannot navigate: no session file"))
       ((not (equal pi-coding-agent--tree-browser-loaded-file path))
        (message "Pi: Session changed since the tree was loaded — refresh with g"))
       (t
        (let ((proc (buffer-local-value 'pi-coding-agent--process chat-buf)))
          (unless (pi-coding-agent--session-live-process-p proc)
            (user-error "Pi process is not running"))
          (cond
           ((pi-coding-agent--browse-transition-refused-p chat-buf "navigate")
            nil)
           (t
            (let ((session (pi-coding-agent-jsonl-read-file path)))
              (cond
               ((null session)
                (message
                 "Pi: Cannot navigate: session file is unreadable or not a pi session file: %s"
                 path))
               ((not (plist-get (plist-get session :header) :version))
                (message
                 "Pi: Session file uses an old format; open it with pi once to migrate, then refresh with g"))
               (t
                (let ((target (pi-coding-agent-jsonl-navigation-target
                               session node-id)))
                  (cond
                   ((null target)
                    (message "Pi: No such tree node — refresh with g"))
                   ((null (plist-get target :leaf-id))
                    (message
                     "Pi: Cannot rewind before the first message; fork it from the chat instead"))
                   ((plist-get target :current-p)
                    (if (not (plist-get target :prefill))
                        (message "Pi: Already at current position")
                      (pi-coding-agent--browse-prefill-input
                       input-buf (plist-get target :prefill))
                      (pi-coding-agent--browse-navigate-message node-id)
                      (pi-coding-agent--browse-quit-when-settled
                       chat-buf (selected-window) path)))
                   (t
                    (condition-case err
                        (pi-coding-agent--session-file-cwd-or-error path)
                      ;; Re-signal the guard's own wording before any
                      ;; write happens.
                      (user-error (signal (car err) (cdr err))))
                    (let ((lines (pi-coding-agent-jsonl-navigation-lines
                                  path (plist-get target :leaf-id))))
                      (if (null lines)
                          (message
                           "Pi: Cannot navigate: session file is unreadable or not a pi session file: %s"
                           path)
                        (when (pi-coding-agent--browse-rewrite-session-file
                               path lines)
                          (pi-coding-agent--resume-selected-session
                           proc chat-buf path)
                          (pi-coding-agent--browse-prefill-input
                           input-buf (plist-get target :prefill))
                          (pi-coding-agent--browse-navigate-message node-id)
                          (pi-coding-agent--browse-quit-when-settled
                           chat-buf (selected-window) path))))))))))))))))))

(defun pi-coding-agent--browse-navigate-message (node-id)
  "Message the navigate success for NODE-ID from the cached tree.
The preview comes from `pi-coding-agent--tree-find-node' and
`pi-coding-agent--tree-node-preview' over the browser's cached tree
with no refetch, truncated to 60; without a preview the bare
\"Pi: Navigated\"."
  (let* ((node (when (vectorp pi-coding-agent--tree-browser-tree)
                 (pi-coding-agent--tree-find-node
                  pi-coding-agent--tree-browser-tree node-id)))
         (preview (if node (pi-coding-agent--tree-node-preview node) "")))
    (if (and (stringp preview) (not (string-empty-p preview)))
        (message "Pi: Navigated to %s"
                 (pi-coding-agent--truncate-string preview 60))
      (message "Pi: Navigated"))))

(defun pi-coding-agent--browse-rewrite-session-file (path lines)
  "Atomically replace the session file at PATH with LINES.
LINES is the `pi-coding-agent-jsonl-navigation-lines' vector; the
joined bytes gain one final LF.  The ONLY call that touches PATH is
the closing `rename-file': bytes land without coding or end-of-line
conversion in a sibling temp file (`.pi-nav-…' in PATH's directory)
that carries PATH's modes best-effort, then replace PATH.  On ordinary
local files the sibling is on the same filesystem and rename is the
atomic commit; pi holds no persistent descriptor on session files, so
its next append/read opens the replacement.  On TRAMP a handler may
degrade rename to copy+delete: replacement can be visible in pieces
and a remote failure cannot promise a byte-identical original.  This
is accepted because in-place writing is strictly worse.

The rewrite reorders complete raw lines only.  It always adds one final
LF; CR bytes returned for CRLF lines remain in place, so an all-CRLF
file with its final delimiter stays all-CRLF.  A file missing its
trailing newline gains LF (or CRLF when its final raw line ends in CR).
The ready guard only idles the linked pi process: an independent
append/change between the
fresh line read and local rename can still be lost.  `unwind-protect'
removes the temp on an error or quit before commit.  Thus on ordinary
local files pre-commit failures leave PATH byte-identical and no temp
behind; errors report \"Pi: Navigate failed: …\" and return nil, while quits
propagate after cleanup.  Success returns non-nil."
  (let* ((contents (concat (mapconcat #'identity (append lines nil) "\n")
                           "\n"))
         (tmp (make-temp-name
               (concat (file-name-directory path) ".pi-nav-")))
         (swapped nil))
    (condition-case err
        (unwind-protect
            (progn
              (let ((coding-system-for-write 'no-conversion))
                ;; VISIT 0: no "Wrote file" message, no lockfile — and
                ;; the target is TMP, never PATH.  LINES are unibyte raw
                ;; file lines, so no coding or EOL conversion is allowed.
                (write-region contents nil tmp nil 0))
              (ignore-errors
                (set-file-modes tmp (file-modes path)))
              (rename-file tmp path t)
              (setq swapped t))
          (unless swapped
            (ignore-errors (delete-file tmp))))
      (error
       (message "Pi: Navigate failed: %s" (error-message-string err))
       nil))))

(defun pi-coding-agent--browse-prefill-input (input-buf text)
  "Replace INPUT-BUF's draft with TEXT; nil TEXT still erases.
Erasing unsent input is deliberate (the fork command's precedent):
the navigate prefill replaces whatever draft was in flight.  Runs
immediately after the resume RPC is scheduled — the transition latch
blocks sending until the switch settles, so the text cannot leak into
the outgoing session.  Failures are non-fatal."
  (when (buffer-live-p input-buf)
    (condition-case err
        (with-current-buffer input-buf
          (erase-buffer)
          (when text (insert text)))
      (error
       (message "Pi: Failed to prefill prompt - %s"
                (error-message-string err))))))

(defun pi-coding-agent--browse-set-label (node-id label)
  "Set LABEL (string, or nil to clear) on tree node NODE-ID.
Appends a `label' entry to the linked chat's session file out-of-band
via `pi-coding-agent--browse-append-session-entry' — pi's
appendLabelChange shape, with the :label key omitted entirely on a
clear (the load-time fold treats an absent or empty label as
cleared).  The append also makes the label entry the file's new raw
leaf; the next fetch's projected leaf still resolves up to the last
visible entry, so the active path does not move.  Instead of
re-reading the file, the cached projected tree is patched locally
\\(`pi-coding-agent--tree-apply-label'\\) and re-rendered: section
identity is the node id, which labeling never changes, so point
survives.  Report \"Pi: Label set to LABEL\" or \"Pi: Label cleared\".

Guards: no resolvable session file messages \"Pi: Cannot label: no
session file\" (nothing is written anywhere — session files are
append-only and must never be created here); a fresh resolution that
disagrees with `pi-coding-agent--tree-browser-loaded-file' (the chat
switched sessions behind the browser's back) messages \"Pi: Session
changed since the tree was loaded — refresh with g\" and appends
nothing — writing into the old file would still be harmless for pi (a
benign sibling), but the browser would then show a label its tree no
longer reflects."
  (let ((path (pi-coding-agent--tree-browser-chat-session-file)))
    (cond
     ((not path)
      (message "Pi: Cannot label: no session file"))
     ((not (equal pi-coding-agent--tree-browser-loaded-file path))
      (message "Pi: Session changed since the tree was loaded — refresh with g"))
     (t
      (when (pi-coding-agent--browse-append-session-entry
             path "label"
             (append (list :targetId node-id)
                     (when label (list :label label)))
             "label")
        (setq pi-coding-agent--tree-browser-tree
              (or (pi-coding-agent--tree-apply-label
                   pi-coding-agent--tree-browser-tree node-id label)
                  pi-coding-agent--tree-browser-tree))
        (pi-coding-agent--tree-browser-rerender)
        (if label
            (message "Pi: Label set to %s" label)
          (message "Pi: Label cleared")))))))

;;;; Tree Browser Fetch and Render

(defun pi-coding-agent--tree-browser-fetch-and-render ()
  "Fetch tree and re-render the tree browser.
The tree is read from the linked chat's session file on disk, so no
live pi process is required.  The callback reports (TREE LEAF-ID
MESSAGE): a non-nil MESSAGE renders as an error state with a zero
visible count.  On success, `pi-coding-agent--tree-browser-loaded-file'
records the file resolved before the deferred read, so a chat session
switch mid-read leaves the label and navigation guards comparing
against the tree actually displayed; it is nil on error states.

The point anchor is captured before the loading-state render — that
render destroys the node sections, so without carrying the anchor
across the cycle the final render would find nothing under point to
restore (see `pi-coding-agent--browse-rerender-preserving-point').
A refresh issued while another fetch is still loading finds no
sections to capture and reuses the in-flight cycle's anchor (see
`pi-coding-agent--tree-browser-fetch-anchor').

The loading state is painted EXPLICITLY: the deferred read is a
single `(run-at-time 0 ...)' hop, and Emacs runs due 0-timers before
redisplaying, so without the forced `redisplay' here the loading
render would never become visible (the chunked session scan yields
to redisplay between its slices; one timer hop never does)."
  (let* ((buf (current-buffer))
         (loaded (pi-coding-agent--tree-browser-chat-session-file))
         (anchor (or (pi-coding-agent--browse-capture-point-anchor)
                     ;; Mid-flight refresh: the loading render already
                     ;; destroyed the sections under point, so carry
                     ;; the anchor the in-flight cycle captured.
                     (and pi-coding-agent--tree-browser-loading
                          pi-coding-agent--tree-browser-fetch-anchor))))
    (setq pi-coding-agent--tree-browser-loading t
          pi-coding-agent--tree-browser-fetch-anchor anchor)
    ;; Loading-state render: default point behavior (nothing to keep).
    (pi-coding-agent--tree-browser-rerender)
    ;; Paint it before scheduling the read — see docstring.
    (redisplay)
    (pi-coding-agent--browse-load-tree
     (lambda (tree leaf-id message)
       (when (buffer-live-p buf)
         ;; The load calls back from a timer in whatever buffer is
         ;; current; render in the browser buffer, not that one.
         (with-current-buffer buf
           (setq pi-coding-agent--tree-browser-loading nil
                 pi-coding-agent--tree-browser-fetch-anchor nil
                 pi-coding-agent--tree-browser-error message
                 pi-coding-agent--tree-browser-tree tree
                 pi-coding-agent--tree-browser-leaf-id leaf-id
                 pi-coding-agent--tree-browser-loaded-file
                 (and (not message) loaded))
           (pi-coding-agent--tree-browser-rerender anchor)))))))

(defun pi-coding-agent--tree-browser-rerender (&optional fallback)
  "Re-render the tree browser from local state, preserving point.
FALLBACK is a pre-fetch `(IDENT . OFFSET)' anchor handed to
`pi-coding-agent--browse-rerender-preserving-point' for the fetch
cycle's final render."
  (pi-coding-agent--browse-rerender-preserving-point
   (current-buffer) #'pi-coding-agent--tree-browser-render fallback))

;;;; Tree Browser Refresh Integration

(defun pi-coding-agent-browse-refresh ()
  "Refresh the current browse buffer from disk."
  (interactive)
  (cond
   ((derived-mode-p 'pi-coding-agent-session-browser-mode)
    (pi-coding-agent--session-browser-fetch-and-render))
   ((derived-mode-p 'pi-coding-agent-tree-browser-mode)
    (pi-coding-agent--tree-browser-fetch-and-render))
   (t (message "Pi: Not in a browse buffer"))))

;;;; Entry Points

;;;###autoload
(defun pi-coding-agent-session-browser ()
  "Open the session browser for the current project."
  (interactive)
  (let* ((dir (pi-coding-agent--session-directory))
         (new-p (not (get-buffer
                      (pi-coding-agent--session-browser-buffer-name dir))))
         (buf (pi-coding-agent--get-or-create-session-browser dir)))
    ;; Link to the chat session.  Only the chat buffer is cached here;
    ;; `pi-coding-agent--get-process' resolves the process live.
    (when-let* ((chat-buf (pi-coding-agent--get-chat-buffer)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer buf
          (setq pi-coding-agent--chat-buffer chat-buf))))
    (pop-to-buffer buf)
    (pi-coding-agent--browse-apply-margins)
    (pi-coding-agent--session-browser-fetch-and-render)
    (when new-p
      (message "Pi: Press ? for available commands"))))

;;;###autoload
(defun pi-coding-agent-tree-browser ()
  "Open the tree browser for the current session.
Guard first: the tree browser reads the linked chat's session file
from disk, so without a live chat session there is nothing to browse
— signal `user-error' \"No pi session to browse\" BEFORE creating any
browser buffer (a buffer with no link would only render the link
error forever)."
  (interactive)
  (let ((chat-buf (pi-coding-agent--get-chat-buffer)))
    (unless (and chat-buf (buffer-live-p chat-buf))
      (user-error "No pi session to browse")))
  (let* ((dir (pi-coding-agent--session-directory))
         (new-p (not (get-buffer
                      (pi-coding-agent--tree-browser-buffer-name dir))))
         (buf (pi-coding-agent--get-or-create-tree-browser dir)))
    ;; Link to the chat session.  Only the chat buffer is cached here;
    ;; the session file is resolved live from its state on every fetch.
    (when-let* ((chat-buf (pi-coding-agent--get-chat-buffer)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer buf
          (setq pi-coding-agent--chat-buffer chat-buf))))
    (pop-to-buffer buf)
    (pi-coding-agent--browse-apply-margins)
    (pi-coding-agent--tree-browser-fetch-and-render)
    (when new-p
      (message "Pi: Press ? for available commands"))))

(provide 'pi-coding-agent-browse)
;;; pi-coding-agent-browse.el ends here
