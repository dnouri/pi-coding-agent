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
;;   - Tree Browser: navigate conversation tree, label, summarize (like TUI /tree)
;;
;; This module contains:
;;   - RPC command builders for browsing commands
;;   - Response parsers for list_sessions, get_tree, navigate_tree, set_label
;;   - Tree helper functions (active path, flattening, filtering)
;;   - Client-side search/filter logic
;;
;; Depends on: pi-coding-agent-core (RPC), pi-coding-agent-ui (shared state).
;; Does NOT depend on: pi-coding-agent-render or pi-coding-agent-input.

;;; Code:

(require 'pi-coding-agent-core)
(require 'pi-coding-agent-ui)
(require 'cl-lib)
(require 'magit-section)
(require 'transient)

;; Forward declarations for functions in other modules (avoid circular deps)
(declare-function pi-coding-agent-set-session-name "pi-coding-agent-menu")
(declare-function pi-coding-agent--load-session-history "pi-coding-agent-menu")
(declare-function pi-coding-agent--update-session-name-from-file "pi-coding-agent-menu")

;;;; RPC Command Builders

(defun pi-coding-agent--build-list-sessions-command (scope)
  "Build a `list_sessions' RPC command plist.
SCOPE is \"current\" or \"all\"."
  (list :type "list_sessions" :scope scope))

(defun pi-coding-agent--build-get-tree-command ()
  "Build a `get_tree' RPC command plist."
  (list :type "get_tree"))

(defun pi-coding-agent--build-navigate-tree-command (target-id summarize custom-instructions)
  "Build a `navigate_tree' RPC command plist.
TARGET-ID is the node to navigate to.
SUMMARIZE when non-nil triggers branch summarization.
CUSTOM-INSTRUCTIONS is an optional string for custom summary guidance."
  (let ((cmd (list :type "navigate_tree" :targetId target-id)))
    (when summarize
      (setq cmd (plist-put cmd :summarize t)))
    (when custom-instructions
      (setq cmd (plist-put cmd :customInstructions custom-instructions)))
    cmd))

(defun pi-coding-agent--build-set-label-command (entry-id label)
  "Build a `set_label' RPC command plist.
ENTRY-ID is the node to label.
LABEL is the label string, or nil to clear."
  (let ((cmd (list :type "set_label" :entryId entry-id)))
    (when label
      (setq cmd (plist-put cmd :label label)))
    cmd))

(defun pi-coding-agent--build-abort-branch-summary-command ()
  "Build an `abort_branch_summary' RPC command plist."
  (list :type "abort_branch_summary"))

;;;; Response Parsers

(defun pi-coding-agent--parse-session-list (response)
  "Parse a `list_sessions' RESPONSE into a list of session plists.
Returns nil on failure or empty result."
  (when (eq (plist-get response :success) t)
    (let* ((data (plist-get response :data))
           (sessions-vec (plist-get data :sessions)))
      (when (and sessions-vec (> (length sessions-vec) 0))
        (append sessions-vec nil)))))

(defun pi-coding-agent--parse-tree (response)
  "Parse a `get_tree' RESPONSE into a tree data plist.
Returns plist with :tree (vector) and :leafId (string), or nil on failure."
  (when (eq (plist-get response :success) t)
    (plist-get response :data)))

(defun pi-coding-agent--parse-navigate-result (response)
  "Parse a `navigate_tree' RESPONSE into a result plist.
Returns plist with :cancelled, :editorText, :summaryEntry, or nil on failure."
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

(setf (alist-get 'session magit--section-type-alist)
      'pi-coding-agent-session-section)

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
    (define-key map (kbd "RET") #'pi-coding-agent-session-browser-switch)
    (define-key map (kbd "?") #'pi-coding-agent-session-browser-dispatch)
    (define-key map (kbd "h") #'pi-coding-agent-session-browser-dispatch)
    map)
  "Keymap for the session browser.")

(defvar pi-coding-agent-session-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'pi-coding-agent-session-browser-switch)
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
  "Parsed session list from last `list_sessions' response.")

(defvar-local pi-coding-agent--session-browser-search-query nil
  "Current search query string, or nil.")

(defvar-local pi-coding-agent--session-browser-search-tokens nil
  "Parsed search tokens from `pi-coding-agent--session-browser-search-query'.")

(defvar-local pi-coding-agent--session-browser-loading nil
  "Non-nil while a fetch is in progress.")

(defvar-local pi-coding-agent--session-browser-error nil
  "Error message string from last fetch, or nil on success.")

;;;; Session Browser Dispatch Transient

(defun pi-coding-agent--session-dispatch-heading ()
  "Return heading string for the session browser dispatch transient.
Shows current sort mode, scope, and named-only state.
Sibling of `pi-coding-agent--session-browser-header-line' — both
format the same state variables for different contexts."
  ;; Push-then-nreverse: initial list items appear first after nreverse,
  ;; so list them in reverse of desired output order.
  (let ((parts (list (format "scope:%s" pi-coding-agent--session-browser-scope)
                     (format "sort:%s" pi-coding-agent--session-browser-sort))))
    (when pi-coding-agent--session-browser-named-only
      (push "named-only" parts))
    (mapconcat #'identity (nreverse parts) " │ ")))

(transient-define-prefix pi-coding-agent-session-browser-dispatch ()
  "Session browser help."
  [:description pi-coding-agent--session-dispatch-heading
   ["Actions"
    ("RET" "switch" pi-coding-agent-session-browser-switch)
    ("r" "rename" pi-coding-agent-session-browser-rename)
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
  (format "*pi-coding-agent-sessions:%s*" (abbreviate-file-name dir)))

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
         (count (length (or pi-coding-agent--session-browser-items '())))
         (parts (list (format "Sessions [%s]" scope)
                      (format "sort:%s" sort))))
    (when named (push "named-only" parts))
    (when query (push (format "/%s" query) parts))
    (push (format "(%d)" count) parts)
    (push (pi-coding-agent--propertize-face "?:help" 'shadow) parts)
    (mapconcat #'identity (nreverse parts) " │ ")))

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
  "Switch to the session at point.
On success, dismisses the browser window via `quit-window'."
  (interactive)
  (when-let* ((section (magit-current-section))
              (path (oref section value))
              (proc (pi-coding-agent--get-process))
              (chat-buf (pi-coding-agent--get-chat-buffer)))
    (let ((win (selected-window)))
      (pi-coding-agent--rpc-async proc
          (list :type "switch_session" :sessionPath path)
        (lambda (response)
          (let* ((data (plist-get response :data))
                 (cancelled (plist-get data :cancelled)))
            (if (and (plist-get response :success)
                     (pi-coding-agent--json-false-p cancelled))
                (progn
                  ;; Refresh state
                  (pi-coding-agent--rpc-async proc '(:type "get_state")
                    (lambda (resp)
                      (pi-coding-agent--apply-state-response chat-buf resp)))
                  ;; Reload history
                  (pi-coding-agent--load-session-history
                   proc
                   (lambda (count)
                     (message "Pi: Switched session (%d messages)" count))
                   chat-buf)
                  ;; Update session name
                  (when (buffer-live-p chat-buf)
                    (with-current-buffer chat-buf
                      (pi-coding-agent--update-session-name-from-file path)))
                  ;; Dismiss the browser
                  (when (window-live-p win)
                    (quit-window nil win)))
              (message "Pi: Session switch cancelled"))))))))

(defun pi-coding-agent-session-browser-rename ()
  "Rename the current session.
Only works for the currently active session."
  (interactive)
  (when-let* ((section (magit-current-section))
              (path (oref section value))
              (state (and (pi-coding-agent--get-chat-buffer)
                          (buffer-local-value 'pi-coding-agent--state
                                              (pi-coding-agent--get-chat-buffer))))
              (current-file (plist-get state :session-file)))
    (if (equal path current-file)
        (call-interactively #'pi-coding-agent-set-session-name)
      (message "Pi: Can only rename the current session (upstream limitation)"))))

;;;; Fetch and Render

(defun pi-coding-agent--session-browser-fetch-and-render ()
  "Fetch sessions from the server and re-render the buffer."
  (let* ((buf (current-buffer))
         (proc (pi-coding-agent--get-process))
         (scope pi-coding-agent--session-browser-scope))
    (if (not proc)
        (message "Pi: No active process")
      (with-current-buffer buf
        (setq pi-coding-agent--session-browser-loading t)
        (pi-coding-agent--session-browser-rerender))
      (pi-coding-agent--rpc-async proc
          (pi-coding-agent--build-list-sessions-command scope)
        (lambda (response)
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (let ((success (eq (plist-get response :success) t)))
                (setq pi-coding-agent--session-browser-loading nil
                      pi-coding-agent--session-browser-error
                      (unless success
                        (or (plist-get response :error)
                            "list_sessions not supported by this pi version"))
                      pi-coding-agent--session-browser-items
                      (when success
                        (pi-coding-agent--parse-session-list response))))
              (pi-coding-agent--session-browser-rerender))))))))

(defun pi-coding-agent--session-browser-rerender ()
  "Re-render the session browser from local state.
Preserves point using magit-section identity."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (pi-coding-agent--session-browser-render (current-buffer))
    (goto-char (point-min))
    (magit-section-show (magit-current-section))
    (force-mode-line-update)))

;;;; Tree Browser Section Classes and Keymaps

(defclass pi-coding-agent-tree-node-section (magit-section)
  ((keymap :initform 'pi-coding-agent-tree-node-section-map))
  "Section class for a tree node in the tree browser.")

(setf (alist-get 'tree-node magit--section-type-alist)
      'pi-coding-agent-tree-node-section)

(defvar pi-coding-agent-tree-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map pi-coding-agent-browse-mode-map)
    (define-key map (kbd "f") #'pi-coding-agent-tree-browser-cycle-filter)
    (define-key map (kbd "l") #'pi-coding-agent-tree-browser-set-label)
    (define-key map (kbd "/") #'pi-coding-agent-tree-browser-search)
    (define-key map (kbd "RET") #'pi-coding-agent-tree-browser-navigate)
    (define-key map (kbd "S") #'pi-coding-agent-tree-browser-summarize)
    (define-key map (kbd "C-c C-k") #'pi-coding-agent-tree-browser-abort-summarization)
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
  "Parsed tree from last `get_tree' response (vector of root nodes).")

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

(defvar-local pi-coding-agent--tree-browser-summarizing nil
  "Non-nil while a branch summarization is in progress.")

(defconst pi-coding-agent--tree-filter-modes
  '("no-tools" "default" "user-only" "labeled-only" "all")
  "Available filter modes for the tree browser.")

;;;; Tree Browser Dispatch Transient

(defun pi-coding-agent--tree-dispatch-heading ()
  "Return heading string for the tree browser dispatch transient.
Shows current filter mode.
Sibling of `pi-coding-agent--tree-browser-header-line' — both
format the same state variables for different contexts."
  (format "filter:%s" pi-coding-agent--tree-browser-filter))

(transient-define-prefix pi-coding-agent-tree-browser-dispatch ()
  "Tree browser help."
  [:description pi-coding-agent--tree-dispatch-heading
   ["Actions"
    ("RET" "navigate" pi-coding-agent-tree-browser-navigate)
    ("S" "summarize" pi-coding-agent-tree-browser-summarize)
    ("C-c C-k" "abort summary" pi-coding-agent-tree-browser-abort-summarization)
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
  (format "*pi-coding-agent-tree:%s*" (abbreviate-file-name dir)))

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
          (insert (pi-coding-agent--propertize-face
                   "Loading tree..."
                   'pi-coding-agent-activity-phase)
                  "\n"))
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
                    (when-let ((label (plist-get node :label)))
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
         (total pi-coding-agent--tree-browser-visible-count)
         (parts (list (format "Tree [%s]" filter)
                      (format "(%d)" total))))
    (when query (push (format "/%s" query) parts))
    (push (pi-coding-agent--propertize-face "?:help" 'shadow) parts)
    (mapconcat #'identity (nreverse parts) " │ ")))

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

(defun pi-coding-agent--tree-browser-with-node (action)
  "Call ACTION with (proc node-id chat-buf tree-buf on-success).
Guards: verifies section, process, chat buffer, and not-at-leaf.
On success the tree browser window is dismissed via `quit-window'."
  (when-let* ((section (magit-current-section))
              (node-id (oref section value))
              (proc (pi-coding-agent--get-process))
              (chat-buf (pi-coding-agent--get-chat-buffer)))
    (if (equal node-id pi-coding-agent--tree-browser-leaf-id)
        (message "Pi: Already at current position")
      (let ((tree-buf (current-buffer))
            (win (selected-window)))
        (funcall action proc node-id chat-buf tree-buf
                 (lambda ()
                   (when (window-live-p win)
                     (quit-window nil win))))))))

(defun pi-coding-agent-tree-browser-navigate ()
  "Navigate to the tree node at point without summarization.
On success, dismisses the tree browser via `quit-window'."
  (interactive)
  (pi-coding-agent--tree-browser-with-node
   (lambda (proc node-id chat-buf tree-buf on-success)
     (pi-coding-agent--navigate-tree-async
      proc node-id nil nil chat-buf tree-buf on-success))))

(defun pi-coding-agent-tree-browser-summarize ()
  "Navigate to the tree node at point with branch summarization.
Prompts for optional custom summary instructions.  Empty input uses the
default summarization prompt.  `C-g' at the prompt cancels.
On success, dismisses the tree browser via `quit-window'."
  (interactive)
  (pi-coding-agent--tree-browser-with-node
   #'pi-coding-agent--tree-summarize-and-navigate))

(defun pi-coding-agent--tree-summarize-and-navigate
    (proc node-id chat-buf tree-buf &optional on-success)
  "Prompt for summary instructions, then navigate with summarization.
PROC is the pi process.  NODE-ID is the target.
CHAT-BUF and TREE-BUF are refreshed on success.
ON-SUCCESS is an optional callback invoked after successful navigation.
`C-g' at the prompt cancels without sending any RPC."
  (condition-case nil
      (let* ((input (read-string "Summary instructions (RET for default): "))
             (custom-instructions (unless (string-empty-p input) input)))
        (message "Pi: Summarizing...")
        (pi-coding-agent--navigate-tree-async
         proc node-id t custom-instructions chat-buf tree-buf on-success))
    (quit (message "Pi: Summarization cancelled"))))

(defun pi-coding-agent-tree-browser-abort-summarization ()
  "Abort an in-progress branch summarization.
Sends `abort_branch_summary' to the pi process if a summarization
is currently in flight.  Does nothing if no summarization is active."
  (interactive)
  (if (not pi-coding-agent--tree-browser-summarizing)
      (message "Pi: No summarization in progress")
    (setq pi-coding-agent--tree-browser-summarizing nil)
    (when-let ((proc (pi-coding-agent--get-process)))
      (pi-coding-agent--rpc-async proc
          (pi-coding-agent--build-abort-branch-summary-command)
        (lambda (_response)
          (message "Pi: Summarization aborted"))))))

(defun pi-coding-agent--navigate-tree-async
    (proc node-id summarize custom-instructions chat-buf tree-buf
          &optional on-success)
  "Send navigate_tree RPC and handle the response.
PROC is the pi process.  NODE-ID is the target.
SUMMARIZE and CUSTOM-INSTRUCTIONS control branch summary behavior.
CHAT-BUF and TREE-BUF are refreshed on success.
ON-SUCCESS is an optional callback invoked after successful navigation."
  (when (and summarize tree-buf (buffer-live-p tree-buf))
    (with-current-buffer tree-buf
      (setq pi-coding-agent--tree-browser-summarizing t)))
  (pi-coding-agent--rpc-async proc
      (pi-coding-agent--build-navigate-tree-command
       node-id summarize custom-instructions)
    (lambda (response)
      (when (and tree-buf (buffer-live-p tree-buf))
        (with-current-buffer tree-buf
          (setq pi-coding-agent--tree-browser-summarizing nil)))
      (let ((result (pi-coding-agent--parse-navigate-result response)))
        (cond
         ;; Success (not cancelled, not aborted)
         ((and result
               (not (pi-coding-agent--normalize-boolean
                     (plist-get result :cancelled))))
          (pi-coding-agent--handle-navigate-success
           proc result chat-buf tree-buf)
          (when on-success (funcall on-success)))
         ;; Aborted
         ((and result (plist-get result :aborted))
          (message "Pi: Summarization aborted"))
         ;; Cancelled or failed
         (t
          (message "Pi: Navigation cancelled")))))))

(defun pi-coding-agent--handle-navigate-success (proc result chat-buf tree-buf)
  "Handle a successful navigate_tree RESULT.
PROC is used to refresh chat.  CHAT-BUF and TREE-BUF are updated."
  ;; Refresh chat
  (pi-coding-agent--load-session-history
   proc
   (lambda (count)
     (message "Pi: Navigated (%d messages)" count))
   chat-buf)
  ;; Populate input if editorText present
  (when-let ((text (plist-get result :editorText))
             (input-buf (and (buffer-live-p chat-buf)
                             (buffer-local-value
                              'pi-coding-agent--input-buffer
                              chat-buf))))
    (when (buffer-live-p input-buf)
      (with-current-buffer input-buf
        (erase-buffer)
        (insert text))))
  ;; Refresh tree
  (when (buffer-live-p tree-buf)
    (with-current-buffer tree-buf
      (pi-coding-agent--tree-browser-fetch-and-render))))


(defun pi-coding-agent-tree-browser-set-label ()
  "Set or clear a label on the tree node at point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (node-id (oref section value))
              (proc (pi-coding-agent--get-process)))
    (let* ((current-label nil)
           ;; Find current label from tree data
           (_ (when pi-coding-agent--tree-browser-tree
                (setq current-label
                      (pi-coding-agent--tree-find-label
                       pi-coding-agent--tree-browser-tree node-id))))
           (new-label (read-string
                       (if current-label
                           (format "Label (current: %s, empty to clear): " current-label)
                         "Label: ")
                       current-label))
           (label (if (string-empty-p (string-trim new-label)) nil new-label))
           (tree-buf (current-buffer)))
      (pi-coding-agent--rpc-async proc
          (pi-coding-agent--build-set-label-command node-id label)
        (lambda (response)
          (if (eq (plist-get response :success) t)
              (progn
                (message "Pi: Label %s" (if label (format "set to \"%s\"" label) "cleared"))
                (when (buffer-live-p tree-buf)
                  (with-current-buffer tree-buf
                    (pi-coding-agent--tree-browser-fetch-and-render))))
            (message "Pi: Failed to set label")))))))

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

;;;; Tree Browser Fetch and Render

(defun pi-coding-agent--tree-browser-fetch-and-render ()
  "Fetch tree from the server and re-render."
  (let ((buf (current-buffer))
        (proc (pi-coding-agent--get-process)))
    (if (not proc)
        (message "Pi: No active process")
      (with-current-buffer buf
        (setq pi-coding-agent--tree-browser-loading t)
        (pi-coding-agent--tree-browser-rerender))
      (pi-coding-agent--rpc-async proc
          (pi-coding-agent--build-get-tree-command)
        (lambda (response)
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (let ((tree-data (pi-coding-agent--parse-tree response)))
                (setq pi-coding-agent--tree-browser-loading nil
                      pi-coding-agent--tree-browser-tree
                      (plist-get tree-data :tree)
                      pi-coding-agent--tree-browser-leaf-id
                      (plist-get tree-data :leafId)))
              (pi-coding-agent--tree-browser-rerender))))))))

(defun pi-coding-agent--tree-browser-rerender ()
  "Re-render the tree browser from local state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (pi-coding-agent--tree-browser-render (current-buffer))
    (goto-char (point-min))
    (when (magit-current-section)
      (magit-section-show (magit-current-section)))
    (force-mode-line-update)))

;;;; Tree Browser Refresh Integration

(defun pi-coding-agent-browse-refresh ()
  "Refresh the current browse buffer from the server."
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
    ;; Link to chat session
    (when-let ((chat-buf (pi-coding-agent--get-chat-buffer)))
      (with-current-buffer buf
        (setq pi-coding-agent--chat-buffer chat-buf
              pi-coding-agent--process
              (buffer-local-value 'pi-coding-agent--process chat-buf))))
    (pop-to-buffer buf)
    (pi-coding-agent--browse-apply-margins)
    (pi-coding-agent--session-browser-fetch-and-render)
    (when new-p
      (message "Press ? for available commands"))))

;;;###autoload
(defun pi-coding-agent-tree-browser ()
  "Open the tree browser for the current session."
  (interactive)
  (let* ((dir (pi-coding-agent--session-directory))
         (new-p (not (get-buffer
                      (pi-coding-agent--tree-browser-buffer-name dir))))
         (buf (pi-coding-agent--get-or-create-tree-browser dir)))
    ;; Link to chat session
    (when-let ((chat-buf (pi-coding-agent--get-chat-buffer)))
      (with-current-buffer buf
        (setq pi-coding-agent--chat-buffer chat-buf
              pi-coding-agent--process
              (buffer-local-value 'pi-coding-agent--process chat-buf))))
    (pop-to-buffer buf)
    (pi-coding-agent--browse-apply-margins)
    (pi-coding-agent--tree-browser-fetch-and-render)
    (when new-p
      (message "Press ? for available commands"))))

(provide 'pi-coding-agent-browse)
;;; pi-coding-agent-browse.el ends here
