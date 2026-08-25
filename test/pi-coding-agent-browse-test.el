;;; pi-coding-agent-browse-test.el --- Tests for browsing module -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for pi-coding-agent-browse.el — session and tree browser
;; helper functions and response parsing.

;;; Code:

(require 'ert)
(require 'json)
(require 'pi-coding-agent-browse)
(require 'pi-coding-agent-test-common)

;;;; Test Fixtures

(defun pi-coding-agent-test--fixture-sessions ()
  "Session items in the browse dialect, from browse-sessions.json.
Stands in for the dropped `pi-coding-agent--parse-session-list'."
  (append (plist-get (plist-get (pi-coding-agent-test--read-json-fixture
                                 "browse-sessions.json")
                                :data)
                     :sessions)
          nil))

;;;; Session Display

(ert-deftest pi-coding-agent-test-session-display-name ()
  "Session display name prefers name over firstMessage."
  ;; Named session
  (should (equal (pi-coding-agent--session-display-name
                  '(:name "My Session" :firstMessage "some prompt"))
                 "My Session"))
  ;; Unnamed session
  (should (equal (pi-coding-agent--session-display-name
                  '(:firstMessage "Fix the bug in login.py"))
                 "Fix the bug in login.py"))
  ;; No name, no firstMessage
  (should (equal (pi-coding-agent--session-display-name
                  '(:id "abc-123"))
                 "[empty session]"))
  ;; Newlines in firstMessage collapsed to spaces
  (should (equal (pi-coding-agent--session-display-name
                  '(:firstMessage "Fix the bug\nin login.py"))
                 "Fix the bug in login.py"))
  ;; Multiple newlines and surrounding whitespace collapsed
  (should (equal (pi-coding-agent--session-display-name
                  '(:firstMessage "First line\n\nSecond line\n  Third"))
                 "First line Second line Third"))
  ;; Newlines in name also collapsed
  (should (equal (pi-coding-agent--session-display-name
                  '(:name "My\nSession" :firstMessage "prompt"))
                 "My Session")))

(ert-deftest pi-coding-agent-test-first-nonempty-line ()
  "Extract first non-empty line from a string."
  ;; Single line
  (should (equal (pi-coding-agent--first-nonempty-line "hello") "hello"))
  ;; Multi-line returns first
  (should (equal (pi-coding-agent--first-nonempty-line "first\nsecond") "first"))
  ;; Skips leading blank lines
  (should (equal (pi-coding-agent--first-nonempty-line "\n\nactual") "actual"))
  ;; Nil returns empty string
  (should (equal (pi-coding-agent--first-nonempty-line nil) ""))
  ;; Empty string returns empty string
  (should (equal (pi-coding-agent--first-nonempty-line "") ""))
  ;; Only whitespace returns empty string
  (should (equal (pi-coding-agent--first-nonempty-line "\n  \n") "")))

;;;; Tree Parsing

(ert-deftest pi-coding-agent-test-parse-tree ()
  "Parse get_tree response into tree data."
  (let* ((response (pi-coding-agent-test--read-json-fixture "browse-tree.json"))
         (tree-data (pi-coding-agent--parse-tree response)))
    (should tree-data)
    (should (equal (plist-get tree-data :leafId) "node-8"))
    ;; Tree has two roots
    (let ((roots (plist-get tree-data :tree)))
      (should (= (length roots) 2))
      ;; First root is a user message
      (let ((first (aref roots 0)))
        (should (equal (plist-get first :type) "message"))
        (should (equal (plist-get first :role) "user"))))))

(ert-deftest pi-coding-agent-test-parse-tree-error ()
  "Return nil for failed get_tree response."
  (let ((response '(:type "response" :command "get_tree"
                    :success :false :error "no session")))
    (should (null (pi-coding-agent--parse-tree response)))))

;;;; Margin Age Formatting

(ert-deftest pi-coding-agent-test-margin-age-seconds ()
  "Margin age format for seconds."
  (should (equal (pi-coding-agent--margin-age 1) '(1 . "second")))
  (should (equal (pi-coding-agent--margin-age 30) '(30 . "second")))
  (should (equal (pi-coding-agent--margin-age 59) '(59 . "second"))))

(ert-deftest pi-coding-agent-test-margin-age-minutes ()
  "Margin age format for minutes."
  (should (equal (pi-coding-agent--margin-age 60) '(1 . "minute")))
  (should (equal (pi-coding-agent--margin-age 120) '(2 . "minute")))
  (should (equal (pi-coding-agent--margin-age 3599) '(59 . "minute"))))

(ert-deftest pi-coding-agent-test-margin-age-hours ()
  "Margin age format for hours."
  (should (equal (pi-coding-agent--margin-age 3600) '(1 . "hour")))
  (should (equal (pi-coding-agent--margin-age 7200) '(2 . "hour")))
  (should (equal (pi-coding-agent--margin-age 86399) '(23 . "hour"))))

(ert-deftest pi-coding-agent-test-margin-age-days ()
  "Margin age format for days."
  (should (equal (pi-coding-agent--margin-age 86400) '(1 . "day")))
  (should (equal (pi-coding-agent--margin-age 604799) '(6 . "day"))))

(ert-deftest pi-coding-agent-test-margin-age-weeks ()
  "Margin age format for weeks."
  (should (equal (pi-coding-agent--margin-age 604800) '(1 . "week")))
  (should (equal (pi-coding-agent--margin-age 2629799) '(4 . "week"))))

(ert-deftest pi-coding-agent-test-margin-age-months ()
  "Margin age format for months."
  (should (equal (pi-coding-agent--margin-age 2629800) '(1 . "month")))
  (should (equal (pi-coding-agent--margin-age 31557599) '(11 . "month"))))

(ert-deftest pi-coding-agent-test-margin-age-years ()
  "Margin age format for years."
  (should (equal (pi-coding-agent--margin-age 31557600) '(1 . "year")))
  (should (equal (pi-coding-agent--margin-age 63115200) '(2 . "year"))))

(ert-deftest pi-coding-agent-test-margin-age-zero ()
  "Margin age of zero seconds."
  (should (equal (pi-coding-agent--margin-age 0) '(0 . "second"))))

(ert-deftest pi-coding-agent-test-format-margin-age ()
  "Format margin age as aligned string."
  ;; Singular: no trailing s
  (should (equal (pi-coding-agent--format-margin-age 1) " 1 second "))
  ;; Plural: trailing s
  (should (equal (pi-coding-agent--format-margin-age 120) " 2 minutes"))
  ;; Right-justified count
  (should (equal (pi-coding-agent--format-margin-age 3600) " 1 hour   "))
  ;; Large count
  (should (equal (pi-coding-agent--format-margin-age 86400) " 1 day    "))
  ;; Multi-digit count (10 minutes)
  (should (equal (pi-coding-agent--format-margin-age 600) "10 minutes"))
  ;; Week boundary
  (should (equal (pi-coding-agent--format-margin-age 604800) " 1 week   ")))

(ert-deftest pi-coding-agent-test-format-margin-age-from-iso ()
  "Format ISO timestamp as margin age string."
  (cl-letf (((symbol-function 'current-time)
             (lambda () (encode-time '(0 0 12 24 2 2026 nil nil 0)))))
    ;; 5 minutes ago
    (should (equal (pi-coding-agent--format-margin-age-from-iso
                    "2026-02-24T11:55:00.000Z")
                   " 5 minutes"))
    ;; 2 hours ago
    (should (equal (pi-coding-agent--format-margin-age-from-iso
                    "2026-02-24T10:00:00.000Z")
                   " 2 hours  "))))

;;;; Margin Infrastructure

(ert-deftest pi-coding-agent-test-propertize-face ()
  "Propertize-face sets both face and font-lock-face."
  (let ((s (pi-coding-agent--propertize-face "hello" 'bold)))
    (should (equal (get-text-property 0 'face s) 'bold))
    (should (equal (get-text-property 0 'font-lock-face s) 'bold))))

(ert-deftest pi-coding-agent-test-session-margin-width ()
  "Session margin width is computed from age spec."
  ;; Width = count(4) + " msgs "(5) + age(2+1+max-unit-len) = 19
  ;; With 1 char padding = 20
  (should (integerp pi-coding-agent--session-margin-width))
  (should (>= pi-coding-agent--session-margin-width 19)))

(ert-deftest pi-coding-agent-test-tree-margin-width ()
  "Tree margin width accommodates labels."
  (should (integerp pi-coding-agent--tree-margin-width))
  (should (>= pi-coding-agent--tree-margin-width 14)))

(ert-deftest pi-coding-agent-test-make-margin-overlay ()
  "Make-margin-overlay creates overlay with correct properties."
  (with-temp-buffer
    (insert "first line\n")
    (insert "second line\n")
    ;; Create overlay on the second line (point is after it)
    (pi-coding-agent--make-margin-overlay "test margin")
    (let* ((ovs (overlays-in (point-min) (point-max)))
           (o (car ovs)))
      (should o)
      ;; Evaporate property set
      (should (overlay-get o 'evaporate))
      ;; Before-string contains the display spec
      (let* ((bs (overlay-get o 'before-string))
             (display (get-text-property 0 'display bs)))
        (should display)
        ;; Display spec is ((margin right-margin) STRING)
        (should (equal (car display) '(margin right-margin)))
        (should (equal (cadr display) "test margin"))))))

(ert-deftest pi-coding-agent-test-make-margin-overlay-nil-string ()
  "Make-margin-overlay with nil uses a space."
  (with-temp-buffer
    (insert "a line\n")
    (pi-coding-agent--make-margin-overlay nil)
    (let* ((ovs (overlays-in (point-min) (point-max)))
           (o (car ovs))
           (bs (overlay-get o 'before-string))
           (display (get-text-property 0 'display bs)))
      (should (equal (cadr display) " ")))))

(ert-deftest pi-coding-agent-test-browse-apply-margins ()
  "Apply-margins sets the right margin on the window showing the buffer."
  (let ((buf (generate-new-buffer " *test-margins*"))
        (prev-buf (window-buffer (selected-window)))
        (prev-margins (window-margins (selected-window))))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buf)
          (with-current-buffer buf
            (setq pi-coding-agent--browse-margin-width 20)
            (pi-coding-agent--browse-apply-margins))
          (should (equal (cdr (window-margins (selected-window))) 20)))
      (set-window-margins (selected-window)
                          (car prev-margins) (cdr prev-margins))
      (set-window-buffer (selected-window) prev-buf)
      (kill-buffer buf))))

(ert-deftest pi-coding-agent-test-browse-mode-sets-right-margin-width ()
  "Browse mode sets buffer-local `right-margin-width'.
This ensures margins are cleaned up when `quit-window' switches to
another buffer — Emacs resets window margins from the new buffer's
`right-margin-width' during `set-window-buffer'."
  (let ((tree-buf (generate-new-buffer " *test-tree*"))
        (session-buf (generate-new-buffer " *test-sessions*")))
    (unwind-protect
        (progn
          (with-current-buffer tree-buf
            (pi-coding-agent-tree-browser-mode)
            (should (= right-margin-width
                       pi-coding-agent--tree-margin-width)))
          (with-current-buffer session-buf
            (pi-coding-agent-session-browser-mode)
            (should (= right-margin-width
                       pi-coding-agent--session-margin-width))))
      (kill-buffer tree-buf)
      (kill-buffer session-buf))))

(ert-deftest pi-coding-agent-test-browse-mode-no-margin-leak ()
  "Mode setup must not set margins on unrelated windows.
When the browse buffer is created via `with-current-buffer' (not yet
displayed), `--browse-apply-margins' must not touch `selected-window'."
  (let ((other-buf (current-buffer))
        (browse-buf (generate-new-buffer " *test-tree-leak*")))
    (unwind-protect
        (progn
          ;; Record the current window's margins before mode setup
          (set-window-margins (selected-window) nil nil)
          (should-not (cdr (window-margins (selected-window))))
          ;; Create browse buffer in background (not displayed)
          (with-current-buffer browse-buf
            (pi-coding-agent-tree-browser-mode))
          ;; The selected window (showing other-buf) must NOT have margins
          (should-not (cdr (window-margins (selected-window)))))
      (kill-buffer browse-buf))))

;;;; Active Path Detection

(ert-deftest pi-coding-agent-test-active-path-ids ()
  "Compute set of node IDs on the active path from root to leaf."
  (let* ((response (pi-coding-agent-test--read-json-fixture "browse-tree.json"))
         (tree-data (pi-coding-agent--parse-tree response))
         (active (pi-coding-agent--active-path-ids
                  (plist-get tree-data :tree)
                  (plist-get tree-data :leafId))))
    ;; The path from root to node-8: node-1 → node-2 → node-3 → node-4 → node-5 → node-6 → node-7 → node-8
    (should (gethash "node-1" active))
    (should (gethash "node-8" active))
    (should (gethash "node-4" active))
    ;; Abandoned branch node should NOT be on active path
    (should-not (gethash "node-9" active))
    ;; Compaction root node-10 is not on active path
    (should-not (gethash "node-10" active))))

;;;; Deep Tree Safety

(defun pi-coding-agent-test--make-deep-tree (n)
  "Create a single-chain tree of N nodes for depth testing."
  (let ((node (list :id (format "node-%d" n)
                    :type "message" :role "user"
                    :preview (format "message %d" n)
                    :timestamp "2026-01-01T00:00:00Z"
                    :children (vector))))
    (cl-loop for i from (1- n) downto 1
             do (setq node (list :id (format "node-%d" i)
                                 :type "message"
                                 :role (if (= (mod i 2) 1) "user" "assistant")
                                 :preview (format "message %d" i)
                                 :timestamp "2026-01-01T00:00:00Z"
                                 :children (vector node))))
    (vector node)))

(ert-deftest pi-coding-agent-test-flatten-tree-deep-chain ()
  "Flatten a linear chain deeper than max-lisp-eval-depth."
  (let* ((n 2000)
         (tree (pi-coding-agent-test--make-deep-tree n))
         (leaf-id (format "node-%d" n))
         (flat (pi-coding-agent--flatten-tree-for-display
                tree leaf-id "default")))
    (should (= (length flat) n))))

(ert-deftest pi-coding-agent-test-subtree-contains-active-deep ()
  "Subtree-contains-active-p works on chains deeper than max-lisp-eval-depth."
  (let* ((n 2000)
         (tree (pi-coding-agent-test--make-deep-tree n))
         (active-ids (make-hash-table :test 'equal)))
    (puthash (format "node-%d" n) t active-ids)
    (should (pi-coding-agent--subtree-contains-active-p
             (aref tree 0) active-ids))))

;;;; Tree Flattening

(ert-deftest pi-coding-agent-test-flatten-tree-for-display ()
  "Flatten tree into display-ordered list with indent levels and prefixes."
  (let* ((response (pi-coding-agent-test--read-json-fixture "browse-tree.json"))
         (tree-data (pi-coding-agent--parse-tree response))
         (flat (pi-coding-agent--flatten-tree-for-display
                (plist-get tree-data :tree)
                (plist-get tree-data :leafId)
                "default")))
    ;; Should return a list of (node indent prefix) lists
    (should (listp flat))
    (should (> (length flat) 0))
    ;; First item should be the first root
    (let* ((first-entry (car flat))
           (node (nth 0 first-entry))
           (indent (nth 1 first-entry))
           (prefix (nth 2 first-entry)))
      (should (equal (plist-get node :id) "node-1"))
      (should (= indent 0))
      (should (stringp prefix)))))

(ert-deftest pi-coding-agent-test-flatten-tree-connector-prefixes ()
  "Branch children get ├─/└─ connectors; chain nodes get gutter continuation."
  (let* ((response (pi-coding-agent-test--read-json-fixture "browse-tree.json"))
         (tree-data (pi-coding-agent--parse-tree response))
         (flat (pi-coding-agent--flatten-tree-for-display
                (plist-get tree-data :tree)
                (plist-get tree-data :leafId)
                "default"))
         ;; Build alist of (id . prefix) for easy lookup
         (prefix-alist (mapcar (lambda (entry)
                                 (cons (plist-get (nth 0 entry) :id)
                                       (nth 2 entry)))
                               flat)))
    ;; Root-level single-child chain: no prefix
    (should (equal (alist-get "node-1" prefix-alist nil nil #'equal) ""))
    (should (equal (alist-get "node-2" prefix-alist nil nil #'equal) ""))
    (should (equal (alist-get "node-3" prefix-alist nil nil #'equal) ""))
    (should (equal (alist-get "node-4" prefix-alist nil nil #'equal) ""))
    ;; Branch point children: first gets ├─, last gets └─
    ;; node-5 is first (active branch), node-9 is last
    (should (equal (alist-get "node-5" prefix-alist nil nil #'equal) "├─ "))
    (should (equal (alist-get "node-9" prefix-alist nil nil #'equal) "└─ "))
    ;; Descendants within active branch: gutter continuation
    (should (equal (alist-get "node-6" prefix-alist nil nil #'equal) "│  "))
    (should (equal (alist-get "node-7" prefix-alist nil nil #'equal) "│  "))
    (should (equal (alist-get "node-8" prefix-alist nil nil #'equal) "│  "))
    ;; Second root and its child: no prefix (no top-level connectors)
    (should (equal (alist-get "node-10" prefix-alist nil nil #'equal) ""))
    (should (equal (alist-get "node-11" prefix-alist nil nil #'equal) ""))))

(ert-deftest pi-coding-agent-test-flatten-tree-connectors-no-tools-filter ()
  "Connectors work when tool nodes are filtered out."
  (let* ((response (pi-coding-agent-test--read-json-fixture "browse-tree.json"))
         (tree-data (pi-coding-agent--parse-tree response))
         (flat (pi-coding-agent--flatten-tree-for-display
                (plist-get tree-data :tree)
                (plist-get tree-data :leafId)
                "no-tools"))
         (prefix-alist (mapcar (lambda (entry)
                                 (cons (plist-get (nth 0 entry) :id)
                                       (nth 2 entry)))
                               flat))
         (id-list (mapcar (lambda (entry) (plist-get (nth 0 entry) :id)) flat)))
    ;; Tool nodes should be absent
    (should-not (member "node-3" id-list))
    (should-not (member "node-6" id-list))
    ;; Branch connectors still correct (node-5 first, node-9 last)
    (should (equal (alist-get "node-5" prefix-alist nil nil #'equal) "├─ "))
    (should (equal (alist-get "node-9" prefix-alist nil nil #'equal) "└─ "))
    ;; Chain descendant of active branch still gets gutter
    (should (equal (alist-get "node-7" prefix-alist nil nil #'equal) "│  "))
    (should (equal (alist-get "node-8" prefix-alist nil nil #'equal) "│  "))))

(ert-deftest pi-coding-agent-test-flatten-tree-connectors-single-root ()
  "Single-root tree has no top-level connectors."
  (let* ((tree (list '(:id "r1" :type "message" :role "user"
                       :children [(:id "c1" :type "message" :role "assistant"
                                  :preview "hi" :children [])])))
         (flat (pi-coding-agent--flatten-tree-for-display tree "c1" "default"))
         (prefixes (mapcar (lambda (e) (nth 2 e)) flat)))
    ;; Both nodes at root level, single-child chain — no connectors
    (should (equal prefixes '("" "")))))

(ert-deftest pi-coding-agent-test-flatten-tree-connectors-nested-branches ()
  "Nested branch points produce correct multi-level gutter stacks."
  (let* ((tree (list
                '(:id "root" :type "message" :role "user" :preview "root"
                  :children
                  [(:id "a1" :type "message" :role "assistant" :preview "a1"
                    :children
                    [(:id "u2" :type "message" :role "user" :preview "u2"
                      :children [])
                     (:id "u3" :type "message" :role "user" :preview "u3"
                      :children [])])
                   (:id "a2" :type "message" :role "assistant" :preview "a2"
                    :children [])])))
         ;; leaf is u2 so a1 branch is active
         (flat (pi-coding-agent--flatten-tree-for-display tree "u2" "default"))
         (prefix-alist (mapcar (lambda (entry)
                                 (cons (plist-get (nth 0 entry) :id)
                                       (nth 2 entry)))
                               flat)))
    ;; root: no prefix
    (should (equal (alist-get "root" prefix-alist nil nil #'equal) ""))
    ;; First branch children: a1 (active, first), a2 (last)
    (should (equal (alist-get "a1" prefix-alist nil nil #'equal) "├─ "))
    (should (equal (alist-get "a2" prefix-alist nil nil #'equal) "└─ "))
    ;; Nested branch under a1: u2 (active, first), u3 (last)
    ;; Gutter from outer branch (│) + inner connector
    (should (equal (alist-get "u2" prefix-alist nil nil #'equal) "│  ├─ "))
    (should (equal (alist-get "u3" prefix-alist nil nil #'equal) "│  └─ "))))

(ert-deftest pi-coding-agent-test-flatten-tree-connectors-three-siblings ()
  "Three siblings at a branch point: ├─, ├─, └─."
  (let* ((tree (list
                '(:id "root" :type "message" :role "user" :preview "q"
                  :children
                  [(:id "c1" :type "message" :role "assistant"
                    :preview "first" :children [])
                   (:id "c2" :type "message" :role "assistant"
                    :preview "second" :children [])
                   (:id "c3" :type "message" :role "assistant"
                    :preview "third" :children [])])))
         (flat (pi-coding-agent--flatten-tree-for-display tree "c1" "default"))
         (prefix-alist (mapcar (lambda (entry)
                                 (cons (plist-get (nth 0 entry) :id)
                                       (nth 2 entry)))
                               flat)))
    (should (equal (alist-get "root" prefix-alist nil nil #'equal) ""))
    ;; Active child first, then others in order
    (should (equal (alist-get "c1" prefix-alist nil nil #'equal) "├─ "))
    (should (equal (alist-get "c2" prefix-alist nil nil #'equal) "├─ "))
    (should (equal (alist-get "c3" prefix-alist nil nil #'equal) "└─ "))))

;;;; Filter Predicates

(ert-deftest pi-coding-agent-test-filter-default ()
  "Default filter shows messages, tool results, compaction, branch summary."
  (should (pi-coding-agent--browse-node-visible-p
           '(:type "message" :role "user") "default"))
  (should (pi-coding-agent--browse-node-visible-p
           '(:type "message" :role "assistant" :preview "hello") "default"))
  (should (pi-coding-agent--browse-node-visible-p
           '(:type "tool_result") "default"))
  (should (pi-coding-agent--browse-node-visible-p
           '(:type "compaction") "default"))
  (should (pi-coding-agent--browse-node-visible-p
           '(:type "branch_summary") "default"))
  ;; Model change hidden in default
  (should-not (pi-coding-agent--browse-node-visible-p
               '(:type "model_change") "default"))
  ;; Thinking level change hidden in default
  (should-not (pi-coding-agent--browse-node-visible-p
               '(:type "thinking_level_change") "default")))

(ert-deftest pi-coding-agent-test-filter-no-tools ()
  "No-tools filter hides tool_result entries."
  (should (pi-coding-agent--browse-node-visible-p
           '(:type "message" :role "user") "no-tools"))
  (should-not (pi-coding-agent--browse-node-visible-p
               '(:type "tool_result") "no-tools")))

(ert-deftest pi-coding-agent-test-filter-user-only ()
  "User-only filter shows only user messages."
  (should (pi-coding-agent--browse-node-visible-p
           '(:type "message" :role "user") "user-only"))
  (should-not (pi-coding-agent--browse-node-visible-p
               '(:type "message" :role "assistant" :preview "hello") "user-only"))
  (should-not (pi-coding-agent--browse-node-visible-p
               '(:type "tool_result") "user-only")))

(ert-deftest pi-coding-agent-test-filter-labeled-only ()
  "Labeled-only filter shows only entries with labels."
  (should (pi-coding-agent--browse-node-visible-p
           '(:type "message" :role "user" :label "checkpoint") "labeled-only"))
  (should-not (pi-coding-agent--browse-node-visible-p
               '(:type "message" :role "user") "labeled-only")))

(ert-deftest pi-coding-agent-test-filter-all ()
  "All filter shows settings entries that other modes hide."
  (should (pi-coding-agent--browse-node-visible-p
           '(:type "model_change") "all"))
  (should (pi-coding-agent--browse-node-visible-p
           '(:type "thinking_level_change") "all")))

(ert-deftest pi-coding-agent-test-filter-empty-assistant ()
  "Empty assistant messages are hidden (unless they are the leaf)."
  ;; Empty assistant with no useful content
  (should-not (pi-coding-agent--browse-node-visible-p
               '(:type "message" :role "assistant" :preview "") "default"))
  ;; Aborted assistant is shown
  (should (pi-coding-agent--browse-node-visible-p
           '(:type "message" :role "assistant" :preview "" :stopReason "aborted") "default"))
  ;; Assistant with error is shown
  (should (pi-coding-agent--browse-node-visible-p
           '(:type "message" :role "assistant" :preview "" :errorMessage "rate limit") "default")))

(ert-deftest pi-coding-agent-test-empty-assistant-hidden-in-all-modes ()
  "Empty assistant messages are hidden in ALL filter modes.
Per TUI tree-selector.ts:282-293 and PLAN-BROWSING.md line 560:
empty assistants are a universal pre-filter, not mode-specific."
  (let ((empty-ast '(:type "message" :role "assistant" :preview "(no content)"))
        (empty-ast-blank '(:type "message" :role "assistant" :preview "")))
    (dolist (mode '("default" "no-tools" "all"))
      (should-not (pi-coding-agent--browse-node-visible-p empty-ast mode))
      (should-not (pi-coding-agent--browse-node-visible-p empty-ast-blank mode)))))

(ert-deftest pi-coding-agent-test-empty-assistant-shown-when-aborted-all-modes ()
  "Aborted/error assistant messages are shown even if empty, in all modes."
  (let ((aborted '(:type "message" :role "assistant" :preview ""
                          :stopReason "aborted"))
        (errored '(:type "message" :role "assistant" :preview ""
                          :errorMessage "rate limit")))
    (dolist (mode '("default" "no-tools" "all"))
      (should (pi-coding-agent--browse-node-visible-p aborted mode))
      (should (pi-coding-agent--browse-node-visible-p errored mode)))))

;;;; Search/Filter

(ert-deftest pi-coding-agent-test-matches-filter-p ()
  "Space-separated regexp token matching."
  ;; Single token
  (should (pi-coding-agent--matches-filter-p "Fix the login bug" '("login")))
  ;; Multiple tokens (AND)
  (should (pi-coding-agent--matches-filter-p "Fix the login bug" '("login" "bug")))
  ;; Non-match
  (should-not (pi-coding-agent--matches-filter-p "Fix the login bug" '("database")))
  ;; Regexp token
  (should (pi-coding-agent--matches-filter-p "Fix the login bug" '("log.*bug")))
  ;; Empty tokens list matches everything
  (should (pi-coding-agent--matches-filter-p "anything" nil)))

;;;; Session Sorting

(ert-deftest pi-coding-agent-test-session-sort-cycle ()
  "Sort mode cycles through threaded → recent → relevance."
  (should (equal (pi-coding-agent--session-sort-next "threaded") "recent"))
  (should (equal (pi-coding-agent--session-sort-next "recent") "relevance"))
  (should (equal (pi-coding-agent--session-sort-next "relevance") "threaded")))

(ert-deftest pi-coding-agent-test-session-sort-recent ()
  "Sort by recent puts newest modified first."
  (let ((items (list '(:modified "2026-02-20T10:00:00Z" :id "old")
                     '(:modified "2026-02-24T10:00:00Z" :id "new")
                     '(:modified "2026-02-22T10:00:00Z" :id "mid"))))
    (let ((sorted (pi-coding-agent--session-sort-items items "recent")))
      (should (equal (plist-get (nth 0 sorted) :id) "new"))
      (should (equal (plist-get (nth 1 sorted) :id) "mid"))
      (should (equal (plist-get (nth 2 sorted) :id) "old")))))

(ert-deftest pi-coding-agent-test-session-sort-relevance ()
  "Sort by relevance puts highest message count first."
  (let ((items (list '(:messageCount 10 :id "small")
                     '(:messageCount 500 :id "big")
                     '(:messageCount 100 :id "med"))))
    (let ((sorted (pi-coding-agent--session-sort-items items "relevance")))
      (should (equal (plist-get (nth 0 sorted) :id) "big"))
      (should (equal (plist-get (nth 1 sorted) :id) "med"))
      (should (equal (plist-get (nth 2 sorted) :id) "small")))))

;;;; Session Threading

(ert-deftest pi-coding-agent-test-session-threading ()
  "Thread items into parent-child structure."
  (let* ((items (pi-coding-agent-test--fixture-sessions))
         (threaded (pi-coding-agent--session-thread-items items)))
    ;; Should have entries with depth
    (should (> (length threaded) 0))
    ;; Root items have depth 0
    (let ((roots (cl-remove-if-not (lambda (e) (= (cdr e) 0)) threaded)))
      (should (>= (length roots) 3)))
    ;; Session ccc-333 is a child of bbb-222, should have depth 1
    (let ((child (cl-find-if (lambda (e)
                               (equal (plist-get (car e) :id) "ccc-333"))
                             threaded)))
      (should child)
      (should (= (cdr child) 1)))))

;;;; Session Filter

(ert-deftest pi-coding-agent-test-session-filter-named ()
  "Named filter keeps only sessions with a name."
  (let* ((items (pi-coding-agent-test--fixture-sessions))
         (named (pi-coding-agent--session-filter-named items)))
    ;; Only bbb-222 and ddd-444 have names
    (should (= (length named) 2))
    (should (cl-every (lambda (item)
                        (plist-get item :name))
                      named))))

(ert-deftest pi-coding-agent-test-session-filter-search ()
  "Search filter matches against name and first message."
  (let ((items (pi-coding-agent-test--fixture-sessions)))
    ;; Search for "database"
    (let ((found (pi-coding-agent--session-filter-search items '("database"))))
      (should (= (length found) 2))  ; bbb-222 and ccc-333 mention database
      )
    ;; Search for "CI" matches Setup CI/CD
    (let ((found (pi-coding-agent--session-filter-search items '("CI"))))
      (should (>= (length found) 1)))))

;;;; Time Groups

(ert-deftest pi-coding-agent-test-session-time-group ()
  "Time group labels for ISO timestamps."
  ;; Now → Today
  (let ((now (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" (current-time) t)))
    (should (equal (pi-coding-agent--session-time-group now) "Today")))
  ;; 2 days ago → Yesterday or This Week depending on time of day
  ;; 30 days ago → Older
  (let ((old (format-time-string "%Y-%m-%dT%H:%M:%S.000Z"
                                 (time-subtract (current-time) (days-to-time 30))
                                 t)))
    (should (equal (pi-coding-agent--session-time-group old) "Older"))))

;;;; Session Browser Rendering

(ert-deftest pi-coding-agent-test-session-browser-render-flat ()
  "Render sessions as flat list in a buffer."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (setq pi-coding-agent--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 42 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/b.jsonl" :firstMessage "Fix the bug"
                  :messageCount 10 :modified "2026-02-23T10:00:00Z")))
    (setq pi-coding-agent--session-browser-sort "relevance")
    (pi-coding-agent--session-browser-rerender)
    ;; Buffer should contain session names
    (should (string-match-p "Session A" (buffer-string)))
    (should (string-match-p "Fix the bug" (buffer-string)))
    ;; Session A has more messages, should come first in relevance sort
    (let ((pos-a (string-match "Session A" (buffer-string)))
          (pos-b (string-match "Fix the bug" (buffer-string))))
      (should (< pos-a pos-b)))
    ;; Count and age should NOT be in buffer text (they're in margins)
    (should-not (string-match-p "42 msgs" (buffer-string)))
    (should-not (string-match-p "10 msgs" (buffer-string)))))

(ert-deftest pi-coding-agent-test-session-browser-render-threaded ()
  "Render sessions with threading connectors."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (setq pi-coding-agent--session-browser-items
          (list '(:path "/test/parent.jsonl" :name "Parent Session"
                  :messageCount 100 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/child.jsonl" :firstMessage "Child branch"
                  :parentSessionPath "/test/parent.jsonl"
                  :messageCount 20 :modified "2026-02-24T11:00:00Z")))
    (setq pi-coding-agent--session-browser-sort "threaded")
    (pi-coding-agent--session-browser-rerender)
    ;; Should contain threading connector
    (should (string-match-p "└─" (buffer-string)))
    ;; Parent before child
    (let ((pos-p (string-match "Parent Session" (buffer-string)))
          (pos-c (string-match "Child branch" (buffer-string))))
      (should (< pos-p pos-c)))))

(ert-deftest pi-coding-agent-test-session-browser-fork-prefix-flat ()
  "Forked sessions show `fork:' prefix in non-threaded modes."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (setq pi-coding-agent--session-browser-items
          (list '(:path "/test/parent.jsonl" :name "Parent Session"
                  :messageCount 100 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/child.jsonl" :firstMessage "Child branch"
                  :parentSessionPath "/test/parent.jsonl"
                  :messageCount 20 :modified "2026-02-24T11:00:00Z")))
    (setq pi-coding-agent--session-browser-sort "relevance")
    (pi-coding-agent--session-browser-rerender)
    ;; Fork prefix should appear before child session
    (should (string-match-p "fork:" (buffer-string)))
    ;; But NOT before parent
    (let ((text (buffer-string)))
      (should-not (string-match-p "fork:.*Parent Session" text)))))

(ert-deftest pi-coding-agent-test-session-browser-fork-prefix-threaded ()
  "Forked sessions do NOT show `fork:' prefix in threaded mode."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (setq pi-coding-agent--session-browser-items
          (list '(:path "/test/parent.jsonl" :name "Parent Session"
                  :messageCount 100 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/child.jsonl" :firstMessage "Child branch"
                  :parentSessionPath "/test/parent.jsonl"
                  :messageCount 20 :modified "2026-02-24T11:00:00Z")))
    (setq pi-coding-agent--session-browser-sort "threaded")
    (pi-coding-agent--session-browser-rerender)
    ;; Threading connector should appear, but NOT fork: prefix
    (should (string-match-p "└─" (buffer-string)))
    (should-not (string-match-p "fork:" (buffer-string)))))

(ert-deftest pi-coding-agent-test-session-browser-margin-overlays ()
  "Session entries have right-margin overlays with count and age."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (setq pi-coding-agent--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 42 :modified "2026-02-24T10:00:00Z")))
    (setq pi-coding-agent--session-browser-sort "relevance")
    (pi-coding-agent--session-browser-rerender)
    ;; Should have at least one overlay
    (let ((ovs (overlays-in (point-min) (point-max))))
      (should (> (length ovs) 0))
      ;; Find our margin overlay (has before-string with margin display)
      (let* ((margin-ovs (cl-remove-if-not
                          (lambda (o)
                            (let ((bs (overlay-get o 'before-string)))
                              (and bs (get-text-property 0 'display bs))))
                          ovs))
             (ov (car margin-ovs))
             (bs (overlay-get ov 'before-string))
             (display (get-text-property 0 'display bs))
             (content (cadr display)))
        (should (equal (car display) '(margin right-margin)))
        ;; Content should contain message count
        (should (string-match-p "42 msgs" content))))))

(ert-deftest pi-coding-agent-test-session-browser-no-name-truncation ()
  "Session names are not truncated."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (let ((long-name (make-string 80 ?x)))
      (setq pi-coding-agent--session-browser-items
            (list (list :path "/test/a.jsonl" :name long-name
                        :messageCount 1 :modified "2026-02-24T10:00:00Z")))
      (setq pi-coding-agent--session-browser-sort "relevance")
      (pi-coding-agent--session-browser-rerender)
      ;; Full name should appear, not truncated
      (should (string-match-p long-name (buffer-string))))))

(ert-deftest pi-coding-agent-test-session-browser-render-loading ()
  "Render loading indicator."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (setq pi-coding-agent--session-browser-loading t)
    (pi-coding-agent--session-browser-rerender)
    (should (string-match-p "Loading" (buffer-string)))))

(ert-deftest pi-coding-agent-test-session-browser-render-empty ()
  "Render empty state when no sessions."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (setq pi-coding-agent--session-browser-items nil)
    (pi-coding-agent--session-browser-rerender)
    (should (string-match-p "No sessions found" (buffer-string)))))

(ert-deftest pi-coding-agent-test-session-browser-header-line ()
  "Header-line shows scope, sort, and filter state."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (setq pi-coding-agent--session-browser-scope "current"
          pi-coding-agent--session-browser-sort "threaded"
          pi-coding-agent--session-browser-items '((:id "a") (:id "b")))
    (let ((header (pi-coding-agent--session-browser-header-line)))
      (should (string-match-p "current" header))
      (should (string-match-p "threaded" header))
      (should (string-match-p "(2)" header)))))

;;;; Tree Node Formatting

(ert-deftest pi-coding-agent-test-tree-node-face ()
  "Correct face for each node type."
  (should (eq (pi-coding-agent--tree-node-face
               '(:type "message" :role "user"))
              'pi-coding-agent-tree-user))
  (should (eq (pi-coding-agent--tree-node-face
               '(:type "message" :role "assistant"))
              'pi-coding-agent-tree-assistant))
  (should (eq (pi-coding-agent--tree-node-face
               '(:type "tool_result"))
              'pi-coding-agent-tree-tool))
  (should (eq (pi-coding-agent--tree-node-face
               '(:type "compaction"))
              'pi-coding-agent-tree-compaction))
  (should (eq (pi-coding-agent--tree-node-face
               '(:type "branch_summary"))
              'pi-coding-agent-tree-summary)))

(ert-deftest pi-coding-agent-test-tree-node-type-label ()
  "Short type labels for tree nodes."
  (should (equal (pi-coding-agent--tree-node-type-label
                  '(:type "message" :role "user"))
                 "you"))
  (should (equal (pi-coding-agent--tree-node-type-label
                  '(:type "message" :role "assistant"))
                 "ast"))
  (should (equal (pi-coding-agent--tree-node-type-label
                  '(:type "tool_result" :toolName "Read"))
                 "Read"))
  (should (equal (pi-coding-agent--tree-node-type-label
                  '(:type "compaction"))
                 "compact")))

;;;; Tool Preview Unpacking

(ert-deftest pi-coding-agent-test-tree-strip-bracket-preview-formatted ()
  "Strip bracket wrapper from formattedToolCall."
  (should (equal (pi-coding-agent--tree-strip-bracket-preview
                  '(:type "tool_result" :toolName "read"
                    :formattedToolCall "[read: ~/file.py:10-29]"
                    :preview "[read: ~/file.py:10-29]"))
                 "~/file.py:10-29")))

(ert-deftest pi-coding-agent-test-tree-strip-bracket-preview-read ()
  "Read tool strips wrapper, shows path."
  (should (equal (pi-coding-agent--tree-strip-bracket-preview
                  '(:type "tool_result" :toolName "Read"
                    :preview "[Read: db/connection.py]"))
                 "db/connection.py")))

(ert-deftest pi-coding-agent-test-tree-strip-bracket-preview-bash ()
  "Bash tool strips wrapper, shows command."
  (should (equal (pi-coding-agent--tree-strip-bracket-preview
                  '(:type "tool_result" :toolName "bash"
                    :formattedToolCall "[bash: git status]"
                    :preview "[bash: git status]"))
                 "git status")))

(ert-deftest pi-coding-agent-test-tree-strip-bracket-preview-no-args ()
  "Tool with no args returns empty string."
  (should (equal (pi-coding-agent--tree-strip-bracket-preview
                  '(:type "tool_result" :toolName "unknown"
                    :preview "[unknown]"))
                 "")))

(ert-deftest pi-coding-agent-test-tree-strip-bracket-preview-plain-text ()
  "Preview without brackets returned as-is."
  (should (equal (pi-coding-agent--tree-strip-bracket-preview
                  '(:type "tool_result" :toolName "custom"
                    :preview "some plain output"))
                 "some plain output")))

(ert-deftest pi-coding-agent-test-tree-strip-bracket-preview-in-node-line ()
  "Tool result in formatted node line shows unwrapped preview."
  (let ((line (pi-coding-agent--tree-format-node-line
               '(:type "tool_result" :toolName "Read"
                 :preview "[Read: db/connection.py]")
               nil)))
    ;; Should NOT have the bracketed format
    (should-not (string-match-p "\\[Read:" line))
    ;; Should have the unwrapped path
    (should (string-match-p "db/connection.py" line))))

(ert-deftest pi-coding-agent-test-tree-node-preview-message ()
  "Regular message nodes return preview as-is."
  (should (equal (pi-coding-agent--tree-node-preview
                  '(:type "message" :role "user" :preview "hello world"))
                 "hello world"))
  (should (equal (pi-coding-agent--tree-node-preview
                  '(:type "message" :role "assistant" :preview "sure thing"))
                 "sure thing"))
  ;; Missing preview returns empty string
  (should (equal (pi-coding-agent--tree-node-preview
                  '(:type "message" :role "user"))
                 "")))

(ert-deftest pi-coding-agent-test-tree-node-preview-branch-summary ()
  "Branch summary nodes return first line of summary, not full text."
  ;; Multi-line summary returns only first line
  (should (equal (pi-coding-agent--tree-node-preview
                  '(:type "branch_summary"
                    :summary "The user explored TDD.\n\n## Goal\nLearn testing."))
                 "The user explored TDD."))
  ;; Single-line summary returned as-is
  (should (equal (pi-coding-agent--tree-node-preview
                  '(:type "branch_summary"
                    :summary "Short summary"))
                 "Short summary"))
  ;; Missing summary returns empty string
  (should (equal (pi-coding-agent--tree-node-preview
                  '(:type "branch_summary"))
                 ""))
  ;; Summary starting with blank lines skips to first non-empty line
  (should (equal (pi-coding-agent--tree-node-preview
                  '(:type "branch_summary"
                    :summary "\n\nActual summary here\nMore text"))
                 "Actual summary here")))

(ert-deftest pi-coding-agent-test-tree-node-preview-bash-execution ()
  "Bash execution message strips bracket wrapper from preview.
Upstream changed format from `[bash]: cmd' to `[bash: cmd]'.
The type label already shows `sh', so brackets are redundant."
  ;; tree-node-preview strips the wrapper
  (should (equal (pi-coding-agent--tree-node-preview
                  '(:type "message" :role "bashExecution"
                    :preview "[bash: git status]"))
                 "git status"))
  ;; Formatted node line shows stripped preview
  (let ((line (pi-coding-agent--tree-format-node-line
               '(:type "message" :role "bashExecution"
                 :preview "[bash: git log --oneline]")
               nil)))
    (should-not (string-match-p "\\[bash:" line))
    (should (string-match-p "git log --oneline" line))))

(ert-deftest pi-coding-agent-test-tree-format-node-active ()
  "Active path nodes get bullet marker."
  (let ((line (pi-coding-agent--tree-format-node-line
               '(:type "message" :role "user" :preview "hello") t)))
    (should (string-match-p "•" line))
    (should (string-match-p "hello" line))))

(ert-deftest pi-coding-agent-test-tree-format-node-inactive ()
  "Inactive nodes get space instead of bullet."
  (let ((line (pi-coding-agent--tree-format-node-line
               '(:type "message" :role "user" :preview "hello") nil)))
    (should-not (string-match-p "•" line))
    (should (string-match-p "hello" line))))

(ert-deftest pi-coding-agent-test-tree-format-node-with-label ()
  "Labeled nodes do NOT include label in the line text (labels go in margin)."
  (let ((line (pi-coding-agent--tree-format-node-line
               '(:type "message" :role "user" :preview "hello"
                 :label "checkpoint")
               nil)))
    ;; Label should not be in the main text
    (should-not (string-match-p "\\[checkpoint\\]" line))
    ;; But preview should still appear
    (should (string-match-p "hello" line))))

;;;; Tree Browser Rendering

(ert-deftest pi-coding-agent-test-tree-browser-render ()
  "Render tree from fixture data."
  (with-temp-buffer
    (pi-coding-agent-tree-browser-mode)
    (let* ((response (pi-coding-agent-test--read-json-fixture "browse-tree.json"))
           (tree-data (pi-coding-agent--parse-tree response)))
      (setq pi-coding-agent--tree-browser-tree (plist-get tree-data :tree)
            pi-coding-agent--tree-browser-leaf-id (plist-get tree-data :leafId)
            pi-coding-agent--tree-browser-filter "default")
      (pi-coding-agent--tree-browser-rerender)
      ;; Buffer should contain node content
      (should (string-match-p "refactor" (buffer-string)))
      ;; Active path nodes should have bullet marker
      (should (string-match-p "•" (buffer-string)))
      ;; Label should NOT be in buffer text (it's in margin overlay)
      (should-not (string-match-p "\\[checkpoint\\]" (buffer-string))))))

(ert-deftest pi-coding-agent-test-tree-browser-render-connectors ()
  "Tree connectors appear in rendered buffer at branch points."
  (with-temp-buffer
    (pi-coding-agent-tree-browser-mode)
    (let* ((response (pi-coding-agent-test--read-json-fixture "browse-tree.json"))
           (tree-data (pi-coding-agent--parse-tree response)))
      (setq pi-coding-agent--tree-browser-tree (plist-get tree-data :tree)
            pi-coding-agent--tree-browser-leaf-id (plist-get tree-data :leafId)
            pi-coding-agent--tree-browser-filter "default")
      (pi-coding-agent--tree-browser-rerender)
      (let ((text (buffer-string)))
        ;; Branch connectors should appear
        (should (string-match-p "├─" text))
        (should (string-match-p "└─" text))
        ;; Gutter continuation should appear
        (should (string-match-p "│" text))
        ;; Active branch child line: connector + bullet
        (should (string-match-p "├─ •" text))
        ;; Last branch child: connector without bullet (inactive)
        (should (string-match-p "└─  " text))))))

(ert-deftest pi-coding-agent-test-tree-browser-label-in-margin ()
  "Labels appear as right-margin overlays, not inline text."
  (with-temp-buffer
    (pi-coding-agent-tree-browser-mode)
    (let* ((response (pi-coding-agent-test--read-json-fixture "browse-tree.json"))
           (tree-data (pi-coding-agent--parse-tree response)))
      (setq pi-coding-agent--tree-browser-tree (plist-get tree-data :tree)
            pi-coding-agent--tree-browser-leaf-id (plist-get tree-data :leafId)
            pi-coding-agent--tree-browser-filter "default")
      (pi-coding-agent--tree-browser-rerender)
      ;; Find margin overlays
      (let* ((ovs (overlays-in (point-min) (point-max)))
             (margin-ovs (cl-remove-if-not
                          (lambda (o)
                            (let ((bs (overlay-get o 'before-string)))
                              (and bs (get-text-property 0 'display bs))))
                          ovs)))
        ;; Should have at least one margin overlay (for the labeled node)
        (should (> (length margin-ovs) 0))
        ;; Find the one containing "checkpoint"
        (should (cl-some
                 (lambda (o)
                   (let* ((bs (overlay-get o 'before-string))
                          (display (get-text-property 0 'display bs))
                          (content (cadr display)))
                     (string-match-p "checkpoint" content)))
                 margin-ovs))))))

(ert-deftest pi-coding-agent-test-tree-browser-label-truncation ()
  "Long labels are truncated with ellipsis to fit the right margin."
  (with-temp-buffer
    (pi-coding-agent-tree-browser-mode)
    (let ((tree (vector (list :id "n1" :type "message" :role "user"
                              :preview "hello" :timestamp "2026-01-01T00:00:00Z"
                              :label "this-is-a-very-long-label-name"
                              :children (vector)))))
      (setq pi-coding-agent--tree-browser-tree tree
            pi-coding-agent--tree-browser-leaf-id "n1"
            pi-coding-agent--tree-browser-filter "default")
      (pi-coding-agent--tree-browser-rerender)
      ;; Find the margin overlay
      (let* ((ovs (overlays-in (point-min) (point-max)))
             (margin-ovs (cl-remove-if-not
                          (lambda (o)
                            (let ((bs (overlay-get o 'before-string)))
                              (and bs (get-text-property 0 'display bs))))
                          ovs))
             (content (when margin-ovs
                        (let* ((bs (overlay-get (car margin-ovs) 'before-string))
                               (display (get-text-property 0 'display bs)))
                          (cadr display)))))
        ;; Should exist and be truncated
        (should content)
        ;; Should contain ellipsis
        (should (string-match-p "…" content))
        ;; Total formatted length should fit: [truncated…] ≤ margin width
        (should (<= (length content) pi-coding-agent--tree-margin-width))
        ;; Should NOT contain the full label
        (should-not (string-match-p "this-is-a-very-long-label-name" content))))))

(ert-deftest pi-coding-agent-test-tree-browser-short-label-not-truncated ()
  "Short labels are not truncated."
  (with-temp-buffer
    (pi-coding-agent-tree-browser-mode)
    (let ((tree (vector (list :id "n1" :type "message" :role "user"
                              :preview "hello" :timestamp "2026-01-01T00:00:00Z"
                              :label "ok"
                              :children (vector)))))
      (setq pi-coding-agent--tree-browser-tree tree
            pi-coding-agent--tree-browser-leaf-id "n1"
            pi-coding-agent--tree-browser-filter "default")
      (pi-coding-agent--tree-browser-rerender)
      (let* ((ovs (overlays-in (point-min) (point-max)))
             (margin-ovs (cl-remove-if-not
                          (lambda (o)
                            (let ((bs (overlay-get o 'before-string)))
                              (and bs (get-text-property 0 'display bs))))
                          ovs))
             (content (when margin-ovs
                        (let* ((bs (overlay-get (car margin-ovs) 'before-string))
                               (display (get-text-property 0 'display bs)))
                          (cadr display)))))
        ;; Should contain the full label
        (should (string-match-p "\\[ok\\]" content))
        ;; Should NOT contain ellipsis
        (should-not (string-match-p "…" content))))))

(ert-deftest pi-coding-agent-test-tree-browser-render-empty ()
  "Render empty tree."
  (with-temp-buffer
    (pi-coding-agent-tree-browser-mode)
    (setq pi-coding-agent--tree-browser-tree nil)
    (pi-coding-agent--tree-browser-rerender)
    (should (string-match-p "No conversation tree" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tree-browser-render-user-filter ()
  "User-only filter shows only user messages."
  (with-temp-buffer
    (pi-coding-agent-tree-browser-mode)
    (let* ((response (pi-coding-agent-test--read-json-fixture "browse-tree.json"))
           (tree-data (pi-coding-agent--parse-tree response)))
      (setq pi-coding-agent--tree-browser-tree (plist-get tree-data :tree)
            pi-coding-agent--tree-browser-leaf-id (plist-get tree-data :leafId)
            pi-coding-agent--tree-browser-filter "user-only")
      (pi-coding-agent--tree-browser-rerender)
      ;; Should have user nodes
      (should (string-match-p "you" (buffer-string)))
      ;; Should NOT have assistant nodes
      (should-not (string-match-p "\\bast\\b" (buffer-string))))))

(ert-deftest pi-coding-agent-test-tree-browser-initial-filter ()
  "Tree browser opens with no-tools filter."
  (with-temp-buffer
    (pi-coding-agent-tree-browser-mode)
    (should (equal pi-coding-agent--tree-browser-filter "no-tools"))))

(ert-deftest pi-coding-agent-test-tree-browser-header-line ()
  "Header-line shows filter mode and count."
  (with-temp-buffer
    (pi-coding-agent-tree-browser-mode)
    (let* ((response (pi-coding-agent-test--read-json-fixture "browse-tree.json"))
           (tree-data (pi-coding-agent--parse-tree response)))
      (setq pi-coding-agent--tree-browser-tree (plist-get tree-data :tree)
            pi-coding-agent--tree-browser-leaf-id (plist-get tree-data :leafId)
            pi-coding-agent--tree-browser-filter "no-tools")
      (let ((header (pi-coding-agent--tree-browser-header-line)))
        (should (string-match-p "no-tools" header))
        (should (string-match-p "([0-9]+)" header))))))

;;;; Error States

(ert-deftest pi-coding-agent-test-session-browser-rpc-error ()
  "Session browser shows error when loading failed."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (setq pi-coding-agent--session-browser-error
          "session scan failed")
    (pi-coding-agent--session-browser-rerender)
    (should (string-match-p "Error:" (buffer-string)))
    (should (string-match-p "scan failed" (buffer-string)))))

(ert-deftest pi-coding-agent-test-session-browser-rpc-error-cleared-on-success ()
  "A successful fetch clears a stale error state.
Phase 2: the fetch reads sessions from disk, so the process mock is
vestigial and none is consulted.  The environment is isolated to an
empty sessions root and timers run synchronously so the chunked scan
completes in-call."
  (let ((root (pi-coding-agent-test--make-temp-directory "pi-err-clear")))
    (with-temp-buffer
      (pi-coding-agent-session-browser-mode)
      (setq pi-coding-agent--session-browser-error "some error")
      (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                 (lambda () 'fake))
                ((symbol-function 'pi-coding-agent--session-list-directory)
                 (lambda (&optional _chat-buf) nil))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest args) (apply fn args))))
        (let ((process-environment
               (cons (format "PI_CODING_AGENT_DIR=%s" (directory-file-name root))
                     process-environment)))
          (pi-coding-agent--session-browser-fetch-and-render)))
      (should-not pi-coding-agent--session-browser-error)
      (should-not pi-coding-agent--session-browser-loading)
      (should (string-match-p "No sessions found" (buffer-string))))))

;;;; Tree Find Label

(ert-deftest pi-coding-agent-test-tree-find-label ()
  "Find label for a node ID in the tree."
  (let* ((response (pi-coding-agent-test--read-json-fixture "browse-tree.json"))
         (tree (plist-get (plist-get response :data) :tree)))
    ;; node-7 has label "checkpoint"
    (should (equal (pi-coding-agent--tree-find-label tree "node-7")
                   "checkpoint"))
    ;; node-1 has no label
    (should (null (pi-coding-agent--tree-find-label tree "node-1")))))

;;;; Session Browser Dispatch Transient

(ert-deftest pi-coding-agent-test-session-browser-dispatch-binding ()
  "Session browser binds `?' and `h' to the dispatch transient."
  (should (eq (lookup-key pi-coding-agent-session-browser-mode-map "?")
              'pi-coding-agent-session-browser-dispatch))
  (should (eq (lookup-key pi-coding-agent-session-browser-mode-map "h")
              'pi-coding-agent-session-browser-dispatch)))

(ert-deftest pi-coding-agent-test-session-browser-dispatch-is-transient ()
  "Session browser dispatch is a transient prefix command."
  (should (commandp 'pi-coding-agent-session-browser-dispatch))
  (should (get 'pi-coding-agent-session-browser-dispatch 'transient--prefix)))

(ert-deftest pi-coding-agent-test-session-browser-dispatch-suffixes ()
  "Session browser dispatch wires all keys to the correct commands."
  (let ((expected
         '(("RET" . pi-coding-agent-session-browser-switch)
           ("r"   . pi-coding-agent-session-browser-rename)
           ("s"   . pi-coding-agent-session-browser-cycle-sort)
           ("f"   . pi-coding-agent-session-browser-toggle-named)
           ("t"   . pi-coding-agent-session-browser-toggle-scope)
           ("/"   . pi-coding-agent-session-browser-search)
           ("g"   . pi-coding-agent-browse-refresh)
           ("q"   . quit-window))))
    (dolist (pair expected)
      (let* ((key (car pair))
             (cmd (cdr pair))
             (suffix (transient-get-suffix
                      'pi-coding-agent-session-browser-dispatch key))
             (actual (plist-get (cdr suffix) :command)))
        (should (eq actual cmd))))))

(ert-deftest pi-coding-agent-test-session-dispatch-heading ()
  "Session dispatch heading reflects buffer-local state."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    ;; Default state: scope before sort, no named-only
    (should (equal (pi-coding-agent--session-dispatch-heading)
                   "scope:current │ sort:threaded"))
    ;; All state active
    (setq pi-coding-agent--session-browser-sort "recent"
          pi-coding-agent--session-browser-scope "all"
          pi-coding-agent--session-browser-named-only t)
    (should (equal (pi-coding-agent--session-dispatch-heading)
                   "scope:all │ sort:recent │ named-only"))))

;;;; Tree Browser Dispatch Transient

(ert-deftest pi-coding-agent-test-tree-browser-dispatch-binding ()
  "Tree browser binds `?' and `h' to the dispatch transient."
  (should (eq (lookup-key pi-coding-agent-tree-browser-mode-map "?")
              'pi-coding-agent-tree-browser-dispatch))
  (should (eq (lookup-key pi-coding-agent-tree-browser-mode-map "h")
              'pi-coding-agent-tree-browser-dispatch)))

(ert-deftest pi-coding-agent-test-tree-browser-dispatch-is-transient ()
  "Tree browser dispatch is a transient prefix command."
  (should (commandp 'pi-coding-agent-tree-browser-dispatch))
  (should (get 'pi-coding-agent-tree-browser-dispatch 'transient--prefix)))

(ert-deftest pi-coding-agent-test-tree-browser-dispatch-suffixes ()
  "Tree browser dispatch wires all keys to the correct commands.
The summarize (`S') and abort (`C-c C-k') suffixes were dropped with
the summarize feature (needs navigate_tree RPC)."
  (let ((expected
         '(("RET" . pi-coding-agent-tree-browser-navigate)
           ("l"   . pi-coding-agent-tree-browser-set-label)
           ("f"   . pi-coding-agent-tree-browser-cycle-filter)
           ("/"   . pi-coding-agent-tree-browser-search)
           ("g"   . pi-coding-agent-browse-refresh)
           ("q"   . quit-window))))
    (dolist (pair expected)
      (let* ((key (car pair))
             (cmd (cdr pair))
             (suffix (transient-get-suffix
                      'pi-coding-agent-tree-browser-dispatch key))
             (actual (plist-get (cdr suffix) :command)))
        (should (eq actual cmd))))))

(ert-deftest pi-coding-agent-test-tree-dispatch-heading ()
  "Tree dispatch heading reflects buffer-local filter state."
  (with-temp-buffer
    (pi-coding-agent-tree-browser-mode)
    ;; Default state (initial filter is no-tools)
    (let ((heading (pi-coding-agent--tree-dispatch-heading)))
      (should (string-match-p "filter:no-tools" heading)))
    ;; Change state
    (setq pi-coding-agent--tree-browser-filter "user-only")
    (let ((heading (pi-coding-agent--tree-dispatch-heading)))
      (should (string-match-p "filter:user-only" heading)))))

;;;; Header-Line Help Hint

(ert-deftest pi-coding-agent-test-session-browser-header-line-help-hint ()
  "Session browser header-line includes `?:help' hint."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (setq pi-coding-agent--session-browser-items '((:id "a")))
    (let ((header (pi-coding-agent--session-browser-header-line)))
      (should (string-match-p "?:help" header)))))

(ert-deftest pi-coding-agent-test-tree-browser-header-line-help-hint ()
  "Tree browser header-line includes `?:help' hint."
  (with-temp-buffer
    (pi-coding-agent-tree-browser-mode)
    (let ((header (pi-coding-agent--tree-browser-header-line)))
      (should (string-match-p "?:help" header)))))

;;;; Startup Message

(ert-deftest pi-coding-agent-test-session-browser-startup-message ()
  "Session browser shows help hint message on first creation."
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages)))
              ((symbol-function 'pi-coding-agent--session-browser-fetch-and-render)
               #'ignore)
              ((symbol-function 'pi-coding-agent--get-chat-buffer)
               (lambda () nil))
              ((symbol-function 'pi-coding-agent--session-directory)
               (lambda () "/tmp/pi-test/")))
      (pi-coding-agent-session-browser)
      (unwind-protect
          (should (member "Pi: Press ? for available commands" messages))
        (when-let ((buf (get-buffer
                         (pi-coding-agent--session-browser-buffer-name
                          "/tmp/pi-test/"))))
          (kill-buffer buf))))))

(ert-deftest pi-coding-agent-test-tree-browser-startup-message ()
  "Tree browser shows help hint message on first creation."
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages)))
              ((symbol-function 'pi-coding-agent--tree-browser-fetch-and-render)
               #'ignore)
              ((symbol-function 'pi-coding-agent--get-chat-buffer)
               (lambda () nil))
              ((symbol-function 'pi-coding-agent--session-directory)
               (lambda () "/tmp/pi-test/")))
      (pi-coding-agent-tree-browser)
      (unwind-protect
          (should (member "Pi: Press ? for available commands" messages))
        (when-let ((buf (get-buffer
                         (pi-coding-agent--tree-browser-buffer-name
                          "/tmp/pi-test/"))))
          (kill-buffer buf))))))

;;;; Point Restoration (Phase 0 fix)

(ert-deftest pi-coding-agent-test-session-browser-rerender-restores-point ()
  "Rerender restores point to the same section and column.
pr-145's docstring claim was false: erasing the buffer always moved
point to bob.  Phase 0 restores it by section identity (value match)."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (setq pi-coding-agent--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 42 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/b.jsonl" :name "Session B"
                  :messageCount 20 :modified "2026-02-23T10:00:00Z")
                '(:path "/test/c.jsonl" :name "Session C"
                  :messageCount 10 :modified "2026-02-22T10:00:00Z")))
    (setq pi-coding-agent--session-browser-sort "relevance")
    (pi-coding-agent--session-browser-rerender)
    ;; Move point into session B's line, a few columns past bol
    (goto-char (point-min))
    (search-forward "Session B")
    (beginning-of-line)
    (forward-char 2)
    (let ((column (current-column)))
      (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
      (pi-coding-agent--session-browser-rerender)
      ;; Same section under point, same column
      (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
      (should (= (current-column) column)))))

(ert-deftest pi-coding-agent-test-session-browser-rerender-point-min-when-gone ()
  "Rerender falls back to point-min when the section at point disappears."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (setq pi-coding-agent--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 42 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/b.jsonl" :firstMessage "Unnamed prompt"
                  :messageCount 20 :modified "2026-02-23T10:00:00Z")))
    (setq pi-coding-agent--session-browser-sort "relevance")
    (pi-coding-agent--session-browser-rerender)
    ;; Point on the unnamed session
    (goto-char (point-min))
    (search-forward "Unnamed prompt")
    (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
    ;; Named-only filter removes it; its section is gone after rerender
    (setq pi-coding-agent--session-browser-named-only t)
    (pi-coding-agent--session-browser-rerender)
    (should (= (point) (point-min)))))

(ert-deftest pi-coding-agent-test-browse-rerender-syncs-window-point ()
  "Rerender restores `window-point', not just the buffer's own point.
The final fetch render runs from a timer while ANOTHER window is
selected: `goto-char' inside `with-current-buffer' moves the buffer's
point only, and every window displaying the browser buffer keeps its
own point — which `erase-buffer' already collapsed to bob.  The pane
shows point-at-top although the restore did work on the buffer's own
point (the intermittent instrumentation-vs-pane disagreement from
E2E).  The rerender must also `set-window-point' on live windows
displaying the buffer (same idiom as
`pi-coding-agent--with-scroll-preservation' in ui.el)."
  (let* ((browser (get-buffer-create " *pi-test-browser-winpoint*"))
         (scratch (get-buffer-create " *pi-test-scratch-winpoint*"))
         (w (selected-window))
         (other (split-window))
         (orig-buffer (window-buffer w)))
    (unwind-protect
        (progn
          (set-window-buffer w browser)
          (set-window-buffer other scratch)
          (with-current-buffer browser
            (pi-coding-agent-session-browser-mode)
            (setq pi-coding-agent--session-browser-items
                  (list '(:path "/test/a.jsonl" :name "Session A"
                          :messageCount 42 :modified "2026-02-24T10:00:00Z")
                        '(:path "/test/b.jsonl" :name "Session B"
                          :messageCount 20 :modified "2026-02-23T10:00:00Z")
                        '(:path "/test/c.jsonl" :name "Session C"
                          :messageCount 10 :modified "2026-02-22T10:00:00Z")))
            (setq pi-coding-agent--session-browser-sort "relevance")
            (pi-coding-agent--session-browser-rerender))
          ;; Put W's point on the middle row (relevance order is A, B, C),
          ;; a few columns past bol, while W is selected so the buffer's
          ;; own point follows.
          (select-window w)
          (with-current-buffer browser
            (goto-char (point-min))
            (search-forward "Session B")
            (beginning-of-line)
            (forward-char 2))
          ;; Sanity: W's point sits on session B's section.
          (should (equal (oref (with-current-buffer browser
                                 (magit-section-at (window-point w)))
                               value)
                         "/test/b.jsonl"))
          ;; Timer-like context: the rerender runs with ANOTHER window
          ;; selected (the mechanism is window selection, not the timer).
          (select-window other)
          (should-not (eq (selected-window) w))
          (with-current-buffer browser
            (pi-coding-agent--session-browser-rerender))
          ;; W's window-point must sit on the same section again (the
          ;; section is looked up by buffer position, in the browser
          ;; buffer — `magit-section-at' reads text properties in the
          ;; current buffer).
          (let ((pos (window-point w)))
            (should (equal (oref (with-current-buffer browser
                                   (magit-section-at pos))
                                 value)
                           "/test/b.jsonl"))
            (should (= (with-current-buffer browser
                         (save-excursion (goto-char pos) (current-column)))
                       2))))
      (delete-other-windows)
      (set-window-buffer (selected-window) orig-buffer)
      (kill-buffer browser)
      (kill-buffer scratch))))

(ert-deftest pi-coding-agent-test-session-browser-fetch-preserves-point ()
  "The full fetch cycle (`g' refresh) keeps point on the same row.
`--session-browser-fetch-and-render' renders an intermediate loading
state with no session sections before the final items render; point
must survive the whole cycle, not just a plain rerender (E2E defect
A4: `g' dropped point to bob because the loading render's rerender
lost the captured section ident)."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (let ((items (list '(:path "/test/a.jsonl" :name "Session A"
                         :messageCount 42 :modified "2026-02-24T10:00:00Z")
                       '(:path "/test/b.jsonl" :name "Session B"
                         :messageCount 20 :modified "2026-02-23T10:00:00Z")
                       '(:path "/test/c.jsonl" :name "Session C"
                         :messageCount 10 :modified "2026-02-22T10:00:00Z"))))
      (setq pi-coding-agent--session-browser-items items
            pi-coding-agent--session-browser-sort "relevance")
      (pi-coding-agent--session-browser-rerender)
      ;; Point on the middle row (relevance order is A, B, C), a few
      ;; columns past bol
      (goto-char (point-min))
      (search-forward "Session B")
      (beginning-of-line)
      (forward-char 2)
      (let ((column (current-column)))
        (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
        ;; Refresh: the scan returns the SAME items, synchronously
        (cl-letf (((symbol-function 'pi-coding-agent--browse-load-sessions)
                   (lambda (_scope callback)
                     (funcall callback items nil)))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args)
                     (apply fn args))))
          (pi-coding-agent--session-browser-fetch-and-render))
        (should-not pi-coding-agent--session-browser-loading)
        ;; Same section under point, same column, after the final render
        (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
        (should (= (current-column) column))))))

(ert-deftest pi-coding-agent-test-session-browser-fetch-point-min-when-gone ()
  "The fetch cycle falls back to point-min when the row at point is gone.
A refresh whose new item set no longer contains the pointed-at session
must leave point at bob, not on a stale neighbor — the same fallback a
plain rerender already guarantees."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (setq pi-coding-agent--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 42 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/b.jsonl" :name "Session B"
                  :messageCount 20 :modified "2026-02-23T10:00:00Z")
                '(:path "/test/c.jsonl" :name "Session C"
                  :messageCount 10 :modified "2026-02-22T10:00:00Z")))
    (setq pi-coding-agent--session-browser-sort "relevance")
    (pi-coding-agent--session-browser-rerender)
    ;; Point on the middle row
    (goto-char (point-min))
    (search-forward "Session B")
    (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
    ;; Refresh returns a set without session B (the named-only effect,
    ;; via a different item set)
    (cl-letf (((symbol-function 'pi-coding-agent--browse-load-sessions)
               (lambda (_scope callback)
                 (funcall callback
                          (list '(:path "/test/a.jsonl" :name "Session A"
                                  :messageCount 42 :modified "2026-02-24T10:00:00Z")
                                '(:path "/test/c.jsonl" :name "Session C"
                                  :messageCount 10 :modified "2026-02-22T10:00:00Z"))
                          nil)))
              ((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args)
                 (apply fn args))))
      (pi-coding-agent--session-browser-fetch-and-render))
    (should (= (point) (point-min)))))

(ert-deftest pi-coding-agent-test-session-browser-refresh-during-load-preserves-point ()
  "Pressing `g' while a load is in flight keeps point on its row.
A refresh issued during another refresh used to lose point twice
over: the first fetch's loading render already destroyed the session
sections (point sits on the bare loading line under the root
section, so the second fetch captures no anchor), and the
fetch token drops the older scan before its callback ever renders.
The surviving fetch's final render then has neither a fresh anchor
nor a fallback, and point lands at bob.  Point must survive a refresh
issued during another refresh."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (let ((items (list '(:path "/test/a.jsonl" :name "Session A"
                         :messageCount 42 :modified "2026-02-24T10:00:00Z")
                       '(:path "/test/b.jsonl" :name "Session B"
                         :messageCount 20 :modified "2026-02-23T10:00:00Z")
                       '(:path "/test/c.jsonl" :name "Session C"
                         :messageCount 10 :modified "2026-02-22T10:00:00Z")))
          (in-flight-callback nil))
      (setq pi-coding-agent--session-browser-items items
            pi-coding-agent--session-browser-sort "relevance")
      (pi-coding-agent--session-browser-rerender)
      ;; Point on the middle row (relevance order is A, B, C), a few
      ;; columns past bol
      (goto-char (point-min))
      (search-forward "Session B")
      (beginning-of-line)
      (forward-char 2)
      (let ((column (current-column)))
        (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
        (cl-letf (((symbol-function 'pi-coding-agent--browse-load-sessions)
                   ;; Fetch A: return control with the scan mid-flight —
                   ;; capture the callback, funcall nothing yet.
                   (lambda (_scope callback)
                     (setq in-flight-callback callback)))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args)
                     (apply fn args))))
          (pi-coding-agent--session-browser-fetch-and-render)
          ;; The first fetch is still loading; its loading render left
          ;; no session sections to anchor to.
          (should pi-coding-agent--session-browser-loading)
          (should (string-match-p "Loading" (buffer-string)))
          (should in-flight-callback)
          ;; While loading, press `g' again: fetch B reports the same
          ;; items synchronously (and, as with the real fetch token,
          ;; fetch A's callback never runs — it is dropped, not queued).
          (cl-letf (((symbol-function 'pi-coding-agent--browse-load-sessions)
                     (lambda (_scope callback)
                       (funcall callback items nil))))
            (pi-coding-agent--session-browser-fetch-and-render))
          (should-not pi-coding-agent--session-browser-loading))
        ;; The final render lands on the same middle row.
        (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
        (should (= (current-column) column))))))

(ert-deftest pi-coding-agent-test-session-browser-fetch-renders-in-browser-buffer ()
  "The fetch callback renders in the browser buffer, not the caller's.
The real async scan reports back from a timer in whatever buffer
happens to be current; the final rerender must land in the browser
buffer (latent defect: it used to run outside `with-current-buffer',
leaving the browser stuck on its loading state)."
  (let ((items (list '(:path "/test/a.jsonl" :name "Session A"
                       :messageCount 42 :modified "2026-02-24T10:00:00Z")))
        (other (get-buffer-create " *pi-test-fetch-other*")))
    (unwind-protect
        (with-temp-buffer
          (pi-coding-agent-session-browser-mode)
          (cl-letf (((symbol-function 'pi-coding-agent--browse-load-sessions)
                     (lambda (_scope callback)
                       ;; Callback fires with some OTHER buffer current.
                       (with-current-buffer other
                         (funcall callback items nil))))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args)
                       (apply fn args))))
            (pi-coding-agent--session-browser-fetch-and-render))
          ;; The browser buffer got the rows and cleared loading...
          (should-not pi-coding-agent--session-browser-loading)
          (should (string-match-p "Session A" (buffer-string)))
          ;; ...and the other buffer got no render.
          (should-not (string-match-p "Session A"
                                      (with-current-buffer other
                                        (buffer-string)))))
      (kill-buffer other))))

(ert-deftest pi-coding-agent-test-tree-browser-fetch-renders-in-browser-buffer ()
  "The tree fetch callback renders in the browser buffer, not the caller's.
Same latent defect as the session browser: the async load reports from
a timer in whatever buffer is current."
  (let* ((tree-data (pi-coding-agent--parse-tree
                     (pi-coding-agent-test--read-json-fixture "browse-tree.json")))
         (other (get-buffer-create " *pi-test-tree-other*")))
    (unwind-protect
        (with-temp-buffer
          (pi-coding-agent-tree-browser-mode)
          (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                     (lambda () 'fake))
                    ((symbol-function 'pi-coding-agent--browse-load-tree)
                     (lambda (callback)
                       ;; Callback fires with some OTHER buffer current.
                       (with-current-buffer other
                         (funcall callback
                                  (plist-get tree-data :tree)
                                  (plist-get tree-data :leafId)))))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args)
                       (apply fn args))))
            (pi-coding-agent--tree-browser-fetch-and-render))
          ;; The browser buffer got the tree and cleared loading...
          (should-not pi-coding-agent--tree-browser-loading)
          (should (string-match-p "Actually" (buffer-string)))
          ;; ...and the other buffer got no render.
          (should-not (string-match-p "Actually"
                                      (with-current-buffer other
                                        (buffer-string)))))
      (kill-buffer other))))

(ert-deftest pi-coding-agent-test-tree-browser-rerender-restores-point ()
  "Tree rerender keeps point on the same node across filter changes."
  (with-temp-buffer
    (pi-coding-agent-tree-browser-mode)
    (let* ((response (pi-coding-agent-test--read-json-fixture "browse-tree.json"))
           (tree-data (pi-coding-agent--parse-tree response)))
      (setq pi-coding-agent--tree-browser-tree (plist-get tree-data :tree)
            pi-coding-agent--tree-browser-leaf-id (plist-get tree-data :leafId)
            pi-coding-agent--tree-browser-filter "default")
      (pi-coding-agent--tree-browser-rerender)
      ;; node-4 (user message) survives the no-tools filter
      (goto-char (point-min))
      (search-forward "Actually")
      (should (equal (oref (magit-current-section) value) "node-4"))
      (setq pi-coding-agent--tree-browser-filter "no-tools")
      (pi-coding-agent--tree-browser-rerender)
      (should (equal (oref (magit-current-section) value) "node-4")))))

;;;; Phase 0 Stub Seams

(ert-deftest pi-coding-agent-test-browse-stub-loaders-render-empty-states ()
  "Phase 0 stub seam callbacks render empty states without errors.
The session browser needs no live process (the Phase 2 disk-scan
relaxation); the tree browser keeps its process guard until Phase 3."
  (let ((session-buf (generate-new-buffer " *test-sessions*"))
        (tree-buf (generate-new-buffer " *test-tree*"))
        (root (pi-coding-agent-test--make-temp-directory "pi-stub-root")))
    (unwind-protect
        (progn
          (with-current-buffer session-buf
            (pi-coding-agent-session-browser-mode)
            ;; No --get-process mock: reading from disk needs no process.
            (let ((default-directory root)
                  (process-environment
                   (cons (format "PI_CODING_AGENT_DIR=%s"
                                (directory-file-name root))
                         process-environment)))
              (cl-letf (((symbol-function 'pi-coding-agent--session-list-directory)
                         (lambda (&optional _chat-buf) nil))
                        ((symbol-function 'run-at-time)
                         (lambda (_secs _repeat fn &rest args)
                           (apply fn args))))
                (pi-coding-agent--session-browser-fetch-and-render)))
            (should-not pi-coding-agent--session-browser-loading)
            (should-not pi-coding-agent--session-browser-error)
            (should (string-match-p "No sessions found" (buffer-string))))
          (with-current-buffer tree-buf
            (pi-coding-agent-tree-browser-mode)
            (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                       (lambda () 'fake)))
              (pi-coding-agent--tree-browser-fetch-and-render)
              (should-not pi-coding-agent--tree-browser-loading)
              (should (string-match-p "No conversation tree" (buffer-string))))))
      (kill-buffer session-buf)
      (kill-buffer tree-buf))))

(ert-deftest pi-coding-agent-test-browse-stub-actions-signal-user-error ()
  "Action seams signal `user-error' instead of pretending to succeed.
The switch seam's Phase 2 contract is the no-session error when no
chat buffer is linked; navigate and label stay user-error stubs until
Phases 3-4."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (should (equal (error-message-string
                    (should-error
                     (pi-coding-agent--browse-switch-session "/test/a.jsonl")
                     :type 'user-error))
                   "No pi session to switch to")))
  (should-error
      (pi-coding-agent--browse-navigate "node-1")
    :type 'user-error)
  (should-error
      (pi-coding-agent--browse-set-label "node-1" "label")
    :type 'user-error))

;;;; Phase 2: Raw Session File Helpers

(defconst pi-coding-agent-test--browse-timestamp "2026-03-02T10:00:00.000Z"
  "Fixed entry timestamp for browse test session lines.")

(defconst pi-coding-agent-test--iso-second-re
  "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z\\'"
  "Regexp for a second-resolution UTC ISO-8601 timestamp.")

(defun pi-coding-agent-test--jsonl-line (type id parent &rest payload)
  "Return a raw JSONL line for an entry of TYPE, ID, and PARENT id.
PARENT is an id string or nil (JSON null).  PAYLOAD is the plist tail
(:message, :targetId, :name, ...)."
  (json-encode (append (list :type type :id id :parentId parent
                             :timestamp pi-coding-agent-test--browse-timestamp)
                       payload)))

(defun pi-coding-agent-test--user-line (id parent text)
  "Return a raw user message JSONL line with content TEXT."
  (pi-coding-agent-test--jsonl-line
   "message" id parent :message `(:role "user" :content ,text)))

(defun pi-coding-agent-test--write-session-lines (path lines &optional omit-final-newline)
  "Write raw string LINES to PATH, separated by single newlines.
OMIT-FINAL-NEWLINE skips the trailing newline, producing the shape of a
crashed or hand-edited session file (rename must re-add the separator)."
  (with-temp-file path
    (when lines
      (insert (mapconcat #'identity lines "\n"))
      (unless omit-final-newline (insert "\n")))))

(defun pi-coding-agent-test--file-contents (path)
  "Return the raw contents of PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun pi-coding-agent-test--make-session-header (id &rest extra)
  "Return a raw session header line with id ID and EXTRA plist tail."
  (json-encode (append (list :type "session" :version 3 :id id
                             :timestamp pi-coding-agent-test--browse-timestamp
                             :cwd "/home/fake/a")
                       extra)))

(defmacro pi-coding-agent-test--with-browse-link (chat-buf &rest body)
  "Run BODY in a session-browser buffer linked to CHAT-BUF."
  (declare (indent 1) (debug (sexp body)))
  `(with-temp-buffer
     (pi-coding-agent-session-browser-mode)
     (setq pi-coding-agent--chat-buffer ,chat-buf)
     ,@body))

;;;; Phase 2: Disk Scan and Chunked Loading

(ert-deftest pi-coding-agent-test-scan-discovers-tree ()
  "--browse-load-sessions scans the sessions tree from disk.
scope=all finds every munged --…-- directory under the sessions root,
threads forks through :parentSessionPath, reads names from
session_info, and skips non-session JSONL files, .subagents sidecars,
and non-munged directories.  scope=current scans one directory."
  (let* ((root (pi-coding-agent-test--make-temp-directory "pi-scan-root"))
         (sessions (expand-file-name "sessions" root))
         (dir-a (expand-file-name "--home-fake-a--" sessions))
         (dir-b (expand-file-name "--home-fake-b--" sessions))
         (subagents (expand-file-name ".subagents" dir-a))
         (stray (expand-file-name "straydir" sessions))
         (root-path (expand-file-name "root.jsonl" dir-a))
         (fork-path (expand-file-name "fork.jsonl" dir-a))
         (other-path (expand-file-name "other.jsonl" dir-b))
         (broken-path (expand-file-name "broken.jsonl" dir-b))
         (sub-path (expand-file-name "sub.jsonl" subagents))
         (stray-path (expand-file-name "stray.jsonl" stray))
         (calls nil))
    (make-directory dir-a t)
    (make-directory dir-b t)
    (make-directory subagents t)
    (make-directory stray t)
    (pi-coding-agent-test--write-session-lines
     root-path
     (list (pi-coding-agent-test--make-session-header "sid-root")
           (pi-coding-agent-test--user-line "m1" nil "fix the parser")
           (pi-coding-agent-test--jsonl-line
            "message" "m2" "m1"
            :message '(:role "toolResult" :toolCallId "tc1" :toolName "read"))
           (pi-coding-agent-test--jsonl-line
            "message" "m3" "m2"
            :message '(:role "assistant" :content "done"))
           (pi-coding-agent-test--jsonl-line
            "session_info" "s1" "m3" :name "Root work")))
    (pi-coding-agent-test--write-session-lines
     fork-path
     (list (pi-coding-agent-test--make-session-header
            "sid-fork" :parentSession root-path)
           (pi-coding-agent-test--user-line "f1" nil "try the other way")))
    (pi-coding-agent-test--write-session-lines
     other-path
     (list (pi-coding-agent-test--make-session-header "sid-other")
           (pi-coding-agent-test--user-line "o1" nil "unrelated work")))
    ;; Decoy: a .jsonl file that is not a session (no header line).
    (pi-coding-agent-test--write-session-lines
     broken-path
     (list (pi-coding-agent-test--user-line "x1" nil "decoy")))
    ;; Valid sessions in excluded locations: a .subagents sidecar (the
    ;; scan is single-level inside munged dirs) and a non-munged dir.
    (pi-coding-agent-test--write-session-lines
     sub-path (list (pi-coding-agent-test--make-session-header "sid-sub")))
    (pi-coding-agent-test--write-session-lines
     stray-path (list (pi-coding-agent-test--make-session-header "sid-stray")))
    (with-temp-buffer
      (pi-coding-agent-session-browser-mode)
      (let ((default-directory root)
            (process-environment
             (cons (format "PI_CODING_AGENT_DIR=%s" (directory-file-name root))
                   process-environment)))
        (cl-letf (((symbol-function 'pi-coding-agent--session-list-directory)
                   (lambda (&optional _chat-buf) nil))
                  ;; Synchronous timers: the chunked scan completes here.
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args) (apply fn args))))
          (pi-coding-agent--browse-load-sessions
           "all" (lambda (items error) (push (list items error) calls))))
        (should (eq (length calls) 1))
        (pcase-let ((`(,items ,error) (car calls)))
          (should-not error)
          (should (= (length items) 3))
          (let ((paths (mapcar (lambda (item) (plist-get item :path)) items)))
            (should (member root-path paths))
            (should (member fork-path paths))
            (should (member other-path paths))
            (should-not (member broken-path paths))
            (should-not (member sub-path paths))
            (should-not (member stray-path paths)))
          (let ((root-item (cl-find root-path items
                                    :key (lambda (i) (plist-get i :path))
                                    :test #'equal))
                (fork-item (cl-find fork-path items
                                    :key (lambda (i) (plist-get i :path))
                                    :test #'equal)))
            (should root-item)
            (should fork-item)
            (should (equal (plist-get root-item :id) "sid-root"))
            (should (equal (plist-get root-item :cwd) "/home/fake/a"))
            (should (equal (plist-get root-item :created)
                           pi-coding-agent-test--browse-timestamp))
            (should (equal (plist-get root-item :name) "Root work"))
            (should (equal (plist-get root-item :firstMessage) "fix the parser"))
            (should (= (plist-get root-item :messageCount) 3))
            (should (string-match-p pi-coding-agent-test--iso-second-re
                                    (plist-get root-item :modified)))
            ;; The fork threads to its parent session file.
            (should (equal (plist-get fork-item :parentSessionPath) root-path))
            (should-not (plist-get fork-item :name))))
        ;; scope=current scans exactly one directory: the menu-supplied
        ;; session list directory.
        (setq calls nil)
        (cl-letf (((symbol-function 'pi-coding-agent--session-list-directory)
                   (lambda (&optional _chat-buf) dir-a))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args) (apply fn args))))
          (pi-coding-agent--browse-load-sessions
           "current" (lambda (items error) (push (list items error) calls))))
        (pcase-let ((`(,items ,error) (car calls)))
          (should-not error)
          (should (equal (sort (mapcar (lambda (i) (plist-get i :path)) items)
                               #'string<)
                         (sort (list root-path fork-path) #'string<))))))))

(ert-deftest pi-coding-agent-test-load-sessions-chunked ()
  "--browse-load-sessions chunks long scans and reports once.
The per-file reader is slowed past the 25 ms slice budget so the scan
spans several slices.  Synchronous timers deliver exactly one final
callback with every item, and a superseded fetch's callback is dropped
by the fetch token."
  (let* ((root (pi-coding-agent-test--make-temp-directory "pi-chunk-root"))
         (sessions (expand-file-name "sessions" root))
         (dir (expand-file-name "--home-fake-a--" sessions))
         (paths nil))
    (make-directory dir t)
    (dotimes (i 60)
      (let ((path (expand-file-name (format "s%03d.jsonl" i) dir)))
        (push path paths)
        (pi-coding-agent-test--write-session-lines
         path (list (pi-coding-agent-test--make-session-header
                     (format "sid-%03d" i))))))
    (setq paths (nreverse paths))
    (with-temp-buffer
      (pi-coding-agent-session-browser-mode)
      (let ((default-directory root)
            (process-environment
             (cons (format "PI_CODING_AGENT_DIR=%s" (directory-file-name root))
                   process-environment)))
        (cl-letf (((symbol-function 'pi-coding-agent--session-list-directory)
                   (lambda (&optional _chat-buf) nil))
                  ;; 2 ms per file: 60 files need several 25 ms slices.
                  ((symbol-function 'pi-coding-agent-jsonl-read-session-info)
                   (lambda (path)
                     (sleep-for 0 2)
                     (list :path path
                           :id (file-name-nondirectory path)
                           :cwd "/home/fake/a"
                           :created pi-coding-agent-test--browse-timestamp
                           :modified "2026-03-02T10:00:00Z"
                           :messageCount 0))))
          ;; Synchronous timers: one final callback, all 60 items.
          (let ((calls nil))
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_secs _repeat fn &rest args) (apply fn args))))
              (pi-coding-agent--browse-load-sessions
               "all" (lambda (items error) (push (list items error) calls))))
            (should (eq (length calls) 1))
            (pcase-let ((`(,items ,error) (car calls)))
              (should-not error)
              (should (= (length items) 60))
              (should (equal (sort (mapcar (lambda (i) (plist-get i :path)) items)
                                   #'string<)
                             (sort (copy-sequence paths) #'string<)))))
          ;; Deferred timers: the older fetch is superseded before any
          ;; slice runs, so its callback is dropped by the fetch token.
          (let ((calls-a nil) (calls-b nil) (queue nil))
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_secs _repeat fn &rest args)
                         (push (cons fn args) queue))))
              (pi-coding-agent--browse-load-sessions
               "all" (lambda (items error) (push (list items error) calls-a)))
              (pi-coding-agent--browse-load-sessions
               "all" (lambda (items error) (push (list items error) calls-b)))
              (while queue
                (let ((job (pop queue)))
                  (apply (car job) (cdr job)))))
            (should-not calls-a)
            (should (eq (length calls-b) 1))
            (pcase-let ((`(,items ,error) (car calls-b)))
              (should-not error)
              (should (= (length items) 60))))
          ;; Mid-flight supersession: A completes one slice (~12 files)
          ;; before B supersedes it; the token still drops A at its next
          ;; slice boundary, and B reports alone with every item.
          (let ((calls-a nil) (calls-b nil) (queue nil))
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_secs _repeat fn &rest args)
                         (push (cons fn args) queue))))
              (pi-coding-agent--browse-load-sessions
               "all" (lambda (items error) (push (list items error) calls-a)))
              (let ((job (pop queue)))
                (apply (car job) (cdr job)))
              (pi-coding-agent--browse-load-sessions
               "all" (lambda (items error) (push (list items error) calls-b)))
              (while queue
                (let ((job (pop queue)))
                  (apply (car job) (cdr job)))))
            (should-not calls-a)
            (should (eq (length calls-b) 1))
            (pcase-let ((`(,items ,error) (car calls-b)))
              (should-not error)
              (should (= (length items) 60))
              (should (equal (sort (mapcar (lambda (i) (plist-get i :path)) items)
                                   #'string<)
                             (sort (copy-sequence paths) #'string<))))))))))

(ert-deftest pi-coding-agent-test-load-sessions-error-as-string ()
  "Directory resolution failures surface as an error string, not a signal."
  (let ((calls nil))
    (with-temp-buffer
      (pi-coding-agent-session-browser-mode)
      (cl-letf (((symbol-function 'pi-coding-agent--session-list-directory)
                 (lambda (&optional _chat-buf)
                   (signal 'file-error '("Cannot access sessions directory"))))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest args) (apply fn args))))
        (pi-coding-agent--browse-load-sessions
         "current" (lambda (items error) (push (list items error) calls))))
      (should (eq (length calls) 1))
      (pcase-let ((`(,items ,error) (car calls)))
        (should-not items)
        (should (stringp error))
        (should (string-match-p "Cannot list sessions" error))))))

;;;; Phase 2: Fetch Relaxation

(ert-deftest pi-coding-agent-test-fetch-without-process ()
  "The session browser fetch proceeds without a live pi process.
Phase 2 reads sessions from disk, so the Phase 0 no-process guard is
gone for the session browser (the tree browser keeps it until Phase 3)."
  (let ((root (pi-coding-agent-test--make-temp-directory "pi-noproc-root")))
    (with-temp-buffer
      (pi-coding-agent-session-browser-mode)
      (let ((default-directory root)
            (process-environment
             (cons (format "PI_CODING_AGENT_DIR=%s" (directory-file-name root))
                   process-environment)))
        (cl-letf (((symbol-function 'pi-coding-agent--session-list-directory)
                   (lambda (&optional _chat-buf) nil))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args) (apply fn args))))
          (pi-coding-agent--session-browser-fetch-and-render)))
      (should-not pi-coding-agent--session-browser-loading)
      (should-not pi-coding-agent--session-browser-error)
      (should (string-match-p "No sessions found" (buffer-string))))))

;;;; Phase 2: Switch

(ert-deftest pi-coding-agent-test-switch-calls-resume ()
  "--browse-switch-session guards, then delegates to the resume flow.
The busy guard runs first with (CHAT-BUF \"switch\"); the delegation
receives (PROC CHAT-BUF PATH) verbatim."
  (let* ((chat-buf (generate-new-buffer " *test-switch-chat*"))
         (proc (start-process "pi-switch-test" nil "sleep" "30"))
         (ready-calls nil)
         (resume-calls nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pi-coding-agent--process proc))
          (pi-coding-agent-test--with-browse-link chat-buf
            (cl-letf (((symbol-function 'pi-coding-agent--session-transition-ready-p)
                       (lambda (chat-buf action)
                         (push (list chat-buf action) ready-calls)
                         t))
                      ((symbol-function 'pi-coding-agent--resume-selected-session)
                       (lambda (proc chat-buf path)
                         (push (list proc chat-buf path) resume-calls))))
              (pi-coding-agent--browse-switch-session "/tmp/some-session.jsonl")))
          (should (equal ready-calls (list (list chat-buf "switch"))))
          (should (equal resume-calls
                         (list (list proc chat-buf "/tmp/some-session.jsonl")))))
      (delete-process proc)
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-switch-busy-guard ()
  "A busy chat session blocks the switch before any resume attempt."
  (let* ((chat-buf (generate-new-buffer " *test-busy-chat*"))
         (proc (start-process "pi-busy-test" nil "sleep" "30"))
         (resume-calls nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pi-coding-agent--process proc))
          (pi-coding-agent-test--with-browse-link chat-buf
            (cl-letf (((symbol-function 'pi-coding-agent--session-transition-ready-p)
                       (lambda (&rest _) nil))
                      ((symbol-function 'pi-coding-agent--resume-selected-session)
                       (lambda (&rest _) (push t resume-calls))))
              ;; Returns quietly: the guard reports the reason itself.
              (pi-coding-agent--browse-switch-session "/tmp/some-session.jsonl")))
          (should-not resume-calls))
      (delete-process proc)
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-switch-no-session ()
  "Switching with no linked chat session signals a `user-error'."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (should (equal (error-message-string
                    (should-error
                     (pi-coding-agent--browse-switch-session "/test/a.jsonl")
                     :type 'user-error))
                   "No pi session to switch to"))))

(ert-deftest pi-coding-agent-test-quit-when-settled ()
  "--browse-quit-when-settled waits out the transition, then quits only
when the chat landed on the requested session file AND the window still
shows a session browser.  Timers run synchronously; the transition looks
busy once, then settles.  A repurposed window, a dead chat buffer, and a
landed-elsewhere state all leave the window alone."
  (let* ((chat-buf (generate-new-buffer " *test-settled-chat*"))
         (win (selected-window))
         (orig-buf (window-buffer win))
         (browser-buf (generate-new-buffer " *test-settled-browser*"))
         (other-buf (generate-new-buffer " *test-settled-other*"))
         (path "/tmp/target-session.jsonl"))
    (unwind-protect
        (let ((quit-calls nil) (polls 0))
          (with-current-buffer browser-buf
            (pi-coding-agent-session-browser-mode))
          (set-window-buffer win browser-buf)
          ;; Settled onto the target: one busy poll, then quit-window.
          (with-current-buffer chat-buf
            (setq pi-coding-agent--state (list :session-file path)))
          (cl-letf (((symbol-function 'pi-coding-agent--session-transition-active-p)
                     (lambda (&optional _chat-buf)
                       (setq polls (1+ polls))
                       (<= polls 1)))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args) (apply fn args)))
                    ((symbol-function 'quit-window)
                     (lambda (&rest args) (push args quit-calls))))
            (pi-coding-agent--browse-quit-when-settled chat-buf win path))
          (should (>= polls 2))
          (should (equal quit-calls (list (list nil win))))
          ;; Settled elsewhere: the browser stays open.
          (setq quit-calls nil polls 0)
          (with-current-buffer chat-buf
            (setq pi-coding-agent--state (list :session-file "/tmp/other.jsonl")))
          (cl-letf (((symbol-function 'pi-coding-agent--session-transition-active-p)
                     (lambda (&optional _chat-buf)
                       (setq polls (1+ polls))
                       (<= polls 1)))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args) (apply fn args)))
                    ((symbol-function 'quit-window)
                     (lambda (&rest args) (push args quit-calls))))
            (pi-coding-agent--browse-quit-when-settled chat-buf win path))
          (should (>= polls 2))
          (should-not quit-calls)
          ;; Window repurposed mid-poll (browse buffer killed): landing on
          ;; the target must NOT quit whatever the window shows now.
          (setq quit-calls nil polls 0)
          (with-current-buffer chat-buf
            (setq pi-coding-agent--state (list :session-file path)))
          (set-window-buffer win other-buf)
          (cl-letf (((symbol-function 'pi-coding-agent--session-transition-active-p)
                     (lambda (&optional _chat-buf)
                       (setq polls (1+ polls))
                       (<= polls 1)))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args) (apply fn args)))
                    ((symbol-function 'quit-window)
                     (lambda (&rest args) (push args quit-calls))))
            (pi-coding-agent--browse-quit-when-settled chat-buf win path))
          (should (>= polls 2))
          (should-not quit-calls)
          ;; Dead chat buffer: the poll ends quietly, no signal, no quit.
          (setq quit-calls nil)
          (set-window-buffer win browser-buf)
          (kill-buffer chat-buf)
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args) (apply fn args)))
                    ((symbol-function 'quit-window)
                     (lambda (&rest args) (push args quit-calls))))
            (pi-coding-agent--browse-quit-when-settled chat-buf win path))
          (should-not quit-calls))
      (set-window-buffer win orig-buf)
      (kill-buffer browser-buf)
      (kill-buffer other-buf)
      (when (buffer-live-p chat-buf) (kill-buffer chat-buf)))))

;;;; Phase 2: Rename

(defun pi-coding-agent-test--rename-at-point (item chat-buf input)
  "Run `session-browser-rename' with INPUT at ITEM's section.
ITEM is a session plist (its :name locates the section); CHAT-BUF is
the browse buffer's chat link.  The post-rename refresh is stubbed
out; callers mock the rename seams they assert on."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (setq pi-coding-agent--chat-buffer chat-buf
          pi-coding-agent--session-browser-items (list item))
    (pi-coding-agent--session-browser-rerender)
    (goto-char (point-min))
    (search-forward (plist-get item :name))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) input))
              ((symbol-function 'pi-coding-agent--session-browser-fetch-and-render)
               #'ignore))
      (pi-coding-agent-session-browser-rename))))

(ert-deftest pi-coding-agent-test-rename-other-session-appends ()
  "Renaming a non-current session appends exactly one session_info line.
The line carries a fresh 8-hex id, parents to the id of the file's last
line, a UTC ISO timestamp no older than the rename, and the cleaned
name.  A missing trailing newline gets a separator; prior bytes stay
byte-for-byte intact."
  (let* ((dir (pi-coding-agent-test--make-temp-directory "pi-rename-append"))
         (path (expand-file-name "target.jsonl" dir))
         (current-path (expand-file-name "current.jsonl" dir))
         (before-lines
          (list (pi-coding-agent-test--make-session-header "sid-target")
                (pi-coding-agent-test--user-line "m1" nil "investigate the flaky test")
                (pi-coding-agent-test--jsonl-line
                 "message" "m2" "m1"
                 :message '(:role "assistant" :content "found it"))
                (pi-coding-agent-test--jsonl-line
                 "session_info" "s1" "m2" :name "Old name")))
         (chat-buf (generate-new-buffer " *test-rename-chat*"))
         (start-iso (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t)))
    ;; No trailing newline: the append must add the separator itself.
    (pi-coding-agent-test--write-session-lines path before-lines t)
    (pi-coding-agent-test--write-session-lines
     current-path (list (pi-coding-agent-test--make-session-header "sid-current")))
    (unwind-protect
        (let ((before (pi-coding-agent-test--file-contents path)))
          (with-current-buffer chat-buf
            (setq pi-coding-agent--state (list :session-file current-path)))
          (pi-coding-agent-test--rename-at-point
           (list :path path :name "Old name" :messageCount 2
                 :modified "2026-03-02T10:00:00Z")
           chat-buf
           "  Renamed\nSession  ")
          (let* ((after (pi-coding-agent-test--file-contents path)))
            ;; Exactly one line was appended, after a separator.
            (should-not (equal after before))
            (let* ((appended (car (split-string (substring after (length before))
                                                 "\n" t)))
                   (entry (json-parse-string appended :object-type 'plist)))
              (should (string-prefix-p before after))
              (should (string-suffix-p (concat appended "\n") after))
              (should (equal (plist-get entry :type) "session_info"))
              (should (string-match-p "\\`[0-9a-f]\\{8\\}\\'"
                                     (plist-get entry :id)))
              (should (not (member (plist-get entry :id)
                                   '("m1" "m2" "s1"))))
              ;; parentId is the id of the last line before the append.
              (should (equal (plist-get entry :parentId) "s1"))
              ;; The name is trimmed with newlines collapsed.
              (should (equal (plist-get entry :name) "Renamed Session"))
              (let ((ts (plist-get entry :timestamp)))
                (should (string-match-p
                         "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\.[0-9]\\{3\\}Z\\'"
                         ts))
                (should (not (string< ts start-iso)))))))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-rename-append-garbage-tail ()
  "Renaming a session with a garbage final line parents past the garbage.
pi's loader skips malformed lines, so the append's :parentId must be the
id of the last PARSEABLE line — parenting to the garbage (or to nil)
would detach the whole conversation from the reload context.  The
garbage bytes stay byte-for-byte intact."
  (let* ((dir (pi-coding-agent-test--make-temp-directory "pi-rename-garbage"))
         (path (expand-file-name "torn.jsonl" dir))
         (garbage "{\"type\":\"message\",\"id\":\"torn\",\"paren")
         (chat-buf (generate-new-buffer " *test-garbage-chat*")))
    (pi-coding-agent-test--write-session-lines
     path
     (list (pi-coding-agent-test--make-session-header "sid-g")
           (pi-coding-agent-test--user-line "g1" nil "check the flaky test")
           (pi-coding-agent-test--jsonl-line
            "message" "g2" "g1"
            :message '(:role "assistant" :content "fixed"))
           garbage))
    (unwind-protect
        (let ((before (pi-coding-agent-test--file-contents path)))
          (with-current-buffer chat-buf
            (setq pi-coding-agent--state
                  (list :session-file "/tmp/somewhere-else.jsonl")))
          (pi-coding-agent-test--rename-at-point
           (list :path path :name "Torn tail session" :messageCount 2
                 :modified "2026-03-02T10:00:00Z")
           chat-buf "Fixed name")
          (let* ((after (pi-coding-agent-test--file-contents path))
                 (appended (car (split-string (substring after (length before))
                                              "\n" t)))
                 (entry (json-parse-string appended :object-type 'plist))
                 (state (pi-coding-agent--browse-session-file-state path)))
            (should (string-prefix-p before after))
            ;; The collision set sees every id in the file, torn line or not.
            (dolist (id '("sid-g" "g1" "g2" "torn"))
              (should (gethash id (plist-get state :ids))))
            (should-not (gethash "no-such-id" (plist-get state :ids)))
            ;; Parent is the last parseable line, not the torn one.
            (should (equal (plist-get entry :parentId) "g2"))
            ;; Fresh id: collides with nothing already in the file.
            (should (not (member (plist-get entry :id)
                                 '("sid-g" "g1" "g2" "torn"))))
            (should (equal (plist-get entry :name) "Fixed name"))))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-rename-append-unreadable-file ()
  "Renaming a session whose file vanished cancels with a message.
No line is appended, no file is created, and the browser is not
refreshed (the fetch-and-render seam stays silent)."
  (let* ((dir (pi-coding-agent-test--make-temp-directory "pi-rename-missing"))
         (path (expand-file-name "ghost.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-missing-chat*"))
         (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pi-coding-agent--state
                  (list :session-file "/tmp/somewhere-else.jsonl")))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (pi-coding-agent-test--rename-at-point
             (list :path path :name "Ghost session" :messageCount 0
                   :modified "2026-03-02T10:00:00Z")
             chat-buf "Ghost name"))
          (should-not (file-exists-p path))
          (should (cl-some (lambda (m) (string-match-p "unreadable" m))
                           messages)))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-rename-dispatch ()
  "Rename routes by current-vs-other session and cancels on empty input.
Current: `set-session-name' RPC only, no file append.  Other: file
append only, no RPC.  Empty (whitespace) input cancels with a message:
no RPC, no append (no clearing in Phase 2)."
  (let* ((dir (pi-coding-agent-test--make-temp-directory "pi-rename-dispatch"))
         (path (expand-file-name "target.jsonl" dir))
         (current-path (expand-file-name "current.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-dispatch-chat*"))
         (set-name-calls nil)
         (messages nil))
    (pi-coding-agent-test--write-session-lines
     path
     (list (pi-coding-agent-test--make-session-header "sid-target")
           (pi-coding-agent-test--user-line "m1" nil "other session")
           (pi-coding-agent-test--jsonl-line
            "session_info" "s1" "m1" :name "Target name")))
    (pi-coding-agent-test--write-session-lines
     current-path (list (pi-coding-agent-test--make-session-header "sid-current")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq pi-coding-agent--state (list :session-file current-path)))
          ;; Current session: RPC rename, no append to any file.
          (let ((target-before (pi-coding-agent-test--file-contents path))
                (current-before (pi-coding-agent-test--file-contents current-path)))
            (cl-letf (((symbol-function 'pi-coding-agent-set-session-name)
                       (lambda (&rest args)
                         (interactive)
                         (push args set-name-calls))))
              (pi-coding-agent-test--rename-at-point
               (list :path current-path :name "Current session" :messageCount 0
                     :modified "2026-03-02T10:00:00Z")
               chat-buf "  New\nName  "))
            (should (equal set-name-calls '(("New Name"))))
            (should (equal (pi-coding-agent-test--file-contents path)
                           target-before))
            (should (equal (pi-coding-agent-test--file-contents current-path)
                           current-before)))
          ;; Other session: append, no RPC.
          (setq set-name-calls nil)
          (let ((before (pi-coding-agent-test--file-contents path)))
            (cl-letf (((symbol-function 'pi-coding-agent-set-session-name)
                       (lambda (&rest args)
                         (interactive)
                         (push args set-name-calls))))
              (pi-coding-agent-test--rename-at-point
               (list :path path :name "Target name" :messageCount 1
                     :modified "2026-03-02T10:00:00Z")
               chat-buf "Fresh name"))
            (should-not set-name-calls)
            (should-not (equal (pi-coding-agent-test--file-contents path) before))
            (should (string-match-p "Fresh name"
                                    (pi-coding-agent-test--file-contents path))))
          ;; Empty input: cancelled for both paths; message only.
          (setq set-name-calls nil)
          (let ((target-before (pi-coding-agent-test--file-contents path))
                (current-before (pi-coding-agent-test--file-contents current-path)))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages)))
                      ((symbol-function 'pi-coding-agent-set-session-name)
                       (lambda (&rest args)
                         (interactive)
                         (push args set-name-calls))))
              (pi-coding-agent-test--rename-at-point
               (list :path path :name "Target name" :messageCount 1
                     :modified "2026-03-02T10:00:00Z")
               chat-buf "  \n "))
            (should-not set-name-calls)
            (should (equal (pi-coding-agent-test--file-contents path)
                           target-before))
            (should (equal (pi-coding-agent-test--file-contents current-path)
                           current-before))
            (should (member "Pi: Rename cancelled" messages))))
      (kill-buffer chat-buf))))

(provide 'pi-coding-agent-browse-test)
;;; pi-coding-agent-browse-test.el ends here
