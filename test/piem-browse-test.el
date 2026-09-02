;;; piem-browse-test.el --- Tests for browsing module -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for piem-browse.el — session and tree browser
;; helper functions and response parsing.

;;; Code:

(require 'ert)
(require 'json)
(require 'transient)
(require 'piem-browse)
(require 'piem-jsonl)
(require 'piem-test-common)

;;;; Test Fixtures

(defun piem-test--fixture-sessions ()
  "Session items in the browse dialect, from browse-sessions.json.
Stands in for the dropped `piem--parse-session-list'."
  (append (plist-get (plist-get (piem-test--read-json-fixture
                                 "browse-sessions.json")
                                :data)
                     :sessions)
          nil))

;;;; Session Display

(ert-deftest piem-test-session-display-name ()
  "Session display name prefers name over firstMessage."
  ;; Named session
  (should (equal (piem--session-display-name
                  '(:name "My Session" :firstMessage "some prompt"))
                 "My Session"))
  ;; Unnamed session
  (should (equal (piem--session-display-name
                  '(:firstMessage "Fix the bug in login.py"))
                 "Fix the bug in login.py"))
  ;; No name, no firstMessage
  (should (equal (piem--session-display-name
                  '(:id "abc-123"))
                 "[empty session]"))
  ;; Newlines in firstMessage collapsed to spaces
  (should (equal (piem--session-display-name
                  '(:firstMessage "Fix the bug\nin login.py"))
                 "Fix the bug in login.py"))
  ;; Multiple newlines and surrounding whitespace collapsed
  (should (equal (piem--session-display-name
                  '(:firstMessage "First line\n\nSecond line\n  Third"))
                 "First line Second line Third"))
  ;; Newlines in name also collapsed
  (should (equal (piem--session-display-name
                  '(:name "My\nSession" :firstMessage "prompt"))
                 "My Session")))

(ert-deftest piem-test-first-nonempty-line ()
  "Extract first non-empty line from a string."
  ;; Single line
  (should (equal (piem--first-nonempty-line "hello") "hello"))
  ;; Multi-line returns first
  (should (equal (piem--first-nonempty-line "first\nsecond") "first"))
  ;; Skips leading blank lines
  (should (equal (piem--first-nonempty-line "\n\nactual") "actual"))
  ;; Nil returns empty string
  (should (equal (piem--first-nonempty-line nil) ""))
  ;; Empty string returns empty string
  (should (equal (piem--first-nonempty-line "") ""))
  ;; Only whitespace returns empty string
  (should (equal (piem--first-nonempty-line "\n  \n") "")))

;;;; Tree Parsing

(ert-deftest piem-test-parse-tree ()
  "Parse get_tree response into tree data."
  (let* ((response (piem-test--read-json-fixture "browse-tree.json"))
         (tree-data (piem--parse-tree response)))
    (should tree-data)
    (should (equal (plist-get tree-data :leafId) "node-8"))
    ;; Tree has two roots
    (let ((roots (plist-get tree-data :tree)))
      (should (= (length roots) 2))
      ;; First root is a user message
      (let ((first (aref roots 0)))
        (should (equal (plist-get first :type) "message"))
        (should (equal (plist-get first :role) "user"))))))

(ert-deftest piem-test-parse-tree-error ()
  "Return nil for failed get_tree response."
  (let ((response '(:type "response" :command "get_tree"
                    :success :false :error "no session")))
    (should (null (piem--parse-tree response)))))

;;;; Margin Age Formatting

(ert-deftest piem-test-margin-age-seconds ()
  "Margin age format for seconds."
  (should (equal (piem--margin-age 1) '(1 . "second")))
  (should (equal (piem--margin-age 30) '(30 . "second")))
  (should (equal (piem--margin-age 59) '(59 . "second"))))

(ert-deftest piem-test-margin-age-minutes ()
  "Margin age format for minutes."
  (should (equal (piem--margin-age 60) '(1 . "minute")))
  (should (equal (piem--margin-age 120) '(2 . "minute")))
  (should (equal (piem--margin-age 3599) '(59 . "minute"))))

(ert-deftest piem-test-margin-age-hours ()
  "Margin age format for hours."
  (should (equal (piem--margin-age 3600) '(1 . "hour")))
  (should (equal (piem--margin-age 7200) '(2 . "hour")))
  (should (equal (piem--margin-age 86399) '(23 . "hour"))))

(ert-deftest piem-test-margin-age-days ()
  "Margin age format for days."
  (should (equal (piem--margin-age 86400) '(1 . "day")))
  (should (equal (piem--margin-age 604799) '(6 . "day"))))

(ert-deftest piem-test-margin-age-weeks ()
  "Margin age format for weeks."
  (should (equal (piem--margin-age 604800) '(1 . "week")))
  (should (equal (piem--margin-age 2629799) '(4 . "week"))))

(ert-deftest piem-test-margin-age-months ()
  "Margin age format for months."
  (should (equal (piem--margin-age 2629800) '(1 . "month")))
  (should (equal (piem--margin-age 31557599) '(11 . "month"))))

(ert-deftest piem-test-margin-age-years ()
  "Margin age format for years."
  (should (equal (piem--margin-age 31557600) '(1 . "year")))
  (should (equal (piem--margin-age 63115200) '(2 . "year"))))

(ert-deftest piem-test-margin-age-zero ()
  "Margin age of zero seconds."
  (should (equal (piem--margin-age 0) '(0 . "second"))))

(ert-deftest piem-test-format-margin-age ()
  "Format margin age as aligned string."
  ;; Singular: no trailing s
  (should (equal (piem--format-margin-age 1) " 1 second "))
  ;; Plural: trailing s
  (should (equal (piem--format-margin-age 120) " 2 minutes"))
  ;; Right-justified count
  (should (equal (piem--format-margin-age 3600) " 1 hour   "))
  ;; Large count
  (should (equal (piem--format-margin-age 86400) " 1 day    "))
  ;; Multi-digit count (10 minutes)
  (should (equal (piem--format-margin-age 600) "10 minutes"))
  ;; Week boundary
  (should (equal (piem--format-margin-age 604800) " 1 week   ")))

(ert-deftest piem-test-format-margin-age-from-iso ()
  "Format ISO timestamp as margin age string."
  (cl-letf (((symbol-function 'current-time)
             (lambda () (encode-time '(0 0 12 24 2 2026 nil nil 0)))))
    ;; 5 minutes ago
    (should (equal (piem--format-margin-age-from-iso
                    "2026-02-24T11:55:00.000Z")
                   " 5 minutes"))
    ;; 2 hours ago
    (should (equal (piem--format-margin-age-from-iso
                    "2026-02-24T10:00:00.000Z")
                   " 2 hours  "))))

;;;; Margin Infrastructure

(ert-deftest piem-test-propertize-face ()
  "Propertize-face sets both face and font-lock-face."
  (let ((s (piem--propertize-face "hello" 'bold)))
    (should (equal (get-text-property 0 'face s) 'bold))
    (should (equal (get-text-property 0 'font-lock-face s) 'bold))))

(ert-deftest piem-test-session-margin-width ()
  "Session margin width is computed from age spec."
  ;; Width = count(4) + " msgs "(5) + age(2+1+max-unit-len) = 19
  ;; With 1 char padding = 20
  (should (integerp piem--session-margin-width))
  (should (>= piem--session-margin-width 19)))

(ert-deftest piem-test-tree-margin-width ()
  "Tree margin width accommodates labels."
  (should (integerp piem--tree-margin-width))
  (should (>= piem--tree-margin-width 14)))

(ert-deftest piem-test-make-margin-overlay ()
  "Make-margin-overlay creates overlay with correct properties."
  (with-temp-buffer
    (insert "first line\n")
    (insert "second line\n")
    ;; Create overlay on the second line (point is after it)
    (piem--make-margin-overlay "test margin")
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

(ert-deftest piem-test-make-margin-overlay-nil-string ()
  "Make-margin-overlay with nil uses a space."
  (with-temp-buffer
    (insert "a line\n")
    (piem--make-margin-overlay nil)
    (let* ((ovs (overlays-in (point-min) (point-max)))
           (o (car ovs))
           (bs (overlay-get o 'before-string))
           (display (get-text-property 0 'display bs)))
      (should (equal (cadr display) " ")))))

(ert-deftest piem-test-browse-apply-margins ()
  "Apply-margins sets the right margin on the window showing the buffer."
  (let ((buf (generate-new-buffer " *test-margins*"))
        (prev-buf (window-buffer (selected-window)))
        (prev-margins (window-margins (selected-window))))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buf)
          (with-current-buffer buf
            (setq piem--browse-margin-width 20)
            (piem--browse-apply-margins))
          (should (equal (cdr (window-margins (selected-window))) 20)))
      (set-window-margins (selected-window)
                          (car prev-margins) (cdr prev-margins))
      (set-window-buffer (selected-window) prev-buf)
      (kill-buffer buf))))

(ert-deftest piem-test-browse-mode-sets-right-margin-width ()
  "Browse mode sets buffer-local `right-margin-width'.
This ensures margins are cleaned up when `quit-window' switches to
another buffer — Emacs resets window margins from the new buffer's
`right-margin-width' during `set-window-buffer'."
  (let ((tree-buf (generate-new-buffer " *test-tree*"))
        (session-buf (generate-new-buffer " *test-sessions*")))
    (unwind-protect
        (progn
          (with-current-buffer tree-buf
            (piem-tree-browser-mode)
            (should (= right-margin-width
                       piem--tree-margin-width)))
          (with-current-buffer session-buf
            (piem-session-browser-mode)
            (should (= right-margin-width
                       piem--session-margin-width))))
      (kill-buffer tree-buf)
      (kill-buffer session-buf))))

(ert-deftest piem-test-browse-mode-no-margin-leak ()
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
            (piem-tree-browser-mode))
          ;; The selected window (showing other-buf) must NOT have margins
          (should-not (cdr (window-margins (selected-window)))))
      (kill-buffer browse-buf))))

;;;; Active Path Detection

(ert-deftest piem-test-active-path-ids ()
  "Compute set of node IDs on the active path from root to leaf."
  (let* ((response (piem-test--read-json-fixture "browse-tree.json"))
         (tree-data (piem--parse-tree response))
         (active (piem--active-path-ids
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

(defun piem-test--make-deep-tree (n)
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

(ert-deftest piem-test-flatten-tree-deep-chain ()
  "Flatten a linear chain deeper than max-lisp-eval-depth."
  (let* ((n 2000)
         (tree (piem-test--make-deep-tree n))
         (leaf-id (format "node-%d" n))
         (flat (piem--flatten-tree-for-display
                tree leaf-id "default")))
    (should (= (length flat) n))))

(ert-deftest piem-test-subtree-contains-active-deep ()
  "Subtree-contains-active-p works on chains deeper than max-lisp-eval-depth."
  (let* ((n 2000)
         (tree (piem-test--make-deep-tree n))
         (active-ids (make-hash-table :test 'equal)))
    (puthash (format "node-%d" n) t active-ids)
    (should (piem--subtree-contains-active-p
             (aref tree 0) active-ids))))

;;;; Tree Flattening

(ert-deftest piem-test-flatten-tree-for-display ()
  "Flatten tree into display-ordered list with indent levels and prefixes."
  (let* ((response (piem-test--read-json-fixture "browse-tree.json"))
         (tree-data (piem--parse-tree response))
         (flat (piem--flatten-tree-for-display
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

(ert-deftest piem-test-flatten-tree-connector-prefixes ()
  "Branch children get ├─/└─ connectors; chain nodes get gutter continuation."
  (let* ((response (piem-test--read-json-fixture "browse-tree.json"))
         (tree-data (piem--parse-tree response))
         (flat (piem--flatten-tree-for-display
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

(ert-deftest piem-test-flatten-tree-connectors-no-tools-filter ()
  "Connectors work when tool nodes are filtered out."
  (let* ((response (piem-test--read-json-fixture "browse-tree.json"))
         (tree-data (piem--parse-tree response))
         (flat (piem--flatten-tree-for-display
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

(ert-deftest piem-test-flatten-tree-connectors-single-root ()
  "Single-root tree has no top-level connectors."
  (let* ((tree (list '(:id "r1" :type "message" :role "user"
                       :children [(:id "c1" :type "message" :role "assistant"
                                  :preview "hi" :children [])])))
         (flat (piem--flatten-tree-for-display tree "c1" "default"))
         (prefixes (mapcar (lambda (e) (nth 2 e)) flat)))
    ;; Both nodes at root level, single-child chain — no connectors
    (should (equal prefixes '("" "")))))

(ert-deftest piem-test-flatten-tree-connectors-nested-branches ()
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
         (flat (piem--flatten-tree-for-display tree "u2" "default"))
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

(ert-deftest piem-test-flatten-tree-connectors-three-siblings ()
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
         (flat (piem--flatten-tree-for-display tree "c1" "default"))
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

(ert-deftest piem-test-filter-default ()
  "Default filter shows messages, tool results, compaction, branch summary."
  (should (piem--browse-node-visible-p
           '(:type "message" :role "user") "default"))
  (should (piem--browse-node-visible-p
           '(:type "message" :role "assistant" :preview "hello") "default"))
  (should (piem--browse-node-visible-p
           '(:type "tool_result") "default"))
  (should (piem--browse-node-visible-p
           '(:type "compaction") "default"))
  (should (piem--browse-node-visible-p
           '(:type "branch_summary") "default"))
  ;; Model change hidden in default
  (should-not (piem--browse-node-visible-p
               '(:type "model_change") "default"))
  ;; Thinking level change hidden in default
  (should-not (piem--browse-node-visible-p
               '(:type "thinking_level_change") "default")))

(ert-deftest piem-test-filter-no-tools ()
  "No-tools filter hides tool_result entries."
  (should (piem--browse-node-visible-p
           '(:type "message" :role "user") "no-tools"))
  (should-not (piem--browse-node-visible-p
               '(:type "tool_result") "no-tools")))

(ert-deftest piem-test-filter-user-only ()
  "User-only filter shows only user messages."
  (should (piem--browse-node-visible-p
           '(:type "message" :role "user") "user-only"))
  (should-not (piem--browse-node-visible-p
               '(:type "message" :role "assistant" :preview "hello") "user-only"))
  (should-not (piem--browse-node-visible-p
               '(:type "tool_result") "user-only")))

(ert-deftest piem-test-filter-labeled-only ()
  "Labeled-only filter shows only entries with labels."
  (should (piem--browse-node-visible-p
           '(:type "message" :role "user" :label "checkpoint") "labeled-only"))
  (should-not (piem--browse-node-visible-p
               '(:type "message" :role "user") "labeled-only")))

(ert-deftest piem-test-filter-all ()
  "All filter shows settings entries that other modes hide."
  (should (piem--browse-node-visible-p
           '(:type "model_change") "all"))
  (should (piem--browse-node-visible-p
           '(:type "thinking_level_change") "all")))

(ert-deftest piem-test-filter-empty-assistant ()
  "Empty assistant messages are hidden (unless they are the leaf)."
  ;; Empty assistant with no useful content
  (should-not (piem--browse-node-visible-p
               '(:type "message" :role "assistant" :preview "") "default"))
  ;; Aborted assistant is shown
  (should (piem--browse-node-visible-p
           '(:type "message" :role "assistant" :preview "" :stopReason "aborted") "default"))
  ;; Assistant with error is shown
  (should (piem--browse-node-visible-p
           '(:type "message" :role "assistant" :preview "" :errorMessage "rate limit") "default")))

(ert-deftest piem-test-empty-assistant-hidden-in-all-modes ()
  "Empty assistant messages are hidden in ALL filter modes.
Per TUI tree-selector.ts:282-293 and PLAN-BROWSING.md line 560:
empty assistants are a universal pre-filter, not mode-specific."
  (let ((empty-ast '(:type "message" :role "assistant" :preview "(no content)"))
        (empty-ast-blank '(:type "message" :role "assistant" :preview "")))
    (dolist (mode '("default" "no-tools" "all"))
      (should-not (piem--browse-node-visible-p empty-ast mode))
      (should-not (piem--browse-node-visible-p empty-ast-blank mode)))))

(ert-deftest piem-test-empty-assistant-shown-when-aborted-all-modes ()
  "Aborted/error assistant messages are shown even if empty, in all modes."
  (let ((aborted '(:type "message" :role "assistant" :preview ""
                          :stopReason "aborted"))
        (errored '(:type "message" :role "assistant" :preview ""
                          :errorMessage "rate limit")))
    (dolist (mode '("default" "no-tools" "all"))
      (should (piem--browse-node-visible-p aborted mode))
      (should (piem--browse-node-visible-p errored mode)))))

;;;; Search/Filter

(ert-deftest piem-test-matches-filter-p ()
  "Space-separated regexp token matching."
  ;; Single token
  (should (piem--matches-filter-p "Fix the login bug" '("login")))
  ;; Multiple tokens (AND)
  (should (piem--matches-filter-p "Fix the login bug" '("login" "bug")))
  ;; Non-match
  (should-not (piem--matches-filter-p "Fix the login bug" '("database")))
  ;; Regexp token
  (should (piem--matches-filter-p "Fix the login bug" '("log.*bug")))
  ;; Empty tokens list matches everything
  (should (piem--matches-filter-p "anything" nil)))

;;;; Session Sorting

(ert-deftest piem-test-session-sort-cycle ()
  "Sort mode cycles through threaded → recent → relevance."
  (should (equal (piem--session-sort-next "threaded") "recent"))
  (should (equal (piem--session-sort-next "recent") "relevance"))
  (should (equal (piem--session-sort-next "relevance") "threaded")))

(ert-deftest piem-test-session-sort-recent ()
  "Sort by recent puts newest modified first."
  (let ((items (list '(:modified "2026-02-20T10:00:00Z" :id "old")
                     '(:modified "2026-02-24T10:00:00Z" :id "new")
                     '(:modified "2026-02-22T10:00:00Z" :id "mid"))))
    (let ((sorted (piem--session-sort-items items "recent")))
      (should (equal (plist-get (nth 0 sorted) :id) "new"))
      (should (equal (plist-get (nth 1 sorted) :id) "mid"))
      (should (equal (plist-get (nth 2 sorted) :id) "old")))))

(ert-deftest piem-test-session-sort-relevance ()
  "Sort by relevance puts highest message count first."
  (let ((items (list '(:messageCount 10 :id "small")
                     '(:messageCount 500 :id "big")
                     '(:messageCount 100 :id "med"))))
    (let ((sorted (piem--session-sort-items items "relevance")))
      (should (equal (plist-get (nth 0 sorted) :id) "big"))
      (should (equal (plist-get (nth 1 sorted) :id) "med"))
      (should (equal (plist-get (nth 2 sorted) :id) "small")))))

;;;; Session Threading

(ert-deftest piem-test-session-threading ()
  "Thread items into parent-child structure."
  (let* ((items (piem-test--fixture-sessions))
         (threaded (piem--session-thread-items items)))
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

(ert-deftest piem-test-session-filter-named ()
  "Named filter keeps only sessions with a name."
  (let* ((items (piem-test--fixture-sessions))
         (named (piem--session-filter-named items)))
    ;; Only bbb-222 and ddd-444 have names
    (should (= (length named) 2))
    (should (cl-every (lambda (item)
                        (plist-get item :name))
                      named))))

(ert-deftest piem-test-session-filter-search ()
  "Search filter matches against name and first message."
  (let ((items (piem-test--fixture-sessions)))
    ;; Search for "database"
    (let ((found (piem--session-filter-search items '("database"))))
      (should (= (length found) 2))  ; bbb-222 and ccc-333 mention database
      )
    ;; Search for "CI" matches Setup CI/CD
    (let ((found (piem--session-filter-search items '("CI"))))
      (should (>= (length found) 1)))))

;;;; Time Groups

(ert-deftest piem-test-session-time-group ()
  "Time group labels for ISO timestamps."
  ;; Now → Today
  (let ((now (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" (current-time) t)))
    (should (equal (piem--session-time-group now) "Today")))
  ;; 2 days ago → Yesterday or This Week depending on time of day
  ;; 30 days ago → Older
  (let ((old (format-time-string "%Y-%m-%dT%H:%M:%S.000Z"
                                 (time-subtract (current-time) (days-to-time 30))
                                 t)))
    (should (equal (piem--session-time-group old) "Older"))))

;;;; Session Browser Rendering

(ert-deftest piem-test-session-browser-render-flat ()
  "Render sessions as flat list in a buffer."
  (with-temp-buffer
    (piem-session-browser-mode)
    (setq piem--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 42 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/b.jsonl" :firstMessage "Fix the bug"
                  :messageCount 10 :modified "2026-02-23T10:00:00Z")))
    (setq piem--session-browser-sort "relevance")
    (piem--session-browser-rerender)
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

(ert-deftest piem-test-session-browser-render-threaded ()
  "Render sessions with threading connectors."
  (with-temp-buffer
    (piem-session-browser-mode)
    (setq piem--session-browser-items
          (list '(:path "/test/parent.jsonl" :name "Parent Session"
                  :messageCount 100 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/child.jsonl" :firstMessage "Child branch"
                  :parentSessionPath "/test/parent.jsonl"
                  :messageCount 20 :modified "2026-02-24T11:00:00Z")))
    (setq piem--session-browser-sort "threaded")
    (piem--session-browser-rerender)
    ;; Should contain threading connector
    (should (string-match-p "└─" (buffer-string)))
    ;; Parent before child
    (let ((pos-p (string-match "Parent Session" (buffer-string)))
          (pos-c (string-match "Child branch" (buffer-string))))
      (should (< pos-p pos-c)))))

(ert-deftest piem-test-session-browser-fork-prefix-flat ()
  "Forked sessions show `fork:' prefix in non-threaded modes."
  (with-temp-buffer
    (piem-session-browser-mode)
    (setq piem--session-browser-items
          (list '(:path "/test/parent.jsonl" :name "Parent Session"
                  :messageCount 100 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/child.jsonl" :firstMessage "Child branch"
                  :parentSessionPath "/test/parent.jsonl"
                  :messageCount 20 :modified "2026-02-24T11:00:00Z")))
    (setq piem--session-browser-sort "relevance")
    (piem--session-browser-rerender)
    ;; Fork prefix should appear before child session
    (should (string-match-p "fork:" (buffer-string)))
    ;; But NOT before parent
    (let ((text (buffer-string)))
      (should-not (string-match-p "fork:.*Parent Session" text)))))

(ert-deftest piem-test-session-browser-fork-prefix-threaded ()
  "Forked sessions do NOT show `fork:' prefix in threaded mode."
  (with-temp-buffer
    (piem-session-browser-mode)
    (setq piem--session-browser-items
          (list '(:path "/test/parent.jsonl" :name "Parent Session"
                  :messageCount 100 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/child.jsonl" :firstMessage "Child branch"
                  :parentSessionPath "/test/parent.jsonl"
                  :messageCount 20 :modified "2026-02-24T11:00:00Z")))
    (setq piem--session-browser-sort "threaded")
    (piem--session-browser-rerender)
    ;; Threading connector should appear, but NOT fork: prefix
    (should (string-match-p "└─" (buffer-string)))
    (should-not (string-match-p "fork:" (buffer-string)))))

(ert-deftest piem-test-session-browser-margin-overlays ()
  "Session entries have right-margin overlays with count and age."
  (with-temp-buffer
    (piem-session-browser-mode)
    (setq piem--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 42 :modified "2026-02-24T10:00:00Z")))
    (setq piem--session-browser-sort "relevance")
    (piem--session-browser-rerender)
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

(ert-deftest piem-test-session-browser-no-name-truncation ()
  "Session names are not truncated."
  (with-temp-buffer
    (piem-session-browser-mode)
    (let ((long-name (make-string 80 ?x)))
      (setq piem--session-browser-items
            (list (list :path "/test/a.jsonl" :name long-name
                        :messageCount 1 :modified "2026-02-24T10:00:00Z")))
      (setq piem--session-browser-sort "relevance")
      (piem--session-browser-rerender)
      ;; Full name should appear, not truncated
      (should (string-match-p long-name (buffer-string))))))

(ert-deftest piem-test-session-browser-render-loading ()
  "Render loading indicator."
  (with-temp-buffer
    (piem-session-browser-mode)
    (setq piem--session-browser-loading t)
    (piem--session-browser-rerender)
    (should (string-match-p "Loading" (buffer-string)))))

(ert-deftest piem-test-session-browser-render-empty ()
  "Render empty state when no sessions."
  (with-temp-buffer
    (piem-session-browser-mode)
    (setq piem--session-browser-items nil)
    (piem--session-browser-rerender)
    (should (string-match-p "No sessions found" (buffer-string)))))

(ert-deftest piem-test-session-browser-header-line ()
  "Header-line shows scope, sort, and filter state."
  (with-temp-buffer
    (piem-session-browser-mode)
    (setq piem--session-browser-scope "current"
          piem--session-browser-sort "threaded"
          piem--session-browser-items '((:id "a") (:id "b")))
    (let ((header (piem--session-browser-header-line)))
      (should (string-match-p "current" header))
      (should (string-match-p "threaded" header))
      (should (string-match-p "(2)" header)))))

;;;; Tree Node Formatting

(ert-deftest piem-test-tree-node-face ()
  "Correct face for each node type."
  (should (eq (piem--tree-node-face
               '(:type "message" :role "user"))
              'piem-tree-user))
  (should (eq (piem--tree-node-face
               '(:type "message" :role "assistant"))
              'piem-tree-assistant))
  (should (eq (piem--tree-node-face
               '(:type "tool_result"))
              'piem-tree-tool))
  (should (eq (piem--tree-node-face
               '(:type "compaction"))
              'piem-tree-compaction))
  (should (eq (piem--tree-node-face
               '(:type "branch_summary"))
              'piem-tree-summary)))

(ert-deftest piem-test-tree-node-type-label ()
  "Short type labels for tree nodes."
  (should (equal (piem--tree-node-type-label
                  '(:type "message" :role "user"))
                 "you"))
  (should (equal (piem--tree-node-type-label
                  '(:type "message" :role "assistant"))
                 "ast"))
  (should (equal (piem--tree-node-type-label
                  '(:type "tool_result" :toolName "Read"))
                 "Read"))
  (should (equal (piem--tree-node-type-label
                  '(:type "compaction"))
                 "compact")))

;;;; Tool Preview Unpacking

(ert-deftest piem-test-tree-strip-bracket-preview-formatted ()
  "Strip bracket wrapper from formattedToolCall."
  (should (equal (piem--tree-strip-bracket-preview
                  '(:type "tool_result" :toolName "read"
                    :formattedToolCall "[read: ~/file.py:10-29]"
                    :preview "[read: ~/file.py:10-29]"))
                 "~/file.py:10-29")))

(ert-deftest piem-test-tree-strip-bracket-preview-read ()
  "Read tool strips wrapper, shows path."
  (should (equal (piem--tree-strip-bracket-preview
                  '(:type "tool_result" :toolName "Read"
                    :preview "[Read: db/connection.py]"))
                 "db/connection.py")))

(ert-deftest piem-test-tree-strip-bracket-preview-bash ()
  "Bash tool strips wrapper, shows command."
  (should (equal (piem--tree-strip-bracket-preview
                  '(:type "tool_result" :toolName "bash"
                    :formattedToolCall "[bash: git status]"
                    :preview "[bash: git status]"))
                 "git status")))

(ert-deftest piem-test-tree-strip-bracket-preview-no-args ()
  "Tool with no args returns empty string."
  (should (equal (piem--tree-strip-bracket-preview
                  '(:type "tool_result" :toolName "unknown"
                    :preview "[unknown]"))
                 "")))

(ert-deftest piem-test-tree-strip-bracket-preview-plain-text ()
  "Preview without brackets returned as-is."
  (should (equal (piem--tree-strip-bracket-preview
                  '(:type "tool_result" :toolName "custom"
                    :preview "some plain output"))
                 "some plain output")))

(ert-deftest piem-test-tree-strip-bracket-preview-in-node-line ()
  "Tool result in formatted node line shows unwrapped preview."
  (let ((line (piem--tree-format-node-line
               '(:type "tool_result" :toolName "Read"
                 :preview "[Read: db/connection.py]")
               nil)))
    ;; Should NOT have the bracketed format
    (should-not (string-match-p "\\[Read:" line))
    ;; Should have the unwrapped path
    (should (string-match-p "db/connection.py" line))))

(ert-deftest piem-test-tree-node-preview-message ()
  "Regular message nodes return preview as-is."
  (should (equal (piem--tree-node-preview
                  '(:type "message" :role "user" :preview "hello world"))
                 "hello world"))
  (should (equal (piem--tree-node-preview
                  '(:type "message" :role "assistant" :preview "sure thing"))
                 "sure thing"))
  ;; Missing preview returns empty string
  (should (equal (piem--tree-node-preview
                  '(:type "message" :role "user"))
                 "")))

(ert-deftest piem-test-tree-node-preview-branch-summary ()
  "Branch summary nodes return first line of summary, not full text."
  ;; Multi-line summary returns only first line
  (should (equal (piem--tree-node-preview
                  '(:type "branch_summary"
                    :summary "The user explored TDD.\n\n## Goal\nLearn testing."))
                 "The user explored TDD."))
  ;; Single-line summary returned as-is
  (should (equal (piem--tree-node-preview
                  '(:type "branch_summary"
                    :summary "Short summary"))
                 "Short summary"))
  ;; Missing summary returns empty string
  (should (equal (piem--tree-node-preview
                  '(:type "branch_summary"))
                 ""))
  ;; Summary starting with blank lines skips to first non-empty line
  (should (equal (piem--tree-node-preview
                  '(:type "branch_summary"
                    :summary "\n\nActual summary here\nMore text"))
                 "Actual summary here")))

(ert-deftest piem-test-tree-node-preview-bash-execution ()
  "Bash execution message strips bracket wrapper from preview.
Upstream changed format from `[bash]: cmd' to `[bash: cmd]'.
The type label already shows `sh', so brackets are redundant."
  ;; tree-node-preview strips the wrapper
  (should (equal (piem--tree-node-preview
                  '(:type "message" :role "bashExecution"
                    :preview "[bash: git status]"))
                 "git status"))
  ;; Formatted node line shows stripped preview
  (let ((line (piem--tree-format-node-line
               '(:type "message" :role "bashExecution"
                 :preview "[bash: git log --oneline]")
               nil)))
    (should-not (string-match-p "\\[bash:" line))
    (should (string-match-p "git log --oneline" line))))

(ert-deftest piem-test-tree-format-node-active ()
  "Active path nodes get bullet marker."
  (let ((line (piem--tree-format-node-line
               '(:type "message" :role "user" :preview "hello") t)))
    (should (string-match-p "•" line))
    (should (string-match-p "hello" line))))

(ert-deftest piem-test-tree-format-node-inactive ()
  "Inactive nodes get space instead of bullet."
  (let ((line (piem--tree-format-node-line
               '(:type "message" :role "user" :preview "hello") nil)))
    (should-not (string-match-p "•" line))
    (should (string-match-p "hello" line))))

(ert-deftest piem-test-tree-format-node-with-label ()
  "Labeled nodes do NOT include label in the line text (labels go in margin)."
  (let ((line (piem--tree-format-node-line
               '(:type "message" :role "user" :preview "hello"
                 :label "checkpoint")
               nil)))
    ;; Label should not be in the main text
    (should-not (string-match-p "\\[checkpoint\\]" line))
    ;; But preview should still appear
    (should (string-match-p "hello" line))))

;;;; Tree Browser Rendering

(ert-deftest piem-test-tree-browser-render ()
  "Render tree from fixture data."
  (with-temp-buffer
    (piem-tree-browser-mode)
    (let* ((response (piem-test--read-json-fixture "browse-tree.json"))
           (tree-data (piem--parse-tree response)))
      (setq piem--tree-browser-tree (plist-get tree-data :tree)
            piem--tree-browser-leaf-id (plist-get tree-data :leafId)
            piem--tree-browser-filter "default")
      (piem--tree-browser-rerender)
      ;; Buffer should contain node content
      (should (string-match-p "refactor" (buffer-string)))
      ;; Active path nodes should have bullet marker
      (should (string-match-p "•" (buffer-string)))
      ;; Label should NOT be in buffer text (it's in margin overlay)
      (should-not (string-match-p "\\[checkpoint\\]" (buffer-string))))))

(ert-deftest piem-test-tree-browser-render-connectors ()
  "Tree connectors appear in rendered buffer at branch points."
  (with-temp-buffer
    (piem-tree-browser-mode)
    (let* ((response (piem-test--read-json-fixture "browse-tree.json"))
           (tree-data (piem--parse-tree response)))
      (setq piem--tree-browser-tree (plist-get tree-data :tree)
            piem--tree-browser-leaf-id (plist-get tree-data :leafId)
            piem--tree-browser-filter "default")
      (piem--tree-browser-rerender)
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

(ert-deftest piem-test-tree-browser-label-in-margin ()
  "Labels appear as right-margin overlays, not inline text."
  (with-temp-buffer
    (piem-tree-browser-mode)
    (let* ((response (piem-test--read-json-fixture "browse-tree.json"))
           (tree-data (piem--parse-tree response)))
      (setq piem--tree-browser-tree (plist-get tree-data :tree)
            piem--tree-browser-leaf-id (plist-get tree-data :leafId)
            piem--tree-browser-filter "default")
      (piem--tree-browser-rerender)
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

(ert-deftest piem-test-tree-browser-label-truncation ()
  "Long labels are truncated with ellipsis to fit the right margin."
  (with-temp-buffer
    (piem-tree-browser-mode)
    (let ((tree (vector (list :id "n1" :type "message" :role "user"
                              :preview "hello" :timestamp "2026-01-01T00:00:00Z"
                              :label "this-is-a-very-long-label-name"
                              :children (vector)))))
      (setq piem--tree-browser-tree tree
            piem--tree-browser-leaf-id "n1"
            piem--tree-browser-filter "default")
      (piem--tree-browser-rerender)
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
        (should (<= (length content) piem--tree-margin-width))
        ;; Should NOT contain the full label
        (should-not (string-match-p "this-is-a-very-long-label-name" content))))))

(ert-deftest piem-test-tree-browser-short-label-not-truncated ()
  "Short labels are not truncated."
  (with-temp-buffer
    (piem-tree-browser-mode)
    (let ((tree (vector (list :id "n1" :type "message" :role "user"
                              :preview "hello" :timestamp "2026-01-01T00:00:00Z"
                              :label "ok"
                              :children (vector)))))
      (setq piem--tree-browser-tree tree
            piem--tree-browser-leaf-id "n1"
            piem--tree-browser-filter "default")
      (piem--tree-browser-rerender)
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

(ert-deftest piem-test-tree-browser-render-empty ()
  "Render empty tree."
  (with-temp-buffer
    (piem-tree-browser-mode)
    (setq piem--tree-browser-tree nil)
    (piem--tree-browser-rerender)
    (should (string-match-p "No conversation tree" (buffer-string)))))

(ert-deftest piem-test-tree-browser-render-user-filter ()
  "User-only filter shows only user messages."
  (with-temp-buffer
    (piem-tree-browser-mode)
    (let* ((response (piem-test--read-json-fixture "browse-tree.json"))
           (tree-data (piem--parse-tree response)))
      (setq piem--tree-browser-tree (plist-get tree-data :tree)
            piem--tree-browser-leaf-id (plist-get tree-data :leafId)
            piem--tree-browser-filter "user-only")
      (piem--tree-browser-rerender)
      ;; Should have user nodes
      (should (string-match-p "you" (buffer-string)))
      ;; Should NOT have assistant nodes
      (should-not (string-match-p "\\bast\\b" (buffer-string))))))

(ert-deftest piem-test-tree-browser-initial-filter ()
  "Tree browser opens with no-tools filter."
  (with-temp-buffer
    (piem-tree-browser-mode)
    (should (equal piem--tree-browser-filter "no-tools"))))

(ert-deftest piem-test-tree-browser-header-line ()
  "Header-line shows filter mode and count."
  (with-temp-buffer
    (piem-tree-browser-mode)
    (let* ((response (piem-test--read-json-fixture "browse-tree.json"))
           (tree-data (piem--parse-tree response)))
      (setq piem--tree-browser-tree (plist-get tree-data :tree)
            piem--tree-browser-leaf-id (plist-get tree-data :leafId)
            piem--tree-browser-filter "no-tools")
      (let ((header (piem--tree-browser-header-line)))
        (should (string-match-p "no-tools" header))
        (should (string-match-p "([0-9]+)" header))))))

;;;; Error States

(ert-deftest piem-test-session-browser-rpc-error ()
  "Session browser shows error when loading failed."
  (with-temp-buffer
    (piem-session-browser-mode)
    (setq piem--session-browser-error
          "session scan failed")
    (piem--session-browser-rerender)
    (should (string-match-p "Error:" (buffer-string)))
    (should (string-match-p "scan failed" (buffer-string)))))

(ert-deftest piem-test-session-browser-rpc-error-cleared-on-success ()
  "A successful fetch clears a stale error state.
Phase 2: the fetch reads sessions from disk, so the process mock is
vestigial and none is consulted.  The environment is isolated to an
empty sessions root and timers run synchronously so the chunked scan
completes in-call."
  (let ((root (piem-test--make-temp-directory "pi-err-clear")))
    (with-temp-buffer
      (piem-session-browser-mode)
      (setq piem--session-browser-error "some error")
      (cl-letf (((symbol-function 'piem--get-process)
                 (lambda () 'fake))
                ((symbol-function 'piem--session-list-directory)
                 (lambda (&optional _chat-buf) nil))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest args) (apply fn args))))
        (let ((process-environment
               (cons (format "PI_CODING_AGENT_DIR=%s" (directory-file-name root))
                     process-environment)))
          (piem--session-browser-fetch-and-render)))
      (should-not piem--session-browser-error)
      (should-not piem--session-browser-loading)
      (should (string-match-p "No sessions found" (buffer-string))))))

;;;; Tree Find Label

(ert-deftest piem-test-tree-find-label ()
  "Find label for a node ID in the tree."
  (let* ((response (piem-test--read-json-fixture "browse-tree.json"))
         (tree (plist-get (plist-get response :data) :tree)))
    ;; node-7 has label "checkpoint"
    (should (equal (piem--tree-find-label tree "node-7")
                   "checkpoint"))
    ;; node-1 has no label
    (should (null (piem--tree-find-label tree "node-1")))))

;;;; Session Browser Dispatch Transient

(ert-deftest piem-test-session-browser-dispatch-binding ()
  "Session browser binds `?' and `h' to the dispatch transient."
  (should (eq (lookup-key piem-session-browser-mode-map "?")
              'piem-session-browser-dispatch))
  (should (eq (lookup-key piem-session-browser-mode-map "h")
              'piem-session-browser-dispatch)))

(ert-deftest piem-test-session-browser-dispatch-is-transient ()
  "Session browser dispatch is a transient prefix command."
  (should (commandp 'piem-session-browser-dispatch))
  (should (get 'piem-session-browser-dispatch 'transient--prefix)))

(ert-deftest piem-test-session-browser-dispatch-suffixes ()
  "Session browser dispatch wires all keys to the correct commands."
  (let ((expected
         '(("RET" . piem-session-browser-switch)
           ("r"   . piem-session-browser-rename)
           ("s"   . piem-session-browser-cycle-sort)
           ("f"   . piem-session-browser-toggle-named)
           ("t"   . piem-session-browser-toggle-scope)
           ("/"   . piem-session-browser-search)
           ("g"   . piem-browse-refresh)
           ("q"   . quit-window))))
    (dolist (pair expected)
      (let* ((key (car pair))
             (cmd (cdr pair))
             (suffix (transient-get-suffix
                      'piem-session-browser-dispatch key))
             (actual (plist-get (cdr suffix) :command)))
        (should (eq actual cmd))))))

(ert-deftest piem-test-session-dispatch-heading ()
  "Session dispatch heading reflects buffer-local state."
  (with-temp-buffer
    (piem-session-browser-mode)
    ;; Default state: scope before sort, no named-only
    (should (equal (piem--session-dispatch-heading)
                   "scope:current │ sort:threaded"))
    ;; All state active
    (setq piem--session-browser-sort "recent"
          piem--session-browser-scope "all"
          piem--session-browser-named-only t)
    (should (equal (piem--session-dispatch-heading)
                   "scope:all │ sort:recent │ named-only"))))

;;;; Tree Browser Dispatch Transient

(ert-deftest piem-test-tree-browser-dispatch-binding ()
  "Tree browser binds `?' and `h' to the dispatch transient."
  (should (eq (lookup-key piem-tree-browser-mode-map "?")
              'piem-tree-browser-dispatch))
  (should (eq (lookup-key piem-tree-browser-mode-map "h")
              'piem-tree-browser-dispatch)))

(ert-deftest piem-test-tree-browser-dispatch-is-transient ()
  "Tree browser dispatch is a transient prefix command."
  (should (commandp 'piem-tree-browser-dispatch))
  (should (get 'piem-tree-browser-dispatch 'transient--prefix)))

(ert-deftest piem-test-tree-browser-dispatch-suffixes ()
  "Tree browser dispatch wires all keys to the correct commands.
The summarize (`S') and abort (`C-c C-k') suffixes were dropped with
the summarize feature (needs navigate_tree RPC)."
  (let ((expected
         '(("RET" . piem-tree-browser-navigate)
           ("l"   . piem-tree-browser-set-label)
           ("f"   . piem-tree-browser-cycle-filter)
           ("/"   . piem-tree-browser-search)
           ("g"   . piem-browse-refresh)
           ("q"   . quit-window))))
    (dolist (pair expected)
      (let* ((key (car pair))
             (cmd (cdr pair))
             (suffix (transient-get-suffix
                      'piem-tree-browser-dispatch key))
             (actual (plist-get (cdr suffix) :command)))
        (should (eq actual cmd))))))

(ert-deftest piem-test-tree-dispatch-heading ()
  "Tree dispatch heading reflects buffer-local filter state."
  (with-temp-buffer
    (piem-tree-browser-mode)
    ;; Default state (initial filter is no-tools)
    (let ((heading (piem--tree-dispatch-heading)))
      (should (string-match-p "filter:no-tools" heading)))
    ;; Change state
    (setq piem--tree-browser-filter "user-only")
    (let ((heading (piem--tree-dispatch-heading)))
      (should (string-match-p "filter:user-only" heading)))))

(ert-deftest piem-test-dispatch-headings-read-shadowed-buffer ()
  "Both dispatch headings read the invoking browser's buffer-locals on
transient's real rendering path.
`transient--insert-group' formats group descriptions inside
`transient-with-shadowed-buffer' — with the INVOKING buffer current,
not the transient's own temp buffer — so the headings' buffer-local
reads are correct there.  This pins that contract the way transient
exercises it: evaluated with an unrelated buffer current and only the
shadowed binding pointing at the browser.  A refactor that breaks the
dependency (e.g. resolving the state from the wrong buffer) fails
here."
  ;; Session heading: shadowed to a browser with every toggle set.
  (with-temp-buffer
    (piem-session-browser-mode)
    (setq piem--session-browser-scope "all"
          piem--session-browser-sort "recent"
          piem--session-browser-named-only t)
    (let ((browser-buf (current-buffer)))
      (with-temp-buffer
        ;; Stands in for transient's temp buffer: some unrelated
        ;; buffer is current; only the shadowed binding names the
        ;; invoking browser.
        (let ((transient--shadowed-buffer browser-buf))
          (should (equal (transient-with-shadowed-buffer
                           (piem--session-dispatch-heading))
                         "scope:all │ sort:recent │ named-only"))))))
  ;; Tree heading: same path, distinct filter state.
  (with-temp-buffer
    (piem-tree-browser-mode)
    (setq piem--tree-browser-filter "user-only")
    (let ((browser-buf (current-buffer)))
      (with-temp-buffer
        (let ((transient--shadowed-buffer browser-buf))
          (should (equal (transient-with-shadowed-buffer
                           (piem--tree-dispatch-heading))
                         "filter:user-only")))))))

;;;; Header-Line Help Hint

(ert-deftest piem-test-session-browser-header-line-help-hint ()
  "Session browser header-line includes `?:help' hint."
  (with-temp-buffer
    (piem-session-browser-mode)
    (setq piem--session-browser-items '((:id "a")))
    (let ((header (piem--session-browser-header-line)))
      (should (string-match-p "?:help" header)))))

(ert-deftest piem-test-tree-browser-header-line-help-hint ()
  "Tree browser header-line includes `?:help' hint."
  (with-temp-buffer
    (piem-tree-browser-mode)
    (let ((header (piem--tree-browser-header-line)))
      (should (string-match-p "?:help" header)))))

;;;; Startup Message

(ert-deftest piem-test-session-browser-startup-message ()
  "Session browser shows help hint message on first creation."
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages)))
              ((symbol-function 'piem--session-browser-fetch-and-render)
               #'ignore)
              ((symbol-function 'piem--get-chat-buffer)
               (lambda () nil))
              ((symbol-function 'piem--session-directory)
               (lambda () "/tmp/pi-test/")))
      (piem-session-browser)
      (unwind-protect
          (should (member "Pi: Press ? for available commands" messages))
        (when-let ((buf (get-buffer
                         (piem--session-browser-buffer-name
                          "/tmp/pi-test/"))))
          (kill-buffer buf))))))

(ert-deftest piem-test-tree-browser-startup-message ()
  "Tree browser shows help hint message on first creation.
Phase 3's entry-point guard requires a live chat link, so the test
provides one — a plain live buffer suffices; only liveness is
checked, and the fetch seam stays stubbed out."
  (let ((messages nil)
        (chat-buf (generate-new-buffer " *test-tree-startup-chat*")))
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages)))
                  ((symbol-function 'piem--tree-browser-fetch-and-render)
                   #'ignore)
                  ((symbol-function 'piem--get-chat-buffer)
                   (lambda () chat-buf))
                  ((symbol-function 'piem--session-directory)
                   (lambda () "/tmp/pi-test/")))
          (piem-tree-browser)
          (should (member "Pi: Press ? for available commands" messages)))
      (when-let ((buf (get-buffer
                       (piem--tree-browser-buffer-name
                        "/tmp/pi-test/"))))
        (kill-buffer buf))
      (kill-buffer chat-buf))))

;;;; Point Restoration (Phase 0 fix)

(ert-deftest piem-test-session-browser-rerender-restores-point ()
  "Rerender restores point to the same section and column.
pr-145's docstring claim was false: erasing the buffer always moved
point to bob.  Phase 0 restores it by section identity (value match)."
  (with-temp-buffer
    (piem-session-browser-mode)
    (setq piem--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 42 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/b.jsonl" :name "Session B"
                  :messageCount 20 :modified "2026-02-23T10:00:00Z")
                '(:path "/test/c.jsonl" :name "Session C"
                  :messageCount 10 :modified "2026-02-22T10:00:00Z")))
    (setq piem--session-browser-sort "relevance")
    (piem--session-browser-rerender)
    ;; Move point into session B's line, a few columns past bol
    (goto-char (point-min))
    (search-forward "Session B")
    (beginning-of-line)
    (forward-char 2)
    (let ((column (current-column)))
      (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
      (piem--session-browser-rerender)
      ;; Same section under point, same column
      (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
      (should (= (current-column) column)))))

(ert-deftest piem-test-session-browser-rerender-point-min-when-gone ()
  "Rerender falls back to point-min when the section at point disappears."
  (with-temp-buffer
    (piem-session-browser-mode)
    (setq piem--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 42 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/b.jsonl" :firstMessage "Unnamed prompt"
                  :messageCount 20 :modified "2026-02-23T10:00:00Z")))
    (setq piem--session-browser-sort "relevance")
    (piem--session-browser-rerender)
    ;; Point on the unnamed session
    (goto-char (point-min))
    (search-forward "Unnamed prompt")
    (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
    ;; Named-only filter removes it; its section is gone after rerender
    (setq piem--session-browser-named-only t)
    (piem--session-browser-rerender)
    (should (= (point) (point-min)))))

(ert-deftest piem-test-browse-rerender-syncs-window-point ()
  "Rerender restores `window-point', not just the buffer's own point.
The final fetch render runs from a timer while ANOTHER window is
selected: `goto-char' inside `with-current-buffer' moves the buffer's
point only, and every window displaying the browser buffer keeps its
own point — which `erase-buffer' already collapsed to bob.  The pane
shows point-at-top although the restore did work on the buffer's own
point (the intermittent instrumentation-vs-pane disagreement from
E2E).  The rerender must also `set-window-point' on live windows
displaying the buffer (same idiom as
`piem--with-scroll-preservation' in ui.el)."
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
            (piem-session-browser-mode)
            (setq piem--session-browser-items
                  (list '(:path "/test/a.jsonl" :name "Session A"
                          :messageCount 42 :modified "2026-02-24T10:00:00Z")
                        '(:path "/test/b.jsonl" :name "Session B"
                          :messageCount 20 :modified "2026-02-23T10:00:00Z")
                        '(:path "/test/c.jsonl" :name "Session C"
                          :messageCount 10 :modified "2026-02-22T10:00:00Z")))
            (setq piem--session-browser-sort "relevance")
            (piem--session-browser-rerender))
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
            (piem--session-browser-rerender))
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

(ert-deftest piem-test-session-browser-fetch-preserves-point ()
  "The full fetch cycle (`g' refresh) keeps point on the same row.
`--session-browser-fetch-and-render' renders an intermediate loading
state with no session sections before the final items render; point
must survive the whole cycle, not just a plain rerender (E2E defect
A4: `g' dropped point to bob because the loading render's rerender
lost the captured section ident)."
  (with-temp-buffer
    (piem-session-browser-mode)
    (let ((items (list '(:path "/test/a.jsonl" :name "Session A"
                         :messageCount 42 :modified "2026-02-24T10:00:00Z")
                       '(:path "/test/b.jsonl" :name "Session B"
                         :messageCount 20 :modified "2026-02-23T10:00:00Z")
                       '(:path "/test/c.jsonl" :name "Session C"
                         :messageCount 10 :modified "2026-02-22T10:00:00Z"))))
      (setq piem--session-browser-items items
            piem--session-browser-sort "relevance")
      (piem--session-browser-rerender)
      ;; Point on the middle row (relevance order is A, B, C), a few
      ;; columns past bol
      (goto-char (point-min))
      (search-forward "Session B")
      (beginning-of-line)
      (forward-char 2)
      (let ((column (current-column)))
        (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
        ;; Refresh: the scan returns the SAME items, synchronously
        (cl-letf (((symbol-function 'piem--browse-load-sessions)
                   (lambda (_scope callback)
                     (funcall callback items nil)))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args)
                     (apply fn args))))
          (piem--session-browser-fetch-and-render))
        (should-not piem--session-browser-loading)
        ;; Same section under point, same column, after the final render
        (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
        (should (= (current-column) column))))))

(ert-deftest piem-test-session-browser-fetch-point-min-when-gone ()
  "The fetch cycle falls back to point-min when the row at point is gone.
A refresh whose new item set no longer contains the pointed-at session
must leave point at bob, not on a stale neighbor — the same fallback a
plain rerender already guarantees."
  (with-temp-buffer
    (piem-session-browser-mode)
    (setq piem--session-browser-items
          (list '(:path "/test/a.jsonl" :name "Session A"
                  :messageCount 42 :modified "2026-02-24T10:00:00Z")
                '(:path "/test/b.jsonl" :name "Session B"
                  :messageCount 20 :modified "2026-02-23T10:00:00Z")
                '(:path "/test/c.jsonl" :name "Session C"
                  :messageCount 10 :modified "2026-02-22T10:00:00Z")))
    (setq piem--session-browser-sort "relevance")
    (piem--session-browser-rerender)
    ;; Point on the middle row
    (goto-char (point-min))
    (search-forward "Session B")
    (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
    ;; Refresh returns a set without session B (the named-only effect,
    ;; via a different item set)
    (cl-letf (((symbol-function 'piem--browse-load-sessions)
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
      (piem--session-browser-fetch-and-render))
    (should (= (point) (point-min)))))

(ert-deftest piem-test-session-browser-refresh-during-load-preserves-point ()
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
    (piem-session-browser-mode)
    (let ((items (list '(:path "/test/a.jsonl" :name "Session A"
                         :messageCount 42 :modified "2026-02-24T10:00:00Z")
                       '(:path "/test/b.jsonl" :name "Session B"
                         :messageCount 20 :modified "2026-02-23T10:00:00Z")
                       '(:path "/test/c.jsonl" :name "Session C"
                         :messageCount 10 :modified "2026-02-22T10:00:00Z")))
          (in-flight-callback nil))
      (setq piem--session-browser-items items
            piem--session-browser-sort "relevance")
      (piem--session-browser-rerender)
      ;; Point on the middle row (relevance order is A, B, C), a few
      ;; columns past bol
      (goto-char (point-min))
      (search-forward "Session B")
      (beginning-of-line)
      (forward-char 2)
      (let ((column (current-column)))
        (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
        (cl-letf (((symbol-function 'piem--browse-load-sessions)
                   ;; Fetch A: return control with the scan mid-flight —
                   ;; capture the callback, funcall nothing yet.
                   (lambda (_scope callback)
                     (setq in-flight-callback callback)))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args)
                     (apply fn args))))
          (piem--session-browser-fetch-and-render)
          ;; The first fetch is still loading; its loading render left
          ;; no session sections to anchor to.
          (should piem--session-browser-loading)
          (should (string-match-p "Loading" (buffer-string)))
          (should in-flight-callback)
          ;; While loading, press `g' again: fetch B reports the same
          ;; items synchronously (and, as with the real fetch token,
          ;; fetch A's callback never runs — it is dropped, not queued).
          (cl-letf (((symbol-function 'piem--browse-load-sessions)
                     (lambda (_scope callback)
                       (funcall callback items nil))))
            (piem--session-browser-fetch-and-render))
          (should-not piem--session-browser-loading))
        ;; The final render lands on the same middle row.
        (should (equal (oref (magit-current-section) value) "/test/b.jsonl"))
        (should (= (current-column) column))))))

(ert-deftest piem-test-session-browser-fetch-renders-in-browser-buffer ()
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
          (piem-session-browser-mode)
          (cl-letf (((symbol-function 'piem--browse-load-sessions)
                     (lambda (_scope callback)
                       ;; Callback fires with some OTHER buffer current.
                       (with-current-buffer other
                         (funcall callback items nil))))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args)
                       (apply fn args))))
            (piem--session-browser-fetch-and-render))
          ;; The browser buffer got the rows and cleared loading...
          (should-not piem--session-browser-loading)
          (should (string-match-p "Session A" (buffer-string)))
          ;; ...and the other buffer got no render.
          (should-not (string-match-p "Session A"
                                      (with-current-buffer other
                                        (buffer-string)))))
      (kill-buffer other))))

(ert-deftest piem-test-tree-browser-fetch-renders-in-browser-buffer ()
  "The tree fetch callback renders in the browser buffer, not the caller's.
Same latent defect as the session browser: the deferred disk read calls
back from a timer in whatever buffer is current.  The Phase 3 seam
callback receives (TREE LEAF-ID MESSAGE); the mock reports success, so
MESSAGE is nil and no process mock is needed anywhere (the tree comes
from disk, not the RPC)."
  (let* ((tree-data (piem--parse-tree
                     (piem-test--read-json-fixture "browse-tree.json")))
         (other (get-buffer-create " *pi-test-tree-other*")))
    (unwind-protect
        (with-temp-buffer
          (piem-tree-browser-mode)
          (cl-letf (((symbol-function 'piem--browse-load-tree)
                     (lambda (callback)
                       ;; Callback fires with some OTHER buffer current.
                       (with-current-buffer other
                         (funcall callback
                                  (plist-get tree-data :tree)
                                  (plist-get tree-data :leafId)
                                  nil))))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args)
                       (apply fn args))))
            (piem--tree-browser-fetch-and-render))
          ;; The browser buffer got the tree and cleared loading...
          (should-not piem--tree-browser-loading)
          (should (string-match-p "Actually" (buffer-string)))
          ;; ...and the other buffer got no render.
          (should-not (string-match-p "Actually"
                                      (with-current-buffer other
                                        (buffer-string)))))
      (kill-buffer other))))

(ert-deftest piem-test-tree-browser-rerender-restores-point ()
  "Tree rerender keeps point on the same node across filter changes."
  (with-temp-buffer
    (piem-tree-browser-mode)
    (let* ((response (piem-test--read-json-fixture "browse-tree.json"))
           (tree-data (piem--parse-tree response)))
      (setq piem--tree-browser-tree (plist-get tree-data :tree)
            piem--tree-browser-leaf-id (plist-get tree-data :leafId)
            piem--tree-browser-filter "default")
      (piem--tree-browser-rerender)
      ;; node-4 (user message) survives the no-tools filter
      (goto-char (point-min))
      (search-forward "Actually")
      (should (equal (oref (magit-current-section) value) "node-4"))
      (setq piem--tree-browser-filter "no-tools")
      (piem--tree-browser-rerender)
      (should (equal (oref (magit-current-section) value) "node-4")))))

;;;; Phase 0 Stub Seams

(ert-deftest piem-test-browse-stub-loaders-render-empty-states ()
  "Seam callbacks render empty and error states without signaling.
Both browsers read from disk, so neither needs a live process or a
--get-process mock: the session browser (Phase 2 disk scan) renders
its empty state with no sessions, and the tree browser (Phase 3 disk
read) with no linked chat renders its link-error message."
  (let ((session-buf (generate-new-buffer " *test-sessions*"))
        (tree-buf (generate-new-buffer " *test-tree*"))
        (root (piem-test--make-temp-directory "pi-stub-root")))
    (unwind-protect
        (progn
          (with-current-buffer session-buf
            (piem-session-browser-mode)
            ;; No --get-process mock: reading from disk needs no process.
            (let ((default-directory root)
                  (process-environment
                   (cons (format "PI_CODING_AGENT_DIR=%s"
                                (directory-file-name root))
                         process-environment)))
              (cl-letf (((symbol-function 'piem--session-list-directory)
                         (lambda (&optional _chat-buf) nil))
                        ((symbol-function 'run-at-time)
                         (lambda (_secs _repeat fn &rest args)
                           (apply fn args))))
                (piem--session-browser-fetch-and-render)))
            (should-not piem--session-browser-loading)
            (should-not piem--session-browser-error)
            (should (string-match-p "No sessions found" (buffer-string))))
          (with-current-buffer tree-buf
            (piem-tree-browser-mode)
            ;; No process mock and no chat link: the fetch renders the
            ;; link-error state instead of signaling.
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_secs _repeat fn &rest args)
                         (apply fn args))))
              (piem--tree-browser-fetch-and-render))
            (should-not piem--tree-browser-loading)
            (should (string-match-p "No linked pi chat session"
                                    (buffer-string)))))
      (kill-buffer session-buf)
      (kill-buffer tree-buf))))

(ert-deftest piem-test-browse-stub-actions-signal-user-error ()
  "Action seam contracts without a linked chat session.
The switch seam's Phase 2 contract is the no-session error when no
chat buffer is linked; labeling went live in Phase 3 and reports
Cannot-label instead of signaling.  Navigate went live in Phase 4 —
its guard contracts are pinned by the navigate tests below, so only
the RET routing stays pinned here, a structural binding check."
  (with-temp-buffer
    (piem-session-browser-mode)
    (should (equal (error-message-string
                    (should-error
                     (piem--browse-switch-session "/test/a.jsonl")
                     :type 'user-error))
                   "No pi session to switch to")))
  ;; RET still routes the tree browser — mode map and node sections —
  ;; into the navigate command.
  (should (eq (lookup-key piem-tree-browser-mode-map (kbd "RET"))
              'piem-tree-browser-navigate))
  (should (eq (lookup-key piem-tree-node-section-map (kbd "RET"))
              'piem-tree-browser-navigate))
  ;; Labeling without a resolvable session file: message, no signal.
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (piem--browse-set-label "node-1" "label"))
    (should (cl-some (lambda (m)
                       (string-match-p "Cannot label: no session file" m))
                     messages))))

;;;; Phase 2: Raw Session File Helpers

(defconst piem-test--browse-timestamp "2026-03-02T10:00:00.000Z"
  "Fixed entry timestamp for browse test session lines.")

(defconst piem-test--iso-second-re
  "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z\\'"
  "Regexp for a second-resolution UTC ISO-8601 timestamp.")

(defun piem-test--jsonl-line (type id parent &rest payload)
  "Return a raw JSONL line for an entry of TYPE, ID, and PARENT id.
PARENT is an id string or nil (JSON null).  PAYLOAD is the plist tail
(:message, :targetId, :name, ...)."
  (json-encode (append (list :type type :id id :parentId parent
                             :timestamp piem-test--browse-timestamp)
                       payload)))

(defun piem-test--user-line (id parent text)
  "Return a raw user message JSONL line with content TEXT."
  (piem-test--jsonl-line
   "message" id parent :message `(:role "user" :content ,text)))

(defun piem-test--write-session-lines (path lines &optional omit-final-newline)
  "Write raw string LINES to PATH, separated by single newlines.
OMIT-FINAL-NEWLINE skips the trailing newline, producing the shape of a
crashed or hand-edited session file (rename must re-add the separator)."
  (with-temp-file path
    (when lines
      (insert (mapconcat #'identity lines "\n"))
      (unless omit-final-newline (insert "\n")))))

(defun piem-test--file-contents (path &optional literally)
  "Return PATH's contents.
When LITERALLY is non-nil, return unibyte file bytes with no coding or
end-of-line conversion; otherwise return decoded text."
  (with-temp-buffer
    (if literally
        (progn
          (set-buffer-multibyte nil)
          (insert-file-contents-literally path))
      (insert-file-contents path))
    (buffer-string)))

(defun piem-test--make-session-header (id &rest extra)
  "Return a raw session header line with id ID and EXTRA plist tail."
  (json-encode (append (list :type "session" :version 3 :id id
                             :timestamp piem-test--browse-timestamp
                             :cwd "/home/fake/a")
                       extra)))

(defmacro piem-test--with-browse-link (chat-buf &rest body)
  "Run BODY in a session-browser buffer linked to CHAT-BUF."
  (declare (indent 1) (debug (sexp body)))
  `(with-temp-buffer
     (piem-session-browser-mode)
     (setq piem--chat-buffer ,chat-buf)
     ,@body))

;;;; Phase 2: Disk Scan and Chunked Loading

(ert-deftest piem-test-browse-current-session-directory-without-menu ()
  "Fall back to the munged current-project directory without menu.el."
  (let ((saved-function
         (symbol-function 'piem--session-list-directory))
        (sandbox (make-temp-file "pi-browse-current-" t)))
    (unwind-protect
        (let ((agent-root (expand-file-name "agent" sandbox))
              (project (expand-file-name "project" sandbox)))
          (make-directory agent-root)
          (make-directory project)
          (fmakunbound 'piem--session-list-directory)
          (let ((default-directory (file-name-as-directory project))
                (piem--chat-buffer nil)
                (process-environment (copy-sequence process-environment)))
            (setenv "PI_CODING_AGENT_DIR" agent-root)
            (should
             (equal
              (piem--browse-current-session-directory)
              (piem-jsonl-session-dir-for-cwd
               default-directory)))))
      (fset 'piem--session-list-directory saved-function)
      (when (file-directory-p sandbox)
        (delete-directory sandbox t)))))

(ert-deftest piem-test-scan-discovers-tree ()
  "--browse-load-sessions scans the sessions tree from disk.
scope=all finds every munged --…-- directory under the sessions root,
threads forks through :parentSessionPath, reads names from
session_info, and skips non-session JSONL files, .subagents sidecars,
and non-munged directories.  scope=current scans one directory."
  (let* ((root (piem-test--make-temp-directory "pi-scan-root"))
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
    (piem-test--write-session-lines
     root-path
     (list (piem-test--make-session-header "sid-root")
           (piem-test--user-line "m1" nil "fix the parser")
           (piem-test--jsonl-line
            "message" "m2" "m1"
            :message '(:role "toolResult" :toolCallId "tc1" :toolName "read"))
           (piem-test--jsonl-line
            "message" "m3" "m2"
            :message '(:role "assistant" :content "done"))
           (piem-test--jsonl-line
            "session_info" "s1" "m3" :name "Root work")))
    (piem-test--write-session-lines
     fork-path
     (list (piem-test--make-session-header
            "sid-fork" :parentSession root-path)
           (piem-test--user-line "f1" nil "try the other way")))
    (piem-test--write-session-lines
     other-path
     (list (piem-test--make-session-header "sid-other")
           (piem-test--user-line "o1" nil "unrelated work")))
    ;; Decoy: a .jsonl file that is not a session (no header line).
    (piem-test--write-session-lines
     broken-path
     (list (piem-test--user-line "x1" nil "decoy")))
    ;; Valid sessions in excluded locations: a .subagents sidecar (the
    ;; scan is single-level inside munged dirs) and a non-munged dir.
    (piem-test--write-session-lines
     sub-path (list (piem-test--make-session-header "sid-sub")))
    (piem-test--write-session-lines
     stray-path (list (piem-test--make-session-header "sid-stray")))
    (with-temp-buffer
      (piem-session-browser-mode)
      (let ((default-directory root)
            (process-environment
             (cons (format "PI_CODING_AGENT_DIR=%s" (directory-file-name root))
                   process-environment)))
        (cl-letf (((symbol-function 'piem--session-list-directory)
                   (lambda (&optional _chat-buf) nil))
                  ;; Synchronous timers: the chunked scan completes here.
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args) (apply fn args))))
          (piem--browse-load-sessions
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
                           piem-test--browse-timestamp))
            (should (equal (plist-get root-item :name) "Root work"))
            (should (equal (plist-get root-item :firstMessage) "fix the parser"))
            (should (= (plist-get root-item :messageCount) 3))
            (should (string-match-p piem-test--iso-second-re
                                    (plist-get root-item :modified)))
            ;; The fork threads to its parent session file.
            (should (equal (plist-get fork-item :parentSessionPath) root-path))
            (should-not (plist-get fork-item :name))))
        ;; scope=current scans exactly one directory: the menu-supplied
        ;; session list directory.
        (setq calls nil)
        (cl-letf (((symbol-function 'piem--session-list-directory)
                   (lambda (&optional _chat-buf) dir-a))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args) (apply fn args))))
          (piem--browse-load-sessions
           "current" (lambda (items error) (push (list items error) calls))))
        (pcase-let ((`(,items ,error) (car calls)))
          (should-not error)
          (should (equal (sort (mapcar (lambda (i) (plist-get i :path)) items)
                               #'string<)
                         (sort (list root-path fork-path) #'string<))))))))

(ert-deftest piem-test-load-sessions-chunked ()
  "--browse-load-sessions chunks long scans and reports once.
The per-file reader is slowed past the 25 ms slice budget so the scan
spans several slices.  Synchronous timers deliver exactly one final
callback with every item, and a superseded fetch's callback is dropped
by the fetch token."
  (let* ((root (piem-test--make-temp-directory "pi-chunk-root"))
         (sessions (expand-file-name "sessions" root))
         (dir (expand-file-name "--home-fake-a--" sessions))
         (paths nil))
    (make-directory dir t)
    (dotimes (i 60)
      (let ((path (expand-file-name (format "s%03d.jsonl" i) dir)))
        (push path paths)
        (piem-test--write-session-lines
         path (list (piem-test--make-session-header
                     (format "sid-%03d" i))))))
    (setq paths (nreverse paths))
    (with-temp-buffer
      (piem-session-browser-mode)
      (let ((default-directory root)
            (process-environment
             (cons (format "PI_CODING_AGENT_DIR=%s" (directory-file-name root))
                   process-environment)))
        (cl-letf (((symbol-function 'piem--session-list-directory)
                   (lambda (&optional _chat-buf) nil))
                  ;; 2 ms per file: 60 files need several 25 ms slices.
                  ((symbol-function 'piem-jsonl-read-session-info)
                   (lambda (path)
                     (sleep-for 0 2)
                     (list :path path
                           :id (file-name-nondirectory path)
                           :cwd "/home/fake/a"
                           :created piem-test--browse-timestamp
                           :modified "2026-03-02T10:00:00Z"
                           :messageCount 0))))
          ;; Synchronous timers: one final callback, all 60 items.
          (let ((calls nil))
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_secs _repeat fn &rest args) (apply fn args))))
              (piem--browse-load-sessions
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
              (piem--browse-load-sessions
               "all" (lambda (items error) (push (list items error) calls-a)))
              (piem--browse-load-sessions
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
              (piem--browse-load-sessions
               "all" (lambda (items error) (push (list items error) calls-a)))
              (let ((job (pop queue)))
                (apply (car job) (cdr job)))
              (piem--browse-load-sessions
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

(ert-deftest piem-test-load-sessions-error-as-string ()
  "Directory resolution failures surface as an error string, not a signal."
  (let ((calls nil))
    (with-temp-buffer
      (piem-session-browser-mode)
      (cl-letf (((symbol-function 'piem--session-list-directory)
                 (lambda (&optional _chat-buf)
                   (signal 'file-error '("Cannot access sessions directory"))))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest args) (apply fn args))))
        (piem--browse-load-sessions
         "current" (lambda (items error) (push (list items error) calls))))
      (should (eq (length calls) 1))
      (pcase-let ((`(,items ,error) (car calls)))
        (should-not items)
        (should (stringp error))
        (should (string-match-p "Cannot list sessions" error))))))

(ert-deftest piem-test-load-sessions-interrupted-by-quit ()
  "A quit during a scan slice reports an error state, not a stuck
loading render (session-side analog of
`piem-test-load-tree-interrupted-by-quit').  C-g against a
slow scan raises `quit' — not `error' — inside the slice loop; the
seam must still call back exactly once so the loading state clears
and the browser names the interruption."
  (let* ((root (piem-test--make-temp-directory "pi-scan-quit"))
         (sessions (expand-file-name "sessions" root))
         (dir (expand-file-name "--home-fake-a--" sessions))
         (path (expand-file-name "session.jsonl" dir))
         (calls nil))
    (make-directory dir t)
    (piem-test--write-session-lines
     path (list (piem-test--make-session-header "sid-quit")))
    (with-temp-buffer
      (piem-session-browser-mode)
      ;; The fetch cycle reads the buffer-local scope; "all" sees the
      ;; munged directory below ("current" would munge the temp root
      ;; itself, which holds no sessions).
      (setq piem--session-browser-scope "all")
      (let ((default-directory root)
            (process-environment
             (cons (format "PI_CODING_AGENT_DIR=%s" (directory-file-name root))
                   process-environment)))
        (cl-letf (((symbol-function 'piem--session-list-directory)
                   (lambda (&optional _chat-buf) nil))
                  ((symbol-function 'piem-jsonl-read-session-info)
                   (lambda (_path) (signal 'quit nil)))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args) (apply fn args))))
          ;; The seam reports the interruption exactly once.
          (piem--browse-load-sessions
           "all" (lambda (items error) (push (list items error) calls)))
          (should (eq (length calls) 1))
          (pcase-let ((`(,items ,error) (car calls)))
            (should-not items)
            (should (string-match-p "interrupted" error)))
          ;; The full fetch cycle clears the loading state and shows
          ;; the interruption instead of "Loading sessions...".
          (piem--session-browser-fetch-and-render))
        (should-not piem--session-browser-loading)
        (should (string-match-p "interrupted"
                                piem--session-browser-error))
        (should (string-match-p "interrupted" (buffer-string)))))))

;;;; Phase 2: Fetch Relaxation

(ert-deftest piem-test-fetch-without-process ()
  "The session browser fetch proceeds without a live pi process.
Phase 2 reads sessions from disk, so the Phase 0 no-process guard is
gone for the session browser (the tree browser keeps it until Phase 3)."
  (let ((root (piem-test--make-temp-directory "pi-noproc-root")))
    (with-temp-buffer
      (piem-session-browser-mode)
      (let ((default-directory root)
            (process-environment
             (cons (format "PI_CODING_AGENT_DIR=%s" (directory-file-name root))
                   process-environment)))
        (cl-letf (((symbol-function 'piem--session-list-directory)
                   (lambda (&optional _chat-buf) nil))
                  ((symbol-function 'run-at-time)
                   (lambda (_secs _repeat fn &rest args) (apply fn args))))
          (piem--session-browser-fetch-and-render)))
      (should-not piem--session-browser-loading)
      (should-not piem--session-browser-error)
      (should (string-match-p "No sessions found" (buffer-string))))))

;;;; Phase 2: Switch

(ert-deftest piem-test-switch-calls-resume ()
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
            (setq piem--process proc))
          (piem-test--with-browse-link chat-buf
            (cl-letf (((symbol-function 'piem--session-transition-ready-p)
                       (lambda (chat-buf action)
                         (push (list chat-buf action) ready-calls)
                         t))
                      ((symbol-function 'piem--resume-selected-session)
                       (lambda (proc chat-buf path)
                         (push (list proc chat-buf path) resume-calls))))
              (piem--browse-switch-session "/tmp/some-session.jsonl")))
          (should (equal ready-calls (list (list chat-buf "switch"))))
          (should (equal resume-calls
                         (list (list proc chat-buf "/tmp/some-session.jsonl")))))
      (delete-process proc)
      (kill-buffer chat-buf))))

(ert-deftest piem-test-switch-busy-guard ()
  "A busy chat session blocks the switch before any resume attempt."
  (let* ((chat-buf (generate-new-buffer " *test-busy-chat*"))
         (proc (start-process "pi-busy-test" nil "sleep" "30"))
         (resume-calls nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq piem--process proc))
          (piem-test--with-browse-link chat-buf
            (cl-letf (((symbol-function 'piem--session-transition-ready-p)
                       (lambda (&rest _) nil))
                      ((symbol-function 'piem--resume-selected-session)
                       (lambda (&rest _) (push t resume-calls))))
              ;; Returns quietly: the guard reports the reason itself.
              (piem--browse-switch-session "/tmp/some-session.jsonl")))
          (should-not resume-calls))
      (delete-process proc)
      (kill-buffer chat-buf))))

(ert-deftest piem-test-switch-active-transition-guard ()
  "An in-flight session transition blocks a second switch before any
resume attempt.  The transition latch keeps the status idle, so
`--session-transition-ready-p' cannot see it (same gate navigate
already has); without the explicit `--session-transition-active-p'
check, RET on two rows would start two racing switch_session
transitions."
  (let* ((chat-buf (generate-new-buffer " *test-switch-active-chat*"))
         (proc (start-process "pi-switch-active-test" nil "sleep" "30"))
         (ready-calls nil)
         (resume-calls nil)
         (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq piem--process proc
                  ;; Mid-transition: the latch is set but the status is
                  ;; still idle, so the ready guard alone would pass.
                  piem--session-transition-active t))
          (piem-test--with-browse-link chat-buf
            (cl-letf (((symbol-function 'piem--session-transition-ready-p)
                       (lambda (chat-buf action)
                         (push (list chat-buf action) ready-calls)
                         t))
                      ((symbol-function 'piem--resume-selected-session)
                       (lambda (&rest _) (push t resume-calls)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages))))
              (piem--browse-switch-session "/tmp/some-session.jsonl")))
          (should (member "Pi: Cannot switch while switching sessions"
                          messages))
          ;; The active gate fires before the ready guard runs at all.
          (should-not ready-calls)
          (should-not resume-calls))
      (delete-process proc)
      (kill-buffer chat-buf))))

(ert-deftest piem-test-switch-no-session ()
  "Switching with no linked chat session signals a `user-error'."
  (with-temp-buffer
    (piem-session-browser-mode)
    (should (equal (error-message-string
                    (should-error
                     (piem--browse-switch-session "/test/a.jsonl")
                     :type 'user-error))
                   "No pi session to switch to"))))

(ert-deftest piem-test-quit-when-settled ()
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
            (piem-session-browser-mode))
          (set-window-buffer win browser-buf)
          ;; Settled onto the target: one busy poll, then quit-window.
          (with-current-buffer chat-buf
            (setq piem--state (list :session-file path)))
          (cl-letf (((symbol-function 'piem--session-transition-active-p)
                     (lambda (&optional _chat-buf)
                       (setq polls (1+ polls))
                       (<= polls 1)))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args) (apply fn args)))
                    ((symbol-function 'quit-window)
                     (lambda (&rest args) (push args quit-calls))))
            (piem--browse-quit-when-settled chat-buf win path))
          (should (>= polls 2))
          (should (equal quit-calls (list (list nil win))))
          ;; Settled elsewhere: the browser stays open.
          (setq quit-calls nil polls 0)
          (with-current-buffer chat-buf
            (setq piem--state (list :session-file "/tmp/other.jsonl")))
          (cl-letf (((symbol-function 'piem--session-transition-active-p)
                     (lambda (&optional _chat-buf)
                       (setq polls (1+ polls))
                       (<= polls 1)))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args) (apply fn args)))
                    ((symbol-function 'quit-window)
                     (lambda (&rest args) (push args quit-calls))))
            (piem--browse-quit-when-settled chat-buf win path))
          (should (>= polls 2))
          (should-not quit-calls)
          ;; Window repurposed mid-poll (browse buffer killed): landing on
          ;; the target must NOT quit whatever the window shows now.
          (setq quit-calls nil polls 0)
          (with-current-buffer chat-buf
            (setq piem--state (list :session-file path)))
          (set-window-buffer win other-buf)
          (cl-letf (((symbol-function 'piem--session-transition-active-p)
                     (lambda (&optional _chat-buf)
                       (setq polls (1+ polls))
                       (<= polls 1)))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args) (apply fn args)))
                    ((symbol-function 'quit-window)
                     (lambda (&rest args) (push args quit-calls))))
            (piem--browse-quit-when-settled chat-buf win path))
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
            (piem--browse-quit-when-settled chat-buf win path))
          (should-not quit-calls))
      (set-window-buffer win orig-buf)
      (kill-buffer browser-buf)
      (kill-buffer other-buf)
      (when (buffer-live-p chat-buf) (kill-buffer chat-buf)))))

;;;; Phase 2: Rename

(defun piem-test--rename-at-point (item chat-buf input)
  "Run `session-browser-rename' with INPUT at ITEM's section.
ITEM is a session plist (its :name locates the section); CHAT-BUF is
the browse buffer's chat link.  The post-rename refresh is stubbed
out; callers mock the rename seams they assert on."
  (with-temp-buffer
    (piem-session-browser-mode)
    (setq piem--chat-buffer chat-buf
          piem--session-browser-items (list item))
    (piem--session-browser-rerender)
    (goto-char (point-min))
    (search-forward (plist-get item :name))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &rest _) input))
              ((symbol-function 'piem--session-browser-fetch-and-render)
               #'ignore))
      (piem-session-browser-rename))))

(ert-deftest piem-test-rename-other-session-appends ()
  "Renaming a non-current session appends exactly one session_info line.
The line carries a fresh 8-hex id, parents to the id of the file's last
line, a UTC ISO timestamp no older than the rename, and the cleaned
name.  A missing trailing newline gets a separator; prior bytes stay
byte-for-byte intact."
  (let* ((dir (piem-test--make-temp-directory "pi-rename-append"))
         (path (expand-file-name "target.jsonl" dir))
         (current-path (expand-file-name "current.jsonl" dir))
         (before-lines
          (list (piem-test--make-session-header "sid-target")
                (piem-test--user-line "m1" nil "investigate the flaky test")
                (piem-test--jsonl-line
                 "message" "m2" "m1"
                 :message '(:role "assistant" :content "found it"))
                (piem-test--jsonl-line
                 "session_info" "s1" "m2" :name "Old name")))
         (chat-buf (generate-new-buffer " *test-rename-chat*"))
         (start-iso (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t)))
    ;; No trailing newline: the append must add the separator itself.
    (piem-test--write-session-lines path before-lines t)
    (piem-test--write-session-lines
     current-path (list (piem-test--make-session-header "sid-current")))
    (unwind-protect
        (let ((before (piem-test--file-contents path)))
          (with-current-buffer chat-buf
            (setq piem--state (list :session-file current-path)))
          (piem-test--rename-at-point
           (list :path path :name "Old name" :messageCount 2
                 :modified "2026-03-02T10:00:00Z")
           chat-buf
           "  Renamed\nSession  ")
          (let* ((after (piem-test--file-contents path)))
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

(ert-deftest piem-test-rename-append-garbage-tail ()
  "Renaming a session with a garbage final line parents past the garbage.
pi's loader skips malformed lines, so the append's :parentId must be the
id of the last PARSEABLE line — parenting to the garbage (or to nil)
would detach the whole conversation from the reload context.  The
garbage bytes stay byte-for-byte intact."
  (let* ((dir (piem-test--make-temp-directory "pi-rename-garbage"))
         (path (expand-file-name "torn.jsonl" dir))
         (garbage "{\"type\":\"message\",\"id\":\"torn\",\"paren")
         (chat-buf (generate-new-buffer " *test-garbage-chat*")))
    (piem-test--write-session-lines
     path
     (list (piem-test--make-session-header "sid-g")
           (piem-test--user-line "g1" nil "check the flaky test")
           (piem-test--jsonl-line
            "message" "g2" "g1"
            :message '(:role "assistant" :content "fixed"))
           garbage))
    (unwind-protect
        (let ((before (piem-test--file-contents path)))
          (with-current-buffer chat-buf
            (setq piem--state
                  (list :session-file "/tmp/somewhere-else.jsonl")))
          (piem-test--rename-at-point
           (list :path path :name "Torn tail session" :messageCount 2
                 :modified "2026-03-02T10:00:00Z")
           chat-buf "Fixed name")
          (let* ((after (piem-test--file-contents path))
                 (appended (car (split-string (substring after (length before))
                                              "\n" t)))
                 (entry (json-parse-string appended :object-type 'plist))
                 (state (piem--browse-session-file-state path)))
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

(ert-deftest piem-test-rename-append-unreadable-file ()
  "Renaming a session whose file vanished cancels with a message.
No line is appended, no file is created, and the browser is not
refreshed (the fetch-and-render seam stays silent)."
  (let* ((dir (piem-test--make-temp-directory "pi-rename-missing"))
         (path (expand-file-name "ghost.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-missing-chat*"))
         (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq piem--state
                  (list :session-file "/tmp/somewhere-else.jsonl")))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (piem-test--rename-at-point
             (list :path path :name "Ghost session" :messageCount 0
                   :modified "2026-03-02T10:00:00Z")
             chat-buf "Ghost name"))
          (should-not (file-exists-p path))
          (should (cl-some (lambda (m) (string-match-p "unreadable" m))
                           messages)))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-rename-dispatch ()
  "Rename routes by current-vs-other session and cancels on empty input.
Current: `set-session-name' RPC only, no file append.  Other: file
append only, no RPC.  Empty (whitespace) input cancels with a message:
no RPC, no append (no clearing in Phase 2)."
  (let* ((dir (piem-test--make-temp-directory "pi-rename-dispatch"))
         (path (expand-file-name "target.jsonl" dir))
         (current-path (expand-file-name "current.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-dispatch-chat*"))
         (set-name-calls nil)
         (messages nil))
    (piem-test--write-session-lines
     path
     (list (piem-test--make-session-header "sid-target")
           (piem-test--user-line "m1" nil "other session")
           (piem-test--jsonl-line
            "session_info" "s1" "m1" :name "Target name")))
    (piem-test--write-session-lines
     current-path (list (piem-test--make-session-header "sid-current")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq piem--state (list :session-file current-path)))
          ;; Current session: RPC rename, no append to any file.
          (let ((target-before (piem-test--file-contents path))
                (current-before (piem-test--file-contents current-path)))
            (cl-letf (((symbol-function 'piem-set-session-name)
                       (lambda (&rest args)
                         (interactive)
                         (push args set-name-calls))))
              (piem-test--rename-at-point
               (list :path current-path :name "Current session" :messageCount 0
                     :modified "2026-03-02T10:00:00Z")
               chat-buf "  New\nName  "))
            (should (equal set-name-calls '(("New Name"))))
            (should (equal (piem-test--file-contents path)
                           target-before))
            (should (equal (piem-test--file-contents current-path)
                           current-before)))
          ;; Other session: append, no RPC.
          (setq set-name-calls nil)
          (let ((before (piem-test--file-contents path)))
            (cl-letf (((symbol-function 'piem-set-session-name)
                       (lambda (&rest args)
                         (interactive)
                         (push args set-name-calls))))
              (piem-test--rename-at-point
               (list :path path :name "Target name" :messageCount 1
                     :modified "2026-03-02T10:00:00Z")
               chat-buf "Fresh name"))
            (should-not set-name-calls)
            (should-not (equal (piem-test--file-contents path) before))
            (should (string-match-p "Fresh name"
                                    (piem-test--file-contents path))))
          ;; Empty input: cancelled for both paths; message only.
          (setq set-name-calls nil)
          (let ((target-before (piem-test--file-contents path))
                (current-before (piem-test--file-contents current-path)))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages)))
                      ((symbol-function 'piem-set-session-name)
                       (lambda (&rest args)
                         (interactive)
                         (push args set-name-calls))))
              (piem-test--rename-at-point
               (list :path path :name "Target name" :messageCount 1
                     :modified "2026-03-02T10:00:00Z")
               chat-buf "  \n "))
            (should-not set-name-calls)
            (should (equal (piem-test--file-contents path)
                           target-before))
            (should (equal (piem-test--file-contents current-path)
                           current-before))
            (should (member "Pi: Rename cancelled" messages))))
      (kill-buffer chat-buf))))

;;;; Phase 3: Tree Browser Live (disk-based) + Labels

(defmacro piem-test--with-tree-link (chat-buf &rest body)
  "Run BODY in a tree-browser buffer linked to CHAT-BUF."
  (declare (indent 1) (debug (sexp body)))
  `(with-temp-buffer
     (piem-tree-browser-mode)
     (setq piem--chat-buffer ,chat-buf)
     ,@body))

(defun piem-test--live-session-lines (&optional with-label)
  "Return raw session lines for a realistic small session.
Header, a user prompt, an assistant grep tool round-trip, and a final
assistant text turn.  WITH-LABEL appends one label line targeting the
user message, so the raw leaf is a filtered label entry and the
projected leaf resolves up to the last visible entry."
  (append
   (list (piem-test--make-session-header "sid-live")
         (piem-test--user-line "m1" nil "fix the parser")
         (piem-test--jsonl-line
          "message" "m2" "m1"
          :message '(:role "assistant"
                     :content [(:type "text" :text "checking the Makefile")
                               (:type "toolCall" :id "tc1" :name "grep"
                                      :arguments (:pattern "ldflags"
                                                  :path "/srv/demo/Makefile"))]
                     :stopReason "tool_calls"))
         (piem-test--jsonl-line
          "message" "m3" "m2"
          :message '(:role "toolResult" :toolCallId "tc1" :toolName "grep"
                     :output [(:type "text" :text "Makefile:14: LDFLAGS")]))
         (piem-test--jsonl-line
          "message" "m4" "m3"
          :message '(:role "assistant" :content "done"
                             :stopReason "end_turn")))
   (when with-label
     (list (piem-test--jsonl-line
            "label" "l1" "m4" :targetId "m1" :label "checkpoint")))))

(defun piem-test--tree-find-node (tree id)
  "Find the projected node with :id ID in TREE; nil when absent.
Traversal is iterative."
  (let ((stack (append tree nil))
        (found nil))
    (while (and stack (not found))
      (let ((node (pop stack)))
        (if (equal (plist-get node :id) id)
            (setq found node)
          (setq stack (append (append (plist-get node :children) nil)
                              stack)))))
    found))

(defun piem-test--margin-overlay-contains-p (text)
  "Return non-nil when a right-margin overlay shows TEXT in this buffer."
  (cl-some
   (lambda (o)
     (let* ((bs (overlay-get o 'before-string))
            (display (and bs (get-text-property 0 'display bs))))
       (and display (string-match-p (regexp-quote text) (cadr display)))))
   (overlays-in (point-min) (point-max))))

(defmacro piem-test--sync-timers (body)
  "Run BODY with `run-at-time' shimmed to run its job synchronously."
  (declare (indent 0) (debug (lambda)))
  `(cl-letf (((symbol-function 'run-at-time)
              (lambda (_secs _repeat fn &rest args)
                (apply fn args))))
     (funcall ,body)))

(ert-deftest piem-test-load-tree-reads-and-projects-session-file ()
  "--browse-load-tree reads and projects the linked chat's session file.
The seam callback receives (TREE LEAF-ID MESSAGE): TREE and LEAF-ID are
`equal' to the direct jsonl pipeline (read-file, build-tree,
project-tree) over the same file, and MESSAGE is nil on success.  The
label line folds onto its target and the raw leaf (the label entry)
resolves up to the last visible entry.  The fetch cycle records the
freshly resolved file in `--tree-browser-loaded-file' and renders it."
  (let* ((dir (piem-test--make-temp-directory "pi-tree-live"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-live-chat*"))
         (calls nil))
    (piem-test--write-session-lines
     path (piem-test--live-session-lines t))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq piem--state (list :session-file path)))
          (piem-test--with-tree-link chat-buf
            ;; Direct seam call: the deferred read delivers synchronously
            ;; under the timer shim.
            (piem-test--sync-timers
              (lambda ()
                (piem--browse-load-tree
                 (lambda (tree leaf-id message)
                   (push (list tree leaf-id message) calls)))))
            (should (equal (length calls) 1))
            (pcase-let ((`(,tree ,leaf-id ,message) (car calls)))
              (should-not message)
              (let* ((session (piem-jsonl-read-file path))
                     (built (piem-jsonl-build-tree
                             (plist-get session :entries)))
                     (expected (piem-jsonl-project-tree
                                (plist-get built :tree)
                                (plist-get built :leafId))))
                (should (equal tree (plist-get expected :tree)))
                (should (equal leaf-id (plist-get expected :leafId))))
              ;; The label folds onto its target; the raw leaf is the
              ;; label entry, whose projected leaf resolves up to m4.
              (should (equal (plist-get
                              (piem-test--tree-find-node tree "m1")
                              :label)
                             "checkpoint"))
              (should (equal leaf-id "m4")))
            ;; Fetch cycle: the loaded file is recorded and the tree
            ;; renders in the browser buffer.
            (piem-test--sync-timers
              (lambda ()
                (piem--tree-browser-fetch-and-render)))
            (should (equal piem--tree-browser-loaded-file path))
            (should-not piem--tree-browser-loading)
            (should-not piem--tree-browser-error)
            (should (string-match-p "fix the parser" (buffer-string)))))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-load-tree-no-chat-link ()
  "A tree fetch with no linked chat renders the link error state.
The message names the pi chat session, the fetch renders it as text
with a zero visible count, and nothing is treated as loaded.  The
entry point guards the same condition with a `user-error' before any
browser buffer is created."
  (with-temp-buffer
    (piem-tree-browser-mode)
    (piem-test--sync-timers
      (lambda () (piem--tree-browser-fetch-and-render)))
    (should (string-match-p "No linked pi chat session" (buffer-string)))
    (should-not piem--tree-browser-loading)
    (should (string-match-p "chat" piem--tree-browser-error))
    (should (= piem--tree-browser-visible-count 0))
    (should-not piem--tree-browser-loaded-file))
  ;; Entry point: the guard fires before any buffer is created.
  (let* ((created nil)
         (guard-buf (generate-new-buffer " *pi-test-tree-guard*")))
    (unwind-protect
        (cl-letf (((symbol-function 'piem--get-chat-buffer)
                   (lambda () nil))
                  ((symbol-function 'piem--get-or-create-tree-browser)
                   (lambda (&rest _)
                     (setq created t)
                     guard-buf))
                  ((symbol-function 'pop-to-buffer) #'ignore)
                  ((symbol-function 'piem--browse-apply-margins)
                   #'ignore)
                  ((symbol-function
                    'piem--tree-browser-fetch-and-render)
                   #'ignore))
          (should (equal (error-message-string
                          (should-error (piem-tree-browser)
                                        :type 'user-error))
                         "No pi session to browse"))
          (should-not created))
      (kill-buffer guard-buf))))

(ert-deftest piem-test-load-tree-no-session-file-yet ()
  "A live chat link whose session file does not exist yet renders the
not-yet state — both a state without :session-file and one naming a
nonexistent path.  No signal in either case; the visible count is zero."
  (let* ((dir (piem-test--make-temp-directory "pi-tree-nofile"))
         (chat-buf (generate-new-buffer " *test-tree-nofile-chat*")))
    (unwind-protect
        (piem-test--with-tree-link chat-buf
          ;; State without a :session-file key.
          (with-current-buffer chat-buf
            (setq piem--state (list :messageCount 0)))
          (piem-test--sync-timers
            (lambda () (piem--tree-browser-fetch-and-render)))
          (should (string-match-p "No session file yet" (buffer-string)))
          (should (string-match-p "No session file yet"
                                  piem--tree-browser-error))
          (should (= piem--tree-browser-visible-count 0))
          (should-not piem--tree-browser-loaded-file)
          ;; State naming a path that does not exist yet: the file is
          ;; only created on the first assistant reply.
          (with-current-buffer chat-buf
            (setq piem--state
                  (list :session-file (expand-file-name "pending.jsonl" dir))))
          (piem-test--sync-timers
            (lambda () (piem--tree-browser-fetch-and-render)))
          (should (string-match-p "No session file yet" (buffer-string)))
          (should (string-match-p "first assistant"
                                  piem--tree-browser-error)))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-load-tree-unreadable-session-file ()
  "An existing session file that does not parse as a pi session renders
the unreadable state naming the file — garbage and headerless files
both read as nil, which is an error message, never a signal."
  (let* ((dir (piem-test--make-temp-directory "pi-tree-garbage"))
         (garbage (expand-file-name "garbage.jsonl" dir))
         (headerless (expand-file-name "headerless.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-garbage-chat*")))
    (with-temp-file garbage
      (insert "this is not json\n{\"type\":\"message\",\"id\":\"g1\"}\n"))
    (piem-test--write-session-lines
     headerless (list (piem-test--user-line "g1" nil "decoy")))
    (unwind-protect
        (piem-test--with-tree-link chat-buf
          (dolist (path (list garbage headerless))
            (with-current-buffer chat-buf
              (setq piem--state (list :session-file path)))
            (piem-test--sync-timers
              (lambda () (piem--tree-browser-fetch-and-render)))
            (should (string-match-p "unreadable" (buffer-string)))
            (should (string-match-p
                     (format "Session file is unreadable or not a pi session file: %s"
                             (regexp-quote path))
                     piem--tree-browser-error))
            (should (= piem--tree-browser-visible-count 0))
            (should-not piem--tree-browser-loaded-file)))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-load-tree-deferred-and-tokened ()
  "--browse-load-tree defers the disk read and honors the fetch token.
The read is queued through `run-at-time'; a superseding fetch bumps
the buffer's fetch token so the older timer drops itself without
calling back, and the newer one reports exactly once with the
projected tree and no error."
  (let* ((dir (piem-test--make-temp-directory "pi-tree-defer"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-defer-chat*")))
    (piem-test--write-session-lines
     path (piem-test--live-session-lines))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq piem--state (list :session-file path)))
          (piem-test--with-tree-link chat-buf
            ;; Deferred timers: A is queued, B supersedes it before any
            ;; timer runs; running the queue drops A and reports B once.
            (let ((calls-a nil) (calls-b nil) (queue nil))
              (cl-letf (((symbol-function 'run-at-time)
                         (lambda (_secs _repeat fn &rest args)
                           (push (cons fn args) queue))))
                (piem--browse-load-tree
                 (lambda (tree leaf-id message)
                   (push (list tree leaf-id message) calls-a)))
                (piem--browse-load-tree
                 (lambda (tree leaf-id message)
                   (push (list tree leaf-id message) calls-b)))
                (while queue
                  (let ((job (pop queue)))
                    (apply (car job) (cdr job)))))
              (should-not calls-a)
              (should (equal (length calls-b) 1))
              (pcase-let ((`(,tree ,leaf-id ,message) (car calls-b)))
                (should-not message)
                (should (equal leaf-id "m4"))
                (should (equal (plist-get
                                (piem-test--tree-find-node tree "m1")
                                :preview)
                               "fix the parser"))))))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-tree-fetch-without-process ()
  "The tree browser fetch proceeds without a live pi process.
Phase 3 reads the tree from the linked chat's session file on disk, so
the Phase 0 no-process guard is gone: real previews render with no
--get-process mock anywhere, the loading state clears, and no error is
recorded."
  (let* ((dir (piem-test--make-temp-directory "pi-tree-noproc"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-noproc-chat*")))
    (piem-test--write-session-lines
     path (piem-test--live-session-lines))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq piem--state (list :session-file path)))
          (piem-test--with-tree-link chat-buf
            (piem-test--sync-timers
              (lambda () (piem--tree-browser-fetch-and-render)))
            (should (string-match-p "fix the parser" (buffer-string)))
            (should-not piem--tree-browser-loading)
            (should-not piem--tree-browser-error)))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-set-label-appends-label-entry ()
  "Setting a label appends exactly one label entry to the session file.
The line carries a fresh 8-hex id colliding with nothing, :parentId is
the last parseable line's id, :targetId names the node, the timestamp
is ISO-ms no older than the call, and prior bytes stay byte-for-byte
intact (a missing trailing newline gains a separator first).  The
cached projected tree is patched and re-rendered in place — the file
is NOT read again — and the label shows as a margin overlay."
  (let* ((dir (piem-test--make-temp-directory "pi-label-append"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-label-append-chat*"))
         (start-iso (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t))
         (messages nil)
         (read-calls 0)
         (real-read (symbol-function 'piem-jsonl-read-file)))
    ;; No trailing newline: the append must add the separator itself.
    (piem-test--write-session-lines
     path (piem-test--live-session-lines) t)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq piem--state (list :session-file path)))
          (piem-test--with-tree-link chat-buf
            (cl-letf (((symbol-function 'piem-jsonl-read-file)
                       (lambda (p)
                         (cl-incf read-calls)
                         (funcall real-read p))))
              (piem-test--sync-timers
                (lambda () (piem--tree-browser-fetch-and-render)))
              (let ((reads-after-fetch read-calls)
                    (before (piem-test--file-contents path)))
                (cl-letf (((symbol-function 'message)
                           (lambda (fmt &rest args)
                             (push (apply #'format fmt args) messages))))
                  (piem--browse-set-label "m2" "keep this"))
                ;; Exactly one line appended after a separator.
                (let* ((after (piem-test--file-contents path))
                       (appended (car (split-string
                                       (substring after (length before))
                                       "\n" t)))
                       (entry (json-parse-string appended :object-type 'plist)))
                  (should (string-prefix-p before after))
                  (should (string-suffix-p (concat appended "\n") after))
                  (should (equal (plist-get entry :type) "label"))
                  (should (equal (plist-get entry :targetId) "m2"))
                  (should (equal (plist-get entry :label) "keep this"))
                  (should (string-match-p "\\`[0-9a-f]\\{8\\}\\'"
                                          (plist-get entry :id)))
                  (should (not (member (plist-get entry :id)
                                       '("sid-live" "m1" "m2" "m3" "m4"))))
                  ;; parentId is the last parseable line's id.
                  (should (equal (plist-get entry :parentId) "m4"))
                  (let ((ts (plist-get entry :timestamp)))
                    (should (string-match-p
                             "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\.[0-9]\\{3\\}Z\\'"
                             ts))
                    (should (not (string< ts start-iso)))))
                ;; Patched in place: no re-read of the session file.
                (should (= read-calls reads-after-fetch))
                (should (equal (plist-get
                                (piem-test--tree-find-node
                                 piem--tree-browser-tree "m2")
                                :label)
                               "keep this"))
                (should (piem-test--margin-overlay-contains-p
                         "keep this"))
                (should (cl-some (lambda (m) (string-match-p "Label set" m))
                                 messages))))))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-set-label-clear-omits-label-key ()
  "Clearing a label appends a label entry with the label key omitted.
pi's appendLabelChange shape omits :label on a clear (the load-time
fold treats absent as clear), so the raw line has no \"label\" key at
all; the cached tree loses :label and the margin overlay disappears."
  (let* ((dir (piem-test--make-temp-directory "pi-label-clear"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-label-clear-chat*"))
         (messages nil))
    (piem-test--write-session-lines
     path (append (piem-test--live-session-lines)
                  (list (piem-test--jsonl-line
                         "label" "l1" "m4" :targetId "m2" :label "old tag"))))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq piem--state (list :session-file path)))
          (piem-test--with-tree-link chat-buf
            (piem-test--sync-timers
              (lambda () (piem--tree-browser-fetch-and-render)))
            (should (piem-test--margin-overlay-contains-p "old tag"))
            (let ((before (piem-test--file-contents path)))
              (cl-letf (((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (apply #'format fmt args) messages))))
                (piem--browse-set-label "m2" nil))
              (let* ((after (piem-test--file-contents path))
                     (appended (car (split-string
                                     (substring after (length before))
                                     "\n" t)))
                     (entry (json-parse-string appended :object-type 'plist)))
                (should (string-prefix-p before after))
                (should (equal (plist-get entry :type) "label"))
                (should (equal (plist-get entry :targetId) "m2"))
                ;; The clear omits the label key entirely — the only
                ;; "label" substring left is the "type":"label" pair.
                (should-not (plist-get entry :label))
                (should-not (string-match-p "\"label\":" appended))
                ;; parentId is the last parseable line's id (the old
                ;; label line).
                (should (equal (plist-get entry :parentId) "l1"))))
            ;; The cached tree and buffer lose the label.
            (should-not (plist-get
                         (piem-test--tree-find-node
                          piem--tree-browser-tree "m2")
                         :label))
            (should-not (piem-test--margin-overlay-contains-p
                         "old tag"))
            (should (cl-some (lambda (m) (string-match-p "Label cleared" m))
                             messages))))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-label-survives-refetch ()
  "An appended label survives a refetch and stays on the same leaf.
The label folds back from disk, and although the file's last line is
now the label entry itself (the new raw leaf), the projected leaf
still resolves up to the pre-label visible leaf."
  (let* ((dir (piem-test--make-temp-directory "pi-label-refetch"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-label-refetch-chat*")))
    (piem-test--write-session-lines
     path (piem-test--live-session-lines))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq piem--state (list :session-file path)))
          (piem-test--with-tree-link chat-buf
            (piem-test--sync-timers
              (lambda () (piem--tree-browser-fetch-and-render)))
            (should (equal piem--tree-browser-leaf-id "m4"))
            (piem-test--sync-timers
              (lambda () (piem--tree-browser-fetch-and-render)))
            (should (equal piem--tree-browser-leaf-id "m4"))
            (piem--browse-set-label "m1" "checkpoint")
            ;; The local patch and a fresh disk fold agree exactly: the
            ;; patched cached tree and leaf equal a whole fresh
            ;; projection of the file (label pair in the canonical
            ;; after-:timestamp slot, clear leaving no pair at all).
            (let ((fresh (piem-jsonl-project-session-file path)))
              (should (equal piem--tree-browser-tree
                             (plist-get fresh :tree)))
              (should (equal piem--tree-browser-leaf-id
                             (plist-get fresh :leafId))))
            ;; The file's last line is now the label entry.
            (let* ((session (piem-jsonl-read-file path))
                   (raw-leaf (plist-get session :leafId)))
              (should (string-match-p "\\`[0-9a-f]\\{8\\}\\'" raw-leaf))
              (should-not (member raw-leaf '("m1" "m2" "m3" "m4"))))
            ;; Refetch: folded from disk, leaf unchanged, still shown.
            (piem-test--sync-timers
              (lambda () (piem--tree-browser-fetch-and-render)))
            (should (equal piem--tree-browser-leaf-id "m4"))
            (should (equal (plist-get
                            (piem-test--tree-find-node
                             piem--tree-browser-tree "m1")
                            :label)
                           "checkpoint"))
            (should (piem-test--margin-overlay-contains-p
                     "checkpoint"))
            (should-not piem--tree-browser-error)))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-load-tree-interrupted-by-quit ()
  "A quit during the deferred read reports an error state, not a stuck
loading render.  C-g against a huge file raises `quit' — not `error' —
inside the blocking read; the seam must still call back so the loading
state clears and the buffer names the interruption."
  (let* ((dir (piem-test--make-temp-directory "pi-tree-quit"))
         (path (expand-file-name "session.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-quit-chat*")))
    (piem-test--write-session-lines
     path (piem-test--live-session-lines))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq piem--state (list :session-file path)))
          (piem-test--with-tree-link chat-buf
            (cl-letf (((symbol-function 'piem-jsonl-project-session-file)
                       (lambda (_path) (signal 'quit nil))))
              (piem-test--sync-timers
                (lambda () (piem--tree-browser-fetch-and-render))))
            (should-not piem--tree-browser-loading)
            (should (string-match-p "interrupted"
                                    piem--tree-browser-error))
            (should (string-match-p "interrupted" (buffer-string)))
            (should (= piem--tree-browser-visible-count 0))))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-load-tree-mid-read-session-switch ()
  "A chat session switch between fetch start and the deferred read
leaves `--tree-browser-loaded-file' pinned to the file the fetch
actually read.  Resolving the link again at callback time would arm
the labeler against the NEW session while the browser still shows the
OLD tree, appending a node id from one file into the other; instead
labeling must refuse with the refresh message and touch nothing."
  (let* ((dir (piem-test--make-temp-directory "pi-tree-midsw"))
         (path-a (expand-file-name "a.jsonl" dir))
         (path-b (expand-file-name "b.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-tree-midsw-chat*"))
         (messages nil))
    (piem-test--write-session-lines
     path-a (piem-test--live-session-lines))
    (piem-test--write-session-lines
     path-b (piem-test--live-session-lines))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq piem--state (list :session-file path-a)))
          (piem-test--with-tree-link chat-buf
            (cl-letf* ((real (symbol-function 'piem-jsonl-project-session-file))
                       ((symbol-function 'piem-jsonl-project-session-file)
                        (lambda (p)
                          ;; The chat switches sessions while the
                          ;; deferred read runs; the read itself
                          ;; still returns path-a's projection.
                          (with-current-buffer chat-buf
                            (setq piem--state
                                  (list :session-file path-b)))
                          (funcall real p))))
              (piem-test--sync-timers
                (lambda () (piem--tree-browser-fetch-and-render))))
            (should (equal piem--tree-browser-loaded-file
                           path-a))
            ;; Fresh resolution now disagrees: labeling refuses.
            (let ((before-b (piem-test--file-contents path-b)))
              (cl-letf (((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (apply #'format fmt args) messages))))
                (piem--browse-set-label "m1" "late"))
              (should (member
                       "Pi: Session changed since the tree was loaded — refresh with g"
                       messages))
              (should (equal (piem-test--file-contents path-b)
                             before-b)))))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-tree-fetch-paints-loading-state ()
  "The loading render is painted before the deferred read is scheduled.
Emacs runs due 0-timers before redisplaying, so a single timer hop to
the read starves the loading paint entirely (verified mechanically:
mid-read the terminal still shows the previous buffer contents).  The
fetch must force a `redisplay' between the loading render and the
seam call."
  (with-temp-buffer
    (piem-tree-browser-mode)
    (let ((log nil))
      (cl-letf (((symbol-function 'redisplay)
                 (lambda (&rest _) (push :redisplay log)))
                ((symbol-function 'piem--browse-load-tree)
                 (lambda (_callback) (push :load log))))
        (piem--tree-browser-fetch-and-render))
      ;; Strict order: the paint lands before the seam (and thus
      ;; before any deferred read) is even scheduled.
      (should (equal log '(:load :redisplay))))))

(ert-deftest piem-test-set-label-rejects-stale-session ()
  "Labeling refuses when the chat moved to another session file.
The loaded-file guard compares against a fresh resolution of the chat
link: a mismatch messages instead of appending, and neither the loaded
nor the current file changes."
  (let* ((dir (piem-test--make-temp-directory "pi-label-stale"))
         (path-a (expand-file-name "a.jsonl" dir))
         (path-b (expand-file-name "b.jsonl" dir))
         (chat-buf (generate-new-buffer " *test-label-stale-chat*"))
         (messages nil))
    (piem-test--write-session-lines
     path-a (piem-test--live-session-lines))
    (piem-test--write-session-lines
     path-b (piem-test--live-session-lines))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq piem--state (list :session-file path-a)))
          (piem-test--with-tree-link chat-buf
            (piem-test--sync-timers
              (lambda () (piem--tree-browser-fetch-and-render)))
            (should (equal piem--tree-browser-loaded-file path-a))
            ;; The chat switches sessions behind the browser's back.
            (with-current-buffer chat-buf
              (setq piem--state (list :session-file path-b)))
            (let ((before-a (piem-test--file-contents path-a))
                  (before-b (piem-test--file-contents path-b)))
              (cl-letf (((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (apply #'format fmt args) messages))))
                (piem--browse-set-label "m1" "late"))
              (should (member
                       "Pi: Session changed since the tree was loaded — refresh with g"
                       messages))
              (should (equal (piem-test--file-contents path-a)
                             before-a))
              (should (equal (piem-test--file-contents path-b)
                             before-b)))))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-set-label-no-session-file ()
  "Labeling with no resolvable session file reports and writes nothing.
Both a dead chat link and a live link whose state has no :session-file
message \"Pi: Cannot label: no session file\"; no file is created or
appended anywhere."
  (let* ((dir (piem-test--make-temp-directory "pi-label-nofile"))
         (chat-buf (generate-new-buffer " *test-label-nofile-chat*"))
         (messages nil))
    (unwind-protect
        (progn
          ;; Live link, state without a session file.
          (with-current-buffer chat-buf
            (setq piem--state (list :messageCount 3)))
          (piem-test--with-tree-link chat-buf
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages))))
              (piem--browse-set-label "m1" "nowhere")
              ;; No chat link at all.
              (setq piem--chat-buffer nil)
              (piem--browse-set-label "m1" "nowhere")
              (should (equal (cl-count-if
                              (lambda (m)
                                (string-match-p
                                 "\\`Pi: Cannot label: no session file\\'" m))
                              messages)
                             2))))
          ;; Nothing was written anywhere in the scratch directory.
          (should-not (directory-files dir nil "\\.jsonl\\'")))
      (kill-buffer chat-buf))))

;;;; Phase 4: Tree Navigation

(defun piem-test--navigable-session-lines ()
  "Return raw session lines with a branch point for navigation tests.
u1 (root user) has two assistant children — b1, an abandoned sibling,
and a1, the active branch — whose user child u2 is the raw leaf.
Navigating to u2 must rewind the leaf to a1: the header stays first,
the off-chain b1 and u2 keep their relative order ahead of the root
chain u1, a1, and a1 becomes the new last line."
  (list (piem-test--make-session-header "sid-nav")
        (piem-test--user-line "u1" nil "fix the parser")
        (piem-test--jsonl-line
         "message" "b1" "u1"
         :message '(:role "assistant" :content "abandoned branch"
                    :stopReason "end_turn"))
        (piem-test--jsonl-line
         "message" "a1" "u1"
         :message '(:role "assistant" :content "checking"
                    :stopReason "end_turn"))
        (piem-test--user-line "u2" "a1" "try the other way")))

(defun piem-test--navigate-rewritten-contents
    (lines &optional separator)
  "Return expected bytes after navigating LINES to u2.
LINES start (header, u1, b1, a1, u2); any remaining malformed lines
are off-chain.  The stable partition is header, b1, u2, malformed…,
u1, a1.  SEPARATOR defaults to LF and is also appended once at end."
  (let ((separator (string-as-unibyte (or separator "\n"))))
    (concat (mapconcat #'string-as-unibyte
                       (append (list (nth 0 lines) (nth 2 lines)
                                     (nth 4 lines))
                               (nthcdr 5 lines)
                               (list (nth 1 lines) (nth 3 lines)))
                       separator)
            separator)))

(defmacro piem-test--with-navigate-fixture
    (lines path chat-buf input-buf proc messages resume-calls quit-calls
     ready-calls &rest body)
  "Run BODY inside a tree browser wired for a `--browse-navigate' call.
LINES is an expression yielding the raw session lines written to a
fresh PATH in a temp directory.  CHAT-BUF's state names PATH, its
process is a live PROC, and its linked INPUT-BUF starts with a stale
draft.  The tree browser fetches synchronously first, so
`--tree-browser-loaded-file' is PATH.  message,
`--resume-selected-session', `--browse-quit-when-settled', the resume
cwd pre-flight (`--session-file-cwd-or-error', satisfied with the
session directory), and `--session-transition-ready-p' (which answers
ready) are spied into MESSAGES, RESUME-CALLS, QUIT-CALLS, and
READY-CALLS.  BODY runs inside the `cl-letf*', so a nested `cl-letf'
overrides any spy."
  (declare (indent 9))
  (let ((dir (gensym "nav-dir")))
    `(let* ((,dir (piem-test--make-temp-directory "pi-nav"))
            (,path (expand-file-name "session.jsonl" ,dir))
            (,proc (start-process "pi-nav-test" nil "sleep" "30"))
            (,chat-buf (generate-new-buffer " *test-nav-chat*"))
            (,input-buf (generate-new-buffer " *test-nav-input*")))
       (unwind-protect
           (progn
             (piem-test--write-session-lines ,path ,lines)
             (with-current-buffer ,chat-buf
               (setq piem--state (list :session-file ,path)
                     piem--process ,proc
                     piem--input-buffer ,input-buf))
             (with-current-buffer ,input-buf
               (insert "stale draft"))
             (with-temp-buffer
               (piem-tree-browser-mode)
               (setq piem--chat-buffer ,chat-buf)
               (piem-test--sync-timers
                 (lambda ()
                   (piem--tree-browser-fetch-and-render)))
               (let ((,messages nil)
                     (,resume-calls nil)
                     (,quit-calls nil)
                     (,ready-calls nil))
                 (cl-letf* (((symbol-function 'message)
                             (lambda (fmt &rest args)
                               (push (apply #'format fmt args) ,messages)))
                            ((symbol-function
                              'piem--resume-selected-session)
                             (lambda (&rest args) (push args ,resume-calls)))
                            ((symbol-function
                              'piem--browse-quit-when-settled)
                             (lambda (&rest args) (push args ,quit-calls)))
                            ((symbol-function
                              'piem--session-file-cwd-or-error)
                             (lambda (&rest _) ,dir))
                            ((symbol-function
                              'piem--session-transition-ready-p)
                             (lambda (&rest args)
                               (push args ,ready-calls)
                               t)))
                   ,@body))))
         (delete-process ,proc)
         (piem-test--kill-live-buffers ,chat-buf ,input-buf)))))

(ert-deftest piem-test-navigate-guards ()
  "Navigate refuses, in order, before touching anything: no linked
chat (`user-error'), no resolvable session file, a stale loaded file
(the chat switched sessions behind the browser), a dead process
(`user-error'), an active session transition, and a not-ready chat.
Every refusal leaves the session files byte-identical and the resume
flow uncalled."
  ;; No linked chat: user-error before any other guard.
  (with-temp-buffer
    (piem-tree-browser-mode)
    (setq piem--chat-buffer nil)
    (should (equal (error-message-string
                    (should-error
                     (piem--browse-navigate "u2")
                     :type 'user-error))
                   "No pi session to navigate")))
  ;; No session file: message, nothing written.
  (let* ((chat-buf (generate-new-buffer " *test-nav-nofile-chat*"))
         (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq piem--state (list :messageCount 3)))
          (piem-test--with-tree-link chat-buf
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages))))
              (piem--browse-navigate "u2"))
            (should (member "Pi: Cannot navigate: no session file"
                            messages))))
      (kill-buffer chat-buf)))
  ;; Stale loaded file: the chat moved to another session.
  (piem-test--with-navigate-fixture
      (piem-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let* ((other (expand-file-name
                   "other.jsonl" (file-name-directory path)))
           (before (piem-test--file-contents path)))
      (piem-test--write-session-lines
       other (piem-test--navigable-session-lines))
      (let ((other-before (piem-test--file-contents other)))
        (with-current-buffer chat-buf
          (setq piem--state (list :session-file other)))
        (piem--browse-navigate "u2")
        (should (member
                 "Pi: Session changed since the tree was loaded — refresh with g"
                 messages))
        (should (equal (piem-test--file-contents path) before))
        (should (equal (piem-test--file-contents other)
                       other-before))
        (should-not resume-calls))))
  ;; Dead process: user-error, before the transition guards.
  (piem-test--with-navigate-fixture
      (piem-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (piem-test--file-contents path)))
      (with-current-buffer chat-buf
        (setq piem--process nil))
      (should (equal (error-message-string
                      (should-error
                       (piem--browse-navigate "u2")
                       :type 'user-error))
                     "Pi process is not running"))
      (should (equal (piem-test--file-contents path) before))
      (should-not resume-calls)
      (should-not ready-calls)))
  ;; Active transition: message, and the ready guard never runs.
  (piem-test--with-navigate-fixture
      (piem-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (piem-test--file-contents path)))
      (with-current-buffer chat-buf
        (setq piem--session-transition-active t))
      (piem--browse-navigate "u2")
      (should (member "Pi: Cannot navigate while switching sessions"
                      messages))
      (should (equal (piem-test--file-contents path) before))
      (should-not resume-calls)
      (should-not ready-calls)))
  ;; Not ready: the guard reports its own refusal and navigate
  ;; returns quietly, passing the "navigate" action.
  (piem-test--with-navigate-fixture
      (piem-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (piem-test--file-contents path)))
      (cl-letf (((symbol-function
                  'piem--session-transition-ready-p)
                 (lambda (chat-buf action)
                   (push (list chat-buf action) ready-calls)
                   nil)))
        (piem--browse-navigate "u2"))
      (should (equal ready-calls (list (list chat-buf "navigate"))))
      (should (equal (piem-test--file-contents path) before))
      (should-not resume-calls))))

(ert-deftest piem-test-navigate-old-format ()
  "A version-1 session file (no header :version, no entry ids) reads
fine but refuses navigation with the migrate hint — the version guard
fires before any node lookup.  The file is untouched and no switch is
scheduled."
  (let ((old-lines
         (list (json-encode
                (list :type "session"
                      :id "sid-old"
                      :timestamp piem-test--browse-timestamp
                      :cwd "/home/fake/a"))
               (json-encode
                (list :type "message"
                      :timestamp piem-test--browse-timestamp
                      :message '(:role "user" :content "v1 prompt"))))))
    (piem-test--with-navigate-fixture
        old-lines path chat-buf input-buf proc messages resume-calls
        quit-calls ready-calls
      (let ((before (piem-test--file-contents path)))
        (piem--browse-navigate "u1")
        (should (member
                 "Pi: Session file uses an old format; open it with pi once to migrate, then refresh with g"
                 messages))
        (should (equal (piem-test--file-contents path) before))
        (should-not resume-calls)))))

(ert-deftest piem-test-navigate-unknown-node ()
  "A node id the session file does not carry refuses with the refresh
hint; the file is untouched and no switch is scheduled."
  (piem-test--with-navigate-fixture
      (piem-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (piem-test--file-contents path)))
      (piem--browse-navigate "deadbeef")
      (should (member "Pi: No such tree node — refresh with g" messages))
      (should (equal (piem-test--file-contents path) before))
      (should-not resume-calls))))

(ert-deftest piem-test-navigate-already-at-position ()
  "Targeting the current position (no prefill to restore) just says
so: no write, no switch, no settle-wait.  The raw leaf is a trailing
label child of a1, which resolves up to a1 — the target itself."
  (piem-test--with-navigate-fixture
      (list (piem-test--make-session-header "sid-here")
            (piem-test--user-line "u1" nil "fix the parser")
            (piem-test--jsonl-line
             "message" "a1" "u1"
             :message '(:role "assistant" :content "checking"
                        :stopReason "end_turn"))
            (piem-test--jsonl-line
             "label" "l1" "a1" :targetId "u1" :label "checkpoint"))
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (piem-test--file-contents path)))
      (piem--browse-navigate "a1")
      (should (member "Pi: Already at current position" messages))
      (should (equal (piem-test--file-contents path) before))
      (should-not resume-calls)
      (should-not quit-calls)
      ;; The stale draft survives: there is nothing to re-edit.
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "stale draft")))))

(ert-deftest piem-test-navigate-prefill-only ()
  "Re-editing a prompt the file already sits on (a previous navigate
put its parent last) prefills the input buffer and waits out the
settle — but writes nothing and switches nothing."
  (piem-test--with-navigate-fixture
      (list (piem-test--make-session-header "sid-again")
            (piem-test--user-line "u2" "u1" "try the other way")
            (piem-test--user-line "u1" nil "fix the parser"))
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (piem-test--file-contents path)))
      (piem--browse-navigate "u2")
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "try the other way"))
      (should (member "Pi: Navigated to try the other way" messages))
      (should (equal quit-calls
                     (list (list chat-buf (selected-window) path))))
      (should (equal (piem-test--file-contents path) before))
      (should-not resume-calls))))

(ert-deftest piem-test-navigate-rewrites-and-switches ()
  "The full navigate atomically moves the chain last and switches.
The fresh disk input is adversarial CRLF JSONL with a structurally
malformed line containing byte FF.  Every original line and byte,
including every CR, survives the stable partition exactly; a final
CRLF remains.  The resume flow gets (PROC CHAT-BUF PATH), input is
prefilled, success is messaged, settle-wait is scheduled, and no temp
file remains."
  (piem-test--with-navigate-fixture
      (piem-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let* ((malformed
            (concat (string-as-unibyte
                     "{\"type\":\"message\",\"id\":\"torn\",\"payload\":\"")
                    (unibyte-string #xff)))
           (lines (append (piem-test--navigable-session-lines)
                          (list malformed)))
           (original (concat (mapconcat #'string-as-unibyte lines
                                         (string-as-unibyte "\r\n"))
                             (string-as-unibyte "\r\n")))
           (expected (piem-test--navigate-rewritten-contents
                      lines "\r\n")))
      ;; Replace only the temp fixture, never real session data.  The
      ;; browser cache is already loaded; navigate must use this fresh
      ;; on-disk CRLF shape for both target and line-order reads.
      (let ((coding-system-for-write 'no-conversion))
        (write-region original nil path nil 0))
      (piem--browse-navigate "u2")
      (should (equal (piem-test--file-contents path t) expected))
      (should (equal resume-calls (list (list proc chat-buf path))))
      (should (equal quit-calls
                     (list (list chat-buf (selected-window) path))))
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "try the other way"))
      (should (member "Pi: Navigated to try the other way" messages))
      (should-not (directory-files (file-name-directory path)
                                   nil "\\.pi-nav-")))))

(ert-deftest piem-test-navigate-shape ()
  "The navigated file reads back in the navigated shape: read-file's
:leafId is the computed leaf (a1), and the projection's active path
from that leaf holds exactly the expected visible ids — u1 and a1,
not the off-branch b1 nor the rewound-under u2."
  (piem-test--with-navigate-fixture
      (piem-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (piem--browse-navigate "u2")
    (let* ((session (piem-jsonl-read-file path))
           (result (piem-jsonl-project-session-file path))
           (active (piem--active-path-ids
                    (plist-get result :tree) (plist-get result :leafId))))
      (should (equal (plist-get session :leafId) "a1"))
      (should (equal (plist-get result :leafId) "a1"))
      (should (gethash "u1" active))
      (should (gethash "a1" active))
      (should-not (gethash "u2" active))
      (should-not (gethash "b1" active)))))

(ert-deftest piem-test-navigate-atomic-failure ()
  "Errors and quits before commit leave the original byte-identical.
For both write-region and rename-file legs, `unwind-protect' removes
any .pi-nav- temp, the switch and prefill do not run, errors report a
navigate failure, and quits propagate.  The quitting write first
creates a partial temp file, proving cleanup rather than non-creation."
  (piem-test--with-navigate-fixture
      (piem-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (piem-test--file-contents path))
          (dir (file-name-directory path)))
      ;; write-region failure: the temp file never lands.
      (cl-letf (((symbol-function 'write-region)
                 (lambda (&rest _)
                   (signal 'file-error '("write failed")))))
        (piem--browse-navigate "u2"))
      (should (equal (piem-test--file-contents path) before))
      (should-not (directory-files dir nil "\\.pi-nav-"))
      (should-not resume-calls)
      (should (cl-some (lambda (m) (string-match-p "\\`Pi: Navigate failed: " m))
                       messages))
      ;; rename-file failure: the temp file is removed again, never
      ;; swapped in.
      (setq messages nil)
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (&rest _)
                   (signal 'file-error '("rename failed")))))
        (piem--browse-navigate "u2"))
      (should (equal (piem-test--file-contents path) before))
      (should-not (directory-files dir nil "\\.pi-nav-"))
      (should-not resume-calls)
      (should (cl-some (lambda (m) (string-match-p "\\`Pi: Navigate failed: " m))
                       messages))
      ;; A quit after a partial temp write is not an `error', so it
      ;; propagates; the unwind still removes the file.
      (let ((real-write (symbol-function 'write-region)))
        (cl-letf (((symbol-function 'write-region)
                   (lambda (_start _end filename &rest _)
                     (funcall real-write "partial" nil filename nil 0)
                     (signal 'quit nil))))
          (should (eq (condition-case nil
                          (progn
                            (piem--browse-navigate "u2")
                            'returned)
                        (quit 'quit))
                      'quit))))
      (should (equal (piem-test--file-contents path) before))
      (should-not (directory-files dir nil "\\.pi-nav-"))
      (should-not resume-calls)
      ;; The rename leg also owns a completed temp file when quit
      ;; arrives; its unwind removes that file and leaves PATH alone.
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (&rest _) (signal 'quit nil))))
        (should (eq (condition-case nil
                        (progn
                          (piem--browse-navigate "u2")
                          'returned)
                      (quit 'quit))
                    'quit)))
      (should (equal (piem-test--file-contents path) before))
      (should-not (directory-files dir nil "\\.pi-nav-"))
      (should-not resume-calls)
      ;; No failed or interrupted attempt touched the input buffer.
      (should (equal (with-current-buffer input-buf (buffer-string))
                     "stale draft")))))

(ert-deftest piem-test-navigate-preflight-cwd ()
  "A resume cwd failure re-signals its `user-error' BEFORE any write:
the file is untouched, no temp file appears, and no switch runs."
  (piem-test--with-navigate-fixture
      (piem-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (piem-test--file-contents path)))
      (cl-letf (((symbol-function
                  'piem--session-file-cwd-or-error)
                 (lambda (&rest _)
                   (user-error
                    "Stored session cwd is not an existing directory"))))
        (should (equal (error-message-string
                        (should-error
                         (piem--browse-navigate "u2")
                         :type 'user-error))
                       "Stored session cwd is not an existing directory")))
      (should (equal (piem-test--file-contents path) before))
      (should-not (directory-files (file-name-directory path)
                                   nil "\\.pi-nav-"))
      (should-not resume-calls))))

(ert-deftest piem-test-navigate-root-user-message ()
  "Navigating to the root user message has no parent to rewind to:
the fork hint fires, nothing is written, no switch runs."
  (piem-test--with-navigate-fixture
      (piem-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (let ((before (piem-test--file-contents path)))
      (piem--browse-navigate "u1")
      (should (member
               "Pi: Cannot rewind before the first message; fork it from the chat instead"
               messages))
      (should (equal (piem-test--file-contents path) before))
      (should-not resume-calls))))

(ert-deftest piem-test-navigate-preserves-permissions ()
  "The rewrite carries the original file's modes onto the replacement
(best effort): a 0600 session is still 0600 after navigating, and the
full flow ran (a switch was scheduled onto the rewritten file)."
  (skip-unless (not (zerop (user-uid))))
  (piem-test--with-navigate-fixture
      (piem-test--navigable-session-lines)
      path chat-buf input-buf proc messages resume-calls quit-calls
      ready-calls
    (set-file-modes path #o600)
    (piem--browse-navigate "u2")
    (should resume-calls)
    (should (equal (file-modes path) #o600))))

(ert-deftest piem-test-quit-when-settled-tree-window ()
  "--browse-poll-settled also dismisses TREE browser windows: the
window check accepts any pi browse buffer via `derived-mode-p', not
just the session browser (V14)."
  (let* ((chat-buf (generate-new-buffer " *test-settled-tree-chat*"))
         (win (selected-window))
         (orig-buf (window-buffer win))
         (browser-buf (generate-new-buffer " *test-settled-tree*"))
         (path "/tmp/target-session.jsonl")
         (quit-calls nil)
         (polls 0))
    (unwind-protect
        (progn
          (with-current-buffer browser-buf
            (piem-tree-browser-mode))
          (set-window-buffer win browser-buf)
          (with-current-buffer chat-buf
            (setq piem--state (list :session-file path)))
          ;; Settled onto the target after one busy poll: the window
          ;; shows a TREE browser and must be quit.
          (cl-letf (((symbol-function
                      'piem--session-transition-active-p)
                     (lambda (&optional _chat-buf)
                       (setq polls (1+ polls))
                       (<= polls 1)))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args) (apply fn args)))
                    ((symbol-function 'quit-window)
                     (lambda (&rest args) (push args quit-calls))))
            (piem--browse-quit-when-settled chat-buf win path))
          (should (>= polls 2))
          (should (equal quit-calls (list (list nil win)))))
      (set-window-buffer win orig-buf)
      (kill-buffer browser-buf)
      (when (buffer-live-p chat-buf) (kill-buffer chat-buf)))))

(provide 'piem-browse-test)
;;; piem-browse-test.el ends here
