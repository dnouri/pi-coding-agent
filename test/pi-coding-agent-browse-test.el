;;; pi-coding-agent-browse-test.el --- Tests for browsing module -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for pi-coding-agent-browse.el — session and tree browser
;; helper functions and response parsing.

;;; Code:

(require 'ert)
(require 'pi-coding-agent-browse)
(require 'pi-coding-agent-test-common)

;;;; Test Fixtures

(defvar pi-coding-agent-test--fixture-dir
  (expand-file-name "test/fixtures/"
                    (or (and load-file-name
                             (file-name-directory
                              (directory-file-name
                               (file-name-directory load-file-name))))
                        (locate-dominating-file default-directory "Makefile")
                        default-directory))
  "Directory containing JSON test fixtures.")

(defun pi-coding-agent-test--read-json-fixture (filename)
  "Read JSON fixture FILENAME from test/fixtures/ and return as plist."
  (let ((path (expand-file-name filename pi-coding-agent-test--fixture-dir)))
    (with-temp-buffer
      (insert-file-contents path)
      (json-parse-string (buffer-string) :object-type 'plist))))

;;;; Session Parsing

(ert-deftest pi-coding-agent-test-parse-session-list ()
  "Parse list_sessions response into session items."
  (let* ((response (pi-coding-agent-test--read-json-fixture "browse-sessions.json"))
         (items (pi-coding-agent--parse-session-list response)))
    (should (= (length items) 5))
    ;; First item fields
    (let ((first (car items)))
      (should (equal (plist-get first :id) "aaa-111"))
      (should (equal (plist-get first :path)
                     "/home/user/.pi/agent/sessions/--home-user-co-project--/2026-02-24_aaa.jsonl"))
      (should (= (plist-get first :messageCount) 42))
      (should (stringp (plist-get first :firstMessage))))))

(ert-deftest pi-coding-agent-test-parse-session-list-error ()
  "Return nil for failed list_sessions response."
  (let ((response '(:type "response" :command "list_sessions"
                    :success :false :error "timeout")))
    (should (null (pi-coding-agent--parse-session-list response)))))

(ert-deftest pi-coding-agent-test-parse-session-list-empty ()
  "Return empty list for response with no sessions."
  (let ((response '(:type "response" :command "list_sessions"
                    :success t :data (:sessions []))))
    (should (equal (pi-coding-agent--parse-session-list response) nil))))

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

;;;; Navigate Parsing

(ert-deftest pi-coding-agent-test-parse-navigate-result ()
  "Parse navigate_tree response."
  (let* ((response (pi-coding-agent-test--read-json-fixture "browse-navigate.json"))
         (result (pi-coding-agent--parse-navigate-result response)))
    (should result)
    ;; JSON false parses as :false — use normalize-boolean
    (should-not (pi-coding-agent--normalize-boolean (plist-get result :cancelled)))
    (should (equal (plist-get result :editorText)
                   "Actually, let's try a different approach"))
    (let ((summary (plist-get result :summaryEntry)))
      (should summary)
      (should (equal (plist-get summary :id) "summary-1"))
      (should (stringp (plist-get summary :summary))))))

(ert-deftest pi-coding-agent-test-parse-navigate-cancelled ()
  "Parse cancelled navigate_tree response."
  (let ((response '(:type "response" :command "navigate_tree"
                    :success t :data (:cancelled t))))
    (let ((result (pi-coding-agent--parse-navigate-result response)))
      (should result)
      (should (eq (plist-get result :cancelled) t)))))

(ert-deftest pi-coding-agent-test-parse-navigate-error ()
  "Return nil for failed navigate_tree response."
  (let ((response '(:type "response" :command "navigate_tree"
                    :success :false :error "bad target")))
    (should (null (pi-coding-agent--parse-navigate-result response)))))

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
  "Apply-margins reads width from buffer-local variable."
  (with-temp-buffer
    (setq pi-coding-agent--browse-margin-width 20)
    ;; Should not error; the function checks window-live-p
    (pi-coding-agent--browse-apply-margins)
    ;; Verify the variable was set correctly
    (should (= pi-coding-agent--browse-margin-width 20))))

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

;;;; RPC Command Builders

(ert-deftest pi-coding-agent-test-build-list-sessions-command ()
  "Build list_sessions RPC command plist."
  (let ((cmd (pi-coding-agent--build-list-sessions-command "current")))
    (should (equal (plist-get cmd :type) "list_sessions"))
    (should (equal (plist-get cmd :scope) "current")))
  (let ((cmd (pi-coding-agent--build-list-sessions-command "all")))
    (should (equal (plist-get cmd :scope) "all"))))

(ert-deftest pi-coding-agent-test-build-get-tree-command ()
  "Build get_tree RPC command plist."
  (let ((cmd (pi-coding-agent--build-get-tree-command)))
    (should (equal (plist-get cmd :type) "get_tree"))))

(ert-deftest pi-coding-agent-test-build-navigate-tree-command ()
  "Build navigate_tree RPC command plist."
  ;; Without summarize
  (let ((cmd (pi-coding-agent--build-navigate-tree-command "node-4" nil nil)))
    (should (equal (plist-get cmd :type) "navigate_tree"))
    (should (equal (plist-get cmd :targetId) "node-4"))
    (should-not (plist-get cmd :summarize)))
  ;; With summarize
  (let ((cmd (pi-coding-agent--build-navigate-tree-command "node-4" t nil)))
    (should (eq (plist-get cmd :summarize) t)))
  ;; With custom instructions
  (let ((cmd (pi-coding-agent--build-navigate-tree-command "node-4" t "Focus on tests")))
    (should (eq (plist-get cmd :summarize) t))
    (should (equal (plist-get cmd :customInstructions) "Focus on tests"))))

(ert-deftest pi-coding-agent-test-build-set-label-command ()
  "Build set_label RPC command plist."
  ;; Set label
  (let ((cmd (pi-coding-agent--build-set-label-command "node-7" "checkpoint")))
    (should (equal (plist-get cmd :type) "set_label"))
    (should (equal (plist-get cmd :entryId) "node-7"))
    (should (equal (plist-get cmd :label) "checkpoint")))
  ;; Clear label (nil)
  (let ((cmd (pi-coding-agent--build-set-label-command "node-7" nil)))
    (should (equal (plist-get cmd :entryId) "node-7"))
    (should-not (plist-get cmd :label))))

(ert-deftest pi-coding-agent-test-build-abort-branch-summary-command ()
  "Build abort_branch_summary RPC command plist."
  (let ((cmd (pi-coding-agent--build-abort-branch-summary-command)))
    (should (equal (plist-get cmd :type) "abort_branch_summary"))))

(ert-deftest pi-coding-agent-test-abort-summarization-sends-rpc ()
  "Aborting an in-flight summarization sends abort_branch_summary."
  (let* ((sent-commands nil)
         (fake-proc 'fake-process)
         (buf (generate-new-buffer " *test-tree*")))
    (unwind-protect
        (with-current-buffer buf
          (pi-coding-agent-tree-browser-mode)
          (setq pi-coding-agent--tree-browser-summarizing t)
          (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                     (lambda () fake-proc))
                    ((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc cmd _cb)
                       (push (plist-get cmd :type) sent-commands))))
            (pi-coding-agent-tree-browser-abort-summarization)
            (should (member "abort_branch_summary" sent-commands))
            (should-not pi-coding-agent--tree-browser-summarizing)))
      (kill-buffer buf))))

(ert-deftest pi-coding-agent-test-abort-summarization-noop-when-idle ()
  "Aborting when no summarization is in progress does nothing."
  (let* ((sent-commands nil)
         (buf (generate-new-buffer " *test-tree*")))
    (unwind-protect
        (with-current-buffer buf
          (pi-coding-agent-tree-browser-mode)
          (should-not pi-coding-agent--tree-browser-summarizing)
          (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc cmd _cb)
                       (push (plist-get cmd :type) sent-commands))))
            (pi-coding-agent-tree-browser-abort-summarization)
            (should-not sent-commands)))
      (kill-buffer buf))))

(ert-deftest pi-coding-agent-test-navigate-tree-sets-summarizing-flag ()
  "Navigate with summarize sets the summarizing flag, callback clears it."
  (let* ((captured-callback nil)
         (fake-proc 'fake-process)
         (tree-buf (generate-new-buffer " *test-tree*"))
         (chat-buf (generate-new-buffer " *test-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer tree-buf
            (pi-coding-agent-tree-browser-mode))
          (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc _cmd cb)
                       (setq captured-callback cb))))
            ;; Send navigate with summarize=t
            (pi-coding-agent--navigate-tree-async
             fake-proc "node-1" t nil chat-buf tree-buf)
            ;; Flag should be set before callback fires
            (with-current-buffer tree-buf
              (should pi-coding-agent--tree-browser-summarizing))
            ;; Simulate aborted response
            (funcall captured-callback
                     '(:type "response" :command "navigate_tree"
                       :success t :data (:cancelled t :aborted t)))
            ;; Flag should be cleared after callback
            (with-current-buffer tree-buf
              (should-not pi-coding-agent--tree-browser-summarizing))))
      (kill-buffer tree-buf)
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-navigate-tree-no-flag-without-summarize ()
  "Navigate without summarize does not set the summarizing flag."
  (let* ((fake-proc 'fake-process)
         (tree-buf (generate-new-buffer " *test-tree*"))
         (chat-buf (generate-new-buffer " *test-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer tree-buf
            (pi-coding-agent-tree-browser-mode))
          (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc _cmd _cb) nil)))
            (pi-coding-agent--navigate-tree-async
             fake-proc "node-1" nil nil chat-buf tree-buf)
            (with-current-buffer tree-buf
              (should-not pi-coding-agent--tree-browser-summarizing))))
      (kill-buffer tree-buf)
      (kill-buffer chat-buf))))

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
  (let* ((response (pi-coding-agent-test--read-json-fixture "browse-sessions.json"))
         (items (pi-coding-agent--parse-session-list response))
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
  (let* ((response (pi-coding-agent-test--read-json-fixture "browse-sessions.json"))
         (items (pi-coding-agent--parse-session-list response))
         (named (pi-coding-agent--session-filter-named items)))
    ;; Only bbb-222 and ddd-444 have names
    (should (= (length named) 2))
    (should (cl-every (lambda (item)
                        (plist-get item :name))
                      named))))

(ert-deftest pi-coding-agent-test-session-filter-search ()
  "Search filter matches against name and first message."
  (let* ((response (pi-coding-agent-test--read-json-fixture "browse-sessions.json"))
         (items (pi-coding-agent--parse-session-list response)))
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

;;;; RPC Error Handling

(ert-deftest pi-coding-agent-test-session-browser-rpc-error ()
  "Session browser shows error when RPC fails."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    (setq pi-coding-agent--session-browser-error
          "list_sessions not supported by this pi version")
    (pi-coding-agent--session-browser-rerender)
    (should (string-match-p "Error:" (buffer-string)))
    (should (string-match-p "not supported" (buffer-string)))))

(ert-deftest pi-coding-agent-test-session-browser-rpc-error-cleared-on-success ()
  "Error is cleared when a subsequent fetch succeeds."
  (with-temp-buffer
    (pi-coding-agent-session-browser-mode)
    ;; Simulate error state
    (setq pi-coding-agent--session-browser-error "some error")
    ;; Simulate successful callback
    (let ((response '(:success t :data (:sessions []))))
      (let ((success (eq (plist-get response :success) t)))
        (setq pi-coding-agent--session-browser-error
              (unless success "should not appear")
              pi-coding-agent--session-browser-items nil)))
    (pi-coding-agent--session-browser-rerender)
    (should-not (string-match-p "Error:" (buffer-string)))))

;;;; Summarize-and-Navigate

(ert-deftest pi-coding-agent-test-summarize-navigate-default-instructions ()
  "S with empty input navigates with summarize=true, no custom instructions."
  (let ((sent-cmd nil))
    (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
               (lambda (_proc cmd callback)
                 (setq sent-cmd cmd)
                 (funcall callback
                          '(:success t
                            :data (:cancelled :false)))))
              ((symbol-function 'pi-coding-agent--handle-navigate-success)
               #'ignore)
              ((symbol-function 'read-string)
               (lambda (_prompt) "")))
      (pi-coding-agent--tree-summarize-and-navigate
       'fake-proc "node-1" (current-buffer) (current-buffer))
      (should (equal (plist-get sent-cmd :summarize) t))
      (should (null (plist-get sent-cmd :customInstructions))))))

(ert-deftest pi-coding-agent-test-summarize-navigate-custom-instructions ()
  "S with custom text passes instructions to RPC."
  (let ((sent-cmd nil))
    (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
               (lambda (_proc cmd callback)
                 (setq sent-cmd cmd)
                 (funcall callback
                          '(:success t
                            :data (:cancelled :false)))))
              ((symbol-function 'pi-coding-agent--handle-navigate-success)
               #'ignore)
              ((symbol-function 'read-string)
               (lambda (_prompt) "Focus on key decisions")))
      (pi-coding-agent--tree-summarize-and-navigate
       'fake-proc "node-1" (current-buffer) (current-buffer))
      (should (equal (plist-get sent-cmd :summarize) t))
      (should (equal (plist-get sent-cmd :customInstructions)
                     "Focus on key decisions")))))

(ert-deftest pi-coding-agent-test-summarize-navigate-quit-cancels ()
  "C-g at the read-string prompt cancels without sending RPC."
  (let ((rpc-called nil))
    (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
               (lambda (_proc _cmd _callback)
                 (setq rpc-called t)))
              ((symbol-function 'read-string)
               (lambda (_prompt) (signal 'quit nil))))
      (pi-coding-agent--tree-summarize-and-navigate
       'fake-proc "node-1" (current-buffer) (current-buffer))
      (should-not rpc-called))))

(ert-deftest pi-coding-agent-test-navigate-tree-async-success ()
  "Navigate tree async calls handler on success."
  (let ((navigated nil)
        (chat-refreshed nil))
    (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
               (lambda (_proc _cmd callback)
                 (funcall callback
                          '(:success t
                            :data (:cancelled :false
                                   :editorText "test text")))))
              ((symbol-function 'pi-coding-agent--handle-navigate-success)
               (lambda (_proc result _chat _tree)
                 (setq navigated t
                       chat-refreshed (plist-get result :editorText)))))
      (pi-coding-agent--navigate-tree-async
       'fake-proc "node-1" nil nil
       (current-buffer) (current-buffer))
      (should navigated)
      (should (equal chat-refreshed "test text")))))

(ert-deftest pi-coding-agent-test-navigate-tree-async-on-success-callback ()
  "On-success callback fires after successful navigation."
  (let ((callback-called nil))
    (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
               (lambda (_proc _cmd callback)
                 (funcall callback
                          '(:success t
                            :data (:cancelled :false)))))
              ((symbol-function 'pi-coding-agent--handle-navigate-success)
               #'ignore))
      (pi-coding-agent--navigate-tree-async
       'fake-proc "node-1" nil nil
       (current-buffer) (current-buffer)
       (lambda () (setq callback-called t)))
      (should callback-called))))

(ert-deftest pi-coding-agent-test-navigate-tree-async-no-callback-on-cancel ()
  "On-success callback does not fire when navigation is cancelled."
  (let ((callback-called nil))
    (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
               (lambda (_proc _cmd callback)
                 (funcall callback
                          '(:success t
                            :data (:cancelled t))))))
      (pi-coding-agent--navigate-tree-async
       'fake-proc "node-1" nil nil
       (current-buffer) (current-buffer)
       (lambda () (setq callback-called t)))
      (should-not callback-called))))

(ert-deftest pi-coding-agent-test-navigate-tree-async-cancelled ()
  "Navigate tree async shows message when cancelled."
  (let ((messages nil))
    (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
               (lambda (_proc _cmd callback)
                 (funcall callback
                          '(:success t
                            :data (:cancelled t)))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (pi-coding-agent--navigate-tree-async
       'fake-proc "node-1" nil nil
       (current-buffer) (current-buffer))
      (should (cl-some (lambda (m) (string-match-p "cancelled" m))
                       messages)))))

;;;; Session Browser Switch

(ert-deftest pi-coding-agent-test-session-browser-switch-quits-on-success ()
  "Successful session switch calls quit-window on the browser window."
  (let ((quit-called nil)
        (quit-kill-arg nil))
    (with-temp-buffer
      (pi-coding-agent-session-browser-mode)
      (setq pi-coding-agent--session-browser-items
            (list '(:path "/test/a.jsonl" :name "Session A"
                    :messageCount 5 :modified "2026-02-24T10:00:00Z")))
      (setq pi-coding-agent--session-browser-sort "relevance")
      (pi-coding-agent--session-browser-rerender)
      (goto-char (point-min))
      (let ((chat-buf (generate-new-buffer " *test-chat*")))
        (unwind-protect
            (progn
              ;; Set process on chat buffer (--get-process reads from it)
              (with-current-buffer chat-buf
                (setq pi-coding-agent--process 'fake-proc))
              (setq pi-coding-agent--chat-buffer chat-buf)
              (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                         (lambda (_proc cmd callback)
                           (when (equal (plist-get cmd :type) "switch_session")
                             (funcall callback
                                      '(:success t
                                        :data (:cancelled :false))))))
                        ((symbol-function 'pi-coding-agent--load-session-history)
                         #'ignore)
                        ((symbol-function 'pi-coding-agent--apply-state-response)
                         #'ignore)
                        ((symbol-function 'pi-coding-agent--update-session-name-from-file)
                         #'ignore)
                        ((symbol-function 'quit-window)
                         (lambda (kill &optional _window)
                           (setq quit-called t
                                 quit-kill-arg kill))))
                (pi-coding-agent-session-browser-switch)))
          (kill-buffer chat-buf)))
      (should quit-called)
      ;; Should bury, not kill
      (should (null quit-kill-arg)))))

(ert-deftest pi-coding-agent-test-session-browser-switch-stays-on-cancel ()
  "Cancelled session switch does NOT call quit-window."
  (let ((quit-called nil))
    (with-temp-buffer
      (pi-coding-agent-session-browser-mode)
      (setq pi-coding-agent--session-browser-items
            (list '(:path "/test/a.jsonl" :name "Session A"
                    :messageCount 5 :modified "2026-02-24T10:00:00Z")))
      (setq pi-coding-agent--session-browser-sort "relevance")
      (pi-coding-agent--session-browser-rerender)
      (goto-char (point-min))
      (let ((chat-buf (generate-new-buffer " *test-chat*")))
        (unwind-protect
            (progn
              (with-current-buffer chat-buf
                (setq pi-coding-agent--process 'fake-proc))
              (setq pi-coding-agent--chat-buffer chat-buf)
              (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                         (lambda (_proc cmd callback)
                           (when (equal (plist-get cmd :type) "switch_session")
                             (funcall callback
                                      '(:success t
                                        :data (:cancelled t))))))
                        ((symbol-function 'quit-window)
                         (lambda (&rest _)
                           (setq quit-called t)))
                        ((symbol-function 'message) #'ignore))
                (pi-coding-agent-session-browser-switch)))
          (kill-buffer chat-buf)))
      (should-not quit-called))))

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
    ;; Default state: sort before scope, no named-only
    (should (equal (pi-coding-agent--session-dispatch-heading)
                   "sort:threaded │ scope:current"))
    ;; All state active
    (setq pi-coding-agent--session-browser-sort "recent"
          pi-coding-agent--session-browser-scope "all"
          pi-coding-agent--session-browser-named-only t)
    (should (equal (pi-coding-agent--session-dispatch-heading)
                   "sort:recent │ scope:all │ named-only"))))

;;;; Tree Browser Dispatch Transient

(ert-deftest pi-coding-agent-test-tree-browser-summarize-binding ()
  "Tree browser binds `S' to the summarize command."
  (should (eq (lookup-key pi-coding-agent-tree-browser-mode-map "S")
              'pi-coding-agent-tree-browser-summarize)))

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
  "Tree browser dispatch wires all keys to the correct commands."
  (let ((expected
         '(("RET" . pi-coding-agent-tree-browser-navigate)
           ("S"   . pi-coding-agent-tree-browser-summarize)
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
          (should (member "Press ? for available commands" messages))
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
          (should (member "Press ? for available commands" messages))
        (when-let ((buf (get-buffer
                         (pi-coding-agent--tree-browser-buffer-name
                          "/tmp/pi-test/"))))
          (kill-buffer buf))))))

(provide 'pi-coding-agent-browse-test)
;;; pi-coding-agent-browse-test.el ends here
