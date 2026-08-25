;;; pi-coding-agent-jsonl-test.el --- Tests for JSONL session module -*- lexical-binding: t; -*-

;;; Commentary:

;; Test suite for pi-coding-agent-jsonl.el: JSONL session reading, raw
;; session-tree building, RPC-style display projection, and tool-call
;; preview formatting.
;;
;; Three golden fixtures under test/fixtures/ form the oracle and are
;; mutually consistent by construction (same session, same ids):
;;
;;   browse-session.jsonl   - realistic session file (header + 35 entries)
;;                            covering the full matrix: fork with a shared
;;                            toolCallId in both branches, label
;;                            set/clear/re-set, session_info x2, custom
;;                            entries (one with promoted children),
;;                            custom_message x2 (string and block content,
;;                            one with display false), aborted/no-content/
;;                            errorMessage assistants, compaction,
;;                            branch_summary, model_change,
;;                            thinking_level_change, bashExecution, user
;;                            string and block content, assistant with
;;                            read+bash+default-json tool calls plus an
;;                            unresolvable toolCallId, sibling timestamps
;;                            deliberately out of file order, and a final
;;                            label line as the raw leaf.
;;   browse-raw.json        - expected build-tree output (raw nested
;;                            :entry/:children/:label nodes, exactly the
;;                            shape of pi's get_tree before projection)
;;   browse-projected.json  - expected project-tree output (filtered nodes
;;                            gone, children promoted, hand-computed
;;                            previews)
;;
;; Golden comparisons canonicalize both sides through a JSON round-trip
;; (json-encode, then json-parse-string).  That makes the comparison
;; tolerant of nil-versus-:null and list-versus-vector representation on
;; the implementation side, but it still pins key order and key presence.
;;
;; Internal helper signatures pinned by the unit tables below; the green
;; implementation must provide them under the `pi-coding-agent--jsonl-'
;; prefix:
;;
;;   (pi-coding-agent--jsonl-extract-text CONTENT &optional MAX-LEN)
;;   (pi-coding-agent--jsonl-normalize-preview TEXT)
;;   (pi-coding-agent--jsonl-shorten-path PATH)
;;   (pi-coding-agent--jsonl-arg-number ARGS KEY)

;;; Code:

(require 'ert)
(require 'json)
(require 'pi-coding-agent-jsonl)
(require 'pi-coding-agent-test-common)

;;;; Helpers

(defconst pi-coding-agent-test--jsonl-session-file "browse-session.jsonl"
  "Golden session fixture for the jsonl module.")

(defun pi-coding-agent-test--jsonl-session-path ()
  "Return the absolute path of the golden session fixture."
  (expand-file-name pi-coding-agent-test--jsonl-session-file
                    pi-coding-agent-test--fixture-dir))

(defun pi-coding-agent-test--write-jsonl (path lines)
  "Write LINES (entry plists, header first) to PATH, one JSON per line.
Nil plist values encode as JSON null, as in real session files.  The
keywords :null, :true, and :false (how tests spell JSON literals)
encode as their JSON counterparts."
  (with-temp-file path
    (dolist (line lines)
      (insert (json-encode (pi-coding-agent-test--jsonl-literalize line))
              "\n"))))

(defun pi-coding-agent-test--jsonl-normalize (data)
  "Normalize plist tree DATA recursively for JSON encoding.
`json-encode' stringifies the keywords :null, :true, and :false when
they appear as object values, so map them to their encodable
counterparts first."
  (cond
   ((eq data :null) nil)
   ((eq data :true) t)
   ((eq data :false) 'false)
   ((vectorp data)
    (vconcat (mapcar #'pi-coding-agent-test--jsonl-normalize data)))
   ((consp data)
    (let (out)
      (while (consp data)
        (let ((key (pop data)))
          (push key out)
          (push (if (consp data)
                    (pi-coding-agent-test--jsonl-normalize (pop data))
                  nil)
                out)))
      (nreverse out)))
   (t data)))

(defun pi-coding-agent-test--jsonl-literalize (data)
  "Recursively map JSON-literal keywords in plist tree DATA to the
values `json-encode' serializes as null, true, and false."
  (cond
   ((eq data :null) nil)
   ((eq data :true) t)
   ((eq data :false) :json-false)
   ((vectorp data)
    (vconcat (mapcar #'pi-coding-agent-test--jsonl-literalize data)))
   ((consp data)
    (let (out)
      (while (consp data)
        (let ((key (pop data)))
          (push key out)
          (push (if (consp data)
                    (pi-coding-agent-test--jsonl-literalize (pop data))
                  nil)
                out)))
      (nreverse out)))
   (t data)))

(defun pi-coding-agent-test--jsonl-canonicalize (data)
  "Canonicalize plist tree DATA through a JSON round-trip."
  (json-parse-string
   (json-encode (pi-coding-agent-test--jsonl-normalize data))
   :object-type 'plist))

(defun pi-coding-agent-test--jsonl-fixture-canonical (filename)
  "Read JSON fixture FILENAME and canonicalize it like actual results."
  (pi-coding-agent-test--jsonl-canonicalize
   (pi-coding-agent-test--read-json-fixture filename)))

(defun pi-coding-agent-test--jsonl-build-lines (lines)
  "Write LINES (header first) to a temp file and build the raw tree.
Returns the (:tree ... :leafId ...) plist from `build-tree'."
  (let* ((dir (pi-coding-agent-test--make-temp-directory "pi-jsonl-build"))
         (path (expand-file-name "session.jsonl" dir))
         (session (progn (pi-coding-agent-test--write-jsonl path lines)
                         (pi-coding-agent-jsonl-read-file path))))
    (pi-coding-agent-jsonl-build-tree (plist-get session :entries))))

(defun pi-coding-agent-test--jsonl-project-lines (lines)
  "Write LINES (header first) to a temp file, then read, build, project.
Returns the (:tree ... :leafId ...) plist from `project-tree'."
  (pcase-let ((`(,raw-tree ,raw-leaf)
               (pcase (pi-coding-agent-test--jsonl-build-lines lines)
                 ((map (:tree rt) (:leafId rl)) (list rt rl)))))
    (pi-coding-agent-jsonl-project-tree raw-tree raw-leaf)))

(defun pi-coding-agent-test--jsonl-find (tree id)
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

(defun pi-coding-agent-test--jsonl-find-raw (tree id)
  "Find the raw node whose :entry has :id ID in TREE; nil when absent."
  (let ((stack (append tree nil))
        (found nil))
    (while (and stack (not found))
      (let ((node (pop stack)))
        (if (equal (plist-get (plist-get node :entry) :id) id)
            (setq found node)
          (setq stack (append (append (plist-get node :children) nil)
                              stack)))))
    found))

(defconst pi-coding-agent-test--jsonl-header
  '(:type "session" :version 3 :id "5eed0000-0000-4000-8000-000000000000"
    :timestamp "2026-03-02T10:00:00.000Z" :cwd "/tmp/pi-jsonl-test")
  "Reusable session header for small in-test sessions.")

(defun pi-coding-agent-test--jsonl-entry (type id parent second &rest payload)
  "Return an entry plist of TYPE, ID, and PARENT (string or nil).
SECOND is the seconds component of the 2026-03-02T10:00:00Z timestamp;
PAYLOAD is the plist tail (:message, :targetId, ...)."
  (append (list :type type :id id :parentId parent
                :timestamp (format "2026-03-02T10:%02d:%02d.000Z"
                                   (/ second 60) (% second 60)))
          payload))

(defun pi-coding-agent-test--jsonl-msg (id parent second message)
  "Return a message entry whose message plist is MESSAGE."
  (pi-coding-agent-test--jsonl-entry "message" id parent second
                                     :message message))

(defun pi-coding-agent-test--jsonl-asst (id parent second &rest props)
  "Return an assistant message entry with message plist PROPS."
  (pi-coding-agent-test--jsonl-msg
   id parent second (append '(:role "assistant") props)))

;;;; Golden round-trip tests

(ert-deftest pi-coding-agent-test-jsonl-build-tree-golden ()
  "build-tree over the golden session matches browse-raw.json."
  (let* ((session (pi-coding-agent-jsonl-read-file
                   (pi-coding-agent-test--jsonl-session-path)))
         (built (pi-coding-agent-jsonl-build-tree
                 (plist-get session :entries))))
    (should (equal (pi-coding-agent-test--jsonl-canonicalize built)
                   (pi-coding-agent-test--jsonl-fixture-canonical
                    "browse-raw.json")))))

(ert-deftest pi-coding-agent-test-jsonl-project-tree-golden ()
  "project-tree over browse-raw.json matches browse-projected.json."
  (let* ((raw (pi-coding-agent-test--read-json-fixture "browse-raw.json"))
         (result (pi-coding-agent-jsonl-project-tree
                  (plist-get raw :tree) (plist-get raw :leafId))))
    (should (equal (pi-coding-agent-test--jsonl-canonicalize result)
                   (pi-coding-agent-test--jsonl-fixture-canonical
                    "browse-projected.json")))))

(ert-deftest pi-coding-agent-test-jsonl-round-trip-golden ()
  "read-file, build-tree, and project-tree chained match the projection."
  (let* ((session (pi-coding-agent-jsonl-read-file
                   (pi-coding-agent-test--jsonl-session-path)))
         (built (pi-coding-agent-jsonl-build-tree
                 (plist-get session :entries)))
         (result (pi-coding-agent-jsonl-project-tree
                  (plist-get built :tree) (plist-get built :leafId))))
    (should (equal (pi-coding-agent-test--jsonl-canonicalize result)
                   (pi-coding-agent-test--jsonl-fixture-canonical
                    "browse-projected.json")))))

;;;; read-file

(ert-deftest pi-coding-agent-test-jsonl-read-file-missing ()
  "A nonexistent path reads as nil."
  (should-not (pi-coding-agent-jsonl-read-file
               "/nonexistent/pi-jsonl/no-such-file.jsonl")))

(ert-deftest pi-coding-agent-test-jsonl-read-file-empty-or-headerless ()
  "Empty files and files without a session header line read as nil."
  (let* ((dir (pi-coding-agent-test--make-temp-directory "pi-jsonl-bad"))
         (empty (expand-file-name "empty.jsonl" dir))
         (garbage (expand-file-name "garbage.jsonl" dir)))
    (with-temp-file empty)
    (with-temp-file garbage
      (insert "{\"type\":\"message\",\"id\":\"g1\"\n"
              "this is not json\n"
              "\n"))
    (should-not (pi-coding-agent-jsonl-read-file empty))
    (should-not (pi-coding-agent-jsonl-read-file garbage))))

(ert-deftest pi-coding-agent-test-jsonl-read-file-header-only ()
  "A header-only session reads as an empty entry vector with nil leaf."
  (let* ((dir (pi-coding-agent-test--make-temp-directory "pi-jsonl-hdr"))
         (path (expand-file-name "hdr.jsonl" dir))
         (data (progn (pi-coding-agent-test--write-jsonl
                       path (list pi-coding-agent-test--jsonl-header))
                      (pi-coding-agent-jsonl-read-file path))))
    (should data)
    (should (equal (plist-get data :path) path))
    (should (equal (plist-get (plist-get data :header) :cwd)
                   "/tmp/pi-jsonl-test"))
    (should (equal (plist-get data :entries) []))
    (should-not (plist-get data :leafId))
    (should-not (plist-get data :name))))

(ert-deftest pi-coding-agent-test-jsonl-read-file-fixture-shape ()
  "File order, leaf id, and session name for the golden session."
  (let* ((data (pi-coding-agent-jsonl-read-file
                (pi-coding-agent-test--jsonl-session-path)))
         (entries (plist-get data :entries)))
    (should (= (length entries) 35))
    ;; First entry in file order is the branch-B head: the fixture models
    ;; a session file after a navigation rewrite, so line order and tree
    ;; order deliberately disagree.
    (should (equal (plist-get (aref entries 0) :id) "aa000019"))
    ;; The leaf is the id of the last line, whatever its type.
    (should (equal (plist-get data :leafId) "aa000023"))
    ;; The latest session_info entry wins.
    (should (equal (plist-get data :name) "Build fixed"))))

(ert-deftest pi-coding-agent-test-jsonl-read-file-skips-bad-lines ()
  "Malformed and blank lines are skipped silently."
  (let* ((dir (pi-coding-agent-test--make-temp-directory "pi-jsonl-skip"))
         (path (expand-file-name "bad.jsonl" dir))
         (data (progn
                 (with-temp-file path
                   (insert (json-encode pi-coding-agent-test--jsonl-header) "\n"
                           "{not json at all\n"
                           "\n"
                           "{\"type\":\"message\",\"id\":\"trunc\"\n"
                           (json-encode '(:type "message" :id "ok1" :parentId nil
                                          :timestamp "2026-03-02T10:00:01.000Z"
                                          :message (:role "user" :content "hi")))
                           "\n"))
                 (pi-coding-agent-jsonl-read-file path))))
    (should data)
    (should (= (length (plist-get data :entries)) 1))
    (should (equal (plist-get data :leafId) "ok1"))))

;;;; build-tree: label folding, roots, sibling sorting

(ert-deftest pi-coding-agent-test-jsonl-label-folding ()
  "Labels replay in file order: set, clear, re-set; latest wins."
  (let* ((built (pi-coding-agent-test--jsonl-build-lines
                 (list pi-coding-agent-test--jsonl-header
                       (pi-coding-agent-test--jsonl-msg
                        "u1" nil 0 '(:role "user" :content "root"))
                       (pi-coding-agent-test--jsonl-entry
                        "label" "l1" "u1" 1 :targetId "u1" :label "first")
                       (pi-coding-agent-test--jsonl-entry
                        "label" "l2" "l1" 2 :targetId "u1")
                       (pi-coding-agent-test--jsonl-entry
                        "label" "l3" "l2" 3 :targetId "u1" :label "final"))))
         (tree (plist-get built :tree))
         (root (aref tree 0)))
    (should (equal (plist-get root :label) "final"))
    (should (equal (plist-get root :labelTimestamp)
                   "2026-03-02T10:00:03.000Z"))
    ;; Label entries are ordinary raw-tree nodes chained under the target.
    (let ((chain (plist-get root :children)))
      (should (= (length chain) 1))
      (should (equal (plist-get (plist-get (aref chain 0) :entry) :id) "l1"))
      (should (equal (plist-get built :leafId) "l3")))))

(ert-deftest pi-coding-agent-test-jsonl-label-clear-without-reset ()
  "A cleared label stays cleared; the :label key is omitted entirely."
  (let* ((built (pi-coding-agent-test--jsonl-build-lines
                 (list pi-coding-agent-test--jsonl-header
                       (pi-coding-agent-test--jsonl-msg
                        "u1" nil 0 '(:role "user" :content "root"))
                       (pi-coding-agent-test--jsonl-msg
                        "u2" "u1" 1 '(:role "assistant" :content "node"))
                       (pi-coding-agent-test--jsonl-entry
                        "label" "l1" "u2" 2 :targetId "u2" :label "temp")
                       (pi-coding-agent-test--jsonl-entry
                        "label" "l2" "l1" 3 :targetId "u2"))))
         (u2 (pi-coding-agent-test--jsonl-find-raw
              (plist-get built :tree) "u2")))
    (should u2)
    (should-not (plist-member u2 :label))
    (should-not (plist-member u2 :labelTimestamp))))

(ert-deftest pi-coding-agent-test-jsonl-label-empty-string-clears ()
  "An empty-string label clears like an absent one (JS truthiness)."
  (let* ((built (pi-coding-agent-test--jsonl-build-lines
                 (list pi-coding-agent-test--jsonl-header
                       (pi-coding-agent-test--jsonl-msg
                        "u1" nil 0 '(:role "user" :content "root"))
                       (pi-coding-agent-test--jsonl-entry
                        "label" "l1" "u1" 1 :targetId "u1" :label "keep")
                       (pi-coding-agent-test--jsonl-entry
                        "label" "l2" "l1" 2 :targetId "u1" :label ""))))
         (u1 (pi-coding-agent-test--jsonl-find-raw
              (plist-get built :tree) "u1")))
    (should u1)
    (should-not (plist-member u1 :label))
    (should-not (plist-member u1 :labelTimestamp))))

(ert-deftest pi-coding-agent-test-jsonl-read-file-session-name-trim ()
  "Session names trim; a blank latest name clears to nil."
  (let* ((dir (pi-coding-agent-test--make-temp-directory "pi-jsonl-name"))
         (padded (expand-file-name "padded.jsonl" dir))
         (cleared (expand-file-name "cleared.jsonl" dir)))
    (pi-coding-agent-test--write-jsonl
     padded (list pi-coding-agent-test--jsonl-header
                  (pi-coding-agent-test--jsonl-entry
                   "session_info" "s1" nil 0 :name "  Build hunt  ")))
    (should (equal (plist-get (pi-coding-agent-jsonl-read-file padded) :name)
                   "Build hunt"))
    (pi-coding-agent-test--write-jsonl
     cleared (list pi-coding-agent-test--jsonl-header
                   (pi-coding-agent-test--jsonl-entry
                    "session_info" "s1" nil 0 :name "Build")
                   (pi-coding-agent-test--jsonl-entry
                    "session_info" "s2" "s1" 1 :name "   ")))
    (should-not (plist-get (pi-coding-agent-jsonl-read-file cleared) :name))))

(ert-deftest pi-coding-agent-test-jsonl-roots-and-cycles ()
  "Null, self, and missing parents are roots; cycle nodes are dropped."
  (let* ((built (pi-coding-agent-test--jsonl-build-lines
                 (list pi-coding-agent-test--jsonl-header
                       (pi-coding-agent-test--jsonl-msg
                        "r1" nil 0 '(:role "user" :content "root"))
                       (pi-coding-agent-test--jsonl-msg
                        "c1" "r1" 1 '(:role "user" :content "child"))
                       (pi-coding-agent-test--jsonl-msg
                        "s1" "s1" 2 '(:role "user" :content "self parent"))
                       (pi-coding-agent-test--jsonl-msg
                        "o1" "missing" 3 '(:role "user" :content "orphan"))
                       (pi-coding-agent-test--jsonl-msg
                        "x1" "x2" 4 '(:role "user" :content "cycle a"))
                       (pi-coding-agent-test--jsonl-msg
                        "x2" "x1" 5 '(:role "user" :content "cycle b")))))
         (tree (plist-get built :tree)))
    (should (equal (mapcar (lambda (node)
                             (plist-get (plist-get node :entry) :id))
                           (append tree nil))
                   '("r1" "s1" "o1")))
    ;; The cycle pair is unreachable and dropped entirely.
    (should-not (pi-coding-agent-test--jsonl-find-raw tree "x1"))
    (should-not (pi-coding-agent-test--jsonl-find-raw tree "x2"))
    ;; leafId still mirrors the physical last line, cycles or not.
    (should (equal (plist-get built :leafId) "x2"))
    ;; Projection keeps root order: roots seed the stack in natural
    ;; order, so a multi-root tree projects in file order.
    (should (equal (mapcar (lambda (node) (plist-get node :id))
                           (append (plist-get
                                    (pi-coding-agent-jsonl-project-tree tree)
                                    :tree)
                                   nil))
                   '("r1" "s1" "o1")))))

(ert-deftest pi-coding-agent-test-jsonl-sibling-sort-and-stable-ties ()
  "Children sort by timestamp; ties keep file order."
  (let* ((built (pi-coding-agent-test--jsonl-build-lines
                 (list pi-coding-agent-test--jsonl-header
                       (pi-coding-agent-test--jsonl-msg
                        "p" nil 0 '(:role "user" :content "parent"))
                       (pi-coding-agent-test--jsonl-msg
                        "late" "p" 30 '(:role "user" :content "file first"))
                       (pi-coding-agent-test--jsonl-msg
                        "early" "p" 10 '(:role "user" :content "ts first"))
                       (pi-coding-agent-test--jsonl-msg
                        "tie1" "p" 20 '(:role "user" :content "tie a"))
                       (pi-coding-agent-test--jsonl-msg
                        "tie2" "p" 20 '(:role "user" :content "tie b")))))
         (parent (aref (plist-get built :tree) 0))
         (kids (mapcar (lambda (node)
                         (plist-get (plist-get node :entry) :id))
                       (append (plist-get parent :children) nil))))
    (should (equal kids '("early" "tie1" "tie2" "late")))))

;;;; project-tree: roles, previews, leaf resolution

(ert-deftest pi-coding-agent-test-jsonl-role-mapping ()
  "Known roles pass through; anything else maps to unknown with rawRole."
  (let* ((result (pi-coding-agent-test--jsonl-project-lines
                  (list pi-coding-agent-test--jsonl-header
                        (pi-coding-agent-test--jsonl-msg
                         "u1" nil 0 '(:role "user" :content "hi"))
                        (pi-coding-agent-test--jsonl-msg
                         "c1" "u1" 1 '(:role "custom" :content "custom text"))
                        (pi-coding-agent-test--jsonl-msg
                         "b1" "c1" 2 '(:role "branchSummary"
                                        :content "branch summary text"))
                        (pi-coding-agent-test--jsonl-msg
                         "k1" "b1" 3 '(:role "compactionSummary"
                                        :content "compaction text"))
                        (pi-coding-agent-test--jsonl-msg
                         "w1" "k1" 4 '(:role "weird" :content "odd text")))))
         (tree (plist-get result :tree)))
    (should (equal (plist-get (pi-coding-agent-test--jsonl-find tree "u1")
                              :role)
                   "user"))
    (should (equal (plist-get (pi-coding-agent-test--jsonl-find tree "c1")
                              :role)
                   "custom"))
    (should (equal (plist-get (pi-coding-agent-test--jsonl-find tree "b1")
                              :role)
                   "branchSummary"))
    (should (equal (plist-get (pi-coding-agent-test--jsonl-find tree "k1")
                              :role)
                   "compactionSummary"))
    (let ((weird (pi-coding-agent-test--jsonl-find tree "w1")))
      (should (equal (plist-get weird :role) "unknown"))
      (should (equal (plist-get weird :rawRole) "weird"))
      (should (equal (plist-get weird :preview) "odd text")))))

(ert-deftest pi-coding-agent-test-jsonl-malformed-payloads-degrade ()
  "Null messages, null content, and null blocks degrade, never crash.
pi parses session files without validation; old or hand-edited files
can carry them.  The reference throws here; we project empties."
  (let* ((result (pi-coding-agent-test--jsonl-project-lines
                  (list pi-coding-agent-test--jsonl-header
                        ;; "message": null.
                        (pi-coding-agent-test--jsonl-entry
                         "message" "m1" nil 0 :message :null)
                        (pi-coding-agent-test--jsonl-msg
                         "m2" "m1" 1 '(:role "user" :content :null))
                        (pi-coding-agent-test--jsonl-msg
                         "m3" "m2" 2 '(:role "user"
                                        :content [:null
                                                  (:type "text"
                                                   :text "after null")]))
                        (pi-coding-agent-test--jsonl-msg
                         "m4" "m3" 3 '(:role "assistant" :content :null)))))
         (tree (plist-get result :tree)))
    (let ((m1 (pi-coding-agent-test--jsonl-find tree "m1")))
      (should (equal (plist-get m1 :role) "unknown"))
      (should-not (plist-get m1 :rawRole))
      (should (equal (plist-get m1 :preview) "")))
    (should (equal (plist-get (pi-coding-agent-test--jsonl-find tree "m2")
                              :preview)
                   ""))
    (should (equal (plist-get (pi-coding-agent-test--jsonl-find tree "m3")
                              :preview)
                   "after null"))
    (let ((m4 (pi-coding-agent-test--jsonl-find tree "m4")))
      (should (equal (plist-get m4 :preview) "(no content)"))
      (should (plist-member m4 :stopReason)))))

(ert-deftest pi-coding-agent-test-jsonl-assistant-preview-fallbacks ()
  "Assistant preview precedence: text, aborted, errorMessage, no content."
  (let* ((result (pi-coding-agent-test--jsonl-project-lines
                  (list pi-coding-agent-test--jsonl-header
                        (pi-coding-agent-test--jsonl-asst
                         "a1" nil 0 :content "Text wins over aborted."
                         :stopReason "aborted")
                        (pi-coding-agent-test--jsonl-asst
                         "a2" "a1" 1 :content "" :stopReason "end_turn")
                        (pi-coding-agent-test--jsonl-asst
                         "a3" "a2" 2 :content " \n\t ")
                        (pi-coding-agent-test--jsonl-asst
                         "a4" "a3" 3 :content [] :stopReason "aborted")
                        (pi-coding-agent-test--jsonl-asst
                         "a5" "a4" 4 :content [] :stopReason "error"
                         :errorMessage "boom:\nbad")
                        (pi-coding-agent-test--jsonl-asst
                         "a6" "a5" 5 :content []))))
         (tree (plist-get result :tree)))
    ;; Non-empty text beats every fallback.
    (let ((a1 (pi-coding-agent-test--jsonl-find tree "a1")))
      (should (equal (plist-get a1 :preview) "Text wins over aborted."))
      (should (equal (plist-get a1 :stopReason) "aborted")))
    ;; Empty-string content is falsy: the fallback chain applies.
    (let ((a2 (pi-coding-agent-test--jsonl-find tree "a2")))
      (should (equal (plist-get a2 :preview) "(no content)"))
      (should (equal (plist-get a2 :stopReason) "end_turn")))
    ;; Whitespace-only content is truthy: preview is the empty string,
    ;; not a fallback.
    (should (equal (plist-get (pi-coding-agent-test--jsonl-find tree "a3")
                              :preview)
                   ""))
    (should (equal (plist-get (pi-coding-agent-test--jsonl-find tree "a4")
                              :preview)
                   "(aborted)"))
    ;; errorMessage previews normalized, without the 200 cap.
    (let ((a5 (pi-coding-agent-test--jsonl-find tree "a5")))
      (should (equal (plist-get a5 :preview) "boom: bad"))
      (should (equal (plist-get a5 :errorMessage) "boom:\nbad"))
      (should (equal (plist-get a5 :stopReason) "error")))
    ;; stopReason is always present on assistant nodes, even when nil.
    (let ((a6 (pi-coding-agent-test--jsonl-find tree "a6")))
      (should (equal (plist-get a6 :preview) "(no content)"))
      (should (plist-member a6 :stopReason))
      (should-not (plist-member a6 :errorMessage)))))

(ert-deftest pi-coding-agent-test-jsonl-fork-branch-scoped-resolution ()
  "A shared toolCallId resolves per branch, beating the global map."
  (let* ((session (pi-coding-agent-jsonl-read-file
                   (pi-coding-agent-test--jsonl-session-path)))
         (built (pi-coding-agent-jsonl-build-tree
                 (plist-get session :entries)))
         (result (pi-coding-agent-jsonl-project-tree
                  (plist-get built :tree) (plist-get built :leafId)))
         (tree (plist-get result :tree))
         (ra (pi-coding-agent-test--jsonl-find tree "aa00000a"))
         (rb (pi-coding-agent-test--jsonl-find tree "aa00001a")))
    ;; Both branches call tc04; each toolResult resolves against the
    ;; assistant on its own branch path.
    (should (equal (plist-get ra :formattedToolCall)
                   "[read: /srv/demo/scripts/build.sh:1-40]"))
    (should (equal (plist-get ra :toolName) "read"))
    (should (equal (plist-get rb :formattedToolCall)
                   "[bash: grep -rn 'error' /var/log/demo.log | head -20]"))
    (should (equal (plist-get rb :toolName) "bash"))
    (should-not (equal (plist-get ra :formattedToolCall)
                       (plist-get rb :formattedToolCall)))))

(ert-deftest pi-coding-agent-test-jsonl-branch-map-sibling-isolation ()
  "Tool-call maps stay branch-scoped: a sibling's redefinition never
leaks into another sibling's tool results, and the global map (where
the redefinition also wins) never overrides the branch one."
  (let* ((result (pi-coding-agent-test--jsonl-project-lines
                  (list pi-coding-agent-test--jsonl-header
                        ;; a1 calls tc09 with read /a.py.
                        (pi-coding-agent-test--jsonl-msg
                         "a1" nil 0
                         '(:role "assistant"
                           :content [(:type "toolCall" :id "tc09"
                                            :name "read"
                                            :arguments (:path "/a.py"))]
                           :stopReason "tool_calls"))
                        ;; x1, a1's first child, redefines tc09 with bash.
                        (pi-coding-agent-test--jsonl-msg
                         "x1" "a1" 1
                         '(:role "assistant"
                           :content [(:type "toolCall" :id "tc09"
                                            :name "bash"
                                            :arguments (:command "echo hi"))]
                           :stopReason "tool_calls"))
                        ;; x1's own result resolves the redefinition.
                        (pi-coding-agent-test--jsonl-msg
                         "t2" "x1" 2 '(:role "toolResult" :toolCallId "tc09"))
                        ;; t1, a1's second child, must see a1's original,
                        ;; not x1's and not the global map's bash.
                        (pi-coding-agent-test--jsonl-msg
                         "t1" "a1" 3 '(:role "toolResult" :toolCallId "tc09")))))
         (tree (plist-get result :tree)))
    (let ((t1 (pi-coding-agent-test--jsonl-find tree "t1")))
      (should (equal (plist-get t1 :formattedToolCall) "[read: /a.py]"))
      (should (equal (plist-get t1 :toolName) "read")))
    (should (equal (plist-get (pi-coding-agent-test--jsonl-find tree "t2")
                              :formattedToolCall)
                   "[bash: echo hi]"))))

(ert-deftest pi-coding-agent-test-jsonl-projected-leaf-resolution ()
  "Projected leaf walks raw parents to the nearest visible entry."
  (let* ((raw (pi-coding-agent-test--read-json-fixture "browse-raw.json"))
         (tree (plist-get raw :tree)))
    ;; nil and unknown leaf ids resolve to nil.
    (should-not (plist-get (pi-coding-agent-jsonl-project-tree tree)
                           :leafId))
    (should-not (plist-get (pi-coding-agent-jsonl-project-tree tree "deadbeef")
                           :leafId))
    ;; The raw leaf is a label entry: walk up the bookkeeping chain.
    (should (equal (plist-get (pi-coding-agent-jsonl-project-tree
                               tree (plist-get raw :leafId))
                              :leafId)
                   "aa000018"))
    ;; A filtered session_info leaf walks up the same way.
    (should (equal (plist-get (pi-coding-agent-jsonl-project-tree
                               tree "aa000022")
                              :leafId)
                   "aa000018"))))

(ert-deftest pi-coding-agent-test-jsonl-preview-truncation ()
  "Previews slice content to 200 chars before normalization."
  (let* ((s199 (make-string 199 ?a))
         (s200 (make-string 200 ?a))
         (s201 (make-string 201 ?a))
         (result (pi-coding-agent-test--jsonl-project-lines
                  (list pi-coding-agent-test--jsonl-header
                        (pi-coding-agent-test--jsonl-msg
                         "u199" nil 0 `(:role "user" :content ,s199))
                        (pi-coding-agent-test--jsonl-msg
                         "u200" "u199" 1 `(:role "user" :content ,s200))
                        (pi-coding-agent-test--jsonl-msg
                         "u201" "u200" 2 `(:role "user" :content ,s201))
                        (pi-coding-agent-test--jsonl-msg
                         "ujoin" "u201" 3
                         `(:role "user"
                           :content [(:type "text" :text ,(make-string 150 ?x))
                                     (:type "text" :text ,(make-string 150 ?y))]))
                        (pi-coding-agent-test--jsonl-msg
                         "utrim" "ujoin" 4
                         `(:role "user"
                           :content ,(concat "keep" (make-string 250 ?\s)))))))
         (tree (plist-get result :tree)))
    (should (equal (plist-get (pi-coding-agent-test--jsonl-find tree "u199")
                              :preview)
                   s199))
    (should (equal (plist-get (pi-coding-agent-test--jsonl-find tree "u200")
                              :preview)
                   s200))
    (should (equal (plist-get (pi-coding-agent-test--jsonl-find tree "u201")
                              :preview)
                   s200))
    ;; Blocks join with a space first, then slice.
    (should (equal (plist-get (pi-coding-agent-test--jsonl-find tree "ujoin")
                              :preview)
                   (concat (make-string 150 ?x) " " (make-string 49 ?y))))
    ;; Slice happens before trimming, so trailing padding disappears.
    (should (equal (plist-get (pi-coding-agent-test--jsonl-find tree "utrim")
                              :preview)
                   "keep"))))

;;;; format-tool-call unit tables

(ert-deftest pi-coding-agent-test-jsonl-format-tool-call-read ()
  "read previews: shortening, offset/limit arithmetic, falsy ends."
  (let ((process-environment '("HOME=/home/tester")))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "read" '(:path "/home/tester/proj/a.py"
                            :offset 10 :limit 20))
                   "[read: ~/proj/a.py:10-29]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :offset 7))
                   "[read: /tmp/x.py:7]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :limit 20))
                   "[read: /tmp/x.py:1-20]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py"))
                   "[read: /tmp/x.py]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "read" '(:file_path "/tmp/b.py"))
                   "[read: /tmp/b.py]"))
    ;; Empty-string paths fall through to file_path.
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "read" '(:path "" :file_path "/tmp/c.py"))
                   "[read: /tmp/c.py]"))
    ;; JSON null normalizes to absent.
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "read" '(:path :null :file_path "/tmp/z.py"))
                   "[read: /tmp/z.py]"))
    ;; JSON null is present-but-null: null !== undefined triggers the
    ;; suffix; offset ?? 1 and null-limit arithmetic coerce to 1 and 0.
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :offset :null))
                   "[read: /tmp/x.py:1]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :limit :null))
                   "[read: /tmp/x.py:1]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :offset 10 :limit :null))
                   "[read: /tmp/x.py:10-9]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call "read" '())
                   "[read: ]"))
    ;; offset 0 stays 0 (explicit zero survives the ?? 1 default).
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :offset 0 :limit 5))
                   "[read: /tmp/x.py:0-4]"))
    ;; limit 0 with an offset: end = start - 1, ported as-is.
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :offset 3 :limit 0))
                   "[read: /tmp/x.py:3-2]"))
    ;; limit 0 alone: end 0 is JS-falsy, so no -end suffix.
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "read" '(:path "/tmp/x.py" :limit 0))
                   "[read: /tmp/x.py:1]"))))

(ert-deftest pi-coding-agent-test-jsonl-format-tool-call-write-edit ()
  "write and edit previews shorten their paths."
  (let ((process-environment '("HOME=/home/tester")))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "write" '(:path "/home/tester/w.txt"))
                   "[write: ~/w.txt]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "edit" '(:file_path "/tmp/e.py"))
                   "[edit: /tmp/e.py]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "write" '(:path "" :file_path ""))
                   "[write: ]"))))

(ert-deftest pi-coding-agent-test-jsonl-format-tool-call-bash ()
  "bash previews normalize whitespace and truncate at 50 chars."
  (let ((process-environment '("HOME=/home/tester"))
        (cmd50 (make-string 50 ?a)))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "bash" (list :command cmd50))
                   (concat "[bash: " cmd50 "]")))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "bash" (list :command (concat cmd50 "Z")))
                   (concat "[bash: " cmd50 "...]")))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "bash" '(:command "  ls\t-l\nfoo  "))
                   "[bash: ls -l foo]"))
    ;; Empty or absent command falls through to the empty string.
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "bash" '(:command ""))
                   "[bash: ]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call "bash" '())
                   "[bash: ]"))))

(ert-deftest pi-coding-agent-test-jsonl-format-tool-call-search ()
  "grep, find, and ls previews with path defaults."
  (let ((process-environment '("HOME=/home/tester")))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "grep" '(:pattern "TODO" :path "/home/tester/src"))
                   "[grep: /TODO/ in ~/src]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "grep" '(:pattern "TODO"))
                   "[grep: /TODO/ in .]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "grep" '(:path "/x"))
                   "[grep: // in /x]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "find" '(:pattern "*.el" :path "/tmp"))
                   "[find: *.el in /tmp]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call "find" '())
                   "[find:  in .]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "ls" '(:path "/tmp"))
                   "[ls: /tmp]"))
    (should (equal (pi-coding-agent-jsonl-format-tool-call "ls" '())
                   "[ls: .]"))))

(ert-deftest pi-coding-agent-test-jsonl-format-tool-call-default-json ()
  "Unknown tools fall back to compact JSON args with a 40-char cap."
  (let ((process-environment '("HOME=/home/tester")))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "list_issues" '(:repo "acme/api" :state "open"))
                   "[list_issues: {\"repo\":\"acme/api\",\"state\":\"open\"}]"))
    (let* ((args40 (list :k (make-string 32 ?x)))
           (json40 (json-encode args40))
           (args41 (list :k (make-string 33 ?x)))
           (json41 (json-encode args41)))
      (should (= (length json40) 40))
      (should (= (length json41) 41))
      (should (equal (pi-coding-agent-jsonl-format-tool-call "toolx" args40)
                     (concat "[toolx: " json40 "]")))
      (should (equal (pi-coding-agent-jsonl-format-tool-call "toolx" args41)
                     (concat "[toolx: " (substring json41 0 40) "...]"))))
    (should (equal (pi-coding-agent-jsonl-format-tool-call
                    "flag_tool" '(:n 0 :ok :true))
                   "[flag_tool: {\"n\":0,\"ok\":true}]"))))

;;;; Internal helper unit tables

(ert-deftest pi-coding-agent-test-jsonl-shorten-path ()
  "HOME (then USERPROFILE) prefixes collapse to ~, blindly."
  (let ((process-environment '("HOME=/home/tester")))
    (should (equal (pi-coding-agent--jsonl-shorten-path "/home/tester/x/el")
                   "~/x/el"))
    (should (equal (pi-coding-agent--jsonl-shorten-path "/opt/etc/conf")
                   "/opt/etc/conf"))
    ;; Blind prefix replacement, ported as-is (do not "fix").
    (let ((process-environment '("HOME=/home/tes")))
      (should (equal (pi-coding-agent--jsonl-shorten-path "/home/tester/x")
                     "~ter/x")))
    ;; USERPROFILE applies when HOME is unset.
    (let ((process-environment '("USERPROFILE=/up")))
      (should (equal (pi-coding-agent--jsonl-shorten-path "/up/f.txt")
                     "~/f.txt"))
      (should (equal (pi-coding-agent--jsonl-shorten-path "/var/log/x")
                     "/var/log/x")))
    ;; With neither variable set, nothing is shortened.
    (let ((process-environment '()))
      (should (equal (pi-coding-agent--jsonl-shorten-path "/up/f.txt")
                     "/up/f.txt")))))

(ert-deftest pi-coding-agent-test-jsonl-extract-text ()
  "Strings pass through; block vectors join text blocks with spaces."
  (should (equal (pi-coding-agent--jsonl-extract-text "hello world" 5)
                 "hello"))
  (should (equal (pi-coding-agent--jsonl-extract-text "hello") "hello"))
  ;; Slicing clamps at the end of the string.
  (should (equal (pi-coding-agent--jsonl-extract-text "plain" 200) "plain"))
  (should (equal (pi-coding-agent--jsonl-extract-text "abc" 0) ""))
  (should (equal (pi-coding-agent--jsonl-extract-text nil) ""))
  (should (equal (pi-coding-agent--jsonl-extract-text "") ""))
  (should (equal (pi-coding-agent--jsonl-extract-text (vector)) ""))
  ;; Only text blocks with string text contribute; join with spaces.
  (should (equal (pi-coding-agent--jsonl-extract-text
                  (vector '(:type "text" :text "a")
                          '(:type "image")
                          '(:type "text" :text "b")))
                 "a b"))
  (should (equal (pi-coding-agent--jsonl-extract-text
                  (vector '(:type "text" :text 5)))
                 ""))
  ;; Joining happens before slicing.
  (should (equal (pi-coding-agent--jsonl-extract-text
                  (vector '(:type "text" :text "abcdef")) 4)
                 "abcd")))

(ert-deftest pi-coding-agent-test-jsonl-normalize-preview ()
  "Newlines and tabs become spaces and the result is trimmed; \\r stays."
  (should (equal (pi-coding-agent--jsonl-normalize-preview "a\nb\tc") "a b c"))
  (should (equal (pi-coding-agent--jsonl-normalize-preview "  x  ") "x"))
  (should (equal (pi-coding-agent--jsonl-normalize-preview "a\rb") "a\rb"))
  (should (equal (pi-coding-agent--jsonl-normalize-preview "\n\t x \t\n") "x")))

(ert-deftest pi-coding-agent-test-jsonl-arg-number ()
  "Numeric arg lookup: zero counts as present; JSON null reads as
present-but-null; strings do not."
  (should (equal (pi-coding-agent--jsonl-arg-number '(:offset 0) :offset) 0))
  (should (equal (pi-coding-agent--jsonl-arg-number '(:limit 0) :limit) 0))
  (should (equal (pi-coding-agent--jsonl-arg-number '(:offset 5) :offset) 5))
  (should (eq (pi-coding-agent--jsonl-arg-number '(:offset :null) :offset)
              :null))
  (should-not (pi-coding-agent--jsonl-arg-number '() :offset))
  (should-not (pi-coding-agent--jsonl-arg-number '(:offset "5") :offset))
  (should-not (pi-coding-agent--jsonl-arg-number '(:limit 5) :offset)))

(ert-deftest pi-coding-agent-test-jsonl-duplicate-id-cycle-terminates ()
  "Duplicate ids closing a cycle terminate instead of looping forever.
pi generates collision-checked unique ids, so only hand-edited files
hit this; the expansion guard keeps building total."
  (let* ((built (pi-coding-agent-test--jsonl-build-lines
                 (list pi-coding-agent-test--jsonl-header
                       (pi-coding-agent-test--jsonl-msg
                        "dup1" nil 0 '(:role "user" :content "root"))
                       (pi-coding-agent-test--jsonl-msg
                        "mid" "dup1" 1 '(:role "user" :content "middle"))
                       ;; Same id as the root, parented into the chain.
                       (pi-coding-agent-test--jsonl-msg
                        "dup1" "mid" 2 '(:role "user" :content "dup")))))
         (tree (plist-get built :tree)))
    (should (= (length tree) 1))
    (should (equal (plist-get (plist-get (aref tree 0) :entry) :id) "dup1"))))

;;;; Deep chain

(ert-deftest pi-coding-agent-test-jsonl-deep-chain ()
  "A 2500-entry chain with a forked tip builds and projects iteratively."
  (let* ((dir (pi-coding-agent-test--make-temp-directory "pi-jsonl-deep"))
         (path (expand-file-name "deep.jsonl" dir))
         (lines (list pi-coding-agent-test--jsonl-header)))
    (dotimes (i 2500)
      (push (pi-coding-agent-test--jsonl-msg
             (format "dc%04d" i)
             (if (zerop i) nil (format "dc%04d" (1- i)))
             i
             `(:role "user" :content ,(format "tick %d" i)))
            lines))
    ;; The fork branches off near the tip with a later timestamp and is
    ;; appended last, so it provides the leaf.
    (push (pi-coding-agent-test--jsonl-msg
           "df0000" "dc2498" 2999 '(:role "user" :content "fork"))
          lines)
    (push (pi-coding-agent-test--jsonl-msg
           "df0001" "df0000" 3000 '(:role "user" :content "fork tip"))
          lines)
    (pi-coding-agent-test--write-jsonl path (nreverse lines))
    (let* ((session (pi-coding-agent-jsonl-read-file path))
           (built (pi-coding-agent-jsonl-build-tree
                   (plist-get session :entries)))
           (result (pi-coding-agent-jsonl-project-tree
                    (plist-get built :tree) (plist-get built :leafId)))
           (tree (plist-get result :tree)))
      (should (equal (plist-get built :leafId) "df0001"))
      (should (equal (plist-get result :leafId) "df0001"))
      (should (equal (plist-get (aref tree 0) :id) "dc0000"))
      ;; Walk the single-child spine down to the fork point.
      (let ((node (aref tree 0))
            (hops 0))
        (while (not (equal (plist-get node :id) "dc2498"))
          (setq node (aref (plist-get node :children) 0)
                hops (1+ hops)))
        (should (= hops 2498))
        (let ((kids (plist-get node :children)))
          (should (= (length kids) 2))
          ;; Timestamp order: chain tip first, fork second.
          (should (equal (plist-get (aref kids 0) :id) "dc2499"))
          (should (equal (plist-get (aref kids 1) :id) "df0000"))
          (should (= (length (plist-get (aref kids 0) :children)) 0))
          (should (equal (plist-get (aref (plist-get (aref kids 1) :children) 0)
                                   :id)
                         "df0001"))))
      ;; Every one of the 2502 entries survives projection.
      (let ((count 0)
            (stack (append tree nil)))
        (while stack
          (let ((node (pop stack)))
            (setq count (1+ count))
            (setq stack (append (append (plist-get node :children) nil)
                                stack))))
        (should (= count 2502))))))

(provide 'pi-coding-agent-jsonl-test)
;;; pi-coding-agent-jsonl-test.el ends here
