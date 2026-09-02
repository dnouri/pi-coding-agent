;;; piem-fake-pi-test.el --- Black-box tests for fake pi harness -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests exercise the Python fake-pi harness as a real subprocess over
;; stdin/stdout.  They intentionally avoid poking Python internals so the fake
;; stays accountable to the JSONL RPC contract.

;;; Code:

(require 'ert)
(require 'piem)
(require 'piem-jsonl)
(require 'piem-test-common)
(require 'seq)

(defconst piem-fake-pi-test--timeout 5
  "Timeout in seconds for fake-pi black-box tests.")

(defun piem-fake-pi-test--process-filter (proc output)
  "Capture JSONL OUTPUT from fake-pi PROC."
  (let* ((partial (or (process-get proc 'fake-pi-partial) ""))
         (result (piem--accumulate-lines partial output))
         (lines (car result))
         (objects (process-get proc 'fake-pi-objects))
         (invalid (process-get proc 'fake-pi-invalid-lines))
         (new-objects nil)
         (new-invalid nil))
    (process-put proc 'fake-pi-raw-output
                 (concat (or (process-get proc 'fake-pi-raw-output) "") output))
    (process-put proc 'fake-pi-partial (cdr result))
    (dolist (line lines)
      (if-let ((json (piem--parse-json-line line)))
          (push json new-objects)
        (push line new-invalid)))
    (process-put proc 'fake-pi-objects (nconc objects (nreverse new-objects)))
    (process-put proc 'fake-pi-invalid-lines (nconc invalid (nreverse new-invalid)))))

(defun piem-fake-pi-test--start-process (scenario &optional extra-args)
  "Start fake-pi for SCENARIO with optional EXTRA-ARGS."
  (let ((proc (make-process
               :name (format "fake-pi-test-%s" scenario)
               :command (append (piem-test-fake-pi-executable)
                                (list "--mode" "rpc")
                                (piem-test-fake-pi-extra-args scenario extra-args))
               :connection-type 'pipe
               :coding 'utf-8-unix
               :filter #'piem-fake-pi-test--process-filter
               :noquery t)))
    (set-process-query-on-exit-flag proc nil)
    proc))

(defun piem-fake-pi-test--stop-process (proc)
  "Stop fake-pi PROC gracefully, with a bounded forced-kill fallback."
  (when (processp proc)
    (set-process-query-on-exit-flag proc nil)
    (when (process-live-p proc)
      ;; EOF lets the harness run its finally block and remove its temporary
      ;; session root.  Force termination only if graceful shutdown wedges.
      (ignore-errors (process-send-eof proc))
      (unless (piem-test-wait-until
               (lambda () (not (process-live-p proc))) 2 0.01 proc)
        (delete-process proc)))))

(defmacro piem-fake-pi-test-with-process (spec &rest body)
  "Bind PROC to a fake-pi process for SPEC, run BODY, then clean up.
SPEC is (PROC SCENARIO &rest EXTRA-ARGS)."
  (declare (indent 1) (debug t))
  (let ((proc (nth 0 spec))
        (scenario (nth 1 spec))
        (extra-args (nthcdr 2 spec)))
    `(let ((,proc (piem-fake-pi-test--start-process ,scenario (list ,@extra-args))))
       (unwind-protect
           (progn ,@body)
         (piem-fake-pi-test--stop-process ,proc)))))

(defun piem-fake-pi-test--send (proc command)
  "Send COMMAND plist to fake-pi PROC."
  (process-send-string proc (piem--encode-command command)))

(defun piem-fake-pi-test--pop-object (proc &optional timeout)
  "Pop the next parsed JSON object from PROC within TIMEOUT seconds."
  (unless (piem-test-wait-until
           (lambda () (process-get proc 'fake-pi-objects))
           (or timeout piem-fake-pi-test--timeout)
           0.01
           proc)
    (ert-fail
     (format "Timed out waiting for fake-pi output\nraw=%S\ninvalid=%S"
             (process-get proc 'fake-pi-raw-output)
             (process-get proc 'fake-pi-invalid-lines))))
  (let* ((objects (process-get proc 'fake-pi-objects))
         (next (car objects)))
    (process-put proc 'fake-pi-objects (cdr objects))
    next))

(defun piem-fake-pi-test--collect-until (proc predicate &optional timeout)
  "Collect objects from PROC until PREDICATE returns non-nil for the latest one."
  (let* ((items nil)
         (limit (or timeout piem-fake-pi-test--timeout))
         (deadline (+ (float-time) limit))
         done)
    (while (not done)
      (let* ((remaining (- deadline (float-time)))
             (item (piem-fake-pi-test--pop-object
                    proc (max 0.0 remaining))))
        (push item items)
        (setq done (funcall predicate item))))
    (nreverse items)))

(ert-deftest piem-fake-pi-test-collect-until-spends-one-timeout-budget ()
  "Repeated reads should spend one timeout budget instead of resetting it."
  (let ((timeouts nil)
        (items '((:type "message_start") (:type "agent_end"))))
    (cl-letf (((symbol-function 'piem-fake-pi-test--pop-object)
               (lambda (_proc timeout)
                 (push timeout timeouts)
                 (sleep-for 0.01)
                 (pop items))))
      (piem-fake-pi-test--collect-until
       :ignored
       (lambda (item) (equal (plist-get item :type) "agent_end"))
       1.0))
    (setq timeouts (nreverse timeouts))
    (should (= (length timeouts) 2))
    (should (> (car timeouts) (cadr timeouts)))))

(defun piem-fake-pi-test--event-types (objects)
  "Return the :type fields from OBJECTS."
  (mapcar (lambda (obj) (plist-get obj :type)) objects))

(defun piem-fake-pi-test--events-of-type (objects type)
  "Return events from OBJECTS whose top-level type is TYPE."
  (seq-filter (lambda (obj) (equal (plist-get obj :type) type)) objects))

(defun piem-fake-pi-test--message-events (objects type role)
  "Return TYPE message events from OBJECTS whose message has ROLE."
  (seq-filter
   (lambda (obj)
     (and (equal (plist-get obj :type) type)
          (equal (plist-get (plist-get obj :message) :role) role)))
   objects))

(defun piem-fake-pi-test--message-updates (objects type)
  "Return message updates from OBJECTS whose nested event has TYPE."
  (seq-filter
   (lambda (obj)
     (and (equal (plist-get obj :type) "message_update")
          (equal (plist-get (plist-get obj :assistantMessageEvent) :type) type)))
   objects))

(defun piem-fake-pi-test--rpc (proc command &optional timeout)
  "Send COMMAND to PROC and return its correlated response within TIMEOUT."
  (piem-fake-pi-test--send proc command)
  (let ((response (piem-fake-pi-test--pop-object proc timeout)))
    (unless (and (equal (plist-get response :type) "response")
                 (equal (plist-get response :command)
                        (plist-get command :type)))
      (ert-fail (format "Unexpected fake-pi response for %S: %S"
                        command response)))
    (when (plist-member command :id)
      (unless (equal (plist-get response :id) (plist-get command :id))
        (ert-fail (format "Fake-pi response lost request id for %S: %S"
                          command response))))
    response))

(defun piem-fake-pi-test--read-jsonl-file (path)
  "Parse every nonblank JSONL record in PATH into a vector of plists."
  (unless (file-readable-p path)
    (ert-fail (format "JSONL file is not readable: %S" path)))
  (with-temp-buffer
    (insert-file-contents path)
    (let ((line-number 0)
          (records nil))
      (goto-char (point-min))
      (while (not (eobp))
        (setq line-number (1+ line-number))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (unless (string-empty-p (string-trim line))
            (condition-case err
                (push (json-parse-string line
                                         :object-type 'plist
                                         :array-type 'array
                                         :null-object :null
                                         :false-object :false)
                      records)
              (error
               (ert-fail
                (format "Invalid JSONL at %s:%d: %s"
                        path line-number (error-message-string err)))))))
        (forward-line 1))
      (vconcat (nreverse records)))))

(defun piem-fake-pi-test--write-jsonl-file (path records)
  "Write RECORDS as strict JSONL to PATH."
  (with-temp-file path
    (dolist (record records)
      (insert (json-serialize record
                              :null-object :null
                              :false-object :false)
              "\n"))))

(defun piem-fake-pi-test--canonical-json (value)
  "Return an order-insensitive canonical representation of JSON VALUE."
  (cond
   ((vectorp value)
    (cons :array
          (mapcar #'piem-fake-pi-test--canonical-json
                  (append value nil))))
   ((and (consp value) (keywordp (car value)))
    (let ((cursor value)
          (pairs nil))
      (while cursor
        (unless (and (consp cursor) (consp (cdr cursor)))
          (ert-fail (format "Malformed JSON plist in test expectation: %S"
                            value)))
        (push (cons (car cursor)
                    (piem-fake-pi-test--canonical-json
                     (cadr cursor)))
              pairs)
        (setq cursor (cddr cursor)))
      (cons :object
            (sort pairs
                  (lambda (a b)
                    (string< (symbol-name (car a))
                             (symbol-name (car b))))))))
   (t value)))

(defun piem-fake-pi-test--json-equal-p (a b)
  "Return non-nil when JSON-shaped values A and B are semantically equal."
  (equal (piem-fake-pi-test--canonical-json a)
         (piem-fake-pi-test--canonical-json b)))

(defun piem-fake-pi-test--iso-timestamp-p (value)
  "Return non-nil when VALUE is a strict UTC ISO timestamp."
  (and (stringp value)
       (string-match-p
        "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\.[0-9]\\{3\\}Z\\'"
        value)
       (condition-case nil
           (progn (date-to-time value) t)
         (error nil))))

(defun piem-fake-pi-test--iso-to-ms (timestamp)
  "Convert ISO TIMESTAMP to Unix milliseconds."
  (truncate (* 1000 (float-time (date-to-time timestamp)))))

(defun piem-fake-pi-test--assert-v3-header (header)
  "Assert that HEADER has Pi's materialized v3 session shape."
  (should (equal (plist-get header :type) "session"))
  (should (numberp (plist-get header :version)))
  (should (= (plist-get header :version) 3))
  (should (stringp (plist-get header :id)))
  (should (piem-fake-pi-test--iso-timestamp-p
           (plist-get header :timestamp)))
  (let ((cwd (plist-get header :cwd)))
    (should (stringp cwd))
    (should (file-name-absolute-p cwd))
    (should (file-directory-p cwd))))

(defun piem-fake-pi-test--assert-entry-base (entry)
  "Assert that nonheader session ENTRY has the required v3 base fields."
  (should (stringp (plist-get entry :id)))
  (should (plist-member entry :parentId))
  (let ((parent-id (plist-get entry :parentId)))
    (should (or (stringp parent-id)
                (piem--json-null-p parent-id))))
  (should (piem-fake-pi-test--iso-timestamp-p
           (plist-get entry :timestamp))))

(defun piem-fake-pi-test--entry-by-id (entries id)
  "Return the entry in vector ENTRIES whose id is ID."
  (seq-find (lambda (entry) (equal (plist-get entry :id) id)) entries))

(defun piem-fake-pi-test--assert-valid-v3-records
    (header entries)
  "Assert that HEADER and append-ordered ENTRIES form a valid v3 file."
  (piem-fake-pi-test--assert-v3-header header)
  (let ((seen (make-hash-table :test #'equal)))
    (dotimes (i (length entries))
      (let* ((entry (aref entries i))
             (id (plist-get entry :id)))
        (piem-fake-pi-test--assert-entry-base entry)
        (should-not (gethash id seen))
        (puthash id t seen)))))

(defun piem-fake-pi-test--tree-nodes (tree)
  "Return every node in TREE using iterative preorder traversal."
  (let ((pending (append tree nil))
        (nodes nil))
    (while pending
      (let* ((node (pop pending))
             (children (plist-get node :children)))
        (push node nodes)
        (setq pending (append (append children nil) pending))))
    (nreverse nodes)))

(defun piem-fake-pi-test--zero-usage ()
  "Return a complete zero-valued Pi usage object."
  '(:input 0 :output 0 :cacheRead 0 :cacheWrite 0 :totalTokens 0
    :cost (:input 0 :output 0 :cacheRead 0 :cacheWrite 0 :total 0)))

(defun piem-fake-pi-test--assistant-message (text timestamp)
  "Return a valid persisted assistant message containing TEXT at TIMESTAMP."
  (list :role "assistant"
        :content (vector (list :type "text" :text text))
        :api "fake-api"
        :provider "fake"
        :model "fake-model"
        :usage (piem-fake-pi-test--zero-usage)
        :stopReason "stop"
        :timestamp timestamp))

(defun piem-fake-pi-test--user-message (text timestamp)
  "Return a valid persisted user message containing TEXT at TIMESTAMP."
  (list :role "user"
        :content (vector (list :type "text" :text text))
        :timestamp timestamp))

(defun piem-fake-pi-test--write-branched-v3-session (directory)
  "Write the Phase 5 branched v3 target under DIRECTORY and describe it."
  (let* ((path (expand-file-name "phase5-branched-target.jsonl" directory))
         (cwd (directory-file-name (expand-file-name directory)))
         (session-id "11111111-2222-4333-8444-555555555555")
         (root-user-id "10000001")
         (root-assistant-id "10000002")
         (active-user-id "10000003")
         (abandoned-user-id "10000004")
         (abandoned-assistant-id "10000005")
         (branch-summary-id "10000006")
         (custom-message-id "10000007")
         (old-label-id "10000008")
         (latest-label-id "10000009")
         (session-info-id "1000000a")
         (compaction-id "1000000b")
         (post-assistant-id "1000000c")
         (orphan-id "20000001")
         (thinking-level-id "20000002")
         (clear-label-set-id "20000003")
         (clear-label-id "20000004")
         (branch-summary "BRANCH SUMMARY: abandoned experiment recorded.")
         (custom-content "CUSTOM ACTIVE CONTEXT")
         (compaction-summary "COMPACTION SUMMARY: root exchange condensed.")
         (session-name "Phase 5 Branched Target")
         (latest-label "Latest active checkpoint")
         (header
          (list :type "session" :version 3 :id session-id
                :timestamp "2026-02-03T04:05:00.000Z" :cwd cwd))
         ;; The active sibling is physically first but has the later timestamp.
         ;; get_tree must therefore place the abandoned sibling first.
         (entries
          (vector
           (list :type "message" :id root-user-id :parentId :null
                 :timestamp "2026-02-03T04:05:01.000Z"
                 :message (piem-fake-pi-test--user-message
                           "ROOT USER CONTENT" 1770091501000))
           (list :type "message" :id root-assistant-id
                 :parentId root-user-id
                 :timestamp "2026-02-03T04:05:02.000Z"
                 :message (piem-fake-pi-test--assistant-message
                           "ROOT ASSISTANT CONTENT" 1770091502000))
           (list :type "message" :id active-user-id
                 :parentId root-assistant-id
                 :timestamp "2026-02-03T04:05:06.000Z"
                 :message (piem-fake-pi-test--user-message
                           "ACTIVE RETAINED USER CONTENT" 1770091506000))
           (list :type "message" :id abandoned-user-id
                 :parentId root-assistant-id
                 :timestamp "2026-02-03T04:05:04.000Z"
                 :message (piem-fake-pi-test--user-message
                           "ABANDONED USER CONTENT" 1770091504000))
           (list :type "message" :id abandoned-assistant-id
                 :parentId abandoned-user-id
                 :timestamp "2026-02-03T04:05:05.000Z"
                 :message (piem-fake-pi-test--assistant-message
                           "ABANDONED ASSISTANT CONTENT" 1770091505000))
           (list :type "custom" :id orphan-id
                 :parentId "missing-parent"
                 :timestamp "2026-02-03T04:05:03.500Z"
                 :customType "orphan-bookkeeping")
           ;; A real Pi 0.84.2 resume appends the current thinking level when
           ;; the active branch has none.  Include one so the target remains
           ;; byte-for-byte stable under both Pi and the fake.
           (list :type "thinking_level_change" :id thinking-level-id
                 :parentId active-user-id
                 :timestamp "2026-02-03T04:05:06.500Z"
                 :thinkingLevel "off")
           (list :type "branch_summary" :id branch-summary-id
                 :parentId thinking-level-id
                 :timestamp "2026-02-03T04:05:07.000Z"
                 :fromId abandoned-assistant-id :summary branch-summary)
           (list :type "custom_message" :id custom-message-id
                 :parentId branch-summary-id
                 :timestamp "2026-02-03T04:05:08.000Z"
                 :customType "phase-five-test" :content custom-content
                 :display t :details '(:origin "fake-rpc-red"))
           (list :type "label" :id old-label-id
                 :parentId custom-message-id
                 :timestamp "2026-02-03T04:05:09.000Z"
                 :targetId active-user-id :label "Earlier checkpoint")
           (list :type "label" :id latest-label-id
                 :parentId old-label-id
                 :timestamp "2026-02-03T04:05:10.000Z"
                 :targetId active-user-id :label latest-label)
           (list :type "label" :id clear-label-set-id
                 :parentId latest-label-id
                 :timestamp "2026-02-03T04:05:10.250Z"
                 :targetId root-assistant-id :label "Temporary root label")
           (list :type "label" :id clear-label-id
                 :parentId clear-label-set-id
                 :timestamp "2026-02-03T04:05:10.500Z"
                 :targetId root-assistant-id)
           (list :type "session_info" :id session-info-id
                 :parentId clear-label-id
                 :timestamp "2026-02-03T04:05:11.000Z"
                 :name session-name)
           (list :type "compaction" :id compaction-id
                 :parentId session-info-id
                 :timestamp "2026-02-03T04:05:12.000Z"
                 :summary compaction-summary
                 :firstKeptEntryId active-user-id :tokensBefore 4321)
           (list :type "message" :id post-assistant-id
                 :parentId compaction-id
                 :timestamp "2026-02-03T04:05:13.000Z"
                 :message (piem-fake-pi-test--assistant-message
                           "POST-COMPACTION ASSISTANT CONTENT"
                           1770091513000)))))
    (piem-fake-pi-test--write-jsonl-file
     path (cons header (append entries nil)))
    ;; Parse the actual bytes back so raw-entry expectations use precisely the
    ;; same JSON dialect as subprocess responses.
    (let* ((records (piem-fake-pi-test--read-jsonl-file path))
           (parsed-entries (seq-subseq records 1))
           (active-entry (piem-fake-pi-test--entry-by-id
                          parsed-entries active-user-id))
           (post-entry (piem-fake-pi-test--entry-by-id
                        parsed-entries post-assistant-id))
           (expected-messages
            (vector
             (list :role "compactionSummary" :summary compaction-summary
                   :tokensBefore 4321
                   :timestamp
                   (piem-fake-pi-test--iso-to-ms
                    "2026-02-03T04:05:12.000Z"))
             (plist-get active-entry :message)
             (list :role "branchSummary" :summary branch-summary
                   :fromId abandoned-assistant-id
                   :timestamp
                   (piem-fake-pi-test--iso-to-ms
                    "2026-02-03T04:05:07.000Z"))
             (list :role "custom" :customType "phase-five-test"
                   :content custom-content :display t
                   :details '(:origin "fake-rpc-red")
                   :timestamp
                   (piem-fake-pi-test--iso-to-ms
                    "2026-02-03T04:05:08.000Z"))
             (plist-get post-entry :message))))
      (piem-fake-pi-test--assert-valid-v3-records
       (aref records 0) parsed-entries)
      (list :path path
            :entries parsed-entries
            :expected-messages expected-messages
            :session-id session-id
            :session-name session-name
            :branch-summary branch-summary
            :latest-label latest-label
            :latest-label-timestamp "2026-02-03T04:05:10.000Z"
            :root-user-id root-user-id
            :root-assistant-id root-assistant-id
            :active-user-id active-user-id
            :abandoned-user-id abandoned-user-id
            :abandoned-assistant-id abandoned-assistant-id
            :orphan-id orphan-id
            :post-assistant-id post-assistant-id))))

(defun piem-fake-pi-test--logged-input-types-after-switch (records)
  "Return input command types in log RECORDS from the first switch onward."
  (let ((started nil)
        (types nil))
    (dotimes (i (length records))
      (let* ((record (aref records i))
             (payload (plist-get record :payload))
             (type (plist-get payload :type)))
        (when (and (equal (plist-get record :direction) "in")
                   (stringp type))
          (when (equal type "switch_session")
            (setq started t))
          (when started
            (push type types)))))
    (nreverse types)))

(defun piem-fake-pi-test--wait-or-fail
    (proc predicate description &optional timeout)
  "Wait for PREDICATE with PROC, or fail clearly with DESCRIPTION."
  (unless (piem-test-wait-until
           predicate
           (or timeout piem-fake-pi-test--timeout)
           0.01
           proc)
    (ert-fail (format "Timed out waiting for %s (process status: %S)"
                      description (and (processp proc) (process-status proc))))))

(defun piem-fake-pi-test--run-cli (&rest args)
  "Run fake-pi with ARGS and return `(:exit-code N :output STRING)'."
  (let ((command
         (concat
          (mapconcat #'shell-quote-argument
                     (append (piem-test-fake-pi-executable) args)
                     " ")
          " 2>&1")))
    (with-temp-buffer
      (list :exit-code (call-process-shell-command command nil (current-buffer) nil)
            :output (buffer-string)))))

(defmacro piem-fake-pi-test-with-session (spec &rest body)
  "Create a real piem session against fake-pi, then run BODY.
SPEC is (SESSION SCENARIO &rest EXTRA-ARGS)."
  (declare (indent 1) (debug t))
  (let ((session (nth 0 spec))
        (scenario (nth 1 spec))
        (extra-args (nthcdr 2 spec)))
    `(let* ((default-directory "/tmp/")
            (piem-executable
             (piem-test-fake-pi-executable))
            (piem-extra-args
             (piem-test-fake-pi-extra-args ,scenario (list ,@extra-args)))
            (,session nil))
       (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                 ((symbol-function 'piem--display-buffers) #'ignore))
         (unwind-protect
             (progn
               (piem)
               (let ((chat-name (piem-test--chat-buffer-name default-directory)))
                 (should
                  (piem-test-wait-until
                   (lambda ()
                     (let* ((chat-buf (get-buffer chat-name))
                            (input-buf (and chat-buf
                                            (with-current-buffer chat-buf
                                              piem--input-buffer)))
                            (proc (and chat-buf
                                       (with-current-buffer chat-buf
                                         piem--process))))
                       (and (buffer-live-p chat-buf)
                            (buffer-live-p input-buf)
                            (process-live-p proc))))
                   piem-fake-pi-test--timeout
                   0.01))
                 (let* ((chat-buf (get-buffer chat-name))
                        (input-buf (with-current-buffer chat-buf
                                     piem--input-buffer))
                        (proc (with-current-buffer chat-buf
                                piem--process)))
                   (setq ,session (list :chat-buffer chat-buf
                                        :input-buffer input-buf
                                        :process proc))
                   ,@body)))
           (let* ((original-chat
                   (get-buffer
                    (piem-test--chat-buffer-name default-directory)))
                  (chat-buf (or (plist-get ,session :chat-buffer)
                                original-chat))
                  (input-buf (plist-get ,session :input-buffer))
                  (proc (or (plist-get ,session :process)
                            (and (buffer-live-p chat-buf)
                                 (buffer-local-value
                                  'piem--process chat-buf)))))
             (piem-fake-pi-test--stop-process proc)
             ;; A resume can rename and retarget both buffers, so clean up the
             ;; captured objects rather than relying only on their startup names.
             (piem-test--kill-live-buffers input-buf chat-buf)
             (when (and (buffer-live-p original-chat)
                        (not (eq original-chat chat-buf)))
               (piem-test--kill-live-buffers original-chat)))
           (piem-test--kill-session-buffers default-directory))))))

(ert-deftest piem-fake-pi-test-get-state-handles-split-jsonl-record ()
  "get_state survives a deliberately split JSONL response."
  (piem-fake-pi-test-with-process
      (proc "prompt-lifecycle" "--split-response" "get_state:24")
    (piem-fake-pi-test--send proc '(:type "get_state"))
    (let* ((response (piem-fake-pi-test--pop-object proc))
           (data (plist-get response :data))
           (session-file (plist-get data :sessionFile)))
      (should (equal (plist-get response :type) "response"))
      (should (eq (plist-get response :success) t))
      (should (equal (plist-get response :command) "get_state"))
      (should (file-exists-p session-file)))))

(ert-deftest piem-fake-pi-test-requires-newline-before-eof ()
  "EOF alone must not act as an implicit JSONL record delimiter."
  (piem-fake-pi-test-with-process (proc "prompt-lifecycle")
    (process-send-string proc "{\"type\":\"get_state\"}")
    (process-send-eof proc)
    (should
     (piem-test-wait-until
      (lambda () (not (process-live-p proc)))
      piem-fake-pi-test--timeout
      0.01))
    (should-not (process-get proc 'fake-pi-objects))))

(ert-deftest piem-fake-pi-test-cli-rejects-unsupported-mode ()
  "The fake should fail fast when asked to run in an unsupported mode."
  (let* ((result (piem-fake-pi-test--run-cli
                  "--mode" "interactive"
                  "--scenario" "prompt-lifecycle"))
         (output (plist-get result :output)))
    (should-not (eq (plist-get result :exit-code) 0))
    (should (string-match-p "invalid choice" output))
    (should (string-match-p "interactive" output))))

(ert-deftest piem-fake-pi-test-cli-reports-missing-scenario-cleanly ()
  "The fake should name a missing scenario instead of showing a traceback."
  (let* ((result (piem-fake-pi-test--run-cli "--scenario" "does-not-exist"))
         (output (plist-get result :output)))
    (should-not (eq (plist-get result :exit-code) 0))
    (should (string-match-p "scenario not found: does-not-exist" output))
    (should-not (string-match-p "Traceback" output))))

(ert-deftest piem-fake-pi-test-cleans-session-root-on-exit ()
  "The fake removes its temporary session directory when the process exits."
  (piem-fake-pi-test-with-process (proc "prompt-lifecycle")
    (piem-fake-pi-test--send proc '(:type "get_state"))
    (let* ((response (piem-fake-pi-test--pop-object proc))
           (session-file (plist-get (plist-get response :data) :sessionFile))
           (session-root (directory-file-name (file-name-directory session-file))))
      (should (file-directory-p session-root))
      (process-send-eof proc)
      (should
       (piem-test-wait-until
        (lambda () (not (process-live-p proc)))
        piem-fake-pi-test--timeout
        0.01))
      (should-not (file-exists-p session-root)))))

(ert-deftest piem-fake-pi-test-get-commands-returns-configured-commands ()
  "get_commands returns the scenario's slash-command list." 
  (piem-fake-pi-test-with-process (proc "extension-confirm")
    (piem-fake-pi-test--send proc '(:type "get_commands"))
    (let* ((response (piem-fake-pi-test--pop-object proc))
           (commands (plist-get (plist-get response :data) :commands))
           (first (aref commands 0)))
      (should (eq (plist-get response :success) t))
      (should (vectorp commands))
      (should (equal (plist-get first :name) "test-confirm"))
      (should (equal (plist-get first :source) "extension")))))

(ert-deftest piem-fake-pi-test-set-model-and-thinking-level-update-state ()
  "set_model and set_thinking_level change subsequent get_state responses."
  (piem-fake-pi-test-with-process (proc "prompt-lifecycle")
    (piem-fake-pi-test--send
     proc '(:type "set_model" :provider "fake-provider" :modelId "fake-large"))
    (let ((model-response (piem-fake-pi-test--pop-object proc)))
      (should (eq (plist-get model-response :success) t))
      (should (equal (plist-get (plist-get model-response :data) :id) "fake-large")))
    (piem-fake-pi-test--send proc '(:type "set_thinking_level" :level "high"))
    (should (eq (plist-get (piem-fake-pi-test--pop-object proc) :success) t))
    (piem-fake-pi-test--send proc '(:type "get_state"))
    (let* ((state (piem-fake-pi-test--pop-object proc))
           (data (plist-get state :data))
           (model (plist-get data :model)))
      (should (equal (plist-get model :provider) "fake-provider"))
      (should (equal (plist-get model :id) "fake-large"))
      (should (equal (plist-get data :thinkingLevel) "high")))))

(ert-deftest piem-fake-pi-test-generated-session-is-valid-v3-with-entry-rpcs ()
  "A normal fake prompt persists valid v3 entries and exposes their raw IDs."
  (piem-fake-pi-test-with-process (proc "prompt-lifecycle")
    (let ((prompt "phase five generated session"))
      (let ((response
             (piem-fake-pi-test--rpc
              proc (list :id "generated-prompt" :type "prompt"
                         :message prompt))))
        (should (eq (plist-get response :success) t)))
      (piem-fake-pi-test--collect-until
       proc (lambda (object) (equal (plist-get object :type) "agent_end")))
      (let* ((state-response
              (piem-fake-pi-test--rpc
               proc '(:id "generated-state" :type "get_state")))
             (state (plist-get state-response :data))
             (session-file (plist-get state :sessionFile))
             (records
              (piem-fake-pi-test--read-jsonl-file session-file))
             (header (and (> (length records) 0) (aref records 0)))
             (entries (seq-subseq records 1))
             (fork-response
              (piem-fake-pi-test--rpc
               proc '(:id "generated-forks" :type "get_fork_messages")))
             (entries-response
              (piem-fake-pi-test--rpc
               proc '(:id "generated-entries" :type "get_entries"))))
        (should (eq (plist-get state-response :success) t))
        (should (file-exists-p session-file))
        (should (= (length entries) 2))
        (piem-fake-pi-test--assert-v3-header header)
        (should (equal (plist-get header :id) (plist-get state :sessionId)))
        (let ((previous-id nil))
          (dotimes (i (length entries))
            (let* ((entry (aref entries i))
                   (parent-id (plist-get entry :parentId)))
              (piem-fake-pi-test--assert-entry-base entry)
              (if (= i 0)
                  (should (piem--json-null-p parent-id))
                (should (equal parent-id previous-id)))
              (setq previous-id (plist-get entry :id)))))
        (let* ((user-entry
                (seq-find
                 (lambda (entry)
                   (and (equal (plist-get entry :type) "message")
                        (equal (plist-get (plist-get entry :message) :role)
                               "user")))
                 entries))
               (fork-messages
                (plist-get (plist-get fork-response :data) :messages)))
          (should user-entry)
          (should (eq (plist-get fork-response :success) t))
          (should
           (piem-fake-pi-test--json-equal-p
            fork-messages
            (vector (list :entryId (plist-get user-entry :id)
                          :text prompt)))))
        (should (eq (plist-get entries-response :success) t))
        (let ((data (plist-get entries-response :data)))
          (should (equal (plist-get data :entries) entries))
          (should (equal (plist-get data :leafId)
                         (plist-get (aref entries (1- (length entries)))
                                    :id))))))))

(ert-deftest piem-fake-pi-test-switch-session-roundtrips-branched-v3 ()
  "A switched branched v3 file preserves raw tree and projected history contracts."
  (let ((target-dir
         (file-name-as-directory
          (make-temp-file "piem-fake-pi-branched-" t))))
    (unwind-protect
        (let* ((fixture
                (piem-fake-pi-test--write-branched-v3-session
                 target-dir))
               (path (plist-get fixture :path))
               (entries (plist-get fixture :entries))
               (active-id (plist-get fixture :active-user-id))
               (leaf-id (plist-get fixture :post-assistant-id)))
          (piem-fake-pi-test-with-process (proc "prompt-lifecycle")
            (let ((switch-response
                   (piem-fake-pi-test--rpc
                    proc (list :id "branched-switch"
                               :type "switch_session"
                               :sessionPath path))))
              (should
               (piem-fake-pi-test--json-equal-p
                switch-response
                '(:id "branched-switch" :type "response"
                  :command "switch_session" :success t
                  :data (:cancelled :false)))))
            (let* ((state-response
                    (piem-fake-pi-test--rpc
                     proc '(:id "branched-state" :type "get_state")))
                   (state (plist-get state-response :data)))
              (should (eq (plist-get state-response :success) t))
              (should (equal (plist-get state :sessionFile) path))
              (should (equal (plist-get state :sessionId)
                             (plist-get fixture :session-id)))
              (should (equal (plist-get state :sessionName)
                             (plist-get fixture :session-name)))
              (should (= (plist-get state :messageCount)
                         (length (plist-get fixture :expected-messages)))))
            (let* ((response
                    (piem-fake-pi-test--rpc
                     proc '(:id "branched-entries" :type "get_entries")))
                   (data (plist-get response :data)))
              (should (eq (plist-get response :success) t))
              (should (equal (plist-get data :entries) entries))
              (should (equal (plist-get data :leafId) leaf-id)))
            (let* ((response
                    (piem-fake-pi-test--rpc
                     proc (list :id "branched-since" :type "get_entries"
                                :since active-id)))
                   (data (plist-get response :data)))
              (should (eq (plist-get response :success) t))
              ;; ACTIVE-ID is the third physical entry.  The abandoned sibling
              ;; follows it on disk even though it sorts above it in the tree.
              (should (equal (plist-get data :entries)
                             (seq-subseq entries 3)))
              (should (equal (plist-get data :leafId) leaf-id)))
            (let* ((response
                    (piem-fake-pi-test--rpc
                     proc '(:id "branched-tree" :type "get_tree")))
                   (data (plist-get response :data))
                   (tree (plist-get data :tree))
                   (nodes (piem-fake-pi-test--tree-nodes tree)))
              (should (eq (plist-get response :success) t))
              (should (equal (plist-get data :leafId) leaf-id))
              (should (= (length tree) 2))
              (should
               (equal
                (mapcar (lambda (root)
                          (plist-get (plist-get root :entry) :id))
                        (append tree nil))
                (list (plist-get fixture :root-user-id)
                      (plist-get fixture :orphan-id))))
              (should (= (length nodes) (length entries)))
              ;; Every raw bookkeeping entry remains a first-class tree node.
              (dotimes (i (length entries))
                (let* ((entry (aref entries i))
                       (node
                        (seq-find
                         (lambda (candidate)
                           (equal (plist-get
                                   (plist-get candidate :entry) :id)
                                  (plist-get entry :id)))
                         nodes)))
                  (should node)
                  (should
                   (piem-fake-pi-test--json-equal-p
                    (plist-get node :entry) entry))))
              (dolist (type '("branch_summary" "custom" "custom_message"
                              "thinking_level_change" "label" "session_info"
                              "compaction"))
                (should
                 (seq-find
                  (lambda (node)
                    (equal (plist-get (plist-get node :entry) :type) type))
                  nodes)))
              (should
               (= 4
                  (length
                   (seq-filter
                    (lambda (node)
                      (equal (plist-get (plist-get node :entry) :type)
                             "label"))
                    nodes))))
              (let* ((root-assistant-node
                      (seq-find
                       (lambda (node)
                         (equal (plist-get (plist-get node :entry) :id)
                                (plist-get fixture :root-assistant-id)))
                       nodes))
                     (child-ids
                      (mapcar
                       (lambda (node)
                         (plist-get (plist-get node :entry) :id))
                       (append (plist-get root-assistant-node :children) nil))))
                (should
                 (equal child-ids
                        (list (plist-get fixture :abandoned-user-id)
                              active-id))))
              (let ((active-node
                     (seq-find
                      (lambda (node)
                        (equal (plist-get (plist-get node :entry) :id)
                               active-id))
                      nodes)))
                (should (equal (plist-get active-node :label)
                               (plist-get fixture :latest-label)))
                (should (equal (plist-get active-node :labelTimestamp)
                               (plist-get fixture
                                          :latest-label-timestamp))))
              (let ((cleared-node
                     (seq-find
                      (lambda (node)
                        (equal (plist-get (plist-get node :entry) :id)
                               (plist-get fixture :root-assistant-id)))
                      nodes)))
                (should-not (plist-member cleared-node :label))
                (should-not (plist-member cleared-node :labelTimestamp))))
            (let* ((response
                    (piem-fake-pi-test--rpc
                     proc '(:id "branched-messages" :type "get_messages")))
                   (messages
                    (plist-get (plist-get response :data) :messages)))
              (should (eq (plist-get response :success) t))
              (should
               (equal (mapcar (lambda (message) (plist-get message :role))
                              (append messages nil))
                      '("compactionSummary" "user" "branchSummary"
                        "custom" "assistant")))
              (should
               (piem-fake-pi-test--json-equal-p
                messages (plist-get fixture :expected-messages)))
              (let ((printed (prin1-to-string messages)))
                (should-not (string-match-p "ABANDONED USER CONTENT" printed))
                (should-not
                 (string-match-p "ABANDONED ASSISTANT CONTENT" printed))))
            (let* ((response
                    (piem-fake-pi-test--rpc
                     proc '(:id "branched-forks"
                            :type "get_fork_messages")))
                   (messages
                    (plist-get (plist-get response :data) :messages)))
              (should (eq (plist-get response :success) t))
              (should
               (piem-fake-pi-test--json-equal-p
                messages
                (vector
                 (list :entryId (plist-get fixture :root-user-id)
                       :text "ROOT USER CONTENT")
                 (list :entryId active-id
                       :text "ACTIVE RETAINED USER CONTENT")
                 (list :entryId (plist-get fixture :abandoned-user-id)
                       :text "ABANDONED USER CONTENT")))))
            (let ((response
                   (piem-fake-pi-test--rpc
                    proc '(:id "rename-after-switch"
                           :type "set_session_name"
                           :name "  Renamed\r\nafter switch  "))))
              (should (eq (plist-get response :success) t)))
            (let* ((response
                    (piem-fake-pi-test--rpc
                     proc '(:id "entries-after-rename" :type "get_entries")))
                   (data (plist-get response :data))
                   (renamed-entries (plist-get data :entries))
                   (name-entry (aref renamed-entries
                                     (1- (length renamed-entries)))))
              (should (= (length renamed-entries) (1+ (length entries))))
              (should (equal (plist-get name-entry :type) "session_info"))
              (should (equal (plist-get name-entry :parentId) leaf-id))
              (should (equal (plist-get name-entry :name)
                             "Renamed after switch"))
              (should (equal (plist-get data :leafId)
                             (plist-get name-entry :id))))
            (let* ((state-response
                    (piem-fake-pi-test--rpc
                     proc '(:id "state-after-rename" :type "get_state")))
                   (state (plist-get state-response :data)))
              (should (equal (plist-get state :sessionName)
                             "Renamed after switch"))
              ;; session_info advances the raw leaf but projects no message.
              (should (= (plist-get state :messageCount)
                         (length (plist-get fixture :expected-messages)))))))
      (delete-directory target-dir t))))

(ert-deftest piem-fake-pi-test-switch-failures-are-transactional-and-missing-initializes ()
  "Invalid switches preserve state; a missing absolute target becomes empty v3."
  (let* ((target-dir
          (file-name-as-directory
           (make-temp-file "piem-fake-pi-switch-" t)))
         (malformed-path (expand-file-name "malformed.jsonl" target-dir))
         (missing-path (expand-file-name "new-empty.jsonl" target-dir)))
    (unwind-protect
        (progn
          (with-temp-file malformed-path
            (insert "this is not a pi session\n"))
          (piem-fake-pi-test-with-process
              (proc "extension-confirm" "--extension-timeout-ms" "0")
            (should
             (eq (plist-get
                  (piem-fake-pi-test--rpc
                   proc '(:id "blocking-prompt" :type "prompt"
                          :message "/test-confirm"))
                  :success)
                 t))
            (should (equal (plist-get
                            (piem-fake-pi-test--pop-object proc)
                            :type)
                           "agent_start"))
            (should (equal (plist-get
                            (piem-fake-pi-test--pop-object proc)
                            :type)
                           "extension_ui_request"))
            (let* ((baseline-response
                    (piem-fake-pi-test--rpc
                     proc '(:id "baseline-state" :type "get_state")))
                   (baseline (plist-get baseline-response :data))
                   (invalid-targets
                    (list (cons "non-string" 17)
                          (cons "directory" target-dir)
                          (cons "malformed" malformed-path))))
              (should (eq (plist-get baseline-response :success) t))
              (should (eq (plist-get baseline :isStreaming) t))
              (dolist (case invalid-targets)
                (let* ((label (car case))
                       (response
                        (piem-fake-pi-test--rpc
                         proc (list :id (concat "invalid-" label)
                                    :type "switch_session"
                                    :sessionPath (cdr case)))))
                  (should (piem--json-false-p
                           (plist-get response :success)))
                  (should (stringp (plist-get response :error)))
                  (should-not (plist-member response :data))
                  (let ((after
                         (piem-fake-pi-test--rpc
                          proc (list :id (concat "state-after-" label)
                                     :type "get_state"))))
                    (should (eq (plist-get after :success) t))
                    ;; Validation failure neither retargets nor stops the
                    ;; extension worker waiting for its response.
                    (should
                     (piem-fake-pi-test--json-equal-p
                      (plist-get after :data) baseline)))))
              (should-not (file-exists-p missing-path))
              (piem-fake-pi-test--send
               proc (list :id "missing-switch"
                          :type "switch_session"
                          :sessionPath missing-path))
              (let* ((events
                      (piem-fake-pi-test--collect-until
                       proc
                       (lambda (event)
                         (and (equal (plist-get event :type) "response")
                              (equal (plist-get event :id) "missing-switch")))))
                     (response (car (last events)))
                     (agent-end
                      (seq-find (lambda (event)
                                  (equal (plist-get event :type) "agent_end"))
                                events)))
                (should agent-end)
                (should (< (seq-position events agent-end #'eq)
                           (seq-position events response #'eq)))
                (should
                 (piem-fake-pi-test--json-equal-p
                  response
                  '(:id "missing-switch" :type "response"
                    :command "switch_session" :success t
                    :data (:cancelled :false)))))
              (should (file-exists-p missing-path))
              (let* ((records
                      (piem-fake-pi-test--read-jsonl-file
                       missing-path))
                     (header (and (= (length records) 1)
                                  (aref records 0)))
                     (state-response
                      (piem-fake-pi-test--rpc
                       proc '(:id "missing-state" :type "get_state")))
                     (state (plist-get state-response :data)))
                (should (= (length records) 1))
                (piem-fake-pi-test--assert-v3-header header)
                (should (eq (plist-get state-response :success) t))
                (should (equal (plist-get state :sessionFile) missing-path))
                (should (equal (plist-get state :sessionId)
                               (plist-get header :id)))
                (should (= (plist-get state :messageCount) 0))
                (should (eq (plist-get state :isStreaming) :false))
                (should-not (plist-member state :sessionName)))
              (let ((entries-response
                     (piem-fake-pi-test--rpc
                      proc '(:id "empty-entries" :type "get_entries")))
                    (tree-response
                     (piem-fake-pi-test--rpc
                      proc '(:id "empty-tree" :type "get_tree")))
                    (messages-response
                     (piem-fake-pi-test--rpc
                      proc '(:id "empty-messages" :type "get_messages"))))
                (should
                 (piem-fake-pi-test--json-equal-p
                  entries-response
                  '(:id "empty-entries" :type "response"
                    :command "get_entries" :success t
                    :data (:entries [] :leafId :null))))
                (should
                 (piem-fake-pi-test--json-equal-p
                  tree-response
                  '(:id "empty-tree" :type "response"
                    :command "get_tree" :success t
                    :data (:tree [] :leafId :null))))
                (should
                 (piem-fake-pi-test--json-equal-p
                  messages-response
                  '(:id "empty-messages" :type "response"
                    :command "get_messages" :success t
                    :data (:messages []))))))))
      (delete-directory target-dir t))))

(ert-deftest piem-fake-pi-test-resume-selected-session-full-contract ()
  "The retained resume choreography settles and renders fake target history."
  (let* ((target-dir
          (file-name-as-directory
           (make-temp-file "piem-fake-pi-resume-" t)))
         (fixture
          (piem-fake-pi-test--write-branched-v3-session target-dir))
         (target-path (plist-get fixture :path))
         (log-file (expand-file-name "fake-rpc.log" target-dir)))
    (unwind-protect
        (piem-fake-pi-test-with-session
            (session "extension-confirm" "--log-file" log-file)
          (let* ((chat-buf (plist-get session :chat-buffer))
                 (proc (plist-get session :process)))
            (piem-fake-pi-test--wait-or-fail
             proc
             (lambda ()
               (with-current-buffer chat-buf
                 (and (plist-get piem--state :session-id)
                      (not (piem--session-transition-active-p))
                      (seq-find
                       (lambda (command)
                         (equal (plist-get command :name) "test-confirm"))
                       piem--commands))))
             "initial fake state and commands")
            ;; Make the post-switch command refresh observable rather than
            ;; inheriting the startup command cache.
            (with-current-buffer chat-buf
              (setq piem--commands nil)
              (piem--resume-selected-session
               proc chat-buf target-path))
            (piem-fake-pi-test--wait-or-fail
             proc
             (lambda ()
               (with-current-buffer chat-buf
                 (not (piem--session-transition-active-p))))
             "resume transition settlement")
            (let* ((log-records
                    (piem-fake-pi-test--read-jsonl-file log-file))
                   (types
                    (piem-fake-pi-test--logged-input-types-after-switch
                     log-records))
                   (required
                    '("switch_session" "get_state" "get_messages"
                      "get_commands"))
                   (observed
                    (seq-filter (lambda (type) (member type required)) types)))
              ;; Ignore get_session_stats: history refresh may request it for
              ;; the header, but these four retained calls are the transition.
              (should (equal observed required)))
            (piem-fake-pi-test--wait-or-fail
             proc
             (lambda ()
               (with-current-buffer chat-buf
                 (seq-find
                  (lambda (command)
                    (equal (plist-get command :name) "test-confirm"))
                  piem--commands)))
             "post-resume get_commands refresh" 2)
            (with-current-buffer chat-buf
              (should (equal (plist-get piem--state :session-id)
                             (plist-get fixture :session-id)))
              (should (equal (plist-get piem--state :session-file)
                             target-path))
              (should (= (plist-get piem--state :message-count)
                         (length (plist-get fixture :expected-messages))))
              (should (equal piem--session-name
                             (plist-get fixture :session-name)))
              (should
               (piem-fake-pi-test--json-equal-p
                piem--canonical-messages
                (plist-get fixture :expected-messages)))
              (let* ((text (buffer-substring-no-properties
                            (point-min) (point-max)))
                     (heading-pos
                      (string-match "^Branch Summary · [^\n]+\n=+\n" text))
                     (summary-pos
                      (string-match
                       (regexp-quote (plist-get fixture :branch-summary))
                       text))
                     (assistant-pos
                      (string-match "POST-COMPACTION ASSISTANT CONTENT" text)))
                (should heading-pos)
                (should summary-pos)
                (should assistant-pos)
                (should (< heading-pos summary-pos assistant-pos))
                (should
                 (= 1
                    (piem-test--count-matches
                     "^Branch Summary · " text)))))))
      (delete-directory target-dir t))))

(ert-deftest piem-fake-pi-test-session-starts-through-emacs-seam ()
  "The fake works through `piem' startup and rendering paths." 
  (piem-fake-pi-test-with-session (session "prompt-lifecycle")
    (let* ((chat-buf (plist-get session :chat-buffer))
           (input-buf (plist-get session :input-buffer)))
      (with-current-buffer input-buf
        (erase-buffer)
        (insert "hello seam")
        (piem-send))
      (should
       (piem-test-wait-until
        (lambda ()
          (with-current-buffer chat-buf
            (string-match-p "Fake reply for: hello seam"
                            (buffer-string))))
        piem-fake-pi-test--timeout
        0.01
        (plist-get session :process)))
      (with-current-buffer chat-buf
        (should (file-exists-p (plist-get piem--state :session-file)))))))

(ert-deftest piem-fake-pi-test-prompt-image-persists-canonical-content ()
  "A UI-attached PNG survives the fake prompt and canonical history contract."
  (let* ((dir (make-temp-file "piem-fake-pi-image-" t))
         (path (piem-test--write-prompt-image
                (expand-file-name "pixel.png" dir) 'png))
         (data (piem-test--prompt-image-base64 'png))
         (text "Describe this fake-contract pixel"))
    (unwind-protect
        (piem-fake-pi-test-with-session
            (session "prompt-lifecycle")
          (let ((chat-buf (plist-get session :chat-buffer))
                (input-buf (plist-get session :input-buffer))
                (proc (plist-get session :process)))
            (piem-fake-pi-test--wait-or-fail
             proc
             (lambda ()
               (piem--model-supports-image-input-p chat-buf))
             "vision model state")
            (with-current-buffer input-buf
              (erase-buffer)
              (insert text)
              (cl-letf (((symbol-function 'read-file-name)
                         (lambda (&rest _) path)))
                (call-interactively #'piem-attach-image))
              (delete-file path)
              (piem-send))
            (piem-fake-pi-test--wait-or-fail
             proc
             (lambda ()
               (with-current-buffer chat-buf
                 (and (eq piem--status 'idle)
                      (not (piem--prompt-start-wait-active-p))
                      (string-match-p "Fake reply for:" (buffer-string)))))
             "image prompt settlement")
            (with-current-buffer chat-buf
              (should (string-match-p "Image: image/png" (buffer-string))))
            (let* ((response
                    (piem--rpc-sync
                     proc '(:type "get_messages")
                     piem-fake-pi-test--timeout))
                   (messages (plist-get (plist-get response :data) :messages))
                   (user (seq-find
                          (lambda (message)
                            (equal (plist-get message :role) "user"))
                          (append messages nil))))
              (should (eq (plist-get response :success) t))
              (should
               (equal (plist-get user :content)
                      (vector (list :type "text" :text text)
                              (list :type "image" :data data
                                    :mimeType "image/png")))))))
      (delete-directory dir t))))

(ert-deftest piem-fake-pi-test-extension-confirm-displays-through-emacs-seam ()
  "An extension confirm round-trip renders the follow-up message in chat." 
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
    (piem-fake-pi-test-with-session
        (session "extension-confirm" "--extension-timeout-ms" "500")
      (let* ((chat-buf (plist-get session :chat-buffer))
             (input-buf (plist-get session :input-buffer)))
        (with-current-buffer input-buf
          (erase-buffer)
          (insert "/test-confirm")
          (piem-send))
        (should
         (piem-test-wait-until
          (lambda ()
            (with-current-buffer chat-buf
              (string-match-p "CONFIRMED" (buffer-string))))
          piem-fake-pi-test--timeout
          0.01
          (plist-get session :process)))))))

(ert-deftest piem-fake-pi-test-custom-message-command-emits-visible-message ()
  "A custom-message command emits visible custom message events."
  (piem-fake-pi-test-with-process (proc "extension-message")
    (piem-fake-pi-test--send proc '(:type "prompt" :message "/test-message"))
    (let ((response (piem-fake-pi-test--pop-object proc)))
      (should (eq (plist-get response :success) t))
      (should (equal (plist-get response :command) "prompt")))
    (let* ((start (piem-fake-pi-test--pop-object proc))
           (message (plist-get start :message)))
      (should (equal (plist-get start :type) "message_start"))
      (should (equal (plist-get message :role) "custom"))
      (should (eq (plist-get message :display) t))
      (should (equal (plist-get message :content) "Test message from extension")))
    (let* ((end (piem-fake-pi-test--pop-object proc))
           (message (plist-get end :message)))
      (should (equal (plist-get end :type) "message_end"))
      (should (equal (plist-get message :role) "custom")))))

(ert-deftest piem-fake-pi-test-custom-noop-command-skips-message-events ()
  "A no-op custom-message command returns without emitting display events."
  (piem-fake-pi-test-with-process (proc "extension-noop")
    (piem-fake-pi-test--send proc '(:type "prompt" :message "/test-noop"))
    (let ((response (piem-fake-pi-test--pop-object proc)))
      (should (eq (plist-get response :success) t))
      (should (equal (plist-get response :command) "prompt")))
    (piem-fake-pi-test--send proc '(:type "get_state"))
    (let ((response (piem-fake-pi-test--pop-object proc)))
      (should (equal (plist-get response :command) "get_state"))
      (should (eq (plist-get response :success) t)))))

(ert-deftest piem-fake-pi-test-prompt-response-precedes-stream-events ()
  "prompt returns success first, then streams lifecycle events."
  (piem-fake-pi-test-with-process (proc "prompt-lifecycle")
    (piem-fake-pi-test--send proc '(:type "prompt" :message "hello fake pi"))
    (let* ((response (piem-fake-pi-test--pop-object proc))
           (events (piem-fake-pi-test--collect-until
                    proc
                    (lambda (obj) (equal (plist-get obj :type) "agent_end"))))
           (assistant-start (seq-find
                             (lambda (obj)
                               (and (equal (plist-get obj :type) "message_start")
                                    (equal (plist-get (plist-get obj :message) :role)
                                           "assistant")))
                             events))
           (message-updates
            (piem-fake-pi-test--events-of-type events "message_update"))
           (text-deltas
            (piem-fake-pi-test--message-updates events "text_delta")))
      (should (equal (plist-get response :type) "response"))
      (should (eq (plist-get response :success) t))
      (should (equal (plist-get response :command) "prompt"))
      (should (equal (car (piem-fake-pi-test--event-types events)) "agent_start"))
      (should assistant-start)
      (should (> (length text-deltas) 0))
      (dolist (update message-updates)
        (should (plist-member update :usage))
        (should-not (plist-member update :message))
        (should-not (plist-member (plist-get update :assistantMessageEvent)
                                  :partial)))
      (should (equal (car (last (piem-fake-pi-test--event-types events)))
                     "agent_end")))))

(ert-deftest piem-fake-pi-test-tool-stream-emits-tool-events ()
  "tool_stream emits an ordered, correlated, delta-only RPC lifecycle."
  (piem-fake-pi-test-with-process (proc "tool-read")
    (piem-fake-pi-test--send proc '(:type "prompt" :message "use the tool"))
    (should (equal (plist-get (piem-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (let* ((events (piem-fake-pi-test--collect-until
                    proc
                    (lambda (obj) (equal (plist-get obj :type) "agent_end"))))
           (assistant-starts
            (piem-fake-pi-test--message-events
             events "message_start" "assistant"))
           (assistant-ends
            (piem-fake-pi-test--message-events
             events "message_end" "assistant"))
           (tool-result-start
            (car (piem-fake-pi-test--message-events
                  events "message_start" "toolResult")))
           (tool-result-end
            (car (piem-fake-pi-test--message-events
                  events "message_end" "toolResult")))
           (message-updates
            (piem-fake-pi-test--events-of-type events "message_update"))
           (toolcall-start-update
            (car (piem-fake-pi-test--message-updates
                  events "toolcall_start")))
           (toolcall-delta-updates
            (piem-fake-pi-test--message-updates events "toolcall_delta"))
           (toolcall-end-update
            (car (piem-fake-pi-test--message-updates
                  events "toolcall_end")))
           (text-delta-updates
            (piem-fake-pi-test--message-updates events "text_delta"))
           (tool-execution-start
            (car (piem-fake-pi-test--events-of-type
                  events "tool_execution_start")))
           (tool-execution-update
            (car (piem-fake-pi-test--events-of-type
                  events "tool_execution_update")))
           (tool-execution-end
            (car (piem-fake-pi-test--events-of-type
                  events "tool_execution_end")))
           (agent-end (car (last events)))
           (lifecycle
            (mapcar
             (lambda (obj)
               (pcase (plist-get obj :type)
                 ((or "message_start" "message_end")
                  (format "%s:%s"
                          (plist-get obj :type)
                          (plist-get (plist-get obj :message) :role)))
                 ("message_update"
                  (plist-get (plist-get obj :assistantMessageEvent) :type))
                 (type type)))
             events)))
      (should
       (equal lifecycle
              '("agent_start"
                "message_start:user" "message_end:user"
                "message_start:assistant"
                "toolcall_start"
                "toolcall_delta" "toolcall_delta" "toolcall_delta"
                "toolcall_end"
                "message_end:assistant"
                "tool_execution_start" "tool_execution_update"
                "tool_execution_end"
                "message_start:toolResult" "message_end:toolResult"
                "message_start:assistant"
                "text_delta" "text_delta"
                "message_end:assistant"
                "agent_end")))
      (should (> (length toolcall-delta-updates) 1))
      (should (> (length text-delta-updates) 0))
      (dolist (update message-updates)
        (should (plist-member update :usage))
        (should-not (plist-member update :message))
        (should-not (plist-member (plist-get update :assistantMessageEvent)
                                  :partial)))
      (let* ((first-assistant-end (car assistant-ends))
             (second-assistant-end (cadr assistant-ends))
             (toolcall-start-event
              (plist-get toolcall-start-update :assistantMessageEvent))
             (toolcall-end-event
              (plist-get toolcall-end-update :assistantMessageEvent))
             (call-id (plist-get toolcall-start-event :id))
             (streamed-arguments
              (mapconcat
               (lambda (update)
                 (plist-get (plist-get update :assistantMessageEvent) :delta))
               toolcall-delta-updates
               ""))
             (tool-call (plist-get toolcall-end-event :toolCall))
             (tool-assistant-message (plist-get first-assistant-end :message))
             (tool-call-from-message
              (aref (plist-get tool-assistant-message :content) 0))
             (tool-result-message (plist-get tool-result-start :message))
             (final-message (plist-get second-assistant-end :message))
             (agent-messages (plist-get agent-end :messages)))
        (dolist (start assistant-starts)
          (should (= (length (plist-get (plist-get start :message) :content)) 0))
          (should (equal (plist-get (plist-get start :message) :stopReason)
                         "pending")))
        (should (and (stringp call-id) (> (length call-id) 0)))
        (should (= (plist-get toolcall-start-event :contentIndex) 0))
        (should (equal (plist-get toolcall-start-event :toolName) "read"))
        (should (= (plist-get toolcall-end-event :contentIndex) 0))
        (dolist (update toolcall-delta-updates)
          (should (= (plist-get (plist-get update :assistantMessageEvent)
                                :contentIndex)
                     0)))
        (should (equal (piem--parse-json-line streamed-arguments)
                       '(:path "/tmp/fake-tool.txt")))
        (should (equal tool-call tool-call-from-message))
        (should (equal (plist-get tool-assistant-message :stopReason) "toolUse"))
        (should (equal (plist-get tool-call :id) call-id))
        (should (equal (plist-get tool-call :name) "read"))
        (should (equal (plist-get (plist-get tool-call :arguments) :path)
                       "/tmp/fake-tool.txt"))
        (dolist (event (list tool-execution-start
                             tool-execution-update
                             tool-execution-end))
          (should (equal (plist-get event :toolCallId) call-id))
          (should (equal (plist-get event :toolName) "read")))
        (should (equal (plist-get (plist-get tool-execution-start :args) :path)
                       "/tmp/fake-tool.txt"))
        (should (equal (plist-get (aref (plist-get (plist-get tool-execution-update
                                                               :partialResult)
                                                   :content)
                                          0)
                                  :text)
                       "fake tool output\n"))
        (should (equal (plist-get tool-execution-end :isError) :false))
        (should (equal (plist-get tool-result-end :message) tool-result-message))
        (should (equal (plist-get tool-result-message :toolCallId) call-id))
        (should (equal (plist-get tool-result-message :toolName) "read"))
        (should (equal (plist-get tool-result-message :isError) :false))
        (should (equal (plist-get tool-result-message :content)
                       (plist-get (plist-get tool-execution-end :result) :content)))
        (should (equal (plist-get tool-result-message :details)
                       (plist-get (plist-get tool-execution-end :result) :details)))
        (should (equal (plist-get (aref (plist-get tool-result-message :content) 0)
                                  :text)
                       "fake tool output\nmore output\n"))
        (should (equal (mapconcat
                        (lambda (update)
                          (plist-get (plist-get update :assistantMessageEvent) :delta))
                        text-delta-updates
                        "")
                       "Tool finished"))
        (should (equal (plist-get final-message :stopReason) "stop"))
        (should (equal (plist-get (aref (plist-get final-message :content) 0) :text)
                       "Tool finished"))
        (should (equal (mapcar (lambda (message) (plist-get message :role))
                               agent-messages)
                       '("user" "assistant" "toolResult" "assistant")))
        (should (equal (plist-get (aref agent-messages 1) :content)
                       (plist-get tool-assistant-message :content)))
        (should (equal (plist-get (aref agent-messages 2) :toolCallId) call-id))
        (should (equal (plist-get agent-end :willRetry) :false))))))

(ert-deftest piem-fake-pi-test-abort-stops-streaming ()
  "abort stops an in-flight prompt and leaves the fake idle."
  (piem-fake-pi-test-with-process (proc "prompt-lifecycle")
    (piem-fake-pi-test--send proc '(:type "prompt" :message "abort me"))
    (should (equal (plist-get (piem-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (let ((seen-first-delta nil)
          (agent-end nil)
          (aborted-message nil)
          (saw-stop-message-end nil)
          (saw-abort-response nil))
      (while (not seen-first-delta)
        (let* ((obj (piem-fake-pi-test--pop-object proc))
               (event-type (plist-get obj :type))
               (msg-event (plist-get obj :assistantMessageEvent)))
          (when (and (equal event-type "message_update")
                     (equal (plist-get msg-event :type) "text_delta"))
            (setq seen-first-delta t))))
      (piem-fake-pi-test--send proc '(:type "abort"))
      (while (not (and saw-abort-response agent-end))
        (let ((obj (piem-fake-pi-test--pop-object proc)))
          (pcase (plist-get obj :type)
            ("response"
             (when (equal (plist-get obj :command) "abort")
               (setq saw-abort-response (eq (plist-get obj :success) t))))
            ("message_end"
             (pcase (plist-get (plist-get obj :message) :stopReason)
               ("aborted" (setq aborted-message (plist-get obj :message)))
               ("stop" (setq saw-stop-message-end t))))
            ("agent_end"
             (setq agent-end obj)))))
      (should saw-abort-response)
      (should aborted-message)
      (should (equal (plist-get aborted-message :errorMessage)
                     "Request was aborted"))
      (should-not saw-stop-message-end)
      (let ((messages (plist-get agent-end :messages)))
        (should (equal (aref messages (1- (length messages)))
                       aborted-message)))
      (piem-fake-pi-test--send proc '(:type "get_state"))
      (let* ((state (piem-fake-pi-test--pop-object proc))
             (data (plist-get state :data)))
        (should (eq (plist-get data :isStreaming) :false))))))

(ert-deftest piem-fake-pi-test-tool-abort-ends-partial-assistant-message ()
  "Aborting tool generation emits its authoritative partial message first."
  (piem-fake-pi-test-with-process (proc "tool-abort")
    (piem-fake-pi-test--send proc
                                        '(:type "prompt" :message "abort tool"))
    (should (equal (plist-get (piem-fake-pi-test--pop-object proc)
                              :command)
                   "prompt"))
    (let ((events nil)
          (delta-count 0)
          (abort-response nil)
          (agent-end nil))
      (while (< delta-count 3)
        (let ((event (piem-fake-pi-test--pop-object proc)))
          (push event events)
          (when (and (equal (plist-get event :type) "message_update")
                     (equal (plist-get
                             (plist-get event :assistantMessageEvent) :type)
                            "toolcall_delta"))
            (setq delta-count (1+ delta-count)))))
      (piem-fake-pi-test--send proc '(:type "abort"))
      (while (not (and abort-response agent-end))
        (let ((event (piem-fake-pi-test--pop-object proc)))
          (push event events)
          (pcase (plist-get event :type)
            ("response"
             (when (equal (plist-get event :command) "abort")
               (setq abort-response event)))
            ("agent_end"
             (setq agent-end event)))))
      (setq events (nreverse events))
      (let* ((aborted-end
              (seq-find
               (lambda (event)
                 (and (equal (plist-get event :type) "message_end")
                      (equal (plist-get (plist-get event :message) :stopReason)
                             "aborted")))
               events))
             (aborted-message (and aborted-end
                                   (plist-get aborted-end :message)))
             (messages (plist-get agent-end :messages)))
        (should (eq (plist-get abort-response :success) t))
        (should aborted-message)
        (should (equal (plist-get aborted-message :errorMessage)
                       "Request was aborted"))
        (should (equal (plist-get (aref (plist-get aborted-message :content) 0)
                                  :name)
                       "read"))
        (should (< (seq-position events aborted-end #'eq)
                   (seq-position events agent-end #'eq)))
        (should-not
         (seq-find
          (lambda (event)
            (or (equal (plist-get event :type) "tool_execution_start")
                (and (equal (plist-get event :type) "message_update")
                     (equal (plist-get
                             (plist-get event :assistantMessageEvent) :type)
                            "toolcall_end"))))
          events))
        (should (equal (aref messages (1- (length messages)))
                       aborted-message))))))

(ert-deftest piem-fake-pi-test-steer-queues-another-turn ()
  "steer queues another user turn and delivers it before agent_end."
  (piem-fake-pi-test-with-process (proc "prompt-lifecycle")
    (piem-fake-pi-test--send proc '(:type "prompt" :message "first turn"))
    (should (equal (plist-get (piem-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (let ((seen-first-delta nil)
          (saw-steer-response nil)
          (saw-agent-end nil)
          (user-starts 0)
          (steered-reply nil))
      (while (not seen-first-delta)
        (let* ((obj (piem-fake-pi-test--pop-object proc))
               (msg-event (plist-get obj :assistantMessageEvent)))
          (when (and (equal (plist-get obj :type) "message_start")
                     (equal (plist-get (plist-get obj :message) :role) "user"))
            (setq user-starts (1+ user-starts)))
          (when (and (equal (plist-get obj :type) "message_update")
                     (equal (plist-get msg-event :type) "text_delta"))
            (setq seen-first-delta t))))
      (piem-fake-pi-test--send proc '(:type "steer" :message "second turn"))
      (while (not saw-agent-end)
        (let ((obj (piem-fake-pi-test--pop-object proc)))
          (pcase (plist-get obj :type)
            ("response"
             (when (equal (plist-get obj :command) "steer")
               (setq saw-steer-response (eq (plist-get obj :success) t))))
            ("message_start"
             (when (equal (plist-get (plist-get obj :message) :role) "user")
               (setq user-starts (1+ user-starts))))
            ("message_end"
             (let ((message (plist-get obj :message)))
               (when (and (equal (plist-get message :role) "assistant")
                          (string-match-p "Steered fake reply for: second turn"
                                          (or (plist-get (aref (plist-get message :content) 0)
                                                         :text)
                                              "")))
                 (setq steered-reply t))))
            ("agent_end"
             (setq saw-agent-end t)))))
      (should saw-steer-response)
      (should saw-agent-end)
      (should (= user-starts 2))
      (should steered-reply)
      (piem-fake-pi-test--send proc '(:type "get_fork_messages"))
      (let* ((fork-response (piem-fake-pi-test--pop-object proc))
             (messages (plist-get (plist-get fork-response :data) :messages)))
        (should (= (length messages) 2))
        (should (equal (plist-get (aref messages 1) :text) "second turn"))))))

(ert-deftest piem-fake-pi-test-new-session-resets-count-and-path ()
  "new_session resets state and returns a fresh real session file path."
  (piem-fake-pi-test-with-process (proc "prompt-lifecycle")
    (piem-fake-pi-test--send proc '(:type "prompt" :message "before reset"))
    (should (equal (plist-get (piem-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (piem-fake-pi-test--collect-until
     proc (lambda (obj) (equal (plist-get obj :type) "agent_end")))
    (piem-fake-pi-test--send proc '(:type "get_state"))
    (let* ((before-state (piem-fake-pi-test--pop-object proc))
           (before-data (plist-get before-state :data))
           (before-file (plist-get before-data :sessionFile)))
      (should (> (plist-get before-data :messageCount) 0))
      (piem-fake-pi-test--send proc '(:type "new_session"))
      (let ((response (piem-fake-pi-test--pop-object proc)))
        (should (eq (plist-get response :success) t))
        (should (eq (plist-get (plist-get response :data) :cancelled) :false)))
      (piem-fake-pi-test--send proc '(:type "get_state"))
      (let* ((after-state (piem-fake-pi-test--pop-object proc))
             (after-data (plist-get after-state :data))
             (after-file (plist-get after-data :sessionFile)))
        (should (equal (plist-get after-data :messageCount) 0))
        (should (not (equal after-file before-file)))
        (should (file-exists-p after-file))))))

(ert-deftest piem-fake-pi-test-new-session-waits-for-old-run-to-stop ()
  "new_session should not leak stale streaming events after it succeeds."
  (piem-fake-pi-test-with-process (proc "prompt-lifecycle")
    (piem-fake-pi-test--send proc '(:type "prompt" :message "before reset"))
    (should (equal (plist-get (piem-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (let ((seen-first-delta nil)
          (new-session-response nil))
      (while (not seen-first-delta)
        (let* ((obj (piem-fake-pi-test--pop-object proc))
               (msg-event (plist-get obj :assistantMessageEvent)))
          (when (and (equal (plist-get obj :type) "message_update")
                     (equal (plist-get msg-event :type) "text_delta"))
            (setq seen-first-delta t))))
      (piem-fake-pi-test--send proc '(:type "new_session"))
      (while (not new-session-response)
        (let ((obj (piem-fake-pi-test--pop-object proc)))
          (when (and (equal (plist-get obj :type) "response")
                     (equal (plist-get obj :command) "new_session"))
            (setq new-session-response obj))))
      (sleep-for 0.2)
      (should-not (process-get proc 'fake-pi-objects))
      (piem-fake-pi-test--send proc '(:type "get_state"))
      (let* ((state (piem-fake-pi-test--pop-object proc))
             (data (plist-get state :data)))
        (should (equal (plist-get data :messageCount) 0))
        (should (eq (plist-get data :isStreaming) :false))))))

(ert-deftest piem-fake-pi-test-set-session-name-writes-session-info ()
  "set_session_name appends a real session_info entry that Emacs can parse."
  (piem-fake-pi-test-with-process (proc "prompt-lifecycle")
    (piem-fake-pi-test--send proc '(:type "prompt" :message "session me"))
    (should (equal (plist-get (piem-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (piem-fake-pi-test--collect-until
     proc (lambda (obj) (equal (plist-get obj :type) "agent_end")))
    (piem-fake-pi-test--send proc '(:type "get_state"))
    (let* ((state (piem-fake-pi-test--pop-object proc))
           (session-file (plist-get (plist-get state :data) :sessionFile)))
      (piem-fake-pi-test--send
       proc '(:type "set_session_name" :name "  Fake\r\nHarness Session  "))
      (let ((response (piem-fake-pi-test--pop-object proc)))
        (should (eq (plist-get response :success) t)))
      (with-temp-buffer
        (insert-file-contents session-file)
        (should (string-match-p "session_info" (buffer-string)))
        (should (string-match-p "Fake Harness Session" (buffer-string))))
      (let ((metadata
             (piem-jsonl-read-session-info session-file)))
        (should metadata)
        (should (equal (plist-get metadata :name)
                       "Fake Harness Session"))))))

(ert-deftest piem-fake-pi-test-extension-confirm-zero-timeout-disables-expiry ()
  "An override of 0 disables dialog expiry for manual debugging." 
  (piem-fake-pi-test-with-process
      (proc "extension-confirm" "--extension-timeout-ms" "0")
    (piem-fake-pi-test--send proc '(:type "prompt" :message "/test-confirm"))
    (should (equal (plist-get (piem-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (should (equal (plist-get (piem-fake-pi-test--pop-object proc) :type)
                   "agent_start"))
    (let ((request (piem-fake-pi-test--pop-object proc)))
      (should (equal (plist-get request :type) "extension_ui_request"))
      (should-not (plist-member request :timeout))
      (sleep-for 0.2)
      (should-not (process-get proc 'fake-pi-objects))
      (piem-fake-pi-test--send
       proc
       (list :type "extension_ui_response"
             :id (plist-get request :id)
             :confirmed t))
      (let* ((events (piem-fake-pi-test--collect-until
                      proc
                      (lambda (obj) (equal (plist-get obj :type) "agent_end"))))
             (custom-end (seq-find
                          (lambda (obj)
                            (and (equal (plist-get obj :type) "message_end")
                                 (equal (plist-get (plist-get obj :message) :content)
                                        "CONFIRMED")))
                          events)))
        (should custom-end)))))

(ert-deftest piem-fake-pi-test-extension-confirm-honors-timeout-override ()
  "CLI timeout override allows a delayed extension UI response to succeed."
  (piem-fake-pi-test-with-process
      (proc "extension-confirm" "--extension-timeout-ms" "500")
    (piem-fake-pi-test--send proc '(:type "prompt" :message "/test-confirm"))
    (let* ((prompt-response (piem-fake-pi-test--pop-object proc))
           (agent-start (piem-fake-pi-test--pop-object proc))
           (request (piem-fake-pi-test--pop-object proc)))
      (should (equal (plist-get prompt-response :command) "prompt"))
      (should (equal (plist-get agent-start :type) "agent_start"))
      (should (equal (plist-get request :type) "extension_ui_request"))
      (should (equal (plist-get request :method) "confirm"))
      (should (= (plist-get request :timeout) 500))
      (sleep-for 0.15)
      (piem-fake-pi-test--send
       proc
       (list :type "extension_ui_response"
             :id (plist-get request :id)
             :confirmed t))
      (let* ((events (piem-fake-pi-test--collect-until
                      proc
                      (lambda (obj) (equal (plist-get obj :type) "agent_end"))))
             (custom-end (seq-find
                          (lambda (obj)
                            (and (equal (plist-get obj :type) "message_end")
                                 (equal (plist-get (plist-get obj :message) :content)
                                        "CONFIRMED")))
                          events)))
        (should custom-end)))))

(provide 'piem-fake-pi-test)
;;; piem-fake-pi-test.el ends here
