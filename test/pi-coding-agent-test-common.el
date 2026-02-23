;;; pi-coding-agent-test-common.el --- Shared test utilities and configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Common definitions shared across pi-coding-agent test files.
;; Centralizes timeout values, the mock-session macro, and toolcall
;; streaming helpers for easy adjustment (e.g., slow CI).

;;; Code:

(require 'cl-lib) ; for cl-letf in mock-session macro

;;; Timeout Configuration

(defvar pi-coding-agent-test-short-wait 0.5
  "Short wait in seconds for async operations to complete.")

(defvar pi-coding-agent-test-poll-interval 0.1
  "Polling interval in seconds for waiting loops.")

(defvar pi-coding-agent-test-rpc-timeout 10
  "Timeout in seconds for RPC calls in tests.")

(defvar pi-coding-agent-test-integration-timeout 600
  "Timeout in seconds for integration tests.
The steer test requires two LLM round-trips.  With qwen3:1.7b at
maxTokens=500 on CPU-only CI runners, each turn can take up to 150s.
Two turns: ~300s.  600s provides sufficient margin for noisy runners.")

(defvar pi-coding-agent-test-gui-timeout 180
  "Timeout in seconds for GUI tests (includes real LLM responses).
qwen3:1.7b tool calls take 50-65s on CI (5x local speed).  The
previous 90s timeout gave only 38%% margin; 180s gives ~177%%.")

;;;; Formatting Helpers

(defun pi-coding-agent-test-format-elapsed (seconds)
  "Format SECONDS as a human-readable duration with millisecond precision."
  (format "%.3fs" (float seconds)))

;;;; Waiting Helpers

(defun pi-coding-agent-test-wait-until (predicate &optional timeout poll-interval process)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse.
POLL-INTERVAL controls how often to check (default
`pi-coding-agent-test-poll-interval'). If PROCESS is non-nil, it is
passed to `accept-process-output' to allow process I/O.

Returns the predicate value, or nil on timeout."
  (let* ((timeout (or timeout pi-coding-agent-test-short-wait))
         (poll-interval (or poll-interval pi-coding-agent-test-poll-interval))
         (start (float-time))
         (result (funcall predicate)))
    (while (and (not result)
                (< (- (float-time) start) timeout))
      (accept-process-output process poll-interval)
      (setq result (funcall predicate)))
    result))

(defun pi-coding-agent-test-wait-for-process-exit (process &optional timeout)
  "Wait until PROCESS is no longer live, up to TIMEOUT seconds.
Returns non-nil if the process exits before the timeout."
  (pi-coding-agent-test-wait-until
   (lambda () (not (process-live-p process)))
   (or timeout pi-coding-agent-test-short-wait)
   pi-coding-agent-test-poll-interval
   process))

;;;; Toolcall Streaming Helpers

(defun pi-coding-agent-test--count-matches (regexp string)
  "Count non-overlapping occurrences of REGEXP in STRING."
  (let ((count 0) (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defmacro pi-coding-agent-test--with-toolcall (tool-name args &rest body)
  "Set up a chat buffer with a streaming tool call, then run BODY.
Creates a temp buffer in chat mode, fires agent_start, message_start,
and toolcall_start for TOOL-NAME with ARGS (a plist).  The tool call
ID is \"call_1\" and contentIndex is 0."
  (declare (indent 2) (debug (sexp sexp body)))
  `(with-temp-buffer
     (pi-coding-agent-chat-mode)
     (pi-coding-agent--handle-display-event '(:type "agent_start"))
     (pi-coding-agent--handle-display-event '(:type "message_start"))
     (pi-coding-agent--handle-display-event
      `(:type "message_update"
        :assistantMessageEvent (:type "toolcall_start" :contentIndex 0)
        :message (:role "assistant"
                  :content [(:type "toolCall" :id "call_1"
                             :name ,,tool-name :arguments ,,args)])))
     ,@body))

(defun pi-coding-agent-test--send-delta (tool-name args)
  "Send a toolcall_delta event for TOOL-NAME with ARGS.
Uses tool call ID \"call_1\" and contentIndex 0."
  (pi-coding-agent--handle-display-event
   `(:type "message_update"
     :assistantMessageEvent (:type "toolcall_delta" :contentIndex 0 :delta "x")
     :message (:role "assistant"
               :content [(:type "toolCall" :id "call_1"
                          :name ,tool-name :arguments ,args)]))))

;;;; Mock Session

(defmacro pi-coding-agent-test-with-mock-session (dir &rest body)
  "Execute BODY with a mocked pi session in DIR, cleaning up after.
DIR should be a unique directory path like \"/tmp/pi-coding-agent-test-foo/\".
Mocks `project-current', `pi-coding-agent--start-process', and
`pi-coding-agent--display-buffers'.
Automatically cleans up chat and input buffers."
  (declare (indent 1) (debug t))
  `(let ((default-directory ,dir))
     (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
               ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
               ((symbol-function 'pi-coding-agent--display-buffers) #'ignore))
       (unwind-protect
           (progn (pi-coding-agent) ,@body)
         (pi-coding-agent-test--kill-session-buffers ,dir)))))

(defun pi-coding-agent-test--chat-buffer-name (dir &optional session)
  "Return the chat buffer name for DIR and optional SESSION."
  (pi-coding-agent--buffer-name :chat dir session))

(defun pi-coding-agent-test--input-buffer-name (dir &optional session)
  "Return the input buffer name for DIR and optional SESSION."
  (pi-coding-agent--buffer-name :input dir session))

(defun pi-coding-agent-test--kill-session-buffers (dir &optional session)
  "Kill chat and input buffers for DIR and optional SESSION."
  (ignore-errors (kill-buffer (pi-coding-agent-test--chat-buffer-name dir session)))
  (ignore-errors (kill-buffer (pi-coding-agent-test--input-buffer-name dir session))))

;;;; Two-Session Fixture

(defmacro pi-coding-agent-test--with-two-sessions (buf-a buf-b &rest body)
  "Execute BODY with two independent chat-mode buffers BUF-A and BUF-B.
Both buffers are created with `pi-coding-agent-chat-mode' activated.
Buffers are killed after BODY completes, even on error."
  (declare (indent 2) (debug (symbolp symbolp body)))
  `(let ((,buf-a (generate-new-buffer "*test-chat-A*"))
         (,buf-b (generate-new-buffer "*test-chat-B*")))
     (with-current-buffer ,buf-a (pi-coding-agent-chat-mode))
     (with-current-buffer ,buf-b (pi-coding-agent-chat-mode))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (kill-buffer ,buf-a))
       (ignore-errors (kill-buffer ,buf-b)))))

;;;; Tree Fixtures

(defun pi-coding-agent-test--build-tree (&rest specs)
  "Build a conversation tree from flat node SPECS.
Each SPEC is (ID PARENT-OVERRIDE TYPE &rest PROPS) where:
- ID is the node identifier string
- PARENT-OVERRIDE is nil (auto-chain to previous node) or a parent ID
- TYPE is \"message\", \"compaction\", \"model_change\", etc.
- PROPS are keyword plist properties (:role, :preview, etc.)
First node with nil PARENT-OVERRIDE becomes the root.
Returns (:tree VECTOR :leafId LAST-ID)."
  (let ((nodes (make-hash-table :test 'equal))
        (child-ids (make-hash-table :test 'equal))
        (roots nil)
        (prev-id nil)
        (last-id nil))
    ;; Pass 1: create nodes, track parent-child relationships
    (dolist (spec specs)
      (let* ((id (nth 0 spec))
             (parent-override (nth 1 spec))
             (type (nth 2 spec))
             (props (nthcdr 3 spec))
             (parent-id (or parent-override prev-id))
             (node (append (list :id id :type type)
                           (when parent-id (list :parentId parent-id))
                           props)))
        (puthash id node nodes)
        (if parent-id
            (puthash parent-id
                     (append (gethash parent-id child-ids) (list id))
                     child-ids)
          (push id roots))
        (setq prev-id id
              last-id id)))
    ;; Pass 2: build nested structure with :children vectors
    (cl-labels ((build (id)
                  (let* ((node (gethash id nodes))
                         (kids (gethash id child-ids))
                         (child-vec (if kids
                                        (apply #'vector (mapcar #'build kids))
                                      [])))
                    (append node (list :children child-vec)))))
      (list :tree (apply #'vector (mapcar #'build (nreverse roots)))
            :leafId last-id))))

(defun pi-coding-agent-test--make-3turn-tree ()
  "Return tree data for a 3-turn conversation: u1→a1→u2→a2→u3→a3."
  (pi-coding-agent-test--build-tree
   '("u1" nil "message" :role "user" :preview "First question")
   '("a1" nil "message" :role "assistant" :preview "First answer")
   '("u2" nil "message" :role "user" :preview "Second question")
   '("a2" nil "message" :role "assistant" :preview "Second answer")
   '("u3" nil "message" :role "user" :preview "Third question")
   '("a3" nil "message" :role "assistant" :preview "Third answer")))

(defun pi-coding-agent-test--make-3turn-fork-messages ()
  "Return get_fork_messages payload for three user turns."
  [(:entryId "u1" :text "First question")
   (:entryId "u2" :text "Second question")
   (:entryId "u3" :text "Third question")])

;;;; Chat Buffer Fixtures

(defun pi-coding-agent-test--insert-chat-turns ()
  "Insert a 3-turn chat with setext headings into current buffer.
Returns the buffer with content ready for navigation tests."
  (insert "Pi 1.0.0\n========\nWelcome\n\n"
          "You · 10:00\n===========\nFirst question\n\n"
          "Assistant\n=========\nFirst answer\n\n"
          "You · 10:05\n===========\nSecond question\n\n"
          "Assistant\n=========\nSecond answer\n\n"
          "You · 10:10\n===========\nThird question\n\n"
          "Assistant\n=========\nThird answer\n"))

(provide 'pi-coding-agent-test-common)
;;; pi-coding-agent-test-common.el ends here
