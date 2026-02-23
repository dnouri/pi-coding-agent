;;; pi-coding-agent-core-test.el --- Tests for pi-coding-agent-core -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the core pi-coding-agent functionality: JSON parsing,
;; line accumulation, and command encoding.

;;; Code:

(require 'ert)
(require 'pi-coding-agent-core)
(require 'pi-coding-agent-test-common)

;;;; JSON Parsing Tests

(ert-deftest pi-coding-agent-test-parse-json-response ()
  "Parse a valid JSON response."
  (let ((result (pi-coding-agent--parse-json-line "{\"type\":\"response\",\"id\":\"req_1\",\"success\":true}")))
    (should (equal (plist-get result :type) "response"))
    (should (equal (plist-get result :id) "req_1"))
    (should (eq (plist-get result :success) t))))

(ert-deftest pi-coding-agent-test-parse-json-event ()
  "Parse a valid JSON event (no id field)."
  (let ((result (pi-coding-agent--parse-json-line "{\"type\":\"agent_start\"}")))
    (should (equal (plist-get result :type) "agent_start"))
    (should (null (plist-get result :id)))))

(ert-deftest pi-coding-agent-test-parse-json-with-nested-data ()
  "Parse JSON with nested objects."
  (let ((result (pi-coding-agent--parse-json-line "{\"type\":\"response\",\"data\":{\"model\":\"claude\",\"count\":42}}")))
    (should (equal (plist-get result :type) "response"))
    (let ((data (plist-get result :data)))
      (should (equal (plist-get data :model) "claude"))
      (should (equal (plist-get data :count) 42)))))

(ert-deftest pi-coding-agent-test-parse-json-malformed ()
  "Malformed JSON returns nil, not an error."
  (should (null (pi-coding-agent--parse-json-line "not valid json")))
  (should (null (pi-coding-agent--parse-json-line "")))
  (should (null (pi-coding-agent--parse-json-line "{")))
  (should (null (pi-coding-agent--parse-json-line "{\"unterminated"))))

(ert-deftest pi-coding-agent-test-parse-json-boolean-false ()
  "JSON false parses to :false, not nil."
  (let ((result (pi-coding-agent--parse-json-line "{\"isStreaming\":false}")))
    (should (eq (plist-get result :isStreaming) :false))))

(ert-deftest pi-coding-agent-test-parse-json-unicode ()
  "Unicode content is preserved correctly."
  (let ((result (pi-coding-agent--parse-json-line "{\"msg\":\"Hello 世界 🌍\"}")))
    (should (equal (plist-get result :msg) "Hello 世界 🌍"))))

;;;; Line Accumulation Tests

(ert-deftest pi-coding-agent-test-accumulate-complete-line ()
  "A complete line (ending with newline) is extracted."
  (let ((result (pi-coding-agent--accumulate-lines "" "foo\n")))
    (should (equal (car result) '("foo")))
    (should (equal (cdr result) ""))))

(ert-deftest pi-coding-agent-test-accumulate-partial-line ()
  "A partial line (no newline) is saved as remainder."
  (let ((result (pi-coding-agent--accumulate-lines "" "foo")))
    (should (null (car result)))
    (should (equal (cdr result) "foo"))))

(ert-deftest pi-coding-agent-test-accumulate-multiple-lines ()
  "Multiple complete lines are all extracted."
  (let ((result (pi-coding-agent--accumulate-lines "" "foo\nbar\nbaz\n")))
    (should (equal (car result) '("foo" "bar" "baz")))
    (should (equal (cdr result) ""))))

(ert-deftest pi-coding-agent-test-accumulate-partial-then-complete ()
  "Partial line followed by completion works correctly."
  (let* ((result1 (pi-coding-agent--accumulate-lines "" "fo"))
         (result2 (pi-coding-agent--accumulate-lines (cdr result1) "o\nbar\n")))
    (should (null (car result1)))
    (should (equal (cdr result1) "fo"))
    (should (equal (car result2) '("foo" "bar")))
    (should (equal (cdr result2) ""))))

(ert-deftest pi-coding-agent-test-accumulate-mixed-complete-and-partial ()
  "Complete lines extracted, partial line saved."
  (let ((result (pi-coding-agent--accumulate-lines "" "foo\nbar")))
    (should (equal (car result) '("foo")))
    (should (equal (cdr result) "bar"))))

(ert-deftest pi-coding-agent-test-accumulate-with-existing-remainder ()
  "Existing remainder is prepended to new chunk."
  (let ((result (pi-coding-agent--accumulate-lines "hel" "lo\nworld\n")))
    (should (equal (car result) '("hello" "world")))
    (should (equal (cdr result) ""))))

;;;; JSON Encoding Tests

(ert-deftest pi-coding-agent-test-encode-simple-command ()
  "Encode a simple command to JSON."
  (let ((result (pi-coding-agent--encode-command '(:type "prompt" :message "hello"))))
    (should (string-suffix-p "\n" result))
    (let ((parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
      (should (equal (plist-get parsed :type) "prompt"))
      (should (equal (plist-get parsed :message) "hello")))))

(ert-deftest pi-coding-agent-test-encode-command-with-id ()
  "Encoded command includes id field."
  (let* ((result (pi-coding-agent--encode-command '(:type "get_state" :id "req_1")))
         (parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
    (should (equal (plist-get parsed :type) "get_state"))
    (should (equal (plist-get parsed :id) "req_1"))))

(ert-deftest pi-coding-agent-test-encode-nested-command ()
  "Encode command with nested data."
  (let* ((result (pi-coding-agent--encode-command '(:type "set_model" :data (:provider "anthropic" :model "claude"))))
         (parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
    (should (equal (plist-get parsed :type) "set_model"))
    (let ((data (plist-get parsed :data)))
      (should (equal (plist-get data :provider) "anthropic")))))

(ert-deftest pi-coding-agent-test-encode-command-with-array ()
  "Encode command with array values."
  (let* ((result (pi-coding-agent--encode-command '(:type "prompt" :attachments [])))
         (parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
    (should (equal (plist-get parsed :attachments) []))))

;;;; Process Cleanup Tests

(ert-deftest pi-coding-agent-test-process-exit-clears-pending ()
  "Process exit clears pending request state."
  (let ((pi-coding-agent--request-id-counter 0)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pi-coding-agent--get-pending-requests fake-proc))
              (pending-types (pi-coding-agent--get-pending-command-types fake-proc)))
          (puthash "req_1" #'ignore pending)
          (puthash "req_2" #'ignore pending)
          (puthash "req_1" "get_tree" pending-types)
          (puthash "req_2" "get_state" pending-types)
          (pi-coding-agent--handle-process-exit fake-proc "finished\n")
          (should (= (hash-table-count pending) 0))
          (should (= (hash-table-count pending-types) 0)))
      (ignore-errors (delete-process fake-proc)))))

(ert-deftest pi-coding-agent-test-process-exit-calls-callbacks-with-error ()
  "Process exit calls pending callbacks with error response."
  (let ((pi-coding-agent--request-id-counter 0)
        (received nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pi-coding-agent--get-pending-requests fake-proc)))
          (puthash "req_1" (lambda (r) (setq received r)) pending)
          (pi-coding-agent--handle-process-exit fake-proc "finished\n")
          (should received)
          (should (eq (plist-get received :success) :false))
          (should (plist-get received :error)))
      (ignore-errors (delete-process fake-proc)))))

;;;; Response Dispatch Tests

(ert-deftest pi-coding-agent-test-dispatch-response-calls-callback ()
  "Response with matching ID calls stored callback."
  (let ((received nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pi-coding-agent--get-pending-requests fake-proc)))
          (puthash "req_1" (lambda (r) (setq received r)) pending)
          (pi-coding-agent--dispatch-response fake-proc '(:type "response" :id "req_1" :success t))
          (should received)
          (should (eq (plist-get received :success) t)))
      (delete-process fake-proc))))

(ert-deftest pi-coding-agent-test-dispatch-response-removes-callback ()
  "Response removes callback from pending requests after calling."
  (let ((fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pi-coding-agent--get-pending-requests fake-proc)))
          (puthash "req_1" #'ignore pending)
          (pi-coding-agent--dispatch-response fake-proc '(:type "response" :id "req_1" :success t))
          (should (null (gethash "req_1" pending))))
      (delete-process fake-proc))))

(ert-deftest pi-coding-agent-test-dispatch-idless-response-to-sole-pending ()
  "Id-less response routes to sole pending callback."
  (let ((received nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pi-coding-agent--get-pending-requests fake-proc)))
          (puthash "req_1" (lambda (r) (setq received r)) pending)
          (pi-coding-agent--dispatch-response
           fake-proc
           '(:type "response" :command "get_tree" :success nil :error "Unknown command: get_tree"))
          (should (equal (plist-get received :error) "Unknown command: get_tree"))
          (should (= (hash-table-count pending) 0)))
      (delete-process fake-proc))))

(ert-deftest pi-coding-agent-test-dispatch-idless-response-matches-command ()
  "Id-less response with :command routes to matching request."
  (let ((pi-coding-agent--request-id-counter 0)
        (received-tree nil)
        (received-state nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'process-send-string) #'ignore))
          (pi-coding-agent--rpc-async
           fake-proc
           '(:type "get_tree")
           (lambda (response)
             (setq received-tree response)))
          (pi-coding-agent--rpc-async
           fake-proc
           '(:type "get_state")
           (lambda (response)
             (setq received-state response)))
          (pi-coding-agent--dispatch-response
           fake-proc
           '(:type "response" :command "get_tree" :success nil :error "Unknown command: get_tree"))
          (should (equal (plist-get received-tree :error) "Unknown command: get_tree"))
          (should-not received-state)
          (should (= (hash-table-count (pi-coding-agent--get-pending-requests fake-proc)) 1)))
      (delete-process fake-proc))))

(ert-deftest pi-coding-agent-test-dispatch-event-calls-handler ()
  "Events call the process handler."
  (let ((event-received nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          (process-put fake-proc 'pi-coding-agent-display-handler
                       (lambda (e) (setq event-received e)))
          (pi-coding-agent--dispatch-response fake-proc '(:type "agent_start"))
          (should event-received)
          (should (equal (plist-get event-received :type) "agent_start")))
      (delete-process fake-proc))))

(ert-deftest pi-coding-agent-test-dispatch-unknown-id-no-crash ()
  "Unknown response IDs do not crash."
  (let ((fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (should (null (pi-coding-agent--dispatch-response fake-proc '(:type "response" :id "unknown" :success t))))
      (delete-process fake-proc))))

;;;; RPC Send Tests

(ert-deftest pi-coding-agent-test-rpc-async-stores-callback ()
  "Sending a command stores the callback in process's pending requests."
  (let ((pi-coding-agent--request-id-counter 0)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          (pi-coding-agent--rpc-async fake-proc '(:type "get_state") #'ignore)
          (let ((pending (pi-coding-agent--get-pending-requests fake-proc)))
            (should (gethash "req_1" pending))))
      (delete-process fake-proc))))

(ert-deftest pi-coding-agent-test-rpc-async-sends-json-with-id ()
  "Sending a command writes JSON with ID to process."
  (let ((pi-coding-agent--request-id-counter 0)
        (output-buffer (generate-new-buffer " *test-output*")))
    (unwind-protect
        (let ((fake-proc (start-process "cat" output-buffer "cat")))
          (unwind-protect
              (progn
                (pi-coding-agent--rpc-async fake-proc '(:type "get_state") #'ignore)
                (should
                 (pi-coding-agent-test-wait-until
                  (lambda ()
                    (with-current-buffer output-buffer
                      (> (buffer-size) 0)))
                  pi-coding-agent-test-short-wait
                  pi-coding-agent-test-poll-interval
                  fake-proc))
                (with-current-buffer output-buffer
                  (let* ((sent (buffer-string))
                         (json (json-parse-string (string-trim sent) :object-type 'plist)))
                    (should (equal (plist-get json :type) "get_state"))
                    (should (equal (plist-get json :id) "req_1")))))
            (delete-process fake-proc)))
      (kill-buffer output-buffer))))

;;;; Request ID Management Tests

(ert-deftest pi-coding-agent-test-request-id-increments ()
  "Request IDs increment with each call."
  (let ((pi-coding-agent--request-id-counter 0))
    (should (equal (pi-coding-agent--next-request-id) "req_1"))
    (should (equal (pi-coding-agent--next-request-id) "req_2"))
    (should (equal (pi-coding-agent--next-request-id) "req_3"))))

(ert-deftest pi-coding-agent-test-pending-requests-table ()
  "Each process gets its own pending requests table."
  (let ((proc1 (start-process "cat1" nil "cat"))
        (proc2 (start-process "cat2" nil "cat")))
    (unwind-protect
        (let ((pending1 (pi-coding-agent--get-pending-requests proc1))
              (pending2 (pi-coding-agent--get-pending-requests proc2)))
          ;; Tables should be separate
          (puthash "req_1" #'ignore pending1)
          (should (gethash "req_1" pending1))
          (should (null (gethash "req_1" pending2)))
          ;; Calling get again returns the same table
          (should (eq pending1 (pi-coding-agent--get-pending-requests proc1))))
      (ignore-errors (delete-process proc1))
      (ignore-errors (delete-process proc2)))))

;;;; State Event Handling Tests

(ert-deftest pi-coding-agent-test-event-agent-start-sets-streaming ()
  "agent_start event sets pi-coding-agent--status to streaming."
  (let ((pi-coding-agent--status 'idle)
        (pi-coding-agent--state nil))
    (pi-coding-agent--update-state-from-event '(:type "agent_start"))
    (should (eq pi-coding-agent--status 'streaming))))

(ert-deftest pi-coding-agent-test-event-agent-end-clears-streaming ()
  "agent_end event sets pi-coding-agent--status to idle."
  (let ((pi-coding-agent--status 'streaming)
        (pi-coding-agent--state nil))
    (pi-coding-agent--update-state-from-event '(:type "agent_end" :messages []))
    (should (eq pi-coding-agent--status 'idle))))

(ert-deftest pi-coding-agent-test-event-agent-end-stores-messages ()
  "agent_end event stores messages in state."
  (let ((pi-coding-agent--status 'streaming)
        (pi-coding-agent--state (list :messages nil))
        (msgs [(:role "user" :content "hi") (:role "assistant" :content "hello")]))
    (pi-coding-agent--update-state-from-event (list :type "agent_end" :messages msgs))
    (should (plist-get pi-coding-agent--state :messages))))

(ert-deftest pi-coding-agent-test-event-message-start-creates-current-message ()
  "message_start event creates current-message in state."
  (let ((pi-coding-agent--state (list :current-message nil))
        (msg '(:role "assistant" :content [])))
    (pi-coding-agent--update-state-from-event (list :type "message_start" :message msg))
    (should (plist-get pi-coding-agent--state :current-message))))

(ert-deftest pi-coding-agent-test-event-message-update-is-state-noop ()
  "message_update event does not modify state.
Display is handled by the display handler, not by state updates."
  (let ((pi-coding-agent--state (list :current-message '(:role "assistant" :content "Hello"))))
    (pi-coding-agent--update-state-from-event
     '(:type "message_update"
       :message (:role "assistant")
       :assistantMessageEvent (:type "text_delta" :delta " world")))
    (should (equal (plist-get (plist-get pi-coding-agent--state :current-message) :content)
                   "Hello"))))

(ert-deftest pi-coding-agent-test-event-message-end-clears-current-message ()
  "message_end event clears current-message."
  (let ((pi-coding-agent--state (list :current-message '(:role "assistant" :content "done"))))
    (pi-coding-agent--update-state-from-event '(:type "message_end" :message (:role "assistant")))
    (should (null (plist-get pi-coding-agent--state :current-message)))))

(ert-deftest pi-coding-agent-test-event-tool-start-tracks-active-tool ()
  "tool_execution_start adds tool to active-tools."
  (let ((pi-coding-agent--state (list :active-tools nil)))
    (pi-coding-agent--update-state-from-event
     '(:type "tool_execution_start"
       :toolCallId "call_123"
       :toolName "bash"
       :args (:command "ls")))
    (should (gethash "call_123" (plist-get pi-coding-agent--state :active-tools)))))

(ert-deftest pi-coding-agent-test-event-tool-update-stores-partial-result ()
  "tool_execution_update stores partial result."
  (let* ((tools (make-hash-table :test 'equal))
         (pi-coding-agent--state (list :active-tools tools)))
    (puthash "call_123" (list :name "bash") tools)
    (pi-coding-agent--update-state-from-event
     '(:type "tool_execution_update"
       :toolCallId "call_123"
       :partialResult (:content "output")))
    (let ((tool (gethash "call_123" tools)))
      (should (plist-get tool :partial-result)))))

(ert-deftest pi-coding-agent-test-event-tool-end-removes-active-tool ()
  "tool_execution_end removes tool from active-tools."
  (let* ((tools (make-hash-table :test 'equal))
         (pi-coding-agent--state (list :active-tools tools)))
    (puthash "call_123" (list :name "bash") tools)
    (pi-coding-agent--update-state-from-event
     '(:type "tool_execution_end"
       :toolCallId "call_123"
       :result (:content "done")
       :isError :false))
    (should (null (gethash "call_123" tools)))))

(ert-deftest pi-coding-agent-test-ensure-active-tools-from-nil ()
  "pi-coding-agent--ensure-active-tools works when pi-coding-agent--state is nil."
  (let ((pi-coding-agent--state nil))
    (let ((tools (pi-coding-agent--ensure-active-tools)))
      (should (hash-table-p tools))
      (should (hash-table-p (plist-get pi-coding-agent--state :active-tools))))))

(ert-deftest pi-coding-agent-test-response-set-model-updates-state ()
  "set_model response updates model in state."
  (let ((pi-coding-agent--state (list :model nil)))
    (pi-coding-agent--update-state-from-response
     '(:type "response"
       :command "set_model"
       :success t
       :data (:id "claude" :name "Claude")))
    (should (plist-get pi-coding-agent--state :model))
    (should (equal (plist-get (plist-get pi-coding-agent--state :model) :id) "claude"))))

(ert-deftest pi-coding-agent-test-response-cycle-thinking-updates-state ()
  "cycle_thinking_level response updates thinking-level."
  (let ((pi-coding-agent--state (list :thinking-level "off")))
    (pi-coding-agent--update-state-from-response
     '(:type "response"
       :command "cycle_thinking_level"
       :success t
       :data (:level "high")))
    (should (equal (plist-get pi-coding-agent--state :thinking-level) "high"))))

(ert-deftest pi-coding-agent-test-response-failed-does-not-update ()
  "Failed responses do not update state."
  (let ((pi-coding-agent--state (list :model '(:id "original"))))
    (pi-coding-agent--update-state-from-response
     '(:type "response"
       :command "set_model"
       :success :false
       :error "Model not found"))
    (should (equal (plist-get (plist-get pi-coding-agent--state :model) :id) "original"))))

(ert-deftest pi-coding-agent-test-state-needs-verify-when-stale ()
  "State needs verification when timestamp is old."
  (let ((pi-coding-agent--status 'idle)
        (pi-coding-agent--state (list :model "test"))
        (pi-coding-agent--state-timestamp (- (float-time) 60)))  ;; 60 seconds ago
    (should (pi-coding-agent--state-needs-verification-p))))

(ert-deftest pi-coding-agent-test-state-no-verify-when-fresh ()
  "State does not need verification when recently updated."
  (let ((pi-coding-agent--status 'idle)
        (pi-coding-agent--state (list :model "test"))
        (pi-coding-agent--state-timestamp (float-time)))  ;; Now
    (should (not (pi-coding-agent--state-needs-verification-p)))))

(ert-deftest pi-coding-agent-test-state-no-verify-during-streaming ()
  "State does not need verification while streaming."
  (let ((pi-coding-agent--status 'streaming)
        (pi-coding-agent--state (list :model "test"))
        (pi-coding-agent--state-timestamp (- (float-time) 60)))  ;; Old, but streaming
    (should (not (pi-coding-agent--state-needs-verification-p)))))

(ert-deftest pi-coding-agent-test-state-no-verify-when-no-timestamp ()
  "State does not need verification when not initialized."
  (let ((pi-coding-agent--status 'idle)
        (pi-coding-agent--state nil)
        (pi-coding-agent--state-timestamp nil))
    (should (not (pi-coding-agent--state-needs-verification-p)))))

(ert-deftest pi-coding-agent-test-event-dispatch-updates-state ()
  "Events update buffer-local state via handler."
  (let ((pi-coding-agent--status 'idle)
        (pi-coding-agent--state nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          ;; Register a handler that updates state
          (process-put fake-proc 'pi-coding-agent-display-handler
                       (lambda (e)
                         (pi-coding-agent--update-state-from-event e)))
          (pi-coding-agent--handle-event fake-proc '(:type "agent_start"))
          (should (eq pi-coding-agent--status 'streaming)))
      (delete-process fake-proc))))

;;;; State Management Tests

(ert-deftest pi-coding-agent-test-state-from-get-state-response ()
  "State is initialized from get_state response data."
  (let ((response '(:type "response"
                    :command "get_state"
                    :success t
                    :data (:model (:id "claude" :name "Claude")
                           :thinkingLevel "medium"
                           :isStreaming :false
                           :sessionId "test-123"
                           :messageCount 0))))
    (let ((state (pi-coding-agent--extract-state-from-response response)))
      (should state)
      (should (equal (plist-get state :session-id) "test-123"))
      (should (equal (plist-get state :thinking-level) "medium"))
      (should (eq (plist-get state :status) 'idle))
      (should (plist-get state :model)))))

(ert-deftest pi-coding-agent-test-state-extract-status-idle ()
  "Extracted state has status idle when not streaming or compacting."
  (let ((response '(:type "response"
                    :success t
                    :data (:isStreaming :false :isCompacting :false))))
    (let ((state (pi-coding-agent--extract-state-from-response response)))
      (should (eq (plist-get state :status) 'idle)))))

(ert-deftest pi-coding-agent-test-state-extract-status-streaming ()
  "Extracted state has status streaming when isStreaming is true."
  (let ((response '(:type "response"
                    :success t
                    :data (:isStreaming t :isCompacting :false))))
    (let ((state (pi-coding-agent--extract-state-from-response response)))
      (should (eq (plist-get state :status) 'streaming)))))

(ert-deftest pi-coding-agent-test-state-extract-status-compacting ()
  "Extracted state has status compacting when isCompacting is true."
  (let ((response '(:type "response"
                    :success t
                    :data (:isStreaming :false :isCompacting t))))
    (let ((state (pi-coding-agent--extract-state-from-response response)))
      (should (eq (plist-get state :status) 'compacting)))))

;;;; Auto-Retry Event State Tests

(ert-deftest pi-coding-agent-test-event-auto-retry-start-sets-retrying ()
  "auto_retry_start event sets is-retrying to t."
  (let ((pi-coding-agent--status 'streaming)
        (pi-coding-agent--state (list :is-retrying nil)))
    (pi-coding-agent--update-state-from-event
     '(:type "auto_retry_start"
       :attempt 1
       :maxAttempts 3
       :delayMs 2000
       :errorMessage "429 rate_limit_error"))
    (should (eq (plist-get pi-coding-agent--state :is-retrying) t))
    (should (equal (plist-get pi-coding-agent--state :retry-attempt) 1))
    (should (equal (plist-get pi-coding-agent--state :last-error) "429 rate_limit_error"))))

(ert-deftest pi-coding-agent-test-event-auto-retry-end-success-clears-retrying ()
  "auto_retry_end with success clears is-retrying."
  (let ((pi-coding-agent--status 'streaming)
        (pi-coding-agent--state (list :is-retrying t :retry-attempt 2)))
    (pi-coding-agent--update-state-from-event
     '(:type "auto_retry_end"
       :success t
       :attempt 2))
    (should (eq (plist-get pi-coding-agent--state :is-retrying) nil))))

(ert-deftest pi-coding-agent-test-event-auto-retry-end-failure-stores-error ()
  "auto_retry_end with failure stores final error."
  (let ((pi-coding-agent--status 'streaming)
        (pi-coding-agent--state (list :is-retrying t)))
    (pi-coding-agent--update-state-from-event
     '(:type "auto_retry_end"
       :success :false
       :attempt 3
       :finalError "529 overloaded_error: Overloaded"))
    (should (eq (plist-get pi-coding-agent--state :is-retrying) nil))
    (should (equal (plist-get pi-coding-agent--state :last-error) "529 overloaded_error: Overloaded"))))

(ert-deftest pi-coding-agent-test-event-extension-error-stores-error ()
  "extension_error event stores error message in state."
  (let ((pi-coding-agent--status 'streaming)
        (pi-coding-agent--state (list :last-error nil)))
    (pi-coding-agent--update-state-from-event
     '(:type "extension_error"
       :extensionPath "/path/to/extension.ts"
       :event "tool_call"
       :error "TypeError: undefined is not a function"))
    (should (equal (plist-get pi-coding-agent--state :last-error)
                   "TypeError: undefined is not a function"))))

(ert-deftest pi-coding-agent-test-event-agent-start-clears-error-state ()
  "agent_start event clears error and retry state."
  (let ((pi-coding-agent--status 'idle)
        (pi-coding-agent--state (list :is-retrying t
                         :last-error "Previous error")))
    (pi-coding-agent--update-state-from-event '(:type "agent_start"))
    (should (eq pi-coding-agent--status 'streaming))
    (should (eq (plist-get pi-coding-agent--state :is-retrying) nil))
    (should (eq (plist-get pi-coding-agent--state :last-error) nil))))

(ert-deftest pi-coding-agent-test-event-agent-end-clears-retry-state ()
  "agent_end event clears retry state."
  (let ((pi-coding-agent--status 'streaming)
        (pi-coding-agent--state (list :is-retrying t)))
    (pi-coding-agent--update-state-from-event '(:type "agent_end" :messages []))
    (should (eq pi-coding-agent--status 'idle))
    (should (eq (plist-get pi-coding-agent--state :is-retrying) nil))))

;;;; Test Utilities

(ert-deftest pi-coding-agent-test-wait-until-succeeds ()
  "wait-until returns the predicate value when it becomes true."
  (let ((flag nil))
    (run-at-time 0.05 nil (lambda () (setq flag t)))
    (let ((result (pi-coding-agent-test-wait-until (lambda () flag) 0.5 0.01)))
      (should (eq result t)))))

(ert-deftest pi-coding-agent-test-wait-until-times-out ()
  "wait-until returns nil when the predicate stays false."
  (let ((result (pi-coding-agent-test-wait-until (lambda () nil) 0.05 0.01)))
    (should (null result))))

(ert-deftest pi-coding-agent-test-format-elapsed-rounds-to-millis ()
  "Elapsed formatter rounds to milliseconds with suffix."
  (should (equal (pi-coding-agent-test-format-elapsed 1.23456) "1.235s")))

;;;; Executable Customization Tests

(defun pi-coding-agent-test--capture-process-command (executable extra-args)
  "Return the command list that `--start-process' would pass to make-process.
Mocks `make-process' to capture :command, binding
`pi-coding-agent-executable' to EXECUTABLE and
`pi-coding-agent-extra-args' to EXTRA-ARGS."
  (let ((pi-coding-agent-executable executable)
        (pi-coding-agent-extra-args extra-args)
        (captured nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured (plist-get args :command))
                 nil)))
      (ignore-errors (pi-coding-agent--start-process "/tmp/")))
    captured))

(ert-deftest pi-coding-agent-test-start-process-uses-custom-executable ()
  "start-process builds command from `pi-coding-agent-executable'."
  (should (equal (pi-coding-agent-test--capture-process-command '("npx" "pi") nil)
                 '("npx" "pi" "--mode" "rpc"))))

(ert-deftest pi-coding-agent-test-start-process-custom-executable-with-extra-args ()
  "start-process combines custom executable and extra-args."
  (should (equal (pi-coding-agent-test--capture-process-command
                  '("npx" "pi") '("-e" "/path/to/ext.ts"))
                 '("npx" "pi" "--mode" "rpc" "-e" "/path/to/ext.ts"))))

(provide 'pi-coding-agent-core-test)
;;; pi-coding-agent-core-test.el ends here
