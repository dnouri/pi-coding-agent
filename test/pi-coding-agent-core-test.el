;;; pi-core-test.el --- Tests for pi-core.el -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the core pi.el functionality: JSON parsing,
;; line accumulation, and command encoding.

;;; Code:

(require 'ert)
(require 'pi-core)
(require 'pi-test-common)

;;;; JSON Parsing Tests

(ert-deftest pi-test-parse-json-response ()
  "Parse a valid JSON response."
  (let ((result (pi--parse-json-line "{\"type\":\"response\",\"id\":\"req_1\",\"success\":true}")))
    (should (equal (plist-get result :type) "response"))
    (should (equal (plist-get result :id) "req_1"))
    (should (eq (plist-get result :success) t))))

(ert-deftest pi-test-parse-json-event ()
  "Parse a valid JSON event (no id field)."
  (let ((result (pi--parse-json-line "{\"type\":\"agent_start\"}")))
    (should (equal (plist-get result :type) "agent_start"))
    (should (null (plist-get result :id)))))

(ert-deftest pi-test-parse-json-with-nested-data ()
  "Parse JSON with nested objects."
  (let ((result (pi--parse-json-line "{\"type\":\"response\",\"data\":{\"model\":\"claude\",\"count\":42}}")))
    (should (equal (plist-get result :type) "response"))
    (let ((data (plist-get result :data)))
      (should (equal (plist-get data :model) "claude"))
      (should (equal (plist-get data :count) 42)))))

(ert-deftest pi-test-parse-json-malformed ()
  "Malformed JSON returns nil, not an error."
  (should (null (pi--parse-json-line "not valid json")))
  (should (null (pi--parse-json-line "")))
  (should (null (pi--parse-json-line "{")))
  (should (null (pi--parse-json-line "{\"unterminated"))))

(ert-deftest pi-test-parse-json-boolean-false ()
  "JSON false parses to :false, not nil."
  (let ((result (pi--parse-json-line "{\"isStreaming\":false}")))
    (should (eq (plist-get result :isStreaming) :false))))

(ert-deftest pi-test-parse-json-unicode ()
  "Unicode content is preserved correctly."
  (let ((result (pi--parse-json-line "{\"msg\":\"Hello 世界 🌍\"}")))
    (should (equal (plist-get result :msg) "Hello 世界 🌍"))))

;;;; Line Accumulation Tests

(ert-deftest pi-test-accumulate-complete-line ()
  "A complete line (ending with newline) is extracted."
  (let ((result (pi--accumulate-lines "" "foo\n")))
    (should (equal (car result) '("foo")))
    (should (equal (cdr result) ""))))

(ert-deftest pi-test-accumulate-partial-line ()
  "A partial line (no newline) is saved as remainder."
  (let ((result (pi--accumulate-lines "" "foo")))
    (should (null (car result)))
    (should (equal (cdr result) "foo"))))

(ert-deftest pi-test-accumulate-multiple-lines ()
  "Multiple complete lines are all extracted."
  (let ((result (pi--accumulate-lines "" "foo\nbar\nbaz\n")))
    (should (equal (car result) '("foo" "bar" "baz")))
    (should (equal (cdr result) ""))))

(ert-deftest pi-test-accumulate-partial-then-complete ()
  "Partial line followed by completion works correctly."
  (let* ((result1 (pi--accumulate-lines "" "fo"))
         (result2 (pi--accumulate-lines (cdr result1) "o\nbar\n")))
    (should (null (car result1)))
    (should (equal (cdr result1) "fo"))
    (should (equal (car result2) '("foo" "bar")))
    (should (equal (cdr result2) ""))))

(ert-deftest pi-test-accumulate-mixed-complete-and-partial ()
  "Complete lines extracted, partial line saved."
  (let ((result (pi--accumulate-lines "" "foo\nbar")))
    (should (equal (car result) '("foo")))
    (should (equal (cdr result) "bar"))))

(ert-deftest pi-test-accumulate-with-existing-remainder ()
  "Existing remainder is prepended to new chunk."
  (let ((result (pi--accumulate-lines "hel" "lo\nworld\n")))
    (should (equal (car result) '("hello" "world")))
    (should (equal (cdr result) ""))))

;;;; JSON Encoding Tests

(ert-deftest pi-test-encode-simple-command ()
  "Encode a simple command to JSON."
  (let ((result (pi--encode-command '(:type "prompt" :message "hello"))))
    (should (string-suffix-p "\n" result))
    (let ((parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
      (should (equal (plist-get parsed :type) "prompt"))
      (should (equal (plist-get parsed :message) "hello")))))

(ert-deftest pi-test-encode-command-with-id ()
  "Encoded command includes id field."
  (let* ((result (pi--encode-command '(:type "get_state" :id "req_1")))
         (parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
    (should (equal (plist-get parsed :type) "get_state"))
    (should (equal (plist-get parsed :id) "req_1"))))

(ert-deftest pi-test-encode-nested-command ()
  "Encode command with nested data."
  (let* ((result (pi--encode-command '(:type "set_model" :data (:provider "anthropic" :model "claude"))))
         (parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
    (should (equal (plist-get parsed :type) "set_model"))
    (let ((data (plist-get parsed :data)))
      (should (equal (plist-get data :provider) "anthropic")))))

(ert-deftest pi-test-encode-command-with-array ()
  "Encode command with array values."
  (let* ((result (pi--encode-command '(:type "prompt" :attachments [])))
         (parsed (json-parse-string (string-trim-right result) :object-type 'plist)))
    (should (equal (plist-get parsed :attachments) []))))

;;;; Process Cleanup Tests

(ert-deftest pi-test-process-exit-clears-pending ()
  "Process exit clears that process's pending requests."
  (let ((pi--request-id-counter 0)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pi--get-pending-requests fake-proc)))
          (puthash "req_1" #'ignore pending)
          (puthash "req_2" #'ignore pending)
          (pi--handle-process-exit fake-proc "finished\n")
          (should (= (hash-table-count pending) 0)))
      (ignore-errors (delete-process fake-proc)))))

(ert-deftest pi-test-process-exit-calls-callbacks-with-error ()
  "Process exit calls pending callbacks with error response."
  (let ((pi--request-id-counter 0)
        (received nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pi--get-pending-requests fake-proc)))
          (puthash "req_1" (lambda (r) (setq received r)) pending)
          (pi--handle-process-exit fake-proc "finished\n")
          (should received)
          (should (eq (plist-get received :success) :false))
          (should (plist-get received :error)))
      (ignore-errors (delete-process fake-proc)))))

;;;; Response Dispatch Tests

(ert-deftest pi-test-dispatch-response-calls-callback ()
  "Response with matching ID calls stored callback."
  (let ((received nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pi--get-pending-requests fake-proc)))
          (puthash "req_1" (lambda (r) (setq received r)) pending)
          (pi--dispatch-response fake-proc '(:type "response" :id "req_1" :success t))
          (should received)
          (should (eq (plist-get received :success) t)))
      (delete-process fake-proc))))

(ert-deftest pi-test-dispatch-response-removes-callback ()
  "Response removes callback from pending requests after calling."
  (let ((fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (let ((pending (pi--get-pending-requests fake-proc)))
          (puthash "req_1" #'ignore pending)
          (pi--dispatch-response fake-proc '(:type "response" :id "req_1" :success t))
          (should (null (gethash "req_1" pending))))
      (delete-process fake-proc))))

(ert-deftest pi-test-dispatch-event-calls-handler ()
  "Non-response messages call process's handler."
  (let ((event-received nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          ;; Register a handler on the process
          (process-put fake-proc 'pi-display-handler
                       (lambda (e) (setq event-received e)))
          (pi--dispatch-response fake-proc '(:type "agent_start"))
          (should event-received)
          (should (equal (plist-get event-received :type) "agent_start")))
      (delete-process fake-proc))))

(ert-deftest pi-test-dispatch-unknown-id-no-crash ()
  "Response with unknown ID is handled gracefully."
  (let ((fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (should (null (pi--dispatch-response fake-proc '(:type "response" :id "unknown" :success t))))
      (delete-process fake-proc))))

;;;; RPC Send Tests

(ert-deftest pi-test-rpc-async-stores-callback ()
  "Sending a command stores the callback in process's pending requests."
  (let ((pi--request-id-counter 0)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          (pi--rpc-async fake-proc '(:type "get_state") #'ignore)
          (let ((pending (pi--get-pending-requests fake-proc)))
            (should (gethash "req_1" pending))))
      (delete-process fake-proc))))

(ert-deftest pi-test-rpc-async-sends-json-with-id ()
  "Sending a command writes JSON with ID to process."
  (let ((pi--request-id-counter 0)
        (output-buffer (generate-new-buffer " *test-output*")))
    (unwind-protect
        (let ((fake-proc (start-process "cat" output-buffer "cat")))
          (unwind-protect
              (progn
                (pi--rpc-async fake-proc '(:type "get_state") #'ignore)
                (sleep-for pi-test-poll-interval)
                (with-current-buffer output-buffer
                  (let* ((sent (buffer-string))
                         (json (json-parse-string (string-trim sent) :object-type 'plist)))
                    (should (equal (plist-get json :type) "get_state"))
                    (should (equal (plist-get json :id) "req_1")))))
            (delete-process fake-proc)))
      (kill-buffer output-buffer))))

;;;; Request ID Management Tests

(ert-deftest pi-test-request-id-increments ()
  "Request IDs increment with each call."
  (let ((pi--request-id-counter 0))
    (should (equal (pi--next-request-id) "req_1"))
    (should (equal (pi--next-request-id) "req_2"))
    (should (equal (pi--next-request-id) "req_3"))))

(ert-deftest pi-test-pending-requests-table ()
  "Each process gets its own pending requests table."
  (let ((proc1 (start-process "cat1" nil "cat"))
        (proc2 (start-process "cat2" nil "cat")))
    (unwind-protect
        (let ((pending1 (pi--get-pending-requests proc1))
              (pending2 (pi--get-pending-requests proc2)))
          ;; Tables should be separate
          (puthash "req_1" #'ignore pending1)
          (should (gethash "req_1" pending1))
          (should (null (gethash "req_1" pending2)))
          ;; Calling get again returns the same table
          (should (eq pending1 (pi--get-pending-requests proc1))))
      (ignore-errors (delete-process proc1))
      (ignore-errors (delete-process proc2)))))

;;;; State Event Handling Tests

(ert-deftest pi-test-event-agent-start-sets-streaming ()
  "agent_start event sets pi--status to streaming."
  (let ((pi--status 'idle)
        (pi--state nil))
    (pi--update-state-from-event '(:type "agent_start"))
    (should (eq pi--status 'streaming))))

(ert-deftest pi-test-event-agent-end-clears-streaming ()
  "agent_end event sets pi--status to idle."
  (let ((pi--status 'streaming)
        (pi--state nil))
    (pi--update-state-from-event '(:type "agent_end" :messages []))
    (should (eq pi--status 'idle))))

(ert-deftest pi-test-event-agent-end-stores-messages ()
  "agent_end event stores messages in state."
  (let ((pi--status 'streaming)
        (pi--state (list :messages nil))
        (msgs [(:role "user" :content "hi") (:role "assistant" :content "hello")]))
    (pi--update-state-from-event (list :type "agent_end" :messages msgs))
    (should (plist-get pi--state :messages))))

(ert-deftest pi-test-event-message-start-creates-current-message ()
  "message_start event creates current-message in state."
  (let ((pi--state (list :current-message nil))
        (msg '(:role "assistant" :content [])))
    (pi--update-state-from-event (list :type "message_start" :message msg))
    (should (plist-get pi--state :current-message))))

(ert-deftest pi-test-event-message-update-accumulates-text ()
  "message_update event updates current message with delta."
  (let ((pi--state (list :current-message '(:role "assistant" :content "Hello"))))
    (pi--update-state-from-event
     '(:type "message_update"
       :message (:role "assistant")
       :assistantMessageEvent (:type "text_delta" :delta " world")))
    (should (equal (plist-get (plist-get pi--state :current-message) :content)
                   "Hello world"))))

(ert-deftest pi-test-event-message-end-clears-current-message ()
  "message_end event clears current-message."
  (let ((pi--state (list :current-message '(:role "assistant" :content "done"))))
    (pi--update-state-from-event '(:type "message_end" :message (:role "assistant")))
    (should (null (plist-get pi--state :current-message)))))

(ert-deftest pi-test-event-tool-start-tracks-active-tool ()
  "tool_execution_start adds tool to active-tools."
  (let ((pi--state (list :active-tools nil)))
    (pi--update-state-from-event
     '(:type "tool_execution_start"
       :toolCallId "call_123"
       :toolName "bash"
       :args (:command "ls")))
    (should (gethash "call_123" (plist-get pi--state :active-tools)))))

(ert-deftest pi-test-event-tool-update-stores-partial-result ()
  "tool_execution_update stores partial result."
  (let* ((tools (make-hash-table :test 'equal))
         (pi--state (list :active-tools tools)))
    (puthash "call_123" (list :name "bash") tools)
    (pi--update-state-from-event
     '(:type "tool_execution_update"
       :toolCallId "call_123"
       :partialResult (:content "output")))
    (let ((tool (gethash "call_123" tools)))
      (should (plist-get tool :partial-result)))))

(ert-deftest pi-test-event-tool-end-removes-active-tool ()
  "tool_execution_end removes tool from active-tools."
  (let* ((tools (make-hash-table :test 'equal))
         (pi--state (list :active-tools tools)))
    (puthash "call_123" (list :name "bash") tools)
    (pi--update-state-from-event
     '(:type "tool_execution_end"
       :toolCallId "call_123"
       :result (:content "done")
       :isError :false))
    (should (null (gethash "call_123" tools)))))

(ert-deftest pi-test-ensure-active-tools-from-nil ()
  "pi--ensure-active-tools works when pi--state is nil."
  (let ((pi--state nil))
    (let ((tools (pi--ensure-active-tools)))
      (should (hash-table-p tools))
      (should (hash-table-p (plist-get pi--state :active-tools))))))

(ert-deftest pi-test-response-set-model-updates-state ()
  "set_model response updates model in state."
  (let ((pi--state (list :model nil)))
    (pi--update-state-from-response
     '(:type "response"
       :command "set_model"
       :success t
       :data (:id "claude" :name "Claude")))
    (should (plist-get pi--state :model))
    (should (equal (plist-get (plist-get pi--state :model) :id) "claude"))))

(ert-deftest pi-test-response-cycle-thinking-updates-state ()
  "cycle_thinking_level response updates thinking-level."
  (let ((pi--state (list :thinking-level "off")))
    (pi--update-state-from-response
     '(:type "response"
       :command "cycle_thinking_level"
       :success t
       :data (:level "high")))
    (should (equal (plist-get pi--state :thinking-level) "high"))))

(ert-deftest pi-test-response-failed-does-not-update ()
  "Failed responses do not update state."
  (let ((pi--state (list :model '(:id "original"))))
    (pi--update-state-from-response
     '(:type "response"
       :command "set_model"
       :success :false
       :error "Model not found"))
    (should (equal (plist-get (plist-get pi--state :model) :id) "original"))))

(ert-deftest pi-test-state-needs-verify-when-stale ()
  "State needs verification when timestamp is old."
  (let ((pi--status 'idle)
        (pi--state (list :model "test"))
        (pi--state-timestamp (- (float-time) 60)))  ;; 60 seconds ago
    (should (pi--state-needs-verification-p))))

(ert-deftest pi-test-state-no-verify-when-fresh ()
  "State does not need verification when recently updated."
  (let ((pi--status 'idle)
        (pi--state (list :model "test"))
        (pi--state-timestamp (float-time)))  ;; Now
    (should (not (pi--state-needs-verification-p)))))

(ert-deftest pi-test-state-no-verify-during-streaming ()
  "State does not need verification while streaming."
  (let ((pi--status 'streaming)
        (pi--state (list :model "test"))
        (pi--state-timestamp (- (float-time) 60)))  ;; Old, but streaming
    (should (not (pi--state-needs-verification-p)))))

(ert-deftest pi-test-state-no-verify-when-no-timestamp ()
  "State does not need verification when not initialized."
  (let ((pi--status 'idle)
        (pi--state nil)
        (pi--state-timestamp nil))
    (should (not (pi--state-needs-verification-p)))))

(ert-deftest pi-test-event-dispatch-updates-state ()
  "Events update buffer-local state via handler."
  (let ((pi--status 'idle)
        (pi--state nil)
        (fake-proc (start-process "cat" nil "cat")))
    (unwind-protect
        (progn
          ;; Register a handler that updates state
          (process-put fake-proc 'pi-display-handler
                       (lambda (e)
                         (pi--update-state-from-event e)))
          (pi--handle-event fake-proc '(:type "agent_start"))
          (should (eq pi--status 'streaming)))
      (delete-process fake-proc))))

;;;; State Management Tests

(ert-deftest pi-test-state-from-get-state-response ()
  "State is initialized from get_state response data."
  (let ((response '(:type "response"
                    :command "get_state"
                    :success t
                    :data (:model (:id "claude" :name "Claude")
                           :thinkingLevel "medium"
                           :isStreaming :false
                           :sessionId "test-123"
                           :messageCount 0))))
    (let ((state (pi--extract-state-from-response response)))
      (should state)
      (should (equal (plist-get state :session-id) "test-123"))
      (should (equal (plist-get state :thinking-level) "medium"))
      (should (eq (plist-get state :status) 'idle))
      (should (plist-get state :model)))))

(ert-deftest pi-test-state-extract-status-idle ()
  "Extracted state has status idle when not streaming or compacting."
  (let ((response '(:type "response"
                    :success t
                    :data (:isStreaming :false :isCompacting :false))))
    (let ((state (pi--extract-state-from-response response)))
      (should (eq (plist-get state :status) 'idle)))))

(ert-deftest pi-test-state-extract-status-streaming ()
  "Extracted state has status streaming when isStreaming is true."
  (let ((response '(:type "response"
                    :success t
                    :data (:isStreaming t :isCompacting :false))))
    (let ((state (pi--extract-state-from-response response)))
      (should (eq (plist-get state :status) 'streaming)))))

(ert-deftest pi-test-state-extract-status-compacting ()
  "Extracted state has status compacting when isCompacting is true."
  (let ((response '(:type "response"
                    :success t
                    :data (:isStreaming :false :isCompacting t))))
    (let ((state (pi--extract-state-from-response response)))
      (should (eq (plist-get state :status) 'compacting)))))

;;;; Auto-Retry Event State Tests

(ert-deftest pi-test-event-auto-retry-start-sets-retrying ()
  "auto_retry_start event sets is-retrying to t."
  (let ((pi--status 'streaming)
        (pi--state (list :is-retrying nil)))
    (pi--update-state-from-event
     '(:type "auto_retry_start"
       :attempt 1
       :maxAttempts 3
       :delayMs 2000
       :errorMessage "429 rate_limit_error"))
    (should (eq (plist-get pi--state :is-retrying) t))
    (should (equal (plist-get pi--state :retry-attempt) 1))
    (should (equal (plist-get pi--state :last-error) "429 rate_limit_error"))))

(ert-deftest pi-test-event-auto-retry-end-success-clears-retrying ()
  "auto_retry_end with success clears is-retrying."
  (let ((pi--status 'streaming)
        (pi--state (list :is-retrying t :retry-attempt 2)))
    (pi--update-state-from-event
     '(:type "auto_retry_end"
       :success t
       :attempt 2))
    (should (eq (plist-get pi--state :is-retrying) nil))))

(ert-deftest pi-test-event-auto-retry-end-failure-stores-error ()
  "auto_retry_end with failure stores final error."
  (let ((pi--status 'streaming)
        (pi--state (list :is-retrying t)))
    (pi--update-state-from-event
     '(:type "auto_retry_end"
       :success :false
       :attempt 3
       :finalError "529 overloaded_error: Overloaded"))
    (should (eq (plist-get pi--state :is-retrying) nil))
    (should (equal (plist-get pi--state :last-error) "529 overloaded_error: Overloaded"))))

(ert-deftest pi-test-event-hook-error-stores-error ()
  "hook_error event stores error message in state."
  (let ((pi--status 'streaming)
        (pi--state (list :last-error nil)))
    (pi--update-state-from-event
     '(:type "hook_error"
       :hookPath "/path/to/hook.ts"
       :event "tool_call"
       :error "TypeError: undefined is not a function"))
    (should (equal (plist-get pi--state :last-error)
                   "TypeError: undefined is not a function"))))

(ert-deftest pi-test-event-agent-start-clears-error-state ()
  "agent_start event clears error and retry state."
  (let ((pi--status 'idle)
        (pi--state (list :is-retrying t
                         :last-error "Previous error")))
    (pi--update-state-from-event '(:type "agent_start"))
    (should (eq pi--status 'streaming))
    (should (eq (plist-get pi--state :is-retrying) nil))
    (should (eq (plist-get pi--state :last-error) nil))))

(ert-deftest pi-test-event-agent-end-clears-retry-state ()
  "agent_end event clears retry state."
  (let ((pi--status 'streaming)
        (pi--state (list :is-retrying t)))
    (pi--update-state-from-event '(:type "agent_end" :messages []))
    (should (eq pi--status 'idle))
    (should (eq (plist-get pi--state :is-retrying) nil))))

(provide 'pi-core-test)
;;; pi-core-test.el ends here
