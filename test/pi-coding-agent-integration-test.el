;;; pi-coding-agent-integration-test.el --- Integration tests for pi-coding-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; Integration tests that run against a real pi process.
;; These tests require:
;; - pi CLI installed and in PATH
;; - PI_RUN_INTEGRATION environment variable set
;;
;; Run with: PI_RUN_INTEGRATION=1 emacs --batch -L . -L test -l pi-coding-agent-integration-test -f ert-run-tests-batch-and-exit
;;
;; Why both integration and unit tests?
;; ------------------------------------
;; Unit tests (pi-coding-agent-test-event-*, pi-coding-agent-test-state-*) test internal logic with
;; mocked events. They're fast and run on every commit.
;;
;; Integration tests verify the REAL pi process behavior - actual RPC
;; protocol, actual event sequences, actual state transitions. They catch
;; issues that mocks might hide (protocol changes, CLI bugs, etc.).
;;
;; Both layers are valuable:
;; - Unit tests catch logic bugs quickly during development
;; - Integration tests catch protocol/CLI compatibility issues before release

;;; Code:

(require 'ert)
(require 'pi-coding-agent)
(require 'pi-coding-agent-test-common)

(defun pi-coding-agent-integration--skip-unless-available ()
  "Skip test if integration tests should not run."
  (unless (executable-find "pi")
    (ert-skip "pi executable not found"))
  (unless (getenv "PI_RUN_INTEGRATION")
    (ert-skip "PI_RUN_INTEGRATION not set - opt-in required")))

(defmacro pi-coding-agent-integration-with-process (&rest body)
  "Run BODY with a fresh pi process, cleaning up after.
Sets up event dispatching through pi-coding-agent--event-handlers list."
  (declare (indent 0) (debug t))
  `(progn
     (pi-coding-agent-integration--skip-unless-available)
     (let ((proc (pi-coding-agent--start-process default-directory))
           (pi-coding-agent--event-handlers nil))
       ;; Register handler that dispatches to pi-coding-agent--event-handlers
       (process-put proc 'pi-coding-agent-display-handler
                    (lambda (event)
                      (dolist (handler pi-coding-agent--event-handlers)
                        (funcall handler event))))
       (unwind-protect
           (progn ,@body)
         (when (and proc (process-live-p proc))
           (delete-process proc))))))

;;; RPC Protocol Tests (HIGH value)

(ert-deftest pi-coding-agent-integration-process-starts ()
  "Pi process starts and is alive."
  (pi-coding-agent-integration-with-process
    (should (processp proc))
    (should (process-live-p proc))))

(ert-deftest pi-coding-agent-integration-process-query-on-exit ()
  "Pi process has query-on-exit-flag set for kill confirmation."
  (pi-coding-agent-integration-with-process
    (should (process-query-on-exit-flag proc))))

(ert-deftest pi-coding-agent-integration-get-state-succeeds ()
  "get_state command returns successful response."
  (pi-coding-agent-integration-with-process
    (let ((response (pi-coding-agent--rpc-sync proc '(:type "get_state") 10)))
      (should response)
      (should (eq (plist-get response :success) t))
      (should (plist-get response :data)))))

(ert-deftest pi-coding-agent-integration-get-state-has-model ()
  "get_state response includes model information."
  (pi-coding-agent-integration-with-process
    (let* ((response (pi-coding-agent--rpc-sync proc '(:type "get_state") 10))
           (data (plist-get response :data)))
      (should (plist-get data :model)))))

(ert-deftest pi-coding-agent-integration-get-state-has-thinking-level ()
  "get_state response includes thinking level."
  (pi-coding-agent-integration-with-process
    (let* ((response (pi-coding-agent--rpc-sync proc '(:type "get_state") 10))
           (data (plist-get response :data)))
      (should (plist-get data :thinkingLevel)))))

;;; Event Sequence Tests (HIGH value)

(ert-deftest pi-coding-agent-integration-prompt-generates-events ()
  "Sending a prompt generates agent_start and agent_end events."
  (pi-coding-agent-integration-with-process
    (let ((events nil)
          (got-agent-end nil))
      ;; Collect events
      (push (lambda (e)
              (push e events)
              (when (equal (plist-get e :type) "agent_end")
                (setq got-agent-end t)))
            pi-coding-agent--event-handlers)
      ;; Send minimal prompt
      (pi-coding-agent--rpc-async proc '(:type "prompt" :message "Say OK") #'ignore)
      ;; Wait for agent_end
      (with-timeout (pi-coding-agent-test-integration-timeout
                     (ert-fail "Timeout waiting for agent_end"))
        (while (not got-agent-end)
          (accept-process-output proc 0.1)))
      ;; Verify events
      (should (seq-find (lambda (e) (equal (plist-get e :type) "agent_start")) events))
      (should (seq-find (lambda (e) (equal (plist-get e :type) "agent_end")) events)))))

(ert-deftest pi-coding-agent-integration-prompt-generates-message-events ()
  "Prompt generates message_start and message_end events."
  (pi-coding-agent-integration-with-process
    (let ((events nil)
          (got-agent-end nil))
      (push (lambda (e)
              (push e events)
              (when (equal (plist-get e :type) "agent_end")
                (setq got-agent-end t)))
            pi-coding-agent--event-handlers)
      (pi-coding-agent--rpc-async proc '(:type "prompt" :message "Say hi") #'ignore)
      (with-timeout (pi-coding-agent-test-integration-timeout
                     (ert-fail "Timeout"))
        (while (not got-agent-end)
          (accept-process-output proc 0.1)))
      ;; Verify message events
      (should (seq-find (lambda (e) (equal (plist-get e :type) "message_start")) events))
      (should (seq-find (lambda (e) (equal (plist-get e :type) "message_end")) events)))))

(ert-deftest pi-coding-agent-integration-prompt-generates-text-deltas ()
  "Prompt generates message_update events with text content."
  (pi-coding-agent-integration-with-process
    (let ((events nil)
          (got-agent-end nil))
      (push (lambda (e)
              (push e events)
              (when (equal (plist-get e :type) "agent_end")
                (setq got-agent-end t)))
            pi-coding-agent--event-handlers)
      (pi-coding-agent--rpc-async proc '(:type "prompt" :message "Say hello") #'ignore)
      (with-timeout (pi-coding-agent-test-integration-timeout
                     (ert-fail "Timeout"))
        (while (not got-agent-end)
          (accept-process-output proc 0.1)))
      ;; Verify we got text_delta events
      (let ((text-deltas (seq-filter
                          (lambda (e)
                            (and (equal (plist-get e :type) "message_update")
                                 (let ((msg (plist-get e :assistantMessageEvent)))
                                   (equal (plist-get msg :type) "text_delta"))))
                          events)))
        (should (> (length text-deltas) 0))))))

;;; State Consistency Tests (MEDIUM value)

(ert-deftest pi-coding-agent-integration-state-not-streaming-after-complete ()
  "isStreaming is false after response completes."
  (pi-coding-agent-integration-with-process
    (let ((got-agent-end nil))
      (push (lambda (e)
              (when (equal (plist-get e :type) "agent_end")
                (setq got-agent-end t)))
            pi-coding-agent--event-handlers)
      (pi-coding-agent--rpc-async proc '(:type "prompt" :message "Say: done") #'ignore)
      (with-timeout (pi-coding-agent-test-integration-timeout
                     (ert-fail "Timeout"))
        (while (not got-agent-end)
          (accept-process-output proc 0.1)))
      ;; Now check state
      (let* ((state (pi-coding-agent--rpc-sync proc '(:type "get_state") pi-coding-agent-test-rpc-timeout))
             (data (plist-get state :data)))
        (should (eq (plist-get data :isStreaming) :false))))))

(ert-deftest pi-coding-agent-integration-message-count-increases ()
  "messageCount increases after a prompt."
  (pi-coding-agent-integration-with-process
    ;; Get initial count
    (let* ((initial (pi-coding-agent--rpc-sync proc '(:type "get_state") pi-coding-agent-test-rpc-timeout))
           (initial-count (plist-get (plist-get initial :data) :messageCount))
           (got-agent-end nil))
      (push (lambda (e)
              (when (equal (plist-get e :type) "agent_end")
                (setq got-agent-end t)))
            pi-coding-agent--event-handlers)
      (pi-coding-agent--rpc-async proc '(:type "prompt" :message "Say: hello") #'ignore)
      (with-timeout (pi-coding-agent-test-integration-timeout
                     (ert-fail "Timeout"))
        (while (not got-agent-end)
          (accept-process-output proc 0.1)))
      ;; Check count increased
      (let* ((after (pi-coding-agent--rpc-sync proc '(:type "get_state") pi-coding-agent-test-rpc-timeout))
             (after-count (plist-get (plist-get after :data) :messageCount)))
        (should (> after-count initial-count))))))

;;; Abort Tests (MEDIUM value)

(ert-deftest pi-coding-agent-integration-abort-stops-streaming ()
  "Abort command stops streaming and results in agent_end."
  (pi-coding-agent-integration-with-process
    (let ((got-agent-start nil)
          (got-agent-end nil))
      (push (lambda (e)
              (pcase (plist-get e :type)
                ("agent_start" (setq got-agent-start t))
                ("agent_end" (setq got-agent-end t))))
            pi-coding-agent--event-handlers)
      ;; Send a prompt that will generate a long response
      (pi-coding-agent--rpc-async proc
                     '(:type "prompt" :message "Count from 1 to 100 slowly")
                     #'ignore)
      ;; Wait for streaming to start
      (with-timeout (pi-coding-agent-test-rpc-timeout (ert-fail "Timeout waiting for agent_start"))
        (while (not got-agent-start)
          (accept-process-output proc 0.1)))
      ;; Now abort
      (pi-coding-agent--rpc-async proc '(:type "abort") #'ignore)
      ;; Wait for agent_end
      (with-timeout (pi-coding-agent-test-rpc-timeout (ert-fail "Timeout waiting for agent_end after abort"))
        (while (not got-agent-end)
          (accept-process-output proc 0.1)))
      ;; Verify we're no longer streaming
      (let* ((state (pi-coding-agent--rpc-sync proc '(:type "get_state") pi-coding-agent-test-rpc-timeout))
             (data (plist-get state :data)))
        (should (eq (plist-get data :isStreaming) :false))))))

;;; New Session Tests (HIGH value - catches API breaking changes)

(ert-deftest pi-coding-agent-integration-new-session-succeeds ()
  "new_session command returns success and resets message count.
This test verifies the RPC protocol works, not full LLM interaction."
  (pi-coding-agent-integration-with-process
    ;; Verify initial state
    (let* ((before (pi-coding-agent--rpc-sync proc '(:type "get_state") pi-coding-agent-test-rpc-timeout))
           (before-count (plist-get (plist-get before :data) :messageCount)))
      (should (= before-count 0))
      ;; Call new_session (should work even with no messages)
      (let ((response (pi-coding-agent--rpc-sync proc '(:type "new_session") pi-coding-agent-test-rpc-timeout)))
        (should (plist-get response :success))
        (should (eq (plist-get (plist-get response :data) :cancelled) :false)))
      ;; Verify still at 0 messages
      (let* ((after (pi-coding-agent--rpc-sync proc '(:type "get_state") pi-coding-agent-test-rpc-timeout))
             (after-count (plist-get (plist-get after :data) :messageCount)))
        (should (= after-count 0))))))

(ert-deftest pi-coding-agent-integration-get-branch-messages-returns-entry-id ()
  "get_branch_messages returns messages with entryId field (not entryIndex).
Catches API breaking changes in the branch message format."
  (pi-coding-agent-integration-with-process
    ;; Empty session should return empty messages array
    (let ((response (pi-coding-agent--rpc-sync proc '(:type "get_branch_messages") pi-coding-agent-test-rpc-timeout)))
      (should (plist-get response :success))
      (let ((messages (plist-get (plist-get response :data) :messages)))
        (should (vectorp messages))
        ;; If there are messages, verify they have entryId not entryIndex
        (when (> (length messages) 0)
          (let ((first-msg (aref messages 0)))
            (should (plist-get first-msg :entryId))
            (should-not (plist-get first-msg :entryIndex))))))))

(provide 'pi-coding-agent-integration-test)
;;; pi-coding-agent-integration-test.el ends here
