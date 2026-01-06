;;; pi-integration-test.el --- Integration tests for pi.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Integration tests that run against a real pi process.
;; These tests require:
;; - pi CLI installed and in PATH
;; - PI_RUN_INTEGRATION environment variable set
;;
;; Run with: PI_RUN_INTEGRATION=1 emacs --batch -L . -L test -l pi-integration-test -f ert-run-tests-batch-and-exit
;;
;; Why both integration and unit tests?
;; ------------------------------------
;; Unit tests (pi-test-event-*, pi-test-state-*) test internal logic with
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
(require 'pi)
(require 'pi-test-common)

(defun pi-integration--skip-unless-available ()
  "Skip test if integration tests should not run."
  (unless (executable-find "pi")
    (ert-skip "pi executable not found"))
  (unless (getenv "PI_RUN_INTEGRATION")
    (ert-skip "PI_RUN_INTEGRATION not set - opt-in required")))

(defmacro pi-integration-with-process (&rest body)
  "Run BODY with a fresh pi process, cleaning up after.
Sets up event dispatching through pi--event-handlers list."
  (declare (indent 0) (debug t))
  `(progn
     (pi-integration--skip-unless-available)
     (let ((proc (pi--start-process default-directory))
           (pi--event-handlers nil))
       ;; Register handler that dispatches to pi--event-handlers
       (process-put proc 'pi-display-handler
                    (lambda (event)
                      (dolist (handler pi--event-handlers)
                        (funcall handler event))))
       (unwind-protect
           (progn ,@body)
         (when (and proc (process-live-p proc))
           (delete-process proc))))))

;;; RPC Protocol Tests (HIGH value)

(ert-deftest pi-integration-process-starts ()
  "Pi process starts and is alive."
  (pi-integration-with-process
    (should (processp proc))
    (should (process-live-p proc))))

(ert-deftest pi-integration-get-state-succeeds ()
  "get_state command returns successful response."
  (pi-integration-with-process
    (let ((response (pi--rpc-sync proc '(:type "get_state") 10)))
      (should response)
      (should (eq (plist-get response :success) t))
      (should (plist-get response :data)))))

(ert-deftest pi-integration-get-state-has-model ()
  "get_state response includes model information."
  (pi-integration-with-process
    (let* ((response (pi--rpc-sync proc '(:type "get_state") 10))
           (data (plist-get response :data)))
      (should (plist-get data :model)))))

(ert-deftest pi-integration-get-state-has-thinking-level ()
  "get_state response includes thinking level."
  (pi-integration-with-process
    (let* ((response (pi--rpc-sync proc '(:type "get_state") 10))
           (data (plist-get response :data)))
      (should (plist-get data :thinkingLevel)))))

;;; Event Sequence Tests (HIGH value)

(ert-deftest pi-integration-prompt-generates-events ()
  "Sending a prompt generates agent_start and agent_end events."
  (pi-integration-with-process
    (let ((events nil)
          (got-agent-end nil))
      ;; Collect events
      (push (lambda (e)
              (push e events)
              (when (equal (plist-get e :type) "agent_end")
                (setq got-agent-end t)))
            pi--event-handlers)
      ;; Send minimal prompt
      (pi--rpc-async proc '(:type "prompt" :message "Say OK") #'ignore)
      ;; Wait for agent_end
      (with-timeout (pi-test-integration-timeout
                     (ert-fail "Timeout waiting for agent_end"))
        (while (not got-agent-end)
          (accept-process-output proc 0.1)))
      ;; Verify events
      (should (seq-find (lambda (e) (equal (plist-get e :type) "agent_start")) events))
      (should (seq-find (lambda (e) (equal (plist-get e :type) "agent_end")) events)))))

(ert-deftest pi-integration-prompt-generates-message-events ()
  "Prompt generates message_start and message_end events."
  (pi-integration-with-process
    (let ((events nil)
          (got-agent-end nil))
      (push (lambda (e)
              (push e events)
              (when (equal (plist-get e :type) "agent_end")
                (setq got-agent-end t)))
            pi--event-handlers)
      (pi--rpc-async proc '(:type "prompt" :message "Say hi") #'ignore)
      (with-timeout (pi-test-integration-timeout
                     (ert-fail "Timeout"))
        (while (not got-agent-end)
          (accept-process-output proc 0.1)))
      ;; Verify message events
      (should (seq-find (lambda (e) (equal (plist-get e :type) "message_start")) events))
      (should (seq-find (lambda (e) (equal (plist-get e :type) "message_end")) events)))))

(ert-deftest pi-integration-prompt-generates-text-deltas ()
  "Prompt generates message_update events with text content."
  (pi-integration-with-process
    (let ((events nil)
          (got-agent-end nil))
      (push (lambda (e)
              (push e events)
              (when (equal (plist-get e :type) "agent_end")
                (setq got-agent-end t)))
            pi--event-handlers)
      (pi--rpc-async proc '(:type "prompt" :message "Say hello") #'ignore)
      (with-timeout (pi-test-integration-timeout
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

(ert-deftest pi-integration-state-not-streaming-after-complete ()
  "isStreaming is false after response completes."
  (pi-integration-with-process
    (let ((got-agent-end nil))
      (push (lambda (e)
              (when (equal (plist-get e :type) "agent_end")
                (setq got-agent-end t)))
            pi--event-handlers)
      (pi--rpc-async proc '(:type "prompt" :message "Say: done") #'ignore)
      (with-timeout (pi-test-integration-timeout
                     (ert-fail "Timeout"))
        (while (not got-agent-end)
          (accept-process-output proc 0.1)))
      ;; Now check state
      (let* ((state (pi--rpc-sync proc '(:type "get_state") pi-test-rpc-timeout))
             (data (plist-get state :data)))
        (should (eq (plist-get data :isStreaming) :false))))))

(ert-deftest pi-integration-message-count-increases ()
  "messageCount increases after a prompt."
  (pi-integration-with-process
    ;; Get initial count
    (let* ((initial (pi--rpc-sync proc '(:type "get_state") pi-test-rpc-timeout))
           (initial-count (plist-get (plist-get initial :data) :messageCount))
           (got-agent-end nil))
      (push (lambda (e)
              (when (equal (plist-get e :type) "agent_end")
                (setq got-agent-end t)))
            pi--event-handlers)
      (pi--rpc-async proc '(:type "prompt" :message "Say: hello") #'ignore)
      (with-timeout (pi-test-integration-timeout
                     (ert-fail "Timeout"))
        (while (not got-agent-end)
          (accept-process-output proc 0.1)))
      ;; Check count increased
      (let* ((after (pi--rpc-sync proc '(:type "get_state") pi-test-rpc-timeout))
             (after-count (plist-get (plist-get after :data) :messageCount)))
        (should (> after-count initial-count))))))

;;; Abort Tests (MEDIUM value)

(ert-deftest pi-integration-abort-stops-streaming ()
  "Abort command stops streaming and results in agent_end."
  (pi-integration-with-process
    (let ((got-agent-start nil)
          (got-agent-end nil))
      (push (lambda (e)
              (pcase (plist-get e :type)
                ("agent_start" (setq got-agent-start t))
                ("agent_end" (setq got-agent-end t))))
            pi--event-handlers)
      ;; Send a prompt that will generate a long response
      (pi--rpc-async proc
                     '(:type "prompt" :message "Count from 1 to 100 slowly")
                     #'ignore)
      ;; Wait for streaming to start
      (with-timeout (pi-test-rpc-timeout (ert-fail "Timeout waiting for agent_start"))
        (while (not got-agent-start)
          (accept-process-output proc 0.1)))
      ;; Now abort
      (pi--rpc-async proc '(:type "abort") #'ignore)
      ;; Wait for agent_end
      (with-timeout (pi-test-rpc-timeout (ert-fail "Timeout waiting for agent_end after abort"))
        (while (not got-agent-end)
          (accept-process-output proc 0.1)))
      ;; Verify we're no longer streaming
      (let* ((state (pi--rpc-sync proc '(:type "get_state") pi-test-rpc-timeout))
             (data (plist-get state :data)))
        (should (eq (plist-get data :isStreaming) :false))))))

(provide 'pi-integration-test)
;;; pi-integration-test.el ends here
