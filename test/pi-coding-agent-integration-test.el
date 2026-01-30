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

;; NOTE: get_commands RPC requires pi > 0.50.1
;; These tests skip gracefully if the RPC is not available.

(ert-deftest pi-coding-agent-integration-get-commands-succeeds ()
  "get_commands returns successful response."
  (pi-coding-agent-integration-with-process
    (sleep-for 1)
    (let ((response (pi-coding-agent--rpc-sync proc '(:type "get_commands"))))
      ;; Skip if RPC not available (pi <= 0.50.1)
      (unless response
        (ert-skip "get_commands RPC not available (requires pi > 0.50.1)"))
      (should response)
      (should (eq (plist-get response :success) t))
      (should (plist-get response :data)))))

(ert-deftest pi-coding-agent-integration-get-commands-returns-valid-structure ()
  "get_commands returns commands array with valid structure."
  (pi-coding-agent-integration-with-process
    (sleep-for 1)
    (let ((response (pi-coding-agent--rpc-sync proc '(:type "get_commands"))))
      ;; Skip if RPC not available (pi <= 0.50.1)
      (unless response
        (ert-skip "get_commands RPC not available (requires pi > 0.50.1)"))
      (let* ((data (plist-get response :data))
             (commands (plist-get data :commands)))
        ;; Commands should be a vector (JSON array)
        (should (vectorp commands))
        ;; If there are commands, verify structure
        (when (> (length commands) 0)
          (let ((first-cmd (aref commands 0)))
            (should (plist-get first-cmd :name))
            (should (plist-get first-cmd :source))))))))

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
      (pi-coding-agent--rpc-async proc '(:type "prompt" :message "/no_think Say OK") #'ignore)
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
      (pi-coding-agent--rpc-async proc '(:type "prompt" :message "/no_think Say hi") #'ignore)
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
      (pi-coding-agent--rpc-async proc '(:type "prompt" :message "/no_think Say hello") #'ignore)
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
      (pi-coding-agent--rpc-async proc '(:type "prompt" :message "/no_think Say: done") #'ignore)
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
      (pi-coding-agent--rpc-async proc '(:type "prompt" :message "/no_think Say: hello") #'ignore)
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
                     '(:type "prompt" :message "/no_think Count from 1 to 100 slowly")
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

(ert-deftest pi-coding-agent-integration-get-fork-messages-returns-entry-id ()
  "get_fork_messages returns messages with entryId field (not entryIndex).
Catches API breaking changes in the fork message format."
  (pi-coding-agent-integration-with-process
    ;; Empty session should return empty messages array
    (let ((response (pi-coding-agent--rpc-sync proc '(:type "get_fork_messages") pi-coding-agent-test-rpc-timeout)))
      (should (plist-get response :success))
      (let ((messages (plist-get (plist-get response :data) :messages)))
        (should (vectorp messages))
        ;; If there are messages, verify they have entryId not entryIndex
        (when (> (length messages) 0)
          (let ((first-msg (aref messages 0)))
            (should (plist-get first-msg :entryId))
            (should-not (plist-get first-msg :entryIndex))))))))

;;; Message Queuing Tests
;;
;; Only steering uses pi's RPC API.  Follow-up uses a local queue in Emacs,
;; so there's no integration test for it (tested in unit tests instead).

(ert-deftest pi-coding-agent-integration-steer-queues-and-delivers ()
  "Steer message is queued during streaming and delivered after current tool.
Verifies:
1. steer RPC command succeeds
2. message_start with role=user is emitted when delivered
3. The steering message text appears in the event"
  (pi-coding-agent-integration-with-process
    (let ((events nil)
          (got-agent-end nil)
          (user-message-events nil))
      (push (lambda (e)
              (push e events)
              (when (and (equal (plist-get e :type) "message_start")
                         (equal (plist-get (plist-get e :message) :role) "user"))
                (push e user-message-events))
              (when (equal (plist-get e :type) "agent_end")
                (setq got-agent-end t)))
            pi-coding-agent--event-handlers)
      ;; Send initial prompt
      (pi-coding-agent--rpc-async proc '(:type "prompt" :message "/no_think Say: working") #'ignore)
      ;; Wait a moment for streaming to start, then queue steering
      (sleep-for 0.5)
      (let ((queue-response (pi-coding-agent--rpc-sync proc
                              '(:type "steer" :message "/no_think Say: queued-steer-test")
                              pi-coding-agent-test-rpc-timeout)))
        (should (plist-get queue-response :success)))
      ;; Wait for agent_end
      (with-timeout (pi-coding-agent-test-integration-timeout
                     (ert-fail "Timeout waiting for steering message delivery"))
        (while (not got-agent-end)
          (accept-process-output proc 0.1)))
      ;; Verify we got TWO message_start events with role=user
      (should (= (length user-message-events) 2))
      ;; Verify the steering message text appears
      (let ((queued-msg (seq-find
                         (lambda (e)
                           (let* ((msg (plist-get e :message))
                                  (content (plist-get msg :content)))
                             (and content
                                  (> (length content) 0)
                                  (string-match-p "queued-steer-test"
                                                  (or (plist-get (aref content 0) :text) "")))))
                         user-message-events)))
        (should queued-msg)))))

;; Note: Follow-up messages use a local Emacs queue (not pi's RPC follow_up),
;; so there's no integration test for follow-up.  This is simpler and more
;; responsive - the message is displayed immediately when queued, and sent
;; to pi on agent_end.  See unit tests for follow-up queue behavior.
;;
;; Display interleaving (steering message corrupting streaming output) is
;; tested via unit test: pi-coding-agent-test-steering-display-not-interleaved
;;
;; Extension UI requests (extension_ui_request events) are tested via unit
;; tests only.  A true integration test would require a pi extension that
;; makes UI requests, which is outside the scope of core functionality.
;; See: pi-coding-agent-test-extension-ui-* tests in pi-coding-agent-test.el

;;; Session Name Tests

(ert-deftest pi-coding-agent-integration-session-name-persists-across-resume ()
  "Session name set via set-session-name persists and appears in resume picker.
Verifies the full flow:
1. Start session, send a prompt to materialize the session file
2. Set a session name
3. Read file and verify session_info entry was written
4. Call session-metadata and verify name is extracted
5. Call format-session-choice and verify name appears in display"
  (pi-coding-agent-integration-with-process
    ;; Send a prompt to create the session file (it's created lazily)
    (let ((got-agent-end nil))
      (push (lambda (e)
              (when (equal (plist-get e :type) "agent_end")
                (setq got-agent-end t)))
            pi-coding-agent--event-handlers)
      (pi-coding-agent--rpc-async proc '(:type "prompt" :message "/no_think Say: test") #'ignore)
      (with-timeout (pi-coding-agent-test-integration-timeout
                     (ert-fail "Timeout waiting for prompt"))
        (while (not got-agent-end)
          (accept-process-output proc 0.1))))
    ;; Now get session file from state
    (let* ((state-response (pi-coding-agent--rpc-sync proc '(:type "get_state") pi-coding-agent-test-rpc-timeout))
           (session-file (plist-get (plist-get state-response :data) :sessionFile)))
      (should session-file)
      (should (file-exists-p session-file))
      ;; Create a chat buffer to hold state (needed for set-session-name)
      (let ((chat-buf (get-buffer-create "*pi-integration-test-session-name*")))
        (unwind-protect
            (progn
              (with-current-buffer chat-buf
                (pi-coding-agent-chat-mode)
                (setq pi-coding-agent--state (list :session-file session-file))
                (setq pi-coding-agent--process proc)  ; Associate process with buffer
                ;; Set the session name via RPC
                (pi-coding-agent-set-session-name "Integration Test Session")
                ;; Wait for async RPC to complete
                (accept-process-output proc 1.0))
              ;; Verify file contains session_info with our name
              (with-temp-buffer
                (insert-file-contents session-file)
                (should (string-match-p "session_info" (buffer-string)))
                (should (string-match-p "Integration Test Session" (buffer-string))))
              ;; Verify metadata extraction works
              (let ((metadata (pi-coding-agent--session-metadata session-file)))
                (should metadata)
                (should (equal (plist-get metadata :session-name) "Integration Test Session")))
              ;; Verify format-session-choice uses the name
              (let ((choice (pi-coding-agent--format-session-choice session-file)))
                (should (string-match-p "Integration Test Session" (car choice)))))
          (kill-buffer chat-buf))))))

;; Note: "clear session name" test removed - empty string now shows current name
;; instead of clearing (consistent with TUI /name behavior). See unit tests for coverage.

(provide 'pi-coding-agent-integration-test)
;;; pi-coding-agent-integration-test.el ends here
