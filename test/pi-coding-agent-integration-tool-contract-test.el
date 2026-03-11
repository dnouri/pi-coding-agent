;;; pi-coding-agent-integration-tool-contract-test.el --- Shared tool contracts -*- lexical-binding: t; -*-

;;; Commentary:

;; Tool execution remains worth checking at the subprocess boundary, but the
;; contract should stay small and backend-agnostic.

;;; Code:

(require 'ert)
(require 'seq)
(require 'pi-coding-agent-integration-test-common)

(pi-coding-agent-integration-deftest
    (tool-contract-read-turn-emits-shared-tool-events
     :fake-scenario "tool-read-contract")
  "A file-reading turn emits the shared read-tool event surface."
  (let* ((tool-path "/tmp/fake-tool.txt")
         (tool-marker "TOOL_CONTRACT_MARKER")
         (events nil)
         (got-agent-end nil)
         (tool-execution-end nil))
    (unwind-protect
        (progn
          (with-temp-file tool-path
            (insert tool-marker "\n"))
          (push (lambda (event)
                  (push event events)
                  (pcase (plist-get event :type)
                    ("tool_execution_end"
                     (setq tool-execution-end event))
                    ("agent_end"
                     (setq got-agent-end t))))
                pi-coding-agent--event-handlers)
          (let ((prompt-response
                 (pi-coding-agent--rpc-sync
                  proc
                  `(:type "prompt"
                    :message ,(format
                               "/no_think Use the read tool to read the file %s."
                               tool-path))
                  pi-coding-agent-test-rpc-timeout)))
            (should prompt-response)
            (should (eq (plist-get prompt-response :success) t))
            (should (equal (plist-get prompt-response :command) "prompt")))
          (with-timeout (pi-coding-agent-test-integration-timeout
                         (ert-fail "Timeout waiting for tool execution to finish"))
            (while (not tool-execution-end)
              (accept-process-output proc pi-coding-agent-test-poll-interval)))
          (setq events (nreverse events))
          (let* ((toolcall-start
                  (seq-find (lambda (event)
                              (and (equal (plist-get event :type) "message_update")
                                   (equal (plist-get (plist-get event :assistantMessageEvent)
                                                     :type)
                                          "toolcall_start")))
                            events))
                 (tool-execution-start
                  (seq-find (lambda (event)
                              (equal (plist-get event :type) "tool_execution_start"))
                            events))
                 (result-text
                  (and tool-execution-end
                       (pi-coding-agent-integration--message-text
                        (plist-get tool-execution-end :result)))))
            (should toolcall-start)
            (should tool-execution-start)
            (should tool-execution-end)
            (should (equal (plist-get tool-execution-start :toolName) "read"))
            (should (equal (plist-get (plist-get tool-execution-start :args) :path)
                           tool-path))
            (should result-text)
            (should (string-match-p tool-marker result-text)))
          (unless got-agent-end
            (let ((abort-response (pi-coding-agent--rpc-sync proc '(:type "abort")
                                                             pi-coding-agent-test-rpc-timeout)))
              (should abort-response)
              (should (eq (plist-get abort-response :success) t))
              (should (equal (plist-get abort-response :command) "abort")))
            (with-timeout (pi-coding-agent-test-rpc-timeout
                           (ert-fail "Timeout waiting for agent_end after tool-contract abort"))
              (while (not got-agent-end)
                (accept-process-output proc pi-coding-agent-test-poll-interval))))
          (let* ((state (pi-coding-agent--rpc-sync proc '(:type "get_state")
                                                   pi-coding-agent-test-rpc-timeout))
                 (data (plist-get state :data)))
            (should (eq (plist-get data :isStreaming) :false))))
      (delete-file tool-path))))

(provide 'pi-coding-agent-integration-tool-contract-test)
;;; pi-coding-agent-integration-tool-contract-test.el ends here
