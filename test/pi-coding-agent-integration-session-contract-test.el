;;; pi-coding-agent-integration-session-contract-test.el --- Shared session contracts -*- lexical-binding: t; -*-

;;; Commentary:

;; Session persistence behaviors that remain valuable at the subprocess
;; boundary, even though session metadata formatting is already unit-tested.

;;; Code:

(require 'ert)
(require 'pi-coding-agent-integration-test-common)

(pi-coding-agent-integration-deftest
    (session-contract-name-persists-across-session-file)
  "Setting a session name persists backend-visible session metadata."
  (let ((got-agent-end nil))
    (push (lambda (event)
            (when (equal (plist-get event :type) "agent_end")
              (setq got-agent-end t)))
          pi-coding-agent--event-handlers)
    (let ((prompt-response (pi-coding-agent--rpc-sync
                            proc
                            `(:type "prompt"
                              :message
                              ,pi-coding-agent-integration--prompt-session-materialize-message)
                            pi-coding-agent-test-rpc-timeout)))
      (should prompt-response)
      (should (eq (plist-get prompt-response :success) t)))
    (with-timeout (pi-coding-agent-test-integration-timeout
                   (ert-fail "Timeout waiting for session-materializing prompt"))
      (while (not got-agent-end)
        (accept-process-output proc pi-coding-agent-test-poll-interval)))
    (let* ((state-before (pi-coding-agent--rpc-sync proc '(:type "get_state")
                                                    pi-coding-agent-test-rpc-timeout))
           (data-before (plist-get state-before :data))
           (session-file (plist-get data-before :sessionFile)))
      (should session-file)
      (should (file-exists-p session-file))
      (let ((name-response (pi-coding-agent--rpc-sync
                            proc
                            '(:type "set_session_name" :name "Integration Test Session")
                            pi-coding-agent-test-rpc-timeout)))
        (should name-response)
        (should (eq (plist-get name-response :success) t))
        (should (equal (plist-get name-response :command) "set_session_name")))
      (let* ((state-after (pi-coding-agent--rpc-sync proc '(:type "get_state")
                                                     pi-coding-agent-test-rpc-timeout))
             (data-after (plist-get state-after :data)))
        (should (equal (plist-get data-after :sessionFile) session-file))
        (should (equal (plist-get data-after :sessionName)
                       "Integration Test Session")))
      (with-temp-buffer
        (insert-file-contents session-file)
        (should (string-match-p "session_info" (buffer-string)))
        (should (string-match-p "Integration Test Session" (buffer-string)))))))

(provide 'pi-coding-agent-integration-session-contract-test)
;;; pi-coding-agent-integration-session-contract-test.el ends here
