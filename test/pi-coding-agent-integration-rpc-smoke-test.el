;;; pi-coding-agent-integration-rpc-smoke-test.el --- Cheap RPC smoke tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Fast protocol canaries that should stay small and diagnostic.  These tests
;; exercise process startup and cheap request/response compatibility on both
;; backends.

;;; Code:

(require 'ert)
(require 'pi-coding-agent-integration-test-common)

(pi-coding-agent-integration-deftest
    (rpc-smoke-process-starts)
  "The backend process starts and stays alive."
  (should (processp proc))
  (should (process-live-p proc)))

(pi-coding-agent-integration-deftest
    (rpc-smoke-process-query-on-exit)
  "The backend process keeps query-on-exit enabled for session buffers."
  (should (process-query-on-exit-flag proc)))

(pi-coding-agent-integration-deftest
    (rpc-smoke-get-state-succeeds)
  "`get_state' returns a successful response with a state payload."
  (let ((response (pi-coding-agent--rpc-sync proc '(:type "get_state") 10)))
    (should response)
    (should (eq (plist-get response :success) t))
    (should (plist-get response :data))))

(pi-coding-agent-integration-deftest
    (rpc-smoke-get-state-has-model)
  "`get_state' exposes model information consumed by the frontend."
  (let* ((response (pi-coding-agent--rpc-sync proc '(:type "get_state") 10))
         (data (plist-get response :data)))
    (should (plist-get data :model))))

(pi-coding-agent-integration-deftest
    (rpc-smoke-get-state-has-thinking-level)
  "`get_state' exposes the current thinking level."
  (let* ((response (pi-coding-agent--rpc-sync proc '(:type "get_state") 10))
         (data (plist-get response :data)))
    (should (plist-get data :thinkingLevel))))

(pi-coding-agent-integration-deftest
    (rpc-smoke-get-commands-succeeds)
  "`get_commands' eventually succeeds once command discovery has settled."
  (let ((response (pi-coding-agent-integration--rpc-until
                   proc '(:type "get_commands")
                   (lambda (candidate)
                     (and candidate
                          (eq (plist-get candidate :success) t)))
                   5)))
    (should response)
    (should (eq (plist-get response :success) t))
    (should (plist-get response :data))))

(pi-coding-agent-integration-deftest
    (rpc-smoke-get-commands-returns-valid-structure :fake-scenario "extension-confirm")
  "`get_commands' returns a commands vector with frontend-visible fields."
  (let* ((response (pi-coding-agent-integration--rpc-until
                    proc '(:type "get_commands")
                    (lambda (candidate)
                      (and candidate
                           (eq (plist-get candidate :success) t)))
                    5))
         (data (plist-get response :data))
         (commands (plist-get data :commands)))
    (should response)
    (should (vectorp commands))
    (when (> (length commands) 0)
      (let ((first-cmd (aref commands 0)))
        (should (plist-get first-cmd :name))
        (should (plist-get first-cmd :source))))))

(pi-coding-agent-integration-deftest
    (rpc-smoke-get-commands-has-source-info :fake-scenario "extension-confirm")
  "`get_commands' carries sourceInfo with scope on the wire.
Sentinel: fails when the wire format drops scope metadata."
  (let* ((response (pi-coding-agent-integration--rpc-until
                    proc '(:type "get_commands")
                    (lambda (candidate)
                      (and candidate
                           (eq (plist-get candidate :success) t)
                           (let* ((data (plist-get candidate :data))
                                  (cmds (plist-get data :commands)))
                             (and (vectorp cmds) (> (length cmds) 0)))))
                    5))
         (data (plist-get response :data))
         (commands (plist-get data :commands)))
    (when (or (null commands) (= (length commands) 0))
      (ert-skip "No commands available (real backend has no extensions)"))
    (let* ((first-cmd (aref commands 0))
           (source-info (plist-get first-cmd :sourceInfo)))
      (should source-info)
      (should (plist-get source-info :scope)))))

(pi-coding-agent-integration-deftest
    (rpc-smoke-new-session-succeeds)
  "`new_session' succeeds and leaves a fresh session at zero messages."
  (let* ((before (pi-coding-agent--rpc-sync proc '(:type "get_state")
                                            pi-coding-agent-test-rpc-timeout))
         (before-count (plist-get (plist-get before :data) :messageCount)))
    (should (= before-count 0))
    (let ((response (pi-coding-agent--rpc-sync proc '(:type "new_session")
                                               pi-coding-agent-test-rpc-timeout)))
      (should (plist-get response :success))
      (should (eq (plist-get (plist-get response :data) :cancelled) :false)))
    (let* ((after (pi-coding-agent--rpc-sync proc '(:type "get_state")
                                             pi-coding-agent-test-rpc-timeout))
           (after-count (plist-get (plist-get after :data) :messageCount)))
      (should (= after-count 0)))))

(pi-coding-agent-integration-deftest
    (rpc-smoke-get-fork-messages-returns-entry-id)
  "`get_fork_messages' returns an API-compatible message vector."
  (let* ((response (pi-coding-agent--rpc-sync proc '(:type "get_fork_messages")
                                              pi-coding-agent-test-rpc-timeout))
         (messages (plist-get (plist-get response :data) :messages)))
    (should (plist-get response :success))
    (should (vectorp messages))
    (when (> (length messages) 0)
      (let ((first-msg (aref messages 0)))
        (should (plist-get first-msg :entryId))
        (should-not (plist-get first-msg :entryIndex))))))

(provide 'pi-coding-agent-integration-rpc-smoke-test)
;;; pi-coding-agent-integration-rpc-smoke-test.el ends here
