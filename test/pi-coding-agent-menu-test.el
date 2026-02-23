;;; pi-coding-agent-menu-test.el --- Tests for pi-coding-agent-menu -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>

;;; Commentary:

;; Tests for session management, transient menu, model/thinking commands,
;; reconnect, and slash commands via RPC — the menu and session layer.

;;; Code:

(require 'ert)
(require 'pi-coding-agent)
(require 'pi-coding-agent-test-common)

;;; Session Management

(ert-deftest pi-coding-agent-test-buffer-name-default-session ()
  "Buffer name without session name."
  (should (equal (pi-coding-agent--buffer-name :chat "/tmp/proj/" nil)
                 "*pi-coding-agent-chat:/tmp/proj/*")))

(ert-deftest pi-coding-agent-test-buffer-name-named-session ()
  "Buffer name with session name."
  (should (equal (pi-coding-agent--buffer-name :chat "/tmp/proj/" "feature")
                 "*pi-coding-agent-chat:/tmp/proj/<feature>*")))

(ert-deftest pi-coding-agent-test-clear-chat-buffer-resets-to-startup ()
  "Clearing chat buffer shows startup header and resets state."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Add some content
    (let ((inhibit-read-only t))
      (insert "Some existing content\nMore content"))
    ;; Set markers as if streaming happened
    (setq pi-coding-agent--message-start-marker (point-marker))
    (setq pi-coding-agent--streaming-marker (point-marker))
    ;; Clear the buffer
    (pi-coding-agent--clear-chat-buffer)
    ;; Should have startup header
    (should (string-match-p "C-c C-c" (buffer-string)))
    ;; Markers should be reset
    (should (null pi-coding-agent--message-start-marker))
    (should (null pi-coding-agent--streaming-marker))))

(ert-deftest pi-coding-agent-test-clear-chat-buffer-resets-usage ()
  "Clearing chat buffer resets pi-coding-agent--last-usage to nil."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Set usage as if messages were received
    (setq pi-coding-agent--last-usage '(:input 5000 :output 1000 :cacheRead 500 :cacheWrite 100))
    ;; Clear the buffer
    (pi-coding-agent--clear-chat-buffer)
    ;; Usage should be reset
    (should (null pi-coding-agent--last-usage))))

(ert-deftest pi-coding-agent-test-clear-chat-buffer-resets-session-state ()
  "Clearing chat buffer resets all session-specific state."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Set various session state as if we had an active session
    (setq pi-coding-agent--session-name "My Named Session"
          pi-coding-agent--cached-stats '(:messages 10 :cost 0.05)
          pi-coding-agent--last-usage '(:input 5000 :output 1000)
          pi-coding-agent--assistant-header-shown t
          pi-coding-agent--followup-queue '("pending message")
          pi-coding-agent--local-user-message "user text"
          pi-coding-agent--aborted t
          pi-coding-agent--extension-status '(("ext1" . "status"))
          pi-coding-agent--working-message "Reading README..."
          pi-coding-agent--message-start-marker (point-marker)
          pi-coding-agent--streaming-marker (point-marker)
          pi-coding-agent--thinking-marker (point-marker)
          pi-coding-agent--thinking-start-marker (point-marker)
          pi-coding-agent--thinking-raw "pending"
          pi-coding-agent--in-code-block t
          pi-coding-agent--in-thinking-block t
          pi-coding-agent--line-parse-state 'code-fence
          pi-coding-agent--pending-tool-overlay (make-overlay 1 1)
          pi-coding-agent--activity-phase "running")
    ;; Add entry to tool-args-cache
    (puthash "tool-1" '(:path "/test") pi-coding-agent--tool-args-cache)
    ;; Clear the buffer
    (pi-coding-agent--clear-chat-buffer)
    ;; All session state should be reset
    (should (null pi-coding-agent--session-name))
    (should (null pi-coding-agent--cached-stats))
    (should (null pi-coding-agent--last-usage))
    (should (null pi-coding-agent--assistant-header-shown))
    (should (null pi-coding-agent--followup-queue))
    (should (null pi-coding-agent--local-user-message))
    (should (null pi-coding-agent--aborted))
    (should (null pi-coding-agent--extension-status))
    (should (null pi-coding-agent--working-message))
    (should (null pi-coding-agent--message-start-marker))
    (should (null pi-coding-agent--streaming-marker))
    (should (null pi-coding-agent--thinking-marker))
    (should (null pi-coding-agent--thinking-start-marker))
    (should (null pi-coding-agent--thinking-raw))
    (should (null pi-coding-agent--in-code-block))
    (should (null pi-coding-agent--in-thinking-block))
    (should (eq pi-coding-agent--line-parse-state 'line-start))
    (should (null pi-coding-agent--pending-tool-overlay))
    (should (equal pi-coding-agent--activity-phase "idle"))
    ;; Tool args cache should be empty
    (should (= 0 (hash-table-count pi-coding-agent--tool-args-cache)))))

(ert-deftest pi-coding-agent-test-new-session-clears-buffer-from-different-context ()
  "New session clears buffer and updates state even when callback runs elsewhere.
This tests that the async callback properly captures the chat buffer reference,
not relying on current buffer context which may change before callback executes.
Also verifies that the new session-file is stored in state for reload to work."
  (let ((chat-buf (generate-new-buffer "*pi-coding-agent-chat:/tmp/test-new-session/*"))
        (captured-callback nil))
    (unwind-protect
        (progn
          ;; Set up chat buffer with content and old state
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--state '(:session-file "/tmp/old-session.jsonl"))
            (let ((inhibit-read-only t))
              (insert "Existing conversation content\nMore content here")))
          ;; Mock the RPC to capture the new_session callback and handle get_state
          (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                    ((symbol-function 'pi-coding-agent--get-chat-buffer) (lambda () chat-buf))
                    ((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc cmd cb)
                       (cond
                        ((equal (plist-get cmd :type) "new_session")
                         (setq captured-callback cb))
                        ((equal (plist-get cmd :type) "get_state")
                         (funcall cb '(:success t :data (:sessionFile "/tmp/new-session.jsonl")))))))
                    ((symbol-function 'pi-coding-agent--refresh-header) #'ignore))
            ;; Call new-session from the chat buffer
            (with-current-buffer chat-buf
              (pi-coding-agent-new-session))
            ;; Simulate callback being called from a DIFFERENT buffer
            ;; (This is what happens in practice - callbacks run in arbitrary contexts)
            (with-temp-buffer
              (funcall captured-callback '(:success t :data (:cancelled :false)))))
          ;; Verify buffer was cleared
          (with-current-buffer chat-buf
            (should-not (string-match-p "Existing conversation" (buffer-string)))
            (should (string-match-p "C-c C-c" (buffer-string)))
            ;; Verify state was updated with new session file (the actual bug fix)
            (should (equal (plist-get pi-coding-agent--state :session-file)
                           "/tmp/new-session.jsonl"))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pi-coding-agent-test-find-session-returns-existing ()
  "pi-coding-agent--find-session returns existing chat buffer."
  (let ((buf (generate-new-buffer "*pi-coding-agent-chat:/tmp/test-find/*")))
    (unwind-protect
        (should (eq (pi-coding-agent--find-session "/tmp/test-find/" nil) buf))
      (kill-buffer buf))))

(ert-deftest pi-coding-agent-test-find-session-returns-nil-when-missing ()
  "pi-coding-agent--find-session returns nil when no session exists."
  (should (null (pi-coding-agent--find-session "/tmp/nonexistent-session-xyz/" nil))))

(ert-deftest pi-coding-agent-test-pi-coding-agent-reuses-existing-session ()
  "Calling pi twice returns same buffers."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-reuse/"
    (let ((chat1 (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-reuse/*"))
          (input1 (get-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-reuse/*")))
      (pi-coding-agent)  ; call again
      (should (eq chat1 (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-reuse/*")))
      (should (eq input1 (get-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-reuse/*"))))))

(ert-deftest pi-coding-agent-test-named-session-separate-from-default ()
  "Named session creates separate buffers from default."
  (let ((default-directory "/tmp/pi-coding-agent-test-named/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
              ((symbol-function 'pi-coding-agent--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (pi-coding-agent)  ; default session
            (pi-coding-agent "feature")  ; named session
            (should (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-named/*"))
            (should (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-named/<feature>*"))
            (should-not (eq (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-named/*")
                            (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-named/<feature>*"))))
        (ignore-errors (kill-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-named/*"))
        (ignore-errors (kill-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-named/*"))
        (ignore-errors (kill-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-named/<feature>*"))
        (ignore-errors (kill-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-named/<feature>*"))))))

(ert-deftest pi-coding-agent-test-named-session-from-existing-pi-coding-agent-buffer ()
  "Creating named session while in pi buffer creates new session, not reuse."
  (let ((default-directory "/tmp/pi-coding-agent-test-from-pi/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
              ((symbol-function 'pi-coding-agent--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (pi-coding-agent)  ; default session
            ;; Now switch INTO the pi input buffer and create a named session
            (with-current-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-from-pi/*"
              (pi-coding-agent "feature"))  ; should create NEW session
            ;; Both sessions should exist
            (should (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-from-pi/*"))
            (should (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-from-pi/<feature>*"))
            ;; They should be different buffers
            (should-not (eq (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-from-pi/*")
                            (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-from-pi/<feature>*"))))
        (ignore-errors (kill-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-from-pi/*"))
        (ignore-errors (kill-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-from-pi/*"))
        (ignore-errors (kill-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-from-pi/<feature>*"))
        (ignore-errors (kill-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-from-pi/<feature>*"))))))

(ert-deftest pi-coding-agent-test-quit-kills-both-buffers ()
  "pi-coding-agent-quit kills both chat and input buffers."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-quit/"
    (with-current-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-quit/*"
      (pi-coding-agent-quit))
    (should-not (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-quit/*"))
    (should-not (get-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-quit/*"))))

(ert-deftest pi-coding-agent-test-quit-cancelled-preserves-session ()
  "When user cancels quit confirmation, both buffers remain intact and linked."
  (let* ((dir "/tmp/pi-coding-agent-test-quit-cancel/")
         (chat-name (concat "*pi-coding-agent-chat:" dir "*"))
         (input-name (concat "*pi-coding-agent-input:" dir "*"))
         (fake-proc nil))
    (make-directory dir t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process)
               (lambda (_)
                 ;; Create a real process that triggers kill confirmation
                 (setq fake-proc (start-process "pi-test-quit" nil "cat"))
                 (set-process-query-on-exit-flag fake-proc t)
                 fake-proc))
              ((symbol-function 'pi-coding-agent--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (let ((default-directory dir))
              (pi-coding-agent))
            ;; Associate process with chat buffer (as the real code does)
            (with-current-buffer chat-name
              (set-process-buffer fake-proc (current-buffer)))
            ;; User says "no" to quit confirmation - expect user-error
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
              (with-current-buffer input-name
                (should-error (pi-coding-agent-quit) :type 'user-error)))
            ;; Both buffers should still exist
            (should (get-buffer chat-name))
            (should (get-buffer input-name))
            ;; Buffers should still be linked
            (with-current-buffer chat-name
              (should (eq (pi-coding-agent--get-input-buffer)
                          (get-buffer input-name))))
            (with-current-buffer input-name
              (should (eq (pi-coding-agent--get-chat-buffer)
                          (get-buffer chat-name)))))
        ;; Cleanup
        (when (and fake-proc (process-live-p fake-proc))
          (delete-process fake-proc))
        (ignore-errors (kill-buffer chat-name))
        (ignore-errors (kill-buffer input-name))))))

(ert-deftest pi-coding-agent-test-quit-confirmed-kills-both ()
  "When user confirms quit, both buffers are killed without double-prompting."
  (let* ((dir "/tmp/pi-coding-agent-test-quit-confirm/")
         (chat-name (concat "*pi-coding-agent-chat:" dir "*"))
         (input-name (concat "*pi-coding-agent-input:" dir "*"))
         (fake-proc nil)
         (prompt-count 0))
    (make-directory dir t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process)
               (lambda (_)
                 (setq fake-proc (start-process "pi-test-quit" nil "cat"))
                 (set-process-query-on-exit-flag fake-proc t)
                 fake-proc))
              ((symbol-function 'pi-coding-agent--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (let ((default-directory dir))
              (pi-coding-agent))
            (with-current-buffer chat-name
              (set-process-buffer fake-proc (current-buffer)))
            ;; User says "yes" - count how many times we're asked
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (_)
                         (cl-incf prompt-count)
                         t)))
              (with-current-buffer input-name
                (pi-coding-agent-quit)))
            ;; Both buffers should be gone
            (should-not (get-buffer chat-name))
            (should-not (get-buffer input-name))
            ;; Should only be asked once (not twice due to cross-linked hooks)
            (should (<= prompt-count 1)))
        ;; Cleanup
        (when (and fake-proc (process-live-p fake-proc))
          (delete-process fake-proc))
        (ignore-errors (kill-buffer chat-name))
        (ignore-errors (kill-buffer input-name))))))

(ert-deftest pi-coding-agent-test-kill-chat-kills-input ()
  "Killing chat buffer also kills input buffer."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-linked/"
    (kill-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-linked/*")
    (should-not (get-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-linked/*"))))

(ert-deftest pi-coding-agent-test-kill-input-kills-chat ()
  "Killing input buffer also kills chat buffer."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-linked2/"
    (kill-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-linked2/*")
    (should-not (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-linked2/*"))))

;;; Transient Menu

(ert-deftest pi-coding-agent-test-transient-bound-to-key ()
  "C-c C-p is bound to pi-coding-agent-menu in input mode."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should (eq (key-binding (kbd "C-c C-p")) 'pi-coding-agent-menu))))

;;; Chat Navigation

(ert-deftest pi-coding-agent-test-chat-has-navigation-keys ()
  "Chat mode has n/p for navigation, TAB for folding, f for fork."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (should (eq (key-binding "n") 'pi-coding-agent-next-message))
    (should (eq (key-binding "p") 'pi-coding-agent-previous-message))
    (should (eq (key-binding (kbd "TAB")) 'pi-coding-agent-toggle-tool-section))
    (should (eq (key-binding "f") 'pi-coding-agent-fork-at-point))))

;;; Reconnect Tests

(ert-deftest pi-coding-agent-test-reload-restarts-process ()
  "Reload starts new process when old process is dead."
  (let* ((started-new-process nil)
         (switch-session-called nil)
         (session-path-used nil)
         (chat-buf (get-buffer-create "*pi-coding-agent-test-reconnect-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            ;; Set up state with session file (simulating previous get_state)
            (setq pi-coding-agent--state '(:session-file "/tmp/test-session.json"
                                           :model (:name "test-model")))
            ;; Set up dead process
            (let ((dead-proc (start-process "test-dead" nil "true")))
              (should (pi-coding-agent-test-wait-for-process-exit dead-proc))
              (setq pi-coding-agent--process dead-proc))
            ;; Mock functions
            (cl-letf (((symbol-function 'pi-coding-agent--start-process)
                       (lambda (_dir)
                         (setq started-new-process t)
                         (start-process "test-new" nil "cat")))
                      ((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc msg _cb)
                         (when (equal (plist-get msg :type) "switch_session")
                           (setq switch-session-called t
                                 session-path-used (plist-get msg :sessionPath))))))
              ;; Call reload
              (pi-coding-agent-reload)
              ;; Verify
              (should started-new-process)
              (should switch-session-called)
              (should (equal session-path-used "/tmp/test-session.json")))))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (when (and pi-coding-agent--process (process-live-p pi-coding-agent--process))
            (delete-process pi-coding-agent--process)))
        (kill-buffer chat-buf)))))

(ert-deftest pi-coding-agent-test-reload-works-when-process-alive ()
  "Reload restarts even when process is alive (handles hung process)."
  (let* ((started-new-process nil)
         (old-process-killed nil)
         (chat-buf (get-buffer-create "*pi-coding-agent-test-reload-alive-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            ;; Set up state with session file
            (setq pi-coding-agent--state '(:session-file "/tmp/test-session.json"))
            ;; Set up alive process
            (let ((alive-proc (start-process "test-alive" nil "cat")))
              (setq pi-coding-agent--process alive-proc)
              (cl-letf (((symbol-function 'pi-coding-agent--start-process)
                         (lambda (_dir)
                           (setq started-new-process t)
                           (start-process "test-new" nil "cat")))
                        ((symbol-function 'pi-coding-agent--rpc-async)
                         (lambda (_proc _msg _cb) nil)))
                ;; Call reload
                (pi-coding-agent-reload)
                ;; Verify - SHOULD start new process even when old was alive
                (should started-new-process)
                ;; Old process should be killed
                (should-not (process-live-p alive-proc))))))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (when (and pi-coding-agent--process (process-live-p pi-coding-agent--process))
            (delete-process pi-coding-agent--process)))
        (kill-buffer chat-buf)))))

(ert-deftest pi-coding-agent-test-reload-fails-without-session-file ()
  "Reload shows error when no session file in state."
  (let* ((error-shown nil)
         (chat-buf (get-buffer-create "*pi-coding-agent-test-reconnect-no-session*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            ;; State without session file
            (setq pi-coding-agent--state '(:model (:name "test-model")))
            ;; Dead process
            (let ((dead-proc (start-process "test-dead" nil "true")))
              (should (pi-coding-agent-test-wait-for-process-exit dead-proc))
              (setq pi-coding-agent--process dead-proc))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest _args)
                         (when (string-match-p "No session" fmt)
                           (setq error-shown t)))))
              (pi-coding-agent-reload)
              (should error-shown))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pi-coding-agent-test-send-resets-activity-when-process-dead ()
  "Sending when process is dead resets activity phase and status."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-process-dead*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-process-dead-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--input-buffer input-buf
                  pi-coding-agent--activity-phase "running"
                  pi-coding-agent--status 'idle)
            ;; Set up dead process
            (let ((dead-proc (start-process "test-dead" nil "true")))
              (should (pi-coding-agent-test-wait-for-process-exit dead-proc))
              (setq pi-coding-agent--process dead-proc)))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "test message")
            (pi-coding-agent-send))
          ;; Verify activity phase and status reset
          (with-current-buffer chat-buf
            (should (equal pi-coding-agent--activity-phase "idle"))
            (should (eq pi-coding-agent--status 'idle))))
      (when (buffer-live-p chat-buf) (kill-buffer chat-buf))
      (when (buffer-live-p input-buf) (kill-buffer input-buf)))))

;;; Slash Commands via RPC (get_commands)

(ert-deftest pi-coding-agent-test-fetch-commands-parses-response ()
  "fetch-commands extracts command list from RPC response."
  (let* ((callback-result nil)
         (mock-response '(:success t
                          :data (:commands
                                 [(:name "fix-tests" :description "Fix tests" :source "prompt")
                                  (:name "session-name" :description "Set name" :source "extension")])))
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                   (lambda (_proc _msg callback)
                     (funcall callback mock-response))))
          (pi-coding-agent--fetch-commands fake-proc
            (lambda (commands)
              (setq callback-result commands)))
          ;; Verify commands were extracted correctly
          (should (= (length callback-result) 2))
          (should (equal (plist-get (car callback-result) :name) "fix-tests"))
          (should (equal (plist-get (cadr callback-result) :source) "extension")))
      (delete-process fake-proc))))

(ert-deftest pi-coding-agent-test-fetch-commands-handles-failure ()
  "fetch-commands does not call callback on RPC failure."
  (let* ((callback-called nil)
         (mock-response '(:success :false :error "Connection failed"))
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                   (lambda (_proc _msg callback)
                     (funcall callback mock-response))))
          (pi-coding-agent--fetch-commands fake-proc
            (lambda (_) (setq callback-called t)))
          (should-not callback-called))
      (delete-process fake-proc))))

(ert-deftest pi-coding-agent-test-set-commands-propagates-to-input ()
  "set-commands propagates commands to input buffer."
  (with-temp-buffer
    (let* ((input-buf (generate-new-buffer "*test-input*"))
           (pi-coding-agent--input-buffer input-buf)
           (commands '((:name "test" :description "Test cmd" :source "prompt"))))
      (unwind-protect
          (progn
            (pi-coding-agent--set-commands commands)
            ;; Verify local variable set in current buffer
            (should (equal pi-coding-agent--commands commands))
            ;; Verify propagated to input buffer
            (should (equal (buffer-local-value 'pi-coding-agent--commands input-buf)
                           commands)))
        (kill-buffer input-buf)))))

(ert-deftest pi-coding-agent-test-command-capf-uses-commands ()
  "command-capf completion uses pi-coding-agent--commands."
  (with-temp-buffer
    (let ((pi-coding-agent--commands
           '((:name "fix-tests" :description "Fix" :source "prompt")
             (:name "review" :description "Review" :source "prompt"))))
      (insert "/")
      (let ((completion (pi-coding-agent--command-capf)))
        (should completion)
        ;; Third element is the completion candidates
        (should (member "fix-tests" (nth 2 completion)))
        (should (member "review" (nth 2 completion)))))))

(ert-deftest pi-coding-agent-test-run-custom-command-sends-literal ()
  "run-custom-command sends literal /command text, not expanded."
  (let* ((sent-message nil)
         (fake-proc (start-process "test" nil "cat"))
         (cmd '(:name "greet" :description "Greet" :source "prompt")))
    (unwind-protect
        (with-temp-buffer
          (pi-coding-agent-chat-mode)
          (let ((pi-coding-agent--process fake-proc))
            (cl-letf (((symbol-function 'pi-coding-agent--get-chat-buffer)
                       (lambda () (current-buffer)))
                      ((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc msg _cb)
                         (setq sent-message (plist-get msg :message))))
                      ((symbol-function 'read-string)
                       (lambda (&rest _args) "world")))
              (pi-coding-agent--run-custom-command cmd)
              ;; Should send literal /greet world, NOT expanded prompt
              (should (equal sent-message "/greet world")))))
      (delete-process fake-proc))))

(ert-deftest pi-coding-agent-test-run-custom-command-empty-args ()
  "run-custom-command with empty args sends just /command."
  ;; Note: Use "mycommand" not "compact" to avoid collision with built-in /compact handling
  (let* ((sent-message nil)
         (fake-proc (start-process "test" nil "cat"))
         (cmd '(:name "mycommand" :description "My Command" :source "extension")))
    (unwind-protect
        (with-temp-buffer
          (pi-coding-agent-chat-mode)
          (let ((pi-coding-agent--process fake-proc))
            (cl-letf (((symbol-function 'pi-coding-agent--get-chat-buffer)
                       (lambda () (current-buffer)))
                      ((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc msg _cb)
                         (setq sent-message (plist-get msg :message))))
                      ((symbol-function 'read-string)
                       (lambda (&rest _args) "")))
              (pi-coding-agent--run-custom-command cmd)
              ;; Should send just /mycommand without trailing space
              (should (equal sent-message "/mycommand")))))
      (delete-process fake-proc))))

(ert-deftest pi-coding-agent-test-rebuild-menu-shows-prompt-source-as-templates ()
  "rebuild-commands-menu creates Templates section for source \"prompt\".
Pi v0.51.3+ renamed SlashCommandSource from \"template\" to \"prompt\"."
  (let ((pi-coding-agent--commands
         '((:name "fix-tests" :description "Fix tests" :source "prompt" :location "user")
           (:name "review" :description "Code review" :source "prompt" :location "project"))))
    (unwind-protect
        (progn
          (pi-coding-agent--rebuild-commands-menu)
          (should (transient-get-suffix 'pi-coding-agent-menu '(4))))
      (ignore-errors (transient-remove-suffix 'pi-coding-agent-menu '(4))))))

(defun pi-coding-agent-test--suffix-key-bound-p (key)
  "Return non-nil if KEY is bound in current transient suffixes."
  (cl-find-if (lambda (obj) (equal (oref obj key) key))
              transient--suffixes))

(ert-deftest pi-coding-agent-test-submenus-open-with-no-commands ()
  "All submenus open without error when no commands are loaded."
  (let ((pi-coding-agent--commands nil))
    (dolist (menu '(pi-coding-agent-templates-menu
                    pi-coding-agent-extensions-menu
                    pi-coding-agent-skills-menu))
      (transient-setup menu))))

(ert-deftest pi-coding-agent-test-templates-menu-shows-run-keys ()
  "Templates submenu binds letter keys to commands."
  (let ((pi-coding-agent--commands
         '((:name "test-tmpl" :description "A template" :source "prompt"))))
    (transient-setup 'pi-coding-agent-templates-menu)
    (should (pi-coding-agent-test--suffix-key-bound-p "a"))))

(ert-deftest pi-coding-agent-test-templates-menu-shows-edit-keys ()
  "Templates submenu binds uppercase letter keys to edit file paths."
  (let ((pi-coding-agent--commands
         '((:name "uncle-bob" :description "Uncle Bob review"
            :source "prompt" :path "/tmp/uncle-bob.md" :location "user")
           (:name "fix-tests" :description "Fix tests"
            :source "prompt" :path "/tmp/fix-tests.md" :location "project"))))
    (transient-setup 'pi-coding-agent-templates-menu)
    (should (pi-coding-agent-test--suffix-key-bound-p "a"))
    (should (pi-coding-agent-test--suffix-key-bound-p "A"))))

(ert-deftest pi-coding-agent-test-stats-uses-i-key-not-S ()
  "Stats is bound to `i' so it doesn't conflict with Skills `S' key."
  (transient-setup 'pi-coding-agent-menu)
  (should (pi-coding-agent-test--suffix-key-bound-p "i"))
  (should-not (pi-coding-agent-test--suffix-key-bound-p "S")))

(ert-deftest pi-coding-agent-test-submenu-handles-more-than-9-commands ()
  "Submenu with 13 skills uses letter keys without crashing."
  (let ((pi-coding-agent--commands
         (cl-loop for i from 1 to 13
                  collect (list :name (format "skill-%d" i)
                                :description (format "Skill number %d" i)
                                :source "skill"
                                :location "user"))))
    ;; Should not signal an error
    (transient-setup 'pi-coding-agent-skills-menu)
    ;; First and last should be bound
    (should (pi-coding-agent-test--suffix-key-bound-p "a"))
    (should (pi-coding-agent-test--suffix-key-bound-p "m"))))

(ert-deftest pi-coding-agent-test-submenu-run-and-edit-keys-correspond ()
  "Run key `a' and edit key `A' refer to the same command."
  (let ((pi-coding-agent--commands
         '((:name "alpha" :description "First" :source "skill"
            :location "user" :path "/tmp/alpha.md")
           (:name "beta" :description "Second" :source "skill"
            :location "user" :path "/tmp/beta.md"))))
    (transient-setup 'pi-coding-agent-skills-menu)
    ;; Run keys a, b and edit keys A, B should all be bound
    (should (pi-coding-agent-test--suffix-key-bound-p "a"))
    (should (pi-coding-agent-test--suffix-key-bound-p "b"))
    (should (pi-coding-agent-test--suffix-key-bound-p "A"))
    (should (pi-coding-agent-test--suffix-key-bound-p "B"))))

;;; Manual Compaction

(ert-deftest pi-coding-agent-test-compact-sets-status-and-processes-queued-followup ()
  "Manual compact marks session compacting and drains local follow-up queue on success."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-compact-status*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-compact-status-input*"))
        (compact-callback nil)
        (prepared-text nil)
        (prompt-sent nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--process nil)
            (setq pi-coding-agent--input-buffer input-buf)
            (setq pi-coding-agent--followup-queue nil))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf))
          (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                     (lambda () 'mock-proc))
                    ((symbol-function 'process-live-p)
                     (lambda (_proc) t))
                    ((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc cmd cb)
                       (if (equal (plist-get cmd :type) "compact")
                           (setq compact-callback cb)
                         (setq prompt-sent t))))
                    ((symbol-function 'pi-coding-agent--handle-compaction-success) #'ignore)
                    ((symbol-function 'pi-coding-agent--prepare-and-send)
                     (lambda (text) (setq prepared-text text)))
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer chat-buf
              (pi-coding-agent-compact)
              (should (eq pi-coding-agent--status 'compacting)))

            (with-current-buffer input-buf
              (insert "queued during compaction")
              (pi-coding-agent-send)
              (should (string-empty-p (buffer-string))))

            (with-current-buffer chat-buf
              (should-not prompt-sent)
              (should (equal pi-coding-agent--followup-queue '("queued during compaction"))))

            (should (functionp compact-callback))
            (funcall compact-callback '(:success t :data (:tokensBefore 1234 :summary "Done")))

            (with-current-buffer chat-buf
              (should (eq pi-coding-agent--status 'idle))
              (should (null pi-coding-agent--followup-queue)))
            (should (equal prepared-text "queued during compaction"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-compact-dead-process-keeps-idle ()
  "Manual compact should not transition state when process is dead."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-compact-dead-proc*"))
        (rpc-called nil)
        (shown-message nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--followup-queue nil))
          (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                     (lambda () 'dead-proc))
                    ((symbol-function 'process-live-p)
                     (lambda (_proc) nil))
                    ((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (&rest _args)
                       (setq rpc-called t)))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq shown-message (apply #'format fmt args)))))
            (with-current-buffer chat-buf
              (pi-coding-agent-compact)
              (should (eq pi-coding-agent--status 'idle))))
          (should-not rpc-called)
          (should (equal shown-message
                         "Pi: Process died - try M-x pi-coding-agent-reload or C-c C-p R")))
      (kill-buffer chat-buf))))

;;; Fork at Point

(ert-deftest pi-coding-agent-test-fork-at-point-correct-entry-id ()
  "Fork-at-point picks the right entry on second heading."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'idle)
          (pi-coding-agent--process 'mock-proc)
          (forked-entry-id nil)
          (fork-messages (pi-coding-agent-test--make-3turn-fork-messages)))
      (let ((inhibit-read-only t))
        (pi-coding-agent-test--insert-chat-turns))
      (goto-char (point-min))
      (pi-coding-agent-next-message)
      (pi-coding-agent-next-message)
      (should (looking-at "You · 10:05"))
      (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                 (lambda (_proc cmd cb)
                   (cond
                    ((equal (plist-get cmd :type) "get_fork_messages")
                     (funcall cb (list :success t :data (list :messages fork-messages))))
                    ((equal (plist-get cmd :type) "fork")
                     (setq forked-entry-id (plist-get cmd :entryId))
                     (funcall cb '(:success t :data (:text "Second question"))))
                    ((equal (plist-get cmd :type) "get_state")
                     (funcall cb '(:success t :data (:sessionFile "/tmp/forked.jsonl"))))
                    ((equal (plist-get cmd :type) "get_messages")
                     (funcall cb '(:success t :data (:messages [])))))))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'pi-coding-agent--refresh-header) #'ignore))
        (pi-coding-agent-fork-at-point))
      (should (equal forked-entry-id "u2")))))

(ert-deftest pi-coding-agent-test-fork-at-point-confirmation-declined ()
  "Fork-at-point does nothing when confirmation is declined."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'idle)
          (pi-coding-agent--process 'mock-proc)
          (fork-called nil)
          (fork-messages (pi-coding-agent-test--make-3turn-fork-messages)))
      (let ((inhibit-read-only t))
        (pi-coding-agent-test--insert-chat-turns))
      (goto-char (point-min))
      (pi-coding-agent-next-message)
      (pi-coding-agent-next-message)
      (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                 (lambda (_proc cmd cb)
                   (cond
                    ((equal (plist-get cmd :type) "get_fork_messages")
                     (funcall cb (list :success t :data (list :messages fork-messages))))
                    ((equal (plist-get cmd :type) "fork")
                     (setq fork-called t)))))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
        (pi-coding-agent-fork-at-point))
      (should-not fork-called))))

(ert-deftest pi-coding-agent-test-fork-at-point-no-user-turn ()
  "Before first You heading, fork-at-point skips RPC."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'idle)
          (pi-coding-agent--process 'mock-proc)
          (rpc-called nil))
      (let ((inhibit-read-only t))
        (pi-coding-agent-test--insert-chat-turns))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                 (lambda (&rest _) (setq rpc-called t))))
        (pi-coding-agent-fork-at-point))
      (should-not rpc-called))))

(ert-deftest pi-coding-agent-test-fork-at-point-streaming-guard ()
  "During streaming, fork-at-point skips RPC."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--process 'mock-proc)
          (rpc-called nil))
      (let ((inhibit-read-only t))
        (pi-coding-agent-test--insert-chat-turns))
      (goto-char (point-min))
      (pi-coding-agent-next-message)
      (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                 (lambda (&rest _) (setq rpc-called t))))
        (pi-coding-agent-fork-at-point))
      (should-not rpc-called))))

(ert-deftest pi-coding-agent-test-fork-at-point-rpc-failure-shows-error ()
  "Fork-at-point shows an explicit RPC failure message."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'idle)
          (pi-coding-agent--process 'mock-proc)
          (shown-message nil))
      (let ((inhibit-read-only t))
        (pi-coding-agent-test--insert-chat-turns))
      (goto-char (point-min))
      (pi-coding-agent-next-message)
      (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                 (lambda (_proc cmd cb)
                   (when (equal (plist-get cmd :type) "get_fork_messages")
                     (funcall cb '(:success nil :error "Unknown command: get_fork_messages")))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (pi-coding-agent-fork-at-point))
      (should (equal shown-message
                     "Pi: Failed to get fork messages: Unknown command: get_fork_messages")))))

(defconst pi-coding-agent-test--deep-tree-depth 1700
  "Depth used for deep-tree fork and flatten regression tests.")

(ert-deftest pi-coding-agent-test-fork-at-point-deep-tree ()
  "Fork-at-point maps visible ordinals on deep histories."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let* ((depth pi-coding-agent-test--deep-tree-depth)
           (pi-coding-agent--status 'idle)
           (pi-coding-agent--process 'mock-proc)
           (forked-entry-id nil)
           (fork-messages (pi-coding-agent-test--make-deep-fork-messages depth))
           (expected-entry-id (format "n%d" (- depth 2))))
      (let ((inhibit-read-only t))
        (insert "Pi 1.0.0\n========\nWelcome\n\n"
                "You · 10:00\n===========\nOlder visible turn\n\n"
                "Assistant\n=========\nAnswer\n\n"
                "You · 10:01\n===========\nLatest visible turn\n\n"
                "Assistant\n=========\nAnswer\n"))
      (goto-char (point-min))
      (pi-coding-agent-next-message)
      (should (looking-at "You · 10:00"))
      (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                 (lambda (_proc cmd cb)
                   (cond
                    ((equal (plist-get cmd :type) "get_fork_messages")
                     (funcall cb (list :success t :data (list :messages fork-messages))))
                    ((equal (plist-get cmd :type) "fork")
                     (setq forked-entry-id (plist-get cmd :entryId))
                     (funcall cb '(:success t :data (:text "Older visible turn"))))
                    ((equal (plist-get cmd :type) "get_state")
                     (funcall cb '(:success t :data (:sessionFile "/tmp/forked.jsonl"))))
                    ((equal (plist-get cmd :type) "get_messages")
                     (funcall cb '(:success t :data (:messages [])))))))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'pi-coding-agent--refresh-header) #'ignore))
        (pi-coding-agent-fork-at-point))
      (should (equal forked-entry-id expected-entry-id)))))

(ert-deftest pi-coding-agent-test-fork-at-point-compaction ()
  "Fork-at-point uses last-N mapping in compacted sessions."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'idle)
          (pi-coding-agent--process 'mock-proc)
          (forked-entry-id nil)
          (fork-messages
           [(:entryId "u1" :text "Compacted away")
            (:entryId "u2" :text "After compaction")
            (:entryId "u3" :text "Latest")]))
      (let ((inhibit-read-only t))
        (insert "Pi 1.0.0\n========\nWelcome\n\n"
                "Compaction\n==========\nSummary of earlier conversation\n\n"
                "You · 10:05\n===========\nAfter compaction\n\n"
                "Assistant\n=========\nResponse\n\n"
                "You · 10:10\n===========\nLatest\n\n"
                "Assistant\n=========\nFinal\n"))
      (goto-char (point-min))
      (pi-coding-agent-next-message)
      (should (looking-at "You · 10:05"))
      (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                 (lambda (_proc cmd cb)
                   (cond
                    ((equal (plist-get cmd :type) "get_fork_messages")
                     (funcall cb (list :success t :data (list :messages fork-messages))))
                    ((equal (plist-get cmd :type) "fork")
                     (setq forked-entry-id (plist-get cmd :entryId))
                     (funcall cb '(:success t :data (:text "After compaction"))))
                    ((equal (plist-get cmd :type) "get_state")
                     (funcall cb '(:success t :data (:sessionFile "/tmp/forked.jsonl"))))
                    ((equal (plist-get cmd :type) "get_messages")
                     (funcall cb '(:success t :data (:messages [])))))))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                ((symbol-function 'pi-coding-agent--refresh-header) #'ignore))
        (pi-coding-agent-fork-at-point))
      (should (equal forked-entry-id "u2")))))

;;; Fork Entry Resolution

(ert-deftest pi-coding-agent-test-resolve-fork-entry-maps-ordinal ()
  "resolve-fork-entry maps ordinal to entry ID and preview."
  (let* ((fork-messages (pi-coding-agent-test--make-3turn-fork-messages))
         (response (list :success t :data (list :messages fork-messages)))
         (result (pi-coding-agent--resolve-fork-entry response 1 3)))
    (should (equal (car result) "u2"))
    (should (equal (cdr result) "Second question"))))

(ert-deftest pi-coding-agent-test-resolve-fork-entry-compaction ()
  "resolve-fork-entry uses last-N mapping in compacted sessions."
  (let* ((fork-messages (pi-coding-agent-test--make-3turn-fork-messages))
         (response (list :success t :data (list :messages fork-messages)))
         (result (pi-coding-agent--resolve-fork-entry response 0 2)))
    (should (equal (car result) "u2"))))

(ert-deftest pi-coding-agent-test-resolve-fork-entry-failure ()
  "resolve-fork-entry returns nil on failure."
  (let ((response '(:success nil :error "Network error")))
    (should-not (pi-coding-agent--resolve-fork-entry response 0 3))))

(defun pi-coding-agent-test--make-deep-linear-tree (depth)
  "Return a single-branch tree vector with DEPTH nested nodes.
The tree is built iteratively to avoid recursion in test setup."
  (let* ((leaf-id (1- depth))
         (node (list :id (format "n%d" leaf-id)
                     :type "message"
                     :role "user"
                     :preview (format "node %d" leaf-id)
                     :parentId (and (> leaf-id 0) (format "n%d" (1- leaf-id)))
                     :children [])))
    (dotimes (i (1- depth))
      (let ((id (- depth i 2)))
        (setq node (list :id (format "n%d" id)
                         :type "message"
                         :role "user"
                         :preview (format "node %d" id)
                         :parentId (and (> id 0) (format "n%d" (1- id)))
                         :children (vector node)))))
    (vector node)))

(defun pi-coding-agent-test--make-deep-fork-messages (depth)
  "Return DEPTH chronological fork messages."
  (let ((messages (make-vector depth nil)))
    (dotimes (i depth)
      (aset messages i (list :entryId (format "n%d" i)
                             :text (format "node %d" i))))
    messages))

(ert-deftest pi-coding-agent-test-flatten-tree-deep-linear-tree ()
  "flatten-tree handles deep linear trees without eval-depth overflow."
  (let* ((depth pi-coding-agent-test--deep-tree-depth)
         (tree (pi-coding-agent-test--make-deep-linear-tree depth))
         (index (pi-coding-agent--flatten-tree tree)))
    (should (= (hash-table-count index) depth))))

;;; Active Branch Tree Walk

(ert-deftest pi-coding-agent-test-active-branch-linear ()
  "Linear tree: u1 → a1 → u2 → a2 (leaf) returns both user IDs."
  (let* ((data (pi-coding-agent-test--build-tree
                '("u1" nil "message" :role "user" :preview "Hello")
                '("a1" nil "message" :role "assistant" :preview "Hi")
                '("u2" nil "message" :role "user" :preview "More")
                '("a2" nil "message" :role "assistant" :preview "Sure")))
         (index (pi-coding-agent--flatten-tree (plist-get data :tree)))
         (ids (pi-coding-agent--active-branch-user-ids index "a2")))
    (should (equal ids '("u1" "u2")))))

(ert-deftest pi-coding-agent-test-active-branch-branched ()
  "Branched tree: active branch u1 → a1 → u2 → a2, ignores u3 → a3."
  (let* ((data (pi-coding-agent-test--build-tree
                '("u1" nil "message" :role "user" :preview "Hello")
                '("a1" nil "message" :role "assistant" :preview "Hi")
                '("u2" nil "message" :role "user" :preview "Path A")
                '("a2" nil "message" :role "assistant" :preview "Sure A")
                '("u3" "a1" "message" :role "user" :preview "Path B")
                '("a3" nil "message" :role "assistant" :preview "Sure B")))
         (index (pi-coding-agent--flatten-tree (plist-get data :tree)))
         (ids (pi-coding-agent--active-branch-user-ids index "a2")))
    (should (equal ids '("u1" "u2")))))

(ert-deftest pi-coding-agent-test-active-branch-with-compaction ()
  "Tree with compaction node: u1 → a1 → compaction → u2 → a2."
  (let* ((data (pi-coding-agent-test--build-tree
                '("u1" nil "message" :role "user" :preview "First")
                '("a1" nil "message" :role "assistant" :preview "Response")
                '("c1" nil "compaction" :tokensBefore 5000)
                '("u2" nil "message" :role "user" :preview "After compaction")
                '("a2" nil "message" :role "assistant" :preview "Still here")))
         (index (pi-coding-agent--flatten-tree (plist-get data :tree)))
         (ids (pi-coding-agent--active-branch-user-ids index "a2")))
    (should (equal ids '("u1" "u2")))))

(ert-deftest pi-coding-agent-test-active-branch-with-metadata ()
  "Tree with model_change and thinking nodes: only user IDs returned."
  (let* ((data (pi-coding-agent-test--build-tree
                '("u1" nil "message" :role "user" :preview "Hello")
                '("a1" nil "message" :role "assistant" :preview "Hi")
                '("m1" nil "model_change" :provider "anthropic" :modelId "claude-4")
                '("t1" nil "thinking_level_change" :thinkingLevel "high")
                '("u2" nil "message" :role "user" :preview "More")
                '("a2" nil "message" :role "assistant" :preview "Sure")))
         (index (pi-coding-agent--flatten-tree (plist-get data :tree)))
         (ids (pi-coding-agent--active-branch-user-ids index "a2")))
    (should (equal ids '("u1" "u2")))))

(ert-deftest pi-coding-agent-test-active-branch-empty-tree ()
  "Empty tree returns empty list."
  (let* ((index (pi-coding-agent--flatten-tree []))
         (ids (pi-coding-agent--active-branch-user-ids index nil)))
    (should (equal ids nil))))

(ert-deftest pi-coding-agent-test-active-branch-nil-leaf ()
  "Nil leafId returns empty list."
  (let* ((data (pi-coding-agent-test--build-tree
                '("u1" nil "message" :role "user" :preview "Hello")))
         (index (pi-coding-agent--flatten-tree (plist-get data :tree)))
         (ids (pi-coding-agent--active-branch-user-ids index nil)))
    (should (equal ids nil))))

(provide 'pi-coding-agent-menu-test)
;;; pi-coding-agent-menu-test.el ends here
