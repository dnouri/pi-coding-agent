;;; piem-menu-test.el --- Tests for piem-menu -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>

;;; Commentary:

;; Tests for session management, transient menu, model/thinking commands,
;; reconnect, and slash commands via RPC — the menu and session layer.

;;; Code:

(require 'ert)
(require 'piem)
(require 'piem-test-common)

;;; Version Checks

(ert-deftest piem-test-normalize-version-ignores-prefix-and-suffix ()
  "Version parsing should keep only the numeric portion."
  (should (equal "0.12.0"
                 (piem--normalize-version
                  "v0.12.0-15-gfe5214e6-builtin"))))

(ert-deftest piem-test-version-at-least-p-rejects-old-built-in-version ()
  "Older transient versions should fail the minimum version check."
  (should-not (piem--version-at-least-p "0.7.2.2" "0.9.0")))

(ert-deftest piem-test-version-at-least-p-accepts-built-in-snapshot-format ()
  "Snapshot version strings with prefixes should still compare correctly."
  (should (piem--version-at-least-p
           "v0.12.0-15-gfe5214e6-builtin"
           "0.9.0")))

;;; Session Management

(ert-deftest piem-test-buffer-name-default-session ()
  "Buffer name without session name."
  (should (equal (piem--buffer-name :chat "/tmp/proj/" nil)
                 "*piem-chat:/tmp/proj/*")))

(ert-deftest piem-test-buffer-name-named-session ()
  "Buffer name with session name."
  (should (equal (piem--buffer-name :chat "/tmp/proj/" "feature")
                 "*piem-chat:/tmp/proj/<feature>*")))

(ert-deftest piem-test-clear-chat-buffer-resets-to-startup ()
  "Clearing chat buffer shows startup header and resets state."
  (with-temp-buffer
    (piem-chat-mode)
    ;; Add some content
    (let ((inhibit-read-only t))
      (insert "Some existing content\nMore content"))
    ;; Set markers as if streaming happened
    (setq piem--message-start-marker (point-marker))
    (setq piem--streaming-marker (point-marker))
    ;; Clear the buffer
    (piem--clear-chat-buffer)
    ;; Should have startup header
    (should (string-match-p "C-c C-c" (buffer-string)))
    ;; Markers should be reset
    (should (null piem--message-start-marker))
    (should (null piem--streaming-marker))))

(ert-deftest piem-test-clear-chat-buffer-resets-session-state ()
  "Clearing chat buffer resets all session-specific state."
  (with-temp-buffer
    (piem-chat-mode)
    ;; Set various session state as if we had an active session
    (setq piem--session-name "My Named Session"
          piem--cached-stats '(:messages 10 :cost 0.05)
          piem--assistant-header-shown t
          piem--followup-queue '("pending message")
          piem--local-user-message "user text"
          piem--aborted t
          piem--extension-status '(("ext1" . "status"))
          piem--working-message "Reading README..."
          piem--unsupported-extension-ui-methods-warned '("setWidget")
          piem--message-start-marker (point-marker)
          piem--streaming-marker (point-marker)
          piem--thinking-marker (point-marker)
          piem--thinking-start-marker (point-marker)
          piem--thinking-raw "pending"
          piem--in-code-block t
          piem--in-thinking-block t
          piem--line-parse-state 'code-fence
          piem--pending-tool-overlay (make-overlay 1 1)
          piem--activity-phase "running")
    ;; Add entries to tool-args-cache and live tool registry
    (puthash "tool-1" '(:path "/test-a") piem--tool-args-cache)
    (puthash "tool-2" '(:path "/test-b") piem--tool-args-cache)
    (puthash "tool-1" '(:tool-call-id "tool-1") piem--live-tool-blocks)
    (puthash "tool-2" '(:tool-call-id "tool-2") piem--live-tool-blocks)
    ;; Clear the buffer
    (piem--clear-chat-buffer)
    ;; All session state should be reset
    (should (null piem--session-name))
    (should (null piem--cached-stats))
    (should (null piem--assistant-header-shown))
    (should (null piem--followup-queue))
    (should (null piem--local-user-message))
    (should (null piem--aborted))
    (should (null piem--extension-status))
    (should (null piem--working-message))
    (should (null piem--unsupported-extension-ui-methods-warned))
    (should (null piem--message-start-marker))
    (should (null piem--streaming-marker))
    (should (null piem--thinking-marker))
    (should (null piem--thinking-start-marker))
    (should (null piem--thinking-raw))
    (should (null piem--in-code-block))
    (should (null piem--in-thinking-block))
    (should (eq piem--line-parse-state 'line-start))
    (should (null piem--pending-tool-overlay))
    (should (equal piem--activity-phase "idle"))
    ;; Tool args cache and live tool registry should be empty
    (should (= 0 (hash-table-count piem--tool-args-cache)))
    (should (= 0 (hash-table-count piem--live-tool-blocks)))))

(ert-deftest piem-test-clear-chat-buffer-removes-pi-owned-render-overlays ()
  "Clearing chat buffer removes stale pi-owned tool and diff overlays."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((inhibit-read-only t))
      (insert "tool\n+ 1 added\n- 2 removed\n"))
    (let ((tool-ov (make-overlay 1 5 nil nil nil))
          (tool-count 0)
          (diff-count 0))
      (overlay-put tool-ov 'piem-tool-block t)
      (setq piem--pending-tool-overlay tool-ov)
      (piem--apply-diff-overlays 6 (point-max))
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'piem-tool-block)
          (setq tool-count (1+ tool-count)))
        (when (overlay-get ov 'piem-diff-overlay)
          (setq diff-count (1+ diff-count))))
      (should (= tool-count 1))
      (should (= diff-count 4)))
    (piem--clear-chat-buffer)
    (let ((tool-count 0)
          (diff-count 0))
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'piem-tool-block)
          (setq tool-count (1+ tool-count)))
        (when (overlay-get ov 'piem-diff-overlay)
          (setq diff-count (1+ diff-count))))
      (should (= tool-count 0))
      (should (= diff-count 0))
      (should-not piem--pending-tool-overlay))))

(ert-deftest piem-test-new-session-clears-buffer-from-different-context ()
  "New session clears buffer and updates state even when callback runs elsewhere.
This tests that the async callback properly captures the chat buffer reference,
not relying on current buffer context which may change before callback executes.
Also verifies that the new session-file is stored in state for reload to work."
  (let ((chat-buf (generate-new-buffer "*piem-chat:/tmp/test-new-session/*"))
        (captured-callback nil)
        (proc (start-process "test-new-session-state" nil "cat")))
    (unwind-protect
        (progn
          ;; Set up chat buffer with content and old state
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--process proc
                  piem--state '(:session-file "/tmp/old-session.jsonl"))
            (let ((inhibit-read-only t))
              (insert "Existing conversation content\nMore content here")))
          ;; Mock the RPC to capture the new_session callback and handle get_state
          (cl-letf (((symbol-function 'piem--get-process) (lambda () proc))
                    ((symbol-function 'piem--get-chat-buffer) (lambda () chat-buf))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd cb)
                       (cond
                        ((equal (plist-get cmd :type) "new_session")
                         (setq captured-callback cb))
                        ((equal (plist-get cmd :type) "get_state")
                         (funcall cb '(:success t :data (:sessionFile "/tmp/new-session.jsonl")))))))
                    ((symbol-function 'piem--refresh-header) #'ignore))
            ;; Call new-session from the chat buffer
            (with-current-buffer chat-buf
              (piem-new-session))
            ;; Simulate callback being called from a DIFFERENT buffer
            ;; (This is what happens in practice - callbacks run in arbitrary contexts)
            (with-temp-buffer
              (funcall captured-callback '(:success t :data (:cancelled :false)))))
          ;; Verify buffer was cleared
          (with-current-buffer chat-buf
            (should-not (string-match-p "Existing conversation" (buffer-string)))
            (should (string-match-p "C-c C-c" (buffer-string)))
            ;; Verify state was updated with new session file (the actual bug fix)
            (should (equal (plist-get piem--state :session-file)
                           "/tmp/new-session.jsonl"))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-new-session-refuses-prompt-preflight ()
  "New session cannot discard a prompt whose acceptance is unresolved."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--process 'mock-proc
          piem--status 'sending
          piem--prompt-start-wait-active t)
    (let (rpc-called feedback)
      (cl-letf (((symbol-function 'piem--get-process)
                 (lambda () 'mock-proc))
                ((symbol-function 'piem--get-chat-buffer)
                 (lambda () (current-buffer)))
                ((symbol-function 'piem--rpc-async)
                 (lambda (&rest _)
                   (setq rpc-called t)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (setq feedback (apply #'format format-string args))))))
        (piem-new-session))
      (should-not rpc-called)
      (should (string-match-p "Cannot start a new session"
                              (or feedback ""))))))

(ert-deftest piem-test-new-session-preserves-queued-followups ()
  "Reset refuses rather than silently discarding accepted local follow-ups."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--process 'mock-proc
          piem--status 'streaming
          piem--followup-queue '("keep me"))
    (let (rpc-called feedback)
      (cl-letf (((symbol-function 'piem--get-process)
                 (lambda () 'mock-proc))
                ((symbol-function 'piem--get-chat-buffer)
                 (lambda () (current-buffer)))
                ((symbol-function 'piem--rpc-async)
                 (lambda (&rest _)
                   (setq rpc-called t)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (setq feedback (apply #'format format-string args))))))
        (piem-new-session))
      (should-not rpc-called)
      (should (equal piem--followup-queue '("keep me")))
      (should (string-match-p "queued follow-ups" (or feedback ""))))))

(ert-deftest piem-test-new-session-can-reset-server-streaming ()
  "A deliberate reset still reaches Pi while its agent is streaming."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--process 'mock-proc
          piem--status 'streaming)
    (let (callback)
      (cl-letf (((symbol-function 'piem--get-process)
                 (lambda () 'mock-proc))
                ((symbol-function 'piem--get-chat-buffer)
                 (lambda () (current-buffer)))
                ((symbol-function 'piem--rpc-async)
                 (lambda (_process _command cb)
                   (setq callback cb))))
        (piem-new-session)
        (should (functionp callback))
        (should (piem--session-transition-active-p))))))

(ert-deftest piem-test-new-session-blocks-work-until-response ()
  "A scheduled reset owns the session transition until its response."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--process 'mock-proc
          piem--status 'idle)
    (let (callback)
      (cl-letf (((symbol-function 'piem--get-process)
                 (lambda () 'mock-proc))
                ((symbol-function 'piem--get-chat-buffer)
                 (lambda () (current-buffer)))
                ((symbol-function 'piem--rpc-async)
                 (lambda (_process _command cb)
                   (setq callback cb))))
        (piem-new-session)
        (should (functionp callback))
        (should (piem--session-transition-active-p))))))

(ert-deftest piem-test-find-session-returns-existing ()
  "piem--find-session returns an existing chat buffer."
  (let* ((root (piem-test--make-temp-directory
                "piem-test-find-session-"))
         (buf (generate-new-buffer (piem-test--chat-buffer-name root))))
    (unwind-protect
        (with-current-buffer buf
          (piem-chat-mode)
          (setq default-directory root)
          (should (eq (piem--find-session root nil) buf)))
      (kill-buffer buf)
      (ignore-errors (delete-directory root t)))))

(ert-deftest piem-test-find-session-returns-nil-when-missing ()
  "piem--find-session returns nil when no session exists."
  (should (null (piem--find-session "/tmp/nonexistent-session-xyz/" nil))))

(ert-deftest piem-test-piem-reuses-existing-session ()
  "Calling pi twice returns same buffers."
  (piem-test-with-mock-session "/tmp/piem-test-reuse/"
    (let ((chat1 (get-buffer "*piem-chat:/tmp/piem-test-reuse/*"))
          (input1 (get-buffer "*piem-input:/tmp/piem-test-reuse/*")))
      (piem)  ; call again
      (should (eq chat1 (get-buffer "*piem-chat:/tmp/piem-test-reuse/*")))
      (should (eq input1 (get-buffer "*piem-input:/tmp/piem-test-reuse/*"))))))

(ert-deftest piem-test-named-session-separate-from-default ()
  "Named session creates separate buffers from default."
  (let ((default-directory "/tmp/piem-test-named/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil))
              ((symbol-function 'piem--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (piem)  ; default session
            (piem "feature")  ; named session
            (should (get-buffer "*piem-chat:/tmp/piem-test-named/*"))
            (should (get-buffer "*piem-chat:/tmp/piem-test-named/<feature>*"))
            (should-not (eq (get-buffer "*piem-chat:/tmp/piem-test-named/*")
                            (get-buffer "*piem-chat:/tmp/piem-test-named/<feature>*"))))
        (ignore-errors (kill-buffer "*piem-chat:/tmp/piem-test-named/*"))
        (ignore-errors (kill-buffer "*piem-input:/tmp/piem-test-named/*"))
        (ignore-errors (kill-buffer "*piem-chat:/tmp/piem-test-named/<feature>*"))
        (ignore-errors (kill-buffer "*piem-input:/tmp/piem-test-named/<feature>*"))))))

(ert-deftest piem-test-named-session-from-existing-piem-buffer ()
  "Creating named session while in pi buffer creates new session, not reuse."
  (let ((default-directory "/tmp/piem-test-from-pi/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'piem--start-process) (lambda (_) nil))
              ((symbol-function 'piem--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (piem)  ; default session
            ;; Now switch INTO the pi input buffer and create a named session
            (with-current-buffer "*piem-input:/tmp/piem-test-from-pi/*"
              (piem "feature"))  ; should create NEW session
            ;; Both sessions should exist
            (should (get-buffer "*piem-chat:/tmp/piem-test-from-pi/*"))
            (should (get-buffer "*piem-chat:/tmp/piem-test-from-pi/<feature>*"))
            ;; They should be different buffers
            (should-not (eq (get-buffer "*piem-chat:/tmp/piem-test-from-pi/*")
                            (get-buffer "*piem-chat:/tmp/piem-test-from-pi/<feature>*"))))
        (ignore-errors (kill-buffer "*piem-chat:/tmp/piem-test-from-pi/*"))
        (ignore-errors (kill-buffer "*piem-input:/tmp/piem-test-from-pi/*"))
        (ignore-errors (kill-buffer "*piem-chat:/tmp/piem-test-from-pi/<feature>*"))
        (ignore-errors (kill-buffer "*piem-input:/tmp/piem-test-from-pi/<feature>*"))))))

(ert-deftest piem-test-quit-kills-both-buffers ()
  "piem-quit kills both chat and input buffers."
  (piem-test-with-mock-session "/tmp/piem-test-quit/"
    (with-current-buffer "*piem-input:/tmp/piem-test-quit/*"
      (piem-quit))
    (should-not (get-buffer "*piem-chat:/tmp/piem-test-quit/*"))
    (should-not (get-buffer "*piem-input:/tmp/piem-test-quit/*"))))

(defmacro piem-test--with-quit-confirmable-session
    (binding-spec &rest body)
  "Run BODY with a pi session whose live process would prompt on quit.
BINDING-SPEC is (DIR CHAT-NAME INPUT-NAME PROC).  DIR is evaluated once."
  (declare (indent 1) (debug t))
  (let ((dir (nth 0 binding-spec))
        (chat-name (nth 1 binding-spec))
        (input-name (nth 2 binding-spec))
        (proc (nth 3 binding-spec))
        (dir-value (make-symbol "dir-value")))
    `(let* ((,dir-value ,dir)
            (,chat-name (piem-test--chat-buffer-name ,dir-value))
            (,input-name (piem-test--input-buffer-name ,dir-value))
            (,proc nil))
       (make-directory ,dir-value t)
       (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                 ((symbol-function 'piem--start-process)
                  (lambda (_)
                    (setq ,proc (start-process "pi-test-quit" nil "cat"))
                    (set-process-query-on-exit-flag ,proc t)
                    ,proc))
                 ((symbol-function 'piem--display-buffers) #'ignore))
         (unwind-protect
             (progn
               (let ((default-directory ,dir-value))
                 (piem))
               (with-current-buffer ,chat-name
                 (set-process-buffer ,proc (current-buffer)))
               ,@body)
           (when (and ,proc (process-live-p ,proc))
             (delete-process ,proc))
           (piem-test--kill-session-buffers ,dir-value))))))

(ert-deftest piem-test-quit-cancelled-preserves-session ()
  "When user cancels quit confirmation, both buffers remain intact and linked."
  (piem-test--with-quit-confirmable-session
      ("/tmp/piem-test-quit-cancel/" chat-name input-name _proc)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (with-current-buffer input-name
        (should-error (piem-quit) :type 'user-error)))
    (should (get-buffer chat-name))
    (should (get-buffer input-name))
    (with-current-buffer chat-name
      (should (eq (piem--get-input-buffer)
                  (get-buffer input-name))))
    (with-current-buffer input-name
      (should (eq (piem--get-chat-buffer)
                  (get-buffer chat-name))))))

(ert-deftest piem-test-quit-confirmed-kills-both ()
  "When user confirms quit, both buffers are killed without double-prompting."
  (let ((prompt-count 0))
    (piem-test--with-quit-confirmable-session
        ("/tmp/piem-test-quit-confirm/" chat-name input-name _proc)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_)
                   (cl-incf prompt-count)
                   t)))
        (with-current-buffer input-name
          (piem-quit)))
      (should-not (get-buffer chat-name))
      (should-not (get-buffer input-name))
      (should (<= prompt-count 1)))))

(ert-deftest piem-test-quit-without-confirmation-kills-both-without-prompt ()
  "When configured, quitting a live session kills both buffers without prompting."
  (let ((piem-quit-without-confirmation t))
    (piem-test--with-quit-confirmable-session
        ("/tmp/piem-test-quit-no-confirm/" chat-name input-name _proc)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _)
                   (ert-fail "piem-quit prompted unexpectedly"))))
        (with-current-buffer input-name
          (piem-quit)))
      (should-not (get-buffer chat-name))
      (should-not (get-buffer input-name)))))

(ert-deftest piem-test-kill-chat-cancelled-preserves-session ()
  "Killing chat buffer asks before terminating its live process."
  (let ((prompt-count 0))
    (piem-test--with-quit-confirmable-session
        ("/tmp/piem-test-kill-chat-cancel/" chat-name input-name proc)
      ;; GUI test helpers disable this globally; this test needs the default
      ;; Emacs process-buffer query to exercise the chat-buffer contract.
      (let ((kill-buffer-query-functions
             (if (memq #'process-kill-buffer-query-function
                       kill-buffer-query-functions)
                 kill-buffer-query-functions
               (cons #'process-kill-buffer-query-function
                     kill-buffer-query-functions))))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (_)
                     (cl-incf prompt-count)
                     nil)))
          (should-not (kill-buffer chat-name))))
      (should (= prompt-count 1))
      (should (get-buffer chat-name))
      (should (get-buffer input-name))
      (should (process-live-p proc))
      (should (process-query-on-exit-flag proc)))))

(ert-deftest piem-test-kill-chat-prompts-even-when-process-noquery ()
  "Direct chat-buffer kills still use Pi's own prompt for noquery processes."
  (let ((prompt-count 0))
    (piem-test--with-quit-confirmable-session
        ("/tmp/piem-test-kill-chat-noquery/" chat-name input-name proc)
      (set-process-query-on-exit-flag proc nil)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_)
                   (cl-incf prompt-count)
                   nil)))
        (should-not (kill-buffer chat-name)))
      (should (= prompt-count 1))
      (should (get-buffer chat-name))
      (should (get-buffer input-name))
      (should (process-live-p proc))
      (should-not (process-query-on-exit-flag proc)))))

(ert-deftest piem-test-kill-emacs-query-is-installed ()
  "Exiting Emacs consults pi sessions via `kill-emacs-query-functions'."
  (should (memq #'piem--session-kill-emacs-query
                kill-emacs-query-functions)))

(ert-deftest piem-test-kill-emacs-query-prompts-for-live-session ()
  "Exiting Emacs asks once when a session process is still running."
  (let ((prompt-count 0))
    (piem-test--with-quit-confirmable-session
        ("/tmp/piem-test-kill-emacs-query/" chat-name input-name proc)
      (cl-letf (((symbol-function 'process-list) (lambda () (list proc)))
                ((symbol-function 'yes-or-no-p)
                 (lambda (prompt)
                   (cl-incf prompt-count)
                   (should (equal prompt
                                  "Pi session has a running process; exit anyway? "))
                   nil)))
        (should-not (piem--session-kill-emacs-query))
        (should (= prompt-count 1)))
      (cl-letf (((symbol-function 'process-list) (lambda () (list proc)))
                ((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (should (piem--session-kill-emacs-query)))
      (should (get-buffer chat-name))
      (should (get-buffer input-name))
      (should (process-live-p proc)))))

(ert-deftest piem-test-kill-emacs-query-asks-only-when-required ()
  "Exit stays silent for dead, skipped, or configured-away processes."
  (piem-test--with-quit-confirmable-session
      ("/tmp/piem-test-kill-emacs-silent/" _chat _input proc)
    (cl-letf (((symbol-function 'process-list) (lambda () (list proc)))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _)
                 (ert-fail "kill-emacs query prompted unexpectedly"))))
      ;; Intentional teardown marks the process; exit must not ask again.
      (piem--skip-process-kill-confirmation proc)
      (should (piem--session-kill-emacs-query))
      (process-put proc 'piem-skip-kill-confirmation nil)
      ;; Opt-out defcustom applies to Emacs exit as it does to quit.
      (let ((piem-quit-without-confirmation t))
        (should (piem--session-kill-emacs-query)))
      ;; A dead process is nothing to protect.
      (delete-process proc)
      (should (piem--session-kill-emacs-query)))))

(ert-deftest piem-test-kill-input-cancelled-preserves-session ()
  "Killing input buffer asks before terminating the linked live process."
  (let ((prompt-count 0))
    (piem-test--with-quit-confirmable-session
        ("/tmp/piem-test-kill-input-cancel/" chat-name input-name proc)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_)
                   (cl-incf prompt-count)
                   nil)))
        (should-not (kill-buffer input-name)))
      (should (= prompt-count 1))
      (should (get-buffer chat-name))
      (should (get-buffer input-name))
      (should (process-live-p proc))
      (should (process-query-on-exit-flag proc)))))

(ert-deftest piem-test-kill-input-confirmed-kills-session ()
  "Confirming input buffer kill terminates the linked session once."
  (let ((prompt-count 0))
    (piem-test--with-quit-confirmable-session
        ("/tmp/piem-test-kill-input-confirm/" chat-name input-name proc)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_)
                   (cl-incf prompt-count)
                   t)))
        (should (kill-buffer input-name)))
      (should (= prompt-count 1))
      (should-not (get-buffer chat-name))
      (should-not (get-buffer input-name))
      (should-not (process-live-p proc)))))

(ert-deftest piem-test-kill-chat-kills-input ()
  "Killing chat buffer also kills input buffer."
  (piem-test-with-mock-session "/tmp/piem-test-linked/"
    (kill-buffer "*piem-chat:/tmp/piem-test-linked/*")
    (should-not (get-buffer "*piem-input:/tmp/piem-test-linked/*"))))

(ert-deftest piem-test-kill-input-kills-chat ()
  "Killing input buffer also kills chat buffer."
  (piem-test-with-mock-session "/tmp/piem-test-linked2/"
    (kill-buffer "*piem-input:/tmp/piem-test-linked2/*")
    (should-not (get-buffer "*piem-chat:/tmp/piem-test-linked2/*"))))

;;; Transient Menu

(ert-deftest piem-test-transient-bound-to-key ()
  "C-c C-p is bound to piem-menu in input mode."
  (with-temp-buffer
    (piem-input-mode)
    (should (eq (key-binding (kbd "C-c C-p")) 'piem-menu))))

;;; Chat Navigation

(ert-deftest piem-test-chat-has-navigation-keys ()
  "Chat mode has n/p for navigation, TAB for at-point toggles, f for fork."
  (with-temp-buffer
    (piem-chat-mode)
    (should (eq (key-binding "n") 'piem-next-message))
    (should (eq (key-binding "p") 'piem-previous-message))
    (should (eq (key-binding (kbd "TAB")) 'piem-toggle-tool-section))
    (should (eq (key-binding "f") 'piem-fork-at-point))))

;;; Reconnect Tests

(ert-deftest piem-test-reload-restarts-process ()
  "Reload starts new process when old process is dead."
  (let* ((started-new-process nil)
         (switch-session-called nil)
         (session-path-used nil)
         (chat-buf (get-buffer-create "*piem-test-reconnect-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            ;; Set up state with session file (simulating previous get_state)
            (setq piem--state '(:session-file "/tmp/test-session.json"
                                           :model (:name "test-model")))
            ;; Set up dead process
            (let ((dead-proc (start-process "test-dead" nil "true")))
              (should (piem-test-wait-for-process-exit dead-proc))
              (setq piem--process dead-proc))
            ;; Mock functions
            (cl-letf (((symbol-function 'piem--start-process)
                       (lambda (_dir)
                         (setq started-new-process t)
                         (let ((proc (start-process "test-new" nil "cat")))
                           (set-process-query-on-exit-flag proc nil)
                           proc)))
                      ((symbol-function 'piem--rpc-async)
                       (lambda (_proc msg _cb)
                         (when (equal (plist-get msg :type) "switch_session")
                           (setq switch-session-called t
                                 session-path-used (plist-get msg :sessionPath))))))
              ;; Call reload
              (piem-reload)
              ;; Verify
              (should started-new-process)
              (should switch-session-called)
              (should (equal session-path-used "/tmp/test-session.json")))))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (when (and piem--process (process-live-p piem--process))
            (delete-process piem--process)))
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-reload-sends-process-local-session-path-for-remote-session ()
  "Reload sends process-local sessionPath when the Emacs state is remote."
  (let* ((session-path-used nil)
         (new-proc nil)
         (chat-buf (get-buffer-create "*piem-test-remote-reload-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (piem--set-chat-session-identity
             "/ssh:pi-host:/home/pi/project/")
            (setq piem--state
                  '(:session-file "/ssh:pi-host:/home/pi/.pi/sessions/current.jsonl"
                    :model (:name "test-model")))
            (let ((dead-proc (start-process "test-remote-reload-dead" nil "true")))
              (should (piem-test-wait-for-process-exit dead-proc))
              (setq piem--process dead-proc))
            (cl-letf (((symbol-function 'piem--start-process)
                       (lambda (_dir)
                         (setq new-proc
                               (start-process "test-remote-reload-new" nil "cat"))))
                      ((symbol-function 'piem--rpc-async)
                       (lambda (_proc msg _cb)
                         (when (equal (plist-get msg :type) "switch_session")
                           (setq session-path-used
                                 (plist-get msg :sessionPath))))))
              (piem-reload)
              (should (equal session-path-used
                             "/home/pi/.pi/sessions/current.jsonl")))))
      (when (and new-proc (process-live-p new-proc))
        (delete-process new-proc))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (when (and piem--process
                     (process-live-p piem--process))
            (delete-process piem--process)))
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-reload-validates-session-path-before-killing-process ()
  "Reload leaves the old process alive when path validation fails."
  (let* ((started-new-process nil)
         (alive-proc nil)
         (project-dir (piem-test--make-temp-directory
                       "piem-test-reload-validate-project-"))
         (chat-buf (get-buffer-create "*piem-test-reload-validate-chat*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (piem-chat-mode)
          (piem--set-chat-session-identity project-dir)
          (setq piem--state
                '(:session-file "/ssh:pi-host:/home/pi/.pi/sessions/current.jsonl"))
          (setq alive-proc (start-process "test-reload-validate-alive" nil "cat")
                piem--process alive-proc)
          (cl-letf (((symbol-function 'piem--start-process)
                     (lambda (_dir)
                       (setq started-new-process t)
                       (ert-fail "Reload started a process before validation failed")))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (&rest _)
                       (ert-fail "Reload switched session before validation failed"))))
            (should-error (piem-reload) :type 'user-error)
            (should-not started-new-process)
            (should (process-live-p alive-proc))))
      (when (and alive-proc (process-live-p alive-proc))
        (delete-process alive-proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf))
      (delete-directory project-dir t))))

(ert-deftest piem-test-reload-keeps-old-process-until-switch-succeeds ()
  "Reload keeps the old process until the fresh process owns the session."
  (let* ((started-new-process nil)
         (old-process-killed nil)
         (chat-buf (get-buffer-create "*piem-test-reload-alive-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            ;; Set up state with session file
            (setq piem--state '(:session-file "/tmp/test-session.json"))
            ;; Set up alive process
            (let ((alive-proc (start-process "test-alive" nil "cat")))
              (set-process-query-on-exit-flag alive-proc nil)
              (setq piem--process alive-proc)
              (cl-letf (((symbol-function 'piem--start-process)
                         (lambda (_dir)
                           (setq started-new-process t)
                           (let ((proc (start-process "test-new" nil "cat")))
                             (set-process-query-on-exit-flag proc nil)
                             proc)))
                        ((symbol-function 'piem--rpc-async)
                         (lambda (_proc _msg _cb) nil)))
                ;; Call reload
                (piem-reload)
                ;; Verify - SHOULD start new process even when old was alive.
                (should started-new-process)
                ;; The old process remains current until switch_session succeeds.
                (should (process-live-p alive-proc))))))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (when (and piem--process (process-live-p piem--process))
            (delete-process piem--process)))
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-reload-switch-failure-keeps-old-process ()
  "A failed reload switch does not attach the fresh process to the UI."
  (let* ((old-proc nil)
         (new-proc nil)
         (shown-message nil)
         (chat-buf (get-buffer-create "*piem-test-reload-failure-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--state '(:session-file "/tmp/test-session.json"))
            (setq old-proc (start-process "test-reload-failure-old" nil "cat")
                  piem--process old-proc))
          (cl-letf (((symbol-function 'piem--start-process)
                     (lambda (_dir)
                       (setq new-proc
                             (start-process "test-reload-failure-new" nil "cat"))))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc msg cb)
                       (when (equal (plist-get msg :type) "switch_session")
                         (funcall cb '(:success :false :error "nope")))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq shown-message (apply #'format fmt args)))))
            (with-current-buffer chat-buf
              (piem-reload)
              (should (eq piem--process old-proc))
              (should (process-live-p old-proc))
              (should-not (process-live-p new-proc))
              (should-not (piem--session-transition-active-p))
              (should (equal shown-message
                             "Pi: Failed to reload - nope")))))
      (when (and old-proc (process-live-p old-proc))
        (delete-process old-proc))
      (when (and new-proc (process-live-p new-proc))
        (delete-process new-proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-reload-cancelled-keeps-old-process ()
  "A cancelled reload switch keeps the old process current and live."
  (let* ((old-proc nil)
         (new-proc nil)
         (shown-message nil)
         (chat-buf (get-buffer-create
                    "*piem-test-reload-cancelled-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--state '(:session-file "/tmp/test-session.json"))
            (setq old-proc (start-process "test-reload-cancelled-old" nil "cat")
                  piem--process old-proc))
          (cl-letf (((symbol-function 'piem--start-process)
                     (lambda (_dir)
                       (setq new-proc
                             (start-process "test-reload-cancelled-new" nil "cat"))))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc msg cb)
                       (when (equal (plist-get msg :type) "switch_session")
                         (funcall cb '(:success t :data (:cancelled t))))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq shown-message (apply #'format fmt args)))))
            (with-current-buffer chat-buf
              (piem-reload)
              (should (eq piem--process old-proc))
              (should (process-live-p old-proc))
              (should-not (process-live-p new-proc))
              (should-not (piem--session-transition-active-p))
              (should (equal shown-message "Pi: Reload cancelled")))))
      (when (and old-proc (process-live-p old-proc))
        (delete-process old-proc))
      (when (and new-proc (process-live-p new-proc))
        (delete-process new-proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-reload-shows-immediate-feedback ()
  "Reload reports progress before the async session switch finishes."
  (let* ((shown-message nil)
         (chat-buf (get-buffer-create "*piem-test-reload-feedback-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--state '(:session-file "/tmp/test-session.json"))
            (let ((dead-proc (start-process "test-dead" nil "true")))
              (should (piem-test-wait-for-process-exit dead-proc))
              (setq piem--process dead-proc))
            (cl-letf (((symbol-function 'piem--start-process)
                       (lambda (_dir)
                         (let ((proc (start-process "test-new" nil "cat")))
                           (set-process-query-on-exit-flag proc nil)
                           proc)))
                      ((symbol-function 'piem--rpc-async)
                       (lambda (_proc _msg _cb) nil))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq shown-message (apply #'format fmt args)))))
              (piem-reload)
              (should (equal shown-message "Pi: Reloading...")))))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (when (and piem--process (process-live-p piem--process))
            (delete-process piem--process)))
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-reload-fails-without-session-file ()
  "Reload shows error when no session file in state."
  (let* ((error-shown nil)
         (chat-buf (get-buffer-create "*piem-test-reconnect-no-session*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            ;; State without session file
            (setq piem--state '(:model (:name "test-model")))
            ;; Dead process
            (let ((dead-proc (start-process "test-dead" nil "true")))
              (should (piem-test-wait-for-process-exit dead-proc))
              (setq piem--process dead-proc))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest _args)
                         (when (string-match-p "No session" fmt)
                           (setq error-shown t)))))
              (piem-reload)
              (should error-shown))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-reload-rebuilds-session-history ()
  "Reload replays current session history, including thinking and tool output."
  (let* ((chat-buf (get-buffer-create "*piem-test-reload-history-chat*"))
         (rpc-calls nil)
         (messages [(:role "user"
                     :content [(:type "text" :text "How should reload behave?")]
                     :timestamp 1704067200000)
                    (:role "assistant"
                     :content [(:type "text" :text "Answer first.")
                               (:type "thinking" :thinking "Need to double-check.")
                               (:type "toolCall" :id "tc1"
                                :name "read"
                                :arguments (:path "foo.el"))]
                     :timestamp 1704067201000)
                    (:role "toolResult" :toolCallId "tc1"
                     :toolName "read"
                     :content [(:type "text" :text "(defun foo ())")]
                     :isError :json-false
                     :timestamp 1704067202000)])
         (new-proc nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--thinking-display 'visible
                  piem--state '(:session-file "test-session.jsonl"
                                           :model (:name "test-model")))
            (let ((inhibit-read-only t))
              (insert "STALE CONTENT\n"))
            (let ((dead-proc (start-process "test-reload-history-dead" nil "true")))
              (should (piem-test-wait-for-process-exit dead-proc))
              (setq piem--process dead-proc)))
          (cl-letf (((symbol-function 'piem--start-process)
                     (lambda (_dir)
                       (setq new-proc (start-process "test-reload-history-new" nil "cat"))
                       new-proc))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd cb)
                       (push (plist-get cmd :type) rpc-calls)
                       (pcase (plist-get cmd :type)
                         ("switch_session"
                          (funcall cb '(:success t :data (:cancelled :false))))
                         ("get_state"
                          (funcall cb '(:success t
                                        :data (:model (:name "reloaded-model")
                                               :thinkingLevel "medium"
                                               :isStreaming :json-false
                                               :isCompacting :json-false
                                               :sessionId "reload-session"
                                               :sessionFile "test-session.jsonl"
                                               :messageCount 3
                                               :pendingMessageCount 0))))
                         ("get_messages"
                          (funcall cb (list :success t :data (list :messages messages))))
                         ("get_commands"
                          (funcall cb '(:success t :data (:commands []))))
                         (_ (ert-fail (format "Unexpected RPC during reload test: %S" cmd))))))
                    ((symbol-function 'piem--update-session-name-from-file) #'ignore)
                    ((symbol-function 'piem--refresh-header) #'ignore)
                    ((symbol-function 'piem--rebuild-commands-menu) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer chat-buf
              (piem-reload)))
          (with-current-buffer chat-buf
            (let ((text (buffer-string)))
              (should-not (string-match-p "STALE CONTENT" text))
              (should (string-match-p "How should reload behave\\?" text))
              (should (string-match-p "Answer first\\." text))
              (should (string-match-p "> Need to double-check\\." text))
              (should (string-match-p "read foo\\.el" text))
              (should (string-match-p "(defun foo ())" text))))
          (should (member "get_messages" rpc-calls)))
      (when (and new-proc (process-live-p new-proc))
        (delete-process new-proc))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (when (and piem--process (process-live-p piem--process))
            (delete-process piem--process)))
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-reload-transition-waits-for-state-and-history ()
  "Reload keeps sends blocked until both state and history callbacks settle."
  (let* ((dir (piem-test--make-temp-directory
               "piem-test-reload-transition-"))
         (session-file (expand-file-name "current.jsonl" dir))
         (chat-buf (generate-new-buffer "*piem-test-reload-transition-chat*"))
         (input-buf (generate-new-buffer "*piem-test-reload-transition-input*"))
         (old-proc nil)
         (new-proc nil)
         (switch-cb nil)
         (state-cb nil)
         (history-cb nil)
         (sent-text nil))
    (unwind-protect
        (progn
          (with-temp-file session-file (insert ""))
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (piem--set-chat-session-identity dir)
            (piem--set-input-buffer input-buf)
            (setq old-proc (start-process "test-reload-transition-old" nil "cat")
                  piem--process old-proc
                  piem--status 'idle
                  piem--state (list :session-file session-file)))
          (with-current-buffer input-buf
            (piem-input-mode)
            (piem--set-chat-buffer chat-buf))
          (cl-letf (((symbol-function 'piem--start-process)
                     (lambda (_dir)
                       (setq new-proc
                             (start-process "test-reload-transition-new" nil "cat"))
                       new-proc))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd cb)
                       (pcase (plist-get cmd :type)
                         ("switch_session" (setq switch-cb cb))
                         ("get_state" (setq state-cb cb))
                         ("get_messages" (setq history-cb cb))
                         ("get_commands" nil)
                         (_ (ert-fail (format "Unexpected RPC: %S" cmd))))))
                    ((symbol-function 'piem--prepare-and-send)
                     (lambda (text &optional _queued)
                       (setq sent-text text)))
                    ((symbol-function 'piem--update-session-name-from-file)
                     #'ignore)
                    ((symbol-function 'piem--refresh-header) #'ignore)
                    ((symbol-function 'piem--rebuild-commands-menu) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer chat-buf
              (piem-reload))
            (should switch-cb)
            (funcall switch-cb '(:success t :data (:cancelled :false)))
            (should state-cb)
            (should history-cb)
            (with-current-buffer chat-buf
              (should (piem--session-transition-active-p)))
            (with-current-buffer input-buf
              (insert "prompt while reloading")
              (piem-send)
              (should (equal (buffer-string) "prompt while reloading")))
            (should-not sent-text)
            (funcall state-cb
                     `(:success t
                       :data (:model (:name "model")
                              :thinkingLevel "medium"
                              :isStreaming :json-false
                              :isCompacting :json-false
                              :sessionId "reloaded"
                              :sessionFile ,session-file
                              :messageCount 0
                              :pendingMessageCount 0)))
            (with-current-buffer chat-buf
              (should (piem--session-transition-active-p)))
            (with-current-buffer input-buf
              (piem-send)
              (should (equal (buffer-string) "prompt while reloading")))
            (should-not sent-text)
            (funcall history-cb '(:success t :data (:messages [])))
            (with-current-buffer chat-buf
              (should-not (piem--session-transition-active-p)))
            (with-current-buffer input-buf
              (piem-send))
            (should (equal sent-text "prompt while reloading"))))
      (when (and old-proc (process-live-p old-proc))
        (delete-process old-proc))
      (when (and new-proc (process-live-p new-proc))
        (delete-process new-proc))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory dir t))))

(ert-deftest piem-test-reload-command-fetch-error-waits-for-refresh ()
  "Command-fetch scheduling errors do not finish a reload transition early."
  (let* ((dir (piem-test--make-temp-directory
               "piem-test-reload-command-fetch-"))
         (session-file (expand-file-name "current.jsonl" dir))
         (chat-buf (generate-new-buffer
                    "*piem-test-reload-command-fetch-chat*"))
         (old-proc nil)
         (new-proc nil)
         (switch-cb nil)
         (state-cb nil)
         (history-cb nil))
    (unwind-protect
        (progn
          (with-temp-file session-file (insert ""))
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (piem--set-chat-session-identity dir)
            (setq old-proc (start-process "test-reload-cmd-old" nil "cat")
                  piem--process old-proc
                  piem--status 'idle
                  piem--state (list :session-file session-file)))
          (cl-letf (((symbol-function 'piem--start-process)
                     (lambda (_dir)
                       (setq new-proc
                             (start-process "test-reload-cmd-new" nil "cat"))))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd cb)
                       (pcase (plist-get cmd :type)
                         ("switch_session" (setq switch-cb cb))
                         ("get_state" (setq state-cb cb))
                         ("get_messages" (setq history-cb cb))
                         (_ (ert-fail (format "Unexpected RPC: %S" cmd))))))
                    ((symbol-function 'piem--fetch-commands)
                     (lambda (&rest _)
                       (error "commands unavailable")))
                    ((symbol-function 'piem--update-session-name-from-file)
                     #'ignore)
                    ((symbol-function 'piem--display-session-history)
                     #'ignore)
                    ((symbol-function 'piem--refresh-header) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer chat-buf
              (piem-reload))
            (should switch-cb)
            (funcall switch-cb '(:success t :data (:cancelled :false)))
            (should state-cb)
            (should history-cb)
            (with-current-buffer chat-buf
              (should (piem--session-transition-active-p)))
            (funcall state-cb
                     `(:success t
                       :data (:model (:name "model")
                              :thinkingLevel "medium"
                              :isStreaming :json-false
                              :isCompacting :json-false
                              :sessionId "reloaded"
                              :sessionFile ,session-file
                              :messageCount 0
                              :pendingMessageCount 0)))
            (with-current-buffer chat-buf
              (should (piem--session-transition-active-p)))
            (funcall history-cb '(:success t :data (:messages [])))
            (with-current-buffer chat-buf
              (should-not (piem--session-transition-active-p)))))
      (when (and old-proc (process-live-p old-proc))
        (delete-process old-proc))
      (when (and new-proc (process-live-p new-proc))
        (delete-process new-proc))
      (piem-test--kill-live-buffers chat-buf)
      (delete-directory dir t))))

(ert-deftest piem-test-transition-refresh-failure-releases ()
  "A failed state/history refresh still unlocks the active transition."
  (let ((chat-buf (generate-new-buffer "*piem-test-transition-failure*"))
        (proc 'mock-proc)
        (state-cb nil)
        (history-cb nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--process proc)
            (let ((generation (piem--begin-session-transition proc)))
              (cl-letf (((symbol-function 'piem--rpc-async)
                         (lambda (_proc cmd cb)
                           (pcase (plist-get cmd :type)
                             ("get_state" (setq state-cb cb))
                             ("get_messages" (setq history-cb cb))
                             (_ (ert-fail (format "Unexpected RPC: %S" cmd)))))))
                (piem--refresh-transition-state-and-history
                 proc chat-buf generation))))
          (should state-cb)
          (should history-cb)
          (funcall state-cb '(:success :false :error "state failed"))
          (with-current-buffer chat-buf
            (should (piem--session-transition-active-p)))
          (funcall history-cb '(:success :false :error "history failed"))
          (with-current-buffer chat-buf
            (should-not (piem--session-transition-active-p))))
      (piem-test--kill-live-buffers chat-buf))))

(ert-deftest piem-test-stale-transition-refresh-cannot-finish-newer ()
  "An old transition callback cannot unlock a newer transition generation."
  (let ((chat-buf (generate-new-buffer "*piem-test-stale-transition*"))
        (proc 'mock-proc)
        (state-cb nil)
        (history-cb nil)
        (new-generation nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--process proc)
            (let ((old-generation (piem--begin-session-transition proc)))
              (cl-letf (((symbol-function 'piem--rpc-async)
                         (lambda (_proc cmd cb)
                           (pcase (plist-get cmd :type)
                             ("get_state" (setq state-cb cb))
                             ("get_messages" (setq history-cb cb))
                             (_ (ert-fail (format "Unexpected RPC: %S" cmd)))))))
                (piem--refresh-transition-state-and-history
                 proc chat-buf old-generation))))
          (funcall state-cb '(:success :false :error "state failed"))
          (with-current-buffer chat-buf
            (setq new-generation
                  (piem--begin-session-transition proc)))
          (funcall history-cb '(:success :false :error "history failed"))
          (with-current-buffer chat-buf
            (should (piem--session-transition-active-p))
            (should (= piem--session-transition-generation
                       new-generation))
            (piem--finish-session-transition new-generation)))
      (piem-test--kill-live-buffers chat-buf))))

(ert-deftest piem-test-load-session-history-ignores-stale-older-response ()
  "Only the newest in-flight history request may rebuild the chat buffer."
  (let* ((chat-buf (get-buffer-create "*piem-test-history-load-generation*"))
         (callbacks nil)
         (proc (start-process "test-history-load-generation" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--process proc))
          (cl-letf (((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd cb)
                       (should (equal (plist-get cmd :type) "get_messages"))
                       (push cb callbacks)))
                    ((symbol-function 'piem--refresh-header) #'ignore))
            (piem--load-session-history proc nil chat-buf)
            (piem--load-session-history proc nil chat-buf))
          (should (= 2 (length callbacks)))
          (let ((newer (car callbacks))
                (older (cadr callbacks))
                (newer-messages [(:role "assistant"
                                  :content [(:type "text" :text "Newer history")]
                                  :timestamp 1704067200000)])
                (older-messages [(:role "assistant"
                                  :content [(:type "text" :text "Older history")]
                                  :timestamp 1704067201000)]))
            (funcall newer (list :success t :data (list :messages newer-messages)))
            (with-current-buffer chat-buf
              (should (string-match-p "Newer history" (buffer-string)))
              (should-not (string-match-p "Older history" (buffer-string))))
            (funcall older (list :success t :data (list :messages older-messages)))
            (with-current-buffer chat-buf
              (should (string-match-p "Newer history" (buffer-string)))
              (should-not (string-match-p "Older history" (buffer-string))))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-reset-session-state-keeps-history-load-generation-monotonic ()
  "Resetting session state must not let old history callbacks collide with new ones."
  (let* ((chat-buf (get-buffer-create "*piem-test-history-reset-generation*"))
         (callbacks nil)
         (proc (start-process "test-history-reset-generation" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--process proc))
          (cl-letf (((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd cb)
                       (should (equal (plist-get cmd :type) "get_messages"))
                       (push cb callbacks)))
                    ((symbol-function 'piem--refresh-header) #'ignore))
            (piem--load-session-history proc nil chat-buf)
            (with-current-buffer chat-buf
              (piem--reset-session-state)
              (setq piem--process proc))
            (piem--load-session-history proc nil chat-buf))
          (should (= 2 (length callbacks)))
          (let ((newer (car callbacks))
                (older (cadr callbacks)))
            (funcall newer '(:success t :data (:messages [(:role "assistant"
                                                   :content [(:type "text" :text "New session history")]
                                                   :timestamp 1704067200000)])))
            (with-current-buffer chat-buf
              (should (string-match-p "New session history" (buffer-string))))
            (funcall older '(:success t :data (:messages [(:role "assistant"
                                                   :content [(:type "text" :text "Old session history")]
                                                   :timestamp 1704067201000)])))
            (with-current-buffer chat-buf
              (should (string-match-p "New session history" (buffer-string)))
              (should-not (string-match-p "Old session history" (buffer-string))))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-refresh-session-state-ignores-stale-older-response ()
  "Only the newest async get_state refresh may update the chat buffer state."
  (let* ((chat-buf (get-buffer-create "*piem-test-refresh-session-state*"))
         (callbacks nil)
         (proc (start-process "test-refresh-session-state" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--process proc))
          (cl-letf (((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd cb)
                       (should (equal (plist-get cmd :type) "get_state"))
                       (push cb callbacks)))
                    ((symbol-function 'piem--update-session-name-from-file) #'ignore)
                    ((symbol-function 'force-mode-line-update) #'ignore))
            (piem--refresh-session-state proc chat-buf)
            (piem--refresh-session-state proc chat-buf))
          (should (= 2 (length callbacks)))
          (let ((newer (car callbacks))
                (older (cadr callbacks))
                (newer-response
                 '(:success t :data (:model (:name "new")
                                    :thinkingLevel "medium"
                                    :isStreaming :json-false
                                    :isCompacting :json-false
                                    :sessionId "new-session"
                                    :sessionFile "new-session.jsonl"
                                    :messageCount 3
                                    :pendingMessageCount 0)))
                (older-response
                 '(:success t :data (:model (:name "old")
                                    :thinkingLevel "low"
                                    :isStreaming :json-false
                                    :isCompacting :json-false
                                    :sessionId "old-session"
                                    :sessionFile "old-session.jsonl"
                                    :messageCount 1
                                    :pendingMessageCount 0))))
            (funcall older older-response)
            (with-current-buffer chat-buf
              (should-not piem--state))
            (funcall newer newer-response)
            (with-current-buffer chat-buf
              (should (equal (plist-get piem--state :session-file)
                             (expand-file-name
                              "new-session.jsonl"
                              (piem--chat-session-directory)))))
            (funcall older older-response)
            (with-current-buffer chat-buf
              (should (equal (plist-get piem--state :session-file)
                             (expand-file-name
                              "new-session.jsonl"
                              (piem--chat-session-directory)))))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-refresh-session-state-skips-duplicate-name-scan ()
  "Refreshing state does not re-read the same session file for its name."
  (let* ((chat-buf (get-buffer-create "*piem-test-refresh-name*"))
         (session-file "/tmp/pi-session.jsonl")
         (update-calls 0)
         (proc (start-process "test-refresh-session-name" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--process proc))
          (cl-letf (((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd cb)
                       (should (equal (plist-get cmd :type) "get_state"))
                       (funcall cb `(:success t
                                     :data (:model (:name "model")
                                            :thinkingLevel "medium"
                                            :isStreaming :json-false
                                            :isCompacting :json-false
                                            :sessionId "session-id"
                                            :sessionFile ,session-file
                                            :messageCount 0
                                            :pendingMessageCount 0)))))
                    ((symbol-function 'piem--update-session-name-from-file)
                     (lambda (_session-file)
                       (setq update-calls (1+ update-calls))
                       '(:name "Cached name")))
                    ((symbol-function 'force-mode-line-update) #'ignore))
            (piem--refresh-session-state proc chat-buf session-file))
          (should (= update-calls 1)))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-send-resets-activity-when-process-dead ()
  "Sending when process is dead resets activity phase and status."
  (let ((chat-buf (get-buffer-create "*piem-test-process-dead*"))
        (input-buf (get-buffer-create "*piem-test-process-dead-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--input-buffer input-buf
                  piem--activity-phase "running"
                  piem--status 'idle)
            ;; Set up dead process
            (let ((dead-proc (start-process "test-dead" nil "true")))
              (should (piem-test-wait-for-process-exit dead-proc))
              (setq piem--process dead-proc)))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "test message")
            (piem-send))
          ;; Verify activity phase and status reset
          (with-current-buffer chat-buf
            (should (equal piem--activity-phase "idle"))
            (should (eq piem--status 'idle))))
      (when (buffer-live-p chat-buf) (kill-buffer chat-buf))
      (when (buffer-live-p input-buf) (kill-buffer input-buf)))))

;;; Slash Commands via RPC (get_commands)

(ert-deftest piem-test-fetch-commands-parses-response ()
  "fetch-commands extracts command list from RPC response."
  (let* ((callback-result nil)
         (mock-response '(:success t
                          :data (:commands
                                 [(:name "fix-tests" :description "Fix tests" :source "prompt")
                                  (:name "session-name" :description "Set name" :source "extension")])))
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'piem--rpc-async)
                   (lambda (_proc _msg callback)
                     (funcall callback mock-response))))
          (piem--fetch-commands fake-proc
            (lambda (commands)
              (setq callback-result commands)))
          ;; Verify commands were extracted correctly
          (should (= (length callback-result) 2))
          (should (equal (plist-get (car callback-result) :name) "fix-tests"))
          (should (equal (plist-get (cadr callback-result) :source) "extension")))
      (delete-process fake-proc))))

(ert-deftest piem-test-fetch-commands-handles-failure ()
  "fetch-commands does not call callback on RPC failure."
  (let* ((callback-called nil)
         (mock-response '(:success :false :error "Connection failed"))
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'piem--rpc-async)
                   (lambda (_proc _msg callback)
                     (funcall callback mock-response))))
          (piem--fetch-commands fake-proc
            (lambda (_) (setq callback-called t)))
          (should-not callback-called))
      (delete-process fake-proc))))

(ert-deftest piem-test-fetch-commands-ignores-unsafe-source-paths ()
  "Passive command fetch does not store invalid source paths as navigable."
  (let* ((bad (concat "/tmp/a" (string ?\0) "b.md"))
         (callback-result nil)
         (mock-response
          (list :success t
                :data (list
                       :commands
                       (vector
                        (list :name "nul" :source "prompt"
                              :sourceInfo (list :scope "project" :path bad))
                        '(:name "other-remote" :source "prompt"
                          :sourceInfo (:scope "project"
                                       :path "/ssh:other:/tmp/fix.md"))
                        '(:name "ok" :source "prompt"
                          :sourceInfo (:scope "project"
                                       :path "prompts/ok.md"))))))
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'piem--rpc-async)
                   (lambda (_proc _msg callback)
                     (funcall callback mock-response))))
          (piem--fetch-commands
           fake-proc
           (lambda (commands)
             (setq callback-result commands))
           "/ssh:pi-host:/home/pi/project/")
          (should (= (length callback-result) 3))
          (should-not (plist-get (nth 0 callback-result) :path))
          (should-not (plist-get (nth 1 callback-result) :path))
          (should (equal (plist-get (nth 2 callback-result) :path)
                         "/ssh:pi-host:/home/pi/project/prompts/ok.md")))
      (delete-process fake-proc))))

(ert-deftest piem-test-set-commands-propagates-to-input ()
  "set-commands propagates commands to input buffer."
  (with-temp-buffer
    (let* ((input-buf (generate-new-buffer "*test-input*"))
           (piem--input-buffer input-buf)
           (commands '((:name "test" :description "Test cmd" :source "prompt"))))
      (unwind-protect
          (progn
            (piem--set-commands commands)
            ;; Verify local variable set in current buffer
            (should (equal piem--commands commands))
            ;; Verify propagated to input buffer
            (should (equal (buffer-local-value 'piem--commands input-buf)
                           commands)))
        (kill-buffer input-buf)))))

(ert-deftest piem-test-command-capf-uses-commands ()
  "command-capf completion uses piem--commands."
  (with-temp-buffer
    (let ((piem--commands
           '((:name "fix-tests" :description "Fix" :source "prompt")
             (:name "review" :description "Review" :source "prompt"))))
      (insert "/")
      (let ((completion (piem--command-capf)))
        (should completion)
        ;; Third element is the completion candidates
        (should (member "fix-tests" (nth 2 completion)))
        (should (member "review" (nth 2 completion)))))))

(ert-deftest piem-test-run-command-formats-command-text ()
  "run-command builds literal slash commands from NAME and optional args."
  (let ((sent-messages nil)
        (fake-proc (start-process "test" nil "cat")))
    (set-process-query-on-exit-flag fake-proc nil)
    (unwind-protect
        (with-temp-buffer
          (piem-chat-mode)
          (let ((piem--process fake-proc))
            (cl-letf (((symbol-function 'piem--rpc-async)
                       (lambda (_proc msg _cb)
                         (push (plist-get msg :message) sent-messages))))
              (piem-run-command "greet")
              (piem-run-command "greet" "")
              (piem-run-command "greet" "world")
              (should (equal (nreverse sent-messages)
                             '("/greet" "/greet" "/greet world"))))))
      (delete-process fake-proc))))

(ert-deftest piem-test-run-command-uses-linked-input-session ()
  "run-command sends through the chat buffer linked to current input."
  (let ((sent-message nil)
        (fake-proc (start-process "test" nil "cat"))
        (chat-buf (generate-new-buffer " *pi-command-chat*"))
        (input-buf (generate-new-buffer " *pi-command-input*")))
    (set-process-query-on-exit-flag fake-proc nil)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--process fake-proc)
            (piem--set-input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (piem--set-chat-buffer chat-buf)
            (cl-letf (((symbol-function 'piem--rpc-async)
                       (lambda (_proc msg _cb)
                         (setq sent-message (plist-get msg :message)))))
              (piem-run-command "greet" "world")))
          (should (equal sent-message "/greet world")))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-process fake-proc))))

(ert-deftest piem-test-run-command-requires-current-session ()
  "run-command reports a missing current pi session."
  (with-temp-buffer
    (should-error (piem-run-command "greet")
                  :type 'user-error)))

(ert-deftest piem-test-run-command-interactive-requires-session-first ()
  "run-command reports a missing session before prompting interactively."
  (with-temp-buffer
    (let (prompted)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _args)
                   (setq prompted t)
                   "greet"))
                ((symbol-function 'read-string)
                 (lambda (&rest _args)
                   (setq prompted t)
                   "")))
        (should-error (call-interactively #'piem-run-command)
                      :type 'user-error)
        (should-not prompted)))))

(ert-deftest piem-test-run-custom-command-sends-literal ()
  "run-custom-command sends literal /command text, not expanded."
  (let* ((sent-message nil)
         (fake-proc (start-process "test" nil "cat"))
         (cmd '(:name "greet" :description "Greet" :source "prompt")))
    (unwind-protect
        (with-temp-buffer
          (piem-chat-mode)
          (let ((piem--process fake-proc))
            (cl-letf (((symbol-function 'piem--get-chat-buffer)
                       (lambda () (current-buffer)))
                      ((symbol-function 'piem--rpc-async)
                       (lambda (_proc msg _cb)
                         (setq sent-message (plist-get msg :message))))
                      ((symbol-function 'read-string)
                       (lambda (&rest _args) "world")))
              (piem--run-custom-command cmd)
              ;; Should send literal /greet world, NOT expanded prompt
              (should (equal sent-message "/greet world")))))
      (delete-process fake-proc))))

(ert-deftest piem-test-run-custom-command-empty-args ()
  "run-custom-command with empty args sends just /command."
  ;; Note: Use "mycommand" not "compact" to avoid collision with built-in /compact handling
  (let* ((sent-message nil)
         (fake-proc (start-process "test" nil "cat"))
         (cmd '(:name "mycommand" :description "My Command" :source "extension")))
    (unwind-protect
        (with-temp-buffer
          (piem-chat-mode)
          (let ((piem--process fake-proc))
            (cl-letf (((symbol-function 'piem--get-chat-buffer)
                       (lambda () (current-buffer)))
                      ((symbol-function 'piem--rpc-async)
                       (lambda (_proc msg _cb)
                         (setq sent-message (plist-get msg :message))))
                      ((symbol-function 'read-string)
                       (lambda (&rest _args) "")))
              (piem--run-custom-command cmd)
              ;; Should send just /mycommand without trailing space
              (should (equal sent-message "/mycommand")))))
      (delete-process fake-proc))))

(ert-deftest piem-test-rebuild-menu-shows-prompt-source-as-templates ()
  "rebuild-commands-menu creates Templates section for source \"prompt\".
Pi v0.51.3+ renamed SlashCommandSource from \"template\" to \"prompt\"."
  (let ((piem--commands
         '((:name "fix-tests" :description "Fix tests" :source "prompt" :location "user")
           (:name "review" :description "Code review" :source "prompt" :location "project"))))
    (unwind-protect
        (progn
          (piem--rebuild-commands-menu)
          (should (transient-get-suffix 'piem-menu '(3))))
      (ignore-errors (transient-remove-suffix 'piem-menu '(3))))))

(defun piem-test--suffix-key-bound-p (key)
  "Return non-nil if KEY is bound in current transient suffixes."
  (cl-find-if (lambda (obj) (equal (oref obj key) key))
              transient--suffixes))

(ert-deftest piem-test-transient-opens-session-and-tree-browsers ()
  "The main menu exposes the session and tree browser actions."
  (transient-setup 'piem-menu)
  (let ((sessions-suffix
         (piem-test--suffix-key-bound-p "r"))
        (tree-suffix
         (piem-test--suffix-key-bound-p "w")))
    (should sessions-suffix)
    (should (equal (transient-format-description sessions-suffix)
                   "sessions"))
    (should (eq (oref sessions-suffix command)
                'piem-session-browser))
    (should tree-suffix)
    (should (equal (transient-format-description tree-suffix)
                   "tree"))
    (should (eq (oref tree-suffix command)
                'piem-tree-browser))))

(ert-deftest piem-test-submenus-open-with-no-commands ()
  "All submenus open without error when no commands are loaded."
  (let ((piem--commands nil))
    (dolist (menu '(piem-templates-menu
                    piem-extensions-menu
                    piem-skills-menu))
      (transient-setup menu))))

(ert-deftest piem-test-templates-menu-shows-run-keys ()
  "Templates submenu binds letter keys to commands."
  (let ((piem--commands
         '((:name "test-tmpl" :description "A template" :source "prompt"))))
    (transient-setup 'piem-templates-menu)
    (should (piem-test--suffix-key-bound-p "a"))))

(ert-deftest piem-test-templates-menu-shows-edit-keys ()
  "Templates submenu binds uppercase letter keys to edit file paths."
  (let ((piem--commands
         '((:name "uncle-bob" :description "Uncle Bob review"
            :source "prompt" :path "/tmp/uncle-bob.md" :location "user")
           (:name "fix-tests" :description "Fix tests"
            :source "prompt" :path "/tmp/fix-tests.md" :location "project"))))
    (transient-setup 'piem-templates-menu)
    (should (piem-test--suffix-key-bound-p "a"))
    (should (piem-test--suffix-key-bound-p "A"))))

(ert-deftest piem-test-stats-uses-i-key-not-S ()
  "Stats is bound to `i' so it doesn't conflict with Skills `S' key."
  (transient-setup 'piem-menu)
  (should (piem-test--suffix-key-bound-p "i"))
  (should-not (piem-test--suffix-key-bound-p "S")))

(ert-deftest piem-test-submenu-handles-more-than-9-commands ()
  "Submenu with 13 skills uses letter keys without crashing."
  (let ((piem--commands
         (cl-loop for i from 1 to 13
                  collect (list :name (format "skill-%d" i)
                                :description (format "Skill number %d" i)
                                :source "skill"
                                :location "user"))))
    ;; Should not signal an error
    (transient-setup 'piem-skills-menu)
    ;; First and last should be bound
    (should (piem-test--suffix-key-bound-p "a"))
    (should (piem-test--suffix-key-bound-p "m"))))

(ert-deftest piem-test-submenu-run-and-edit-keys-correspond ()
  "Run key `a' and edit key `A' refer to the same command."
  (let ((piem--commands
         '((:name "alpha" :description "First" :source "skill"
            :location "user" :path "/tmp/alpha.md")
           (:name "beta" :description "Second" :source "skill"
            :location "user" :path "/tmp/beta.md"))))
    (transient-setup 'piem-skills-menu)
    ;; Run keys a, b and edit keys A, B should all be bound
    (should (piem-test--suffix-key-bound-p "a"))
    (should (piem-test--suffix-key-bound-p "b"))
    (should (piem-test--suffix-key-bound-p "A"))
    (should (piem-test--suffix-key-bound-p "B"))))

;;; Manual Compaction

(ert-deftest piem-test-manual-compact-event-and-response-render-once ()
  "Manual compact success is rendered from compaction_end, not the RPC response."
  (let ((chat-buf (get-buffer-create "*piem-test-compact-render-once*"))
        (compact-callback nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle)
            (setq piem--followup-queue nil))
          (cl-letf (((symbol-function 'piem--get-process)
                     (lambda () 'mock-proc))
                    ((symbol-function 'process-live-p)
                     (lambda (_proc) t))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd cb)
                       (when (equal (plist-get cmd :type) "compact")
                         (setq compact-callback cb))))
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer chat-buf
              (piem-compact)
              (piem--handle-display-event
               '(:type "compaction_start" :reason "manual"))
              (piem--handle-display-event
               '(:type "compaction_end"
                 :reason "manual"
                 :aborted :false
                 :willRetry :false
                 :result (:tokensBefore 1234
                          :summary "Unique manual compaction summary"
                          :firstKeptEntryId "entry-1"
                          :details nil))))
            (should (functionp compact-callback))
            (funcall compact-callback
                     '(:success t
                       :data (:tokensBefore 1234
                              :summary "Unique manual compaction summary"
                              :firstKeptEntryId "entry-1"
                              :details nil)))
            (with-current-buffer chat-buf
              (should (= 1 (piem-test--count-matches
                            "Unique manual compaction summary"
                            (buffer-string)))))))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-compact-completion-event-processes-queued-followup ()
  "Manual compact queues local input until the compaction_end success event."
  (let ((chat-buf (get-buffer-create "*piem-test-compact-status*"))
        (input-buf (get-buffer-create "*piem-test-compact-status-input*"))
        (compact-callback nil)
        (prepared-texts nil)
        (prompt-sent nil)
        drain-callback
        drain-args)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle)
            (setq piem--process nil)
            (setq piem--input-buffer input-buf)
            (setq piem--followup-queue nil))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf))
          (cl-letf (((symbol-function 'piem--get-process)
                     (lambda () 'mock-proc))
                    ((symbol-function 'process-live-p)
                     (lambda (_proc) t))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd cb)
                       (if (equal (plist-get cmd :type) "compact")
                           (setq compact-callback cb)
                         (setq prompt-sent t))))
                    ((symbol-function 'piem--handle-compaction-success) #'ignore)
                    ((symbol-function 'piem--prepare-and-send)
                     (lambda (text &optional queued)
                       (push text prepared-texts)
                       (when queued
                         (piem--drop-followup text))))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args)
                       (setq drain-callback fn
                             drain-args args)
                       'fake-drain-timer))
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer chat-buf
              (piem-compact)
              (should (eq piem--status 'compacting)))

            (with-current-buffer input-buf
              (insert "queued during compaction")
              (piem-send)
              (should (string-empty-p (buffer-string))))

            (with-current-buffer chat-buf
              (should-not prompt-sent)
              (should (equal piem--followup-queue '("queued during compaction"))))

            (with-current-buffer chat-buf
              (piem--handle-display-event
               '(:type "compaction_end"
                 :reason "manual"
                 :aborted :false
                 :willRetry :false
                 :result (:tokensBefore 1234
                          :summary "Done"
                          :firstKeptEntryId "entry-1"
                          :details nil)))
              (should (eq piem--status 'idle))
              (should (functionp drain-callback))
              (should (equal piem--followup-queue
                             '("queued during compaction")))
              (apply drain-callback drain-args)
              (should (null piem--followup-queue)))
            (should (equal (reverse prepared-texts) '("queued during compaction")))

            (should (functionp compact-callback))
            (funcall compact-callback
                     '(:success t :data (:tokensBefore 1234 :summary "Done")))
            (should (equal (reverse prepared-texts) '("queued during compaction")))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-compact-response-failure-reports-without-event ()
  "A failed compact RPC response reports plumbing failure when no event ended it."
  (let ((chat-buf (get-buffer-create "*piem-test-compact-response-failure*"))
        (input-buf (get-buffer-create "*piem-test-compact-response-failure-input*"))
        (shown-message nil))
    (unwind-protect
        (progn
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf))
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'compacting
                  piem--input-buffer input-buf
                  piem--followup-queue '("queued during failed compact"))
            (piem--set-activity-phase "compact"))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq shown-message (apply #'format fmt args)))))
            (piem--handle-manual-compaction-response
             chat-buf
             '(:success :false :error "transport failed before compaction event")))
          (with-current-buffer chat-buf
            (should (eq piem--status 'idle))
            (should (equal piem--activity-phase "idle"))
            (should (null piem--followup-queue))
            (should-not (string-match-p "Compacted from" (buffer-string))))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "queued during failed compact")))
          (should (equal shown-message
                         "Pi: Compact failed: transport failed before compaction event")))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-compact-dead-process-keeps-idle ()
  "Manual compact should not transition state when process is dead."
  (let ((chat-buf (get-buffer-create "*piem-test-compact-dead-proc*"))
        (rpc-called nil)
        (shown-message nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle)
            (setq piem--followup-queue nil))
          (cl-letf (((symbol-function 'piem--get-process)
                     (lambda () 'dead-proc))
                    ((symbol-function 'process-live-p)
                     (lambda (_proc) nil))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (&rest _args)
                       (setq rpc-called t)))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq shown-message (apply #'format fmt args)))))
            (with-current-buffer chat-buf
              (piem-compact)
              (should (eq piem--status 'idle))))
          (should-not rpc-called)
          (should (equal shown-message
                         "Pi: Process died - try M-x piem-reload or C-c C-p R")))
      (kill-buffer chat-buf))))

(defun piem-test--seed-stale-session-rebuild-state (chat-buf stale-text)
  "Seed CHAT-BUF with stale state so a session rebuild must replace it.
STALE-TEXT is inserted into the buffer and also mirrored into the canonical
message cache so tests can prove both rendered and cached session state were
replaced by the resumed or forked history."
  (with-current-buffer chat-buf
    (setq piem--process 'mock-proc
          piem--state '(:session-id "old-session-id"
                                   :session-file "/tmp/old-session.jsonl"))
    (piem--set-canonical-messages
     [(:role "assistant"
       :content [(:type "text" :text "Old canonical history")]
       :timestamp 1704067200000)])
    (let ((inhibit-read-only t))
      (insert stale-text "\n"))
    (let ((tool-ov (make-overlay 1 6 nil nil nil)))
      (overlay-put tool-ov 'piem-tool-block t)
      (setq piem--pending-tool-overlay tool-ov))
    (puthash "old-tool" '(:path "/tmp/old.el") piem--tool-args-cache)
    (puthash "old-tool" '(:tool-call-id "old-tool") piem--live-tool-blocks)))

(defun piem-test--assert-clean-session-rebuild
    (chat-buf expected-messages stale-text)
  "Assert CHAT-BUF was rebuilt from EXPECTED-MESSAGES and cleared STALE-TEXT."
  (with-current-buffer chat-buf
    (should (equal piem--canonical-messages expected-messages))
    (should-not (string-match-p (regexp-quote stale-text) (buffer-string)))
    (should-not piem--pending-tool-overlay)
    (should (= 0 (hash-table-count piem--tool-args-cache)))
    (should (= 0 (hash-table-count piem--live-tool-blocks)))
    (should-not (cl-some (lambda (ov)
                           (overlay-get ov 'piem-tool-block))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest piem-test-update-session-name-from-file-uses-jsonl-name ()
  "Session-name refresh uses canonical JSONL metadata and clears absent names."
  (let* ((session-file "/tmp/piem-session-name.jsonl")
         (named-info (list :path session-file :name "Canonical name"))
         (unnamed-info (list :path session-file :cwd "/tmp"))
         (responses (list named-info unnamed-info))
         (scanner-paths nil))
    (with-temp-buffer
      (piem-chat-mode)
      (setq piem--session-name "Stale name")
      (cl-letf (((symbol-function 'piem-jsonl-read-session-info)
                 (lambda (path)
                   (push path scanner-paths)
                   (prog1 (car responses)
                     (setq responses (cdr responses))))))
        (should (equal (piem--update-session-name-from-file
                        session-file)
                       named-info))
        (should (equal piem--session-name "Canonical name"))
        (should (equal (piem--update-session-name-from-file
                        session-file)
                       unnamed-info))
        (should-not piem--session-name)
        (should-not responses)
        (should (equal (nreverse scanner-paths)
                       (list session-file session-file)))))))

(ert-deftest piem-test-session-file-cwd-or-error-returns-expanded-directory ()
  "Session-file cwd validator returns an expanded directory name."
  (let* ((project-dir (piem-test--make-temp-directory
                       "piem-test-project-"))
         (session-dir (piem-test--make-temp-directory
                       "piem-test-sessions-"))
         (session-file (expand-file-name "session.jsonl" session-dir)))
    (unwind-protect
        (let ((cwd (directory-file-name project-dir)))
          (piem-test--write-session-file session-file "hello" cwd)
          (should (equal (piem--session-file-cwd-or-error
                          session-file)
                         project-dir)))
      (delete-directory project-dir t)
      (delete-directory session-dir t))))

(ert-deftest piem-test-session-file-cwd-or-error-anchors-remote-cwd ()
  "Remote session header cwd is returned as a TRAMP directory."
  (let ((session-file "/ssh:pi-host:/home/pi/.pi/sessions/session.jsonl")
        (checked-dir nil)
        (scanner-called nil))
    (cl-letf (((symbol-function 'file-readable-p)
               (lambda (path)
                 (equal path session-file)))
              ((symbol-function 'piem-jsonl-read-session-info)
               (lambda (path)
                 (setq scanner-called t)
                 (should (equal path session-file))
                 '(:cwd "/home/pi/project")))
              ((symbol-function 'file-attributes)
               (lambda (&rest _)
                 (error "Unexpected direct metadata scan")))
              ((symbol-function 'file-directory-p)
               (lambda (path)
                 (setq checked-dir path)
                 (equal path "/ssh:pi-host:/home/pi/project/"))))
      (should (equal (piem--session-file-cwd-or-error session-file)
                     "/ssh:pi-host:/home/pi/project/"))
      (should scanner-called)
      (should (equal checked-dir "/ssh:pi-host:/home/pi/project/")))))

(ert-deftest piem-test-session-file-cwd-or-error-preserves-multi-hop-cwd ()
  "Remote session cwd anchoring keeps the full multi-hop TRAMP route."
  (let ((session-file
         "/ssh:bastion|sudo:root@pi-host:/home/pi/.pi/sessions/session.jsonl")
        (expected-dir "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
        (checked-dir nil)
        (scanner-called nil))
    (cl-letf (((symbol-function 'file-readable-p)
               (lambda (path) (equal path session-file)))
              ((symbol-function 'piem-jsonl-read-session-info)
               (lambda (path)
                 (setq scanner-called t)
                 (should (equal path session-file))
                 '(:cwd "/home/pi/project")))
              ((symbol-function 'file-attributes)
               (lambda (&rest _)
                 (error "Unexpected direct metadata scan")))
              ((symbol-function 'file-directory-p)
               (lambda (path)
                 (setq checked-dir path)
                 (equal path expected-dir))))
      (should (equal (piem--session-file-cwd-or-error session-file)
                     expected-dir))
      (should scanner-called)
      (should (equal checked-dir expected-dir)))))

(ert-deftest piem-test-session-file-cwd-or-error-rejects-remote-cwd ()
  "Session header cwd must be process-local before remote anchoring."
  (let ((session-file "/ssh:pi-host:/home/pi/.pi/sessions/session.jsonl")
        (scanner-called nil))
    (cl-letf (((symbol-function 'file-readable-p)
               (lambda (path)
                 (equal path session-file)))
              ((symbol-function 'piem-jsonl-read-session-info)
               (lambda (path)
                 (setq scanner-called t)
                 (should (equal path session-file))
                 '(:cwd "/ssh:pi-host:/home/pi/project")))
              ((symbol-function 'file-attributes)
               (lambda (&rest _)
                 (error "Unexpected direct metadata scan")))
              ((symbol-function 'file-directory-p)
               (lambda (_path)
                 (ert-fail "Remote cwd should be rejected before directory check"))))
      (should-error (piem--session-file-cwd-or-error session-file)
                    :type 'user-error)
      (should scanner-called))))

(ert-deftest piem-test-session-file-cwd-or-error-rejects-home-cwd ()
  "Session header cwd must not depend on home expansion."
  (let ((session-file "/ssh:pi-host:/home/pi/.pi/sessions/session.jsonl"))
    (dolist (cwd '("~" "~/project" "~root/project"))
      (let ((scanner-called nil))
        (cl-letf (((symbol-function 'file-readable-p)
                   (lambda (path)
                     (equal path session-file)))
                  ((symbol-function 'piem-jsonl-read-session-info)
                   (lambda (path)
                     (setq scanner-called t)
                     (should (equal path session-file))
                     (list :cwd cwd)))
                  ((symbol-function 'file-attributes)
                   (lambda (&rest _)
                     (error "Unexpected direct metadata scan")))
                  ((symbol-function 'file-directory-p)
                   (lambda (_path)
                     (ert-fail "Home cwd should be rejected before directory check"))))
          (ert-info ((format "cwd: %s" cwd))
            (should-error (piem--session-file-cwd-or-error session-file)
                          :type 'user-error)
            (should scanner-called)))))))

(ert-deftest piem-test-session-file-cwd-or-error-rejects-unreadable-file ()
  "Session-file cwd validator rejects unreadable files."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-sessions-"))
         (missing-file (expand-file-name "missing.jsonl" session-dir)))
    (unwind-protect
        (should-error (piem--session-file-cwd-or-error missing-file)
                      :type 'user-error)
      (delete-directory session-dir t))))

(ert-deftest piem-test-session-file-cwd-or-error-rejects-invalid-session-metadata ()
  "Session-file cwd validator rejects files without valid session metadata."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-sessions-"))
         (session-file (expand-file-name "not-a-session.jsonl" session-dir)))
    (unwind-protect
        (progn
          (with-temp-file session-file
            (insert "{\"type\":\"message\"}\n"))
          (should-error (piem--session-file-cwd-or-error session-file)
                        :type 'user-error))
      (delete-directory session-dir t))))

(ert-deftest piem-test-session-file-cwd-or-error-rejects-unusable-cwd ()
  "Session-file cwd validator rejects missing, non-string, and empty cwd values."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-sessions-"))
         (cases '(("missing" . "{\"type\":\"session\",\"id\":\"test\"}")
                  ("null" . "{\"type\":\"session\",\"id\":\"test\",\"cwd\":null}")
                  ("number" . "{\"type\":\"session\",\"id\":\"test\",\"cwd\":123}")
                  ("empty" . "{\"type\":\"session\",\"id\":\"test\",\"cwd\":\"\"}"))))
    (unwind-protect
        (dolist (case cases)
          (let ((session-file (expand-file-name
                               (format "%s.jsonl" (car case))
                               session-dir)))
            (with-temp-file session-file
              (insert (cdr case) "\n"))
            (ert-info ((format "cwd case: %s" (car case)))
              (should-error (piem--session-file-cwd-or-error
                             session-file)
                            :type 'user-error))))
      (delete-directory session-dir t))))

(ert-deftest piem-test-session-file-cwd-or-error-rejects-relative-cwd ()
  "Session-file cwd validator rejects cwd values that depend on default-directory."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-sessions-"))
         (session-file (expand-file-name "relative.jsonl" session-dir))
         (relative-cwd "relative-project"))
    (unwind-protect
        (progn
          (make-directory (expand-file-name relative-cwd session-dir))
          (piem-test--write-session-file
           session-file "hello" relative-cwd)
          (let* ((default-directory session-dir)
                 (error-data
                  (should-error (piem--session-file-cwd-or-error
                                 session-file)
                                :type 'user-error))
                 (message (cadr error-data)))
            (should (string-match-p (regexp-quote relative-cwd) message))
            (should (string-match-p (regexp-quote session-file) message))))
      (delete-directory session-dir t))))

(ert-deftest piem-test-session-file-cwd-or-error-rejects-stale-cwd ()
  "Session-file cwd validator rejects cwd values that do not name a directory."
  (let* ((session-dir (piem-test--make-temp-directory
                       "piem-test-sessions-"))
         (session-file (expand-file-name "stale.jsonl" session-dir))
         (stale-cwd (expand-file-name "deleted-project" session-dir)))
    (unwind-protect
        (progn
          (piem-test--write-session-file session-file "hello" stale-cwd)
          (let* ((error-data
                  (should-error (piem--session-file-cwd-or-error
                                 session-file)
                                :type 'user-error))
                 (message (cadr error-data)))
            (should (string-match-p (regexp-quote stale-cwd) message))
            (should (string-match-p (regexp-quote session-file) message))))
      (delete-directory session-dir t))))

(ert-deftest piem-test-session-list-directory-uses-session-file-parent ()
  "Session listing uses the current JSONL session file parent directory."
  (let* ((project-dir (piem-test--make-temp-directory
                       "piem-project-"))
         (expected-dir (file-name-as-directory
                        (expand-file-name "sessions" project-dir))))
    (unwind-protect
        (with-temp-buffer
          (piem-chat-mode)
          (piem--set-chat-session-identity project-dir)
          (setq piem--state '(:session-file "sessions/current.jsonl"))
          (should (equal (piem--session-list-directory (current-buffer))
                         expected-dir))
          (setq piem--state '(:session-file ""))
          (should-not (piem--session-list-directory (current-buffer)))
          (setq piem--state '(:session-file :json-false))
          (should-not (piem--session-list-directory (current-buffer))))
      (delete-directory project-dir t))))

(ert-deftest piem-test-session-list-directory-uses-remote-session-file-parent ()
  "Session listing works when state stores a normalized remote session file."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--set-chat-session-identity
     "/ssh:pi-host:/home/pi/project/")
    (setq piem--state
          '(:session-file "/ssh:pi-host:/home/pi/.pi/sessions/current.jsonl"))
    (should (equal (piem--session-list-directory (current-buffer))
                   "/ssh:pi-host:/home/pi/.pi/sessions/"))))

(ert-deftest piem-test-session-list-directory-preserves-multi-hop-parent ()
  "Session listing keeps the full multi-hop parent directory route."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--set-chat-session-identity
     "/ssh:bastion|sudo:root@pi-host:/home/pi/project/")
    (setq piem--state
          '(:session-file "sessions/current.jsonl"))
    (should (equal (piem--session-list-directory (current-buffer))
                   "/ssh:bastion|sudo:root@pi-host:/home/pi/project/sessions/"))))

(ert-deftest piem-test-resume-selected-session-sends-process-local-remote-path ()
  "Resuming a remote Emacs session file sends process-local sessionPath."
  (let ((proc (start-process "test-remote-resume" nil "cat"))
        (chat-buf (get-buffer-create "*piem-test-remote-resume-chat*"))
        (session-path-used nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (piem--set-chat-session-identity
             "/ssh:pi-host:/home/pi/project/"))
          (cl-letf (((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd cb)
                       (when (equal (plist-get cmd :type) "switch_session")
                         (setq session-path-used (plist-get cmd :sessionPath))
                         (funcall cb '(:success t :data (:cancelled :false))))))
                    ((symbol-function 'piem--session-file-cwd-or-error)
                     (lambda (_path) "/ssh:pi-host:/home/pi/project/"))
                    ((symbol-function 'piem--refresh-session-state)
                     #'ignore)
                    ((symbol-function 'piem--load-session-history)
                     #'ignore)
                    ((symbol-function 'piem--fetch-commands)
                     (lambda (_proc callback _anchor)
                       (funcall callback nil)))
                    ((symbol-function 'message) #'ignore))
            (piem--resume-selected-session
             proc chat-buf
             "/ssh:pi-host:/home/pi/.pi/sessions/target.jsonl"))
          (should (equal session-path-used
                         "/home/pi/.pi/sessions/target.jsonl")))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-resume-selected-session-retargets-session-directory ()
  "Resuming a cross-cwd session moves frontend path ownership too."
  (let* ((old-dir (piem-test--make-temp-directory
                   "piem-test-resume-old-cwd-"))
         (new-dir (piem-test--make-temp-directory
                   "piem-test-resume-new-cwd-"))
         (session-dir (piem-test--make-temp-directory
                       "piem-test-resume-cross-sessions-"))
         (target-session (expand-file-name "target.jsonl" session-dir))
         (chat-buf (generate-new-buffer "*piem-test-resume-cross-chat*"))
         (input-buf (generate-new-buffer "*piem-test-resume-cross-input*"))
         (proc 'mock-proc)
         (refresh-dir nil)
         (commands-anchor nil))
    (unwind-protect
        (progn
          (piem-test--write-session-file
           target-session "target" (directory-file-name new-dir))
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (piem--set-chat-session-identity old-dir)
            (piem--set-input-buffer input-buf)
            (setq piem--process proc))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq default-directory old-dir)
            (piem--set-chat-buffer chat-buf))
          (cl-letf (((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd cb)
                       (should (equal (plist-get cmd :type) "switch_session"))
                       (funcall cb '(:success t :data (:cancelled :false)))))
                    ((symbol-function 'piem--refresh-session-state)
                     (lambda (_proc chat _selected-path
                              &optional _generation _completion)
                       (setq refresh-dir
                             (with-current-buffer chat
                               (piem--chat-session-directory)))))
                    ((symbol-function 'piem--load-session-history)
                     #'ignore)
                    ((symbol-function 'piem--fetch-commands)
                     (lambda (_proc callback anchor)
                       (setq commands-anchor anchor)
                       (funcall callback nil)))
                    ((symbol-function 'message) #'ignore))
            (piem--resume-selected-session
             proc chat-buf target-session))
          (with-current-buffer chat-buf
            (should (equal (piem--chat-session-directory)
                           new-dir)))
          (with-current-buffer input-buf
            (should (equal default-directory new-dir)))
          (should (equal refresh-dir new-dir))
          (should (equal commands-anchor new-dir)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (setq piem--process nil)))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory old-dir t)
      (delete-directory new-dir t)
      (delete-directory session-dir t))))

(ert-deftest piem-test-resume-selected-session-duplicate-target-keeps-source-ready ()
  "Duplicate resume preflight does not leave the source session busy."
  (let* ((source-dir (piem-test--make-temp-directory
                      "piem-test-resume-duplicate-source-"))
         (target-dir (piem-test--make-temp-directory
                      "piem-test-resume-duplicate-target-"))
         (session-dir (piem-test--make-temp-directory
                       "piem-test-resume-duplicate-sessions-"))
         (target-session (expand-file-name "target.jsonl" session-dir))
         (source-chat (generate-new-buffer
                       "*piem-test-resume-duplicate-source*"))
         (target-chat (generate-new-buffer
                       "*piem-test-resume-duplicate-target*"))
         (proc 'mock-proc)
         (rpc-called nil)
         (initial-generation nil))
    (unwind-protect
        (progn
          (piem-test--write-session-file
           target-session "target" (directory-file-name target-dir))
          (with-current-buffer source-chat
            (piem-chat-mode)
            (piem--set-chat-session-identity source-dir)
            (setq piem--process proc
                  piem--status 'idle
                  initial-generation piem--session-transition-generation))
          (with-current-buffer target-chat
            (piem-chat-mode)
            (piem--set-chat-session-identity target-dir)
            (setq piem--status 'idle))
          (cl-letf (((symbol-function 'piem--rpc-async)
                     (lambda (&rest _args)
                       (setq rpc-called t)
                       (ert-fail "Duplicate resume target must not send RPC")))
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer source-chat
              (should-error
               (piem--resume-selected-session
                proc source-chat target-session)
               :type 'user-error))
            (should-not rpc-called)
            (with-current-buffer source-chat
              (should (= piem--session-transition-generation
                         initial-generation))
              (should-not (piem--session-transition-active-p))
              (should-not (piem--session-busy-p))
              (should (piem--session-transition-ready-p
                       source-chat "resume")))))
      (piem-test--kill-live-buffers target-chat source-chat)
      (delete-directory source-dir t)
      (delete-directory target-dir t)
      (delete-directory session-dir t))))

(ert-deftest piem-test-retarget-session-buffers-preserves-named-session ()
  "Cross-cwd resume keeps the frontend named-session identity."
  (let* ((old-dir (piem-test--make-temp-directory
                   "piem-test-retarget-name-old-"))
         (new-dir (piem-test--make-temp-directory
                   "piem-test-retarget-name-new-"))
         (chat-buf (generate-new-buffer "*piem-test-retarget-name-chat*"))
         (input-buf (generate-new-buffer "*piem-test-retarget-name-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (piem--set-chat-session-identity old-dir "side")
            (piem--set-input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (piem--set-chat-buffer chat-buf))
          (with-current-buffer chat-buf
            (piem--retarget-session-buffers new-dir)
            (should (equal (piem--chat-session-directory) new-dir))
            (should (equal (piem--chat-session-name) "side"))
            (should (equal (buffer-name)
                           (piem--buffer-name :chat new-dir "side"))))
          (with-current-buffer input-buf
            (should (equal default-directory new-dir))
            (should (equal (buffer-name)
                           (piem--buffer-name :input new-dir "side")))))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory old-dir t)
      (delete-directory new-dir t))))

(ert-deftest piem-test-resume-selected-session-switches-session-and-rebuilds-history ()
  "Resuming a selected session refreshes chat history and session state."
  (let* ((dir (piem-test--make-temp-directory
               "piem-test-resume-happy-"))
         (session-dir (piem-test--make-temp-directory
                       "piem-test-current-sessions-"))
         (target-session (expand-file-name "target.jsonl" session-dir))
         (resumed-session (expand-file-name "resumed.jsonl" session-dir))
         (shown-message nil)
         (name-scan-path nil)
         (rpc-calls nil))
    (unwind-protect
        (piem-test-with-mock-session dir
          (let* ((chat-buf (get-buffer (piem-test--chat-buffer-name dir)))
                 (messages [(:role "assistant"
                             :content [(:type "text" :text "Resumed history")]
                             :timestamp 1704067200000)]))
            (piem-test--write-session-file
             target-session "Resume target" (directory-file-name dir))
            (piem-test--seed-stale-session-rebuild-state
             chat-buf "STALE RESUME CONTENT")
            (cl-letf (((symbol-function 'piem--rpc-async)
                       (lambda (_proc cmd cb)
                         (push (plist-get cmd :type) rpc-calls)
                         (pcase (plist-get cmd :type)
                           ("switch_session"
                            (with-current-buffer chat-buf
                              (should (piem--session-transition-active-p)))
                            (should (equal (plist-get cmd :sessionPath)
                                           target-session))
                            (funcall cb '(:success t :data (:cancelled :false))))
                           ("get_state"
                            (with-current-buffer chat-buf
                              (should (piem--session-transition-active-p)))
                            (funcall cb `(:success t
                                          :data (:model (:name "resumed-model")
                                                 :thinkingLevel "medium"
                                                 :isStreaming :json-false
                                                 :isCompacting :json-false
                                                 :sessionId "resumed-session-id"
                                                 :sessionFile ,resumed-session
                                                 :messageCount 1
                                                 :pendingMessageCount 0))))
                           ("get_messages"
                            (with-current-buffer chat-buf
                              (should (piem--session-transition-active-p)))
                            (funcall cb (list :success t
                                              :data (list :messages messages))))
                           ("get_commands"
                            (funcall cb '(:success t :data (:commands []))))
                           (_
                            (ert-fail
                             (format "Unexpected RPC during resume test: %S"
                                     cmd))))))
                      ((symbol-function 'piem--update-session-name-from-file)
                       (lambda (path)
                         (setq name-scan-path path
                               piem--session-name "Resume target")
                         '(:name "Resume target")))
                      ((symbol-function 'piem--refresh-header) #'ignore)
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq shown-message (apply #'format fmt args)))))
              (piem--resume-selected-session
               'mock-proc chat-buf target-session))
            (with-current-buffer chat-buf
              (should (equal (plist-get piem--state :session-id)
                             "resumed-session-id"))
              (should (equal (plist-get piem--state :session-file)
                             resumed-session))
              (should (equal piem--session-name "Resume target"))
              (should (string-match-p "Resumed history" (buffer-string)))
              (should-not (piem--session-transition-active-p)))
            (piem-test--assert-clean-session-rebuild
             chat-buf messages "STALE RESUME CONTENT")
            (should (equal name-scan-path target-session))
            (should (equal (nreverse rpc-calls)
                           '("switch_session" "get_state" "get_messages"
                             "get_commands")))
            (should (equal shown-message "Pi: Resumed session (1 messages)"))))
      (delete-directory dir t)
      (delete-directory session-dir t))))

(ert-deftest piem-test-fork-from-input-switches-session-rebuilds-history-and-prefills-input ()
  "Forking from the input buffer rebuilds chat history and prefills input."
  (let ((dir "/tmp/piem-test-fork-happy/")
        (shown-message nil)
        (rpc-calls nil))
    (piem-test-with-mock-session dir
      (let* ((chat-buf (get-buffer (piem-test--chat-buffer-name dir)))
             (input-buf (get-buffer (piem-test--input-buffer-name dir)))
             (fork-messages [(:entryId "u1" :text "First question")
                             (:entryId "u2" :text "Second question")])
             (selected-choice
              (piem--format-fork-message
               '(:entryId "u2" :text "Second question") 1))
             (messages [(:role "user"
                         :content [(:type "text" :text "Second question")]
                         :timestamp 1704067200000)
                        (:role "assistant"
                         :content [(:type "text" :text "Forked answer")]
                         :timestamp 1704067201000)]))
        (piem-test--seed-stale-session-rebuild-state
         chat-buf "STALE FORK CONTENT")
        (with-current-buffer chat-buf
          (setq piem--state
                (plist-put piem--state :model
                           '(:name "Vision" :input ["text" "image"]))))
        (with-current-buffer input-buf
          (let ((path (make-temp-file "pi-prompt-attachment-" nil ".png")))
            (unwind-protect
                (progn
                  (piem-test--write-prompt-image path 'png)
                  (piem-test--attach-image path))
              (delete-file path)))
          (insert "old input text"))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) selected-choice))
                  ((symbol-function 'piem--rpc-async)
                   (lambda (_proc cmd cb)
                     (push (plist-get cmd :type) rpc-calls)
                     (pcase (plist-get cmd :type)
                       ("get_fork_messages"
                        (funcall cb (list :success t :data
                                          (list :messages fork-messages))))
                       ("fork"
                        (should (equal (plist-get cmd :entryId) "u2"))
                        (funcall cb '(:success t :data (:text "Second question"))))
                       ("get_state"
                        (funcall cb '(:success t
                                      :data (:model (:name "forked-model")
                                             :thinkingLevel "high"
                                             :isStreaming :json-false
                                             :isCompacting :json-false
                                             :sessionId "forked-session-id"
                                             :sessionFile "/tmp/forked.jsonl"
                                             :messageCount 2
                                             :pendingMessageCount 0))))
                       ("get_messages"
                        (funcall cb (list :success t :data (list :messages messages))))
                       (_
                        (ert-fail (format "Unexpected RPC during fork test: %S"
                                          cmd))))))
                  ((symbol-function 'piem--update-session-name-from-file)
                   #'ignore)
                  ((symbol-function 'piem--refresh-header) #'ignore)
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq shown-message (apply #'format fmt args)))))
          (with-current-buffer input-buf
            (piem-fork)))
        (with-current-buffer chat-buf
          (should (equal (plist-get piem--state :session-id)
                         "forked-session-id"))
          (should (equal (plist-get piem--state :session-file)
                         "/tmp/forked.jsonl"))
          (should (string-match-p "Second question" (buffer-string)))
          (should (string-match-p "Forked answer" (buffer-string))))
        (with-current-buffer input-buf
          (should (equal (buffer-string) "Second question"))
          (should-not (string-match-p
                       "pi-prompt-attachment-"
                       (piem-test--input-header))))
        (piem-test--assert-clean-session-rebuild
         chat-buf messages "STALE FORK CONTENT")
        (should (equal (nreverse rpc-calls)
                       '("get_fork_messages" "fork" "get_state" "get_messages")))
        (should (equal shown-message
                       "Pi: Branched to new session (2 messages)"))))))

(ert-deftest piem-test-resume-transition-waits-after-retarget ()
  "Resume retargets buffers but keeps sends blocked until state/history settle."
  (let* ((old-dir (piem-test--make-temp-directory
                   "piem-test-resume-transition-old-"))
         (new-dir (piem-test--make-temp-directory
                   "piem-test-resume-transition-new-"))
         (session-dir (piem-test--make-temp-directory
                       "piem-test-resume-transition-sessions-"))
         (target-session (expand-file-name "target.jsonl" session-dir))
         (chat-buf (generate-new-buffer "*piem-test-resume-transition-chat*"))
         (input-buf (generate-new-buffer "*piem-test-resume-transition-input*"))
         (proc 'mock-proc)
         (switch-cb nil)
         (state-cb nil)
         (history-cb nil)
         (sent-text nil))
    (unwind-protect
        (progn
          (piem-test--write-session-file
           target-session "target" (directory-file-name new-dir))
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (piem--set-chat-session-identity old-dir)
            (piem--set-input-buffer input-buf)
            (setq piem--process proc
                  piem--status 'idle))
          (with-current-buffer input-buf
            (piem-input-mode)
            (piem--set-chat-buffer chat-buf)
            (setq default-directory old-dir))
          (cl-letf (((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd cb)
                       (pcase (plist-get cmd :type)
                         ("switch_session" (setq switch-cb cb))
                         ("get_state" (setq state-cb cb))
                         ("get_messages" (setq history-cb cb))
                         ("get_commands" nil)
                         (_ (ert-fail (format "Unexpected RPC: %S" cmd))))))
                    ((symbol-function 'piem--prepare-and-send)
                     (lambda (text &optional _queued)
                       (setq sent-text text)))
                    ((symbol-function 'piem--update-session-name-from-file)
                     #'ignore)
                    ((symbol-function 'piem--refresh-header) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (piem--resume-selected-session
             proc chat-buf target-session)
            (funcall switch-cb '(:success t :data (:cancelled :false)))
            (with-current-buffer chat-buf
              (should (equal (piem--chat-session-directory)
                             new-dir))
              (should (piem--session-transition-active-p)))
            (with-current-buffer input-buf
              (should (equal default-directory new-dir))
              (insert "prompt after resume")
              (piem-send)
              (should (equal (buffer-string) "prompt after resume")))
            (should-not sent-text)
            (funcall state-cb
                     `(:success t
                       :data (:model (:name "resumed-model")
                              :thinkingLevel "medium"
                              :isStreaming :json-false
                              :isCompacting :json-false
                              :sessionId "resumed"
                              :sessionFile ,target-session
                              :messageCount 0
                              :pendingMessageCount 0)))
            (with-current-buffer chat-buf
              (should (piem--session-transition-active-p)))
            (funcall history-cb '(:success t :data (:messages [])))
            (with-current-buffer chat-buf
              (should-not (piem--session-transition-active-p)))
            (with-current-buffer input-buf
              (piem-send))
            (should (equal sent-text "prompt after resume"))))
      (piem-test--kill-live-buffers input-buf chat-buf)
      (delete-directory old-dir t)
      (delete-directory new-dir t)
      (delete-directory session-dir t))))

(ert-deftest piem-test-fork-prefill-blocked-until-history-loaded ()
  "Fork pre-fills input immediately, but send stays blocked until history loads."
  (let ((chat-buf (generate-new-buffer "*piem-test-fork-transition-chat*"))
        (input-buf (generate-new-buffer "*piem-test-fork-transition-input*"))
        (proc 'mock-proc)
        (fork-cb nil)
        (state-cb nil)
        (history-cb nil)
        (sent-text nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (piem--set-input-buffer input-buf)
            (setq piem--process proc
                  piem--status 'idle))
          (with-current-buffer input-buf
            (piem-input-mode)
            (piem--set-chat-buffer chat-buf))
          (cl-letf (((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd cb)
                       (pcase (plist-get cmd :type)
                         ("fork" (setq fork-cb cb))
                         ("get_state" (setq state-cb cb))
                         ("get_messages" (setq history-cb cb))
                         (_ (ert-fail (format "Unexpected RPC: %S" cmd))))))
                    ((symbol-function 'piem--prepare-and-send)
                     (lambda (text &optional _queued)
                       (setq sent-text text)))
                    ((symbol-function 'piem--update-session-name-from-file)
                     #'ignore)
                    ((symbol-function 'piem--refresh-header) #'ignore)
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer input-buf
              (piem--execute-fork proc "u1"))
            (funcall fork-cb '(:success t :data (:text "Forked prompt")))
            (with-current-buffer input-buf
              (should (equal (buffer-string) "Forked prompt"))
              (piem-send)
              (should (equal (buffer-string) "Forked prompt")))
            (should-not sent-text)
            (with-current-buffer chat-buf
              (should (piem--session-transition-active-p)))
            (funcall state-cb
                     '(:success t
                       :data (:model (:name "forked-model")
                              :thinkingLevel "medium"
                              :isStreaming :json-false
                              :isCompacting :json-false
                              :sessionId "forked"
                              :sessionFile "/tmp/forked.jsonl"
                              :messageCount 0
                              :pendingMessageCount 0)))
            (with-current-buffer chat-buf
              (should (piem--session-transition-active-p)))
            (funcall history-cb '(:success t :data (:messages [])))
            (with-current-buffer chat-buf
              (should-not (piem--session-transition-active-p)))
            (with-current-buffer input-buf
              (piem-send))
            (should (equal sent-text "Forked prompt"))))
      (piem-test--kill-live-buffers input-buf chat-buf))))

(ert-deftest piem-test-fork-waits-for-local-user-echo ()
  "Fork refuses to switch sessions while a local prompt is awaiting echo."
  (let ((shown-message nil)
        (rpc-called nil))
    (with-temp-buffer
      (piem-chat-mode)
      (setq piem--status 'idle
            piem--process 'mock-proc
            piem--local-user-message "Hello")
      (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                ((symbol-function 'piem--get-chat-buffer)
                 (lambda () (current-buffer)))
                ((symbol-function 'piem--rpc-async)
                 (lambda (&rest _args)
                   (setq rpc-called t)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (piem-fork)))
    (should-not rpc-called)
    (should (equal shown-message
                   "Pi: Wait for pi to echo your prompt before you fork"))))

;;; Fork at Point

(ert-deftest piem-test-fork-at-point-correct-entry-id ()
  "Fork-at-point picks the right entry on second heading."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((piem--status 'idle)
          (piem--process 'mock-proc)
          (forked-entry-id nil)
          (fork-messages (piem-test--make-3turn-fork-messages)))
      (let ((inhibit-read-only t))
        (piem-test--insert-chat-turns))
      (goto-char (point-min))
      (piem-next-message)
      (piem-next-message)
      (should (looking-at "You · 10:05"))
      (cl-letf (((symbol-function 'piem--rpc-async)
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
                ((symbol-function 'piem--refresh-header) #'ignore))
        (piem-fork-at-point))
      (should (equal forked-entry-id "u2")))))

(ert-deftest piem-test-fork-at-point-confirmation-declined ()
  "Fork-at-point does nothing when confirmation is declined."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((piem--status 'idle)
          (piem--process 'mock-proc)
          (fork-called nil)
          (fork-messages (piem-test--make-3turn-fork-messages)))
      (let ((inhibit-read-only t))
        (piem-test--insert-chat-turns))
      (goto-char (point-min))
      (piem-next-message)
      (piem-next-message)
      (cl-letf (((symbol-function 'piem--rpc-async)
                 (lambda (_proc cmd cb)
                   (cond
                    ((equal (plist-get cmd :type) "get_fork_messages")
                     (funcall cb (list :success t :data (list :messages fork-messages))))
                    ((equal (plist-get cmd :type) "fork")
                     (setq fork-called t)))))
                ((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
        (piem-fork-at-point))
      (should-not fork-called))))

(ert-deftest piem-test-fork-at-point-no-user-turn ()
  "Before first You heading, fork-at-point skips RPC."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((piem--status 'idle)
          (piem--process 'mock-proc)
          (rpc-called nil))
      (let ((inhibit-read-only t))
        (piem-test--insert-chat-turns))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'piem--rpc-async)
                 (lambda (&rest _) (setq rpc-called t))))
        (piem-fork-at-point))
      (should-not rpc-called))))

(ert-deftest piem-test-fork-at-point-streaming-guard ()
  "During streaming, fork-at-point skips RPC."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((piem--status 'streaming)
          (piem--process 'mock-proc)
          (rpc-called nil))
      (let ((inhibit-read-only t))
        (piem-test--insert-chat-turns))
      (goto-char (point-min))
      (piem-next-message)
      (cl-letf (((symbol-function 'piem--rpc-async)
                 (lambda (&rest _) (setq rpc-called t))))
        (piem-fork-at-point))
      (should-not rpc-called))))

(ert-deftest piem-test-fork-at-point-followup-drain-guard ()
  "Fork-at-point skips RPC while a local follow-up drain is pending."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((piem--status 'idle)
          (piem--followup-drain-timer 'fake-drain-timer)
          (piem--process 'mock-proc)
          (rpc-called nil))
      (let ((inhibit-read-only t))
        (piem-test--insert-chat-turns))
      (goto-char (point-min))
      (piem-next-message)
      (cl-letf (((symbol-function 'piem--rpc-async)
                 (lambda (&rest _) (setq rpc-called t)))
                ((symbol-function 'message) #'ignore))
        (piem-fork-at-point))
      (should-not rpc-called))))

(ert-deftest piem-test-fork-at-point-rpc-failure-shows-error ()
  "Fork-at-point shows an explicit RPC failure message."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((piem--status 'idle)
          (piem--process 'mock-proc)
          (shown-message nil))
      (let ((inhibit-read-only t))
        (piem-test--insert-chat-turns))
      (goto-char (point-min))
      (piem-next-message)
      (cl-letf (((symbol-function 'piem--rpc-async)
                 (lambda (_proc cmd cb)
                   (when (equal (plist-get cmd :type) "get_fork_messages")
                     (funcall cb '(:success nil :error "Unknown command: get_fork_messages")))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (piem-fork-at-point))
      (should (equal shown-message
                     "Pi: Failed to get fork messages: Unknown command: get_fork_messages")))))

(defconst piem-test--deep-tree-depth 1700
  "Depth used for deep-tree fork and flatten regression tests.")

(ert-deftest piem-test-fork-at-point-deep-tree ()
  "Fork-at-point maps visible ordinals on deep histories."
  (with-temp-buffer
    (piem-chat-mode)
    (let* ((depth piem-test--deep-tree-depth)
           (piem--status 'idle)
           (piem--process 'mock-proc)
           (forked-entry-id nil)
           (fork-messages (piem-test--make-deep-fork-messages depth))
           (expected-entry-id (format "n%d" (- depth 2))))
      (let ((inhibit-read-only t))
        (insert "Pi 1.0.0\n========\nWelcome\n\n"
                "You · 10:00\n===========\nOlder visible turn\n\n"
                "Assistant\n=========\nAnswer\n\n"
                "You · 10:01\n===========\nLatest visible turn\n\n"
                "Assistant\n=========\nAnswer\n"))
      (goto-char (point-min))
      (piem-next-message)
      (should (looking-at "You · 10:00"))
      (cl-letf (((symbol-function 'piem--rpc-async)
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
                ((symbol-function 'piem--refresh-header) #'ignore))
        (piem-fork-at-point))
      (should (equal forked-entry-id expected-entry-id)))))

(ert-deftest piem-test-fork-at-point-compaction ()
  "Fork-at-point uses last-N mapping in compacted sessions."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((piem--status 'idle)
          (piem--process 'mock-proc)
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
      (piem-next-message)
      (should (looking-at "You · 10:05"))
      (cl-letf (((symbol-function 'piem--rpc-async)
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
                ((symbol-function 'piem--refresh-header) #'ignore))
        (piem-fork-at-point))
      (should (equal forked-entry-id "u2")))))

;;; Fork Entry Resolution

(ert-deftest piem-test-resolve-fork-entry-maps-ordinal ()
  "resolve-fork-entry maps ordinal to entry ID and preview."
  (let* ((fork-messages (piem-test--make-3turn-fork-messages))
         (response (list :success t :data (list :messages fork-messages)))
         (result (piem--resolve-fork-entry response 1 3)))
    (should (equal (car result) "u2"))
    (should (equal (cdr result) "Second question"))))

(ert-deftest piem-test-resolve-fork-entry-compaction ()
  "resolve-fork-entry uses last-N mapping in compacted sessions."
  (let* ((fork-messages (piem-test--make-3turn-fork-messages))
         (response (list :success t :data (list :messages fork-messages)))
         (result (piem--resolve-fork-entry response 0 2)))
    (should (equal (car result) "u2"))))

(ert-deftest piem-test-resolve-fork-entry-failure ()
  "resolve-fork-entry returns nil on failure."
  (let ((response '(:success nil :error "Network error")))
    (should-not (piem--resolve-fork-entry response 0 3))))

(defun piem-test--make-deep-linear-tree (depth)
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

(defun piem-test--make-deep-fork-messages (depth)
  "Return DEPTH chronological fork messages."
  (let ((messages (make-vector depth nil)))
    (dotimes (i depth)
      (aset messages i (list :entryId (format "n%d" i)
                             :text (format "node %d" i))))
    messages))

(ert-deftest piem-test-flatten-tree-deep-linear-tree ()
  "flatten-tree handles deep linear trees without eval-depth overflow."
  (let* ((depth piem-test--deep-tree-depth)
         (tree (piem-test--make-deep-linear-tree depth))
         (index (piem--flatten-tree tree)))
    (should (= (hash-table-count index) depth))))

;;; Active Branch Tree Walk

(ert-deftest piem-test-active-branch-linear ()
  "Linear tree: u1 → a1 → u2 → a2 (leaf) returns both user IDs."
  (let* ((data (piem-test--build-tree
                '("u1" nil "message" :role "user" :preview "Hello")
                '("a1" nil "message" :role "assistant" :preview "Hi")
                '("u2" nil "message" :role "user" :preview "More")
                '("a2" nil "message" :role "assistant" :preview "Sure")))
         (index (piem--flatten-tree (plist-get data :tree)))
         (ids (piem--active-branch-user-ids index "a2")))
    (should (equal ids '("u1" "u2")))))

(ert-deftest piem-test-active-branch-branched ()
  "Branched tree: active branch u1 → a1 → u2 → a2, ignores u3 → a3."
  (let* ((data (piem-test--build-tree
                '("u1" nil "message" :role "user" :preview "Hello")
                '("a1" nil "message" :role "assistant" :preview "Hi")
                '("u2" nil "message" :role "user" :preview "Path A")
                '("a2" nil "message" :role "assistant" :preview "Sure A")
                '("u3" "a1" "message" :role "user" :preview "Path B")
                '("a3" nil "message" :role "assistant" :preview "Sure B")))
         (index (piem--flatten-tree (plist-get data :tree)))
         (ids (piem--active-branch-user-ids index "a2")))
    (should (equal ids '("u1" "u2")))))

(ert-deftest piem-test-active-branch-with-compaction ()
  "Tree with compaction node: u1 → a1 → compaction → u2 → a2."
  (let* ((data (piem-test--build-tree
                '("u1" nil "message" :role "user" :preview "First")
                '("a1" nil "message" :role "assistant" :preview "Response")
                '("c1" nil "compaction" :tokensBefore 5000)
                '("u2" nil "message" :role "user" :preview "After compaction")
                '("a2" nil "message" :role "assistant" :preview "Still here")))
         (index (piem--flatten-tree (plist-get data :tree)))
         (ids (piem--active-branch-user-ids index "a2")))
    (should (equal ids '("u1" "u2")))))

(ert-deftest piem-test-active-branch-with-metadata ()
  "Tree with model_change and thinking nodes: only user IDs returned."
  (let* ((data (piem-test--build-tree
                '("u1" nil "message" :role "user" :preview "Hello")
                '("a1" nil "message" :role "assistant" :preview "Hi")
                '("m1" nil "model_change" :provider "anthropic" :modelId "claude-4")
                '("t1" nil "thinking_level_change" :thinkingLevel "high")
                '("u2" nil "message" :role "user" :preview "More")
                '("a2" nil "message" :role "assistant" :preview "Sure")))
         (index (piem--flatten-tree (plist-get data :tree)))
         (ids (piem--active-branch-user-ids index "a2")))
    (should (equal ids '("u1" "u2")))))

(ert-deftest piem-test-active-branch-empty-tree ()
  "Empty tree returns empty list."
  (let* ((index (piem--flatten-tree []))
         (ids (piem--active-branch-user-ids index nil)))
    (should (equal ids nil))))

(ert-deftest piem-test-active-branch-nil-leaf ()
  "Nil leafId returns empty list."
  (let* ((data (piem-test--build-tree
                '("u1" nil "message" :role "user" :preview "Hello")))
         (index (piem--flatten-tree (plist-get data :tree)))
         (ids (piem--active-branch-user-ids index nil)))
    (should (equal ids nil))))

;;;; State Reading from Input Buffer

(ert-deftest piem-test-menu-model-description-from-input-buffer ()
  "Menu descriptions and display rows read state from the linked chat buffer."
  (let ((piem-thinking-display 'visible))
    (piem-test-with-mock-session "/tmp/piem-test-state/"
      (let ((chat-buf (get-buffer (piem-test--chat-buffer-name
                                   "/tmp/piem-test-state/")))
            (input-buf (get-buffer (piem-test--input-buffer-name
                                    "/tmp/piem-test-state/"))))
        ;; Set state in chat buffer (where it lives)
        (with-current-buffer chat-buf
          (setq piem--state
                '(:model (:name "Claude Opus 4.6" :id "claude-opus-4-6"
                          :provider "anthropic")
                  :thinking-level "high")
                piem--thinking-display 'hidden))
        ;; Call from input buffer (where cursor normally is)
        (with-current-buffer input-buf
          (should (string-match-p "Opus 4.6"
                                  (piem--menu-model-description)))
          (should (string-match-p "high"
                                  (piem--menu-thinking-description)))
          (should (equal 'hidden
                         (piem--menu-current-thinking-display-mode)))
          (should (equal 'visible
                         (piem--menu-default-thinking-display-mode)))
          (should (equal "Model: Opus 4.6 • Thinking: high"
                         (piem--menu-description))))))))

(ert-deftest piem-test-toggle-default-thinking-display-affects-new-chats-only ()
  "Toggling the new-chat default leaves existing chat buffers alone."
  (let ((piem-thinking-display 'hidden)
        shown-message)
    (with-temp-buffer
      (piem-chat-mode)
      (should (eq piem--thinking-display 'hidden))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (piem-toggle-default-thinking-display))
      (should (eq piem--thinking-display 'hidden))
      (should (eq piem-thinking-display 'visible)))
    (with-temp-buffer
      (piem-chat-mode)
      (should (eq piem--thinking-display 'visible)))
    (should (equal shown-message
                   "Pi: New chat buffers will show completed thinking by default"))))

(defun piem-test--menu-collapsed-thinking-stub (text)
  "Return the hidden stub shown for completed thinking TEXT."
  (piem--thinking-hidden-stub
   (piem--thinking-normalize-text text)))

(defun piem-test--menu-history-with-two-thinking-blocks ()
  "Return history with two completed thinking blocks and plain assistant text."
  [(:role "assistant"
    :content [(:type "text" :text "Answer first.")
              (:type "thinking" :thinking "Need to double-check.")
              (:type "text" :text "Final answer.")]
    :timestamp 1704067200000)
   (:role "assistant"
    :content [(:type "text" :text "Another answer.")
              (:type "thinking" :thinking "Second thought.")
              (:type "text" :text "Done.")]
    :timestamp 1704067201000)])

(defun piem-test--menu-history-with-thinking-and-tool ()
  "Return history with completed thinking and a long tool block."
  [(:role "assistant"
    :content [(:type "text" :text "Answer first.")
              (:type "thinking"
               :thinking "Need to double-check.\n\nSecond paragraph.")
              (:type "text" :text "Final answer.")
              (:type "toolCall" :id "call_1"
               :name "read"
               :arguments (:path "example.txt"))]
    :timestamp 1704067200000)
   (:role "toolResult" :toolCallId "call_1"
    :toolName "read"
    :content [(:type "text"
               :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10\nL11\nL12")]
    :isError :json-false
    :timestamp 1704067201000)])

(ert-deftest piem-test-toggle-thinking-display-from-input-buffer-updates-linked-chat ()
  "Toggling from the input buffer updates the linked chat buffer."
  (let ((piem-thinking-display 'visible)
        shown-message)
    (piem-test-with-mock-session "/tmp/piem-test-toggle-linked-chat/"
      (let ((chat-buf (get-buffer (piem-test--chat-buffer-name
                                   "/tmp/piem-test-toggle-linked-chat/")))
            (input-buf (get-buffer (piem-test--input-buffer-name
                                    "/tmp/piem-test-toggle-linked-chat/"))))
        (with-current-buffer chat-buf
          (setq piem--thinking-display 'hidden)
          (piem--display-session-history
           [(:role "assistant"
             :content [(:type "text" :text "Answer first.")
                       (:type "thinking" :thinking "Need to double-check.")]
             :timestamp 1704067200000)]
           (current-buffer)))
        (with-current-buffer input-buf
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq shown-message (apply #'format fmt args)))))
            (piem-toggle-thinking-display)))
        (with-current-buffer chat-buf
          (let ((text (buffer-string)))
            (should (eq piem--thinking-display 'visible))
            (should (string-match-p "^> Need to double-check\\.$" text))
            (should-not (string-match-p
                         (regexp-quote
                          (piem-test--menu-collapsed-thinking-stub
                           "Need to double-check."))
                         text))))))
    (should (equal shown-message
                   "Pi: This chat now shows completed thinking"))))

(ert-deftest piem-test-toggle-thinking-display-overrides-per-block-states ()
  "Whole-buffer toggles apply one display mode to every completed thinking block."
  (let ((piem-thinking-display 'hidden)
        shown-message)
    (with-temp-buffer
      (piem-chat-mode)
      (piem--display-session-history
       (piem-test--menu-history-with-two-thinking-blocks)
       (current-buffer))
      (goto-char (point-min))
      (search-forward (piem-test--menu-collapsed-thinking-stub
                       "Need to double-check."))
      (beginning-of-line)
      (piem-toggle-tool-section)
      (let ((text (buffer-string)))
        (should (string-match-p "^> Need to double-check\\.$" text))
        (should (string-match-p
                 (regexp-quote
                  (piem-test--menu-collapsed-thinking-stub
                   "Second thought."))
                 text)))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (piem-toggle-thinking-display))
      (let ((text (buffer-string)))
        (should (eq piem--thinking-display 'visible))
        (should (string-match-p "^> Need to double-check\\.$" text))
        (should (string-match-p "^> Second thought\\.$" text))
        (should-not (string-match-p
                     (regexp-quote
                      (piem-test--menu-collapsed-thinking-stub
                       "Need to double-check."))
                     text))
        (should-not (string-match-p
                     (regexp-quote
                      (piem-test--menu-collapsed-thinking-stub
                       "Second thought."))
                     text)))
      (piem-toggle-thinking-display)
      (let ((text (buffer-string)))
        (should (eq piem--thinking-display 'hidden))
        (should (string-match-p
                 (regexp-quote
                  (piem-test--menu-collapsed-thinking-stub
                   "Need to double-check."))
                 text))
        (should (string-match-p
                 (regexp-quote
                  (piem-test--menu-collapsed-thinking-stub
                   "Second thought."))
                 text))
        (should-not (string-match-p "^> Need to double-check\\.$" text))
        (should-not (string-match-p "^> Second thought\\.$" text))))
    (should (equal shown-message
                   "Pi: This chat now shows completed thinking"))))

(ert-deftest piem-test-toggle-thinking-display-without-canonical-messages-leaves-buffer-alone ()
  "Without canonical messages, toggling updates only future completed-thinking rendering."
  (let ((piem-thinking-display 'visible)
        shown-message)
    (with-temp-buffer
      (piem-chat-mode)
      (setq piem--status 'idle)
      (let ((inhibit-read-only t))
        (insert "Keep existing buffer text\n"))
      (let ((before (buffer-string)))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq shown-message (apply #'format fmt args)))))
          (piem-toggle-thinking-display))
        (should (eq piem--thinking-display 'hidden))
        (should (equal before (buffer-string)))))
    (should (equal shown-message
                   "Pi: This chat now hides completed thinking"))))

(ert-deftest piem-test-toggle-thinking-display-keeps-local-user-message-visible ()
  "Thinking-display toggles keep a pending local user echo visible."
  (let ((piem-thinking-display 'visible)
        shown-message)
    (with-temp-buffer
      (piem-chat-mode)
      (setq piem--status 'idle
            piem--local-user-message "Hello"
            piem--canonical-messages
            [(:role "assistant"
              :content [(:type "thinking" :thinking "Need to double-check.")]
              :timestamp 1704067200000)])
      (piem--display-user-message "Hello")
      (let ((before (buffer-string)))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq shown-message (apply #'format fmt args)))))
          (piem-toggle-thinking-display))
        (should (eq piem--thinking-display 'hidden))
        (should (equal before (buffer-string)))
        (piem--handle-display-event
         '(:type "message_start"
           :message (:role "user"
                     :content [(:type "text" :text "Hello")]
                     :timestamp 1704067201000)))
        (should (equal before (buffer-string)))
        (should-not piem--local-user-message)))
    (should (equal shown-message
                   "Pi: This chat now hides completed thinking"))))

(ert-deftest piem-test-toggle-thinking-display-keeps-live-custom-message-visible ()
  "Whole-buffer thinking toggles must not delete live custom messages."
  (let ((piem-thinking-display 'visible))
    (with-temp-buffer
      (piem-chat-mode)
      (piem--display-session-history
       [(:role "assistant"
         :content [(:type "text" :text "Answer first.")
                   (:type "thinking" :thinking "Need to double-check.")]
         :timestamp 1704067200000)]
       (current-buffer))
      (piem--handle-display-event
       '(:type "message_start"
         :message (:role "custom" :display t :content "Extension note: keep me")))
      (piem--handle-display-event
       '(:type "message_end"
         :message (:role "custom" :display t :content "Extension note: keep me")))
      (piem-toggle-thinking-display)
      (let ((text (buffer-string)))
        (should (eq piem--thinking-display 'hidden))
        (should (string-match-p "Answer first\\." text))
        (should (string-match-p "Extension note: keep me" text))
        (should (string-match-p
                 (regexp-quote
                  (piem-test--menu-collapsed-thinking-stub
                   "Need to double-check."))
                 text))))))

(ert-deftest piem-test-toggle-thinking-display-preserves-expanded-tool-block ()
  "Whole-buffer thinking toggles must not reset expanded tool output."
  (let ((piem-thinking-display 'hidden))
    (with-temp-buffer
      (piem-chat-mode)
      (piem--display-session-history
       (piem-test--menu-history-with-thinking-and-tool)
       (current-buffer))
      (goto-char (point-min))
      (let ((button (next-button (point-min))))
        (should button)
        (piem--toggle-tool-output button))
      (should (string-match-p "L12" (buffer-string)))
      (should (string-match-p "\\[-\\]" (buffer-string)))
      (piem-toggle-thinking-display)
      (let ((text (buffer-string)))
        (should (string-match-p "L12" text))
        (should (string-match-p "\\[-\\]" text))
        (should-not (string-match-p "\\.\\.\\. ([0-9]+ more lines)" text))))))

(ert-deftest piem-test-menu-model-description-uses-short-name ()
  "Menu model description shows shortened name, not full \"Claude Opus 4.6\"."
  (piem-test-with-mock-session "/tmp/piem-test-short/"
    (let ((chat-buf (get-buffer (piem-test--chat-buffer-name
                                 "/tmp/piem-test-short/"))))
      (with-current-buffer chat-buf
        (setq piem--state
              '(:model (:name "Claude Opus 4.6")))
        (should (string-match-p "Opus 4.6"
                                (piem--menu-model-description)))
        (should-not (string-match-p "Claude"
                                    (piem--menu-model-description)))))))

;;;; Model Selector Completion Styles

(ert-deftest piem-test-select-model-case-insensitive ()
  "Model selector matches case-insensitively: \"opus\" finds \"Opus 4.6\"."
  (let ((models '((:name "Claude Opus 4.6" :id "opus-4-6" :provider "anthropic")
                  (:name "Claude Sonnet 4.5" :id "sonnet-4-5" :provider "anthropic")))
        captured-case captured-styles)
    (let ((buf (generate-new-buffer "*piem-chat:flex-test*")))
      (unwind-protect
          (cl-letf (((symbol-function 'piem--rpc-sync)
                     (lambda (&rest _) (list :data (list :models models))))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc _cmd _cb)))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (setq captured-case completion-ignore-case
                             captured-styles completion-styles)
                       "Opus 4.6")))
            (with-current-buffer buf
              (piem-chat-mode)
              (setq piem--process :fake-proc
                    piem--state '(:model (:name "Claude Sonnet 4.5")))
              (piem-select-model)))
        (with-current-buffer buf (setq piem--process nil))
        (kill-buffer buf)))
    (should captured-case)
    (should (memq 'flex captured-styles))))

(ert-deftest piem-test-select-model-flex-matches-substring ()
  "Flex completion: \"code\" matches \"GPT-5.1 Codex Max\"."
  (let* ((names '("Opus 4.6" "GPT-5.1 Codex Max"))
         (completion-ignore-case t)
         (completion-styles '(basic flex))
         (result (completion-all-completions "code" names nil (length "code"))))
    (when (consp result) (setcdr (last result) nil))
    (should (= 1 (length result)))
    (should (string-match-p "Codex" (car result)))))

(ert-deftest piem-test-select-model-flex-matches-noncontiguous ()
  "Flex completion: \"o46\" matches \"Opus 4.6\" (non-contiguous)."
  (let* ((names '("Opus 4.6" "Sonnet 4.5" "GPT-5.1 Codex Max"))
         (completion-ignore-case t)
         (completion-styles '(basic flex))
         (result (completion-all-completions "o46" names nil (length "o46"))))
    (when (consp result) (setcdr (last result) nil))
    (should (= 1 (length result)))
    (should (string-match-p "Opus 4.6" (car result)))))

(ert-deftest piem-test-select-model-unique-match-auto-selects ()
  "When initial-input uniquely matches one model, skip completing-read."
  (let ((models '((:name "Claude Opus 4.6" :id "opus-4-6" :provider "anthropic")
                  (:name "Claude Sonnet 4.5" :id "sonnet-4-5" :provider "anthropic")))
        completing-read-called set-model-id)
    (let ((buf (generate-new-buffer "*piem-chat:auto-select*")))
      (unwind-protect
          (cl-letf (((symbol-function 'piem--rpc-sync)
                     (lambda (&rest _) (list :data (list :models models))))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd _cb)
                       (setq set-model-id (plist-get cmd :modelId))))
                    ((symbol-function 'completing-read)
                     (lambda (&rest _)
                       (setq completing-read-called t)
                       "Opus 4.6")))
            (with-current-buffer buf
              (piem-chat-mode)
              (setq piem--process :fake-proc
                    piem--state '(:model (:name "Claude Sonnet 4.5")))
              (piem-select-model "op46")))
        (with-current-buffer buf (setq piem--process nil))
        (kill-buffer buf)))
    (should-not completing-read-called)
    (should (equal set-model-id "opus-4-6"))))

(ert-deftest piem-test-select-model-no-match-shows-message ()
  "When initial-input matches nothing, show message and don't set model."
  (let ((models '((:name "Claude Opus 4.6" :id "opus-4-6" :provider "anthropic")))
        set-model-called last-message)
    (let ((buf (generate-new-buffer "*piem-chat:no-match*")))
      (unwind-protect
          (cl-letf (((symbol-function 'piem--rpc-sync)
                     (lambda (&rest _) (list :data (list :models models))))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (&rest _) (setq set-model-called t)))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq last-message (apply #'format fmt args)))))
            (with-current-buffer buf
              (piem-chat-mode)
              (setq piem--process :fake-proc
                    piem--state '(:model (:name "Claude Opus 4.6")))
              (piem-select-model "zzzzz")))
        (with-current-buffer buf (setq piem--process nil))
        (kill-buffer buf)))
    (should-not set-model-called)
    (should (string-match-p "No model matching" last-message))))

(ert-deftest piem-test-select-model-multiple-matches-opens-selector ()
  "When initial-input matches multiple models, fall through to completing-read."
  (let ((models '((:name "Claude Opus 4" :id "opus-4" :provider "anthropic")
                  (:name "Claude Opus 4.5" :id "opus-4-5" :provider "anthropic")
                  (:name "Claude Sonnet 4.5" :id "sonnet-4-5" :provider "anthropic")))
        completing-read-called captured-initial)
    (let ((buf (generate-new-buffer "*piem-chat:multi-match*")))
      (unwind-protect
          (cl-letf (((symbol-function 'piem--rpc-sync)
                     (lambda (&rest _) (list :data (list :models models))))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc _cmd _cb)))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt _coll _pred _req initial &rest _)
                       (setq completing-read-called t
                             captured-initial initial)
                       "Opus 4")))
            (with-current-buffer buf
              (piem-chat-mode)
              (setq piem--process :fake-proc
                    piem--state '(:model (:name "Claude Sonnet 4.5")))
              (piem-select-model "opus")))
        (with-current-buffer buf (setq piem--process nil))
        (kill-buffer buf)))
    (should completing-read-called)
    (should (equal captured-initial "opus"))))

(ert-deftest piem-test-filter-thinking-levels-removes-model-aliases ()
  "Thinking selector only offers distinct provider reasoning levels."
  (should
   (equal
    (piem--filter-thinking-level-aliases
     '("off" "minimal" "low" "medium" "high" "xhigh" "max")
     '(:thinkingLevelMap
       (:minimal "low" :low "low" :medium "medium"
        :high "high" :xhigh "max" :max "max")))
    '("off" "low" "medium" "high" "max"))))

(ert-deftest piem-test-filter-thinking-levels-removes-unsupported-levels ()
  "Explicitly unsupported model thinking levels are omitted."
  (should
   (equal
    (piem--filter-thinking-level-aliases
     '("off" "minimal" "low" "medium" "high" "xhigh" "max")
     '(:thinkingLevelMap
       (:minimal :null :low :null :medium :null :high "high"
        :xhigh "xhigh" :max :null)))
    '("off" "high" "xhigh"))))

(ert-deftest piem-test-get-available-thinking-levels-errors-on-rpc-failure ()
  "Thinking-level selection does not offer unsupported fallback values."
  (cl-letf (((symbol-function 'piem--rpc-sync)
             (lambda (&rest _)
               '(:success :false :error "Unknown command"))))
    (should-error (piem--get-available-thinking-levels :fake-proc)
                  :type 'user-error)))

(ert-deftest piem-test-select-thinking-refreshes-state-from-server ()
  "Thinking selector refreshes state so server clamping is visible in the UI."
  (let (captured-prompt captured-collection rpc-commands last-message)
    (with-temp-buffer
      (piem-chat-mode)
      (setq piem--process :fake-proc
            piem--state '(:thinking-level "low"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt collection &rest _)
                   (setq captured-prompt prompt
                         captured-collection collection)
                   "high"))
                ((symbol-function 'piem--rpc-sync)
                 (lambda (_proc cmd _timeout)
                   (when (equal (plist-get cmd :type) "get_available_thinking_levels")
                     '(:success t
                       :data (:levels ["off" "minimal" "low" "medium" "high" "xhigh"])))))
                ((symbol-function 'piem--rpc-async)
                 (lambda (_proc cmd callback)
                   (push cmd rpc-commands)
                   (pcase (plist-get cmd :type)
                     ("set_thinking_level"
                      (funcall callback '(:success t :command "set_thinking_level")))
                     ("get_state"
                      (funcall callback
                               '(:success t
                                 :data (:thinkingLevel "medium"
                                        :isStreaming nil
                                        :isCompacting nil)))))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq last-message (apply #'format fmt args)))))
        (piem-select-thinking)
        (should (equal (plist-get piem--state :thinking-level) "medium"))))
    (should (equal captured-prompt "Thinking level (current: low): "))
    (should (equal captured-collection
                   '("off" "minimal" "low" "medium" "high" "xhigh")))
    (let ((commands (nreverse rpc-commands)))
      (should (equal (mapcar (lambda (cmd) (plist-get cmd :type)) commands)
                     '("set_thinking_level" "get_state")))
      (should (equal (car commands)
                     '(:type "set_thinking_level" :level "high")))
      (should (equal (cadr commands) '(:type "get_state"))))
    (should (equal last-message "Pi: Thinking level: medium"))))

(ert-deftest piem-test-select-thinking-noop-when-unchanged ()
  "Thinking selector does not send RPC when the user picks the current level."
  (let (rpc-called)
    (with-temp-buffer
      (piem-chat-mode)
      (setq piem--process :fake-proc
            piem--state '(:thinking-level "medium"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "medium"))
                ((symbol-function 'piem--rpc-sync)
                 (lambda (_proc cmd _timeout)
                   (when (equal (plist-get cmd :type) "get_available_thinking_levels")
                     '(:success t
                       :data (:levels ("off" "minimal" "low" "medium" "high" "xhigh"))))))
                ((symbol-function 'piem--rpc-async)
                 (lambda (&rest _)
                   (setq rpc-called t))))
        (piem-select-thinking)))
    (should-not rpc-called)))

(ert-deftest piem-test-select-thinking-errors-without-process ()
  "Thinking selector should fail loudly when no pi process is running."
  (with-temp-buffer
    (piem-chat-mode)
    (should-error (piem-select-thinking) :type 'user-error)))

(ert-deftest piem-test-select-thinking-shows-rpc-error ()
  "Thinking selector reports set_thinking_level RPC failures."
  (let (rpc-commands shown-message)
    (with-temp-buffer
      (piem-chat-mode)
      (setq piem--process :fake-proc
            piem--state '(:thinking-level "low"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "high"))
                ((symbol-function 'piem--rpc-sync)
                 (lambda (_proc cmd _timeout)
                   (when (equal (plist-get cmd :type) "get_available_thinking_levels")
                     '(:success t
                       :data (:levels ("off" "minimal" "low" "medium" "high" "xhigh"))))))
                ((symbol-function 'piem--rpc-async)
                 (lambda (_proc cmd callback)
                   (push cmd rpc-commands)
                   (funcall callback '(:success :false :error "unsupported"))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (piem-select-thinking)
        (should (equal (plist-get piem--state :thinking-level) "low"))))
    (should (equal rpc-commands
                   '((:type "set_thinking_level" :level "high"))))
    (should (equal shown-message
                   "Pi: Failed to set thinking level: unsupported"))))

(ert-deftest piem-test-select-thinking-warns-when-state-refresh-fails ()
  "Thinking selector warns instead of guessing when state refresh fails."
  (let (shown-message)
    (with-temp-buffer
      (piem-chat-mode)
      (setq piem--process :fake-proc
            piem--state '(:thinking-level "low"))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "high"))
                ((symbol-function 'piem--rpc-sync)
                 (lambda (_proc cmd _timeout)
                   (when (equal (plist-get cmd :type) "get_available_thinking_levels")
                     '(:success t
                       :data (:levels ("off" "minimal" "low" "medium" "high" "xhigh"))))))
                ((symbol-function 'piem--rpc-async)
                 (lambda (_proc cmd callback)
                   (pcase (plist-get cmd :type)
                     ("set_thinking_level"
                      (funcall callback '(:success t :command "set_thinking_level")))
                     ("get_state"
                      (funcall callback '(:success nil :error "state unavailable"))))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (piem-select-thinking)
        (should (equal (plist-get piem--state :thinking-level) "low"))))
    (should (equal shown-message
                   "Pi: Thinking level updated, but failed to refresh state: state unavailable"))))

(ert-deftest piem-test-thinking-selector-uses-t-key-leaving-T-for-templates ()
  "Main menu keeps `t', `h', and `H' free without taking Templates `T'."
  (let ((piem--commands
         '((:name "review" :description "Code review" :source "prompt"))))
    (unwind-protect
        (progn
          (piem--rebuild-commands-menu)
          (transient-setup 'piem-menu)
          (let ((thinking-suffix
                 (cl-find-if (lambda (obj)
                               (equal (oref obj key) "t"))
                             transient--suffixes))
                (chat-display-suffix
                 (cl-find-if (lambda (obj)
                               (equal (oref obj key) "h"))
                             transient--suffixes))
                (default-display-suffix
                 (cl-find-if (lambda (obj)
                               (equal (oref obj key) "H"))
                             transient--suffixes))
                (templates-suffix
                 (cl-find-if (lambda (obj)
                               (equal (oref obj key) "T"))
                             transient--suffixes)))
            (should thinking-suffix)
            (should (eq (oref thinking-suffix command)
                        'piem-select-thinking))
            (should chat-display-suffix)
            (should (equal "This chat"
                           (transient-format-description chat-display-suffix)))
            (should default-display-suffix)
            (should (equal "New chat default"
                           (transient-format-description default-display-suffix)))
            (should templates-suffix)))
      (ignore-errors (transient-remove-suffix 'piem-menu '(3))))))

;;; sourceInfo normalization

(ert-deftest piem-test-normalize-command-extracts-source-info ()
  "Normalizer lifts sourceInfo.scope and sourceInfo.path to top level."
  (let* ((raw (list :name "fix" :source "prompt"
                    :sourceInfo '(:scope "user" :path "/home/me/.pi/fix.md")))
         (norm (piem--normalize-command raw)))
    (should (equal (plist-get norm :location) "user"))
    (should (equal (plist-get norm :path) "/home/me/.pi/fix.md"))
    (should (equal (plist-get norm :name) "fix"))
    (should-not (plist-get norm :sourceInfo))))

(ert-deftest piem-test-normalize-command-anchors-remote-source-path ()
  "Command source paths from Pi are normalized to Emacs/TRAMP paths."
  (let* ((anchor "/ssh:pi-host:/home/pi/project/")
         (raw (list :name "fix" :source "prompt"
                    :sourceInfo '(:scope "project" :path "prompts/fix.md")))
         (norm (piem--normalize-command raw anchor)))
    (should (equal (plist-get norm :path)
                   "/ssh:pi-host:/home/pi/project/prompts/fix.md"))))

(ert-deftest piem-test-normalize-command-maps-temporary-scope-to-path ()
  "Pi's temporary command scope belongs in the menu's path bucket."
  (let* ((raw '(:name "one-off" :source "prompt"
                :sourceInfo (:scope "temporary" :path "/tmp/one-off.md")))
         (norm (piem--normalize-command raw "/tmp/project/")))
    (should (equal (plist-get norm :location) "path"))))

(ert-deftest piem-test-normalize-command-ignores-unsafe-source-path ()
  "Command normalization ignores unsafe passive source path metadata."
  (let* ((bad (concat "/tmp/a" (string ?\0) "b.md"))
         (raw (list :name "fix" :source "prompt"
                    :sourceInfo (list :scope "project" :path bad)))
         (norm (piem--normalize-command raw)))
    (should (equal (plist-get norm :location) "project"))
    (should-not (plist-get norm :path))
    (should-not (plist-get norm :sourceInfo))))

(ert-deftest piem-test-normalize-command-ignores-mismatched-remote-source-path ()
  "Command normalization ignores source paths from another TRAMP remote."
  (let* ((anchor "/ssh:pi-host:/home/pi/project/")
         (raw (list :name "fix" :source "prompt"
                    :sourceInfo '(:scope "project"
                                  :path "/ssh:other:/tmp/fix.md")))
         (norm (piem--normalize-command raw anchor)))
    (should (equal (plist-get norm :location) "project"))
    (should-not (plist-get norm :path))
    (should-not (plist-get norm :sourceInfo))))

(ert-deftest piem-test-edit-command-source-opens-remote-emacs-path ()
  "Editing command sources opens the Emacs/TRAMP path for remote sessions."
  (let ((chat-buf (generate-new-buffer "*test-edit-command-source*"))
        (opened-path nil))
    (unwind-protect
        (with-current-buffer chat-buf
          (piem-chat-mode)
          (piem--set-chat-session-identity
           "/ssh:pi-host:/home/pi/project/")
          (cl-letf (((symbol-function 'find-file-other-window)
                     (lambda (path)
                       (setq opened-path path))))
            (piem--edit-command-source "/home/pi/.pi/prompts/fix.md"))
          (should (equal opened-path
                         "/ssh:pi-host:/home/pi/.pi/prompts/fix.md")))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-export-html-uses-process-local-remote-paths ()
  "Remote export sends process-local outputPath and reports an Emacs path."
  (let ((chat-buf (generate-new-buffer "*test-export-html-remote*"))
        (proc (start-process "test-export-html-remote" nil "cat"))
        (sent-command nil)
        (message-text nil))
    (set-process-query-on-exit-flag proc nil)
    (unwind-protect
        (with-current-buffer chat-buf
          (piem-chat-mode)
          (piem--set-chat-session-identity
           "/ssh:pi-host:/home/pi/project/")
          (setq piem--process proc)
          (cl-letf (((symbol-function 'piem--rpc-async)
                     (lambda (_proc command callback)
                       (setq sent-command command)
                       (funcall callback
                                '(:success t
                                  :data (:path "/home/pi/project/reports/out.html")))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
            (piem-export-html "reports/out.html"))
          (should (equal (plist-get sent-command :outputPath)
                         "/home/pi/project/reports/out.html"))
          (should (equal message-text
                         "Pi: Exported to /ssh:pi-host:/home/pi/project/reports/out.html")))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-export-html-ignores-unsafe-response-path ()
  "Malformed backend export paths do not escape the async callback."
  (let ((chat-buf (generate-new-buffer "*test-export-html-unsafe*"))
        (proc (start-process "test-export-html-unsafe" nil "cat"))
        (message-text nil)
        (bad-path (concat "/tmp/out" (string ?\0) ".html")))
    (set-process-query-on-exit-flag proc nil)
    (unwind-protect
        (with-current-buffer chat-buf
          (piem-chat-mode)
          (piem--set-chat-session-identity "/tmp/project/")
          (setq piem--process proc)
          (cl-letf (((symbol-function 'piem--rpc-async)
                     (lambda (_proc _command callback)
                       (funcall callback
                                (list :success t
                                      :data (list :path bad-path)))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq message-text (apply #'format fmt args)))))
            (piem-export-html))
          (should (equal message-text
                         "Pi: Exported, but Pi did not return a usable path")))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-export-html-preserves-remote-home-output-path ()
  "Remote export sends home-relative outputPath without local expansion."
  (let ((chat-buf (generate-new-buffer "*test-export-html-remote-home*"))
        (proc (start-process "test-export-html-remote-home" nil "cat"))
        (sent-command nil))
    (set-process-query-on-exit-flag proc nil)
    (unwind-protect
        (with-current-buffer chat-buf
          (piem-chat-mode)
          (piem--set-chat-session-identity
           "/ssh:pi-host:/home/pi/project/")
          (setq piem--process proc)
          (let ((file-name-handler-alist nil))
            (cl-letf (((symbol-function 'piem--rpc-async)
                       (lambda (_proc command _callback)
                         (setq sent-command command))))
              (piem-export-html "~/out.html")))
          (should (equal (plist-get sent-command :outputPath)
                         "~/out.html")))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-normalize-command-without-source-info ()
  "Normalizer leaves commands unchanged when sourceInfo is absent."
  (let* ((raw '(:name "ext" :source "extension"))
         (norm (piem--normalize-command raw)))
    (should (equal (plist-get norm :name) "ext"))
    (should-not (plist-get norm :location))
    (should-not (plist-get norm :path))))

(ert-deftest piem-test-normalize-command-partial-source-info ()
  "Normalizer handles sourceInfo with scope but no path."
  (let* ((raw '(:name "s" :source "skill"
                :sourceInfo (:scope "project")))
         (norm (piem--normalize-command raw)))
    (should (equal (plist-get norm :location) "project"))
    (should-not (plist-get norm :path))))

(provide 'piem-menu-test)
;;; piem-menu-test.el ends here
