;;; pi-coding-agent-test.el --- Tests for pi-coding-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for pi-coding-agent - the main UI layer.

;;; Code:

(require 'ert)
(require 'pi-coding-agent)
(require 'pi-coding-agent-test-common)

;;; Test Utilities

(defmacro pi-coding-agent-test-with-mock-session (dir &rest body)
  "Execute BODY with a mocked pi session in DIR, cleaning up after.
DIR should be a unique directory path like \"/tmp/pi-coding-agent-test-foo/\".
Mocks `project-current', `pi-coding-agent--start-process', and `pi-coding-agent--display-buffers'.
Automatically cleans up chat and input buffers."
  (declare (indent 1) (debug t))
  `(let ((default-directory ,dir))
     (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
               ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
               ((symbol-function 'pi-coding-agent--display-buffers) #'ignore))
       (unwind-protect
           (progn (pi-coding-agent) ,@body)
         (ignore-errors (kill-buffer (pi-coding-agent--chat-buffer-name ,dir nil)))
         (ignore-errors (kill-buffer (pi-coding-agent--input-buffer-name ,dir nil)))))))

(defun pi-coding-agent-test--slash-completions ()
  "Return command completion candidates for current input buffer."
  (erase-buffer)
  (insert "/")
  (let ((completion (pi-coding-agent--command-capf)))
    (nth 2 completion)))

;;; Buffer Naming

(ert-deftest pi-coding-agent-test-buffer-name-chat ()
  "Buffer name for chat includes abbreviated directory."
  (let ((name (pi-coding-agent--buffer-name :chat "/home/user/project/")))
    (should (string-match-p "\\*pi-coding-agent-chat:" name))
    (should (string-match-p "project" name))))

(ert-deftest pi-coding-agent-test-buffer-name-input ()
  "Buffer name for input includes abbreviated directory."
  (let ((name (pi-coding-agent--buffer-name :input "/home/user/project/")))
    (should (string-match-p "\\*pi-coding-agent-input:" name))
    (should (string-match-p "project" name))))

(ert-deftest pi-coding-agent-test-buffer-name-abbreviates-home ()
  "Buffer name abbreviates home directory to ~."
  (let ((name (pi-coding-agent--buffer-name :chat (expand-file-name "~/myproject/"))))
    (should (string-match-p "~" name))))

(ert-deftest pi-coding-agent-test-path-to-language-known-extension ()
  "path-to-language returns correct language for known extensions."
  (should (equal "python" (pi-coding-agent--path-to-language "/tmp/foo.py")))
  (should (equal "javascript" (pi-coding-agent--path-to-language "/tmp/bar.js")))
  (should (equal "emacs-lisp" (pi-coding-agent--path-to-language "/tmp/baz.el"))))

(ert-deftest pi-coding-agent-test-path-to-language-unknown-extension ()
  "path-to-language returns 'text' for unknown extensions.
This ensures all files get code fences for consistent display."
  (should (equal "text" (pi-coding-agent--path-to-language "/tmp/foo.txt")))
  (should (equal "text" (pi-coding-agent--path-to-language "/tmp/bar.xyz")))
  (should (equal "text" (pi-coding-agent--path-to-language "/tmp/noext"))))

;;; Buffer Creation

(ert-deftest pi-coding-agent-test-get-or-create-buffer-creates-new ()
  "get-or-create-buffer creates a new buffer if none exists."
  (let* ((dir "/tmp/pi-coding-agent-test-unique-12345/")
         (buf (pi-coding-agent--get-or-create-buffer :chat dir)))
    (unwind-protect
        (progn
          (should (bufferp buf))
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest pi-coding-agent-test-get-or-create-buffer-returns-existing ()
  "get-or-create-buffer returns existing buffer."
  (let* ((dir "/tmp/pi-coding-agent-test-unique-67890/")
         (buf1 (pi-coding-agent--get-or-create-buffer :chat dir))
         (buf2 (pi-coding-agent--get-or-create-buffer :chat dir)))
    (unwind-protect
        (should (eq buf1 buf2))
      (when (buffer-live-p buf1)
        (kill-buffer buf1)))))

;;; Major Modes

(ert-deftest pi-coding-agent-test-chat-mode-is-read-only ()
  "pi-coding-agent-chat-mode sets buffer to read-only."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (should buffer-read-only)))

(ert-deftest pi-coding-agent-test-chat-mode-has-word-wrap ()
  "pi-coding-agent-chat-mode enables word wrap."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (should word-wrap)
    (should-not truncate-lines)))

(ert-deftest pi-coding-agent-test-chat-mode-disables-hl-line ()
  "pi-coding-agent-chat-mode disables hl-line to prevent scroll oscillation."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (should-not hl-line-mode)
    (should-not (buffer-local-value 'global-hl-line-mode (current-buffer)))))

(ert-deftest pi-coding-agent-test-input-mode-derives-from-text ()
  "pi-coding-agent-input-mode is derived from text-mode."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should (derived-mode-p 'text-mode))))

(ert-deftest pi-coding-agent-test-input-mode-not-read-only ()
  "pi-coding-agent-input-mode allows editing."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should-not buffer-read-only)))

;;; Session Directory Detection

(ert-deftest pi-coding-agent-test-session-directory-uses-project-root ()
  "Session directory is project root when in a project."
  (let ((default-directory "/tmp/"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) '(vc . "/home/user/myproject/")))
              ((symbol-function 'project-root)
               (lambda (_) "/home/user/myproject/")))
      (should (equal (pi-coding-agent--session-directory) "/home/user/myproject/")))))

(ert-deftest pi-coding-agent-test-session-directory-falls-back-to-default ()
  "Session directory is default-directory when not in a project."
  (let ((default-directory "/tmp/somedir/"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) nil)))
      (should (equal (pi-coding-agent--session-directory) "/tmp/somedir/")))))

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
          pi-coding-agent--message-start-marker (point-marker)
          pi-coding-agent--streaming-marker (point-marker)
          pi-coding-agent--in-code-block t
          pi-coding-agent--in-thinking-block t
          pi-coding-agent--line-parse-state 'code-fence
          pi-coding-agent--pending-tool-overlay (make-overlay 1 1))
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
    (should (null pi-coding-agent--message-start-marker))
    (should (null pi-coding-agent--streaming-marker))
    (should (null pi-coding-agent--in-code-block))
    (should (null pi-coding-agent--in-thinking-block))
    (should (eq pi-coding-agent--line-parse-state 'line-start))
    (should (null pi-coding-agent--pending-tool-overlay))
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

;;; Main Entry Point

(ert-deftest pi-coding-agent-test-pi-coding-agent-creates-chat-buffer ()
  "M-x pi creates a chat buffer."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-main/"
    (should (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-main/*"))))

(ert-deftest pi-coding-agent-test-pi-coding-agent-creates-input-buffer ()
  "M-x pi creates an input buffer."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-main2/"
    (should (get-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-main2/*"))))

(ert-deftest pi-coding-agent-test-pi-coding-agent-sets-major-modes ()
  "M-x pi sets correct major modes on buffers."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-modes/"
    (with-current-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-modes/*"
      (should (derived-mode-p 'pi-coding-agent-chat-mode)))
    (with-current-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-modes/*"
      (should (derived-mode-p 'pi-coding-agent-input-mode)))))

;;; Buffer Linkage

(ert-deftest pi-coding-agent-test-input-buffer-finds-chat ()
  "Input buffer can find associated chat buffer."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-link1/"
    (with-current-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-link1/*"
      (should (eq (pi-coding-agent--get-chat-buffer)
                  (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-link1/*"))))))

(ert-deftest pi-coding-agent-test-chat-buffer-finds-input ()
  "Chat buffer can find associated input buffer."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-link2/"
    (with-current-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-link2/*"
      (should (eq (pi-coding-agent--get-input-buffer)
                  (get-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-link2/*"))))))

(ert-deftest pi-coding-agent-test-get-process-from-chat ()
  "Can get process from chat buffer."
  (let ((default-directory "/tmp/pi-coding-agent-test-proc1/")
        (fake-proc 'mock-process))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) fake-proc))
              ((symbol-function 'pi-coding-agent--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (pi-coding-agent)
            (with-current-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-proc1/*"
              (should (eq (pi-coding-agent--get-process) fake-proc))))
        (ignore-errors (kill-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-proc1/*"))
        (ignore-errors (kill-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-proc1/*"))))))

(ert-deftest pi-coding-agent-test-get-process-from-input ()
  "Can get process from input buffer via chat buffer."
  (let ((default-directory "/tmp/pi-coding-agent-test-proc2/")
        (fake-proc 'mock-process))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) fake-proc))
              ((symbol-function 'pi-coding-agent--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (pi-coding-agent)
            (with-current-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-proc2/*"
              (should (eq (pi-coding-agent--get-process) fake-proc))))
        (ignore-errors (kill-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-proc2/*"))
        (ignore-errors (kill-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-proc2/*"))))))

;;; Sending Prompts

(ert-deftest pi-coding-agent-test-send-extracts-text ()
  "pi-coding-agent-send extracts text from input buffer and clears it."
  (let ((sent-text nil))
    (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-send1/"
      (cl-letf (((symbol-function 'pi-coding-agent--send-prompt)
                 (lambda (text) (setq sent-text text))))
        (with-current-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-send1/*"
          (insert "Hello, pi!")
          (pi-coding-agent-send)
          (should (equal sent-text "Hello, pi!"))
          (should (string-empty-p (buffer-string))))))))

(ert-deftest pi-coding-agent-test-send-empty-is-noop ()
  "pi-coding-agent-send with empty buffer does nothing."
  (let ((send-called nil))
    (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-send2/"
      (cl-letf (((symbol-function 'pi-coding-agent--send-prompt)
                 (lambda (_) (setq send-called t))))
        (with-current-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-send2/*"
          (pi-coding-agent-send)
          (should-not send-called))))))

(ert-deftest pi-coding-agent-test-send-whitespace-only-is-noop ()
  "pi-coding-agent-send with only whitespace does nothing."
  (let ((send-called nil))
    (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-send3/"
      (cl-letf (((symbol-function 'pi-coding-agent--send-prompt)
                 (lambda (_) (setq send-called t))))
        (with-current-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-send3/*"
          (insert "   \n\t  ")
          (pi-coding-agent-send)
          (should-not send-called))))))

(ert-deftest pi-coding-agent-test-send-queues-locally-while-streaming ()
  "pi-coding-agent-send adds to local queue while streaming, no RPC sent."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-stream*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-stream-input*"))
        (rpc-called nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf)
            (setq pi-coding-agent--followup-queue nil))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "My message")
            (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc _cmd _cb) (setq rpc-called t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "queued" (downcase fmt)))
                           (setq message-shown t)))))
              (pi-coding-agent-send))
            ;; Should NOT have called RPC (local queue instead)
            (should-not rpc-called)
            ;; Should have added to local queue in chat buffer
            (with-current-buffer chat-buf
              (should (equal pi-coding-agent--followup-queue '("My message"))))
            ;; Should have shown queued message
            (should message-shown)
            ;; Input should be cleared (message accepted)
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-send-queues-locally-while-compacting ()
  "pi-coding-agent-send adds to local queue while compacting, no RPC sent."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-compact*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-compact-input*"))
        (rpc-called nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'compacting)
            (setq pi-coding-agent--input-buffer input-buf)
            (setq pi-coding-agent--followup-queue nil))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "My message during compaction")
            (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc _cmd _cb) (setq rpc-called t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "queued" (downcase fmt)))
                           (setq message-shown t)))))
              (pi-coding-agent-send))
            ;; Should NOT have called RPC (local queue instead)
            (should-not rpc-called)
            ;; Should have added to local queue in chat buffer
            (with-current-buffer chat-buf
              (should (equal pi-coding-agent--followup-queue '("My message during compaction"))))
            ;; Should have shown queued message
            (should message-shown)
            ;; Input should be cleared (message accepted)
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-slash-compact-handled-locally-not-sent-as-prompt ()
  "/compact in input buffer invokes pi-coding-agent-compact locally, not sent to pi."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-slash-compact*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-slash-compact-input*"))
        (compact-called nil)
        (compact-args nil)
        (prompt-sent nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "/compact")
            (cl-letf (((symbol-function 'pi-coding-agent-compact)
                       (lambda (&optional args) (setq compact-called t compact-args args)))
                      ((symbol-function 'pi-coding-agent--send-prompt)
                       (lambda (_) (setq prompt-sent t))))
              (pi-coding-agent-send))
            ;; Should have called compact function with no args
            (should compact-called)
            (should (null compact-args))
            ;; Should NOT have sent as prompt
            (should-not prompt-sent)
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-slash-compact-with-args-passes-instructions ()
  "/compact with args passes custom instructions to compact function."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-slash-compact-args*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-slash-compact-args-input*"))
        (compact-called nil)
        (compact-args nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "/compact focus on the API design decisions")
            (cl-letf (((symbol-function 'pi-coding-agent-compact)
                       (lambda (&optional args) (setq compact-called t compact-args args))))
              (pi-coding-agent-send))
            ;; Should have called compact function with custom instructions
            (should compact-called)
            (should (equal compact-args "focus on the API design decisions"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-auto-compaction-success-sends-queued-messages ()
  "auto_compaction_end with aborted=false processes followup queue.
Uses :false (JSON false representation) to verify boolean normalization."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((sent-text nil))
      (setq pi-coding-agent--status 'compacting)
      (setq pi-coding-agent--followup-queue '("queued message"))
      (cl-letf (((symbol-function 'pi-coding-agent--send-prompt)
                 (lambda (text) (setq sent-text text))))
        (pi-coding-agent--handle-display-event
         '(:type "auto_compaction_end"
           :aborted :false
           :result (:tokensBefore 1000 :summary "Summary" :timestamp 1234567890000))))
      ;; Queue should be empty after processing
      (should (null pi-coding-agent--followup-queue))
      ;; The queued message should have been sent
      (should (equal sent-text "queued message")))))

(ert-deftest pi-coding-agent-test-auto-compaction-end-aborted-clears-queue ()
  "auto_compaction_end when aborted clears followup queue without sending."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((sent-text nil))
      (setq pi-coding-agent--status 'compacting)
      (setq pi-coding-agent--followup-queue '("queued message"))
      (cl-letf (((symbol-function 'pi-coding-agent--send-prompt)
                 (lambda (text) (setq sent-text text))))
        ;; Simulate auto_compaction_end event (aborted)
        (pi-coding-agent--handle-display-event
         '(:type "auto_compaction_end"
           :aborted t)))
      ;; Queue should be cleared (user cancelled)
      (should (null pi-coding-agent--followup-queue))
      ;; No message should have been sent
      (should (null sent-text)))))

;;; Response Display

(ert-deftest pi-coding-agent-test-append-to-chat-inserts-text ()
  "pi-coding-agent--append-to-chat inserts text at end of chat buffer."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--append-to-chat "Hello")
    (should (equal (buffer-string) "Hello"))))

(ert-deftest pi-coding-agent-test-append-to-chat-appends ()
  "pi-coding-agent--append-to-chat appends to existing content."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t))
      (insert "First"))
    (pi-coding-agent--append-to-chat " Second")
    (should (equal (buffer-string) "First Second"))))

(ert-deftest pi-coding-agent-test-display-agent-start-inserts-separator ()
  "agent_start event inserts a setext heading separator."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (should (string-match-p "Assistant\n===" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-message-delta-appends-text ()
  "message_update text_delta appends text to chat."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)  ; Creates streaming marker
    (pi-coding-agent--display-message-delta "Hello, ")
    (pi-coding-agent--display-message-delta "world!")
    (should (string-match-p "Hello, world!" (buffer-string)))))

(ert-deftest pi-coding-agent-test-delta-transforms-atx-headings ()
  "ATX headings in assistant content are leveled down.
# becomes ##, ## becomes ###, etc. This keeps our setext H1 separators
as the top-level structure."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "# Heading 1\n## Heading 2")
    ;; # should become ##, ## should become ###
    (should (string-match-p "## Heading 1" (buffer-string)))
    (should (string-match-p "### Heading 2" (buffer-string)))
    ;; Original single # should not appear (except as part of ##)
    (should-not (string-match-p "^# " (buffer-string)))))

(ert-deftest pi-coding-agent-test-delta-heading-transform-after-newline ()
  "Heading transform works when # follows newline within delta."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Some text\n# Heading")
    (should (string-match-p "Some text\n## Heading" (buffer-string)))))

(ert-deftest pi-coding-agent-test-delta-heading-transform-across-deltas ()
  "Heading transform works when newline and # are in separate deltas."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Some text\n")
    (pi-coding-agent--display-message-delta "# Heading")
    (should (string-match-p "## Heading" (buffer-string)))))

(ert-deftest pi-coding-agent-test-delta-no-transform-mid-line-hash ()
  "Hash characters mid-line are not transformed."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Use #include or C# language")
    ;; Mid-line # should stay as-is
    (should (string-match-p "#include" (buffer-string)))
    (should (string-match-p "C#" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-thinking-delta-appends-text ()
  "message_update thinking_delta appends text to chat."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)  ; Creates streaming marker
    (pi-coding-agent--display-thinking-delta "Let me think...")
    (pi-coding-agent--display-thinking-delta " about this.")
    (should (string-match-p "Let me think... about this." (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-agent-end-adds-newline ()
  "agent_end normalizes trailing whitespace to single newline."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--append-to-chat "Some response")
    (pi-coding-agent--display-agent-end)
    (should (string-suffix-p "response\n" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-no-blank-line-after-user-header ()
  "User header has no blank line after setext underline.
The hidden === provides visual spacing when `markdown-hide-markup' is t."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-user-message "Hello")
    ;; Pattern: setext heading (You + underline), NO blank line, content
    (should (string-match-p "You\n=+\nHello" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-no-blank-line-after-assistant-header ()
  "Assistant header has no blank line after setext underline.
The hidden === provides visual spacing when `markdown-hide-markup' is t."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Hi")
    ;; Pattern: setext heading (Assistant + underline), NO blank line, content
    (should (string-match-p "Assistant\n=+\nHi" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-delta-leading-newlines-stripped ()
  "Leading newlines from first text delta are stripped.
Models often send \\n\\n before first content, which would create
extra blank lines after the setext header."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "\n\nHi")
    (should (string-match-p "Assistant\n=+\nHi" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-thinking-leading-newlines-stripped ()
  "Leading newlines before thinking block are stripped.
Models may send \\n\\n before thinking content too."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-thinking-start)
    ;; Thinking start should appear directly after header, no blank line
    (should (string-match-p "Assistant\n=+\n>" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-blank-line-before-tool ()
  "Tool block is preceded by blank line when after text."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Let me check.")
    (pi-coding-agent--render-complete-message)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    ;; Pattern: text, blank line, $ command
    (should (string-match-p "check\\.\n\n\\$ ls" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-blank-line-after-tool ()
  "Tool block is followed by blank line."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file.txt"))
                          nil nil)
    ;; Should end with closing fence and blank line
    (should (string-match-p "```\n\n" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-single-blank-line-between-turns ()
  "Only one blank line between agent response and next section header.
agent_end + next section's leading newline must not create triple newlines."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Turn 1: user + assistant
    (pi-coding-agent--display-user-message "Hi")
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Hello!")
    (pi-coding-agent--render-complete-message)
    (pi-coding-agent--display-agent-end)
    ;; Turn 2: user message
    (setq pi-coding-agent--assistant-header-shown nil)
    (pi-coding-agent--display-user-message "Bye")
    ;; Should never have triple newlines (which would be two blank lines)
    (should-not (string-match-p "\n\n\n" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-single-blank-line-before-compaction ()
  "Only one blank line between agent response and compaction header."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Some response.")
    (pi-coding-agent--render-complete-message)
    (pi-coding-agent--display-agent-end)
    (pi-coding-agent--display-compaction-result 50000 "Summary.")
    (should-not (string-match-p "\n\n\n" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-no-double-blank-between-tools ()
  "Consecutive tools have single blank line between them."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file1"))
                          nil nil)
    (pi-coding-agent--display-tool-start "read" '(:path "file.txt"))
    ;; Should have closing fence, blank line, then next tool
    (should (string-match-p "```\n\nread file\\.txt" (buffer-string)))
    (should-not (string-match-p "\n\n\n" (buffer-string)))))

;;; History Display

(ert-deftest pi-coding-agent-test-history-displays-compaction-summary ()
  "Compaction summary messages display with header, tokens, and summary."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((messages [(:role "compactionSummary"
                      :summary "Session was compacted. Key points: user asked about testing."
                      :tokensBefore 50000
                      :timestamp 1704067200000)]))  ; 2024-01-01 00:00:00 UTC
      (pi-coding-agent--display-history-messages messages))
    ;; Should have Compaction header
    (should (string-match-p "Compaction" (buffer-string)))
    ;; Should show tokens
    (should (string-match-p "50,000 tokens" (buffer-string)))
    ;; Should show summary text
    (should (string-match-p "Key points" (buffer-string)))))

;;; Streaming Marker

(ert-deftest pi-coding-agent-test-streaming-marker-created-on-agent-start ()
  "Streaming marker is created on agent_start."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (should (markerp pi-coding-agent--streaming-marker))
    (should (= (marker-position pi-coding-agent--streaming-marker) (point-max)))))

(ert-deftest pi-coding-agent-test-streaming-marker-advances-with-delta ()
  "Streaming marker advances as deltas are inserted."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (let ((initial-pos (marker-position pi-coding-agent--streaming-marker)))
      (pi-coding-agent--display-message-delta "Hello")
      (should (= (marker-position pi-coding-agent--streaming-marker)
                 (+ initial-pos 5))))))

(ert-deftest pi-coding-agent-test-streaming-inserts-at-marker ()
  "Deltas are inserted at the streaming marker position."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "First")
    (pi-coding-agent--display-message-delta " Second")
    (should (string-match-p "First Second" (buffer-string)))))

;;; Auto-scroll

(ert-deftest pi-coding-agent-test-window-following-p-at-end ()
  "pi-coding-agent--window-following-p detects when window-point is at end."
  (with-temp-buffer
    (insert "some content")
    ;; Mock window-point to return point-max
    (cl-letf (((symbol-function 'window-point) (lambda (_w) (point-max))))
      (should (pi-coding-agent--window-following-p 'mock-window)))))

(ert-deftest pi-coding-agent-test-window-following-p-not-at-end ()
  "pi-coding-agent--window-following-p returns nil when window-point is earlier."
  (with-temp-buffer
    (insert "some content")
    ;; Mock window-point to return position before end
    (cl-letf (((symbol-function 'window-point) (lambda (_w) 1)))
      (should-not (pi-coding-agent--window-following-p 'mock-window)))))



;;; Abort Command

(ert-deftest pi-coding-agent-test-abort-sends-command ()
  "pi-coding-agent-abort sends abort command via RPC."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((sent-command nil)
          (pi-coding-agent--status 'streaming))
      (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                ((symbol-function 'pi-coding-agent--get-chat-buffer) (lambda () (current-buffer)))
                ((symbol-function 'pi-coding-agent--rpc-async)
                 (lambda (_proc cmd _cb) (setq sent-command cmd))))
        (pi-coding-agent-abort)
        (should (equal (plist-get sent-command :type) "abort"))))))

(ert-deftest pi-coding-agent-test-abort-noop-when-not-streaming ()
  "pi-coding-agent-abort does nothing when not streaming."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((sent-command nil)
          (pi-coding-agent--status 'idle))
      (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                ((symbol-function 'pi-coding-agent--get-chat-buffer) (lambda () (current-buffer)))
                ((symbol-function 'pi-coding-agent--rpc-async)
                 (lambda (_proc cmd _cb) (setq sent-command cmd))))
        (pi-coding-agent-abort)
        (should (null sent-command))))))

(ert-deftest pi-coding-agent-test-abort-clears-followup-queue ()
  "Aborting clears the follow-up queue so queued messages are not sent.
When user aborts, they want to stop everything - including queued messages."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t)
          (message-was-sent nil))
      (insert "Some streaming content")
      ;; Set up state as if we're streaming with a queued message
      (setq pi-coding-agent--aborted t
            pi-coding-agent--followup-queue '("queued message that should be discarded"))
      ;; Mock send functions to detect if queue processing sends the message
      (cl-letf (((symbol-function 'pi-coding-agent--prepare-and-send)
                 (lambda (_text) (setq message-was-sent t)))
                ((symbol-function 'pi-coding-agent--spinner-stop) #'ignore)
                ((symbol-function 'pi-coding-agent--fontify-timer-stop) #'ignore)
                ((symbol-function 'pi-coding-agent--refresh-header) #'ignore))
        ;; Simulate agent_end arriving after abort
        (pi-coding-agent--display-agent-end)
        ;; Queue should be empty (either cleared or not processed)
        (should (null pi-coding-agent--followup-queue))
        ;; Key assertion: queued message should NOT have been sent
        (should-not message-was-sent)))))

;;; Kill Buffer Protection

(ert-deftest pi-coding-agent-test-handler-removed-on-kill ()
  "Event handler is removed when chat buffer is killed."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((fake-proc (start-process "test" nil "true")))
      (unwind-protect
          (progn
            (setq pi-coding-agent--process fake-proc)
            (pi-coding-agent--register-display-handler fake-proc)
            (should (process-get fake-proc 'pi-coding-agent-display-handler))
            (pi-coding-agent--cleanup-on-kill)
            (should-not (process-get fake-proc 'pi-coding-agent-display-handler)))
        (when (process-live-p fake-proc)
          (delete-process fake-proc))))))

;;; Pandoc Conversion

(ert-deftest pi-coding-agent-test-message-start-marker-created ()
  "Message start position is tracked for later replacement."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t))
      (insert "Previous content\n"))
    (pi-coding-agent--display-agent-start)
    (should (markerp pi-coding-agent--message-start-marker))
    (should (= (marker-position pi-coding-agent--message-start-marker)
               (marker-position pi-coding-agent--streaming-marker)))))

(ert-deftest pi-coding-agent-test-render-complete-message-applies-fontlock ()
  "Rendering applies font-lock to markdown content."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "# Hello\n\n**Bold**")
    ;; Raw markdown should be present
    (should (string-match-p "# Hello" (buffer-string)))
    ;; Now render
    (pi-coding-agent--render-complete-message)
    ;; Markdown stays as markdown (gfm-mode handles display)
    (should (string-match-p "# Hello" (buffer-string)))
    (should (string-match-p "\\*\\*Bold\\*\\*" (buffer-string)))))

(ert-deftest pi-coding-agent-test-render-complete-message-aligns-tables ()
  "Rendering aligns markdown tables."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    ;; Insert unaligned table (columns have different widths)
    (pi-coding-agent--display-message-delta
     "| Short | Much Longer |\n|---|---|\n| a | b |\n")
    ;; Before render: table is unaligned (separators are just ---)
    (should (string-match-p "|---|---|" (buffer-string)))
    ;; Render the message
    (pi-coding-agent--render-complete-message)
    ;; After render: separator dashes should match column widths
    ;; "Short" = 5 chars, "Much Longer" = 11 chars
    ;; So separators should be at least that wide
    (should (string-match-p "|[-]+|[-]+|" (buffer-string)))
    ;; The short separator "---" should now be longer (at least 5 dashes)
    (should-not (string-match-p "|---|---|" (buffer-string)))))

(ert-deftest pi-coding-agent-test-history-text-aligns-tables ()
  "Tables in restored history messages get aligned."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--chat-buffer (current-buffer))
    ;; Simulate rendering history text with a table
    (pi-coding-agent--render-history-text
     "| Short | Much Longer |\n|---|---|\n| a | b |\n")
    ;; After render: tables should be aligned (separators expanded)
    (should-not (string-match-p "|---|---|" (buffer-string)))))

;;; Syntax Highlighting

(ert-deftest pi-coding-agent-test-chat-mode-derives-from-gfm ()
  "Chat mode derives from gfm-mode for syntax highlighting."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (should (derived-mode-p 'gfm-mode))))

(ert-deftest pi-coding-agent-test-chat-mode-fontifies-code ()
  "Code blocks get syntax highlighting."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t))
      (insert "```python\ndef hello():\n    return 42\n```\n")
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "def" nil t)
      (let ((face (get-text-property (match-beginning 0) 'face)))
        ;; Should have font-lock-keyword-face (from python)
        (should (or (eq face 'font-lock-keyword-face)
                    (and (listp face) (memq 'font-lock-keyword-face face))))))))

(ert-deftest pi-coding-agent-test-incomplete-code-block-does-not-break-fontlock ()
  "Incomplete code block during streaming does not break font-lock.
Simulates streaming where code block opening arrives before closing.
Font-lock should handle gracefully: no highlighting until complete,
then proper highlighting once block is closed."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t))
      ;; Simulate streaming: block opened but not closed
      (insert "```python\ndef hello():\n    return 42\n")
      (font-lock-ensure)
      ;; Should not error, buffer should be functional
      (should (eq major-mode 'pi-coding-agent-chat-mode))
      (goto-char (point-min))
      (should (search-forward "def" nil t))
      ;; No highlighting expected for incomplete block
      (let ((face (get-text-property (match-beginning 0) 'face)))
        (should (or (null face)
                    (not (memq 'font-lock-keyword-face (ensure-list face))))))
      ;; Complete the block
      (goto-char (point-max))
      (insert "```\n")
      (font-lock-ensure)
      ;; Now should have highlighting
      (goto-char (point-min))
      (search-forward "def" nil t)
      (let ((face (get-text-property (match-beginning 0) 'face)))
        (should (or (eq face 'font-lock-keyword-face)
                    (and (listp face) (memq 'font-lock-keyword-face face))))))))

;;; User Message Display

(ert-deftest pi-coding-agent-test-display-user-message-inserts-text ()
  "User message is inserted into chat buffer."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-user-message "Hello world")
    (should (string-match-p "Hello world" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-user-message-has-prefix ()
  "User message has You label in setext heading."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-user-message "Test message")
    (should (string-match-p "^You" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-user-message-has-separator ()
  "User message has setext underline separator."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-user-message "Test")
    (should (string-match-p "^===" (buffer-string)))))

(ert-deftest pi-coding-agent-test-send-displays-user-message ()
  "Sending a prompt displays the user message in chat."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-chat*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Hello from test")
            ;; Mock the process to avoid actual RPC
            (setq pi-coding-agent--process nil)
            (pi-coding-agent-send))
          ;; Check chat buffer has the message with You setext heading and content
          (with-current-buffer chat-buf
            (should (string-match-p "^You" (buffer-string)))
            (should (string-match-p "Hello from test" (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-send-slash-command-not-displayed-locally ()
  "Slash commands are NOT displayed locally - pi sends back expanded content.
This avoids showing both the command and its expansion."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-chat*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "/greet world")
            ;; Mock the process to avoid actual RPC
            (setq pi-coding-agent--process nil)
            (pi-coding-agent-send))
          ;; Check chat buffer does NOT have the command - pi will send expanded content
          (with-current-buffer chat-buf
            (should-not (string-match-p "/greet" (buffer-string)))
            ;; local-user-message should be nil for slash commands
            (should-not pi-coding-agent--local-user-message)))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-slash-command-after-abort-no-duplicate-headers ()
  "Sending slash command after abort should not show duplicate Assistant headers.
Regression test for bug where:
1. Assistant streams, user aborts
2. User types /fix-tests in input buffer  
3. Two 'Assistant' headers appear before the user message

The fix: don't set assistant-header-shown to nil when sending slash commands,
since we don't display them locally. Let pi's message_start handle it."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-abort-cmd*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-abort-cmd-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--input-buffer input-buf)
            (setq pi-coding-agent--status 'idle)
            ;; Simulate state after an aborted assistant turn:
            ;; - assistant-header-shown is t (header was shown for aborted turn)
            (setq pi-coding-agent--assistant-header-shown t)
            (let ((inhibit-read-only t))
              (insert "Assistant\n=========\nSome content...\n\n[Aborted]\n\n")))
          
          ;; User sends a slash command from input buffer
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "/fix-tests")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt) #'ignore)
                      ((symbol-function 'pi-coding-agent--spinner-start) #'ignore))
              (pi-coding-agent-send)))
          
          ;; KEY ASSERTION: assistant-header-shown should still be t
          ;; because we didn't display anything locally for slash commands
          (with-current-buffer chat-buf
            (should pi-coding-agent--assistant-header-shown)
            
            ;; Now simulate pi's response sequence
            ;; 1. agent_start - should NOT add header (already shown)
            (pi-coding-agent--handle-display-event '(:type "agent_start"))
            
            ;; Count Assistant headers - should still be just 1
            (let ((count 0)
                  (content (buffer-string)))
              (with-temp-buffer
                (insert content)
                (goto-char (point-min))
                (while (search-forward "Assistant\n=========" nil t)
                  (setq count (1+ count))))
              (should (= count 1)))
            
            ;; 2. message_start with user role (expanded template)
            (pi-coding-agent--handle-display-event
             '(:type "message_start"
               :message (:role "user"
                         :content [(:type "text" :text "Your task is to fix tests...")]
                         :timestamp 1704067200000)))
            
            ;; ISSUE #5: Verify expanded content is actually displayed
            (should (string-match-p "Your task is to fix tests" (buffer-string)))
            
            ;; 3. message_start with assistant role
            (pi-coding-agent--handle-display-event
             '(:type "message_start"
               :message (:role "assistant")))
            
            ;; Final count: should be exactly 2 Assistant headers
            ;; (one from aborted turn, one from new turn)
            (let ((count 0)
                  (content (buffer-string)))
              (with-temp-buffer
                (insert content)
                (goto-char (point-min))
                (while (search-forward "Assistant\n=========" nil t)
                  (setq count (1+ count))))
              (should (= count 2)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-ms-to-time-converts-correctly ()
  "pi-coding-agent--ms-to-time converts milliseconds to Emacs time."
  ;; 1704067200000 ms = 2024-01-01 00:00:00 UTC
  (let ((time (pi-coding-agent--ms-to-time 1704067200000)))
    (should time)
    (should (equal (format-time-string "%Y-%m-%d" time t) "2024-01-01"))))

(ert-deftest pi-coding-agent-test-ms-to-time-returns-nil-for-nil ()
  "pi-coding-agent--ms-to-time returns nil when given nil."
  (should (null (pi-coding-agent--ms-to-time nil))))

(ert-deftest pi-coding-agent-test-format-message-timestamp-today ()
  "Format timestamp shows just time for today."
  (let ((now (current-time)))
    (should (string-match-p "^[0-2][0-9]:[0-5][0-9]$"
                            (pi-coding-agent--format-message-timestamp now)))))

(ert-deftest pi-coding-agent-test-format-message-timestamp-other-day ()
  "Format timestamp shows ISO date and time for other days."
  (let ((yesterday (time-subtract (current-time) (days-to-time 1))))
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-2][0-9]:[0-5][0-9]$"
                            (pi-coding-agent--format-message-timestamp yesterday)))))

(ert-deftest pi-coding-agent-test-display-user-message-with-timestamp ()
  "User message displays with timestamp when provided."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-user-message "Test message" (current-time))
    (let ((content (buffer-string)))
      ;; Setext format: "You · HH:MM" on header line
      (should (string-match-p "You · " content))
      ;; Should have HH:MM timestamp format
      (should (string-match-p "[0-2][0-9]:[0-5][0-9]" content)))))

(ert-deftest pi-coding-agent-test-separator-without-timestamp ()
  "Separator without timestamp is setext H1 heading."
  (let ((sep (pi-coding-agent--make-separator "You")))
    ;; Setext format: label on one line, === underline on next
    (should (string-match-p "^You\n=+$" sep))))

(ert-deftest pi-coding-agent-test-separator-with-timestamp ()
  "Separator with timestamp shows label · time as setext H1."
  (let ((sep (pi-coding-agent--make-separator "You" (current-time))))
    ;; Format: "You · HH:MM" followed by newline and ===
    (should (string-match-p "^You · [0-2][0-9]:[0-5][0-9]\n=+$" sep))))

(ert-deftest pi-coding-agent-test-separator-is-valid-setext-heading ()
  "Separator produces valid markdown setext H1 syntax."
  (let ((sep (pi-coding-agent--make-separator "Assistant")))
    ;; Must have at least 3 = characters for valid setext
    (should (string-match-p "\n===+" sep))
    ;; Underline should match or exceed label length
    (should (>= (length (car (last (split-string sep "\n"))))
                (length "Assistant")))))

;;; Startup Header

(ert-deftest pi-coding-agent-test-startup-header-shows-version ()
  "Startup header includes version."
  (let ((header (pi-coding-agent--format-startup-header)))
    (should (string-match-p "Pi" header))))

(ert-deftest pi-coding-agent-test-startup-header-shows-keybindings ()
  "Startup header includes key keybindings."
  (let ((header (pi-coding-agent--format-startup-header)))
    (should (string-match-p "C-c C-c" header))
    (should (string-match-p "send" header))))

(ert-deftest pi-coding-agent-test-startup-header-shows-pi-coding-agent-version ()
  "Startup header includes pi CLI version."
  (let ((header (pi-coding-agent--format-startup-header)))
    ;; Should show "Pi X.Y.Z" or "Pi Unknown" in separator
    (should (string-match-p "Pi " header))))

;;; Error and Retry Handling

(ert-deftest pi-coding-agent-test-display-retry-start-shows-attempt ()
  "auto_retry_start event shows attempt number and delay."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-retry-start '(:type "auto_retry_start"
                               :attempt 1
                               :maxAttempts 3
                               :delayMs 2000
                               :errorMessage "429 rate_limit_error"))
    (should (string-match-p "Retry 1/3" (buffer-string)))
    (should (string-match-p "2s" (buffer-string)))
    ;; Raw error message is shown as-is
    (should (string-match-p "429 rate_limit_error" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-retry-start-with-overloaded-error ()
  "auto_retry_start shows overloaded error message."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-retry-start '(:type "auto_retry_start"
                               :attempt 2
                               :maxAttempts 3
                               :delayMs 4000
                               :errorMessage "529 overloaded_error: Overloaded"))
    (should (string-match-p "Retry 2/3" (buffer-string)))
    (should (string-match-p "overloaded" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-retry-end-success ()
  "auto_retry_end with success shows success message."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-retry-end '(:type "auto_retry_end"
                             :success t
                             :attempt 2))
    (should (string-match-p "succeeded" (buffer-string)))
    (should (string-match-p "attempt 2" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-retry-end-failure ()
  "auto_retry_end with failure shows final error."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-retry-end '(:type "auto_retry_end"
                             :success :false
                             :attempt 3
                             :finalError "529 overloaded_error: Overloaded"))
    (should (string-match-p "failed" (buffer-string)))
    (should (string-match-p "3 attempts" (buffer-string)))
    (should (string-match-p "overloaded" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-error-shows-message ()
  "pi-coding-agent--display-error shows error message with proper face."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-error "API error: insufficient quota")
    (should (string-match-p "Error:" (buffer-string)))
    (should (string-match-p "insufficient quota" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-error-handles-nil ()
  "pi-coding-agent--display-error handles nil error message."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-error nil)
    (should (string-match-p "Error:" (buffer-string)))
    (should (string-match-p "unknown" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-extension-error ()
  "extension_error event shows extension name and error."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-extension-error '(:type "extension_error"
                              :extensionPath "/home/user/.pi/extensions/before_send.ts"
                              :event "tool_call"
                              :error "TypeError: Cannot read property"))
    (should (string-match-p "Extension error" (buffer-string)))
    (should (string-match-p "before_send.ts" (buffer-string)))
    (should (string-match-p "tool_call" (buffer-string)))
    (should (string-match-p "TypeError" (buffer-string)))))

(ert-deftest pi-coding-agent-test-handle-display-event-retry-start ()
  "pi-coding-agent--handle-display-event handles auto_retry_start."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state nil))
      (pi-coding-agent--handle-display-event '(:type "auto_retry_start"
                                  :attempt 1
                                  :maxAttempts 3
                                  :delayMs 2000
                                  :errorMessage "429 rate_limit_error"))
      (should (string-match-p "Retry" (buffer-string))))))

(ert-deftest pi-coding-agent-test-handle-display-event-retry-end ()
  "pi-coding-agent--handle-display-event handles auto_retry_end."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state nil))
      (pi-coding-agent--handle-display-event '(:type "auto_retry_end"
                                  :success t
                                  :attempt 2))
      (should (string-match-p "succeeded" (buffer-string))))))

(ert-deftest pi-coding-agent-test-handle-display-event-extension-error ()
  "pi-coding-agent--handle-display-event handles extension_error."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state (list :last-error nil)))
      (pi-coding-agent--handle-display-event '(:type "extension_error"
                                  :extensionPath "/path/extension.ts"
                                  :event "before_send"
                                  :error "Extension failed"))
      (should (string-match-p "Extension error" (buffer-string))))))

(ert-deftest pi-coding-agent-test-handle-display-event-message-error ()
  "pi-coding-agent--handle-display-event handles message_update with error type."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Need to set up markers first
    (pi-coding-agent--display-agent-start)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state (list :current-message '(:role "assistant"))))
      (pi-coding-agent--handle-display-event '(:type "message_update"
                                  :message (:role "assistant")
                                  :assistantMessageEvent (:type "error"
                                                          :reason "API connection failed")))
      (should (string-match-p "Error:" (buffer-string)))
      (should (string-match-p "API connection failed" (buffer-string))))))

(ert-deftest pi-coding-agent-test-display-no-model-warning ()
  "pi-coding-agent--display-no-model-warning shows setup instructions."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-no-model-warning)
    (should (string-match-p "No models available" (buffer-string)))
    (should (string-match-p "API key" (buffer-string)))
    (should (string-match-p "pi --login" (buffer-string)))))

;;; Extension UI Request Handling

(ert-deftest pi-coding-agent-test-extension-ui-notify ()
  "extension_ui_request notify method shows message."
  (let ((message-shown nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq message-shown (apply #'format fmt args)))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (let ((pi-coding-agent--process nil))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-1"
             :method "notify"
             :message "Extension loaded successfully"
             :notifyType "info")))
        (should message-shown)
        (should (string-match-p "Extension loaded successfully" message-shown))))))

(ert-deftest pi-coding-agent-test-extension-ui-confirm-yes ()
  "extension_ui_request confirm method uses yes-or-no-p and sends response."
  (let ((response-sent nil))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt) t))
              ((symbol-function 'pi-coding-agent--send-extension-ui-response)
               (lambda (_proc msg)
                 (setq response-sent msg))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (let ((pi-coding-agent--process t))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-2"
             :method "confirm"
             :title "Delete file?"
             :message "This cannot be undone")))
        (should response-sent)
        (should (equal (plist-get response-sent :type) "extension_ui_response"))
        (should (equal (plist-get response-sent :id) "req-2"))
        (should (eq (plist-get response-sent :confirmed) t))))))

(ert-deftest pi-coding-agent-test-extension-ui-confirm-no ()
  "extension_ui_request confirm method sends confirmed:false when user declines."
  (let ((response-sent nil))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt) nil))
              ((symbol-function 'pi-coding-agent--send-extension-ui-response)
               (lambda (_proc msg)
                 (setq response-sent msg))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (let ((pi-coding-agent--process t))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-3"
             :method "confirm"
             :title "Delete?"
             :message "Are you sure?")))
        (should response-sent)
        ;; :json-false is the correct encoding for JSON false in json-encode
        (should (eq (plist-get response-sent :confirmed) :json-false))))))

(ert-deftest pi-coding-agent-test-extension-ui-select ()
  "extension_ui_request select method uses completing-read and sends response."
  (let ((response-sent nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt options &rest _args)
                 (car options)))  ; Return first option
              ((symbol-function 'pi-coding-agent--send-extension-ui-response)
               (lambda (_proc msg)
                 (setq response-sent msg))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (let ((pi-coding-agent--process t))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-4"
             :method "select"
             :title "Pick one:"
             :options ["Option A" "Option B" "Option C"])))
        (should response-sent)
        (should (equal (plist-get response-sent :type) "extension_ui_response"))
        (should (equal (plist-get response-sent :id) "req-4"))
        (should (equal (plist-get response-sent :value) "Option A"))))))

(ert-deftest pi-coding-agent-test-extension-ui-input ()
  "extension_ui_request input method uses read-string and sends response."
  (let ((response-sent nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt &optional _initial) "user input"))
              ((symbol-function 'pi-coding-agent--send-extension-ui-response)
               (lambda (_proc msg)
                 (setq response-sent msg))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (let ((pi-coding-agent--process t))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-5"
             :method "input"
             :title "Enter name:"
             :placeholder "John Doe")))
        (should response-sent)
        (should (equal (plist-get response-sent :type) "extension_ui_response"))
        (should (equal (plist-get response-sent :id) "req-5"))
        (should (equal (plist-get response-sent :value) "user input"))))))

(ert-deftest pi-coding-agent-test-extension-ui-set-editor-text ()
  "extension_ui_request set_editor_text inserts text into input buffer."
  (let ((input-buf (get-buffer-create "*pi-test-input*")))
    (unwind-protect
        (with-temp-buffer
          (pi-coding-agent-chat-mode)
          (setq pi-coding-agent--input-buffer input-buf)
          (with-current-buffer input-buf
            (erase-buffer))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-6"
             :method "set_editor_text"
             :text "Prefilled text"))
          (should (equal (with-current-buffer input-buf (buffer-string))
                         "Prefilled text")))
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-extension-ui-set-status ()
  "extension_ui_request setStatus updates extension status storage."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--extension-status nil)
    (pi-coding-agent--handle-extension-ui-request
     '(:type "extension_ui_request"
       :id "req-7"
       :method "setStatus"
       :statusKey "my-ext"
       :statusText "Processing..."))
    (should (equal (cdr (assoc "my-ext" pi-coding-agent--extension-status))
                   "Processing..."))))

(ert-deftest pi-coding-agent-test-extension-ui-set-status-clear ()
  "extension_ui_request setStatus with nil clears the status."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--extension-status '(("my-ext" . "Old status")))
    (pi-coding-agent--handle-extension-ui-request
     '(:type "extension_ui_request"
       :id "req-8"
       :method "setStatus"
       :statusKey "my-ext"
       :statusText nil))
    (should-not (assoc "my-ext" pi-coding-agent--extension-status))))

(ert-deftest pi-coding-agent-test-header-format-extension-status ()
  "Extension status formats correctly in header-line."
  ;; Empty status returns empty string
  (should (equal (pi-coding-agent--header-format-extension-status nil) ""))
  ;; Single status
  (let ((result (pi-coding-agent--header-format-extension-status '(("ext1" . "Processing...")))))
    (should (string-match-p "│" result))
    (should (string-match-p "Processing" result)))
  ;; Multiple statuses joined with separator
  (let ((result (pi-coding-agent--header-format-extension-status
                 '(("ext1" . "Status 1") ("ext2" . "Status 2")))))
    (should (string-match-p "│" result))
    (should (string-match-p "Status 1" result))
    (should (string-match-p "Status 2" result))
    (should (string-match-p "·" result))))

(ert-deftest pi-coding-agent-test-extension-ui-unknown-cancels ()
  "extension_ui_request with unknown method sends cancelled response."
  (let ((response-sent nil))
    (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
               (lambda (_proc msg _cb)
                 (setq response-sent msg))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (let ((pi-coding-agent--process t))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-9"
             :method "setWidget"
             :widgetKey "my-ext"
             :widgetLines ["Line 1"])))
        (should response-sent)
        (should (equal (plist-get response-sent :type) "extension_ui_response"))
        (should (equal (plist-get response-sent :id) "req-9"))
        (should (eq (plist-get response-sent :cancelled) t))))))

(ert-deftest pi-coding-agent-test-extension-ui-editor-cancels ()
  "extension_ui_request editor method sends cancelled (not supported)."
  (let ((response-sent nil))
    (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
               (lambda (_proc msg _cb)
                 (setq response-sent msg))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (let ((pi-coding-agent--process t))
          (pi-coding-agent--handle-extension-ui-request
           '(:type "extension_ui_request"
             :id "req-10"
             :method "editor"
             :title "Edit:"
             :prefill "some text")))
        (should response-sent)
        (should (eq (plist-get response-sent :cancelled) t))))))

;;; Tool Output

(ert-deftest pi-coding-agent-test-tool-start-inserts-header ()
  "tool_execution_start inserts tool header."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" (list :command "ls -la"))
    ;; Should have $ command format
    (should (string-match-p "\\$ ls -la" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-start-handles-file-path-key ()
  "tool_execution_start handles :file_path key (alternative to :path)."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Test read tool with :file_path
    (pi-coding-agent--display-tool-start "read" '(:file_path "/tmp/test.txt"))
    (should (string-match-p "read /tmp/test.txt" (buffer-string))))
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Test write tool with :file_path
    (pi-coding-agent--display-tool-start "write" '(:file_path "/tmp/out.py"))
    (should (string-match-p "write /tmp/out.py" (buffer-string))))
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Test edit tool with :file_path
    (pi-coding-agent--display-tool-start "edit" '(:file_path "/tmp/edit.rs"))
    (should (string-match-p "edit /tmp/edit.rs" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-end-inserts-result ()
  "tool_execution_end inserts the tool result."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file1\nfile2"))
                          nil nil)
    (should (string-match-p "file1" (buffer-string)))))

(ert-deftest pi-coding-agent-test-bash-output-wrapped-in-text-fence ()
  "Bash output is wrapped in ```text fence for visual consistency."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file1"))
                          nil nil)
    ;; Should have text fence markers
    (should (string-match-p "```text" (buffer-string)))
    (should (string-match-p "```$" (buffer-string)))))

(ert-deftest pi-coding-agent-test-bash-output-strips-ansi-codes ()
  "ANSI escape codes are stripped from bash output."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Simulate colored test output: blue "▶ Test", green "✓ pass"
    ;; \033[34m = blue, \033[32m = green, \033[0m = reset
    (let ((ansi-output "\033[34m▶ AmbientSoundConfig\033[0m\n\033[32m  ✓ \033[0mshould pass"))
      (pi-coding-agent--display-tool-end "bash" '(:command "test")
                            `((:type "text" :text ,ansi-output))
                            nil nil)
      (let ((result (buffer-string)))
        ;; Text content should be preserved
        (should (string-match-p "▶ AmbientSoundConfig" result))
        (should (string-match-p "✓" result))
        (should (string-match-p "should pass" result))
        ;; ANSI escape sequences should be gone
        (should-not (string-match-p "\033" result))
        (should-not (string-match-p "\\[34m" result))
        (should-not (string-match-p "\\[32m" result))
        (should-not (string-match-p "\\[0m" result))))))

(ert-deftest pi-coding-agent-test-tool-output-shows-preview-when-long ()
  "Tool output shows preview lines when it exceeds the limit."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((long-output (mapconcat (lambda (n) (format "line%d" n))
                                  (number-sequence 1 10)
                                  "\n")))
      (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                            `((:type "text" :text ,long-output))
                            nil nil)
      ;; Should have first 5 preview lines (bash limit)
      (should (string-match-p "line1" (buffer-string)))
      (should (string-match-p "line5" (buffer-string)))
      ;; Should have more-lines indicator
      (should (string-match-p "more lines" (buffer-string))))))

(ert-deftest pi-coding-agent-test-tool-output-short-shows-all ()
  "Short tool output shows all lines without truncation."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((short-output "line1\nline2\nline3"))
      (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                            `((:type "text" :text ,short-output))
                            nil nil)
      ;; Should have all lines
      (should (string-match-p "line1" (buffer-string)))
      (should (string-match-p "line2" (buffer-string)))
      (should (string-match-p "line3" (buffer-string)))
      ;; Should NOT have more-lines indicator
      (should-not (string-match-p "more lines" (buffer-string))))))

;;; Diff Overlay Highlighting

(ert-deftest pi-coding-agent-test-apply-diff-overlays-added-line ()
  "Diff overlays should mark added lines with diff-added faces."
  (with-temp-buffer
    ;; Use actual pi format: +<space><padded-linenum><space><code>
    (insert "+ 7     added line\n")
    (pi-coding-agent--apply-diff-overlays (point-min) (point-max))
    (goto-char (point-min))
    ;; Should have overlay with diff-indicator-added on the + character
    (let ((ovs (seq-filter (lambda (ov) (overlay-get ov 'pi-coding-agent-diff-overlay))
                           (overlays-at (point)))))
      (should ovs)
      (should (memq 'diff-indicator-added
                    (mapcar (lambda (ov) (overlay-get ov 'face)) ovs))))))

(ert-deftest pi-coding-agent-test-apply-diff-overlays-removed-line ()
  "Diff overlays should mark removed lines with diff-removed faces."
  (with-temp-buffer
    ;; Use actual pi format: -<space><padded-linenum><space><code>
    (insert "-12     removed line\n")
    (pi-coding-agent--apply-diff-overlays (point-min) (point-max))
    (goto-char (point-min))
    (let ((ovs (seq-filter (lambda (ov) (overlay-get ov 'pi-coding-agent-diff-overlay))
                           (overlays-at (point)))))
      (should ovs)
      (should (memq 'diff-indicator-removed
                    (mapcar (lambda (ov) (overlay-get ov 'face)) ovs))))))

(ert-deftest pi-coding-agent-test-apply-diff-overlays-multiline ()
  "Diff overlays should handle multiple diff lines."
  (with-temp-buffer
    ;; Use actual pi format
    (insert "+ 1     added\n- 2     removed\n")
    (pi-coding-agent--apply-diff-overlays (point-min) (point-max))
    ;; Count diff overlays
    (let ((all-ovs (seq-filter (lambda (ov) (overlay-get ov 'pi-coding-agent-diff-overlay))
                               (overlays-in (point-min) (point-max)))))
      ;; Should have 4 overlays: indicator + line for each of 2 lines
      (should (= 4 (length all-ovs))))))

(ert-deftest pi-coding-agent-test-apply-diff-overlays-line-background ()
  "Diff overlays should apply background color to entire line."
  (with-temp-buffer
    ;; Use actual pi format: "+ 7     def foo():"
    (insert "+ 7     def foo():\n")
    (pi-coding-agent--apply-diff-overlays (point-min) (point-max))
    ;; Check overlay at "def" position (after "+ 7     ")
    (goto-char 9)
    (let ((ovs (seq-filter (lambda (ov) (overlay-get ov 'pi-coding-agent-diff-overlay))
                           (overlays-at (point)))))
      (should ovs)
      ;; Should have diff-added face for background
      (should (memq 'diff-added
                    (mapcar (lambda (ov) (overlay-get ov 'face)) ovs))))))

(ert-deftest pi-coding-agent-test-edit-tool-diff-uses-overlays ()
  "Edit tool output should use overlays for diff highlighting."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--tool-args-cache (make-hash-table :test 'equal))
    (puthash "test" '(:path "/tmp/test.py") pi-coding-agent--tool-args-cache)
    (pi-coding-agent--display-tool-start "edit" '(:path "/tmp/test.py"))
    ;; Use actual pi format
    (let ((diff-content "+ 1     def foo():\n- 2     def bar():"))
      (pi-coding-agent--display-tool-end
       "edit"
       '(:path "/tmp/test.py")
       '((:type "text" :text "Edit successful"))
       (list :diff diff-content)
       nil))
    ;; Should have diff overlays
    (let ((diff-ovs (seq-filter (lambda (ov) (overlay-get ov 'pi-coding-agent-diff-overlay))
                                (overlays-in (point-min) (point-max)))))
      (should (> (length diff-ovs) 0)))
    ;; Check for added line overlay
    (goto-char (point-min))
    (search-forward "+ 1" nil t)
    (let ((ovs (seq-filter (lambda (ov) (overlay-get ov 'pi-coding-agent-diff-overlay))
                           (overlays-at (match-beginning 0)))))
      (should (memq 'diff-indicator-added
                    (mapcar (lambda (ov) (overlay-get ov 'face)) ovs))))))

;;; File Navigation (visit-file)

(ert-deftest pi-coding-agent-test-diff-line-at-point-added ()
  "Should parse line number from added diff line."
  (with-temp-buffer
    (insert "+ 7     added line content")
    (goto-char (point-min))
    (should (= 7 (pi-coding-agent--diff-line-at-point)))))

(ert-deftest pi-coding-agent-test-diff-line-at-point-removed ()
  "Should parse line number from removed diff line."
  (with-temp-buffer
    (insert "-12     removed line content")
    (goto-char (point-min))
    (should (= 12 (pi-coding-agent--diff-line-at-point)))))

(ert-deftest pi-coding-agent-test-diff-line-at-point-context ()
  "Should return nil for context lines (no +/-)."
  (with-temp-buffer
    (insert "  7     context line")
    (goto-char (point-min))
    (should-not (pi-coding-agent--diff-line-at-point))))

(ert-deftest pi-coding-agent-test-diff-line-at-point-mid-line ()
  "Should work when point is anywhere on the line."
  (with-temp-buffer
    (insert "+ 42    some code here")
    (goto-char 15)  ;; Middle of line
    (should (= 42 (pi-coding-agent--diff-line-at-point)))))

(ert-deftest pi-coding-agent-test-code-block-line-at-point-first-line ()
  "Should return 1 for first line of code block content."
  (with-temp-buffer
    (insert "```python\nfirst line\nsecond line\n```")
    (goto-char (point-min))
    (forward-line 1)  ;; On "first line"
    (should (= 1 (pi-coding-agent--code-block-line-at-point)))))

(ert-deftest pi-coding-agent-test-code-block-line-at-point-third-line ()
  "Should return correct line for later lines."
  (with-temp-buffer
    (insert "```python\nline one\nline two\nline three\n```")
    (goto-char (point-min))
    (forward-line 3)  ;; On "line three"
    (should (= 3 (pi-coding-agent--code-block-line-at-point)))))

(ert-deftest pi-coding-agent-test-code-block-line-at-point-on-fence ()
  "Should return nil when on the fence line itself."
  (with-temp-buffer
    (insert "```python\ncontent\n```")
    (goto-char (point-min))  ;; On opening fence
    (should-not (pi-coding-agent--code-block-line-at-point))))

(ert-deftest pi-coding-agent-test-code-block-line-at-point-no-fence ()
  "Should return nil when not in a code block."
  (with-temp-buffer
    (insert "just plain text\nno fences here")
    (goto-char (point-min))
    (should-not (pi-coding-agent--code-block-line-at-point))))

(ert-deftest pi-coding-agent-test-tool-overlay-stores-path ()
  "Tool overlay should store the file path for navigation."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "read" '(:path "/tmp/test.py"))
    ;; The pending overlay should have the path
    (should pi-coding-agent--pending-tool-overlay)
    (should (equal "/tmp/test.py"
                   (overlay-get pi-coding-agent--pending-tool-overlay
                                'pi-coding-agent-tool-path)))))

(ert-deftest pi-coding-agent-test-tool-overlay-stores-path-after-finalize ()
  "Tool overlay should preserve path after finalization."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "edit" '(:path "/tmp/edit.el"))
    (pi-coding-agent--display-tool-end "edit" '(:path "/tmp/edit.el")
                          '((:type "text" :text "done"))
                          '(:diff "+ 1     new line")
                          nil)
    ;; Find the finalized overlay
    (goto-char (point-min))
    (let ((ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block))
                        (overlays-in (point-min) (point-max)))))
      (should ov)
      (should (equal "/tmp/edit.el" (overlay-get ov 'pi-coding-agent-tool-path))))))

(ert-deftest pi-coding-agent-test-tool-overlay-stores-offset ()
  "Tool overlay should store read offset for line calculation."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "read" '(:path "/tmp/file.py" :offset 50))
    (pi-coding-agent--display-tool-end "read" '(:path "/tmp/file.py" :offset 50)
                          '((:type "text" :text "content"))
                          nil nil)
    ;; Find the finalized overlay
    (let ((ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block))
                        (overlays-in (point-min) (point-max)))))
      (should ov)
      (should (= 50 (overlay-get ov 'pi-coding-agent-tool-offset))))))

(ert-deftest pi-coding-agent-test-tool-overlay-offset-defaults-nil ()
  "Tool overlay offset should be nil when not specified."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "read" '(:path "/tmp/file.py"))
    (pi-coding-agent--display-tool-end "read" '(:path "/tmp/file.py")
                          '((:type "text" :text "content"))
                          nil nil)
    (let ((ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block))
                        (overlays-in (point-min) (point-max)))))
      (should ov)
      (should-not (overlay-get ov 'pi-coding-agent-tool-offset)))))

(ert-deftest pi-coding-agent-test-visit-file-from-edit-diff ()
  "visit-file should navigate to correct line from edit diff."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "edit" '(:path "/tmp/test.el"))
    (pi-coding-agent--display-tool-end "edit" '(:path "/tmp/test.el")
                          '((:type "text" :text "done"))
                          '(:diff "+ 42    (defun foo ())")
                          nil)
    ;; Move to the diff line
    (goto-char (point-min))
    (search-forward "+ 42")
    ;; Mock find-file-other-window to create a buffer with enough lines
    ;; Note: goto-char and forward-line are bytecode ops, can't be mocked
    (let (opened-file)
      (cl-letf (((symbol-function 'find-file-other-window)
                 (lambda (path)
                   (setq opened-file path)
                   ;; Create a fake buffer with enough lines and switch to it
                   (set-buffer (get-buffer-create "*test-target*"))
                   (erase-buffer)
                   (dotimes (_ 100) (insert "line\n"))
                   (goto-char (point-min))
                   (current-buffer))))
        (pi-coding-agent-visit-file))
      (should (equal "/tmp/test.el" opened-file))
      ;; Check we're on line 42 (line-number-at-pos is 1-indexed)
      (should (= 42 (line-number-at-pos)))
      (ignore-errors (kill-buffer "*test-target*")))))

(ert-deftest pi-coding-agent-test-visit-file-no-path-errors ()
  "visit-file should error when not on a tool block with path."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t))
      (insert "Just some text, no tool block"))
    (goto-char (point-min))
    (should-error (pi-coding-agent-visit-file) :type 'user-error)))

(ert-deftest pi-coding-agent-test-visit-file-read-with-offset ()
  "visit-file should use offset for read tool line calculation."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "read" '(:path "/tmp/big.py" :offset 100))
    (pi-coding-agent--display-tool-end "read" '(:path "/tmp/big.py" :offset 100)
                          '((:type "text" :text "line 100\nline 101\nline 102"))
                          nil nil)
    ;; Move to line 2 of the code block content (should be file line 101)
    (goto-char (point-min))
    (search-forward "```")
    (forward-line 2)  ;; On "line 101"
    ;; Note: goto-char and forward-line are bytecode ops, can't be mocked
    (cl-letf (((symbol-function 'find-file-other-window)
               (lambda (_path)
                 ;; Create a fake buffer with enough lines and switch to it
                 (set-buffer (get-buffer-create "*test-target*"))
                 (erase-buffer)
                 (dotimes (_ 200) (insert "line\n"))
                 (goto-char (point-min))
                 (current-buffer))))
      (pi-coding-agent-visit-file))
    ;; Line 2 in code block + offset 100 - 1 = 101
    (should (= 101 (line-number-at-pos)))
    (ignore-errors (kill-buffer "*test-target*"))))

(ert-deftest pi-coding-agent-test-visit-file-accounts-for-stripped-blank-lines ()
  "visit-file navigates to correct original line even when blank lines stripped.
File has blanks at lines 3,5. Pressing RET on 'line06' should go to line 6."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; File: line01, line02, (blank), line04, (blank), line06...line15
    (pi-coding-agent--display-tool-start "read" '(:path "/tmp/test.txt"))
    (pi-coding-agent--display-tool-end "read" '(:path "/tmp/test.txt")
                          '((:type "text" :text "line01\nline02\n\nline04\n\nline06\nline07\nline08\nline09\nline10\nline11\nline12\nline13\nline14\nline15"))
                          nil nil)
    (goto-char (point-min))
    (search-forward "line06")
    (beginning-of-line)
    ;; Note: forward-line can't be mocked in byte-compiled code (inlined)
    ;; Instead, check line-number-at-pos after visit-file completes
    (cl-letf (((symbol-function 'find-file-other-window)
               (lambda (_path)
                 (set-buffer (get-buffer-create "*test-target*"))
                 (erase-buffer)
                 (dotimes (_ 20) (insert "line\n"))
                 (goto-char (point-min))
                 (current-buffer))))
      (pi-coding-agent-visit-file))
    ;; Should navigate to line 6, not line 4 (2 blank lines stripped)
    (should (= 6 (line-number-at-pos)))
    (ignore-errors (kill-buffer "*test-target*"))))

;;; Visual Line Truncation Tests

(ert-deftest pi-coding-agent-test-truncate-visual-lines-simple ()
  "Truncation with short lines counts each as one visual line."
  (let ((content "line1\nline2\nline3\nline4\nline5"))
    ;; Width 80, max 3 visual lines -> should get first 3 lines
    (let ((result (pi-coding-agent--truncate-to-visual-lines content 3 80)))
      (should (equal (plist-get result :content) "line1\nline2\nline3"))
      (should (= (plist-get result :visual-lines) 3))
      (should (= (plist-get result :hidden-lines) 2)))))

(ert-deftest pi-coding-agent-test-truncate-visual-lines-wrapping ()
  "Long lines count as multiple visual lines based on width."
  ;; Create content where first line is 160 chars (2 visual lines at width 80)
  (let ((long-line (make-string 160 ?a))
        (short-line "short"))
    (let* ((content (concat long-line "\n" short-line))
           ;; Width 80, max 2 visual lines -> only first line fits (uses 2 visual lines)
           (result (pi-coding-agent--truncate-to-visual-lines content 2 80)))
      (should (equal (plist-get result :content) long-line))
      (should (= (plist-get result :visual-lines) 2))
      (should (= (plist-get result :hidden-lines) 1)))))

(ert-deftest pi-coding-agent-test-truncate-visual-lines-byte-limit ()
  "Truncation respects byte limit in addition to visual lines."
  (let ((pi-coding-agent-preview-max-bytes 50))
    ;; Each line is 10 chars, 5 lines = 54 bytes with newlines
    (let* ((content "aaaaaaaaaa\nbbbbbbbbbb\ncccccccccc\ndddddddddd\neeeeeeeeee")
           (result (pi-coding-agent--truncate-to-visual-lines content 100 80)))
      ;; Should stop before exceeding 50 bytes
      (should (< (length (plist-get result :content)) 50))
      (should (> (plist-get result :hidden-lines) 0)))))

(ert-deftest pi-coding-agent-test-truncate-visual-lines-no-truncation-needed ()
  "Content under limits returns unchanged."
  (let ((content "short\ncontent"))
    (let ((result (pi-coding-agent--truncate-to-visual-lines content 100 80)))
      (should (equal (plist-get result :content) content))
      (should (= (plist-get result :hidden-lines) 0)))))

(ert-deftest pi-coding-agent-test-truncate-visual-lines-trailing-newline ()
  "Trailing newlines don't create phantom hidden lines."
  ;; Content with trailing newline - should count as 3 lines, not 4
  (let ((content "line1\nline2\nline3\n"))
    (let ((result (pi-coding-agent--truncate-to-visual-lines content 5 80)))
      (should (= (plist-get result :hidden-lines) 0))
      (should (= (plist-get result :visual-lines) 3)))))

(ert-deftest pi-coding-agent-test-truncate-visual-lines-single-long-line ()
  "A single line exceeding visual line limit gets truncated.
Regression test: single lines without newlines should still be capped.
If we ask for 5 visual lines at width 80, we should get ~400 chars max."
  ;; 1000 char single line with no newlines - at width 80, this is ~13 visual lines
  (let ((content (make-string 1000 ?x)))
    (let ((result (pi-coding-agent--truncate-to-visual-lines content 5 80)))
      ;; Should be capped to ~5 visual lines worth of content
      ;; 5 * 80 = 400 chars max
      (should (<= (length (plist-get result :content)) 400))
      (should (<= (plist-get result :visual-lines) 5)))))

(ert-deftest pi-coding-agent-test-truncate-visual-lines-single-line-byte-limit ()
  "A single line exceeding byte limit gets truncated.
Regression test: single lines should respect byte limit even with no newlines."
  (let ((pi-coding-agent-preview-max-bytes 100))
    ;; 500 char single line - exceeds 100 byte limit
    (let* ((content (make-string 500 ?y))
           (result (pi-coding-agent--truncate-to-visual-lines content 100 80)))
      ;; Should respect byte limit
      (should (<= (length (plist-get result :content)) 100)))))

(ert-deftest pi-coding-agent-test-tool-output-truncates-long-lines ()
  "Tool output preview accounts for visual line wrapping."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Create output with one very long line (200 chars) that wraps to ~3 visual lines
    ;; Plus 3 more short lines. At width 80 and 5 preview lines limit:
    ;; Line 1: 200 chars = 3 visual lines
    ;; Line 2-3: 2 visual lines
    ;; Total: 5 visual lines (at limit), line 4 should be hidden
    (let* ((long-line (make-string 200 ?x))
           (output (concat long-line "\nline2\nline3\nline4")))
      (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 80)))
        (pi-coding-agent--display-tool-end "bash" '(:command "test")
                              `((:type "text" :text ,output))
                              nil nil))
      ;; Long line should be present
      (should (string-match-p "xxxx" (buffer-string)))
      ;; line4 should be hidden (in "more lines" section)
      (should (string-match-p "more lines" (buffer-string))))))

(ert-deftest pi-coding-agent-test-tab-bound-to-toggle-tool-section ()
  "TAB is bound to pi-coding-agent-toggle-tool-section for tool block handling."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "output"))
                          nil nil)
    ;; Verify we have a tool block with overlay
    (should (string-match-p "\\$ ls" (buffer-string)))
    (goto-char (point-min))
    (should (pi-coding-agent--find-tool-block-bounds))
    ;; pi-coding-agent-toggle-tool-section should be bound to TAB and <tab>
    (should (eq (lookup-key pi-coding-agent-chat-mode-map (kbd "TAB")) 'pi-coding-agent-toggle-tool-section))
    (should (eq (lookup-key pi-coding-agent-chat-mode-map (kbd "<tab>")) 'pi-coding-agent-toggle-tool-section))))

(ert-deftest pi-coding-agent-test-tool-error-indicated ()
  "Tool error is indicated in output."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-end "bash" '(:command "false")
                          '((:type "text" :text "error message"))
                          nil t)
    (should (string-match-p "error" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-success-not-error ()
  "Tool with isError :false should not show error indicator."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "test"))
    (pi-coding-agent--display-tool-end "bash" nil
                          '((:type "text" :text "success output"))
                          nil :false)
    ;; Should have output, success face, no [error]
    (should (string-match-p "success output" (buffer-string)))
    (let ((ov (car (overlays-at (point-min)))))
      (should (eq (overlay-get ov 'face) 'pi-coding-agent-tool-block-success)))
    (should-not (string-match-p "\\[error\\]" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-output-survives-message-render ()
  "Tool output should not be clobbered by subsequent message rendering."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Simulate: message -> tool -> message sequence
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    (pi-coding-agent--handle-display-event 
     '(:type "message_update" 
       :assistantMessageEvent (:type "text_delta" :delta "Running")))
    (pi-coding-agent--handle-display-event '(:type "message_end"))
    
    (pi-coding-agent--handle-display-event 
     '(:type "tool_execution_start" :toolName "bash" :args (:command "ls")))
    (pi-coding-agent--handle-display-event 
     '(:type "tool_execution_end" :toolName "bash"
       :result (:content ((:type "text" :text "file1\nfile2")))))
    
    ;; Second message should NOT clobber tool output
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    (pi-coding-agent--handle-display-event 
     '(:type "message_update"
       :assistantMessageEvent (:type "text_delta" :delta "Done")))
    (pi-coding-agent--handle-display-event '(:type "message_end"))
    
    ;; Tool output must still be present
    (should (string-match-p "file1" (buffer-string)))
    (should (string-match-p "file2" (buffer-string)))
    (should (string-match-p "\\$ ls" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-handler-handles-tool-start ()
  "Display handler processes tool_execution_start events."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((event (list :type "tool_execution_start"
                       :toolName "bash"
                       :args (list :command "echo hello"))))
      (pi-coding-agent--handle-display-event event)
      (should (string-match-p "echo hello" (buffer-string))))))

(ert-deftest pi-coding-agent-test-display-handler-handles-tool-end ()
  "Display handler processes tool_execution_end events."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((event (list :type "tool_execution_end"
                       :toolName "bash"
                       :args (list :command "ls")
                       :result (list :content '((:type "text" :text "output")))
                       :isError nil)))
      (pi-coding-agent--handle-display-event event)
      (should (string-match-p "output" (buffer-string))))))

(ert-deftest pi-coding-agent-test-display-handler-handles-tool-update ()
  "Display handler processes tool_execution_update events."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; First, start the tool
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start"
       :toolName "bash"
       :toolCallId "test-id"
       :args (:command "long-running")))
    ;; Then send an update with partial result (same structure as tool result)
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_update"
       :toolCallId "test-id"
       :partialResult (:content [(:type "text" :text "streaming output line 1")])))
    ;; Should show partial content
    (should (string-match-p "streaming output" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-update-shows-rolling-tail ()
  "Tool updates show rolling tail of output, truncated to visual lines."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Start the tool
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start"
       :toolName "bash"
       :toolCallId "test-id"
       :args (:command "verbose-command")))
    ;; Send update with many lines (more than preview limit)
    (let ((many-lines (mapconcat (lambda (n) (format "line%d" n))
                                 (number-sequence 1 20)
                                 "\n")))
      (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 80)))
        (pi-coding-agent--handle-display-event
         `(:type "tool_execution_update"
           :toolCallId "test-id"
           :partialResult (:content [(:type "text" :text ,many-lines)])))))
    ;; Should show indicator that earlier output is hidden
    (should (string-match-p "earlier output" (buffer-string)))
    ;; Should show last few lines
    (should (string-match-p "line20" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-update-truncates-single-long-line ()
  "Tool updates truncate single lines that exceed visual line limit.
Regression test: streaming output with no newlines should still be capped."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Start the tool
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start"
       :toolName "bash"
       :toolCallId "test-id"
       :args (:command "json-dump")))
    ;; Send update with a single very long line (1000 chars, ~13 visual lines at width 80)
    ;; Preview limit is 5 lines, so this should be truncated
    (let ((long-line (make-string 1000 ?x)))
      (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 80)))
        (pi-coding-agent--handle-display-event
         `(:type "tool_execution_update"
           :toolCallId "test-id"
           :partialResult (:content [(:type "text" :text ,long-line)])))))
    ;; Output should be truncated - 5 visual lines * 80 chars = 400 chars max
    (let ((buffer-content (buffer-string)))
      ;; Should NOT contain all 1000 x's
      (should-not (string-match-p (make-string 500 ?x) buffer-content))
      ;; Should contain truncation indicator
      (should (string-match-p "earlier output\\|truncated" buffer-content)))))

(ert-deftest pi-coding-agent-test-get-tail-lines-basic ()
  "Get-tail-lines returns last N lines correctly."
  (let ((content "line1\nline2\nline3\nline4\nline5"))
    ;; Get last 2 lines
    (let ((result (pi-coding-agent--get-tail-lines content 2)))
      (should (equal (car result) "line4\nline5"))
      (should (eq (cdr result) t)))  ; has hidden content
    ;; Get last 5 lines (all)
    (let ((result (pi-coding-agent--get-tail-lines content 5)))
      (should (equal (car result) content))
      (should (eq (cdr result) nil)))  ; no hidden content
    ;; Get last 10 lines (more than available)
    (let ((result (pi-coding-agent--get-tail-lines content 10)))
      (should (equal (car result) content))
      (should (eq (cdr result) nil)))))

(ert-deftest pi-coding-agent-test-get-tail-lines-trailing-newlines ()
  "Get-tail-lines handles trailing newlines correctly."
  ;; Content with trailing newlines - the function preserves them
  (let ((content "line1\nline2\nline3\n\n"))
    (let ((result (pi-coding-agent--get-tail-lines content 2)))
      ;; Gets last 2 lines including trailing newlines
      (should (equal (car result) "line2\nline3\n\n"))
      (should (eq (cdr result) t)))))

(ert-deftest pi-coding-agent-test-get-tail-lines-empty ()
  "Get-tail-lines handles empty content."
  (let ((result (pi-coding-agent--get-tail-lines "" 5)))
    (should (equal (car result) ""))
    (should (eq (cdr result) nil))))

(ert-deftest pi-coding-agent-test-get-tail-lines-single-line ()
  "Get-tail-lines handles single line content."
  (let ((result (pi-coding-agent--get-tail-lines "just one line" 5)))
    (should (equal (car result) "just one line"))
    (should (eq (cdr result) nil))))

(ert-deftest pi-coding-agent-test-extract-text-from-content-single-block ()
  "Extract-text-from-content handles single text block efficiently."
  (let ((blocks [(:type "text" :text "hello world")]))
    (should (equal (pi-coding-agent--extract-text-from-content blocks)
                   "hello world"))))

(ert-deftest pi-coding-agent-test-extract-text-from-content-multiple-blocks ()
  "Extract-text-from-content concatenates multiple text blocks."
  (let ((blocks [(:type "text" :text "hello ")
                 (:type "image" :data "...")
                 (:type "text" :text "world")]))
    (should (equal (pi-coding-agent--extract-text-from-content blocks)
                   "hello world"))))

(ert-deftest pi-coding-agent-test-extract-text-from-content-empty ()
  "Extract-text-from-content handles empty input."
  (should (equal (pi-coding-agent--extract-text-from-content []) ""))
  (should (equal (pi-coding-agent--extract-text-from-content nil) "")))

(ert-deftest pi-coding-agent-test-extract-last-usage-from-messages ()
  "Extract-last-usage finds usage from last assistant message."
  (let ((messages
         [(:role "user" :content "Hi")
          (:role "assistant"
           :usage (:input 100 :output 50 :cacheRead 0 :cacheWrite 20)
           :stopReason "endTurn")
          (:role "user" :content "More")
          (:role "assistant"
           :usage (:input 200 :output 80 :cacheRead 20 :cacheWrite 30)
           :stopReason "endTurn")]))
    (let ((usage (pi-coding-agent--extract-last-usage messages)))
      (should (equal (plist-get usage :input) 200))
      (should (equal (plist-get usage :output) 80)))))

(ert-deftest pi-coding-agent-test-extract-last-usage-skips-aborted ()
  "Extract-last-usage skips aborted messages."
  (let ((messages
         [(:role "assistant"
           :usage (:input 100 :output 50 :cacheRead 0 :cacheWrite 0)
           :stopReason "endTurn")
          (:role "assistant"
           :usage (:input 0 :output 0 :cacheRead 0 :cacheWrite 0)
           :stopReason "aborted")]))
    (let ((usage (pi-coding-agent--extract-last-usage messages)))
      ;; Should return the non-aborted message's usage
      (should (equal (plist-get usage :input) 100)))))

(ert-deftest pi-coding-agent-test-extract-last-usage-empty ()
  "Extract-last-usage handles empty/nil input."
  (should-not (pi-coding-agent--extract-last-usage []))
  (should-not (pi-coding-agent--extract-last-usage nil)))

(ert-deftest pi-coding-agent-test-extract-last-usage-no-assistant ()
  "Extract-last-usage returns nil when no assistant messages."
  (let ((messages [(:role "user" :content "Hi")]))
    (should-not (pi-coding-agent--extract-last-usage messages))))

(ert-deftest pi-coding-agent-test-tool-update-replaced-by-end ()
  "Tool update content is replaced by final result on tool_execution_end."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_start"
       :toolName "bash"
       :toolCallId "test-id"
       :args (:command "test")))
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_update"
       :toolCallId "test-id"
       :partialResult (:content [(:type "text" :text "partial streaming")])))
    ;; Partial content should be present
    (should (string-match-p "partial streaming" (buffer-string)))
    ;; Now end the tool
    (pi-coding-agent--handle-display-event
     '(:type "tool_execution_end"
       :toolName "bash"
       :toolCallId "test-id"
       :result (:content ((:type "text" :text "final output")))
       :isError nil))
    ;; Streaming content should be replaced
    (should-not (string-match-p "partial streaming" (buffer-string)))
    (should (string-match-p "final output" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-update-preserves-multiline-command-header ()
  "Tool updates preserve command headers that span multiple lines.
Commands with embedded newlines should not have any lines deleted."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((multiline-cmd "echo 'line1'\necho 'line2'"))
      (pi-coding-agent--display-tool-start "bash" `(:command ,multiline-cmd))
      ;; Both lines of header should be present
      (should (string-match-p "echo 'line1'" (buffer-string)))
      (should (string-match-p "echo 'line2'" (buffer-string)))
      ;; Update with streaming content
      (pi-coding-agent--display-tool-update
       '(:content [(:type "text" :text "output from command")]))
      ;; Header should still be intact
      (should (string-match-p "echo 'line1'" (buffer-string)))
      (should (string-match-p "echo 'line2'" (buffer-string)))
      (should (string-match-p "output from command" (buffer-string))))))

(ert-deftest pi-coding-agent-test-tool-end-preserves-multiline-command-header ()
  "Tool end preserves command headers that span multiple lines."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((multiline-cmd "echo 'first'\necho 'second'\necho 'third'"))
      (pi-coding-agent--display-tool-start "bash" `(:command ,multiline-cmd))
      ;; Stream some content first
      (pi-coding-agent--display-tool-update
       '(:content [(:type "text" :text "streaming...")]))
      ;; Then end the tool
      (pi-coding-agent--display-tool-end "bash" `(:command ,multiline-cmd)
                            '((:type "text" :text "final output")) nil nil)
      ;; All three lines of the header should be intact
      (should (string-match-p "echo 'first'" (buffer-string)))
      (should (string-match-p "echo 'second'" (buffer-string)))
      (should (string-match-p "echo 'third'" (buffer-string)))
      (should (string-match-p "final output" (buffer-string))))))

(ert-deftest pi-coding-agent-test-display-handler-handles-thinking-delta ()
  "Display handler processes thinking_delta events."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_delta" :delta "Analyzing...")))
    (should (string-match-p "Analyzing..." (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-compaction-result-shows-header-tokens-summary ()
  "pi-coding-agent--display-compaction-result shows header, token count, and summary."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-compaction-result 50000 "Key points from discussion.")
    ;; Should have Compaction header
    (should (string-match-p "Compaction" (buffer-string)))
    ;; Should show formatted tokens
    (should (string-match-p "50,000 tokens" (buffer-string)))
    ;; Should show summary
    (should (string-match-p "Key points" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-compaction-result-with-timestamp ()
  "pi-coding-agent--display-compaction-result includes timestamp when provided."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((timestamp (seconds-to-time 1704067200))) ; 2024-01-01 00:00 UTC
      (pi-coding-agent--display-compaction-result 30000 "Summary text." timestamp))
    ;; Should have timestamp in header (format depends on locale, check for time marker)
    (should (string-match-p "Compaction" (buffer-string)))
    (should (string-match-p "30,000 tokens" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-compaction-result-shows-markdown ()
  "pi-coding-agent--display-compaction-result displays markdown summary as-is."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-compaction-result 10000 "**Bold** and `code`")
    ;; Markdown stays as markdown
    (should (string-match-p "\\*\\*Bold\\*\\*" (buffer-string)))
    (should (string-match-p "`code`" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-handler-handles-auto-compaction-start ()
  "Display handler processes auto_compaction_start events."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "auto_compaction_start" :reason "threshold"))
    ;; Status should change to compacting
    (should (eq pi-coding-agent--status 'compacting))))

(ert-deftest pi-coding-agent-test-display-handler-handles-auto-compaction-end ()
  "Display handler processes auto_compaction_end with successful result."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Set up initial usage
    (setq pi-coding-agent--last-usage '(:input 5000 :output 1000))
    ;; Simulate compaction end
    (pi-coding-agent--handle-display-event
     '(:type "auto_compaction_end"
       :aborted nil
       :result (:summary "Context was compacted."
                :tokensBefore 50000
                :timestamp 1704067200000)))
    ;; Usage should be reset
    (should (null pi-coding-agent--last-usage))
    ;; Should display compaction info
    (should (string-match-p "Compaction" (buffer-string)))
    (should (string-match-p "50,000" (buffer-string)))))

(ert-deftest pi-coding-agent-test-display-handler-handles-auto-compaction-aborted ()
  "Display handler processes auto_compaction_end when aborted."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--status 'compacting)
    (setq pi-coding-agent--last-usage '(:input 5000 :output 1000))
    (pi-coding-agent--handle-display-event
     '(:type "auto_compaction_end" :aborted t :result nil))
    ;; Status should return to idle
    (should (eq pi-coding-agent--status 'idle))
    ;; Usage should NOT be reset on abort
    (should pi-coding-agent--last-usage)))

(ert-deftest pi-coding-agent-test-thinking-rendered-as-blockquote ()
  "Thinking content renders as markdown blockquote."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    ;; Thinking lifecycle: start -> delta -> end
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_start")))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_delta" :delta "Let me analyze this.")))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_end" :content "Let me analyze this.")))
    ;; Then regular text
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "text_delta" :delta "Here is my answer.")))
    ;; Complete the message (triggers rendering)
    (pi-coding-agent--handle-display-event '(:type "message_end" :message (:role "assistant")))
    ;; After rendering, thinking should be in a blockquote (> prefix)
    (goto-char (point-min))
    (should (search-forward "> Let me analyze this." nil t))
    ;; Regular text should be outside the blockquote
    (should (search-forward "Here is my answer." nil t))
    ;; Should NOT have code fence markers
    (goto-char (point-min))
    (should-not (search-forward "```thinking" nil t))))

(ert-deftest pi-coding-agent-test-thinking-blockquote-has-face ()
  "Thinking blockquote has markdown-blockquote-face after font-lock."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t))
      (insert "> Some thinking here.\n"))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "Some thinking")
    ;; Verify markdown-blockquote-face is applied (may be in a list with other faces)
    (let ((face (get-text-property (point) 'face)))
      (should (or (eq face 'markdown-blockquote-face)
                  (and (listp face) (memq 'markdown-blockquote-face face)))))))

(ert-deftest pi-coding-agent-test-thinking-multiline-blockquote ()
  "Multi-line thinking content has > prefix on each line."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--handle-display-event '(:type "agent_start"))
    (pi-coding-agent--handle-display-event '(:type "message_start"))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_start")))
    ;; Multi-line thinking with newline in delta
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_delta" :delta "First line.\nSecond line.")))
    (pi-coding-agent--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_end" :content "")))
    ;; Each line should have > prefix
    (goto-char (point-min))
    (should (search-forward "> First line." nil t))
    (should (search-forward "> Second line." nil t))))

(ert-deftest pi-coding-agent-test-blockquote-has-wrap-prefix ()
  "Blockquotes have wrap-prefix for continuation lines after font-lock."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t))
      (insert "> Some blockquote content.\n"))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "Some blockquote")
    (should (get-text-property (point) 'wrap-prefix))))

(ert-deftest pi-coding-agent-test-read-tool-gets-syntax-highlighting ()
  "Read tool output gets syntax highlighting based on file path.
The toolCallId is used to correlate start/end events since args
are only present in tool_execution_start, not tool_execution_end."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Start event has args with path
    (pi-coding-agent--handle-display-event
     (list :type "tool_execution_start"
           :toolCallId "call_123"
           :toolName "read"
           :args (list :path "example.py")))
    ;; End event does NOT have args (matches real pi behavior)
    (pi-coding-agent--handle-display-event
     (list :type "tool_execution_end"
           :toolCallId "call_123"
           :toolName "read"
           :result (list :content '((:type "text" :text "def hello():\n    pass")))
           :isError nil))
    ;; Should have python markdown code fence
    (should (string-match-p "```python" (buffer-string)))))

(ert-deftest pi-coding-agent-test-write-tool-gets-syntax-highlighting ()
  "Write tool displays content from args with syntax highlighting.
The content to display comes from args, not from the result
which is just a success message."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Start event has args with path and content
    (pi-coding-agent--handle-display-event
     (list :type "tool_execution_start"
           :toolCallId "call_456"
           :toolName "write"
           :args (list :path "example.rs"
                       :content "fn main() {\n    println!(\"Hello\");\n}")))
    ;; End event has only success message in result
    (pi-coding-agent--handle-display-event
     (list :type "tool_execution_end"
           :toolCallId "call_456"
           :toolName "write"
           :result (list :content '((:type "text" :text "Successfully wrote 42 bytes")))
           :isError nil))
    ;; Should have rust markdown code fence (from args content, not result)
    (should (string-match-p "```rust" (buffer-string)))
    ;; Should show the actual code, not the success message
    (should (string-match-p "fn main" (buffer-string)))))

;;; Transient Menu

(ert-deftest pi-coding-agent-test-transient-bound-to-key ()
  "C-c C-p is bound to pi-coding-agent-menu in input mode."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should (eq (key-binding (kbd "C-c C-p")) 'pi-coding-agent-menu))))

;;; Chat Navigation

(ert-deftest pi-coding-agent-test-chat-has-navigation-keys ()
  "Chat mode has n/p for navigation and TAB for folding."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (should (eq (key-binding "n") 'pi-coding-agent-next-message))
    (should (eq (key-binding "p") 'pi-coding-agent-previous-message))
    (should (eq (key-binding (kbd "TAB")) 'pi-coding-agent-toggle-tool-section))))

;;; Message Queuing

(ert-deftest pi-coding-agent-test-queue-steering-when-streaming-sends-steer ()
  "Queue steering sends steer RPC command when agent is streaming."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-steer*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-steer-input*"))
        (sent-command nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Please stop and focus on X")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc cmd _cb) (setq sent-command cmd))))
              (pi-coding-agent-queue-steering))
            ;; Should send steer command
            (should sent-command)
            (should (equal (plist-get sent-command :type) "steer"))
            (should (equal (plist-get sent-command :message) "Please stop and focus on X"))
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-queue-followup-uses-local-queue ()
  "Queue follow-up adds to local queue, no RPC sent."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-followup*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-followup-input*"))
        (rpc-called nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf)
            (setq pi-coding-agent--followup-queue nil))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "After you're done, also do Y")
            (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc _cmd _cb) (setq rpc-called t))))
              (pi-coding-agent-queue-followup))
            ;; Should NOT call RPC
            (should-not rpc-called)
            ;; Should add to local queue
            (with-current-buffer chat-buf
              (should (member "After you're done, also do Y" pi-coding-agent--followup-queue)))
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-queue-steering-when-idle-refuses ()
  "Queue steering refuses when agent is idle (nothing to interrupt)."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-steer-idle*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-steer-idle-input*"))
        (sent-anything nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Do something")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt)
                       (lambda (_) (setq sent-anything t)))
                      ((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc _cmd _cb) (setq sent-anything t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "nothing\\|idle\\|C-c C-c" (downcase fmt)))
                           (setq message-shown t)))))
              (pi-coding-agent-queue-steering))
            ;; Should NOT send anything
            (should-not sent-anything)
            ;; Should show message about using C-c C-c instead
            (should message-shown)
            ;; Input should be preserved (not accepted)
            (should (equal (buffer-string) "Do something"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-queue-followup-when-idle-sends-prompt ()
  "Queue follow-up sends as normal prompt when agent is idle."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-followup-idle*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-followup-idle-input*"))
        (sent-prompt nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Do something else")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt)
                       (lambda (text) (setq sent-prompt text)))
                      ((symbol-function 'pi-coding-agent--spinner-start) #'ignore))
              (pi-coding-agent-queue-followup))
            ;; Should send as normal prompt
            (should (equal sent-prompt "Do something else"))
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-queue-steering-adds-to-history ()
  "Queue steering adds input to history."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-hist*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-hist-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (setq pi-coding-agent--input-ring (make-ring 10))
            (insert "History test message")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--rpc-async) #'ignore))
              (pi-coding-agent-queue-steering))
            ;; Should be in history
            (should (not (ring-empty-p pi-coding-agent--input-ring)))
            (should (equal (ring-ref pi-coding-agent--input-ring 0) "History test message"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-queue-empty-input-does-nothing ()
  "Queue with empty input does nothing."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-empty*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-empty-input*"))
        (command-sent nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            ;; Empty input (just whitespace)
            (insert "   \n  ")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc _cmd _cb) (setq command-sent t))))
              (pi-coding-agent-queue-steering))
            ;; Should not send anything
            (should-not command-sent)))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-steering-shows-minibuffer-message ()
  "Steering shows feedback in minibuffer but is NOT displayed locally.
Unlike normal sends, steering waits for pi's echo to display at the
correct position in the conversation."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-display*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-display-input*"))
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "My steering message")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--rpc-async) #'ignore)
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "steering\\|sent" (downcase fmt)))
                           (setq message-shown t)))))
              (pi-coding-agent-queue-steering)))
          ;; Should show minibuffer message
          (should message-shown)
          ;; Steering is NOT displayed locally - will be displayed when pi echoes it back
          (with-current-buffer chat-buf
            (should-not (string-match-p "My steering message" (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-input-mode-has-queue-keybindings ()
  "Input mode has C-c C-s for steering (C-c C-c handles follow-up)."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should (eq (key-binding (kbd "C-c C-s")) 'pi-coding-agent-queue-steering))
    ;; C-c C-c handles follow-up when streaming (no separate C-c C-q)
    (should (eq (key-binding (kbd "C-c C-c")) 'pi-coding-agent-send))))

(ert-deftest pi-coding-agent-test-queue-handles-rpc-error ()
  "Queue handles RPC error response by showing message to user."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-error*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-error-input*"))
        (captured-callback nil)
        (error-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Test message")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc _cmd cb) (setq captured-callback cb)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (when (and fmt (string-match-p "error\\|fail" (downcase fmt)))
                           (setq error-shown t)))))
              (pi-coding-agent-queue-steering)
              ;; Simulate error response from RPC
              (when captured-callback
                (funcall captured-callback '(:success :false :error "Queue limit reached")))))
          ;; Should have shown an error message
          (should error-shown))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-queue-with-dead-process-shows-error ()
  "Queue with dead process shows error message."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-dead*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-dead-input*"))
        (error-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Test message")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () nil))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (when (and fmt (string-match-p "process\\|unavailable\\|error" (downcase fmt)))
                           (setq error-shown t)))))
              (pi-coding-agent-queue-steering)))
          ;; Should have shown an error about unavailable process
          (should error-shown))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-send-when-idle-sends-literal-commands ()
  "C-c C-c when idle sends commands literally (pi expands)."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-send-slash*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-send-slash-input*"))
        (sent-prompt nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "/greet world")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt)
                       (lambda (text) (setq sent-prompt text)))
                      ((symbol-function 'pi-coding-agent--spinner-start) #'ignore))
              (pi-coding-agent-send))
            ;; Should send literal command (pi handles expansion)
            (should (equal sent-prompt "/greet world"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

;; Note: pi-coding-agent-test-send-queues-locally-while-streaming covers this case

(ert-deftest pi-coding-agent-test-steering-when-idle-refuses ()
  "C-c C-s when idle shows message and does nothing."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-steer-idle*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-steer-idle-input*"))
        (send-called nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Steer message")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt)
                       (lambda (_) (setq send-called t)))
                      ((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc _cmd _cb) (setq send-called t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "idle\\|nothing\\|use" (downcase fmt)))
                           (setq message-shown t)))))
              (pi-coding-agent-queue-steering))
            ;; Should NOT have sent anything
            (should-not send-called)
            ;; Should have shown a message
            (should message-shown)
            ;; Input should be preserved
            (should (equal (buffer-string) "Steer message"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-message-start-user-echo-ignored-when-displayed-locally ()
  "message_start role=user is ignored when we already displayed the same message locally.
Uses local-user-message to track what we displayed for comparison."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state nil)
          ;; Simulate that we displayed this message locally (normal send)
          (pi-coding-agent--local-user-message "Same message")
          (initial-content (buffer-string)))
      ;; Simulate receiving message_start for a user message (pi echoing back same text)
      (pi-coding-agent--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Same message")]
                   :timestamp 1704067200000)))
      ;; Buffer should be unchanged - pi's echo matches local display, so skip
      (should (equal (buffer-string) initial-content))
      (should-not (string-match-p "Same message" (buffer-string)))
      ;; Variable should be cleared
      (should-not pi-coding-agent--local-user-message))))

(ert-deftest pi-coding-agent-test-message-start-user-displayed-when-different ()
  "message_start role=user IS displayed when pi's text differs from local.
+This handles slash command expansion: user types '/greet', pi sends 'Hello!'."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state nil)
          (pi-coding-agent--local-user-message "/greet world")
          (initial-content (buffer-string)))
      (pi-coding-agent--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Hello world!")]
                   :timestamp 1704067200000)))
      ;; Should be displayed since text differs (expanded template)
      (should (string-match-p "Hello world!" (buffer-string)))
      (should-not pi-coding-agent--local-user-message))))

(ert-deftest pi-coding-agent-test-message-start-user-skipped-when-template-equals-command ()
  "Edge case: if template expands to exactly the command text, we skip display.
This is rare but possible - the local display is already correct."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state nil)
          (pi-coding-agent--local-user-message "/echo hello")
          (initial-content (buffer-string)))
      (pi-coding-agent--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "/echo hello")]
                   :timestamp 1704067200000)))
      ;; Should NOT be displayed - text matches what we displayed locally
      (should (equal (buffer-string) initial-content))
      (should-not pi-coding-agent--local-user-message))))

(ert-deftest pi-coding-agent-test-message-start-user-displayed-when-not-local ()
  "message_start role=user IS displayed when local-user-message is nil (steering case).
Steering messages are not displayed locally - they're displayed from the echo."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state nil)
          ;; Variable is nil - no locally displayed message pending
          (pi-coding-agent--local-user-message nil))
      ;; Simulate receiving message_start for a steering message
      (pi-coding-agent--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Steering message here")]
                   :timestamp 1704067200000)))
      ;; Should be displayed since local-user-message was nil
      (should (string-match-p "Steering message here" (buffer-string)))
      ;; Variable should still be nil
      (should-not pi-coding-agent--local-user-message))))

(ert-deftest pi-coding-agent-test-steering-display-not-interleaved ()
  "Steering message during streaming appears cleanly, not interleaved.
When user sends steering while assistant is streaming, the sequence is:
1. Current assistant output ends cleanly
2. User steering message with header appears
3. New assistant turn begins with its own header

This tests for a bug where user message header and assistant text got
mixed together like:
  > ...count from 1 to
  You · 01:32
  ===========
  STOP NOW
  10 slowly...  <- WRONG: '10 slowly' is assistant text after user msg!"
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state nil)
          (pi-coding-agent--local-user-message nil)
          (pi-coding-agent--assistant-header-shown nil))
      ;; Simulate initial prompt response - assistant starts streaming
      (pi-coding-agent--handle-display-event '(:type "agent_start"))
      (pi-coding-agent--handle-display-event
       '(:type "message_start" :message (:role "assistant")))
      ;; Stream some content
      (pi-coding-agent--handle-display-event
       '(:type "message_update"
         :assistantMessageEvent (:type "text_delta" :delta "Counting: 1, 2, 3, ")))
      (pi-coding-agent--handle-display-event
       '(:type "message_update"
         :assistantMessageEvent (:type "text_delta" :delta "4, 5, 6, ")))

      ;; Now user sends steering - this comes as message_start with role=user
      ;; (steering messages are displayed from pi's echo, not locally)
      (pi-coding-agent--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "STOP-MARKER")]
                   :timestamp 1704067200000)))

      ;; Assistant continues with new turn after steering
      (setq pi-coding-agent--assistant-header-shown nil)  ; Reset for new turn
      (pi-coding-agent--handle-display-event '(:type "agent_start"))
      (pi-coding-agent--handle-display-event
       '(:type "message_start" :message (:role "assistant")))
      (pi-coding-agent--handle-display-event
       '(:type "message_update"
         :assistantMessageEvent (:type "text_delta" :delta "OK, stopping.")))
      (pi-coding-agent--handle-display-event '(:type "agent_end"))

      ;; Now verify the buffer structure
      (let ((content (buffer-string)))
        ;; All expected content should be present
        (should (string-match-p "Counting: 1, 2, 3, 4, 5, 6," content))
        (should (string-match-p "STOP-MARKER" content))
        (should (string-match-p "OK, stopping" content))

        ;; Find positions to verify order
        (let ((first-assistant-pos (string-match "Counting:" content))
              (steering-pos (string-match "STOP-MARKER" content))
              (second-response-pos (string-match "OK, stopping" content)))
          ;; Order must be: first-assistant < steering < second-response
          (should (< first-assistant-pos steering-pos))
          (should (< steering-pos second-response-pos))

          ;; "You" header must appear before the steering message
          (let ((you-header-pos (string-match "You" content)))
            (should you-header-pos)
            (should (< you-header-pos steering-pos)))

          ;; After STOP-MARKER, we should see "Assistant" header before second response
          (let* ((after-steering (substring content steering-pos))
                 (assistant-after-steering (string-match "Assistant" after-steering)))
            (should assistant-after-steering)))

        ;; Verify NO interleaving: counting text should NOT appear after STOP-MARKER
        (let* ((steering-pos (string-match "STOP-MARKER" content))
               (after-steering (substring content (+ steering-pos (length "STOP-MARKER")))))
          ;; Should NOT see counting continuation after the steering message
          (should-not (string-match-p "^[0-9]" (string-trim-left after-steering)))
          (should-not (string-match-p "^, [0-9]" (string-trim-left after-steering))))))))

(ert-deftest pi-coding-agent-test-local-user-message-tracks-display ()
  "The local-user-message variable tracks locally displayed messages.
- Normal send stores the text
- message_start role=user clears it to nil
- Steering doesn't set it (displayed from echo)
- agent_end clears it to nil"
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-echo-flag*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-echo-flag-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf)
            ;; Variable starts as nil
            (should-not pi-coding-agent--local-user-message))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "First message")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt) #'ignore)
                      ((symbol-function 'pi-coding-agent--spinner-start) #'ignore))
              (pi-coding-agent-send)))
          ;; After normal send, variable should store the message text
          (with-current-buffer chat-buf
            (should (equal pi-coding-agent--local-user-message "First message"))
            ;; Simulate pi echo - variable clears to nil
            (pi-coding-agent--handle-display-event
             '(:type "message_start"
               :message (:role "user" :content [(:type "text" :text "First message")])))
            (should-not pi-coding-agent--local-user-message)
            ;; Now simulate steering (doesn't set it)
            (setq pi-coding-agent--status 'streaming))
          (with-current-buffer input-buf
            (erase-buffer)
            (insert "Steer this")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--rpc-async) #'ignore))
              (pi-coding-agent-queue-steering)))
          ;; Variable still nil (steering doesn't set it)
          (with-current-buffer chat-buf
            (should-not pi-coding-agent--local-user-message)
            ;; agent_end clears to nil (in case of edge cases)
            (setq pi-coding-agent--local-user-message "test")  ; Simulate weird state
            (pi-coding-agent--display-agent-end)
            (should-not pi-coding-agent--local-user-message)))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-normal-send-not-duplicated-by-message-start ()
  "Normal send should not be duplicated when message_start arrives.
When user sends a message normally (idle state), we display it immediately.
When pi echoes it back via message_start, we should NOT display it again."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-no-dup*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-no-dup-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Hello pi")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt) #'ignore)
                      ((symbol-function 'pi-coding-agent--spinner-start) #'ignore))
              (pi-coding-agent-send)))
          ;; Now simulate pi echoing the message back via message_start
          (with-current-buffer chat-buf
            (pi-coding-agent--handle-display-event
             '(:type "message_start"
               :message (:role "user"
                         :content [(:type "text" :text "Hello pi")]
                         :timestamp 1704067200000)))
            ;; Count occurrences of "Hello pi" - should be exactly 1
            (let ((count 0)
                  (start 0))
              (while (string-match "Hello pi" (buffer-string) start)
                (setq count (1+ count))
                (setq start (match-end 0)))
              (should (= count 1)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-agent-end-sends-queued-followup ()
  "agent_end pops from followup queue and sends as normal prompt.
When user queues a follow-up (busy state), it goes to local queue.
On agent_end, we pop from queue and send (which displays the message)."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-agent-end-queue*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-agent-end-queue-input*"))
        (sent-prompt nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf)
            (setq pi-coding-agent--followup-queue nil)
            ;; Simulate some prior content
            (let ((inhibit-read-only t))
              (insert "Assistant\n=========\nSome response...\n")))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "My follow-up question")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t)))
              (pi-coding-agent-send)))  ; Adds to local queue when streaming
          ;; Message should be in queue, not in chat yet
          (with-current-buffer chat-buf
            (should (equal pi-coding-agent--followup-queue '("My follow-up question")))
            (should-not (string-match-p "My follow-up question" (buffer-string))))
          ;; Now simulate agent_end - this should pop queue and send
          (with-current-buffer chat-buf
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt)
                       (lambda (text) (setq sent-prompt text)))
                      ((symbol-function 'pi-coding-agent--spinner-start) #'ignore)
                      ((symbol-function 'pi-coding-agent--spinner-stop) #'ignore)
                      ((symbol-function 'pi-coding-agent--fontify-timer-stop) #'ignore)
                      ((symbol-function 'pi-coding-agent--refresh-header) #'ignore))
              (pi-coding-agent--handle-display-event '(:type "agent_end")))
            ;; Queue should be empty now
            (should (null pi-coding-agent--followup-queue))
            ;; Should have sent the queued message
            (should (equal sent-prompt "My follow-up question"))
            ;; Message should now be displayed in chat
            (should (string-match-p "My follow-up question" (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-followup-queue-fifo-order ()
  "Multiple follow-ups are processed in FIFO order."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-fifo*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-fifo-input*"))
        (sent-prompts nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf)
            (setq pi-coding-agent--followup-queue nil))
          ;; Queue three messages while busy
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (dolist (msg '("First message" "Second message" "Third message"))
              (erase-buffer)
              (insert msg)
              (pi-coding-agent-send)))
          ;; All three should be in queue
          (with-current-buffer chat-buf
            (should (= 3 (length pi-coding-agent--followup-queue))))
          ;; Now simulate agent_end three times, capturing what gets sent
          (with-current-buffer chat-buf
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt)
                       (lambda (text) (push text sent-prompts)))
                      ((symbol-function 'pi-coding-agent--spinner-start) #'ignore)
                      ((symbol-function 'pi-coding-agent--spinner-stop) #'ignore)
                      ((symbol-function 'pi-coding-agent--fontify-timer-stop) #'ignore)
                      ((symbol-function 'pi-coding-agent--refresh-header) #'ignore))
              ;; First agent_end
              (pi-coding-agent--handle-display-event '(:type "agent_end"))
              ;; Second agent_end
              (pi-coding-agent--handle-display-event '(:type "agent_end"))
              ;; Third agent_end
              (pi-coding-agent--handle-display-event '(:type "agent_end")))
            ;; Should have sent all three in FIFO order (sent-prompts is reversed)
            (should (equal (reverse sent-prompts)
                           '("First message" "Second message" "Third message")))
            ;; Queue should be empty
            (should (null pi-coding-agent--followup-queue))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-steering-displayed-from-echo ()
  "Steering is NOT displayed locally - it's displayed when pi echoes it back.
This ensures steering appears at the correct position in the conversation
(after the current assistant output completes)."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-steer-echo*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-steer-echo-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf)
            (let ((inhibit-read-only t))
              (insert "Assistant\n=========\nWorking on something...\n")))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Stop and do something else")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--rpc-async) #'ignore))
              (pi-coding-agent-queue-steering)))
          ;; Steering is NOT displayed when sent (unlike normal sends)
          (with-current-buffer chat-buf
            (should-not (string-match-p "Stop and do something else" (buffer-string)))
            ;; local-user-message should still be nil (steering doesn't set it)
            (should-not pi-coding-agent--local-user-message))
          ;; Simulate pi echoing the steering message back via message_start
          (with-current-buffer chat-buf
            (pi-coding-agent--handle-display-event
             '(:type "message_start"
               :message (:role "user"
                         :content [(:type "text" :text "Stop and do something else")]
                         :timestamp 1704067200000)))
            ;; NOW it should be displayed (from the echo)
            (should (string-match-p "Stop and do something else" (buffer-string)))
            ;; Should be displayed exactly once
            (let ((count 0)
                  (start 0))
              (while (string-match "Stop and do something else" (buffer-string) start)
                (setq count (1+ count))
                (setq start (match-end 0)))
              (should (= count 1)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-steering-echo-followed-by-assistant-shows-header ()
  "After steering message, the next assistant message shows its header.
This tests the full flow: steering echo resets the flag, then the next
message_start role=assistant displays the 'Assistant' header."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--local-user-message nil)
          ;; Simulate that first assistant header was already shown
          (pi-coding-agent--assistant-header-shown t))
      ;; First, some assistant content is already in the buffer
      (let ((inhibit-read-only t))
        (insert "Assistant\n=========\nPrevious response...\n"))
      ;; Simulate steering message echo from pi
      (pi-coding-agent--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Stop it")]
                   :timestamp 1704067200000)))
      ;; Steering message should be displayed
      (should (string-match-p "Stop it" (buffer-string)))
      ;; Flag should be reset
      (should-not pi-coding-agent--assistant-header-shown)
      ;; Now simulate the assistant's response to steering
      (pi-coding-agent--handle-display-event
       '(:type "message_start"
         :message (:role "assistant")))
      ;; Now we should see TWO "Assistant" headers in the buffer
      (let ((count 0)
            (start 0)
            (content (buffer-string)))
        (while (string-match "Assistant\n=+" content start)
          (setq count (1+ count))
          (setq start (match-end 0)))
        (should (= count 2))))))

(provide 'pi-coding-agent-test)
;;; pi-coding-agent-test.el ends here

(ert-deftest pi-coding-agent-test-tool-toggle-expands-content ()
  "Toggle button expands collapsed tool output."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Initially collapsed - should have "... (N more lines)"
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    (should-not (string-match-p "L10" (buffer-string)))
    ;; Find and click the button
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (pi-coding-agent-toggle-tool-section)
    ;; Now should show all lines
    (should (string-match-p "L10" (buffer-string)))
    (should (string-match-p "\\[-\\]" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-toggle-collapses-content ()
  "Toggle button collapses expanded tool output."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Expand first
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (pi-coding-agent-toggle-tool-section)
    (should (string-match-p "L10" (buffer-string)))
    ;; Now collapse
    (goto-char (point-min))
    (search-forward "[-]" nil t)
    (backward-char 1)
    (pi-coding-agent-toggle-tool-section)
    ;; Should be collapsed again
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    (should-not (string-match-p "L10" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-toggle-expands-with-highlighting ()
  "Expanded tool output has syntax highlighting applied.
Regression test for issue #32: JIT font-lock doesn't fontify expanded content."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Create a read tool with Python content (>10 lines to trigger collapse)
    ;; The 'def' keyword is on line 11, hidden initially
    (pi-coding-agent--display-tool-start "read" '(:path "test.py"))
    (pi-coding-agent--display-tool-end "read" '(:path "test.py")
                          '((:type "text" :text "line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\ndef hello():\n    return 42"))
                          nil nil)
    ;; Initially collapsed - 'def' is hidden
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    (should-not (string-match-p "def hello" (buffer-string)))
    ;; Expand
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (pi-coding-agent-toggle-tool-section)
    ;; Now 'def' should be visible
    (should (string-match-p "def hello" (buffer-string)))
    ;; Find 'def' keyword and check for syntax highlighting
    (goto-char (point-min))
    (search-forward "def" nil t)
    (let ((face (get-text-property (match-beginning 0) 'face)))
      ;; Should have font-lock-keyword-face from python-mode
      (should (or (eq face 'font-lock-keyword-face)
                  (and (listp face) (memq 'font-lock-keyword-face face)))))))

(ert-deftest pi-coding-agent-test-tab-works-from-anywhere-in-block ()
  "TAB toggles tool output from any position within the block."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Go to the header line (not the button)
    (goto-char (point-min))
    (search-forward "$ ls" nil t)
    (beginning-of-line)
    ;; TAB should still expand
    (pi-coding-agent-toggle-tool-section)
    (should (string-match-p "L10" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tab-preserves-cursor-position ()
  "TAB toggle doesn't jump cursor unnecessarily."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Go to L3 line
    (goto-char (point-min))
    (search-forward "L3" nil t)
    (beginning-of-line)
    (let ((line-content (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
      ;; Expand
      (pi-coding-agent-toggle-tool-section)
      ;; Should still be on a line starting with L
      (should (string-match-p "^L[0-9]" 
                              (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position)))))))

(ert-deftest pi-coding-agent-test-toggle-preserves-window-scroll ()
  "Toggle collapse/expand should preserve window scroll when viewing content before tool.
When window shows content BEFORE the tool block, toggle should not jump away."
  (let ((buf (generate-new-buffer "*test-toggle-scroll*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (pi-coding-agent-chat-mode)
            ;; Add some content before the tool block
            (let ((inhibit-read-only t))
              (insert "Header line 1\nHeader line 2\nHeader line 3\n\n"))
            ;; Create tool output with many lines
            (pi-coding-agent--display-tool-start "read" '(:path "test.el"))
            (pi-coding-agent--display-tool-end
             "read" nil
             `((:type "text"
                :text ,(mapconcat (lambda (n) (format "Line %03d content" n))
                                  (number-sequence 1 50) "\n")))
             nil nil))
          ;; Display buffer in a window so we can test scroll
          (let ((win (display-buffer buf)))
            (when win
              (with-selected-window win
                ;; Position window at the header (before tool block)
                (goto-char (point-min))
                (recenter 0)
                (let ((start-before (window-start win)))
                  ;; Expand the tool content
                  (search-forward "..." nil t)
                  (pi-coding-agent-toggle-tool-section)
                  ;; Window should not have jumped
                  (should (= (window-start win) start-before))
                  ;; Now collapse
                  (search-forward "[-]" nil t)
                  (pi-coding-agent-toggle-tool-section)
                  ;; Window should still be at same position
                  (should (= (window-start win) start-before)))))))
      (kill-buffer buf))))

(ert-deftest pi-coding-agent-test-format-fork-message ()
  "Fork message formatted with index and preview."
  (let ((msg '(:entryId "abc-123" :text "Hello world, this is a test")))
    ;; With index
    (let ((result (pi-coding-agent--format-fork-message msg 2)))
      (should (string-match-p "2:" result))
      (should (string-match-p "Hello world" result)))
    ;; Without index
    (let ((result (pi-coding-agent--format-fork-message msg)))
      (should (string-match-p "Hello world" result))
      (should-not (string-match-p ":" result)))))

(ert-deftest pi-coding-agent-test-fork-detects-empty-messages-vector ()
  "Fork correctly detects empty messages vector from RPC.
JSON arrays are parsed as vectors, and (null []) is nil, not t.
The fork code must use seq-empty-p or length check."
  (let ((rpc-called nil)
        (message-shown nil))
    (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
              ((symbol-function 'pi-coding-agent--rpc-async)
               (lambda (_proc cmd cb)
                 (setq rpc-called t)
                 ;; Simulate response with empty vector (no messages to fork from)
                 (funcall cb '(:success t :data (:messages [])))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when (string-match-p "No messages" fmt)
                   (setq message-shown t)))))
      (pi-coding-agent-fork)
      (should rpc-called)
      ;; Should show "No messages to fork from", not call completing-read
      (should message-shown))))

(ert-deftest pi-coding-agent-test-format-fork-message-handles-nil-text ()
  "Format fork message handles nil text gracefully."
  (let ((msg '(:entryId "abc-123" :text nil)))
    ;; Should not error, should return something displayable
    (let ((result (pi-coding-agent--format-fork-message msg 1)))
      (should (stringp result)))))

(ert-deftest pi-coding-agent-test-load-session-history-uses-provided-buffer ()
  "load-session-history uses provided chat buffer, not current buffer context.
This ensures history loads correctly when callback runs in arbitrary context."
  (let* ((chat-buf (generate-new-buffer "*pi-coding-agent-chat:test-history/*"))
         (rpc-callback nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode))
          ;; Mock RPC to capture callback
          (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc _cmd cb) (setq rpc-callback cb))))
            ;; Call with explicit buffer
            (pi-coding-agent--load-session-history 'mock-proc nil chat-buf))
          ;; Simulate callback from different buffer context
          (with-temp-buffer
            (funcall rpc-callback
                     '(:success t :data (:messages [(:role "user" :content "test")]))))
          ;; Chat buffer should have been updated (has startup header)
          (with-current-buffer chat-buf
            (should (string-match-p "C-c C-c" (buffer-string)))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pi-coding-agent-test-load-session-history-restores-context-usage ()
  "load-session-history sets last-usage from final assistant message.
This ensures context percentage is displayed correctly after resume/fork.
Regression test: context showed 0% after resuming because usage wasn't extracted."
  (let* ((chat-buf (generate-new-buffer "*pi-coding-agent-chat:test-usage/*"))
         (rpc-callback nil)
         ;; Simulate a Fortnite tips conversation with usage data
         (mock-messages
          [(:role "user"
            :content "How do I get better at Fortnite?"
            :timestamp 1704067200000)
           (:role "assistant"
            :content [(:type "text" :text "Here are some tips to improve at Fortnite:\n\n1. **Practice building** - Building is essential\n2. **Land at busy spots** - More combat practice\n3. **Watch pro players** - Learn advanced techniques")]
            :usage (:input 150 :output 80 :cacheRead 0 :cacheWrite 50)
            :stopReason "endTurn"
            :timestamp 1704067260000)
           (:role "user"
            :content "What about aiming?"
            :timestamp 1704067320000)
           (:role "assistant"
            :content [(:type "text" :text "For better aim:\n\n1. Lower your sensitivity\n2. Use aim trainers\n3. Practice tracking moving targets")]
            :usage (:input 280 :output 120 :cacheRead 50 :cacheWrite 30)
            :stopReason "endTurn"
            :timestamp 1704067380000)]))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            ;; Ensure usage starts as nil
            (should-not pi-coding-agent--last-usage))
          ;; Mock RPC to capture callback
          (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc _cmd cb) (setq rpc-callback cb))))
            (pi-coding-agent--load-session-history 'mock-proc nil chat-buf))
          ;; Simulate RPC response with our mock messages
          (funcall rpc-callback
                   `(:success t :data (:messages ,mock-messages)))
          ;; Verify last-usage was extracted from final assistant message
          (with-current-buffer chat-buf
            (should pi-coding-agent--last-usage)
            (should (equal (plist-get pi-coding-agent--last-usage :input) 280))
            (should (equal (plist-get pi-coding-agent--last-usage :output) 120))
            (should (equal (plist-get pi-coding-agent--last-usage :cacheRead) 50))
            (should (equal (plist-get pi-coding-agent--last-usage :cacheWrite) 30))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pi-coding-agent-test-load-session-history-skips-aborted-usage ()
  "load-session-history skips aborted messages when extracting usage.
Aborted messages may have incomplete usage data."
  (let* ((chat-buf (generate-new-buffer "*pi-coding-agent-chat:test-aborted/*"))
         (rpc-callback nil)
         ;; Session where last message was aborted
         (mock-messages
          [(:role "user"
            :content "Tell me about Fortnite"
            :timestamp 1704067200000)
           (:role "assistant"
            :content [(:type "text" :text "Fortnite is a battle royale game...")]
            :usage (:input 100 :output 50 :cacheRead 0 :cacheWrite 20)
            :stopReason "endTurn"
            :timestamp 1704067260000)
           (:role "user"
            :content "More details"
            :timestamp 1704067320000)
           (:role "assistant"
            :content [(:type "text" :text "Well...")]
            :usage (:input 0 :output 0 :cacheRead 0 :cacheWrite 0)
            :stopReason "aborted"
            :timestamp 1704067380000)]))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode))
          (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc _cmd cb) (setq rpc-callback cb))))
            (pi-coding-agent--load-session-history 'mock-proc nil chat-buf))
          (funcall rpc-callback
                   `(:success t :data (:messages ,mock-messages)))
          ;; Should use the non-aborted assistant message's usage
          (with-current-buffer chat-buf
            (should pi-coding-agent--last-usage)
            (should (equal (plist-get pi-coding-agent--last-usage :input) 100))
            (should (equal (plist-get pi-coding-agent--last-usage :output) 50))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pi-coding-agent-test-session-dir-name ()
  "Session directory name derived from project path."
  (should (equal (pi-coding-agent--session-dir-name "/home/daniel/co/pi-coding-agent")
                 "--home-daniel-co-pi-coding-agent--"))
  (should (equal (pi-coding-agent--session-dir-name "/tmp/test")
                 "--tmp-test--")))

(ert-deftest pi-coding-agent-test-list-sessions-sorted-by-mtime ()
  "Sessions are sorted by modification time, most recent first.
Regression test for #25: sessions were sorted by filename (creation time)
and then re-sorted alphabetically by completing-read."
  (let* ((temp-base (make-temp-file "pi-coding-agent-sessions-" t))
         (session-dir (expand-file-name "--test-project--" temp-base))
         ;; Create files with names that would sort differently alphabetically
         (old-file (expand-file-name "2024-01-01_10-00-00.jsonl" session-dir))
         (new-file (expand-file-name "2024-01-01_09-00-00.jsonl" session-dir)))
    (unwind-protect
        (progn
          (make-directory session-dir t)
          (let* ((now (current-time))
                 (old-time (time-subtract now (seconds-to-time 10)))
                 (new-time (time-subtract now (seconds-to-time 5))))
            ;; Create "old" file first
            (with-temp-file old-file (insert "{}"))
            (set-file-times old-file old-time)
            ;; Create "new" file second (more recent mtime despite earlier filename)
            (with-temp-file new-file (insert "{}"))
            (set-file-times new-file new-time))
          ;; Directly call directory-files and sort logic to test sorting
          (let* ((files (directory-files session-dir t "\\.jsonl$"))
                 (sorted (sort files
                               (lambda (a b)
                                 (time-less-p
                                  (file-attribute-modification-time (file-attributes b))
                                  (file-attribute-modification-time (file-attributes a)))))))
            ;; new-file should be first (most recent mtime)
            ;; even though "09-00-00" < "10-00-00" alphabetically
            (should (equal (length sorted) 2))
            (should (string-suffix-p "09-00-00.jsonl" (car sorted)))))
      ;; Cleanup
      (delete-directory temp-base t))))

(ert-deftest pi-coding-agent-test-session-metadata-extracts-first-message ()
  "pi-coding-agent--session-metadata extracts first user message text."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n")
            (insert "{\"type\":\"message\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Hello world\"}]}}\n")
            (insert "{\"type\":\"message\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"Hi\"}]}}\n"))
          (let ((metadata (pi-coding-agent--session-metadata temp-file)))
            (should metadata)
            (should (equal (plist-get metadata :first-message) "Hello world"))
            (should (equal (plist-get metadata :message-count) 2))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-session-metadata-returns-nil-for-empty-file ()
  "pi-coding-agent--session-metadata returns nil for empty or invalid files."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          ;; Empty file
          (should (null (pi-coding-agent--session-metadata temp-file))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-session-metadata-handles-missing-first-message ()
  "pi-coding-agent--session-metadata handles session with only header."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n"))
          (let ((metadata (pi-coding-agent--session-metadata temp-file)))
            (should metadata)
            (should (null (plist-get metadata :first-message)))
            (should (equal (plist-get metadata :message-count) 0))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-session-metadata-extracts-session-name ()
  "pi-coding-agent--session-metadata extracts session name from session_info entry."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n")
            (insert "{\"type\":\"message\",\"id\":\"m1\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Hello\"}]}}\n")
            (insert "{\"type\":\"session_info\",\"id\":\"si1\",\"name\":\"Refactor auth module\"}\n"))
          (let ((metadata (pi-coding-agent--session-metadata temp-file)))
            (should metadata)
            (should (equal (plist-get metadata :session-name) "Refactor auth module"))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-session-metadata-uses-latest-session-name ()
  "pi-coding-agent--session-metadata uses the most recent session_info name."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n")
            (insert "{\"type\":\"session_info\",\"id\":\"si1\",\"name\":\"Old name\"}\n")
            (insert "{\"type\":\"message\",\"id\":\"m1\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Hello\"}]}}\n")
            (insert "{\"type\":\"session_info\",\"id\":\"si2\",\"name\":\"New name\"}\n"))
          (let ((metadata (pi-coding-agent--session-metadata temp-file)))
            (should metadata)
            (should (equal (plist-get metadata :session-name) "New name"))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-session-metadata-ignores-null-name ()
  "pi-coding-agent--session-metadata treats null name as cleared (no name)."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n")
            (insert "{\"type\":\"session_info\",\"id\":\"si1\",\"name\":\"My Session\"}\n")
            (insert "{\"type\":\"message\",\"id\":\"m1\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Hello\"}]}}\n")
            ;; User cleared the session name - null means no name
            (insert "{\"type\":\"session_info\",\"id\":\"si2\",\"name\":null}\n"))
          (let ((metadata (pi-coding-agent--session-metadata temp-file)))
            (should metadata)
            ;; Should be nil, not :null
            (should (null (plist-get metadata :session-name)))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-format-session-choice-fallback-on-cleared-name ()
  "pi-coding-agent--format-session-choice falls back to message when name cleared."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n")
            (insert "{\"type\":\"message\",\"id\":\"m1\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Hello world\"}]}}\n")
            (insert "{\"type\":\"session_info\",\"id\":\"si1\",\"name\":\"My Project\"}\n")
            ;; Name was cleared
            (insert "{\"type\":\"session_info\",\"id\":\"si2\",\"name\":null}\n"))
          (let ((choice (pi-coding-agent--format-session-choice temp-file)))
            ;; Should fall back to first message, not crash
            (should (string-match-p "Hello world" (car choice)))
            (should-not (string-match-p "My Project" (car choice)))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-format-session-choice-prefers-name ()
  "pi-coding-agent--format-session-choice uses session name when available."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n")
            (insert "{\"type\":\"message\",\"id\":\"m1\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Hello world\"}]}}\n")
            (insert "{\"type\":\"session_info\",\"id\":\"si1\",\"name\":\"My Project\"}\n"))
          (let ((choice (pi-coding-agent--format-session-choice temp-file)))
            ;; Should show session name, not first message
            (should (string-match-p "My Project" (car choice)))
            (should-not (string-match-p "Hello world" (car choice)))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-header-line-includes-session-name ()
  "pi-coding-agent--header-line-string includes session name when set."
  (let ((chat-buf (get-buffer-create "*pi-test-header-session-name*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (pi-coding-agent-chat-mode)
          (setq pi-coding-agent--state '(:model (:name "test-model") :thinking-level "high"))
          ;; Without session name
          (setq pi-coding-agent--session-name nil)
          (let ((header (pi-coding-agent--header-line-string)))
            (should-not (string-match-p "My Session" header)))
          ;; With session name
          (setq pi-coding-agent--session-name "My Session")
          (let ((header (pi-coding-agent--header-line-string)))
            (should (string-match-p "My Session" header))
            ;; Should have separator before session name
            (should (string-match-p "│" header))))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-header-line-truncates-long-session-name ()
  "pi-coding-agent--header-line-string truncates long session names."
  (let ((chat-buf (get-buffer-create "*pi-test-header-truncate*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (pi-coding-agent-chat-mode)
          (setq pi-coding-agent--state '(:model (:name "test-model")))
          ;; Set a very long session name (longer than 30 chars)
          (setq pi-coding-agent--session-name "This is a very long session name that should be truncated")
          (let ((header (pi-coding-agent--header-line-string)))
            ;; Should contain truncated version with ellipsis
            (should (string-match-p "This is a very long session" header))
            (should (string-match-p "…" header))
            ;; Should NOT contain the full name
            (should-not (string-match-p "truncated$" header))))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-set-session-name-empty-shows-current ()
  "pi-coding-agent-set-session-name with empty string shows current name."
  (let ((chat-buf (get-buffer-create "*pi-test-show-name*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--session-name "My Session"))
          ;; Capture message output
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (pi-coding-agent-set-session-name "")))
          ;; Should show current name, not change it
          (should (equal (buffer-local-value 'pi-coding-agent--session-name chat-buf)
                         "My Session"))
          (should (member "Pi: Session name: My Session" messages)))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-set-session-name-empty-no-name-shows-message ()
  "pi-coding-agent-set-session-name with empty string and no name shows message."
  (let ((chat-buf (get-buffer-create "*pi-test-no-name*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--session-name nil))
          ;; Capture message output
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (pi-coding-agent-set-session-name "")))
          (should (member "Pi: No session name set" messages)))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-set-session-name-no-process-errors ()
  "pi-coding-agent-set-session-name errors when no process is running."
  (let ((chat-buf (get-buffer-create "*pi-test-no-proc*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode))
          ;; Mock pi-coding-agent--get-process to return nil
          (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                     (lambda () nil)))
            (should-error
             (with-current-buffer chat-buf
               (pi-coding-agent-set-session-name "New Name"))
             :type 'user-error)))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-set-session-name-sends-rpc ()
  "pi-coding-agent-set-session-name sends correct RPC command."
  (let ((chat-buf (get-buffer-create "*pi-test-rpc*"))
        (pi-coding-agent--request-id-counter 0)
        (output-buffer (generate-new-buffer " *test-output*")))
    (unwind-protect
        (let ((fake-proc (start-process "cat" output-buffer "cat")))
          (unwind-protect
              (progn
                (with-current-buffer chat-buf
                  (pi-coding-agent-chat-mode))
                ;; Mock get-process and get-chat-buffer
                (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                           (lambda () fake-proc))
                          ((symbol-function 'pi-coding-agent--get-chat-buffer)
                           (lambda () chat-buf)))
                  (pi-coding-agent-set-session-name "Test Session"))
                ;; Wait for output
                (pi-coding-agent-test-wait-until
                 (lambda ()
                   (with-current-buffer output-buffer
                     (> (buffer-size) 0)))
                 1.0 0.05 fake-proc)
                ;; Verify JSON sent
                (with-current-buffer output-buffer
                  (let* ((sent (buffer-string))
                         (json (json-parse-string (string-trim sent) :object-type 'plist)))
                    (should (equal (plist-get json :type) "set_session_name"))
                    (should (equal (plist-get json :name) "Test Session")))))
            (delete-process fake-proc)))
      (kill-buffer output-buffer)
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-set-session-name-trims-whitespace ()
  "pi-coding-agent-set-session-name trims whitespace from name."
  (let ((chat-buf (get-buffer-create "*pi-test-trim*"))
        (pi-coding-agent--request-id-counter 0)
        (output-buffer (generate-new-buffer " *test-output*")))
    (unwind-protect
        (let ((fake-proc (start-process "cat" output-buffer "cat")))
          (unwind-protect
              (progn
                (with-current-buffer chat-buf
                  (pi-coding-agent-chat-mode))
                ;; Mock get-process and get-chat-buffer
                (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                           (lambda () fake-proc))
                          ((symbol-function 'pi-coding-agent--get-chat-buffer)
                           (lambda () chat-buf)))
                  (pi-coding-agent-set-session-name "  Trimmed Name  "))
                ;; Wait for output
                (pi-coding-agent-test-wait-until
                 (lambda ()
                   (with-current-buffer output-buffer
                     (> (buffer-size) 0)))
                 1.0 0.05 fake-proc)
                ;; Verify JSON sent has trimmed name
                (with-current-buffer output-buffer
                  (let* ((sent (buffer-string))
                         (json (json-parse-string (string-trim sent) :object-type 'plist)))
                    (should (equal (plist-get json :name) "Trimmed Name")))))
            (delete-process fake-proc)))
      (kill-buffer output-buffer)
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-set-session-name-whitespace-only-shows-current ()
  "pi-coding-agent-set-session-name with whitespace-only shows current name."
  (let ((chat-buf (get-buffer-create "*pi-test-ws*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--session-name "Existing Name"))
          ;; Capture message output
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (pi-coding-agent-set-session-name "   ")))  ; whitespace only
          ;; Should show current name, not try to set
          (should (equal (buffer-local-value 'pi-coding-agent--session-name chat-buf)
                         "Existing Name"))
          (should (member "Pi: Session name: Existing Name" messages)))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-set-session-name-rpc-failure-shows-error ()
  "pi-coding-agent-set-session-name shows error on RPC failure."
  (let ((chat-buf (get-buffer-create "*pi-test-fail*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--session-name "Old Name"))
          ;; Mock RPC to call callback with failure
          (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                     (lambda () 'fake-proc))
                    ((symbol-function 'pi-coding-agent--get-chat-buffer)
                     (lambda () chat-buf))
                    ((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc _cmd callback)
                       (funcall callback '(:success nil :error "test error"))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (pi-coding-agent-set-session-name "New Name")))
          ;; Name should NOT be updated
          (should (equal (buffer-local-value 'pi-coding-agent--session-name chat-buf)
                         "Old Name"))
          ;; Error message should be shown
          (should (member "Pi: Failed to set session name: test error" messages)))
      (kill-buffer chat-buf))))

;;; Input History

(ert-deftest pi-coding-agent-test-history-add-to-ring ()
  "pi-coding-agent--history-add adds input to ring."
  (let ((pi-coding-agent--input-ring nil))
    (pi-coding-agent--history-add "first")
    (pi-coding-agent--history-add "second")
    (should (equal (ring-ref (pi-coding-agent--input-ring) 0) "second"))
    (should (equal (ring-ref (pi-coding-agent--input-ring) 1) "first"))))

(ert-deftest pi-coding-agent-test-history-no-duplicate ()
  "pi-coding-agent--history-add skips duplicates of last entry."
  (let ((pi-coding-agent--input-ring nil))
    (pi-coding-agent--history-add "first")
    (pi-coding-agent--history-add "first")
    (should (= (ring-length (pi-coding-agent--input-ring)) 1))))

(ert-deftest pi-coding-agent-test-history-skip-empty ()
  "pi-coding-agent--history-add skips empty input."
  (let ((pi-coding-agent--input-ring nil))
    (pi-coding-agent--history-add "")
    (pi-coding-agent--history-add "   ")
    (should (ring-empty-p (pi-coding-agent--input-ring)))))

(ert-deftest pi-coding-agent-test-history-previous-input ()
  "pi-coding-agent-previous-input navigates backward through history."
  (let ((pi-coding-agent--input-ring nil))
    (pi-coding-agent--history-add "first")
    (pi-coding-agent--history-add "second")
    (with-temp-buffer
      (pi-coding-agent-input-mode)
      (insert "current")
      (pi-coding-agent-previous-input)
      (should (equal (buffer-string) "second"))
      (should (equal pi-coding-agent--input-saved "current"))
      (pi-coding-agent-previous-input)
      (should (equal (buffer-string) "first")))))

(ert-deftest pi-coding-agent-test-history-next-input ()
  "pi-coding-agent-next-input navigates forward and restores saved input."
  (let ((pi-coding-agent--input-ring nil))
    (pi-coding-agent--history-add "first")
    (pi-coding-agent--history-add "second")
    (with-temp-buffer
      (pi-coding-agent-input-mode)
      (insert "current")
      (pi-coding-agent-previous-input)
      (pi-coding-agent-previous-input)
      (should (equal (buffer-string) "first"))
      (pi-coding-agent-next-input)
      (should (equal (buffer-string) "second"))
      (pi-coding-agent-next-input)
      (should (equal (buffer-string) "current")))))

(ert-deftest pi-coding-agent-test-history-keys-bound ()
  "History keys are bound in pi-coding-agent-input-mode."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should (eq (key-binding (kbd "M-p")) 'pi-coding-agent-previous-input))
    (should (eq (key-binding (kbd "M-n")) 'pi-coding-agent-next-input))
    (should (eq (key-binding (kbd "C-r")) 'pi-coding-agent-history-isearch-backward))))

(ert-deftest pi-coding-agent-test-history-isolated-per-buffer ()
  "Input history is isolated per buffer, not shared globally.
Regression test for #27: history was shared across all sessions."
  (let ((buf1 (generate-new-buffer "*pi-coding-agent-input:project-a*"))
        (buf2 (generate-new-buffer "*pi-coding-agent-input:project-b*")))
    (unwind-protect
        (progn
          ;; Add history in buffer 1
          (with-current-buffer buf1
            (pi-coding-agent-input-mode)
            (pi-coding-agent--history-add "project-a-query"))
          ;; Add different history in buffer 2
          (with-current-buffer buf2
            (pi-coding-agent-input-mode)
            (pi-coding-agent--history-add "project-b-query"))
          ;; Buffer 1 should only see its own history
          (with-current-buffer buf1
            (should (= (ring-length (pi-coding-agent--input-ring)) 1))
            (should (equal (ring-ref (pi-coding-agent--input-ring) 0) "project-a-query")))
          ;; Buffer 2 should only see its own history
          (with-current-buffer buf2
            (should (= (ring-length (pi-coding-agent--input-ring)) 1))
            (should (equal (ring-ref (pi-coding-agent--input-ring) 0) "project-b-query"))))
      ;; Cleanup
      (kill-buffer buf1)
      (kill-buffer buf2))))

;;; History Isearch (C-r incremental search)

(ert-deftest pi-coding-agent-test-history-isearch-empty-history-errors ()
  "pi-coding-agent-history-isearch-backward errors with empty history."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should-error (pi-coding-agent-history-isearch-backward) :type 'user-error)))

(ert-deftest pi-coding-agent-test-history-isearch-saves-current-input ()
  "pi-coding-agent-history-isearch-backward saves current buffer content."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (pi-coding-agent--history-add "old command")
    (insert "my current input")
    ;; Mock isearch-backward to avoid actually starting isearch
    (cl-letf (((symbol-function 'isearch-backward) #'ignore))
      (pi-coding-agent-history-isearch-backward))
    (should (equal pi-coding-agent--history-isearch-saved-input "my current input"))))

(ert-deftest pi-coding-agent-test-history-isearch-sets-active-flag ()
  "pi-coding-agent-history-isearch-backward sets the active flag."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (pi-coding-agent--history-add "old command")
    (cl-letf (((symbol-function 'isearch-backward) #'ignore))
      (pi-coding-agent-history-isearch-backward))
    (should pi-coding-agent--history-isearch-active)))

(ert-deftest pi-coding-agent-test-history-isearch-end-restores-on-quit ()
  "pi-coding-agent--history-isearch-end restores input when isearch is quit."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (setq pi-coding-agent--history-isearch-active t)
    (setq pi-coding-agent--history-isearch-saved-input "original input")
    (erase-buffer)
    (insert "some history item")
    ;; Simulate isearch quit
    (let ((isearch-mode-end-hook-quit t))
      (pi-coding-agent--history-isearch-end))
    (should (equal (buffer-string) "original input"))
    (should-not pi-coding-agent--history-isearch-active)))

(ert-deftest pi-coding-agent-test-history-isearch-end-keeps-on-accept ()
  "pi-coding-agent--history-isearch-end keeps history item when accepted."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (setq pi-coding-agent--history-isearch-active t)
    (setq pi-coding-agent--history-isearch-saved-input "original input")
    (erase-buffer)
    (insert "chosen history item")
    ;; Simulate isearch accept (quit is nil)
    (let ((isearch-mode-end-hook-quit nil))
      (pi-coding-agent--history-isearch-end))
    (should (equal (buffer-string) "chosen history item"))
    (should-not pi-coding-agent--history-isearch-active)))

(ert-deftest pi-coding-agent-test-history-isearch-goto-index ()
  "pi-coding-agent--history-isearch-goto loads history item into buffer."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (pi-coding-agent--history-add "first")
    (pi-coding-agent--history-add "second")
    (pi-coding-agent--history-add "third")
    (insert "current")
    (pi-coding-agent--history-isearch-goto 1)  ; "second" (0=third, 1=second)
    (should (equal (buffer-string) "second"))
    (should (= pi-coding-agent--history-isearch-index 1))))

(ert-deftest pi-coding-agent-test-history-isearch-hook-added ()
  "isearch-mode-hook is set up in pi-coding-agent-input-mode."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should (memq 'pi-coding-agent--history-isearch-setup isearch-mode-hook))))

(ert-deftest pi-coding-agent-test-history-isearch-goto-nil-restores-saved ()
  "pi-coding-agent--history-isearch-goto with nil index restores saved input."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (pi-coding-agent--history-add "history item")
    (setq pi-coding-agent--history-isearch-saved-input "my original input")
    (insert "something else")
    (pi-coding-agent--history-isearch-goto nil)
    (should (equal (buffer-string) "my original input"))
    (should (null pi-coding-agent--history-isearch-index))))

(ert-deftest pi-coding-agent-test-history-isearch-goto-empty-saved-input ()
  "pi-coding-agent--history-isearch-goto with nil index and empty saved input."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (pi-coding-agent--history-add "history item")
    (setq pi-coding-agent--history-isearch-saved-input "")
    (insert "something else")
    (pi-coding-agent--history-isearch-goto nil)
    (should (equal (buffer-string) ""))
    (should (null pi-coding-agent--history-isearch-index))))

;;; Input Buffer Slash Completion

(ert-deftest pi-coding-agent-test-command-capf-returns-nil-without-slash ()
  "Completion returns nil when not after slash."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "hello")
    (should-not (pi-coding-agent--command-capf))))

(ert-deftest pi-coding-agent-test-command-capf-returns-nil-at-line-start ()
  "Completion returns nil when point is at beginning of line."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "/test")
    (goto-char (line-beginning-position))
    (should-not (pi-coding-agent--command-capf))))

(ert-deftest pi-coding-agent-test-command-capf-returns-completion-data ()
  "Completion returns data when after slash at start of buffer."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (setq pi-coding-agent--commands '((:name "test-cmd" :description "Test")))
    (insert "/te")
    (let ((result (pi-coding-agent--command-capf)))
      (should result)
      (should (= (nth 0 result) 2))  ; Start after /
      (should (= (nth 1 result) 4))  ; End at point
      (should (member "test-cmd" (nth 2 result))))))

(ert-deftest pi-coding-agent-test-command-capf-ignores-slash-on-later-lines ()
  "Completion ignores / on lines after the first (pi only expands at buffer start)."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (setq pi-coding-agent--commands '((:name "test-cmd" :description "Test")))
    (insert "Some context:\n/te")
    (should-not (pi-coding-agent--command-capf))))



(ert-deftest pi-coding-agent-test-send-prompt-sends-literal ()
  "pi-coding-agent--send-prompt sends text literally (no expansion).
Pi handles command expansion on the server side."
  (let* ((rpc-message nil)
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                   (lambda () fake-proc))
                  ((symbol-function 'pi-coding-agent--rpc-async)
                   (lambda (_proc msg _cb) (setq rpc-message msg))))
          (pi-coding-agent--send-prompt "/greet world")
          ;; Should send literal /greet world, NOT expanded
          (should (equal (plist-get rpc-message :message) "/greet world")))
      (delete-process fake-proc))))

(ert-deftest pi-coding-agent-test-format-session-stats ()
  "Format session stats returns readable string."
  (let ((stats '(:tokens (:input 50000 :output 10000 :total 60000)
                 :cost 0.45
                 :userMessages 5
                 :toolCalls 12)))
    (let ((result (pi-coding-agent--format-session-stats stats)))
      (should (string-match-p "50,000" result))  ; input tokens formatted
      (should (string-match-p "\\$0.45" result)) ; cost
      (should (string-match-p "5" result)))))    ; messages

(ert-deftest pi-coding-agent-test-header-line-shows-model ()
  "Header line displays current model."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model "claude-sonnet-4"))
    (let ((header (pi-coding-agent--header-line-string)))
      (should (string-match-p "sonnet-4" header)))))

(ert-deftest pi-coding-agent-test-header-line-shows-thinking ()
  "Header line displays thinking level."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model "claude-sonnet-4" :thinking-level "high"))
    (let ((header (pi-coding-agent--header-line-string)))
      (should (string-match-p "high" header)))))

(ert-deftest pi-coding-agent-test-header-line-shows-streaming-indicator ()
  "Header line shows spinner when streaming."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model "claude-sonnet-4"))
    (pi-coding-agent--spinner-start)
    (unwind-protect
        (let ((header (pi-coding-agent--header-line-string)))
          (should (string-match-p "[⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]" header)))
      (pi-coding-agent--spinner-stop))))

(ert-deftest pi-coding-agent-test-spinner-stop-with-explicit-buffer ()
  "Spinner stops correctly when buffer is passed explicitly.
Regression test for #24: spinner wouldn't stop if callback ran in
arbitrary buffer context (e.g., process sentinel)."
  (let* ((chat-buf (generate-new-buffer "*pi-coding-agent-chat:test-spinner/*"))
         (original-spinning pi-coding-agent--spinning-sessions))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (pi-coding-agent--spinner-start))
          ;; Verify spinner started
          (should (memq chat-buf pi-coding-agent--spinning-sessions))
          ;; Stop from different buffer context (simulating sentinel/callback)
          (with-temp-buffer
            ;; Without explicit buffer, this would fail to remove chat-buf
            (pi-coding-agent--spinner-stop chat-buf))
          ;; Verify spinner stopped
          (should-not (memq chat-buf pi-coding-agent--spinning-sessions)))
      ;; Cleanup
      (setq pi-coding-agent--spinning-sessions original-spinning)
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pi-coding-agent-test-format-tokens-compact ()
  "Tokens formatted compactly."
  (should (equal "500" (pi-coding-agent--format-tokens-compact 500)))
  (should (equal "5k" (pi-coding-agent--format-tokens-compact 5000)))
  (should (equal "50k" (pi-coding-agent--format-tokens-compact 50000)))
  (should (equal "1.2M" (pi-coding-agent--format-tokens-compact 1200000))))

(ert-deftest pi-coding-agent-test-input-mode-has-header-line ()
  "Input mode sets up header-line-format."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should header-line-format)))

(ert-deftest pi-coding-agent-test-header-line-handles-model-plist ()
  "Header line handles model as plist with :name."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model (:name "claude-sonnet-4" :id "model-123")))
    (let ((header (pi-coding-agent--header-line-string)))
      (should (string-match-p "sonnet-4" header)))))

(ert-deftest pi-coding-agent-test-menu-model-description-buffer-local ()
  "Menu model description uses buffer-local model."
  (let ((buf-a (generate-new-buffer "*pi-coding-agent-chat:model-a*"))
        (buf-b (generate-new-buffer "*pi-coding-agent-chat:model-b*")))
    (unwind-protect
        (let (desc-a desc-b)
          (with-current-buffer buf-a
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--state '(:model (:name "Alpha")))
            (setq desc-a (pi-coding-agent--menu-model-description)))
          (with-current-buffer buf-b
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--state '(:model (:name "Beta")))
            (setq desc-b (pi-coding-agent--menu-model-description)))
          (should (equal (list desc-a desc-b)
                         '("Model: Alpha" "Model: Beta"))))
      (mapc #'kill-buffer (list buf-a buf-b)))))

(ert-deftest pi-coding-agent-test-select-model-updates-current-session-only ()
  "Selecting a model updates only the current session."
  (let* ((buf-a (generate-new-buffer "*pi-coding-agent-chat:model-select-a*"))
         (buf-b (generate-new-buffer "*pi-coding-agent-chat:model-select-b*"))
         (available-models (list (list :id "model-a" :name "Model A" :provider "test")
                                 (list :id "model-b" :name "Model B" :provider "test")))
         (selected-model (list :id "model-b" :name "Model B" :provider "test")))
    (unwind-protect
        (cl-letf (((symbol-function 'pi-coding-agent--rpc-sync)
                   (lambda (&rest _) (list :success t :data (list :models available-models))))
                  ((symbol-function 'pi-coding-agent--rpc-async)
                   (lambda (_proc _cmd callback)
                     (funcall callback (list :success t :command "set_model" :data selected-model))))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "Model B")))
          (with-current-buffer buf-a
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--process :proc-a)
            (setq pi-coding-agent--state '(:model (:name "Model A" :id "model-a"))))
          (with-current-buffer buf-b
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--process :proc-b)
            (setq pi-coding-agent--state '(:model (:name "Model B-old" :id "model-b-old"))))
          (with-current-buffer buf-a
            (pi-coding-agent-select-model))
          (let ((model-a (with-current-buffer buf-a
                           (plist-get (plist-get pi-coding-agent--state :model) :name)))
                (model-b (with-current-buffer buf-b
                           (plist-get (plist-get pi-coding-agent--state :model) :name))))
            (should (equal (list model-a model-b)
                           '("Model B" "Model B-old")))))
      (mapc (lambda (buf)
              (with-current-buffer buf
                (setq pi-coding-agent--process nil))
              (kill-buffer buf))
            (list buf-a buf-b)))))

(ert-deftest pi-coding-agent-test-update-state-refreshes-header ()
  "Updating state should trigger header-line refresh."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model (:name "old-model") :thinking-level "low"))
    (let ((header-before (pi-coding-agent--header-line-string)))
      ;; Simulate state update
      (setq pi-coding-agent--state '(:model (:name "new-model") :thinking-level "high"))
      (let ((header-after (pi-coding-agent--header-line-string)))
        ;; Header string should reflect new state
        (should (string-match-p "new-model" header-after))
        (should (string-match-p "high" header-after))))))

(ert-deftest pi-coding-agent-test-apply-state-response-updates-buffer ()
  "Apply state response updates buffer-local variables in correct buffer."
  (let ((chat-buf (generate-new-buffer "*test-apply-state*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--state nil
                  pi-coding-agent--status nil))
          ;; Call from a different buffer to verify buffer context handling
          (with-temp-buffer
            (pi-coding-agent--apply-state-response
             chat-buf
             '(:success t :data (:isStreaming :false
                                 :sessionFile "/tmp/test.jsonl"
                                 :model "test-model"))))
          ;; Verify state was updated in chat-buf, not temp buffer
          (with-current-buffer chat-buf
            (should (eq pi-coding-agent--status 'idle))
            (should (equal (plist-get pi-coding-agent--state :session-file)
                           "/tmp/test.jsonl"))))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-apply-state-response-handles-dead-buffer ()
  "Apply state response handles dead buffer gracefully."
  (let ((chat-buf (generate-new-buffer "*test-dead-buf*")))
    (kill-buffer chat-buf)
    ;; Should not error when buffer is dead
    (pi-coding-agent--apply-state-response
     chat-buf
     '(:success t :data (:sessionFile "/tmp/test.jsonl")))))

(ert-deftest pi-coding-agent-test-header-line-model-is-clickable ()
  "Model name in header-line has click properties."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model (:name "claude-sonnet-4")))
    (let ((header (pi-coding-agent--header-line-string)))
      ;; Should have local-map property
      (should (get-text-property 0 'local-map header))
      ;; Should have mouse-face for highlight
      (should (get-text-property 0 'mouse-face header)))))

(ert-deftest pi-coding-agent-test-header-line-thinking-is-clickable ()
  "Thinking level in header-line has click properties."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model (:name "test") :thinking-level "high"))
    (let* ((header (pi-coding-agent--header-line-string))
           ;; Find position of "high" in header
           (pos (string-match "high" header)))
      (should pos)
      ;; Should have local-map at that position
      (should (get-text-property pos 'local-map header))
      ;; Should have mouse-face for highlight
      (should (get-text-property pos 'mouse-face header)))))

(ert-deftest pi-coding-agent-test-header-format-context-returns-nil-when-no-window ()
  "Context format returns nil when context window is 0."
  (should (null (pi-coding-agent--header-format-context 1000 0))))

(ert-deftest pi-coding-agent-test-header-format-context-shows-percentage ()
  "Context format shows percentage and window size."
  (let ((result (pi-coding-agent--header-format-context 50000 200000)))
    (should (string-match-p "25.0%%" result))
    (should (string-match-p "200k" result))))

(ert-deftest pi-coding-agent-test-header-format-context-warning-over-70 ()
  "Context format uses warning face over 70%."
  (let ((result (pi-coding-agent--header-format-context 150000 200000)))
    (should (eq (get-text-property 0 'face result) 'warning))))

(ert-deftest pi-coding-agent-test-header-format-context-error-over-90 ()
  "Context format uses error face over 90%."
  (let ((result (pi-coding-agent--header-format-context 190000 200000)))
    (should (eq (get-text-property 0 'face result) 'error))))

(ert-deftest pi-coding-agent-test-message-end-updates-usage-for-normal-completion ()
  "message_end with stopReason=stop updates pi-coding-agent--last-usage."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--last-usage nil)
    (pi-coding-agent--handle-display-event
     '(:type "message_end"
       :message (:role "assistant"
                 :stopReason "stop"
                 :usage (:input 1000 :output 500 :cacheRead 200 :cacheWrite 50))))
    (should pi-coding-agent--last-usage)
    (should (equal (plist-get pi-coding-agent--last-usage :input) 1000))))

(ert-deftest pi-coding-agent-test-message-end-updates-usage-for-tool-use ()
  "message_end with stopReason=toolUse updates pi-coding-agent--last-usage."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--last-usage nil)
    (pi-coding-agent--handle-display-event
     '(:type "message_end"
       :message (:role "assistant"
                 :stopReason "toolUse"
                 :usage (:input 2000 :output 300 :cacheRead 100 :cacheWrite 25))))
    (should pi-coding-agent--last-usage)
    (should (equal (plist-get pi-coding-agent--last-usage :input) 2000))))

(ert-deftest pi-coding-agent-test-message-end-skips-usage-for-aborted ()
  "message_end with stopReason=aborted does NOT update pi-coding-agent--last-usage.
This preserves the previous valid usage for context percentage display."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Set up previous valid usage
    (setq pi-coding-agent--last-usage '(:input 5000 :output 1000 :cacheRead 500 :cacheWrite 100))
    ;; Process an aborted message with zeroed/incomplete usage
    (pi-coding-agent--handle-display-event
     '(:type "message_end"
       :message (:role "assistant"
                 :stopReason "aborted"
                 :usage (:input 0 :output 0 :cacheRead 0 :cacheWrite 0))))
    ;; Should preserve previous usage, not overwrite with zeros
    (should (equal (plist-get pi-coding-agent--last-usage :input) 5000))))

(ert-deftest pi-coding-agent-test-message-end-updates-usage-for-error ()
  "message_end with stopReason=error updates pi-coding-agent--last-usage.
Errors still consume context, so their usage data is valid for display."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--last-usage nil)
    (pi-coding-agent--handle-display-event
     '(:type "message_end"
       :message (:role "assistant"
                 :stopReason "error"
                 :usage (:input 3000 :output 100 :cacheRead 400 :cacheWrite 0))))
    (should pi-coding-agent--last-usage)
    (should (equal (plist-get pi-coding-agent--last-usage :input) 3000))))

(ert-deftest pi-coding-agent-test-header-format-stats-returns-nil-when-no-stats ()
  "Stats format returns nil when stats is nil."
  (should (null (pi-coding-agent--header-format-stats nil nil nil))))

(ert-deftest pi-coding-agent-test-header-format-stats-shows-tokens ()
  "Stats format shows token counts."
  (let* ((stats '(:tokens (:input 1000 :output 500 :cacheRead 2000 :cacheWrite 100)
                  :cost 0.05))
         (result (pi-coding-agent--header-format-stats stats nil nil)))
    (should (string-match-p "↑1k" result))
    (should (string-match-p "↓500" result))
    (should (string-match-p "R2k" result))
    (should (string-match-p "W100" result))
    (should (string-match-p "\\$0.05" result))))

;;; File Reference Completion (@)

(ert-deftest pi-coding-agent-test-at-trigger-context ()
  "@ completion should only trigger at word boundaries, not in emails."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    ;; @ at start of buffer - should trigger
    (erase-buffer)
    (insert "@")
    (should (pi-coding-agent--at-trigger-p))
    ;; @ after space - should trigger
    (erase-buffer)
    (insert "hello @")
    (should (pi-coding-agent--at-trigger-p))
    ;; @ after newline - should trigger
    (erase-buffer)
    (insert "hello\n@")
    (should (pi-coding-agent--at-trigger-p))
    ;; @ after punctuation - should trigger
    (erase-buffer)
    (insert "see:@")
    (should (pi-coding-agent--at-trigger-p))
    ;; @ after alphanumeric (email) - should NOT trigger
    (erase-buffer)
    (insert "user@")
    (should-not (pi-coding-agent--at-trigger-p))
    ;; @ in middle of email - should NOT trigger
    (erase-buffer)
    (insert "test123@")
    (should-not (pi-coding-agent--at-trigger-p))))

(ert-deftest pi-coding-agent-test-file-reference-capf-returns-nil-without-at ()
  "File reference completion returns nil when not after @."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "hello world")
    (should-not (pi-coding-agent--file-reference-capf))))

(ert-deftest pi-coding-agent-test-file-reference-capf-returns-data-after-at ()
  "File reference completion returns data when point is after @."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    ;; Mock project files
    (setq pi-coding-agent--project-files-cache '("file1.el" "file2.py" "dir/file3.ts"))
    (setq pi-coding-agent--project-files-cache-time (float-time))
    (insert "Check @fi")
    (let ((result (pi-coding-agent--file-reference-capf)))
      (should result)
      ;; Start should be after @
      (should (= (nth 0 result) (- (point) 2)))  ; Position after @
      ;; End should be at point
      (should (= (nth 1 result) (point)))
      ;; Candidates should include matching files
      (should (member "file1.el" (nth 2 result)))
      (should (member "file2.py" (nth 2 result))))))

(ert-deftest pi-coding-agent-test-file-reference-capf-empty-prefix ()
  "File reference completion returns all files when no prefix after @."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (setq pi-coding-agent--project-files-cache '("a.el" "b.py" "c.ts"))
    (setq pi-coding-agent--project-files-cache-time (float-time))
    (insert "See @")
    (let ((result (pi-coding-agent--file-reference-capf)))
      (should result)
      ;; Should return all files when prefix is empty
      (should (= (length (nth 2 result)) 3)))))

(ert-deftest pi-coding-agent-test-file-reference-capf-mid-line ()
  "File reference completion works in the middle of a line."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (setq pi-coding-agent--project-files-cache '("test.el"))
    (setq pi-coding-agent--project-files-cache-time (float-time))
    (insert "Look at @te and also")
    (goto-char 11)  ; Position right after "@te"
    (let ((result (pi-coding-agent--file-reference-capf)))
      (should result)
      (should (member "test.el" (nth 2 result))))))

;;; Path Completion (Tab)

(ert-deftest pi-coding-agent-test-path-capf-returns-nil-for-non-path ()
  "Path completion returns nil for text that doesn't look like a path."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "hello world")
    (should-not (pi-coding-agent--path-capf))))

(ert-deftest pi-coding-agent-test-path-capf-returns-nil-for-non-prefixed-path ()
  "Path completion returns nil for paths without ./ ../ ~/ or / prefix."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "src/file.el")
    (should-not (pi-coding-agent--path-capf))))

(ert-deftest pi-coding-agent-test-path-capf-triggers-for-dot-slash ()
  "Path completion triggers for paths starting with ./"
  (let* ((temp-dir (make-temp-file "pi-coding-agent-path-test-" t))
         (test-file (expand-file-name "test.txt" temp-dir)))
    (unwind-protect
        (progn
          (with-temp-file test-file (insert "test"))
          (let ((default-directory temp-dir))
            (with-temp-buffer
              (pi-coding-agent-input-mode)
              (setq pi-coding-agent--chat-buffer (current-buffer))
              ;; Mock session directory
              (cl-letf (((symbol-function 'pi-coding-agent--session-directory)
                         (lambda () temp-dir)))
                (insert "./te")
                (let ((result (pi-coding-agent--path-capf)))
                  (should result)
                  ;; Should have candidates
                  (should (> (length (nth 2 result)) 0)))))))
      (delete-directory temp-dir t))))

(ert-deftest pi-coding-agent-test-path-capf-triggers-for-tilde ()
  "Path completion triggers for paths starting with ~/"
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "~/")
    ;; Just verify it doesn't error and returns something
    ;; (actual completions depend on user's home directory)
    (let ((result (pi-coding-agent--path-capf)))
      ;; May return nil if ~ directory doesn't exist or has no completions
      ;; but should not error
      (should (or (null result) (listp result))))))

(ert-deftest pi-coding-agent-test-path-capf-triggers-for-absolute ()
  "Path completion triggers for absolute paths not at buffer start."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "see /tmp/")
    (let ((result (pi-coding-agent--path-capf)))
      (when result
        (should (listp (nth 2 result)))))))

(ert-deftest pi-coding-agent-test-path-completions-excludes-dot-entries ()
  "Path completions should not include ./ or ../ entries."
  (let* ((temp-dir (make-temp-file "pi-coding-agent-path-test-" t))
         (subdir (expand-file-name "subdir" temp-dir)))
    (unwind-protect
        (progn
          (make-directory subdir)
          (cl-letf (((symbol-function 'pi-coding-agent--session-directory)
                     (lambda () temp-dir)))
            (let ((completions (pi-coding-agent--path-completions "./")))
              ;; Should have the subdir
              (should (member "./subdir/" completions))
              ;; Should NOT have ./ or ../
              (should-not (member "./" completions))
              (should-not (member "./../" completions))
              (should-not (member "././" completions)))))
      (delete-directory temp-dir t))))

(ert-deftest pi-coding-agent-test-complete-command-exists ()
  "pi-coding-agent-complete should be an interactive command."
  (should (commandp 'pi-coding-agent-complete)))

(ert-deftest pi-coding-agent-test-path-capf-skips-slash-at-buffer-start ()
  "Path completion skips / at buffer start to allow slash commands."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "/tmp")
    (should-not (pi-coding-agent--path-capf))))

(ert-deftest pi-coding-agent-test-path-capf-allows-slash-on-later-lines ()
  "Path completion works for / on lines after the first."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "Check this file:\n/tmp/")
    (let ((result (pi-coding-agent--path-capf)))
      (when result
        (should (listp (nth 2 result)))))))

(ert-deftest pi-coding-agent-test-tool-start-creates-overlay ()
  "tool_execution_start creates an overlay with pending face."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    ;; Should have an overlay with pi-coding-agent-tool-block property
    (goto-char (point-min))
    (let* ((overlays (overlays-at (point)))
           (tool-ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block)) overlays)))
      (should tool-ov)
      (should (eq (overlay-get tool-ov 'face) 'pi-coding-agent-tool-block-pending))
      (should (equal (overlay-get tool-ov 'pi-coding-agent-tool-name) "bash")))))

(ert-deftest pi-coding-agent-test-tool-start-header-format ()
  "tool_execution_start uses simple header format, not drawer syntax."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls -la"))
    ;; Should have "$ ls -la" header
    (should (string-match-p "\\$ ls -la" (buffer-string)))
    ;; Should NOT have drawer syntax
    (should-not (string-match-p ":BASH:" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-end-changes-overlay-face ()
  "tool_execution_end changes overlay face from pending to success/error."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    ;; Initially pending
    (let ((ov (car (overlays-at (point-min)))))
      (should (eq (overlay-get ov 'face) 'pi-coding-agent-tool-block-pending)))
    ;; After success
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file.txt"))
                          nil nil)
    (let ((ov (car (overlays-at (point-min)))))
      (should (eq (overlay-get ov 'face) 'pi-coding-agent-tool-block-success)))))

(ert-deftest pi-coding-agent-test-tool-end-error-face ()
  "tool_execution_end sets error face on failure."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "bad"))
    (pi-coding-agent--display-tool-end "bash" '(:command "bad")
                          '((:type "text" :text "error"))
                          nil t)  ; is-error = t
    (let ((ov (car (overlays-at (point-min)))))
      (should (eq (overlay-get ov 'face) 'pi-coding-agent-tool-block-error)))))

(ert-deftest pi-coding-agent-test-tool-end-no-drawer-syntax ()
  "tool_execution_end does not insert :END: marker."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "output"))
                          nil nil)
    (should-not (string-match-p ":END:" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-overlay-does-not-extend-to-subsequent-content ()
  "Tool overlay should not extend when content is inserted after tool block.
Regression test: overlay with rear-advance was extending to subsequent content."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Create a complete tool block
    (pi-coding-agent--display-tool-start "write" '(:path "/tmp/test.txt" :content "hello"))
    (pi-coding-agent--display-tool-end "write" '(:path "/tmp/test.txt" :content "hello")
                          '((:type "text" :text "Written to /tmp/test.txt"))
                          nil nil)
    ;; Simulate inserting more content after the tool (like next message)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "AFTER_TOOL_CONTENT\n"))
    ;; The new content should NOT be inside any tool overlay
    (let* ((new-content-pos (- (point-max) 10))  ; somewhere in AFTER_TOOL_CONTENT
           (overlays (overlays-at new-content-pos))
           (tool-overlay (seq-find
                          (lambda (ov) (overlay-get ov 'pi-coding-agent-tool-block))
                          overlays)))
      (should-not tool-overlay))))

(ert-deftest pi-coding-agent-test-abort-mid-tool-cleans-up-overlay ()
  "Aborting mid-tool should clean up the pending overlay.
When abort happens during tool execution, tool_execution_end never arrives.
display-agent-end must finalize the pending overlay with error face."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Start a tool (creates pending overlay)
    (pi-coding-agent--display-tool-start "bash" '(:command "sleep 100"))
    ;; Verify overlay is pending
    (should pi-coding-agent--pending-tool-overlay)
    (should (eq (overlay-get pi-coding-agent--pending-tool-overlay 'face)
                'pi-coding-agent-tool-block-pending))
    ;; Simulate abort - display-agent-end is called WITHOUT tool-end
    (setq pi-coding-agent--aborted t)
    (pi-coding-agent--display-agent-end)
    ;; Pending overlay variable should be nil
    (should-not pi-coding-agent--pending-tool-overlay)
    ;; But there should still be a finalized overlay with error face
    (goto-char (point-min))
    (let* ((overlays (overlays-at (point)))
           (tool-ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block)) overlays)))
      (should tool-ov)
      (should (eq (overlay-get tool-ov 'face) 'pi-coding-agent-tool-block-error)))
    ;; Content inserted after should NOT be inside the overlay
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "AFTER_ABORT_CONTENT\n"))
    (let* ((new-content-pos (- (point-max) 10))
           (overlays (overlays-at new-content-pos))
           (tool-overlay (seq-find
                          (lambda (ov) (overlay-get ov 'pi-coding-agent-tool-block))
                          overlays)))
      (should-not tool-overlay))))

(ert-deftest pi-coding-agent-test-delta-no-transform-inside-code-block ()
  "Hash inside fenced code block should NOT be transformed."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "```python\n# This is a comment\n```")
    ;; The # inside code block should stay as single #
    (should (string-match-p "^# This is a comment$" (buffer-string)))))

(ert-deftest pi-coding-agent-test-delta-transform-resumes-after-code-block ()
  "Headings after code block closes should be transformed."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "```\n# comment\n```\n# Real Heading")
    ;; Inside block: stays #
    (should (string-match-p "^# comment$" (buffer-string)))
    ;; After block: becomes ##
    (should (string-match-p "^## Real Heading" (buffer-string)))))

(ert-deftest pi-coding-agent-test-delta-code-fence-split-across-deltas ()
  "Code fence split across deltas still detected."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "``")
    (pi-coding-agent--display-message-delta "`python\n# comment\n```")
    ;; Should recognize the split ``` and not transform inside
    (should (string-match-p "^# comment$" (buffer-string)))))

(ert-deftest pi-coding-agent-test-delta-backticks-mid-line-not-fence ()
  "Backticks mid-line don't trigger code block state."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Use ```code``` inline\n# Heading")
    ;; Inline backticks shouldn't affect heading transform
    (should (string-match-p "^## Heading" (buffer-string)))))

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

(ert-deftest pi-coding-agent-test-send-stops-spinner-when-process-dead ()
  "Sending when process is dead stops spinner and resets status."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-spinner-dead*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-spinner-dead-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--input-buffer input-buf)
            ;; Set up dead process
            (let ((dead-proc (start-process "test-dead" nil "true")))
              (should (pi-coding-agent-test-wait-for-process-exit dead-proc))
              (setq pi-coding-agent--process dead-proc)))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "test message")
            (pi-coding-agent-send))
          ;; Verify spinner stopped and status reset
          (with-current-buffer chat-buf
            (should (eq pi-coding-agent--status 'idle))))
      (when (buffer-live-p chat-buf) (kill-buffer chat-buf))
      (when (buffer-live-p input-buf) (kill-buffer input-buf)))))

;;;; Performance Tests

(ert-deftest pi-coding-agent-test-streaming-inhibits-modification-hooks ()
  "Streaming delta functions inhibit modification hooks for performance.
This prevents expensive jit-lock/font-lock from running on each delta,
which caused major performance issues with large buffers."
  (let ((hook-called nil))
    (cl-flet ((test-hook (beg end len) (setq hook-called t)))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (pi-coding-agent--display-agent-start)
        (add-hook 'after-change-functions #'test-hook nil t)
        (setq hook-called nil)
        (pi-coding-agent--display-message-delta "Test delta")
        (should-not hook-called)))))

(ert-deftest pi-coding-agent-test-thinking-delta-inhibits-modification-hooks ()
  "Thinking delta inhibits modification hooks for performance."
  (let ((hook-called nil))
    (cl-flet ((test-hook (beg end len) (setq hook-called t)))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (pi-coding-agent--display-agent-start)
        (add-hook 'after-change-functions #'test-hook nil t)
        (setq hook-called nil)
        (pi-coding-agent--display-thinking-delta "Test thinking")
        (should-not hook-called)))))

(ert-deftest pi-coding-agent-test-tool-update-inhibits-modification-hooks ()
  "Tool update inhibits modification hooks for performance."
  (let ((hook-called nil))
    (cl-flet ((test-hook (beg end len) (setq hook-called t)))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (pi-coding-agent--display-agent-start)
        ;; Create pending tool overlay
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (setq pi-coding-agent--pending-tool-overlay
                (pi-coding-agent--tool-overlay-create "bash"))
          (insert "$ test\n"))
        (add-hook 'after-change-functions #'test-hook nil t)
        (setq hook-called nil)
        (pi-coding-agent--display-tool-update
         '(:content [(:type "text" :text "output")]))
        (should-not hook-called)))))

(ert-deftest pi-coding-agent-test-normal-insert-does-call-hooks ()
  "Control test: normal inserts DO trigger hooks.
This validates that our hook-based tests are meaningful."
  (let ((hook-called nil))
    (cl-flet ((test-hook (beg end len) (setq hook-called t)))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (add-hook 'after-change-functions #'test-hook nil t)
        (setq hook-called nil)
        (let ((inhibit-read-only t))
          (insert "Normal insert"))
        (should hook-called)))))

(ert-deftest pi-coding-agent-test-markdown-backward-search-limit ()
  "The markdown backward search limit improves fontification performance.
In large buffers with many code blocks, markdown-find-previous-block
scans backward through all text properties, causing O(n) slowdown.
The advice limits this scan to `pi-coding-agent-markdown-search-limit' bytes."
  ;; Verify the advice is installed
  (should (advice-member-p #'pi-coding-agent--limit-markdown-backward-search
                           'markdown-find-previous-prop))
  
  ;; Test that the limit only applies in pi-coding-agent-chat-mode
  (with-temp-buffer
    (insert (make-string 100000 ?x))  ;; 100KB buffer
    ;; Add a property ONLY far from end (outside 30KB limit)
    (put-text-property 10000 10001 'markdown-gfm-block-begin t)
    (goto-char (point-max))
    
    ;; In non-pi buffer, should find the property (no limit)
    (let ((result (markdown-find-previous-prop 'markdown-gfm-block-begin)))
      (should result)
      (should (= (car result) 10000)))
    
    ;; In pi-coding-agent-chat-mode, should NOT find it (outside 30KB limit)
    (pi-coding-agent-chat-mode)
    (goto-char (point-max))
    (let ((result (markdown-find-previous-prop 'markdown-gfm-block-begin)))
      ;; Result should be nil or the limit position, not the property
      (should-not (and result (= (car result) 10000))))))

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
                       (lambda (_prompt) "world")))
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
                       (lambda (_prompt) "")))
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
  "Templates submenu binds numbered keys to commands."
  (let ((pi-coding-agent--commands
         '((:name "test-tmpl" :description "A template" :source "prompt"))))
    (transient-setup 'pi-coding-agent-templates-menu)
    (should (pi-coding-agent-test--suffix-key-bound-p "1"))))

(ert-deftest pi-coding-agent-test-templates-menu-shows-edit-keys ()
  "Templates submenu binds shift-number keys to edit file paths."
  (let ((pi-coding-agent--commands
         '((:name "uncle-bob" :description "Uncle Bob review"
            :source "prompt" :path "/tmp/uncle-bob.md" :location "user")
           (:name "fix-tests" :description "Fix tests"
            :source "prompt" :path "/tmp/fix-tests.md" :location "project"))))
    (transient-setup 'pi-coding-agent-templates-menu)
    (should (pi-coding-agent-test--suffix-key-bound-p "1"))
    (should (pi-coding-agent-test--suffix-key-bound-p "!"))))

;;; Optional phscroll install

(ert-deftest pi-coding-agent-test-phscroll-offer-install-when-missing ()
  "Offer to install phscroll when wanted but not available (Emacs 29+)."
  (let ((pi-coding-agent-table-horizontal-scroll t)
        (pi-coding-agent-phscroll-offer-install t)
        (noninteractive nil)
        (installed-url nil))
    (unwind-protect
        (progn
          (setq features (delq 'phscroll features))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_prompt) t))
                    ((symbol-function 'package-vc-install)
                     (lambda (url) (setq installed-url url)))
                    ((symbol-function 'require)
                     #'ignore))
            (pi-coding-agent--maybe-install-phscroll)
            (should (string-match-p "phscroll" installed-url))))
      (require 'phscroll nil t))))

(ert-deftest pi-coding-agent-test-phscroll-decline-suppresses-permanently ()
  "Declining phscroll install persists the suppression."
  (let ((pi-coding-agent-table-horizontal-scroll t)
        (pi-coding-agent-phscroll-offer-install t)
        (noninteractive nil)
        (saved-var nil))
    (unwind-protect
        (progn
          (setq features (delq 'phscroll features))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_prompt) nil))
                    ((symbol-function 'customize-save-variable)
                     (lambda (var val) (setq saved-var var)
                             (set var val))))
            (pi-coding-agent--maybe-install-phscroll)
            (should (eq saved-var 'pi-coding-agent-phscroll-offer-install))
            (should-not pi-coding-agent-phscroll-offer-install)))
      (require 'phscroll nil t))))

(ert-deftest pi-coding-agent-test-phscroll-no-prompt-when-already-loaded ()
  "No prompt when phscroll is already available."
  (let ((pi-coding-agent-table-horizontal-scroll t)
        (pi-coding-agent-phscroll-offer-install t)
        (noninteractive nil)
        (prompted nil))
    (unwind-protect
        (progn
          (provide 'phscroll)
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (_prompt) (setq prompted t))))
            (pi-coding-agent--maybe-install-phscroll)
            (should-not prompted)))
      (setq features (delq 'phscroll features)))))

(ert-deftest pi-coding-agent-test-phscroll-no-prompt-when-scroll-disabled ()
  "No prompt when horizontal scroll is disabled."
  (let ((pi-coding-agent-table-horizontal-scroll nil)
        (pi-coding-agent-phscroll-offer-install t)
        (noninteractive nil)
        (prompted nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) (setq prompted t))))
      (pi-coding-agent--maybe-install-phscroll)
      (should-not prompted))))

(ert-deftest pi-coding-agent-test-phscroll-no-prompt-when-suppressed ()
  "No prompt when user previously declined."
  (let ((pi-coding-agent-table-horizontal-scroll t)
        (pi-coding-agent-phscroll-offer-install nil)
        (noninteractive nil)
        (prompted nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) (setq prompted t))))
      (pi-coding-agent--maybe-install-phscroll)
      (should-not prompted))))

(ert-deftest pi-coding-agent-test-phscroll-no-prompt-in-batch-mode ()
  "Never prompt in batch mode (noninteractive)."
  (let ((pi-coding-agent-table-horizontal-scroll t)
        (pi-coding-agent-phscroll-offer-install t)
        (prompted nil))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) (setq prompted t))))
      (pi-coding-agent--maybe-install-phscroll)
      (should-not prompted))))

(ert-deftest pi-coding-agent-test-phscroll-emacs28-shows-url ()
  "On Emacs 28 (no package-vc-install), show URL and suppress."
  (let ((pi-coding-agent-table-horizontal-scroll t)
        (pi-coding-agent-phscroll-offer-install t)
        (noninteractive nil)
        (shown-message nil)
        (saved-var nil))
    (unwind-protect
        (progn
          (setq features (delq 'phscroll features))
          (cl-letf (((symbol-function 'package-vc-install)
                     nil)
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq shown-message (apply #'format fmt args))))
                    ((symbol-function 'customize-save-variable)
                     (lambda (var val) (setq saved-var var)
                             (set var val))))
            (pi-coding-agent--maybe-install-phscroll)
            (should (string-match-p "phscroll" shown-message))
            (should (eq saved-var 'pi-coding-agent-phscroll-offer-install))))
      (require 'phscroll nil t))))

;;; Table Alignment with Hidden Markup

(defun pi-coding-agent-test--table-col-width (buffer-content)
  "Extract first column width from table separator in BUFFER-CONTENT."
  (let* ((lines (split-string buffer-content "\n"))
         (sep-line (cl-find-if (lambda (l) (string-match-p "^|[-|]+|$" l)) lines)))
    (when (and sep-line (string-match "^|\\([-]+\\)|" sep-line))
      (length (match-string 1 sep-line)))))

(defun pi-coding-agent-test--table-row-visible-widths (buffer-content)
  "Extract visible widths of all content rows (non-separator) in BUFFER-CONTENT.
Returns list of visible widths for each row, excluding the separator line."
  (let ((lines (split-string buffer-content "\n" t))
        widths)
    (dolist (line lines)
      (when (and (string-prefix-p "|" line)
                 (not (string-match-p "^|[-:|]+|$" line)))
        (push (pi-coding-agent--markdown-visible-width line) widths)))
    (nreverse widths)))

(ert-deftest pi-coding-agent-test-table-alignment-with-links ()
  "Column sized for visible link text, not raw [text](url) syntax."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta
     "| Issue | Title |\n|---|---|\n| [#1](http://example.com/1) | Fix bug |\n")
    (pi-coding-agent--render-complete-message)
    (let ((col-width (pi-coding-agent-test--table-col-width (buffer-string))))
      (should col-width)
      (should (< col-width 15)))))

(ert-deftest pi-coding-agent-test-table-alignment-with-bold ()
  "Column sized for visible bold text, not raw **text** syntax."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta
     "| X |\n|---|\n| **VeryLongBold** |\n")
    (pi-coding-agent--render-complete-message)
    (let ((col-width (pi-coding-agent-test--table-col-width (buffer-string))))
      (should col-width)
      (should (<= col-width 15)))))

(ert-deftest pi-coding-agent-test-table-alignment-with-images ()
  "Column sized for image alt text, not raw ![alt](url) syntax."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta
     "| Image | Name |\n|---|---|\n| ![Logo](http://example.com/logo.png) | Test |\n")
    (pi-coding-agent--render-complete-message)
    (let ((col-width (pi-coding-agent-test--table-col-width (buffer-string))))
      (should col-width)
      (should (< col-width 15)))))

(ert-deftest pi-coding-agent-test-table-alignment-with-inline-code ()
  "Column sized for code content, not raw \`code\` syntax."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta
     "| X |\n|---|\n| `verylongcommand` |\n")
    (pi-coding-agent--render-complete-message)
    (let ((col-width (pi-coding-agent-test--table-col-width (buffer-string))))
      (should col-width)
      (should (<= col-width 18)))))

(ert-deftest pi-coding-agent-test-table-alignment-multiple-links ()
  "Column sized for combined visible link texts."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta
     "| Related |\n|---|\n| [A](http://a.com), [B](http://b.com) |\n")
    (pi-coding-agent--render-complete-message)
    (let ((col-width (pi-coding-agent-test--table-col-width (buffer-string))))
      (should col-width)
      (should (< col-width 15)))))

(ert-deftest pi-coding-agent-test-markdown-visible-width-links ()
  "Visible width strips link URLs correctly."
  (should (= (pi-coding-agent--markdown-visible-width "[text](http://example.com)")
             (string-width "text")))
  (should (= (pi-coding-agent--markdown-visible-width "[#123](http://github.com/issues/123)")
             (string-width "#123")))
  ;; Multiple links
  (should (= (pi-coding-agent--markdown-visible-width "[A](http://a.com), [B](http://b.com)")
             (string-width "A, B"))))

(ert-deftest pi-coding-agent-test-markdown-visible-width-images ()
  "Visible width strips image URLs correctly."
  (should (= (pi-coding-agent--markdown-visible-width "![alt text](http://example.com/img.png)")
             (string-width "alt text"))))

(ert-deftest pi-coding-agent-test-markdown-visible-width-bold ()
  "Visible width strips bold markers correctly."
  (should (= (pi-coding-agent--markdown-visible-width "**bold**")
             (string-width "bold")))
  (should (= (pi-coding-agent--markdown-visible-width "normal **bold** normal")
             (string-width "normal bold normal"))))

(ert-deftest pi-coding-agent-test-markdown-visible-width-code ()
  "Visible width strips inline code backticks correctly."
  (should (= (pi-coding-agent--markdown-visible-width "`code`")
             (string-width "code")))
  (should (= (pi-coding-agent--markdown-visible-width "run `ls -la` command")
             (string-width "run ls -la command"))))

(ert-deftest pi-coding-agent-test-markdown-visible-width-plain ()
  "Visible width returns plain text unchanged."
  (should (= (pi-coding-agent--markdown-visible-width "plain text")
             (string-width "plain text")))
  (should (= (pi-coding-agent--markdown-visible-width "12345")
             5)))

(ert-deftest pi-coding-agent-test-markdown-visible-width-italic ()
  "Visible width strips italic markers correctly."
  (should (= (pi-coding-agent--markdown-visible-width "*italic*")
             (string-width "italic")))
  (should (= (pi-coding-agent--markdown-visible-width "normal *italic* text")
             (string-width "normal italic text"))))

(ert-deftest pi-coding-agent-test-markdown-visible-width-strikethrough ()
  "Visible width strips strikethrough markers correctly."
  (should (= (pi-coding-agent--markdown-visible-width "~~strike~~")
             (string-width "strike")))
  (should (= (pi-coding-agent--markdown-visible-width "normal ~~deleted~~ text")
             (string-width "normal deleted text")))
  ;; Numbers with commas (common in tables)
  (should (= (pi-coding-agent--markdown-visible-width "~~4,752~~")
             (string-width "4,752"))))

(ert-deftest pi-coding-agent-test-table-alignment-with-strikethrough ()
  "Table rows with strikethrough align to same width as other rows."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta
     "| A | B |\n|---|---|\n| ~~strike~~ | plain |\n| plain | plain |\n")
    (pi-coding-agent--render-complete-message)
    (let ((widths (pi-coding-agent-test--table-row-visible-widths (buffer-string))))
      (should (= (length widths) 3))
      (should (apply #'= widths)))))

(ert-deftest pi-coding-agent-test-table-alignment-with-italic ()
  "Column sized for visible italic text, not raw *text* syntax."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta
     "| X |\n|---|\n| *VeryLongItalic* |\n")
    (pi-coding-agent--render-complete-message)
    (let ((col-width (pi-coding-agent-test--table-col-width (buffer-string))))
      (should col-width)
      (should (<= col-width 17)))))

(ert-deftest pi-coding-agent-test-table-rows-have-equal-visible-width ()
  "All table rows should have equal visible width after alignment.
When markdown-hide-markup is enabled, rows with inline code like `code`
should be padded extra to compensate for hidden backticks, ensuring
visual alignment."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    ;; Table with one plain row and one row with inline code
    (pi-coding-agent--display-message-delta
     "| Col |\n|---|\n| `code` |\n| normal |\n")
    (pi-coding-agent--render-complete-message)
    (let ((widths (pi-coding-agent-test--table-row-visible-widths (buffer-string))))
      ;; Should have 3 rows: header, code row, normal row
      (should (= (length widths) 3))
      ;; All rows should have the same visible width
      (should (= (nth 0 widths) (nth 1 widths)))
      (should (= (nth 1 widths) (nth 2 widths))))))

(ert-deftest pi-coding-agent-test-table-rows-mixed-markup-alignment ()
  "Table with various markup types should align visually.
Tests real-world scenario with links, code, bold in same table."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta
     "| Feature | Example |\n|---|---|\n| Link | [click](http://x.com) |\n| Code | `example` |\n| Bold | **strong** |\n| Plain | normal text |\n")
    (pi-coding-agent--render-complete-message)
    (let ((widths (pi-coding-agent-test--table-row-visible-widths (buffer-string))))
      ;; Should have 5 rows: header + 4 content rows
      (should (= (length widths) 5))
      ;; All rows should have equal visible width
      (should (apply #'= widths)))))

(ert-deftest pi-coding-agent-test-phscroll-available-p-requires-both ()
  "Phscroll requires both the feature and customization enabled."
  ;; When phscroll is not loaded, should return nil
  (let ((pi-coding-agent-table-horizontal-scroll t))
    (unless (featurep 'phscroll)
      (should-not (pi-coding-agent--phscroll-available-p))))
  ;; When disabled via customization, should return nil
  (let ((pi-coding-agent-table-horizontal-scroll nil))
    (should-not (pi-coding-agent--phscroll-available-p))))

(ert-deftest pi-coding-agent-test-apply-phscroll-graceful-fallback ()
  "Applying phscroll to tables does nothing when phscroll unavailable."
  ;; Should not error when phscroll is not available
  (let ((pi-coding-agent-table-horizontal-scroll nil))
    (with-temp-buffer
      (insert "| A | B |\n|---|---|\n| 1 | 2 |\n")
      ;; Should complete without error
      (pi-coding-agent--apply-phscroll-to-tables (point-min) (point-max))
      ;; Buffer content should be unchanged
      (should (string-match-p "| A | B |" (buffer-string))))))

(ert-deftest pi-coding-agent-test-phscroll-called-after-fontlock-streaming ()
  "When rendering streamed messages, phscroll must be called after font-lock.
Otherwise invisible properties for hidden markup aren't set yet,
causing column misalignment when horizontally scrolling."
  (let ((phscroll-call-invisible-state nil))
    ;; Mock phscroll to capture state when called
    (cl-letf (((symbol-function 'pi-coding-agent--phscroll-available-p)
               (lambda () t))
              ((symbol-function 'phscroll-mode)
               (lambda (&optional _arg) nil))
              ((symbol-function 'phscroll-region)
               (lambda (beg end)
                 ;; Record whether invisible props are set on backticks
                 (save-excursion
                   (goto-char beg)
                   (when (search-forward "`" end t)
                     (setq phscroll-call-invisible-state
                           (get-text-property (1- (point)) 'invisible)))))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (pi-coding-agent--display-agent-start)
        ;; Table with inline code - backticks should be invisible
        (pi-coding-agent--display-message-delta
         "| Col |\n|---|\n| `code` |\n")
        (pi-coding-agent--render-complete-message)
        ;; At the time phscroll-region was called, invisible should be set
        (should (eq phscroll-call-invisible-state 'markdown-markup))))))

(ert-deftest pi-coding-agent-test-phscroll-called-after-fontlock-history ()
  "When rendering history, phscroll must be called after font-lock.
Otherwise invisible properties for hidden markup aren't set yet,
causing column misalignment when horizontally scrolling."
  (let ((phscroll-call-invisible-state nil))
    ;; Mock phscroll to capture state when called
    (cl-letf (((symbol-function 'pi-coding-agent--phscroll-available-p)
               (lambda () t))
              ((symbol-function 'phscroll-mode)
               (lambda (&optional _arg) nil))
              ((symbol-function 'phscroll-region)
               (lambda (beg end)
                 ;; Record whether invisible props are set on backticks
                 (save-excursion
                   (goto-char beg)
                   (when (search-forward "`" end t)
                     (setq phscroll-call-invisible-state
                           (get-text-property (1- (point)) 'invisible)))))))
      (with-temp-buffer
        (pi-coding-agent-chat-mode)
        (setq pi-coding-agent--chat-buffer (current-buffer))
        ;; Render history text with a table containing inline code
        (pi-coding-agent--render-history-text
         "| Col |\n|---|\n| `code` |\n")
        ;; At the time phscroll-region was called, invisible should be set
        (should (eq phscroll-call-invisible-state 'markdown-markup))))))
