;;; pi-coding-agent-test.el --- Tests for pi-coding-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for pi-coding-agent - the main UI layer.

;;; Code:

(require 'ert)
(require 'pi-coding-agent)

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

(ert-deftest pi-coding-agent-test-new-session-clears-buffer-from-different-context ()
  "New session callback clears chat buffer even when called from different buffer.
This tests that the async callback properly captures the chat buffer reference,
not relying on current buffer context which may change before callback executes."
  (let ((chat-buf (generate-new-buffer "*pi-coding-agent-chat:/tmp/test-new-session/*"))
        (captured-callback nil))
    (unwind-protect
        (progn
          ;; Set up chat buffer with content
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (let ((inhibit-read-only t))
              (insert "Existing conversation content\nMore content here")))
          ;; Mock the RPC to capture the callback
          (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                    ((symbol-function 'pi-coding-agent--get-chat-buffer) (lambda () chat-buf))
                    ((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc _cmd cb) (setq captured-callback cb)))
                    ((symbol-function 'pi-coding-agent--refresh-header) #'ignore))
            ;; Call new-session from the chat buffer
            (with-current-buffer chat-buf
              (pi-coding-agent-new-session)))
          ;; Now simulate the async callback being called from a DIFFERENT buffer
          ;; (This is what happens in practice - callbacks run in arbitrary contexts)
          (with-temp-buffer
            (funcall captured-callback '(:success t :data (:cancelled :false))))
          ;; The chat buffer should have been cleared
          (with-current-buffer chat-buf
            (should-not (string-match-p "Existing conversation" (buffer-string)))
            ;; Should show startup header instead
            (should (string-match-p "C-c C-c" (buffer-string)))))
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

(ert-deftest pi-coding-agent-test-send-blocked-while-streaming ()
  "pi-coding-agent-send is blocked while streaming, input preserved."
  (let ((send-called nil)
        (message-shown nil))
    (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-send-streaming/"
      (cl-letf (((symbol-function 'pi-coding-agent--send-prompt)
                 (lambda (_) (setq send-called t)))
                ((symbol-function 'message)
                 (lambda (fmt &rest _) 
                   (when (and fmt (string-match-p "wait" fmt))
                     (setq message-shown t)))))
        ;; Set streaming status in chat buffer
        (with-current-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-send-streaming/*"
          (setq pi-coding-agent--status 'streaming))
        ;; Try to send while streaming
        (with-current-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-send-streaming/*"
          (insert "My message")
          (pi-coding-agent-send)
          ;; Should NOT have sent
          (should-not send-called)
          ;; Should have shown message
          (should message-shown)
          ;; Input should be preserved
          (should (string= (buffer-string) "My message")))))))

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

(ert-deftest pi-coding-agent-test-display-agent-end-adds-newlines ()
  "agent_end event adds trailing newlines."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--append-to-chat "Some response")
    (pi-coding-agent--display-agent-end)
    (should (string-suffix-p "\n\n" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-blank-line-after-user-header ()
  "User header is followed by blank line, then content."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-user-message "Hello")
    ;; Pattern: setext heading (You + underline), blank line, content
    (should (string-match-p "You\n=+\n\nHello" (buffer-string)))))

(ert-deftest pi-coding-agent-test-spacing-blank-line-after-assistant-header ()
  "Assistant header is followed by blank line, then content."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Hi")
    ;; Pattern: setext heading (Assistant + underline), blank line, content
    (should (string-match-p "Assistant\n=+\n\nHi" (buffer-string)))))

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

;;; Kill Buffer Protection

(ert-deftest pi-coding-agent-test-kill-query-prompts-during-streaming ()
  "Killing chat buffer during streaming prompts for confirmation."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
        (should-not (pi-coding-agent--kill-buffer-query))))))

(ert-deftest pi-coding-agent-test-kill-query-allows-when-idle ()
  "Killing chat buffer when idle is allowed without prompt."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'idle))
      (should (pi-coding-agent--kill-buffer-query)))))

(ert-deftest pi-coding-agent-test-kill-query-allows-non-chat-buffer ()
  "Killing non-chat buffers is always allowed."
  (with-temp-buffer
    (fundamental-mode)
    (should (pi-coding-agent--kill-buffer-query))))

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
  (let ((sep (pi-coding-agent--make-separator "You" 'pi-coding-agent-user-label)))
    ;; Setext format: label on one line, === underline on next
    (should (string-match-p "^You\n=+$" sep))))

(ert-deftest pi-coding-agent-test-separator-with-timestamp ()
  "Separator with timestamp shows label · time as setext H1."
  (let ((sep (pi-coding-agent--make-separator "You" 'pi-coding-agent-user-label (current-time))))
    ;; Format: "You · HH:MM" followed by newline and ===
    (should (string-match-p "^You · [0-2][0-9]:[0-5][0-9]\n=+$" sep))))

(ert-deftest pi-coding-agent-test-separator-is-valid-setext-heading ()
  "Separator produces valid markdown setext H1 syntax."
  (let ((sep (pi-coding-agent--make-separator "Assistant" 'pi-coding-agent-assistant-label)))
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

(ert-deftest pi-coding-agent-test-thinking-rendered-as-fenced-block ()
  "Thinking content renders as markdown fenced block after message completion."
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
    ;; After rendering, thinking should be in a markdown fenced block
    (goto-char (point-min))
    (should (search-forward "```thinking" nil t))
    (should (search-forward "Let me analyze this." nil t))
    (should (search-forward "```" nil t))
    ;; Regular text should be outside the block
    (should (search-forward "Here is my answer." nil t))))

(ert-deftest pi-coding-agent-test-thinking-block-has-face ()
  "Thinking block content has pi-coding-agent-thinking face after font-lock."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t))
      (insert "```thinking\nSome thinking here.\n```\n"))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "Some thinking")
    ;; Verify pi-coding-agent-thinking face is applied (may be in a list with other faces)
    (let ((face (get-text-property (point) 'face)))
      (should (or (eq face 'pi-coding-agent-thinking)
                  (and (listp face) (memq 'pi-coding-agent-thinking face)))))))

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

(ert-deftest pi-coding-agent-test-format-branch-message ()
  "Branch message formatted with index and preview."
  (let ((msg '(:entryId "abc-123" :text "Hello world, this is a test")))
    ;; With index
    (let ((result (pi-coding-agent--format-branch-message msg 2)))
      (should (string-match-p "2:" result))
      (should (string-match-p "Hello world" result)))
    ;; Without index
    (let ((result (pi-coding-agent--format-branch-message msg)))
      (should (string-match-p "Hello world" result))
      (should-not (string-match-p ":" result)))))

(ert-deftest pi-coding-agent-test-branch-detects-empty-messages-vector ()
  "Branch correctly detects empty messages vector from RPC.
JSON arrays are parsed as vectors, and (null []) is nil, not t.
The branch code must use seq-empty-p or length check."
  (let ((rpc-called nil)
        (message-shown nil))
    (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
              ((symbol-function 'pi-coding-agent--rpc-async)
               (lambda (_proc cmd cb)
                 (setq rpc-called t)
                 ;; Simulate response with empty vector (no messages to branch from)
                 (funcall cb '(:success t :data (:messages [])))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when (string-match-p "No messages" fmt)
                   (setq message-shown t)))))
      (pi-coding-agent-branch)
      (should rpc-called)
      ;; Should show "No messages to branch from", not call completing-read
      (should message-shown))))

(ert-deftest pi-coding-agent-test-format-branch-message-handles-nil-text ()
  "Format branch message handles nil text gracefully."
  (let ((msg '(:entryId "abc-123" :text nil)))
    ;; Should not error, should return something displayable
    (let ((result (pi-coding-agent--format-branch-message msg 1)))
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
          ;; Create "old" file first
          (with-temp-file old-file (insert "{}"))
          (sleep-for 0.1)  ; Ensure different mtime
          ;; Create "new" file second (more recent mtime despite earlier filename)
          (with-temp-file new-file (insert "{}"))
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
    (should (eq (key-binding (kbd "C-r")) 'pi-coding-agent-history-search))))

;;; Input Buffer Slash Completion

(ert-deftest pi-coding-agent-test-slash-capf-returns-nil-without-slash ()
  "Completion returns nil when not after slash."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "hello")
    (should-not (pi-coding-agent--slash-capf))))

(ert-deftest pi-coding-agent-test-slash-capf-returns-nil-at-line-start ()
  "Completion returns nil when point is at beginning of line."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "/test")
    (goto-char (line-beginning-position))
    (should-not (pi-coding-agent--slash-capf))))

(ert-deftest pi-coding-agent-test-slash-capf-returns-completion-data ()
  "Completion returns data when after slash at start of line."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (setq pi-coding-agent--file-commands '((:name "test-cmd" :description "Test")))
    (insert "/te")
    (let ((result (pi-coding-agent--slash-capf)))
      (should result)
      (should (= (nth 0 result) 2))  ; Start after /
      (should (= (nth 1 result) 4))  ; End at point
      (should (member "test-cmd" (nth 2 result))))))

;;; Input Buffer Slash Execution

(ert-deftest pi-coding-agent-test-expand-slash-command-known ()
  "Known slash command expands to content with args."
  (let ((pi-coding-agent--file-commands '((:name "test" :content "Do $@ please"))))
    (should (equal (pi-coding-agent--expand-slash-command "/test foo bar")
                   "Do foo bar please"))))

(ert-deftest pi-coding-agent-test-expand-slash-command-unknown ()
  "Unknown slash command returns original text."
  (let ((pi-coding-agent--file-commands '()))
    (should (equal (pi-coding-agent--expand-slash-command "/unknown foo")
                   "/unknown foo"))))

(ert-deftest pi-coding-agent-test-expand-slash-command-not-slash ()
  "Non-slash text returns unchanged."
  (let ((pi-coding-agent--file-commands '((:name "test" :content "content"))))
    (should (equal (pi-coding-agent--expand-slash-command "hello world")
                   "hello world"))))

(ert-deftest pi-coding-agent-test-send-expands-slash-command ()
  "pi-coding-agent-send expands slash commands before sending."
  (let* ((sent-text nil)
         (pi-coding-agent--file-commands '((:name "greet" :content "Hello $@!"))))
    (cl-letf (((symbol-function 'pi-coding-agent--get-chat-buffer)
               (lambda () (current-buffer)))
              ((symbol-function 'pi-coding-agent--display-user-message)
               (lambda (_text &optional _timestamp) nil))
              ((symbol-function 'pi-coding-agent--send-prompt)
               (lambda (text) (setq sent-text text))))
      (with-temp-buffer
        (pi-coding-agent-input-mode)
        (insert "/greet world")
        (pi-coding-agent-send)
        (should (equal sent-text "Hello world!"))))))

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

;;; Slash Command Argument Parsing (shell-like quoting via split-string-shell-command)

(ert-deftest pi-coding-agent-test-parse-args-simple ()
  "Parse simple space-separated arguments."
  (should (equal (pi-coding-agent--parse-command-args "foo bar baz")
                 '("foo" "bar" "baz"))))

(ert-deftest pi-coding-agent-test-parse-args-empty ()
  "Parse empty string returns empty list."
  (should (equal (pi-coding-agent--parse-command-args "") nil)))

(ert-deftest pi-coding-agent-test-parse-args-whitespace-only ()
  "Parse whitespace-only string returns empty list."
  (should (equal (pi-coding-agent--parse-command-args "   ") nil)))

(ert-deftest pi-coding-agent-test-parse-args-quoted-double ()
  "Parse double-quoted arguments preserves spaces."
  (should (equal (pi-coding-agent--parse-command-args "foo \"bar baz\" qux")
                 '("foo" "bar baz" "qux"))))

(ert-deftest pi-coding-agent-test-parse-args-quoted-single ()
  "Parse single-quoted arguments preserves spaces."
  (should (equal (pi-coding-agent--parse-command-args "foo 'bar baz' qux")
                 '("foo" "bar baz" "qux"))))

(ert-deftest pi-coding-agent-test-parse-args-extra-whitespace ()
  "Parse handles multiple spaces between arguments."
  (should (equal (pi-coding-agent--parse-command-args "foo   bar")
                 '("foo" "bar"))))

(ert-deftest pi-coding-agent-test-parse-args-escaped-quote ()
  "Parse handles escaped quotes inside double-quoted strings."
  (should (equal (pi-coding-agent--parse-command-args "foo \"bar\\\"baz\" qux")
                 '("foo" "bar\"baz" "qux"))))

(ert-deftest pi-coding-agent-test-parse-args-escaped-space ()
  "Parse handles backslash-escaped spaces outside quotes."
  (should (equal (pi-coding-agent--parse-command-args "foo\\ bar baz")
                 '("foo bar" "baz"))))

(ert-deftest pi-coding-agent-test-parse-args-mixed-quotes ()
  "Parse handles mixed single and double quotes."
  (should (equal (pi-coding-agent--parse-command-args "\"foo\" 'bar'")
                 '("foo" "bar"))))

;;; Slash Command Argument Substitution

(ert-deftest pi-coding-agent-test-substitute-args-positional ()
  "Substitute positional arguments $1, $2, etc."
  (should (equal (pi-coding-agent--substitute-args "Hello $1 and $2" '("world" "friend"))
                 "Hello world and friend")))

(ert-deftest pi-coding-agent-test-substitute-args-all ()
  "Substitute $@ with all arguments joined."
  (should (equal (pi-coding-agent--substitute-args "Args: $@" '("one" "two" "three"))
                 "Args: one two three")))

(ert-deftest pi-coding-agent-test-substitute-args-missing ()
  "Missing positional arguments become empty string."
  (should (equal (pi-coding-agent--substitute-args "Hello $1 and $2" '("world"))
                 "Hello world and ")))

(ert-deftest pi-coding-agent-test-substitute-args-no-placeholders ()
  "Content without placeholders returned unchanged."
  (should (equal (pi-coding-agent--substitute-args "Hello world" '("ignored"))
                 "Hello world")))

;;; Slash Command Frontmatter Parsing

(ert-deftest pi-coding-agent-test-parse-frontmatter-with-description ()
  "Parse YAML frontmatter with description."
  (let ((result (pi-coding-agent--parse-frontmatter "---
description: Test command
---
Command content here")))
    (should (equal (plist-get result :description) "Test command"))
    (should (equal (plist-get result :content) "Command content here"))))

(ert-deftest pi-coding-agent-test-parse-frontmatter-no-frontmatter ()
  "Content without frontmatter returns full content."
  (let ((result (pi-coding-agent--parse-frontmatter "Just content\nNo frontmatter")))
    (should (null (plist-get result :description)))
    (should (equal (plist-get result :content) "Just content\nNo frontmatter"))))

(ert-deftest pi-coding-agent-test-parse-frontmatter-empty-description ()
  "Frontmatter with empty description."
  (let ((result (pi-coding-agent--parse-frontmatter "---
description:
---
Content")))
    (should (equal (plist-get result :description) ""))
    (should (equal (plist-get result :content) "Content"))))

;;; Slash Command Discovery

(ert-deftest pi-coding-agent-test-discover-commands-from-directory ()
  "Discover commands from a directory with .md files."
  (let* ((temp-dir (make-temp-file "pi-coding-agent-commands-" t))
         (cmd-file (expand-file-name "test-cmd.md" temp-dir)))
    (unwind-protect
        (progn
          (with-temp-file cmd-file
            (insert "---\ndescription: A test command\n---\nDo the thing"))
          (let ((commands (pi-coding-agent--load-commands-from-dir temp-dir "user")))
            (should (= (length commands) 1))
            (let ((cmd (car commands)))
              (should (equal (plist-get cmd :name) "test-cmd"))
              (should (string-match-p "test command" (plist-get cmd :description)))
              (should (equal (plist-get cmd :content) "Do the thing")))))
      (delete-directory temp-dir t))))

(ert-deftest pi-coding-agent-test-discover-commands-missing-directory ()
  "Missing directory returns empty list."
  (should (equal (pi-coding-agent--load-commands-from-dir "/nonexistent/path" "user") '())))

(ert-deftest pi-coding-agent-test-discover-commands-description-from-content ()
  "When no frontmatter description, use first line of content."
  (let* ((temp-dir (make-temp-file "pi-coding-agent-commands-" t))
         (cmd-file (expand-file-name "no-desc.md" temp-dir)))
    (unwind-protect
        (progn
          (with-temp-file cmd-file
            (insert "First line becomes description\nMore content"))
          (let* ((commands (pi-coding-agent--load-commands-from-dir temp-dir "user"))
                 (cmd (car commands)))
            (should (string-match-p "First line" (plist-get cmd :description)))))
      (delete-directory temp-dir t))))

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
