;;; pi-test.el --- Tests for pi.el -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for pi.el - the main UI layer.

;;; Code:

(require 'ert)
(require 'pi)

;;; Test Utilities

(defmacro pi-test-with-mock-session (dir &rest body)
  "Execute BODY with a mocked pi session in DIR, cleaning up after.
DIR should be a unique directory path like \"/tmp/pi-test-foo/\".
Mocks `project-current', `pi--start-process', and `pi--display-buffers'.
Automatically cleans up chat and input buffers."
  (declare (indent 1) (debug t))
  `(let ((default-directory ,dir))
     (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
               ((symbol-function 'pi--start-process) (lambda (_) nil))
               ((symbol-function 'pi--display-buffers) #'ignore))
       (unwind-protect
           (progn (pi) ,@body)
         (ignore-errors (kill-buffer (pi--chat-buffer-name ,dir nil)))
         (ignore-errors (kill-buffer (pi--input-buffer-name ,dir nil)))))))

;;; Buffer Naming

(ert-deftest pi-test-buffer-name-chat ()
  "Buffer name for chat includes abbreviated directory."
  (let ((name (pi--buffer-name :chat "/home/user/project/")))
    (should (string-match-p "\\*pi-chat:" name))
    (should (string-match-p "project" name))))

(ert-deftest pi-test-buffer-name-input ()
  "Buffer name for input includes abbreviated directory."
  (let ((name (pi--buffer-name :input "/home/user/project/")))
    (should (string-match-p "\\*pi-input:" name))
    (should (string-match-p "project" name))))

(ert-deftest pi-test-buffer-name-abbreviates-home ()
  "Buffer name abbreviates home directory to ~."
  (let ((name (pi--buffer-name :chat (expand-file-name "~/myproject/"))))
    (should (string-match-p "~" name))))

;;; Buffer Creation

(ert-deftest pi-test-get-or-create-buffer-creates-new ()
  "get-or-create-buffer creates a new buffer if none exists."
  (let* ((dir "/tmp/pi-test-unique-12345/")
         (buf (pi--get-or-create-buffer :chat dir)))
    (unwind-protect
        (progn
          (should (bufferp buf))
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest pi-test-get-or-create-buffer-returns-existing ()
  "get-or-create-buffer returns existing buffer."
  (let* ((dir "/tmp/pi-test-unique-67890/")
         (buf1 (pi--get-or-create-buffer :chat dir))
         (buf2 (pi--get-or-create-buffer :chat dir)))
    (unwind-protect
        (should (eq buf1 buf2))
      (when (buffer-live-p buf1)
        (kill-buffer buf1)))))

;;; Major Modes

(ert-deftest pi-test-chat-mode-is-read-only ()
  "pi-chat-mode sets buffer to read-only."
  (with-temp-buffer
    (pi-chat-mode)
    (should buffer-read-only)))

(ert-deftest pi-test-chat-mode-has-word-wrap ()
  "pi-chat-mode enables word wrap."
  (with-temp-buffer
    (pi-chat-mode)
    (should word-wrap)
    (should-not truncate-lines)))

(ert-deftest pi-test-input-mode-derives-from-text ()
  "pi-input-mode is derived from text-mode."
  (with-temp-buffer
    (pi-input-mode)
    (should (derived-mode-p 'text-mode))))

(ert-deftest pi-test-input-mode-not-read-only ()
  "pi-input-mode allows editing."
  (with-temp-buffer
    (pi-input-mode)
    (should-not buffer-read-only)))

;;; Session Directory Detection

(ert-deftest pi-test-session-directory-uses-project-root ()
  "Session directory is project root when in a project."
  (let ((default-directory "/tmp/"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) '(vc . "/home/user/myproject/")))
              ((symbol-function 'project-root)
               (lambda (_) "/home/user/myproject/")))
      (should (equal (pi--session-directory) "/home/user/myproject/")))))

(ert-deftest pi-test-session-directory-falls-back-to-default ()
  "Session directory is default-directory when not in a project."
  (let ((default-directory "/tmp/somedir/"))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) nil)))
      (should (equal (pi--session-directory) "/tmp/somedir/")))))

;;; Session Management

(ert-deftest pi-test-buffer-name-default-session ()
  "Buffer name without session name."
  (should (equal (pi--buffer-name :chat "/tmp/proj/" nil)
                 "*pi-chat:/tmp/proj/*")))

(ert-deftest pi-test-buffer-name-named-session ()
  "Buffer name with session name."
  (should (equal (pi--buffer-name :chat "/tmp/proj/" "feature")
                 "*pi-chat:/tmp/proj/<feature>*")))

(ert-deftest pi-test-clear-chat-buffer-resets-to-startup ()
  "Clearing chat buffer shows startup header and resets state."
  (with-temp-buffer
    (pi-chat-mode)
    ;; Add some content
    (let ((inhibit-read-only t))
      (insert "Some existing content\nMore content"))
    ;; Set markers as if streaming happened
    (setq pi--message-start-marker (point-marker))
    (setq pi--streaming-marker (point-marker))
    ;; Clear the buffer
    (pi--clear-chat-buffer)
    ;; Should have startup header
    (should (string-match-p "C-c C-c" (buffer-string)))
    ;; Markers should be reset
    (should (null pi--message-start-marker))
    (should (null pi--streaming-marker))))

(ert-deftest pi-test-find-session-returns-existing ()
  "pi--find-session returns existing chat buffer."
  (let ((buf (generate-new-buffer "*pi-chat:/tmp/test-find/*")))
    (unwind-protect
        (should (eq (pi--find-session "/tmp/test-find/" nil) buf))
      (kill-buffer buf))))

(ert-deftest pi-test-find-session-returns-nil-when-missing ()
  "pi--find-session returns nil when no session exists."
  (should (null (pi--find-session "/tmp/nonexistent-session-xyz/" nil))))

(ert-deftest pi-test-pi-reuses-existing-session ()
  "Calling pi twice returns same buffers."
  (pi-test-with-mock-session "/tmp/pi-test-reuse/"
    (let ((chat1 (get-buffer "*pi-chat:/tmp/pi-test-reuse/*"))
          (input1 (get-buffer "*pi-input:/tmp/pi-test-reuse/*")))
      (pi)  ; call again
      (should (eq chat1 (get-buffer "*pi-chat:/tmp/pi-test-reuse/*")))
      (should (eq input1 (get-buffer "*pi-input:/tmp/pi-test-reuse/*"))))))

(ert-deftest pi-test-named-session-separate-from-default ()
  "Named session creates separate buffers from default."
  (let ((default-directory "/tmp/pi-test-named/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi--start-process) (lambda (_) nil))
              ((symbol-function 'pi--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (pi)  ; default session
            (pi "feature")  ; named session
            (should (get-buffer "*pi-chat:/tmp/pi-test-named/*"))
            (should (get-buffer "*pi-chat:/tmp/pi-test-named/<feature>*"))
            (should-not (eq (get-buffer "*pi-chat:/tmp/pi-test-named/*")
                            (get-buffer "*pi-chat:/tmp/pi-test-named/<feature>*"))))
        (ignore-errors (kill-buffer "*pi-chat:/tmp/pi-test-named/*"))
        (ignore-errors (kill-buffer "*pi-input:/tmp/pi-test-named/*"))
        (ignore-errors (kill-buffer "*pi-chat:/tmp/pi-test-named/<feature>*"))
        (ignore-errors (kill-buffer "*pi-input:/tmp/pi-test-named/<feature>*"))))))

(ert-deftest pi-test-quit-kills-both-buffers ()
  "pi-quit kills both chat and input buffers."
  (pi-test-with-mock-session "/tmp/pi-test-quit/"
    (with-current-buffer "*pi-input:/tmp/pi-test-quit/*"
      (pi-quit))
    (should-not (get-buffer "*pi-chat:/tmp/pi-test-quit/*"))
    (should-not (get-buffer "*pi-input:/tmp/pi-test-quit/*"))))

(ert-deftest pi-test-kill-chat-kills-input ()
  "Killing chat buffer also kills input buffer."
  (pi-test-with-mock-session "/tmp/pi-test-linked/"
    (kill-buffer "*pi-chat:/tmp/pi-test-linked/*")
    (should-not (get-buffer "*pi-input:/tmp/pi-test-linked/*"))))

(ert-deftest pi-test-kill-input-kills-chat ()
  "Killing input buffer also kills chat buffer."
  (pi-test-with-mock-session "/tmp/pi-test-linked2/"
    (kill-buffer "*pi-input:/tmp/pi-test-linked2/*")
    (should-not (get-buffer "*pi-chat:/tmp/pi-test-linked2/*"))))

;;; Main Entry Point

(ert-deftest pi-test-pi-creates-chat-buffer ()
  "M-x pi creates a chat buffer."
  (pi-test-with-mock-session "/tmp/pi-test-main/"
    (should (get-buffer "*pi-chat:/tmp/pi-test-main/*"))))

(ert-deftest pi-test-pi-creates-input-buffer ()
  "M-x pi creates an input buffer."
  (pi-test-with-mock-session "/tmp/pi-test-main2/"
    (should (get-buffer "*pi-input:/tmp/pi-test-main2/*"))))

(ert-deftest pi-test-pi-sets-major-modes ()
  "M-x pi sets correct major modes on buffers."
  (pi-test-with-mock-session "/tmp/pi-test-modes/"
    (with-current-buffer "*pi-chat:/tmp/pi-test-modes/*"
      (should (derived-mode-p 'pi-chat-mode)))
    (with-current-buffer "*pi-input:/tmp/pi-test-modes/*"
      (should (derived-mode-p 'pi-input-mode)))))

;;; Buffer Linkage

(ert-deftest pi-test-input-buffer-finds-chat ()
  "Input buffer can find associated chat buffer."
  (pi-test-with-mock-session "/tmp/pi-test-link1/"
    (with-current-buffer "*pi-input:/tmp/pi-test-link1/*"
      (should (eq (pi--get-chat-buffer)
                  (get-buffer "*pi-chat:/tmp/pi-test-link1/*"))))))

(ert-deftest pi-test-chat-buffer-finds-input ()
  "Chat buffer can find associated input buffer."
  (pi-test-with-mock-session "/tmp/pi-test-link2/"
    (with-current-buffer "*pi-chat:/tmp/pi-test-link2/*"
      (should (eq (pi--get-input-buffer)
                  (get-buffer "*pi-input:/tmp/pi-test-link2/*"))))))

(ert-deftest pi-test-get-process-from-chat ()
  "Can get process from chat buffer."
  (let ((default-directory "/tmp/pi-test-proc1/")
        (fake-proc 'mock-process))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi--start-process) (lambda (_) fake-proc))
              ((symbol-function 'pi--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (pi)
            (with-current-buffer "*pi-chat:/tmp/pi-test-proc1/*"
              (should (eq (pi--get-process) fake-proc))))
        (ignore-errors (kill-buffer "*pi-chat:/tmp/pi-test-proc1/*"))
        (ignore-errors (kill-buffer "*pi-input:/tmp/pi-test-proc1/*"))))))

(ert-deftest pi-test-get-process-from-input ()
  "Can get process from input buffer via chat buffer."
  (let ((default-directory "/tmp/pi-test-proc2/")
        (fake-proc 'mock-process))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi--start-process) (lambda (_) fake-proc))
              ((symbol-function 'pi--display-buffers) #'ignore))
      (unwind-protect
          (progn
            (pi)
            (with-current-buffer "*pi-input:/tmp/pi-test-proc2/*"
              (should (eq (pi--get-process) fake-proc))))
        (ignore-errors (kill-buffer "*pi-chat:/tmp/pi-test-proc2/*"))
        (ignore-errors (kill-buffer "*pi-input:/tmp/pi-test-proc2/*"))))))

;;; Sending Prompts

(ert-deftest pi-test-send-extracts-text ()
  "pi-send extracts text from input buffer and clears it."
  (let ((sent-text nil))
    (pi-test-with-mock-session "/tmp/pi-test-send1/"
      (cl-letf (((symbol-function 'pi--send-prompt)
                 (lambda (text) (setq sent-text text))))
        (with-current-buffer "*pi-input:/tmp/pi-test-send1/*"
          (insert "Hello, pi!")
          (pi-send)
          (should (equal sent-text "Hello, pi!"))
          (should (string-empty-p (buffer-string))))))))

(ert-deftest pi-test-send-empty-is-noop ()
  "pi-send with empty buffer does nothing."
  (let ((send-called nil))
    (pi-test-with-mock-session "/tmp/pi-test-send2/"
      (cl-letf (((symbol-function 'pi--send-prompt)
                 (lambda (_) (setq send-called t))))
        (with-current-buffer "*pi-input:/tmp/pi-test-send2/*"
          (pi-send)
          (should-not send-called))))))

(ert-deftest pi-test-send-whitespace-only-is-noop ()
  "pi-send with only whitespace does nothing."
  (let ((send-called nil))
    (pi-test-with-mock-session "/tmp/pi-test-send3/"
      (cl-letf (((symbol-function 'pi--send-prompt)
                 (lambda (_) (setq send-called t))))
        (with-current-buffer "*pi-input:/tmp/pi-test-send3/*"
          (insert "   \n\t  ")
          (pi-send)
          (should-not send-called))))))

(ert-deftest pi-test-send-blocked-while-streaming ()
  "pi-send is blocked while streaming, input preserved."
  (let ((send-called nil)
        (message-shown nil))
    (pi-test-with-mock-session "/tmp/pi-test-send-streaming/"
      (cl-letf (((symbol-function 'pi--send-prompt)
                 (lambda (_) (setq send-called t)))
                ((symbol-function 'message)
                 (lambda (fmt &rest _) 
                   (when (and fmt (string-match-p "wait" fmt))
                     (setq message-shown t)))))
        ;; Set streaming status in chat buffer
        (with-current-buffer "*pi-chat:/tmp/pi-test-send-streaming/*"
          (setq pi--status 'streaming))
        ;; Try to send while streaming
        (with-current-buffer "*pi-input:/tmp/pi-test-send-streaming/*"
          (insert "My message")
          (pi-send)
          ;; Should NOT have sent
          (should-not send-called)
          ;; Should have shown message
          (should message-shown)
          ;; Input should be preserved
          (should (string= (buffer-string) "My message")))))))

;;; Response Display

(ert-deftest pi-test-append-to-chat-inserts-text ()
  "pi--append-to-chat inserts text at end of chat buffer."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--append-to-chat "Hello")
    (should (equal (buffer-string) "Hello"))))

(ert-deftest pi-test-append-to-chat-appends ()
  "pi--append-to-chat appends to existing content."
  (with-temp-buffer
    (pi-chat-mode)
    (let ((inhibit-read-only t))
      (insert "First"))
    (pi--append-to-chat " Second")
    (should (equal (buffer-string) "First Second"))))

(ert-deftest pi-test-display-agent-start-inserts-separator ()
  "agent_start event inserts a separator."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-agent-start)
    (should (string-match-p "─" (buffer-string)))))

(ert-deftest pi-test-display-message-delta-appends-text ()
  "message_update text_delta appends text to chat."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-agent-start)  ; Creates streaming marker
    (pi--display-message-delta "Hello, ")
    (pi--display-message-delta "world!")
    (should (string-match-p "Hello, world!" (buffer-string)))))

(ert-deftest pi-test-display-thinking-delta-appends-text ()
  "message_update thinking_delta appends text to chat."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-agent-start)  ; Creates streaming marker
    (pi--display-thinking-delta "Let me think...")
    (pi--display-thinking-delta " about this.")
    (should (string-match-p "Let me think... about this." (buffer-string)))))

(ert-deftest pi-test-display-agent-end-adds-newlines ()
  "agent_end event adds trailing newlines."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--append-to-chat "Some response")
    (pi--display-agent-end)
    (should (string-suffix-p "\n\n" (buffer-string)))))

(ert-deftest pi-test-spacing-blank-line-after-user-header ()
  "User header is followed by blank line, then content."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-user-message "Hello")
    ;; Pattern: separator, blank line, content
    (should (string-match-p "You ─+\n\nHello" (buffer-string)))))

(ert-deftest pi-test-spacing-blank-line-after-assistant-header ()
  "Assistant header is followed by blank line, then content."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-agent-start)
    (pi--display-message-delta "Hi")
    ;; Pattern: separator, blank line, content
    (should (string-match-p "Assistant ─+\n\nHi" (buffer-string)))))

(ert-deftest pi-test-spacing-blank-line-before-tool ()
  "Tool block is preceded by blank line when after text."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-agent-start)
    (pi--display-message-delta "Let me check.")
    (pi--render-complete-message)
    (pi--display-tool-start "bash" '(:command "ls"))
    ;; Pattern: text, blank line, :BASH:
    (should (string-match-p "check\\.\n\n:BASH:" (buffer-string)))))

(ert-deftest pi-test-spacing-blank-line-after-tool ()
  "Tool block is followed by blank line."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-agent-start)
    (pi--display-tool-start "bash" '(:command "ls"))
    (pi--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file.txt"))
                          nil nil)
    ;; Pattern: :END: followed by blank line
    (should (string-match-p ":END:\n\n" (buffer-string)))))

(ert-deftest pi-test-spacing-no-double-blank-between-tools ()
  "Consecutive tools have single blank line between them."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-agent-start)
    (pi--display-tool-start "bash" '(:command "ls"))
    (pi--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file1"))
                          nil nil)
    (pi--display-tool-start "read" '(:path "file.txt"))
    ;; Should have exactly one blank line, not two
    (should (string-match-p ":END:\n\n:READ:" (buffer-string)))
    (should-not (string-match-p ":END:\n\n\n" (buffer-string)))))

;;; Streaming Marker

(ert-deftest pi-test-streaming-marker-created-on-agent-start ()
  "Streaming marker is created on agent_start."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-agent-start)
    (should (markerp pi--streaming-marker))
    (should (= (marker-position pi--streaming-marker) (point-max)))))

(ert-deftest pi-test-streaming-marker-advances-with-delta ()
  "Streaming marker advances as deltas are inserted."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-agent-start)
    (let ((initial-pos (marker-position pi--streaming-marker)))
      (pi--display-message-delta "Hello")
      (should (= (marker-position pi--streaming-marker)
                 (+ initial-pos 5))))))

(ert-deftest pi-test-streaming-inserts-at-marker ()
  "Deltas are inserted at the streaming marker position."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-agent-start)
    (pi--display-message-delta "First")
    (pi--display-message-delta " Second")
    (should (string-match-p "First Second" (buffer-string)))))

;;; Auto-scroll

(ert-deftest pi-test-window-following-p-at-end ()
  "pi--window-following-p detects when window-point is at end."
  (with-temp-buffer
    (insert "some content")
    ;; Mock window-point to return point-max
    (cl-letf (((symbol-function 'window-point) (lambda (_w) (point-max))))
      (should (pi--window-following-p 'mock-window)))))

(ert-deftest pi-test-window-following-p-not-at-end ()
  "pi--window-following-p returns nil when window-point is earlier."
  (with-temp-buffer
    (insert "some content")
    ;; Mock window-point to return position before end
    (cl-letf (((symbol-function 'window-point) (lambda (_w) 1)))
      (should-not (pi--window-following-p 'mock-window)))))



;;; Abort Command

(ert-deftest pi-test-abort-sends-command ()
  "pi-abort sends abort command via RPC."
  (with-temp-buffer
    (pi-chat-mode)
    (let ((sent-command nil)
          (pi--status 'streaming))
      (cl-letf (((symbol-function 'pi--get-process) (lambda () 'mock-proc))
                ((symbol-function 'pi--get-chat-buffer) (lambda () (current-buffer)))
                ((symbol-function 'pi--rpc-async)
                 (lambda (_proc cmd _cb) (setq sent-command cmd))))
        (pi-abort)
        (should (equal (plist-get sent-command :type) "abort"))))))

(ert-deftest pi-test-abort-noop-when-not-streaming ()
  "pi-abort does nothing when not streaming."
  (with-temp-buffer
    (pi-chat-mode)
    (let ((sent-command nil)
          (pi--status 'idle))
      (cl-letf (((symbol-function 'pi--get-process) (lambda () 'mock-proc))
                ((symbol-function 'pi--get-chat-buffer) (lambda () (current-buffer)))
                ((symbol-function 'pi--rpc-async)
                 (lambda (_proc cmd _cb) (setq sent-command cmd))))
        (pi-abort)
        (should (null sent-command))))))

;;; Kill Buffer Protection

(ert-deftest pi-test-kill-query-prompts-during-streaming ()
  "Killing chat buffer during streaming prompts for confirmation."
  (with-temp-buffer
    (pi-chat-mode)
    (let ((pi--status 'streaming))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
        (should-not (pi--kill-buffer-query))))))

(ert-deftest pi-test-kill-query-allows-when-idle ()
  "Killing chat buffer when idle is allowed without prompt."
  (with-temp-buffer
    (pi-chat-mode)
    (let ((pi--status 'idle))
      (should (pi--kill-buffer-query)))))

(ert-deftest pi-test-kill-query-allows-non-chat-buffer ()
  "Killing non-chat buffers is always allowed."
  (with-temp-buffer
    (fundamental-mode)
    (should (pi--kill-buffer-query))))

(ert-deftest pi-test-handler-removed-on-kill ()
  "Event handler is removed when chat buffer is killed."
  (with-temp-buffer
    (pi-chat-mode)
    (let ((fake-proc (start-process "test" nil "true")))
      (unwind-protect
          (progn
            (setq pi--process fake-proc)
            (pi--register-display-handler fake-proc)
            (should (process-get fake-proc 'pi-display-handler))
            (pi--cleanup-on-kill)
            (should-not (process-get fake-proc 'pi-display-handler)))
        (when (process-live-p fake-proc)
          (delete-process fake-proc))))))

;;; Pandoc Conversion

(ert-deftest pi-test-pandoc-available ()
  "Pandoc executable is available."
  (should (executable-find "pandoc")))

(ert-deftest pi-test-pandoc-convert-header ()
  "Pandoc converts markdown header to org."
  (skip-unless (executable-find "pandoc"))
  (let ((result (pi--pandoc-convert "# Hello")))
    (should (string-match-p "^\\* Hello" result))))

(ert-deftest pi-test-pandoc-convert-bold ()
  "Pandoc converts markdown bold to org."
  (skip-unless (executable-find "pandoc"))
  (let ((result (pi--pandoc-convert "**bold**")))
    (should (string-match-p "\\*bold\\*" result))))

(ert-deftest pi-test-pandoc-convert-code-block ()
  "Pandoc converts markdown code block to org src block."
  (skip-unless (executable-find "pandoc"))
  (let ((result (pi--pandoc-convert "```python\nprint('hi')\n```")))
    (should (string-match-p "#\\+begin_src python" result))
    (should (string-match-p "#\\+end_src" result))))

(ert-deftest pi-test-pandoc-convert-preserves-code ()
  "Pandoc preserves code content in conversion."
  (skip-unless (executable-find "pandoc"))
  (let ((result (pi--pandoc-convert "```\nfoo bar\n```")))
    (should (string-match-p "foo bar" result))))

;;; Post-Processing

(ert-deftest pi-test-post-process-removes-properties ()
  "Post-processing removes :PROPERTIES: blocks."
  (let* ((org-with-props "* Header
:PROPERTIES:
:CUSTOM_ID: header
:END:
Content")
         (result (pi--post-process-org org-with-props)))
    (should-not (string-match-p ":PROPERTIES:" result))
    (should-not (string-match-p ":CUSTOM_ID:" result))
    (should-not (string-match-p ":END:" result))
    (should (string-match-p "\\* Header" result))
    (should (string-match-p "Content" result))))

(ert-deftest pi-test-post-process-preserves-content ()
  "Post-processing preserves regular content."
  (let* ((org "* Header\nParagraph with *bold* text.\n#+begin_src python\ncode\n#+end_src")
         (result (pi--post-process-org org)))
    (should (string-match-p "\\* Header" result))
    (should (string-match-p "\\*bold\\*" result))
    (should (string-match-p "#\\+begin_src python" result))))

(ert-deftest pi-test-post-process-removes-blank-lines ()
  "Post-processing collapses multiple blank lines."
  (let* ((org "Line 1\n\n\n\nLine 2")
         (result (pi--post-process-org org)))
    (should (string-match-p "Line 1\n\nLine 2" result))
    (should-not (string-match-p "\n\n\n" result))))

;;; Complete Rendering Pipeline

(ert-deftest pi-test-markdown-to-org-pipeline ()
  "Full pipeline converts markdown and post-processes."
  (let ((result (pi--markdown-to-org "# Hello\n\n**Bold** text")))
    (should (string-match-p "^\\* Hello" result))
    (should (string-match-p "\\*Bold\\*" result))
    (should-not (string-match-p ":PROPERTIES:" result))))

(ert-deftest pi-test-message-start-marker-created ()
  "Message start position is tracked for later replacement."
  (with-temp-buffer
    (pi-chat-mode)
    (let ((inhibit-read-only t))
      (insert "Previous content\n"))
    (pi--display-agent-start)
    (should (markerp pi--message-start-marker))
    (should (= (marker-position pi--message-start-marker)
               (marker-position pi--streaming-marker)))))

(ert-deftest pi-test-render-complete-message-replaces-content ()
  "Rendering replaces raw markdown with Org."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-agent-start)
    (pi--display-message-delta "# Hello\n\n**Bold**")
    ;; Raw markdown should be present
    (should (string-match-p "# Hello" (buffer-string)))
    ;; Now render
    (pi--render-complete-message)
    ;; Raw markdown should be replaced with Org
    (should (string-match-p "\\* Hello" (buffer-string)))
    (should (string-match-p "\\*Bold\\*" (buffer-string)))
    (should-not (string-match-p "# Hello" (buffer-string)))))

;;; Org Marker Hiding

(ert-deftest pi-test-hide-org-markers-creates-overlays ()
  "Hiding org markers creates invisible overlays."
  (with-temp-buffer
    (insert "#+begin_src python\ncode\n#+end_src\n")
    (pi--hide-org-markers (point-min) (point-max))
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (>= (length overlays) 2)))))

(ert-deftest pi-test-hide-org-markers-makes-invisible ()
  "Hidden markers have invisible property."
  (with-temp-buffer
    (insert "#+begin_src python\ncode\n#+end_src\n")
    (pi--hide-org-markers (point-min) (point-max))
    (let ((overlays (overlays-in (point-min) (point-max))))
      (should (cl-some (lambda (ov) (overlay-get ov 'invisible)) overlays)))))

(ert-deftest pi-test-hide-org-markers-preserves-code ()
  "Code content is not hidden, only markers."
  (with-temp-buffer
    (insert "#+begin_src python\nmy_code()\n#+end_src\n")
    (pi--hide-org-markers (point-min) (point-max))
    ;; Find position of code
    (goto-char (point-min))
    (search-forward "my_code()")
    (let ((code-pos (match-beginning 0)))
      ;; No overlay should cover the code itself
      (should-not (cl-some (lambda (ov) (overlay-get ov 'invisible))
                           (overlays-at code-pos))))))

;;; Error Handling

(ert-deftest pi-test-pandoc-fallback-on-error ()
  "Conversion falls back to raw content on error."
  (cl-letf (((symbol-function 'pi--pandoc-convert)
             (lambda (_) (error "Pandoc failed"))))
    (let ((result (pi--markdown-to-org-safe "# Hello")))
      (should (equal result "# Hello")))))

(ert-deftest pi-test-pandoc-fallback-returns-original ()
  "Fallback returns original markdown unchanged."
  (cl-letf (((symbol-function 'pi--pandoc-convert)
             (lambda (_) (error "Pandoc unavailable"))))
    (let ((markdown "**Bold** and `code`"))
      (should (equal (pi--markdown-to-org-safe markdown) markdown)))))

;;; Syntax Highlighting

(ert-deftest pi-test-chat-mode-derives-from-org ()
  "Chat mode derives from org-mode for syntax highlighting."
  (with-temp-buffer
    (pi-chat-mode)
    (should (derived-mode-p 'org-mode))))

(ert-deftest pi-test-chat-mode-fontifies-code ()
  "Code blocks get syntax highlighting."
  (with-temp-buffer
    (pi-chat-mode)
    (let ((inhibit-read-only t)
          (org-src-fontify-natively t))
      (insert "#+begin_src python\ndef hello():\n    return 42\n#+end_src\n")
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "def" nil t)
      (let ((face (get-text-property (match-beginning 0) 'face)))
        ;; Should have font-lock-keyword-face (from python)
        (should (or (eq face 'font-lock-keyword-face)
                    (and (listp face) (memq 'font-lock-keyword-face face))))))))

;;; User Message Display

(ert-deftest pi-test-display-user-message-inserts-text ()
  "User message is inserted into chat buffer."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-user-message "Hello world")
    (should (string-match-p "Hello world" (buffer-string)))))

(ert-deftest pi-test-display-user-message-has-prefix ()
  "User message has You label in separator."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-user-message "Test message")
    (should (string-match-p " You " (buffer-string)))))

(ert-deftest pi-test-display-user-message-has-separator ()
  "User message has visual separator."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-user-message "Test")
    (should (string-match-p "─" (buffer-string)))))

(ert-deftest pi-test-send-displays-user-message ()
  "Sending a prompt displays the user message in chat."
  (let ((chat-buf (get-buffer-create "*pi-test-chat*"))
        (input-buf (get-buffer-create "*pi-test-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-chat-mode)
            (setq pi--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-input-mode)
            (setq pi--chat-buffer chat-buf)
            (insert "Hello from test")
            ;; Mock the process to avoid actual RPC
            (setq pi--process nil)
            (pi-send))
          ;; Check chat buffer has the message with You separator and content
          (with-current-buffer chat-buf
            (should (string-match-p " You " (buffer-string)))
            (should (string-match-p "Hello from test" (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

;;; Startup Header

(ert-deftest pi-test-startup-header-shows-version ()
  "Startup header includes version."
  (let ((header (pi--format-startup-header)))
    (should (string-match-p "Pi" header))))

(ert-deftest pi-test-startup-header-shows-keybindings ()
  "Startup header includes key keybindings."
  (let ((header (pi--format-startup-header)))
    (should (string-match-p "C-c C-c" header))
    (should (string-match-p "send" header))))

(ert-deftest pi-test-startup-header-shows-pi-version ()
  "Startup header includes pi CLI version."
  (let ((header (pi--format-startup-header)))
    ;; Should show "Pi X.Y.Z" or "Pi Unknown" in separator
    (should (string-match-p "Pi " header))))

;;; Tool Output

(ert-deftest pi-test-tool-start-inserts-header ()
  "tool_execution_start inserts an org drawer header."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-tool-start "bash" (list :command "ls -la"))
    ;; Should have :BASH: drawer and command
    (should (string-match-p ":BASH:" (buffer-string)))
    (should (string-match-p "ls -la" (buffer-string)))))

(ert-deftest pi-test-tool-start-handles-file-path-key ()
  "tool_execution_start handles :file_path key (alternative to :path)."
  (with-temp-buffer
    (pi-chat-mode)
    ;; Test read tool with :file_path
    (pi--display-tool-start "read" '(:file_path "/tmp/test.txt"))
    (should (string-match-p ":READ:" (buffer-string)))
    (should (string-match-p "/tmp/test.txt" (buffer-string))))
  (with-temp-buffer
    (pi-chat-mode)
    ;; Test write tool with :file_path
    (pi--display-tool-start "write" '(:file_path "/tmp/out.py"))
    (should (string-match-p ":WRITE:" (buffer-string)))
    (should (string-match-p "/tmp/out.py" (buffer-string))))
  (with-temp-buffer
    (pi-chat-mode)
    ;; Test edit tool with :file_path
    (pi--display-tool-start "edit" '(:file_path "/tmp/edit.rs"))
    (should (string-match-p ":EDIT:" (buffer-string)))
    (should (string-match-p "/tmp/edit.rs" (buffer-string)))))

(ert-deftest pi-test-tool-end-inserts-result ()
  "tool_execution_end inserts the tool result."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file1\nfile2"))
                          nil nil)
    (should (string-match-p "file1" (buffer-string)))))

(ert-deftest pi-test-tool-output-shows-preview-when-long ()
  "Tool output shows preview lines when it exceeds the limit."
  (with-temp-buffer
    (pi-chat-mode)
    (let ((long-output (mapconcat (lambda (n) (format "line%d" n))
                                  (number-sequence 1 10)
                                  "\n")))
      (pi--display-tool-end "bash" '(:command "ls")
                            `((:type "text" :text ,long-output))
                            nil nil)
      ;; Should have first 5 preview lines (bash limit)
      (should (string-match-p "line1" (buffer-string)))
      (should (string-match-p "line5" (buffer-string)))
      ;; Should have more-lines indicator
      (should (string-match-p "more lines" (buffer-string)))
      ;; Should have :END: to close drawer
      (should (string-match-p ":END:" (buffer-string))))))

(ert-deftest pi-test-tool-output-has-drawer-end ()
  "Tool output closes with :END: drawer marker."
  (with-temp-buffer
    (pi-chat-mode)
    (let ((short-output "file1\nfile2"))
      (pi--display-tool-end "bash" '(:command "ls")
                            `((:type "text" :text ,short-output))
                            nil nil)
      ;; Should have :END: marker
      (should (string-match-p ":END:" (buffer-string))))))

(ert-deftest pi-test-tool-output-short-shows-all ()
  "Short tool output shows all lines without truncation."
  (with-temp-buffer
    (pi-chat-mode)
    (let ((short-output "line1\nline2\nline3"))
      (pi--display-tool-end "bash" '(:command "ls")
                            `((:type "text" :text ,short-output))
                            nil nil)
      ;; Should have all lines
      (should (string-match-p "line1" (buffer-string)))
      (should (string-match-p "line2" (buffer-string)))
      (should (string-match-p "line3" (buffer-string)))
      ;; Should NOT have more-lines indicator
      (should-not (string-match-p "more lines" (buffer-string))))))

(ert-deftest pi-test-tab-calls-org-cycle ()
  "TAB on tool section calls `org-cycle' for drawer folding."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-tool-start "bash" '(:command "ls"))
    (pi--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "output"))
                          nil nil)
    ;; Verify we have an org drawer structure
    (should (string-match-p ":BASH:" (buffer-string)))
    (should (string-match-p ":END:" (buffer-string)))
    ;; pi-toggle-tool-section should be bound to TAB and <tab>
    (should (eq (lookup-key pi-chat-mode-map (kbd "TAB")) 'pi-toggle-tool-section))
    (should (eq (lookup-key pi-chat-mode-map (kbd "<tab>")) 'pi-toggle-tool-section))))

(ert-deftest pi-test-tool-error-indicated ()
  "Tool error is indicated in output."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-tool-end "bash" '(:command "false")
                          '((:type "text" :text "error message"))
                          nil t)
    (should (string-match-p "error" (buffer-string)))))

(ert-deftest pi-test-tool-success-not-error ()
  "Tool with isError :false should not show error indicator."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-tool-end "bash" nil
                          '((:type "text" :text "success output"))
                          nil :false)
    ;; Should have output and :END:, no [error]
    (should (string-match-p "success output" (buffer-string)))
    (should (string-match-p ":END:" (buffer-string)))
    (should-not (string-match-p "\\[error\\]" (buffer-string)))))

(ert-deftest pi-test-tool-output-survives-message-render ()
  "Tool output should not be clobbered by subsequent message rendering."
  (with-temp-buffer
    (pi-chat-mode)
    ;; Simulate: message -> tool -> message sequence
    (pi--handle-display-event '(:type "agent_start"))
    (pi--handle-display-event '(:type "message_start"))
    (pi--handle-display-event 
     '(:type "message_update" 
       :assistantMessageEvent (:type "text_delta" :delta "Running")))
    (pi--handle-display-event '(:type "message_end"))
    
    (pi--handle-display-event 
     '(:type "tool_execution_start" :toolName "bash" :args (:command "ls")))
    (pi--handle-display-event 
     '(:type "tool_execution_end" :toolName "bash"
       :result (:content ((:type "text" :text "file1\nfile2")))))
    
    ;; Second message should NOT clobber tool output
    (pi--handle-display-event '(:type "message_start"))
    (pi--handle-display-event 
     '(:type "message_update"
       :assistantMessageEvent (:type "text_delta" :delta "Done")))
    (pi--handle-display-event '(:type "message_end"))
    
    ;; Tool output must still be present (in drawer)
    (should (string-match-p "file1" (buffer-string)))
    (should (string-match-p "file2" (buffer-string)))
    (should (string-match-p ":END:" (buffer-string)))))

(ert-deftest pi-test-display-handler-handles-tool-start ()
  "Display handler processes tool_execution_start events."
  (with-temp-buffer
    (pi-chat-mode)
    (let ((event (list :type "tool_execution_start"
                       :toolName "bash"
                       :args (list :command "echo hello"))))
      (pi--handle-display-event event)
      (should (string-match-p "echo hello" (buffer-string))))))

(ert-deftest pi-test-display-handler-handles-tool-end ()
  "Display handler processes tool_execution_end events."
  (with-temp-buffer
    (pi-chat-mode)
    (let ((event (list :type "tool_execution_end"
                       :toolName "bash"
                       :args (list :command "ls")
                       :result (list :content '((:type "text" :text "output")))
                       :isError nil)))
      (pi--handle-display-event event)
      (should (string-match-p "output" (buffer-string))))))

(ert-deftest pi-test-display-handler-handles-thinking-delta ()
  "Display handler processes thinking_delta events."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--handle-display-event '(:type "agent_start"))
    (pi--handle-display-event '(:type "message_start"))
    (pi--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_delta" :delta "Analyzing...")))
    (should (string-match-p "Analyzing..." (buffer-string)))))

(ert-deftest pi-test-thinking-rendered-as-src-block ()
  "Thinking content renders as org src thinking block after message completion."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--handle-display-event '(:type "agent_start"))
    (pi--handle-display-event '(:type "message_start"))
    ;; Thinking lifecycle: start -> delta -> end
    (pi--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_start")))
    (pi--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_delta" :delta "Let me analyze this.")))
    (pi--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "thinking_end" :content "Let me analyze this.")))
    ;; Then regular text
    (pi--handle-display-event
     '(:type "message_update"
       :assistantMessageEvent (:type "text_delta" :delta "Here is my answer.")))
    ;; Complete the message (triggers rendering)
    (pi--handle-display-event '(:type "message_end" :message (:role "assistant")))
    ;; After rendering, thinking should be in a src thinking block
    (goto-char (point-min))
    (should (search-forward "#+begin_src thinking" nil t))
    (should (search-forward "Let me analyze this." nil t))
    (should (search-forward "#+end_src" nil t))
    ;; Regular text should be outside the block
    (should (search-forward "Here is my answer." nil t))))

(ert-deftest pi-test-thinking-block-has-face ()
  "Thinking block content has pi-thinking face after font-lock."
  (with-temp-buffer
    (pi-chat-mode)
    (let ((inhibit-read-only t))
      (insert "#+begin_src thinking\nSome thinking here.\n#+end_src\n"))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "Some thinking")
    (should (memq 'pi-thinking (ensure-list (get-text-property (1- (point)) 'face))))))

(ert-deftest pi-test-read-tool-gets-syntax-highlighting ()
  "Read tool output gets syntax highlighting based on file path.
The toolCallId is used to correlate start/end events since args
are only present in tool_execution_start, not tool_execution_end."
  (with-temp-buffer
    (pi-chat-mode)
    ;; Start event has args with path
    (pi--handle-display-event
     (list :type "tool_execution_start"
           :toolCallId "call_123"
           :toolName "read"
           :args (list :path "example.py")))
    ;; End event does NOT have args (matches real pi behavior)
    (pi--handle-display-event
     (list :type "tool_execution_end"
           :toolCallId "call_123"
           :toolName "read"
           :result (list :content '((:type "text" :text "def hello():\n    pass")))
           :isError nil))
    ;; Should have python src block from pandoc conversion
    (should (string-match-p "#\\+begin_src python" (buffer-string)))))

(ert-deftest pi-test-write-tool-gets-syntax-highlighting ()
  "Write tool displays content from args with syntax highlighting.
The content to display comes from args, not from the result
which is just a success message."
  (with-temp-buffer
    (pi-chat-mode)
    ;; Start event has args with path and content
    (pi--handle-display-event
     (list :type "tool_execution_start"
           :toolCallId "call_456"
           :toolName "write"
           :args (list :path "example.rs"
                       :content "fn main() {\n    println!(\"Hello\");\n}")))
    ;; End event has only success message in result
    (pi--handle-display-event
     (list :type "tool_execution_end"
           :toolCallId "call_456"
           :toolName "write"
           :result (list :content '((:type "text" :text "Successfully wrote 42 bytes")))
           :isError nil))
    ;; Should have rust src block (from args content, not result)
    (should (string-match-p "#\\+begin_src rust" (buffer-string)))
    ;; Should show the actual code, not the success message
    (should (string-match-p "fn main" (buffer-string)))))

;;; Transient Menu

(ert-deftest pi-test-transient-bound-to-key ()
  "C-c C-p is bound to pi-menu in input mode."
  (with-temp-buffer
    (pi-input-mode)
    (should (eq (key-binding (kbd "C-c C-p")) 'pi-menu))))

;;; Chat Navigation

(ert-deftest pi-test-chat-has-navigation-keys ()
  "Chat mode has n/p for navigation and TAB for folding."
  (with-temp-buffer
    (pi-chat-mode)
    (should (eq (key-binding "n") 'pi-next-message))
    (should (eq (key-binding "p") 'pi-previous-message))
    (should (eq (key-binding (kbd "TAB")) 'pi-toggle-tool-section))))

;;; Dependency Checking

(ert-deftest pi-test-check-pandoc-returns-status ()
  "pi--check-pandoc returns boolean."
  (should (booleanp (pi--check-pandoc))))

(provide 'pi-test)
;;; pi-test.el ends here

(ert-deftest pi-test-tool-toggle-expands-content ()
  "Toggle button expands collapsed tool output."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-tool-start "bash" '(:command "ls"))
    (pi--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Initially collapsed - should have "... (N more lines)"
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    (should-not (string-match-p "L10" (buffer-string)))
    ;; Find and click the button
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (pi-toggle-tool-section)
    ;; Now should show all lines
    (should (string-match-p "L10" (buffer-string)))
    (should (string-match-p "\\[-\\]" (buffer-string)))))

(ert-deftest pi-test-tool-toggle-collapses-content ()
  "Toggle button collapses expanded tool output."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-tool-start "bash" '(:command "ls"))
    (pi--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Expand first
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (pi-toggle-tool-section)
    (should (string-match-p "L10" (buffer-string)))
    ;; Now collapse
    (goto-char (point-min))
    (search-forward "[-]" nil t)
    (backward-char 1)
    (pi-toggle-tool-section)
    ;; Should be collapsed again
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    (should-not (string-match-p "L10" (buffer-string)))))

(ert-deftest pi-test-tab-works-from-anywhere-in-block ()
  "TAB toggles tool output from any position within the block."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-tool-start "bash" '(:command "ls"))
    (pi--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Go to the :BASH: line (not the button)
    (goto-char (point-min))
    (search-forward ":BASH:" nil t)
    (beginning-of-line)
    ;; TAB should still expand
    (pi-toggle-tool-section)
    (should (string-match-p "L10" (buffer-string)))))

(ert-deftest pi-test-tab-preserves-cursor-position ()
  "TAB toggle doesn't jump cursor unnecessarily."
  (with-temp-buffer
    (pi-chat-mode)
    (pi--display-tool-start "bash" '(:command "ls"))
    (pi--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Go to L3 line
    (goto-char (point-min))
    (search-forward "L3" nil t)
    (beginning-of-line)
    (let ((line-content (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
      ;; Expand
      (pi-toggle-tool-section)
      ;; Should still be on a line starting with L
      (should (string-match-p "^L[0-9]" 
                              (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position)))))))

(ert-deftest pi-test-format-branch-message ()
  "Branch message formatted with index and preview."
  (let ((msg '(:entryIndex 2 :text "Hello world, this is a test")))
    (let ((result (pi--format-branch-message msg)))
      (should (string-match-p "2:" result))
      (should (string-match-p "Hello world" result)))))

(ert-deftest pi-test-session-dir-name ()
  "Session directory name derived from project path."
  (should (equal (pi--session-dir-name "/home/daniel/co/pi.el")
                 "--home-daniel-co-pi.el--"))
  (should (equal (pi--session-dir-name "/tmp/test")
                 "--tmp-test--")))

(ert-deftest pi-test-session-metadata-extracts-first-message ()
  "pi--session-metadata extracts first user message text."
  (let ((temp-file (make-temp-file "pi-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n")
            (insert "{\"type\":\"message\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Hello world\"}]}}\n")
            (insert "{\"type\":\"message\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"Hi\"}]}}\n"))
          (let ((metadata (pi--session-metadata temp-file)))
            (should metadata)
            (should (equal (plist-get metadata :first-message) "Hello world"))
            (should (equal (plist-get metadata :message-count) 2))))
      (delete-file temp-file))))

(ert-deftest pi-test-session-metadata-returns-nil-for-empty-file ()
  "pi--session-metadata returns nil for empty or invalid files."
  (let ((temp-file (make-temp-file "pi-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          ;; Empty file
          (should (null (pi--session-metadata temp-file))))
      (delete-file temp-file))))

(ert-deftest pi-test-session-metadata-handles-missing-first-message ()
  "pi--session-metadata handles session with only header."
  (let ((temp-file (make-temp-file "pi-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n"))
          (let ((metadata (pi--session-metadata temp-file)))
            (should metadata)
            (should (null (plist-get metadata :first-message)))
            (should (equal (plist-get metadata :message-count) 0))))
      (delete-file temp-file))))

;;; Input History

(ert-deftest pi-test-history-add-to-ring ()
  "pi--history-add adds input to ring."
  (let ((pi--input-ring nil))
    (pi--history-add "first")
    (pi--history-add "second")
    (should (equal (ring-ref (pi--input-ring) 0) "second"))
    (should (equal (ring-ref (pi--input-ring) 1) "first"))))

(ert-deftest pi-test-history-no-duplicate ()
  "pi--history-add skips duplicates of last entry."
  (let ((pi--input-ring nil))
    (pi--history-add "first")
    (pi--history-add "first")
    (should (= (ring-length (pi--input-ring)) 1))))

(ert-deftest pi-test-history-skip-empty ()
  "pi--history-add skips empty input."
  (let ((pi--input-ring nil))
    (pi--history-add "")
    (pi--history-add "   ")
    (should (ring-empty-p (pi--input-ring)))))

(ert-deftest pi-test-history-previous-input ()
  "pi-previous-input navigates backward through history."
  (let ((pi--input-ring nil))
    (pi--history-add "first")
    (pi--history-add "second")
    (with-temp-buffer
      (pi-input-mode)
      (insert "current")
      (pi-previous-input)
      (should (equal (buffer-string) "second"))
      (should (equal pi--input-saved "current"))
      (pi-previous-input)
      (should (equal (buffer-string) "first")))))

(ert-deftest pi-test-history-next-input ()
  "pi-next-input navigates forward and restores saved input."
  (let ((pi--input-ring nil))
    (pi--history-add "first")
    (pi--history-add "second")
    (with-temp-buffer
      (pi-input-mode)
      (insert "current")
      (pi-previous-input)
      (pi-previous-input)
      (should (equal (buffer-string) "first"))
      (pi-next-input)
      (should (equal (buffer-string) "second"))
      (pi-next-input)
      (should (equal (buffer-string) "current")))))

(ert-deftest pi-test-history-keys-bound ()
  "History keys are bound in pi-input-mode."
  (with-temp-buffer
    (pi-input-mode)
    (should (eq (key-binding (kbd "M-p")) 'pi-previous-input))
    (should (eq (key-binding (kbd "M-n")) 'pi-next-input))
    (should (eq (key-binding (kbd "C-r")) 'pi-history-search))))

;;; Input Buffer Slash Completion

(ert-deftest pi-test-slash-capf-returns-nil-without-slash ()
  "Completion returns nil when not after slash."
  (with-temp-buffer
    (pi-input-mode)
    (insert "hello")
    (should-not (pi--slash-capf))))

(ert-deftest pi-test-slash-capf-returns-completion-data ()
  "Completion returns data when after slash at start of line."
  (with-temp-buffer
    (pi-input-mode)
    (setq pi--file-commands '((:name "test-cmd" :description "Test")))
    (insert "/te")
    (let ((result (pi--slash-capf)))
      (should result)
      (should (= (nth 0 result) 2))  ; Start after /
      (should (= (nth 1 result) 4))  ; End at point
      (should (member "test-cmd" (nth 2 result))))))

;;; Input Buffer Slash Execution

(ert-deftest pi-test-expand-slash-command-known ()
  "Known slash command expands to content with args."
  (let ((pi--file-commands '((:name "test" :content "Do $@ please"))))
    (should (equal (pi--expand-slash-command "/test foo bar")
                   "Do foo bar please"))))

(ert-deftest pi-test-expand-slash-command-unknown ()
  "Unknown slash command returns original text."
  (let ((pi--file-commands '()))
    (should (equal (pi--expand-slash-command "/unknown foo")
                   "/unknown foo"))))

(ert-deftest pi-test-expand-slash-command-not-slash ()
  "Non-slash text returns unchanged."
  (let ((pi--file-commands '((:name "test" :content "content"))))
    (should (equal (pi--expand-slash-command "hello world")
                   "hello world"))))

(ert-deftest pi-test-send-expands-slash-command ()
  "pi-send expands slash commands before sending."
  (let* ((sent-text nil)
         (pi--file-commands '((:name "greet" :content "Hello $@!"))))
    (cl-letf (((symbol-function 'pi--get-chat-buffer)
               (lambda () (current-buffer)))
              ((symbol-function 'pi--display-user-message)
               (lambda (_) nil))
              ((symbol-function 'pi--send-prompt)
               (lambda (text) (setq sent-text text))))
      (with-temp-buffer
        (pi-input-mode)
        (insert "/greet world")
        (pi-send)
        (should (equal sent-text "Hello world!"))))))

(ert-deftest pi-test-format-session-stats ()
  "Format session stats returns readable string."
  (let ((stats '(:tokens (:input 50000 :output 10000 :total 60000)
                 :cost 0.45
                 :userMessages 5
                 :toolCalls 12)))
    (let ((result (pi--format-session-stats stats)))
      (should (string-match-p "50,000" result))  ; input tokens formatted
      (should (string-match-p "\\$0.45" result)) ; cost
      (should (string-match-p "5" result)))))    ; messages

(ert-deftest pi-test-header-line-shows-model ()
  "Header line displays current model."
  (with-temp-buffer
    (pi-chat-mode)
    (setq pi--state '(:model "claude-sonnet-4"))
    (let ((header (pi--header-line-string)))
      (should (string-match-p "sonnet-4" header)))))

(ert-deftest pi-test-header-line-shows-thinking ()
  "Header line displays thinking level."
  (with-temp-buffer
    (pi-chat-mode)
    (setq pi--state '(:model "claude-sonnet-4" :thinking-level "high"))
    (let ((header (pi--header-line-string)))
      (should (string-match-p "high" header)))))

(ert-deftest pi-test-header-line-shows-streaming-indicator ()
  "Header line shows spinner when streaming."
  (with-temp-buffer
    (pi-chat-mode)
    (setq pi--state '(:model "claude-sonnet-4"))
    (pi--spinner-start)
    (unwind-protect
        (let ((header (pi--header-line-string)))
          (should (string-match-p "[⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]" header)))
      (pi--spinner-stop))))

(ert-deftest pi-test-format-tokens-compact ()
  "Tokens formatted compactly."
  (should (equal "500" (pi--format-tokens-compact 500)))
  (should (equal "5k" (pi--format-tokens-compact 5000)))
  (should (equal "50k" (pi--format-tokens-compact 50000)))
  (should (equal "1.2M" (pi--format-tokens-compact 1200000))))

(ert-deftest pi-test-input-mode-has-header-line ()
  "Input mode sets up header-line-format."
  (with-temp-buffer
    (pi-input-mode)
    (should header-line-format)))

(ert-deftest pi-test-header-line-handles-model-plist ()
  "Header line handles model as plist with :name."
  (with-temp-buffer
    (pi-chat-mode)
    (setq pi--state '(:model (:name "claude-sonnet-4" :id "model-123")))
    (let ((header (pi--header-line-string)))
      (should (string-match-p "sonnet-4" header)))))

(ert-deftest pi-test-update-state-refreshes-header ()
  "Updating state should trigger header-line refresh."
  (with-temp-buffer
    (pi-chat-mode)
    (setq pi--state '(:model (:name "old-model") :thinking-level "low"))
    (let ((header-before (pi--header-line-string)))
      ;; Simulate state update
      (setq pi--state '(:model (:name "new-model") :thinking-level "high"))
      (let ((header-after (pi--header-line-string)))
        ;; Header string should reflect new state
        (should (string-match-p "new-model" header-after))
        (should (string-match-p "high" header-after))))))

(ert-deftest pi-test-header-line-model-is-clickable ()
  "Model name in header-line has click properties."
  (with-temp-buffer
    (pi-chat-mode)
    (setq pi--state '(:model (:name "claude-sonnet-4")))
    (let ((header (pi--header-line-string)))
      ;; Should have local-map property
      (should (get-text-property 0 'local-map header))
      ;; Should have mouse-face for highlight
      (should (get-text-property 0 'mouse-face header)))))

(ert-deftest pi-test-header-line-thinking-is-clickable ()
  "Thinking level in header-line has click properties."
  (with-temp-buffer
    (pi-chat-mode)
    (setq pi--state '(:model (:name "test") :thinking-level "high"))
    (let* ((header (pi--header-line-string))
           ;; Find position of "high" in header
           (pos (string-match "high" header)))
      (should pos)
      ;; Should have local-map at that position
      (should (get-text-property pos 'local-map header))
      ;; Should have mouse-face for highlight
      (should (get-text-property pos 'mouse-face header)))))

(ert-deftest pi-test-header-format-context-returns-nil-when-no-window ()
  "Context format returns nil when context window is 0."
  (should (null (pi--header-format-context 1000 0))))

(ert-deftest pi-test-header-format-context-shows-percentage ()
  "Context format shows percentage and window size."
  (let ((result (pi--header-format-context 50000 200000)))
    (should (string-match-p "25.0%%" result))
    (should (string-match-p "200k" result))))

(ert-deftest pi-test-header-format-context-warning-over-70 ()
  "Context format uses warning face over 70%."
  (let ((result (pi--header-format-context 150000 200000)))
    (should (eq (get-text-property 0 'face result) 'warning))))

(ert-deftest pi-test-header-format-context-error-over-90 ()
  "Context format uses error face over 90%."
  (let ((result (pi--header-format-context 190000 200000)))
    (should (eq (get-text-property 0 'face result) 'error))))

(ert-deftest pi-test-message-end-updates-usage-for-normal-completion ()
  "message_end with stopReason=stop updates pi--last-usage."
  (with-temp-buffer
    (pi-chat-mode)
    (setq pi--last-usage nil)
    (pi--handle-display-event
     '(:type "message_end"
       :message (:role "assistant"
                 :stopReason "stop"
                 :usage (:input 1000 :output 500 :cacheRead 200 :cacheWrite 50))))
    (should pi--last-usage)
    (should (equal (plist-get pi--last-usage :input) 1000))))

(ert-deftest pi-test-message-end-updates-usage-for-tool-use ()
  "message_end with stopReason=toolUse updates pi--last-usage."
  (with-temp-buffer
    (pi-chat-mode)
    (setq pi--last-usage nil)
    (pi--handle-display-event
     '(:type "message_end"
       :message (:role "assistant"
                 :stopReason "toolUse"
                 :usage (:input 2000 :output 300 :cacheRead 100 :cacheWrite 25))))
    (should pi--last-usage)
    (should (equal (plist-get pi--last-usage :input) 2000))))

(ert-deftest pi-test-message-end-skips-usage-for-aborted ()
  "message_end with stopReason=aborted does NOT update pi--last-usage.
This preserves the previous valid usage for context percentage display."
  (with-temp-buffer
    (pi-chat-mode)
    ;; Set up previous valid usage
    (setq pi--last-usage '(:input 5000 :output 1000 :cacheRead 500 :cacheWrite 100))
    ;; Process an aborted message with zeroed/incomplete usage
    (pi--handle-display-event
     '(:type "message_end"
       :message (:role "assistant"
                 :stopReason "aborted"
                 :usage (:input 0 :output 0 :cacheRead 0 :cacheWrite 0))))
    ;; Should preserve previous usage, not overwrite with zeros
    (should (equal (plist-get pi--last-usage :input) 5000))))

(ert-deftest pi-test-message-end-updates-usage-for-error ()
  "message_end with stopReason=error updates pi--last-usage.
Errors still consume context, so their usage data is valid for display."
  (with-temp-buffer
    (pi-chat-mode)
    (setq pi--last-usage nil)
    (pi--handle-display-event
     '(:type "message_end"
       :message (:role "assistant"
                 :stopReason "error"
                 :usage (:input 3000 :output 100 :cacheRead 400 :cacheWrite 0))))
    (should pi--last-usage)
    (should (equal (plist-get pi--last-usage :input) 3000))))

(ert-deftest pi-test-header-format-stats-returns-nil-when-no-stats ()
  "Stats format returns nil when stats is nil."
  (should (null (pi--header-format-stats nil nil nil))))

(ert-deftest pi-test-header-format-stats-shows-tokens ()
  "Stats format shows token counts."
  (let* ((stats '(:tokens (:input 1000 :output 500 :cacheRead 2000 :cacheWrite 100)
                  :cost 0.05))
         (result (pi--header-format-stats stats nil nil)))
    (should (string-match-p "↑1k" result))
    (should (string-match-p "↓500" result))
    (should (string-match-p "R2k" result))
    (should (string-match-p "W100" result))
    (should (string-match-p "\\$0.05" result))))

;;; Slash Command Argument Parsing

(ert-deftest pi-test-parse-args-simple ()
  "Parse simple space-separated arguments."
  (should (equal (pi--parse-command-args "foo bar baz")
                 '("foo" "bar" "baz"))))

(ert-deftest pi-test-parse-args-empty ()
  "Parse empty string returns empty list."
  (should (equal (pi--parse-command-args "") '())))

(ert-deftest pi-test-parse-args-whitespace-only ()
  "Parse whitespace-only string returns empty list."
  (should (equal (pi--parse-command-args "   ") '())))

(ert-deftest pi-test-parse-args-quoted-double ()
  "Parse double-quoted arguments preserves spaces."
  (should (equal (pi--parse-command-args "foo \"bar baz\" qux")
                 '("foo" "bar baz" "qux"))))

(ert-deftest pi-test-parse-args-quoted-single ()
  "Parse single-quoted arguments preserves spaces."
  (should (equal (pi--parse-command-args "foo 'bar baz' qux")
                 '("foo" "bar baz" "qux"))))

(ert-deftest pi-test-parse-args-extra-whitespace ()
  "Parse handles multiple spaces between arguments."
  (should (equal (pi--parse-command-args "foo   bar")
                 '("foo" "bar"))))

;;; Slash Command Argument Substitution

(ert-deftest pi-test-substitute-args-positional ()
  "Substitute positional arguments $1, $2, etc."
  (should (equal (pi--substitute-args "Hello $1 and $2" '("world" "friend"))
                 "Hello world and friend")))

(ert-deftest pi-test-substitute-args-all ()
  "Substitute $@ with all arguments joined."
  (should (equal (pi--substitute-args "Args: $@" '("one" "two" "three"))
                 "Args: one two three")))

(ert-deftest pi-test-substitute-args-missing ()
  "Missing positional arguments become empty string."
  (should (equal (pi--substitute-args "Hello $1 and $2" '("world"))
                 "Hello world and ")))

(ert-deftest pi-test-substitute-args-no-placeholders ()
  "Content without placeholders returned unchanged."
  (should (equal (pi--substitute-args "Hello world" '("ignored"))
                 "Hello world")))

;;; Slash Command Frontmatter Parsing

(ert-deftest pi-test-parse-frontmatter-with-description ()
  "Parse YAML frontmatter with description."
  (let ((result (pi--parse-frontmatter "---
description: Test command
---
Command content here")))
    (should (equal (plist-get result :description) "Test command"))
    (should (equal (plist-get result :content) "Command content here"))))

(ert-deftest pi-test-parse-frontmatter-no-frontmatter ()
  "Content without frontmatter returns full content."
  (let ((result (pi--parse-frontmatter "Just content\nNo frontmatter")))
    (should (null (plist-get result :description)))
    (should (equal (plist-get result :content) "Just content\nNo frontmatter"))))

(ert-deftest pi-test-parse-frontmatter-empty-description ()
  "Frontmatter with empty description."
  (let ((result (pi--parse-frontmatter "---
description:
---
Content")))
    (should (equal (plist-get result :description) ""))
    (should (equal (plist-get result :content) "Content"))))

;;; Slash Command Discovery

(ert-deftest pi-test-discover-commands-from-directory ()
  "Discover commands from a directory with .md files."
  (let* ((temp-dir (make-temp-file "pi-commands-" t))
         (cmd-file (expand-file-name "test-cmd.md" temp-dir)))
    (unwind-protect
        (progn
          (with-temp-file cmd-file
            (insert "---\ndescription: A test command\n---\nDo the thing"))
          (let ((commands (pi--load-commands-from-dir temp-dir "user")))
            (should (= (length commands) 1))
            (let ((cmd (car commands)))
              (should (equal (plist-get cmd :name) "test-cmd"))
              (should (string-match-p "test command" (plist-get cmd :description)))
              (should (equal (plist-get cmd :content) "Do the thing")))))
      (delete-directory temp-dir t))))

(ert-deftest pi-test-discover-commands-missing-directory ()
  "Missing directory returns empty list."
  (should (equal (pi--load-commands-from-dir "/nonexistent/path" "user") '())))

(ert-deftest pi-test-discover-commands-description-from-content ()
  "When no frontmatter description, use first line of content."
  (let* ((temp-dir (make-temp-file "pi-commands-" t))
         (cmd-file (expand-file-name "no-desc.md" temp-dir)))
    (unwind-protect
        (progn
          (with-temp-file cmd-file
            (insert "First line becomes description\nMore content"))
          (let* ((commands (pi--load-commands-from-dir temp-dir "user"))
                 (cmd (car commands)))
            (should (string-match-p "First line" (plist-get cmd :description)))))
      (delete-directory temp-dir t))))
