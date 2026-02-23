;;; pi-coding-agent-ui-test.el --- Tests for pi-coding-agent-ui -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>

;;; Commentary:

;; Tests for buffer naming, creation, major modes, session directory,
;; buffer linkage, and startup header — the UI foundation layer.

;;; Code:

(require 'ert)
(require 'pi-coding-agent)
(require 'pi-coding-agent-test-common)

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
  "pi-coding-agent-input-mode derives from text-mode, not gfm-mode by default."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should (derived-mode-p 'text-mode))
    (should-not (derived-mode-p 'gfm-mode))))

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

(ert-deftest pi-coding-agent-test-display-buffers-uses-current-frame-window-list ()
  "`pi-coding-agent--display-buffers' should query windows in current frame only."
  (let ((root "/tmp/pi-coding-agent-test-display-frame-local/")
        (all-frames-args nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil)))
      (unwind-protect
          (let* ((chat (pi-coding-agent--setup-session root nil))
                 (input (buffer-local-value 'pi-coding-agent--input-buffer chat))
                 (orig-get-buffer-window-list (symbol-function 'get-buffer-window-list)))
            (delete-other-windows)
            (cl-letf (((symbol-function 'get-buffer-window-list)
                       (lambda (buffer minibuf &optional all-frames)
                         (push all-frames all-frames-args)
                         (funcall orig-get-buffer-window-list buffer minibuf all-frames))))
              (pi-coding-agent--display-buffers chat input))
            (should-not (memq t all-frames-args)))
        (pi-coding-agent-test--kill-session-buffers root)
        (delete-other-windows)))))

(ert-deftest pi-coding-agent-test-hide-session-windows-uses-current-frame-window-list ()
  "`pi-coding-agent--hide-session-windows' should query current frame windows only."
  (let ((root "/tmp/pi-coding-agent-test-hide-frame-local/")
        (all-frames-args nil))
    (make-directory root t)
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil)))
      (unwind-protect
          (progn
            (delete-other-windows)
            (switch-to-buffer "*scratch*")
            (setq default-directory root)
            (pi-coding-agent)
            (let ((chat (get-buffer (pi-coding-agent-test--chat-buffer-name root)))
                  (orig-get-buffer-window-list (symbol-function 'get-buffer-window-list)))
              (with-current-buffer chat
                (cl-letf (((symbol-function 'get-buffer-window-list)
                           (lambda (buffer minibuf &optional all-frames)
                             (push all-frames all-frames-args)
                             (funcall orig-get-buffer-window-list buffer minibuf all-frames))))
                  (pi-coding-agent--hide-session-windows)))
              (should-not (memq t all-frames-args))))
        (pi-coding-agent-test--kill-session-buffers root)
        (delete-other-windows)))))

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

(ert-deftest pi-coding-agent-test-startup-header-shows-pi-label ()
  "Startup header includes the product label."
  (let ((header (pi-coding-agent--format-startup-header)))
    (should (string-match-p "^Pi Coding Agent for Emacs$" header))))

(ert-deftest pi-coding-agent-test-request-pi-version-async-waits-before-probe ()
  "Version lookup waits briefly before starting the probe process."
  (let ((scheduled-delay nil)
        (resolved-version nil))
    (cl-letf (((symbol-function 'pi-coding-agent--run-pi-version-once-async)
               (lambda (callback)
                 (funcall callback "0.53.0")))
              ((symbol-function 'run-at-time)
               (lambda (secs _repeat fn &rest args)
                 (setq scheduled-delay secs)
                 (apply fn args)
                 'mock-timer)))
      (pi-coding-agent--request-pi-version-async
       (lambda (version)
         (setq resolved-version version))))
    (should (= scheduled-delay pi-coding-agent--version-probe-delay))
    (should (equal resolved-version "0.53.0"))))

(ert-deftest pi-coding-agent-test-set-process-probes-version-for-current-process ()
  "Setting process starts version probe and stores result for current process."
  (let ((callback nil)
        (messages nil)
        (noninteractive nil)
        (pi-coding-agent-phscroll-offer-install nil)
        (proc (start-process "pi-coding-agent-test-proc" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (pi-coding-agent-chat-mode)
          (cl-letf (((symbol-function 'pi-coding-agent--request-pi-version-async)
                     (lambda (cb)
                       (setq callback cb)
                       nil))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (pi-coding-agent--set-process proc)
            (should callback)
            (funcall callback "0.53.0")
            (should (equal pi-coding-agent--process-version "0.53.0"))
            (should (equal (car messages) "Pi: version 0.53.0"))))
      (when (process-live-p proc)
        (delete-process proc)))))

(ert-deftest pi-coding-agent-test-set-process-version-callback-uses-chat-buffer-context ()
  "Version callback updates chat buffer even when current buffer changed."
  (let ((callback nil)
        (messages nil)
        (noninteractive nil)
        (pi-coding-agent-phscroll-offer-install nil)
        (proc (start-process "pi-coding-agent-test-proc-a" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (pi-coding-agent-chat-mode)
          (let ((chat-buf (current-buffer)))
            (cl-letf (((symbol-function 'pi-coding-agent--request-pi-version-async)
                       (lambda (cb)
                         (setq callback cb)
                         nil))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages))))
              (pi-coding-agent--set-process proc)
              (with-temp-buffer
                (funcall callback "0.53.0"))
              (with-current-buffer chat-buf
                (should (equal pi-coding-agent--process-version "0.53.0")))
              (should (equal (car messages) "Pi: version 0.53.0")))))
      (when (process-live-p proc)
        (delete-process proc)))))

;;; Copy Visible Text

(defmacro pi-coding-agent-test--with-chat-markup (markdown &rest body)
  "Insert MARKDOWN into a chat-mode buffer, fontify, then run BODY.
Buffer is read-only with `inhibit-read-only' used for insertion.
`font-lock-ensure' runs before BODY to apply invisible/display properties."
  (declare (indent 1) (debug (stringp body)))
  `(with-temp-buffer
     (pi-coding-agent-chat-mode)
     (let ((inhibit-read-only t))
       (insert ,markdown))
     (font-lock-ensure)
     ,@body))

(ert-deftest pi-coding-agent-test-visible-text-strips-bold-markers ()
  "visible-text strips invisible bold markers (**)."
  (pi-coding-agent-test--with-chat-markup "Hello **bold** world"
    (should (equal (pi-coding-agent--visible-text (point-min) (point-max))
                   "Hello bold world"))))

(ert-deftest pi-coding-agent-test-visible-text-strips-inline-code-backticks ()
  "visible-text strips invisible backticks around inline code."
  (pi-coding-agent-test--with-chat-markup "Use `foo` here"
    (should (equal (pi-coding-agent--visible-text (point-min) (point-max))
                   "Use foo here"))))

(ert-deftest pi-coding-agent-test-visible-text-strips-code-fences ()
  "visible-text strips invisible code fences and language label."
  (pi-coding-agent-test--with-chat-markup "```python\ndef foo():\n    pass\n```\n"
    (let ((result (pi-coding-agent--visible-text (point-min) (point-max))))
      (should (string-match-p "def foo" result))
      (should-not (string-match-p "```" result))
      (should-not (string-match-p "python" result)))))

(ert-deftest pi-coding-agent-test-visible-text-strips-setext-underline ()
  "visible-text strips display=\"\" setext underline."
  (pi-coding-agent-test--with-chat-markup "Assistant\n=========\n\nHello\n"
    (let ((result (pi-coding-agent--visible-text (point-min) (point-max))))
      (should (string-match-p "Assistant" result))
      (should-not (string-match-p "=====" result))
      (should (string-match-p "Hello" result)))))

(ert-deftest pi-coding-agent-test-visible-text-strips-atx-heading-prefix ()
  "visible-text strips display=\"\" ATX heading prefix characters."
  (pi-coding-agent-test--with-chat-markup "## Code Example\n\nSome text\n"
    (let ((result (pi-coding-agent--visible-text (point-min) (point-max))))
      (should (string-match-p "Code Example" result))
      (should (string-match-p "Some text" result))
      (should-not (string-match-p "^##" result)))))

(ert-deftest pi-coding-agent-test-visible-text-preserves-plain-text ()
  "visible-text preserves text that has no hidden markup."
  (pi-coding-agent-test--with-chat-markup "Just plain text with no markup"
    (should (equal (pi-coding-agent--visible-text (point-min) (point-max))
                   "Just plain text with no markup"))))

(ert-deftest pi-coding-agent-test-copy-raw-markdown-defcustom-default ()
  "pi-coding-agent-copy-raw-markdown defcustom defaults to nil."
  (should (eq pi-coding-agent-copy-raw-markdown nil)))

(ert-deftest pi-coding-agent-test-kill-ring-save-strips-by-default ()
  "kill-ring-save strips hidden markup by default."
  (pi-coding-agent-test--with-chat-markup "Hello **bold** world"
    (kill-ring-save (point-min) (point-max))
    (should (equal (car kill-ring) "Hello bold world"))))

(ert-deftest pi-coding-agent-test-kill-ring-save-keeps-raw-when-enabled ()
  "When copy-raw-markdown is t, kill-ring-save keeps raw markdown."
  (pi-coding-agent-test--with-chat-markup "Hello **bold** world"
    (let ((pi-coding-agent-copy-raw-markdown t))
      (kill-ring-save (point-min) (point-max))
      (should (equal (car kill-ring) "Hello **bold** world")))))

;;; Chat Navigation Behavior

(ert-deftest pi-coding-agent-test-next-message-from-top ()
  "n from point-min reaches first You heading."
  (with-temp-buffer
    (pi-coding-agent-test--insert-chat-turns)
    (goto-char (point-min))
    (pi-coding-agent-next-message)
    (should (looking-at "You · 10:00"))))

(ert-deftest pi-coding-agent-test-next-message-successive ()
  "Successive n reaches each You heading in order."
  (with-temp-buffer
    (pi-coding-agent-test--insert-chat-turns)
    (goto-char (point-min))
    (pi-coding-agent-next-message)
    (should (looking-at "You · 10:00"))
    (pi-coding-agent-next-message)
    (should (looking-at "You · 10:05"))
    (pi-coding-agent-next-message)
    (should (looking-at "You · 10:10"))))

(ert-deftest pi-coding-agent-test-next-message-at-last ()
  "n at last You heading keeps point and shows message."
  (with-temp-buffer
    (pi-coding-agent-test--insert-chat-turns)
    (goto-char (point-min))
    (pi-coding-agent-next-message)
    (pi-coding-agent-next-message)
    (pi-coding-agent-next-message)
    (should (looking-at "You · 10:10"))
    (let ((pos (point))
          (shown-message nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (pi-coding-agent-next-message))
      ;; Point stays on the last heading
      (should (= (point) pos))
      (should (equal shown-message "No more messages")))))

(ert-deftest pi-coding-agent-test-previous-message-from-last ()
  "p from last You heading reaches previous."
  (with-temp-buffer
    (pi-coding-agent-test--insert-chat-turns)
    (goto-char (point-min))
    ;; Navigate to last heading first
    (pi-coding-agent-next-message)
    (pi-coding-agent-next-message)
    (pi-coding-agent-next-message)
    (should (looking-at "You · 10:10"))
    (pi-coding-agent-previous-message)
    (should (looking-at "You · 10:05"))))

(ert-deftest pi-coding-agent-test-previous-message-at-first ()
  "p at first You heading keeps point and shows message."
  (with-temp-buffer
    (pi-coding-agent-test--insert-chat-turns)
    (goto-char (point-min))
    (pi-coding-agent-next-message)
    (should (looking-at "You · 10:00"))
    (let ((pos (point))
          (shown-message nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq shown-message (apply #'format fmt args)))))
        (pi-coding-agent-previous-message))
      ;; Point stays on the first heading
      (should (= (point) pos))
      (should (equal shown-message "No previous message")))))

;;; Turn Detection

(ert-deftest pi-coding-agent-test-turn-index-on-first-heading ()
  "Turn index is 0 when point is on first You heading."
  (with-temp-buffer
    (pi-coding-agent-test--insert-chat-turns)
    (goto-char (point-min))
    (pi-coding-agent-next-message)
    (should (= (pi-coding-agent--user-turn-index-at-point) 0))))

(ert-deftest pi-coding-agent-test-turn-index-in-first-body ()
  "Turn index is 0 when point is in first user message body."
  (with-temp-buffer
    (pi-coding-agent-test--insert-chat-turns)
    (goto-char (point-min))
    (pi-coding-agent-next-message)
    (forward-line 2) ; skip heading + underline into body
    (should (= (pi-coding-agent--user-turn-index-at-point) 0))))

(ert-deftest pi-coding-agent-test-turn-index-on-underline ()
  "Turn index is 0 when point is on === underline of first You."
  (with-temp-buffer
    (pi-coding-agent-test--insert-chat-turns)
    (goto-char (point-min))
    (pi-coding-agent-next-message)
    (forward-line 1) ; on ===
    (should (= (pi-coding-agent--user-turn-index-at-point) 0))))

(ert-deftest pi-coding-agent-test-turn-index-on-second-heading ()
  "Turn index is 1 on second You heading."
  (with-temp-buffer
    (pi-coding-agent-test--insert-chat-turns)
    (goto-char (point-min))
    (pi-coding-agent-next-message)
    (pi-coding-agent-next-message)
    (should (= (pi-coding-agent--user-turn-index-at-point) 1))))

(ert-deftest pi-coding-agent-test-turn-index-on-assistant-heading ()
  "Turn index is index of preceding You when point is on Assistant heading."
  (with-temp-buffer
    (pi-coding-agent-test--insert-chat-turns)
    (goto-char (point-min))
    ;; Navigate to first You, then move into assistant section
    (pi-coding-agent-next-message)
    (forward-line 4) ; past heading + underline + body + blank → "Assistant"
    (should (looking-at "Assistant"))
    (should (= (pi-coding-agent--user-turn-index-at-point) 0))))

(ert-deftest pi-coding-agent-test-turn-index-in-assistant-body ()
  "Turn index is index of preceding You when point is in assistant response."
  (with-temp-buffer
    (pi-coding-agent-test--insert-chat-turns)
    (goto-char (point-min))
    (pi-coding-agent-next-message)
    (forward-line 6) ; heading + underline + body + blank + Assistant + underline → response
    (should (looking-at "First answer"))
    (should (= (pi-coding-agent--user-turn-index-at-point) 0))))

(ert-deftest pi-coding-agent-test-turn-index-before-first-you ()
  "Turn index is nil before first You heading."
  (with-temp-buffer
    (pi-coding-agent-test--insert-chat-turns)
    (goto-char (point-min))
    (should-not (pi-coding-agent--user-turn-index-at-point))))

(ert-deftest pi-coding-agent-test-turn-index-empty-buffer ()
  "Turn index is nil in empty buffer."
  (with-temp-buffer
    (should-not (pi-coding-agent--user-turn-index-at-point))))

(ert-deftest pi-coding-agent-test-turn-index-no-false-match ()
  "Turn index ignores text starting with You without setext underline."
  (with-temp-buffer
    (insert "You mentioned something\nRegular text\n\n"
            "You · 10:00\n===========\nFirst question\n")
    (goto-char (point-min))
    ;; Point is on "You mentioned" which has no === underline
    (should-not (pi-coding-agent--user-turn-index-at-point))
    ;; Move to the real heading
    (goto-char (point-max))
    (should (= (pi-coding-agent--user-turn-index-at-point) 0))))

;;; You Heading Detection

(ert-deftest pi-coding-agent-test-heading-re-matches-plain-you ()
  "Heading regex matches bare `You' at start of line."
  (should (string-match-p pi-coding-agent--you-heading-re "You")))

(ert-deftest pi-coding-agent-test-heading-re-matches-you-with-timestamp ()
  "Heading regex matches `You · 22:10' at start of line."
  (should (string-match-p pi-coding-agent--you-heading-re "You · 22:10")))

(ert-deftest pi-coding-agent-test-heading-re-rejects-you-colon ()
  "Heading regex does not match `You:' (old broken pattern)."
  (should-not (string-match-p pi-coding-agent--you-heading-re "You: hello")))

(ert-deftest pi-coding-agent-test-heading-re-rejects-mid-line ()
  "Heading regex does not match `You' mid-line."
  (should-not (string-match-p pi-coding-agent--you-heading-re "  You · 22:10")))

(ert-deftest pi-coding-agent-test-heading-re-rejects-you-prefix ()
  "Heading regex does not match words starting with You like `Your'."
  (should-not (string-match-p pi-coding-agent--you-heading-re "Your code is fine")))

(ert-deftest pi-coding-agent-test-at-you-heading-p-true ()
  "Predicate returns t when on a valid You setext heading."
  (with-temp-buffer
    (insert "You · 22:10\n===========\n")
    (goto-char (point-min))
    (should (pi-coding-agent--at-you-heading-p))))

(ert-deftest pi-coding-agent-test-at-you-heading-p-no-underline ()
  "Predicate returns nil when You line lacks setext underline."
  (with-temp-buffer
    (insert "You · 22:10\nSome text\n")
    (goto-char (point-min))
    (should-not (pi-coding-agent--at-you-heading-p))))

(ert-deftest pi-coding-agent-test-at-you-heading-p-short-underline ()
  "Predicate returns t with minimum 3-char underline."
  (with-temp-buffer
    (insert "You\n===\n")
    (goto-char (point-min))
    (should (pi-coding-agent--at-you-heading-p))))

(ert-deftest pi-coding-agent-test-at-you-heading-p-wrong-line ()
  "Predicate returns nil when not on the heading line."
  (with-temp-buffer
    (insert "You · 22:10\n===========\nBody text\n")
    (goto-char (point-max))
    (forward-line -1)  ; on "Body text"
    (should-not (pi-coding-agent--at-you-heading-p))))

;;; Executable Customization

(ert-deftest pi-coding-agent-test-check-pi-uses-executable ()
  "check-pi uses car of `pi-coding-agent-executable' for lookup."
  (let ((pi-coding-agent-executable '("npx" "pi")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (cmd) (when (equal cmd "npx") "/usr/bin/npx"))))
      (should (pi-coding-agent--check-pi)))))

(ert-deftest pi-coding-agent-test-check-pi-returns-nil-when-missing ()
  "check-pi returns nil when executable is not found."
  (let ((pi-coding-agent-executable '("nonexistent-binary")))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (should-not (pi-coding-agent--check-pi)))))

(ert-deftest pi-coding-agent-test-executable-default-value ()
  "Default value of pi-coding-agent-executable is (\"pi\")."
  (should (equal (default-value 'pi-coding-agent-executable) '("pi"))))

(ert-deftest pi-coding-agent-test-check-dependencies-names-executable ()
  "Warning message includes the actual executable name."
  (let ((pi-coding-agent-executable '("my-custom-pi"))
        (warning-text nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil))
              ((symbol-function 'display-warning)
               (lambda (_type msg &rest _) (setq warning-text msg))))
      (pi-coding-agent--check-dependencies)
      (should (string-match-p "my-custom-pi" warning-text)))))

(provide 'pi-coding-agent-ui-test)
;;; pi-coding-agent-ui-test.el ends here
