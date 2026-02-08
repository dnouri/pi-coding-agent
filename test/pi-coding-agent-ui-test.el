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

(provide 'pi-coding-agent-ui-test)
;;; pi-coding-agent-ui-test.el ends here
