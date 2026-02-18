;;; pi-coding-agent-test.el --- Tests for pi-coding-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; Entry-point and cross-module integration tests for pi-coding-agent.

;;; Code:

(require 'ert)
(require 'pi-coding-agent)
(require 'pi-coding-agent-test-common)

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

;;; DWIM & Toggle

(ert-deftest pi-coding-agent-test-dwim-reuses-existing-session ()
  "Calling `pi-coding-agent' from a non-pi buffer reuses the existing session."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-dwim/"
    ;; Session exists; now call from a non-pi buffer in the same project
    (with-temp-buffer
      (setq default-directory "/tmp/pi-coding-agent-test-dwim/")
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'pi-coding-agent--display-buffers) #'ignore))
        (pi-coding-agent))
      ;; Should not have created a second chat buffer
      (should (= 1 (length (cl-remove-if-not
                             (lambda (b)
                               (string-prefix-p "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-dwim/"
                                                (buffer-name b)))
                             (buffer-list))))))))

(ert-deftest pi-coding-agent-test-dwim-reuses-named-session ()
  "Calling `pi-coding-agent' from a non-pi buffer finds a named session."
  (let ((default-directory "/tmp/pi-coding-agent-test-dwim-named/")
        (displayed nil))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
              ((symbol-function 'pi-coding-agent--display-buffers)
               (lambda (chat _input) (setq displayed chat))))
      (unwind-protect
          (progn
            ;; Create a named session
            (pi-coding-agent "my-feature")
            ;; Now call M-x pi from a plain buffer — should reuse it
            (with-temp-buffer
              (setq default-directory "/tmp/pi-coding-agent-test-dwim-named/")
              (setq displayed nil)
              (pi-coding-agent)
              (should displayed)
              (should (string-match-p "<my-feature>"
                                      (buffer-name displayed)))))
        (ignore-errors
          (kill-buffer (pi-coding-agent--buffer-name
                        :chat "/tmp/pi-coding-agent-test-dwim-named/" "my-feature")))
        (ignore-errors
          (kill-buffer (pi-coding-agent--buffer-name
                        :input "/tmp/pi-coding-agent-test-dwim-named/" "my-feature")))))))

(ert-deftest pi-coding-agent-test-new-session-with-prefix-arg ()
  "\\[universal-argument] \\[pi-coding-agent] creates a named session."
  (let ((default-directory "/tmp/pi-coding-agent-test-named/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
              ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
              ((symbol-function 'pi-coding-agent--display-buffers) #'ignore)
              ((symbol-function 'read-string) (lambda (&rest _) "my-session")))
      (let ((current-prefix-arg '(4)))
        (unwind-protect
            (progn
              (call-interactively #'pi-coding-agent)
              (should (get-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-named/<my-session>*")))
          (ignore-errors
            (kill-buffer "*pi-coding-agent-chat:/tmp/pi-coding-agent-test-named/<my-session>*"))
          (ignore-errors
            (kill-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-named/<my-session>*")))))))

(ert-deftest pi-coding-agent-test-project-buffers-finds-session ()
  "`pi-coding-agent-project-buffers' returns chat buffer for the current project."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-projbuf/"
    (let ((default-directory "/tmp/pi-coding-agent-test-projbuf/"))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (should (= 1 (length (pi-coding-agent-project-buffers))))
        (should (string-prefix-p "*pi-coding-agent-chat:"
                                 (buffer-name (car (pi-coding-agent-project-buffers)))))))))

(ert-deftest pi-coding-agent-test-project-buffers-excludes-other-projects ()
  "`pi-coding-agent-project-buffers' returns nil for a different project."
  (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-projbuf-a/"
    (let ((default-directory "/tmp/pi-coding-agent-test-projbuf-b/"))
      (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
        (should (null (pi-coding-agent-project-buffers)))))))

(ert-deftest pi-coding-agent-test-toggle-no-session-errors ()
  "`pi-coding-agent-toggle' signals `user-error' when no session exists."
  (let ((default-directory "/tmp/pi-coding-agent-test-no-session/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (should-error (pi-coding-agent-toggle) :type 'user-error))))

(provide 'pi-coding-agent-test)
;;; pi-coding-agent-test.el ends here
