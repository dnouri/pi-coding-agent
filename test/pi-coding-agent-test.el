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

(provide 'pi-coding-agent-test)
;;; pi-coding-agent-test.el ends here
