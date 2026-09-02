;;; piem-evil-test.el --- Tests for piem-evil -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>

;;; Commentary:

;; Tests for the optional Evil integration: initial states, keymap
;; registrations, and the copy-raw-markdown default.  All tests are
;; skipped when Evil is not installed.

;;; Code:

(require 'ert)
(require 'piem)
(when (require 'evil nil t)
  (require 'piem-evil))

(defmacro piem-evil-test--with-evil (&rest body)
  "Skip the test when Evil is unavailable, else run BODY."
  (declare (indent 0) (debug t))
  `(if (not (featurep 'evil))
       (ert-skip "Evil not installed")
     ,@body))

(ert-deftest piem-evil-test-initial-states ()
  "Chat buffers start in motion state, input buffers in insert state."
  (piem-evil-test--with-evil
   (piem-evil-setup)
   (should (eq (evil-initial-state 'piem-chat-mode)
               piem-evil-chat-state))
   (should (eq (evil-initial-state 'piem-input-mode)
               piem-evil-input-state))))

(ert-deftest piem-evil-test-chat-motion-bindings ()
  "Motion state bindings in the chat buffer."
  (piem-evil-test--with-evil
   (piem-evil-setup)
   (let ((map (evil-get-auxiliary-keymap piem-chat-mode-map
                                         'motion)))
     (should map)
     (should (eq (lookup-key map "n") #'piem-next-message))
     (should (eq (lookup-key map "p") #'piem-previous-message))
     (should (eq (lookup-key map "f") #'piem-fork-at-point))
     (should (eq (lookup-key map "?") #'piem-menu))
     (should (eq (lookup-key map "q") #'piem-quit))
     (should (eq (lookup-key map "i") #'piem-evil-insert-input))
     (should (eq (lookup-key map "a") #'piem-evil-append-input))
     (should (eq (lookup-key map (kbd "RET")) #'piem-visit-file))
     (should (eq (lookup-key map (kbd "TAB"))
                 #'piem-toggle-tool-section))
     (should (eq (lookup-key map [tab])
                 #'piem-toggle-tool-section)))))

(ert-deftest piem-evil-test-input-normal-bindings ()
  "Normal state bindings in the input buffer."
  (piem-evil-test--with-evil
   (piem-evil-setup)
   (let ((map (evil-get-auxiliary-keymap piem-input-mode-map
                                         'normal)))
     (should map)
     (should (eq (lookup-key map (kbd "RET")) #'piem-send))
     (should (eq (lookup-key map "q") #'piem-evil-close-input))
     (should (eq (lookup-key map "?") #'piem-menu)))))

(ert-deftest piem-evil-test-copy-raw-markdown-default ()
  "Setup copies raw markdown buffer-locally in chat buffers by default."
  (piem-evil-test--with-evil
   (let ((piem-chat-mode-hook nil))
     (piem-evil-setup)
     (should (memq #'piem-evil--copy-raw-markdown-in-chat
                   piem-chat-mode-hook)))
   (with-temp-buffer
     (piem-evil--copy-raw-markdown-in-chat)
     (should (and (local-variable-p 'piem-copy-raw-markdown)
                  piem-copy-raw-markdown)))))

(ert-deftest piem-evil-test-enter-input-state-respects-option ()
  "Focusing the input window enters `piem-evil-input-state'."
  (piem-evil-test--with-evil
   (dolist (state '(insert normal emacs))
     (let ((piem-evil-input-state state))
       (with-temp-buffer
         (evil-local-mode 1)
         (piem-evil--enter-input-state)
         (should (eq evil-state state)))))))

(ert-deftest piem-evil-test-snipe-disabled-in-chat ()
  "Setup hooks snipe disablement, which turns snipe off in chat buffers."
  (piem-evil-test--with-evil
   (piem-evil-setup)
   (should (memq #'piem-evil--maybe-disable-snipe
                 (default-value 'evil-snipe-local-mode-hook)))
   (should (memq #'piem-evil--maybe-disable-snipe
                 (default-value 'evil-snipe-override-local-mode-hook)))))

(ert-deftest piem-evil-test-snipe-hook-disables-in-chat ()
  "The snipe hook turns snipe off in chat buffers only."
  (piem-evil-test--with-evil
   (skip-unless (require 'evil-snipe nil t))
   (with-temp-buffer
     (evil-snipe-local-mode 1)
     (evil-snipe-override-local-mode 1)
     (setq major-mode 'piem-chat-mode)
     (piem-evil--maybe-disable-snipe)
     (should-not evil-snipe-local-mode)
     (should-not evil-snipe-override-local-mode))
   (with-temp-buffer
     (evil-snipe-local-mode 1)
     (evil-snipe-override-local-mode 1)
     (setq major-mode 'text-mode)
     (piem-evil--maybe-disable-snipe)
     (should evil-snipe-local-mode)
     (should evil-snipe-override-local-mode))))

(ert-deftest piem-evil-test-copy-raw-markdown-opt-out ()
  "Setup does not add the chat mode hook when opted out."
  (piem-evil-test--with-evil
   (let ((piem-evil-copy-raw-markdown nil)
         (piem-chat-mode-hook nil))
     (piem-evil-setup)
     (should-not (memq #'piem-evil--copy-raw-markdown-in-chat
                       piem-chat-mode-hook)))))

(provide 'piem-evil-test)
;;; piem-evil-test.el ends here
