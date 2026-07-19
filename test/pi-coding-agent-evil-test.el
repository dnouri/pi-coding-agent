;;; pi-coding-agent-evil-test.el --- Tests for pi-coding-agent-evil -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>

;;; Commentary:

;; Tests for the optional Evil integration: initial states, keymap
;; registrations, and the copy-raw-markdown default.  All tests are
;; skipped when Evil is not installed.

;;; Code:

(require 'ert)
(require 'pi-coding-agent)
(when (require 'evil nil t)
  (require 'pi-coding-agent-evil))

(defmacro pi-coding-agent-evil-test--with-evil (&rest body)
  "Skip the test when Evil is unavailable, else run BODY."
  (declare (indent 0) (debug t))
  `(if (not (featurep 'evil))
       (ert-skip "Evil not installed")
     ,@body))

(ert-deftest pi-coding-agent-evil-test-initial-states ()
  "Chat buffers start in motion state, input buffers in insert state."
  (pi-coding-agent-evil-test--with-evil
   (pi-coding-agent-evil-setup)
   (should (eq (evil-initial-state 'pi-coding-agent-chat-mode)
               pi-coding-agent-evil-chat-state))
   (should (eq (evil-initial-state 'pi-coding-agent-input-mode)
               pi-coding-agent-evil-input-state))))

(ert-deftest pi-coding-agent-evil-test-chat-motion-bindings ()
  "Motion state bindings in the chat buffer."
  (pi-coding-agent-evil-test--with-evil
   (pi-coding-agent-evil-setup)
   (let ((map (evil-get-auxiliary-keymap pi-coding-agent-chat-mode-map
                                         'motion)))
     (should map)
     (should (eq (lookup-key map "n") #'pi-coding-agent-next-message))
     (should (eq (lookup-key map "p") #'pi-coding-agent-previous-message))
     (should (eq (lookup-key map "f") #'pi-coding-agent-fork-at-point))
     (should (eq (lookup-key map "?") #'pi-coding-agent-menu))
     (should (eq (lookup-key map "q") #'pi-coding-agent-quit))
     (should (eq (lookup-key map "i") #'pi-coding-agent-evil-insert-input))
     (should (eq (lookup-key map "a") #'pi-coding-agent-evil-append-input))
     (should (eq (lookup-key map (kbd "RET")) #'pi-coding-agent-visit-file))
     (should (eq (lookup-key map (kbd "TAB"))
                 #'pi-coding-agent-toggle-tool-section))
     (should (eq (lookup-key map [tab])
                 #'pi-coding-agent-toggle-tool-section)))))

(ert-deftest pi-coding-agent-evil-test-input-normal-bindings ()
  "Normal state bindings in the input buffer."
  (pi-coding-agent-evil-test--with-evil
   (pi-coding-agent-evil-setup)
   (let ((map (evil-get-auxiliary-keymap pi-coding-agent-input-mode-map
                                         'normal)))
     (should map)
     (should (eq (lookup-key map (kbd "RET")) #'pi-coding-agent-send))
     (should (eq (lookup-key map "q") #'pi-coding-agent-evil-close-input))
     (should (eq (lookup-key map "?") #'pi-coding-agent-menu)))))

(ert-deftest pi-coding-agent-evil-test-copy-raw-markdown-default ()
  "Setup enables copying raw markdown by default."
  (pi-coding-agent-evil-test--with-evil
   (let ((pi-coding-agent-copy-raw-markdown nil))
     (pi-coding-agent-evil-setup)
     (should pi-coding-agent-copy-raw-markdown))))

(ert-deftest pi-coding-agent-evil-test-snipe-disabled-in-chat ()
  "Setup registers pi chat mode with `evil-snipe-disabled-modes'."
  (pi-coding-agent-evil-test--with-evil
   (skip-unless (require 'evil-snipe nil t))
   (pi-coding-agent-evil-setup)
   (should (memq 'pi-coding-agent-chat-mode evil-snipe-disabled-modes))))

(ert-deftest pi-coding-agent-evil-test-copy-raw-markdown-opt-out ()
  "Setup leaves `pi-coding-agent-copy-raw-markdown' alone when opted out."
  (pi-coding-agent-evil-test--with-evil
   (let ((pi-coding-agent-evil-copy-raw-markdown nil)
         (pi-coding-agent-copy-raw-markdown nil))
     (pi-coding-agent-evil-setup)
     (should-not pi-coding-agent-copy-raw-markdown))))

(provide 'pi-coding-agent-evil-test)
;;; pi-coding-agent-evil-test.el ends here
