;;; pi-coding-agent-gui-test-utils-test.el --- Unit tests for GUI test utilities -*- lexical-binding: t; -*-

;;; Commentary:

;; Fast tests for the GUI test harness itself.  These cover macro and helper
;; contracts that do not need a visible frame.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'pi-coding-agent-gui-test-utils)

(ert-deftest pi-coding-agent-gui-test-utils-test-with-fresh-session-requires-explicit-backend ()
  "Fresh GUI sessions must declare a backend at the entry point."
  (should-error
   (macroexpand '(pi-coding-agent-gui-test-with-fresh-session
                   (should t)))))

(ert-deftest pi-coding-agent-gui-test-utils-test-with-session-requires-explicit-backend ()
  "Shared GUI sessions must declare a backend at the entry point."
  (should-error
   (macroexpand '(pi-coding-agent-gui-test-with-session
                   (should t)))))

(ert-deftest pi-coding-agent-gui-test-utils-test-ensure-session-rejects-implicit-start ()
  "Implicit session startup without a backend should fail loudly."
  (let ((pi-coding-agent-gui-test--session nil))
    (should-error (pi-coding-agent-gui-test-ensure-session))))

(ert-deftest pi-coding-agent-gui-test-utils-test-ensure-session-preserves-active-backend ()
  "Helper calls may reuse the already-active session backend."
  (let ((options '(:backend fake :fake-scenario "tool-read" :fake-extra-args nil)))
    (cl-letf (((symbol-function 'pi-coding-agent-gui-test-session-active-p)
               (lambda () t))
              ((symbol-function 'pi-coding-agent-gui-test--current-session-options)
               (lambda () options))
              ((symbol-function 'pi-coding-agent-gui-test--session-matches-p)
               (lambda (actual-options)
                 (should (equal actual-options options))
                 t))
              ((symbol-function 'pi-coding-agent-gui-test-ensure-layout)
               (lambda () 'layout-ok)))
      (should (eq (pi-coding-agent-gui-test-ensure-session) 'layout-ok)))))

(provide 'pi-coding-agent-gui-test-utils-test)
;;; pi-coding-agent-gui-test-utils-test.el ends here
