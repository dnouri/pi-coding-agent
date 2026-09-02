;;; piem-gui-test-utils-test.el --- Unit tests for GUI test utilities -*- lexical-binding: t; -*-

;;; Commentary:

;; Fast tests for the GUI test harness itself.  These cover macro and helper
;; contracts that do not need a visible frame.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'piem-gui-test-utils)

(ert-deftest piem-gui-test-utils-test-with-fresh-session-requires-explicit-backend ()
  "Fresh GUI sessions must declare a backend at the entry point."
  (should-error
   (macroexpand '(piem-gui-test-with-fresh-session
                   (should t)))))

(ert-deftest piem-gui-test-utils-test-with-session-requires-explicit-backend ()
  "Shared GUI sessions must declare a backend at the entry point."
  (should-error
   (macroexpand '(piem-gui-test-with-session
                   (should t)))))

(ert-deftest piem-gui-test-utils-test-ensure-session-rejects-implicit-start ()
  "Implicit session startup without a backend should fail loudly."
  (let ((piem-gui-test--session nil))
    (should-error (piem-gui-test-ensure-session))))

(ert-deftest piem-gui-test-utils-test-ensure-session-preserves-active-backend ()
  "Helper calls may reuse the already-active session backend."
  (let ((options '(:backend fake :fake-scenario "tool-read" :fake-extra-args nil)))
    (cl-letf (((symbol-function 'piem-gui-test-session-active-p)
               (lambda () t))
              ((symbol-function 'piem-gui-test--current-session-options)
               (lambda () options))
              ((symbol-function 'piem-gui-test--session-matches-p)
               (lambda (actual-options)
                 (should (equal actual-options options))
                 t))
              ((symbol-function 'piem-gui-test-ensure-layout)
               (lambda () 'layout-ok)))
      (should (eq (piem-gui-test-ensure-session) 'layout-ok)))))

(provide 'piem-gui-test-utils-test)
;;; piem-gui-test-utils-test.el ends here
