;;; pi-coding-agent-integration-test-common-test.el --- Unit tests for integration helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Fast tests for the shared integration helper macros and backend selection.

;;; Code:

(require 'ert)
(require 'pi-coding-agent-integration-test-common)

(ert-deftest pi-coding-agent-integration-test-common-test-deftest-defines-both-backends-when-filtered ()
  "Shared integration macros should define both backend variants.
Runtime environment filters may skip a backend, but they should not change the
set of test definitions produced at macro-expansion time."
  (let ((process-environment (copy-sequence process-environment))
        test-names)
    (setenv "PI_INTEGRATION_BACKENDS" "fake")
    (setq test-names
          (mapcar #'cadr
                  (cdr (macroexpand
                        '(pi-coding-agent-integration-deftest
                             (sample-contract)
                           "Doc"
                           (should t))))))
    (should (equal test-names
                   '(pi-coding-agent-integration-sample-contract/fake
                     pi-coding-agent-integration-sample-contract/real)))))

(provide 'pi-coding-agent-integration-test-common-test)
;;; pi-coding-agent-integration-test-common-test.el ends here
