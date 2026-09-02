;;; piem-integration-test-common-test.el --- Unit tests for integration helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Fast tests for the shared integration helper macros and backend selection.

;;; Code:

(require 'ert)
(require 'piem-integration-test-common)

(ert-deftest piem-integration-test-common-test-deftest-defines-both-backends-when-filtered ()
  "Shared integration macros should define both backend variants.
Runtime environment filters may skip a backend, but they should not change the
set of test definitions produced at macro-expansion time."
  (let ((process-environment (copy-sequence process-environment))
        test-names)
    (setenv "PI_INTEGRATION_BACKENDS" "fake")
    (setq test-names
          (mapcar #'cadr
                  (cdr (macroexpand
                        '(piem-integration-deftest
                             (sample-contract)
                           "Doc"
                           (should t))))))
    (should (equal test-names
                   '(piem-integration-sample-contract/fake
                     piem-integration-sample-contract/real)))))

(ert-deftest piem-integration-test-common-test-uses-tuned-lifecycle-prompt ()
  "Lifecycle contract should keep the shortest proven prompt fixture."
  (should (equal piem-integration--prompt-lifecycle-message
                 "/no_think Say OK")))

(ert-deftest piem-integration-test-common-test-uses-tuned-session-prompt ()
  "Session contract should keep the terse session-materializing prompt."
  (should (equal piem-integration--prompt-session-materialize-message
                 "/no_think Say: test")))

(ert-deftest piem-integration-test-common-test-detects-existing-session-file ()
  "Session-file predicate should require a real file on disk."
  (let ((session-file (make-temp-file "piem-session-file-")))
    (unwind-protect
        (should (piem-integration--response-has-existing-session-file-p
                 `(:data (:sessionFile ,session-file))))
      (delete-file session-file))))

(ert-deftest piem-integration-test-common-test-rejects-missing-session-file ()
  "Session-file predicate should reject absent files."
  (should-not (piem-integration--response-has-existing-session-file-p
               '(:data (:sessionFile "/tmp/definitely-missing-session-file.jsonl")))))

(provide 'piem-integration-test-common-test)
;;; piem-integration-test-common-test.el ends here
