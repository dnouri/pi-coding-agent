;;; pilish-integration-test-common-test.el --- Unit tests for integration helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Fast tests for the shared integration helper macros and backend selection.

;;; Code:

(require 'ert)
(require 'pilish-integration-test-common)

(ert-deftest pilish-integration-test-common-test-deftest-defines-both-backends-when-filtered ()
  "Shared integration macros should define both backend variants.
Runtime environment filters may skip a backend, but they should not change the
set of test definitions produced at macro-expansion time."
  (let ((process-environment (copy-sequence process-environment))
        test-names)
    (setenv "PI_INTEGRATION_BACKENDS" "fake")
    (setq test-names
          (mapcar #'cadr
                  (cdr (macroexpand
                        '(pilish-integration-deftest
                             (sample-contract)
                           "Doc"
                           (should t))))))
    (should (equal test-names
                   '(pilish-integration-sample-contract/fake
                     pilish-integration-sample-contract/real)))))

(ert-deftest pilish-integration-test-common-test-uses-tuned-lifecycle-prompt ()
  "Lifecycle contract should keep the shortest proven prompt fixture."
  (should (equal pilish-integration--prompt-lifecycle-message
                 "/no_think Say OK")))

(ert-deftest pilish-integration-test-common-test-uses-tuned-session-prompt ()
  "Session contract should keep the terse session-materializing prompt."
  (should (equal pilish-integration--prompt-session-materialize-message
                 "/no_think Say: test")))

(ert-deftest pilish-integration-test-common-test-detects-existing-session-file ()
  "Session-file predicate should require a real file on disk."
  (let ((session-file (make-temp-file "pilish-session-file-")))
    (unwind-protect
        (should (pilish-integration--response-has-existing-session-file-p
                 `(:data (:sessionFile ,session-file))))
      (delete-file session-file))))

(ert-deftest pilish-integration-test-common-test-rejects-missing-session-file ()
  "Session-file predicate should reject absent files."
  (should-not (pilish-integration--response-has-existing-session-file-p
               '(:data (:sessionFile "/tmp/definitely-missing-session-file.jsonl")))))

(provide 'pilish-integration-test-common-test)
;;; pilish-integration-test-common-test.el ends here
