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

(ert-deftest pi-coding-agent-integration-test-common-test-uses-tuned-lifecycle-prompt ()
  "Lifecycle contract should keep the shortest proven prompt fixture."
  (should (equal pi-coding-agent-integration--prompt-lifecycle-message
                 "/no_think Say OK")))

(ert-deftest pi-coding-agent-integration-test-common-test-uses-tuned-session-prompt ()
  "Session contract should keep the terse session-materializing prompt."
  (should (equal pi-coding-agent-integration--prompt-session-materialize-message
                 "/no_think Say: test")))

(ert-deftest pi-coding-agent-integration-test-common-test-detects-existing-session-file ()
  "Session-file predicate should require a real file on disk."
  (let ((session-file (make-temp-file "pi-coding-agent-session-file-")))
    (unwind-protect
        (should (pi-coding-agent-integration--response-has-existing-session-file-p
                 `(:data (:sessionFile ,session-file))))
      (delete-file session-file))))

(ert-deftest pi-coding-agent-integration-test-common-test-rejects-missing-session-file ()
  "Session-file predicate should reject absent files."
  (should-not (pi-coding-agent-integration--response-has-existing-session-file-p
               '(:data (:sessionFile "/tmp/definitely-missing-session-file.jsonl")))))

(provide 'pi-coding-agent-integration-test-common-test)
;;; pi-coding-agent-integration-test-common-test.el ends here
