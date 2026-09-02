;;; piem-integration-test.el --- Shared integration suite entry point -*- lexical-binding: t; -*-

;;; Commentary:

;; Loads the shared integration contract modules for both fake and real
;; backends.  See the individual module files for behavior-specific tests.

;;; Code:

(require 'piem-integration-test-common)
(require 'piem-integration-rpc-smoke-test)
(require 'piem-integration-prompt-contract-test)
(require 'piem-integration-session-contract-test)
(require 'piem-integration-steering-contract-test)
(require 'piem-integration-tool-contract-test)

(provide 'piem-integration-test)
;;; piem-integration-test.el ends here
