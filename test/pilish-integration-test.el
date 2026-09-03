;;; pilish-integration-test.el --- Shared integration suite entry point -*- lexical-binding: t; -*-

;;; Commentary:

;; Loads the shared integration contract modules for both fake and real
;; backends.  See the individual module files for behavior-specific tests.

;;; Code:

(require 'pilish-integration-test-common)
(require 'pilish-integration-rpc-smoke-test)
(require 'pilish-integration-prompt-contract-test)
(require 'pilish-integration-session-contract-test)
(require 'pilish-integration-steering-contract-test)
(require 'pilish-integration-tool-contract-test)

(provide 'pilish-integration-test)
;;; pilish-integration-test.el ends here
