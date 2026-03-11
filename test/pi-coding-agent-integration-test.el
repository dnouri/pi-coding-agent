;;; pi-coding-agent-integration-test.el --- Shared integration suite entry point -*- lexical-binding: t; -*-

;;; Commentary:

;; Loads the shared integration contract modules for both fake and real
;; backends.  See the individual module files for behavior-specific tests.

;;; Code:

(require 'pi-coding-agent-integration-test-common)
(require 'pi-coding-agent-integration-rpc-smoke-test)
(require 'pi-coding-agent-integration-prompt-contract-test)
(require 'pi-coding-agent-integration-session-contract-test)
(require 'pi-coding-agent-integration-steering-contract-test)
(require 'pi-coding-agent-integration-tool-contract-test)

(provide 'pi-coding-agent-integration-test)
;;; pi-coding-agent-integration-test.el ends here
