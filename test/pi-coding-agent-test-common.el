;;; pi-test-common.el --- Shared test utilities and configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Common definitions shared across pi.el test files.
;; Centralizes timeout values for easy adjustment (e.g., slow CI).

;;; Code:

;;; Timeout Configuration

(defvar pi-test-short-wait 0.5
  "Short wait in seconds for async operations to complete.")

(defvar pi-test-poll-interval 0.1
  "Polling interval in seconds for waiting loops.")

(defvar pi-test-rpc-timeout 10
  "Timeout in seconds for RPC calls in tests.")

(defvar pi-test-integration-timeout 180
  "Timeout in seconds for integration tests.")

(defvar pi-test-gui-timeout 90
  "Timeout in seconds for GUI tests (includes real LLM responses).")

(provide 'pi-test-common)
;;; pi-test-common.el ends here
