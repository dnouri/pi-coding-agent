;;; pi-coding-agent-test-common.el --- Shared test utilities and configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Common definitions shared across pi-coding-agent test files.
;; Centralizes timeout values for easy adjustment (e.g., slow CI).

;;; Code:

;;; Timeout Configuration

(defvar pi-coding-agent-test-short-wait 0.5
  "Short wait in seconds for async operations to complete.")

(defvar pi-coding-agent-test-poll-interval 0.1
  "Polling interval in seconds for waiting loops.")

(defvar pi-coding-agent-test-rpc-timeout 10
  "Timeout in seconds for RPC calls in tests.")

(defvar pi-coding-agent-test-integration-timeout 180
  "Timeout in seconds for integration tests.")

(defvar pi-coding-agent-test-gui-timeout 90
  "Timeout in seconds for GUI tests (includes real LLM responses).")

;;;; Formatting Helpers

(defun pi-coding-agent-test-format-elapsed (seconds)
  "Format SECONDS as a human-readable duration with millisecond precision."
  (format "%.3fs" (float seconds)))

;;;; Waiting Helpers

(defun pi-coding-agent-test-wait-until (predicate &optional timeout poll-interval process)
  "Wait until PREDICATE returns non-nil or TIMEOUT seconds elapse.
POLL-INTERVAL controls how often to check (default
`pi-coding-agent-test-poll-interval'). If PROCESS is non-nil, it is
passed to `accept-process-output' to allow process I/O.

Returns the predicate value, or nil on timeout."
  (let* ((timeout (or timeout pi-coding-agent-test-short-wait))
         (poll-interval (or poll-interval pi-coding-agent-test-poll-interval))
         (start (float-time))
         (result (funcall predicate)))
    (while (and (not result)
                (< (- (float-time) start) timeout))
      (accept-process-output process poll-interval)
      (setq result (funcall predicate)))
    result))

(defun pi-coding-agent-test-wait-for-process-exit (process &optional timeout)
  "Wait until PROCESS is no longer live, up to TIMEOUT seconds.
Returns non-nil if the process exits before the timeout."
  (pi-coding-agent-test-wait-until
   (lambda () (not (process-live-p process)))
   (or timeout pi-coding-agent-test-short-wait)
   pi-coding-agent-test-poll-interval
   process))

(provide 'pi-coding-agent-test-common)
;;; pi-coding-agent-test-common.el ends here
