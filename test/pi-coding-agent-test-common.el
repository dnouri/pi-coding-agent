;;; pi-coding-agent-test-common.el --- Shared test utilities and configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Common definitions shared across pi-coding-agent test files.
;; Centralizes timeout values, the mock-session macro, and toolcall
;; streaming helpers for easy adjustment (e.g., slow CI).

;;; Code:

(require 'cl-lib) ; for cl-letf in mock-session macro

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

;;;; Toolcall Streaming Helpers

(defun pi-coding-agent-test--count-matches (regexp string)
  "Count non-overlapping occurrences of REGEXP in STRING."
  (let ((count 0) (start 0))
    (while (string-match regexp string start)
      (setq count (1+ count)
            start (match-end 0)))
    count))

(defmacro pi-coding-agent-test--with-toolcall (tool-name args &rest body)
  "Set up a chat buffer with a streaming tool call, then run BODY.
Creates a temp buffer in chat mode, fires agent_start, message_start,
and toolcall_start for TOOL-NAME with ARGS (a plist).  The tool call
ID is \"call_1\" and contentIndex is 0."
  (declare (indent 2) (debug (sexp sexp body)))
  `(with-temp-buffer
     (pi-coding-agent-chat-mode)
     (pi-coding-agent--handle-display-event '(:type "agent_start"))
     (pi-coding-agent--handle-display-event '(:type "message_start"))
     (pi-coding-agent--handle-display-event
      `(:type "message_update"
        :assistantMessageEvent (:type "toolcall_start" :contentIndex 0)
        :message (:role "assistant"
                  :content [(:type "toolCall" :id "call_1"
                             :name ,,tool-name :arguments ,,args)])))
     ,@body))

(defun pi-coding-agent-test--send-delta (tool-name args)
  "Send a toolcall_delta event for TOOL-NAME with ARGS.
Uses tool call ID \"call_1\" and contentIndex 0."
  (pi-coding-agent--handle-display-event
   `(:type "message_update"
     :assistantMessageEvent (:type "toolcall_delta" :contentIndex 0 :delta "x")
     :message (:role "assistant"
               :content [(:type "toolCall" :id "call_1"
                          :name ,tool-name :arguments ,args)]))))

;;;; Mock Session

(defmacro pi-coding-agent-test-with-mock-session (dir &rest body)
  "Execute BODY with a mocked pi session in DIR, cleaning up after.
DIR should be a unique directory path like \"/tmp/pi-coding-agent-test-foo/\".
Mocks `project-current', `pi-coding-agent--start-process', and
`pi-coding-agent--display-buffers'.
Automatically cleans up chat and input buffers."
  (declare (indent 1) (debug t))
  `(let ((default-directory ,dir))
     (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
               ((symbol-function 'pi-coding-agent--start-process) (lambda (_) nil))
               ((symbol-function 'pi-coding-agent--display-buffers) #'ignore))
       (unwind-protect
           (progn (pi-coding-agent) ,@body)
         (ignore-errors (kill-buffer (pi-coding-agent--chat-buffer-name ,dir nil)))
         (ignore-errors (kill-buffer (pi-coding-agent--input-buffer-name ,dir nil)))))))

;;;; Two-Session Fixture

(defmacro pi-coding-agent-test--with-two-sessions (buf-a buf-b &rest body)
  "Execute BODY with two independent chat-mode buffers BUF-A and BUF-B.
Both buffers are created with `pi-coding-agent-chat-mode' activated.
Buffers are killed after BODY completes, even on error."
  (declare (indent 2) (debug (symbolp symbolp body)))
  `(let ((,buf-a (generate-new-buffer "*test-chat-A*"))
         (,buf-b (generate-new-buffer "*test-chat-B*")))
     (with-current-buffer ,buf-a (pi-coding-agent-chat-mode))
     (with-current-buffer ,buf-b (pi-coding-agent-chat-mode))
     (unwind-protect
         (progn ,@body)
       (ignore-errors (kill-buffer ,buf-a))
       (ignore-errors (kill-buffer ,buf-b)))))

(provide 'pi-coding-agent-test-common)
;;; pi-coding-agent-test-common.el ends here
