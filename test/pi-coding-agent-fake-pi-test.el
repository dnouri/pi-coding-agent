;;; pi-coding-agent-fake-pi-test.el --- Black-box tests for fake pi harness -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests exercise the Python fake-pi harness as a real subprocess over
;; stdin/stdout.  They intentionally avoid poking Python internals so the fake
;; stays accountable to the JSONL RPC contract.

;;; Code:

(require 'ert)
(require 'pi-coding-agent)
(require 'pi-coding-agent-test-common)

(defconst pi-coding-agent-fake-pi-test--timeout 5
  "Timeout in seconds for fake-pi black-box tests.")

(defun pi-coding-agent-fake-pi-test--process-filter (proc output)
  "Capture JSONL OUTPUT from fake-pi PROC."
  (let* ((partial (or (process-get proc 'fake-pi-partial) ""))
         (result (pi-coding-agent--accumulate-lines partial output))
         (lines (car result))
         (objects (process-get proc 'fake-pi-objects))
         (invalid (process-get proc 'fake-pi-invalid-lines))
         (new-objects nil)
         (new-invalid nil))
    (process-put proc 'fake-pi-raw-output
                 (concat (or (process-get proc 'fake-pi-raw-output) "") output))
    (process-put proc 'fake-pi-partial (cdr result))
    (dolist (line lines)
      (if-let ((json (pi-coding-agent--parse-json-line line)))
          (push json new-objects)
        (push line new-invalid)))
    (process-put proc 'fake-pi-objects (nconc objects (nreverse new-objects)))
    (process-put proc 'fake-pi-invalid-lines (nconc invalid (nreverse new-invalid)))))

(defun pi-coding-agent-fake-pi-test--start-process (scenario &optional extra-args)
  "Start fake-pi for SCENARIO with optional EXTRA-ARGS."
  (let ((proc (make-process
               :name (format "fake-pi-test-%s" scenario)
               :command (append (pi-coding-agent-test-fake-pi-executable)
                                (list "--mode" "rpc")
                                (pi-coding-agent-test-fake-pi-extra-args scenario extra-args))
               :connection-type 'pipe
               :coding 'utf-8-unix
               :filter #'pi-coding-agent-fake-pi-test--process-filter
               :noquery t)))
    (set-process-query-on-exit-flag proc nil)
    proc))

(defmacro pi-coding-agent-fake-pi-test-with-process (spec &rest body)
  "Bind PROC to a fake-pi process for SPEC, run BODY, then clean up.
SPEC is (PROC SCENARIO &rest EXTRA-ARGS)."
  (declare (indent 1) (debug t))
  (let ((proc (nth 0 spec))
        (scenario (nth 1 spec))
        (extra-args (nthcdr 2 spec)))
    `(let ((,proc (pi-coding-agent-fake-pi-test--start-process ,scenario (list ,@extra-args))))
       (unwind-protect
           (progn ,@body)
         (when (process-live-p ,proc)
           (delete-process ,proc))))))

(defun pi-coding-agent-fake-pi-test--send (proc command)
  "Send COMMAND plist to fake-pi PROC."
  (process-send-string proc (pi-coding-agent--encode-command command)))

(defun pi-coding-agent-fake-pi-test--pop-object (proc &optional timeout)
  "Pop the next parsed JSON object from PROC within TIMEOUT seconds."
  (unless (pi-coding-agent-test-wait-until
           (lambda () (process-get proc 'fake-pi-objects))
           (or timeout pi-coding-agent-fake-pi-test--timeout)
           0.01
           proc)
    (ert-fail
     (format "Timed out waiting for fake-pi output\nraw=%S\ninvalid=%S"
             (process-get proc 'fake-pi-raw-output)
             (process-get proc 'fake-pi-invalid-lines))))
  (let* ((objects (process-get proc 'fake-pi-objects))
         (next (car objects)))
    (process-put proc 'fake-pi-objects (cdr objects))
    next))

(defun pi-coding-agent-fake-pi-test--collect-until (proc predicate &optional timeout)
  "Collect objects from PROC until PREDICATE returns non-nil for the latest one."
  (let* ((items nil)
         (limit (or timeout pi-coding-agent-fake-pi-test--timeout))
         (deadline (+ (float-time) limit))
         done)
    (while (not done)
      (let* ((remaining (- deadline (float-time)))
             (item (pi-coding-agent-fake-pi-test--pop-object
                    proc (max 0.0 remaining))))
        (push item items)
        (setq done (funcall predicate item))))
    (nreverse items)))

(ert-deftest pi-coding-agent-fake-pi-test-collect-until-spends-one-timeout-budget ()
  "Repeated reads should spend one timeout budget instead of resetting it."
  (let ((timeouts nil)
        (items '((:type "message_start") (:type "agent_end"))))
    (cl-letf (((symbol-function 'pi-coding-agent-fake-pi-test--pop-object)
               (lambda (_proc timeout)
                 (push timeout timeouts)
                 (sleep-for 0.01)
                 (pop items))))
      (pi-coding-agent-fake-pi-test--collect-until
       :ignored
       (lambda (item) (equal (plist-get item :type) "agent_end"))
       1.0))
    (setq timeouts (nreverse timeouts))
    (should (= (length timeouts) 2))
    (should (> (car timeouts) (cadr timeouts)))))

(defun pi-coding-agent-fake-pi-test--event-types (objects)
  "Return the :type fields from OBJECTS."
  (mapcar (lambda (obj) (plist-get obj :type)) objects))

(defun pi-coding-agent-fake-pi-test--run-cli (&rest args)
  "Run fake-pi with ARGS and return `(:exit-code N :output STRING)'."
  (let ((command
         (concat
          (mapconcat #'shell-quote-argument
                     (append (pi-coding-agent-test-fake-pi-executable) args)
                     " ")
          " 2>&1")))
    (with-temp-buffer
      (list :exit-code (call-process-shell-command command nil (current-buffer) nil)
            :output (buffer-string)))))

(defmacro pi-coding-agent-fake-pi-test-with-session (spec &rest body)
  "Create a real pi-coding-agent session against fake-pi, then run BODY.
SPEC is (SESSION SCENARIO &rest EXTRA-ARGS)."
  (declare (indent 1) (debug t))
  (let ((session (nth 0 spec))
        (scenario (nth 1 spec))
        (extra-args (nthcdr 2 spec)))
    `(let* ((default-directory "/tmp/")
            (pi-coding-agent-executable
             (pi-coding-agent-test-fake-pi-executable))
            (pi-coding-agent-extra-args
             (pi-coding-agent-test-fake-pi-extra-args ,scenario (list ,@extra-args)))
            (,session nil))
       (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                 ((symbol-function 'pi-coding-agent--display-buffers) #'ignore))
         (unwind-protect
             (progn
               (pi-coding-agent)
               (let ((chat-name (pi-coding-agent-test--chat-buffer-name default-directory)))
                 (should
                  (pi-coding-agent-test-wait-until
                   (lambda ()
                     (let* ((chat-buf (get-buffer chat-name))
                            (input-buf (and chat-buf
                                            (with-current-buffer chat-buf
                                              pi-coding-agent--input-buffer)))
                            (proc (and chat-buf
                                       (with-current-buffer chat-buf
                                         pi-coding-agent--process))))
                       (and (buffer-live-p chat-buf)
                            (buffer-live-p input-buf)
                            (process-live-p proc))))
                   pi-coding-agent-fake-pi-test--timeout
                   0.01))
                 (let* ((chat-buf (get-buffer chat-name))
                        (input-buf (with-current-buffer chat-buf
                                     pi-coding-agent--input-buffer))
                        (proc (with-current-buffer chat-buf
                                pi-coding-agent--process)))
                   (setq ,session (list :chat-buffer chat-buf
                                        :input-buffer input-buf
                                        :process proc))
                   ,@body)))
           (let ((chat-buf (get-buffer (pi-coding-agent-test--chat-buffer-name default-directory))))
             (when chat-buf
               (let ((proc (buffer-local-value 'pi-coding-agent--process chat-buf)))
                 (when (process-live-p proc)
                   (set-process-query-on-exit-flag proc nil)))))
           (pi-coding-agent-test--kill-session-buffers default-directory))))))

(ert-deftest pi-coding-agent-fake-pi-test-get-state-handles-split-jsonl-record ()
  "get_state survives a deliberately split JSONL response."
  (pi-coding-agent-fake-pi-test-with-process
      (proc "prompt-lifecycle" "--split-response" "get_state:24")
    (pi-coding-agent-fake-pi-test--send proc '(:type "get_state"))
    (let* ((response (pi-coding-agent-fake-pi-test--pop-object proc))
           (data (plist-get response :data))
           (session-file (plist-get data :sessionFile)))
      (should (equal (plist-get response :type) "response"))
      (should (eq (plist-get response :success) t))
      (should (equal (plist-get response :command) "get_state"))
      (should (file-exists-p session-file)))))

(ert-deftest pi-coding-agent-fake-pi-test-requires-newline-before-eof ()
  "EOF alone must not act as an implicit JSONL record delimiter."
  (pi-coding-agent-fake-pi-test-with-process (proc "prompt-lifecycle")
    (process-send-string proc "{\"type\":\"get_state\"}")
    (process-send-eof proc)
    (should
     (pi-coding-agent-test-wait-until
      (lambda () (not (process-live-p proc)))
      pi-coding-agent-fake-pi-test--timeout
      0.01))
    (should-not (process-get proc 'fake-pi-objects))))

(ert-deftest pi-coding-agent-fake-pi-test-cli-rejects-unsupported-mode ()
  "The fake should fail fast when asked to run in an unsupported mode."
  (let* ((result (pi-coding-agent-fake-pi-test--run-cli
                  "--mode" "interactive"
                  "--scenario" "prompt-lifecycle"))
         (output (plist-get result :output)))
    (should-not (eq (plist-get result :exit-code) 0))
    (should (string-match-p "invalid choice" output))
    (should (string-match-p "interactive" output))))

(ert-deftest pi-coding-agent-fake-pi-test-cli-reports-missing-scenario-cleanly ()
  "The fake should name a missing scenario instead of showing a traceback."
  (let* ((result (pi-coding-agent-fake-pi-test--run-cli "--scenario" "does-not-exist"))
         (output (plist-get result :output)))
    (should-not (eq (plist-get result :exit-code) 0))
    (should (string-match-p "scenario not found: does-not-exist" output))
    (should-not (string-match-p "Traceback" output))))

(ert-deftest pi-coding-agent-fake-pi-test-cleans-session-root-on-exit ()
  "The fake removes its temporary session directory when the process exits."
  (pi-coding-agent-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pi-coding-agent-fake-pi-test--send proc '(:type "get_state"))
    (let* ((response (pi-coding-agent-fake-pi-test--pop-object proc))
           (session-file (plist-get (plist-get response :data) :sessionFile))
           (session-root (directory-file-name (file-name-directory session-file))))
      (should (file-directory-p session-root))
      (process-send-eof proc)
      (should
       (pi-coding-agent-test-wait-until
        (lambda () (not (process-live-p proc)))
        pi-coding-agent-fake-pi-test--timeout
        0.01))
      (should-not (file-exists-p session-root)))))

(ert-deftest pi-coding-agent-fake-pi-test-get-commands-returns-configured-commands ()
  "get_commands returns the scenario's slash-command list." 
  (pi-coding-agent-fake-pi-test-with-process (proc "extension-confirm")
    (pi-coding-agent-fake-pi-test--send proc '(:type "get_commands"))
    (let* ((response (pi-coding-agent-fake-pi-test--pop-object proc))
           (commands (plist-get (plist-get response :data) :commands))
           (first (aref commands 0)))
      (should (eq (plist-get response :success) t))
      (should (vectorp commands))
      (should (equal (plist-get first :name) "test-confirm"))
      (should (equal (plist-get first :source) "extension")))))

(ert-deftest pi-coding-agent-fake-pi-test-set-model-and-thinking-level-update-state ()
  "set_model and set_thinking_level change subsequent get_state responses."
  (pi-coding-agent-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pi-coding-agent-fake-pi-test--send
     proc '(:type "set_model" :provider "fake-provider" :modelId "fake-large"))
    (let ((model-response (pi-coding-agent-fake-pi-test--pop-object proc)))
      (should (eq (plist-get model-response :success) t))
      (should (equal (plist-get (plist-get model-response :data) :id) "fake-large")))
    (pi-coding-agent-fake-pi-test--send proc '(:type "set_thinking_level" :level "high"))
    (should (eq (plist-get (pi-coding-agent-fake-pi-test--pop-object proc) :success) t))
    (pi-coding-agent-fake-pi-test--send proc '(:type "get_state"))
    (let* ((state (pi-coding-agent-fake-pi-test--pop-object proc))
           (data (plist-get state :data))
           (model (plist-get data :model)))
      (should (equal (plist-get model :provider) "fake-provider"))
      (should (equal (plist-get model :id) "fake-large"))
      (should (equal (plist-get data :thinkingLevel) "high")))))

(ert-deftest pi-coding-agent-fake-pi-test-session-starts-through-emacs-seam ()
  "The fake works through `pi-coding-agent' startup and rendering paths." 
  (pi-coding-agent-fake-pi-test-with-session (session "prompt-lifecycle")
    (let* ((chat-buf (plist-get session :chat-buffer))
           (input-buf (plist-get session :input-buffer)))
      (with-current-buffer input-buf
        (erase-buffer)
        (insert "hello seam")
        (pi-coding-agent-send))
      (should
       (pi-coding-agent-test-wait-until
        (lambda ()
          (with-current-buffer chat-buf
            (string-match-p "Fake reply for: hello seam"
                            (buffer-string))))
        pi-coding-agent-fake-pi-test--timeout
        0.01
        (plist-get session :process)))
      (with-current-buffer chat-buf
        (should (file-exists-p (plist-get pi-coding-agent--state :session-file)))))))

(ert-deftest pi-coding-agent-fake-pi-test-extension-confirm-displays-through-emacs-seam ()
  "An extension confirm round-trip renders the follow-up message in chat." 
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
    (pi-coding-agent-fake-pi-test-with-session
        (session "extension-confirm" "--extension-timeout-ms" "500")
      (let* ((chat-buf (plist-get session :chat-buffer))
             (input-buf (plist-get session :input-buffer)))
        (with-current-buffer input-buf
          (erase-buffer)
          (insert "/test-confirm")
          (pi-coding-agent-send))
        (should
         (pi-coding-agent-test-wait-until
          (lambda ()
            (with-current-buffer chat-buf
              (string-match-p "CONFIRMED" (buffer-string))))
          pi-coding-agent-fake-pi-test--timeout
          0.01
          (plist-get session :process)))))))

(ert-deftest pi-coding-agent-fake-pi-test-custom-message-command-emits-visible-message ()
  "A custom-message command emits visible custom message events."
  (pi-coding-agent-fake-pi-test-with-process (proc "extension-message")
    (pi-coding-agent-fake-pi-test--send proc '(:type "prompt" :message "/test-message"))
    (let ((response (pi-coding-agent-fake-pi-test--pop-object proc)))
      (should (eq (plist-get response :success) t))
      (should (equal (plist-get response :command) "prompt")))
    (let* ((start (pi-coding-agent-fake-pi-test--pop-object proc))
           (message (plist-get start :message)))
      (should (equal (plist-get start :type) "message_start"))
      (should (equal (plist-get message :role) "custom"))
      (should (eq (plist-get message :display) t))
      (should (equal (plist-get message :content) "Test message from extension")))
    (let* ((end (pi-coding-agent-fake-pi-test--pop-object proc))
           (message (plist-get end :message)))
      (should (equal (plist-get end :type) "message_end"))
      (should (equal (plist-get message :role) "custom")))))

(ert-deftest pi-coding-agent-fake-pi-test-custom-noop-command-skips-message-events ()
  "A no-op custom-message command returns without emitting display events."
  (pi-coding-agent-fake-pi-test-with-process (proc "extension-noop")
    (pi-coding-agent-fake-pi-test--send proc '(:type "prompt" :message "/test-noop"))
    (let ((response (pi-coding-agent-fake-pi-test--pop-object proc)))
      (should (eq (plist-get response :success) t))
      (should (equal (plist-get response :command) "prompt")))
    (pi-coding-agent-fake-pi-test--send proc '(:type "get_state"))
    (let ((response (pi-coding-agent-fake-pi-test--pop-object proc)))
      (should (equal (plist-get response :command) "get_state"))
      (should (eq (plist-get response :success) t)))))

(ert-deftest pi-coding-agent-fake-pi-test-prompt-response-precedes-stream-events ()
  "prompt returns success first, then streams lifecycle events."
  (pi-coding-agent-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pi-coding-agent-fake-pi-test--send proc '(:type "prompt" :message "hello fake pi"))
    (let* ((response (pi-coding-agent-fake-pi-test--pop-object proc))
           (events (pi-coding-agent-fake-pi-test--collect-until
                    proc
                    (lambda (obj) (equal (plist-get obj :type) "agent_end"))))
           (assistant-start (seq-find
                             (lambda (obj)
                               (and (equal (plist-get obj :type) "message_start")
                                    (equal (plist-get (plist-get obj :message) :role)
                                           "assistant")))
                             events))
           (text-deltas (seq-filter
                         (lambda (obj)
                           (and (equal (plist-get obj :type) "message_update")
                                (equal (plist-get (plist-get obj :assistantMessageEvent) :type)
                                       "text_delta")))
                         events)))
      (should (equal (plist-get response :type) "response"))
      (should (eq (plist-get response :success) t))
      (should (equal (plist-get response :command) "prompt"))
      (should (equal (car (pi-coding-agent-fake-pi-test--event-types events)) "agent_start"))
      (should assistant-start)
      (should (> (length text-deltas) 0))
      (should (equal (car (last (pi-coding-agent-fake-pi-test--event-types events)))
                     "agent_end")))))

(ert-deftest pi-coding-agent-fake-pi-test-tool-stream-emits-tool-events ()
  "tool_stream scenarios emit the toolcall and tool_execution event surface." 
  (pi-coding-agent-fake-pi-test-with-process (proc "tool-read")
    (pi-coding-agent-fake-pi-test--send proc '(:type "prompt" :message "use the tool"))
    (should (equal (plist-get (pi-coding-agent-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (let* ((events (pi-coding-agent-fake-pi-test--collect-until
                    proc
                    (lambda (obj) (equal (plist-get obj :type) "agent_end"))))
           (toolcall-start (seq-find
                            (lambda (obj)
                              (and (equal (plist-get obj :type) "message_update")
                                   (equal (plist-get (plist-get obj :assistantMessageEvent) :type)
                                          "toolcall_start")))
                            events))
           (tool-execution-start (seq-find
                                  (lambda (obj)
                                    (equal (plist-get obj :type) "tool_execution_start"))
                                  events))
           (tool-execution-update (seq-find
                                   (lambda (obj)
                                     (equal (plist-get obj :type) "tool_execution_update"))
                                   events))
           (tool-execution-end (seq-find
                                (lambda (obj)
                                  (equal (plist-get obj :type) "tool_execution_end"))
                                events)))
      (should toolcall-start)
      (should tool-execution-start)
      (should tool-execution-update)
      (should tool-execution-end)
      (should (equal (plist-get tool-execution-start :toolName) "read"))
      (should (equal (plist-get (plist-get tool-execution-start :args) :path)
                     "/tmp/fake-tool.txt"))
      (should (string-match-p "fake tool output"
                              (plist-get (aref (plist-get (plist-get tool-execution-update
                                                                    :partialResult)
                                                          :content)
                                                 0)
                                         :text)))
      (should (equal (plist-get tool-execution-end :isError) :false)))))

(ert-deftest pi-coding-agent-fake-pi-test-abort-stops-streaming ()
  "abort stops an in-flight prompt and leaves the fake idle."
  (pi-coding-agent-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pi-coding-agent-fake-pi-test--send proc '(:type "prompt" :message "abort me"))
    (should (equal (plist-get (pi-coding-agent-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (let ((seen-first-delta nil)
          (seen-agent-end nil)
          (saw-stop-message-end nil)
          (saw-abort-response nil))
      (while (not seen-first-delta)
        (let* ((obj (pi-coding-agent-fake-pi-test--pop-object proc))
               (event-type (plist-get obj :type))
               (msg-event (plist-get obj :assistantMessageEvent)))
          (when (and (equal event-type "message_update")
                     (equal (plist-get msg-event :type) "text_delta"))
            (setq seen-first-delta t))))
      (pi-coding-agent-fake-pi-test--send proc '(:type "abort"))
      (while (not (and saw-abort-response seen-agent-end))
        (let ((obj (pi-coding-agent-fake-pi-test--pop-object proc)))
          (pcase (plist-get obj :type)
            ("response"
             (when (equal (plist-get obj :command) "abort")
               (setq saw-abort-response (eq (plist-get obj :success) t))))
            ("message_end"
             (when (equal (plist-get (plist-get obj :message) :stopReason) "stop")
               (setq saw-stop-message-end t)))
            ("agent_end"
             (setq seen-agent-end t)))))
      (should saw-abort-response)
      (should seen-agent-end)
      (should-not saw-stop-message-end)
      (pi-coding-agent-fake-pi-test--send proc '(:type "get_state"))
      (let* ((state (pi-coding-agent-fake-pi-test--pop-object proc))
             (data (plist-get state :data)))
        (should (eq (plist-get data :isStreaming) :false))))))

(ert-deftest pi-coding-agent-fake-pi-test-steer-queues-another-turn ()
  "steer queues another user turn and delivers it before agent_end."
  (pi-coding-agent-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pi-coding-agent-fake-pi-test--send proc '(:type "prompt" :message "first turn"))
    (should (equal (plist-get (pi-coding-agent-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (let ((seen-first-delta nil)
          (saw-steer-response nil)
          (saw-agent-end nil)
          (user-starts 0)
          (steered-reply nil))
      (while (not seen-first-delta)
        (let* ((obj (pi-coding-agent-fake-pi-test--pop-object proc))
               (msg-event (plist-get obj :assistantMessageEvent)))
          (when (and (equal (plist-get obj :type) "message_start")
                     (equal (plist-get (plist-get obj :message) :role) "user"))
            (setq user-starts (1+ user-starts)))
          (when (and (equal (plist-get obj :type) "message_update")
                     (equal (plist-get msg-event :type) "text_delta"))
            (setq seen-first-delta t))))
      (pi-coding-agent-fake-pi-test--send proc '(:type "steer" :message "second turn"))
      (while (not saw-agent-end)
        (let ((obj (pi-coding-agent-fake-pi-test--pop-object proc)))
          (pcase (plist-get obj :type)
            ("response"
             (when (equal (plist-get obj :command) "steer")
               (setq saw-steer-response (eq (plist-get obj :success) t))))
            ("message_start"
             (when (equal (plist-get (plist-get obj :message) :role) "user")
               (setq user-starts (1+ user-starts))))
            ("message_end"
             (let ((message (plist-get obj :message)))
               (when (and (equal (plist-get message :role) "assistant")
                          (string-match-p "Steered fake reply for: second turn"
                                          (or (plist-get (aref (plist-get message :content) 0)
                                                         :text)
                                              "")))
                 (setq steered-reply t))))
            ("agent_end"
             (setq saw-agent-end t)))))
      (should saw-steer-response)
      (should saw-agent-end)
      (should (= user-starts 2))
      (should steered-reply)
      (pi-coding-agent-fake-pi-test--send proc '(:type "get_fork_messages"))
      (let* ((fork-response (pi-coding-agent-fake-pi-test--pop-object proc))
             (messages (plist-get (plist-get fork-response :data) :messages)))
        (should (= (length messages) 2))
        (should (equal (plist-get (aref messages 1) :text) "second turn"))))))

(ert-deftest pi-coding-agent-fake-pi-test-new-session-resets-count-and-path ()
  "new_session resets state and returns a fresh real session file path."
  (pi-coding-agent-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pi-coding-agent-fake-pi-test--send proc '(:type "prompt" :message "before reset"))
    (should (equal (plist-get (pi-coding-agent-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (pi-coding-agent-fake-pi-test--collect-until
     proc (lambda (obj) (equal (plist-get obj :type) "agent_end")))
    (pi-coding-agent-fake-pi-test--send proc '(:type "get_state"))
    (let* ((before-state (pi-coding-agent-fake-pi-test--pop-object proc))
           (before-data (plist-get before-state :data))
           (before-file (plist-get before-data :sessionFile)))
      (should (> (plist-get before-data :messageCount) 0))
      (pi-coding-agent-fake-pi-test--send proc '(:type "new_session"))
      (let ((response (pi-coding-agent-fake-pi-test--pop-object proc)))
        (should (eq (plist-get response :success) t))
        (should (eq (plist-get (plist-get response :data) :cancelled) :false)))
      (pi-coding-agent-fake-pi-test--send proc '(:type "get_state"))
      (let* ((after-state (pi-coding-agent-fake-pi-test--pop-object proc))
             (after-data (plist-get after-state :data))
             (after-file (plist-get after-data :sessionFile)))
        (should (equal (plist-get after-data :messageCount) 0))
        (should (not (equal after-file before-file)))
        (should (file-exists-p after-file))))))

(ert-deftest pi-coding-agent-fake-pi-test-new-session-waits-for-old-run-to-stop ()
  "new_session should not leak stale streaming events after it succeeds."
  (pi-coding-agent-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pi-coding-agent-fake-pi-test--send proc '(:type "prompt" :message "before reset"))
    (should (equal (plist-get (pi-coding-agent-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (let ((seen-first-delta nil)
          (new-session-response nil))
      (while (not seen-first-delta)
        (let* ((obj (pi-coding-agent-fake-pi-test--pop-object proc))
               (msg-event (plist-get obj :assistantMessageEvent)))
          (when (and (equal (plist-get obj :type) "message_update")
                     (equal (plist-get msg-event :type) "text_delta"))
            (setq seen-first-delta t))))
      (pi-coding-agent-fake-pi-test--send proc '(:type "new_session"))
      (while (not new-session-response)
        (let ((obj (pi-coding-agent-fake-pi-test--pop-object proc)))
          (when (and (equal (plist-get obj :type) "response")
                     (equal (plist-get obj :command) "new_session"))
            (setq new-session-response obj))))
      (sleep-for 0.2)
      (should-not (process-get proc 'fake-pi-objects))
      (pi-coding-agent-fake-pi-test--send proc '(:type "get_state"))
      (let* ((state (pi-coding-agent-fake-pi-test--pop-object proc))
             (data (plist-get state :data)))
        (should (equal (plist-get data :messageCount) 0))
        (should (eq (plist-get data :isStreaming) :false))))))

(ert-deftest pi-coding-agent-fake-pi-test-set-session-name-writes-session-info ()
  "set_session_name appends a real session_info entry that Emacs can parse."
  (pi-coding-agent-fake-pi-test-with-process (proc "prompt-lifecycle")
    (pi-coding-agent-fake-pi-test--send proc '(:type "prompt" :message "session me"))
    (should (equal (plist-get (pi-coding-agent-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (pi-coding-agent-fake-pi-test--collect-until
     proc (lambda (obj) (equal (plist-get obj :type) "agent_end")))
    (pi-coding-agent-fake-pi-test--send proc '(:type "get_state"))
    (let* ((state (pi-coding-agent-fake-pi-test--pop-object proc))
           (session-file (plist-get (plist-get state :data) :sessionFile)))
      (pi-coding-agent-fake-pi-test--send
       proc '(:type "set_session_name" :name "Fake Harness Session"))
      (let ((response (pi-coding-agent-fake-pi-test--pop-object proc)))
        (should (eq (plist-get response :success) t)))
      (with-temp-buffer
        (insert-file-contents session-file)
        (should (string-match-p "session_info" (buffer-string)))
        (should (string-match-p "Fake Harness Session" (buffer-string))))
      (let ((metadata (pi-coding-agent--session-metadata session-file)))
        (should metadata)
        (should (equal (plist-get metadata :session-name)
                       "Fake Harness Session"))))))

(ert-deftest pi-coding-agent-fake-pi-test-extension-confirm-zero-timeout-disables-expiry ()
  "An override of 0 disables dialog expiry for manual debugging." 
  (pi-coding-agent-fake-pi-test-with-process
      (proc "extension-confirm" "--extension-timeout-ms" "0")
    (pi-coding-agent-fake-pi-test--send proc '(:type "prompt" :message "/test-confirm"))
    (should (equal (plist-get (pi-coding-agent-fake-pi-test--pop-object proc) :command)
                   "prompt"))
    (should (equal (plist-get (pi-coding-agent-fake-pi-test--pop-object proc) :type)
                   "agent_start"))
    (let ((request (pi-coding-agent-fake-pi-test--pop-object proc)))
      (should (equal (plist-get request :type) "extension_ui_request"))
      (should-not (plist-member request :timeout))
      (sleep-for 0.2)
      (should-not (process-get proc 'fake-pi-objects))
      (pi-coding-agent-fake-pi-test--send
       proc
       (list :type "extension_ui_response"
             :id (plist-get request :id)
             :confirmed t))
      (let* ((events (pi-coding-agent-fake-pi-test--collect-until
                      proc
                      (lambda (obj) (equal (plist-get obj :type) "agent_end"))))
             (custom-end (seq-find
                          (lambda (obj)
                            (and (equal (plist-get obj :type) "message_end")
                                 (equal (plist-get (plist-get obj :message) :content)
                                        "CONFIRMED")))
                          events)))
        (should custom-end)))))

(ert-deftest pi-coding-agent-fake-pi-test-extension-confirm-honors-timeout-override ()
  "CLI timeout override allows a delayed extension UI response to succeed."
  (pi-coding-agent-fake-pi-test-with-process
      (proc "extension-confirm" "--extension-timeout-ms" "500")
    (pi-coding-agent-fake-pi-test--send proc '(:type "prompt" :message "/test-confirm"))
    (let* ((prompt-response (pi-coding-agent-fake-pi-test--pop-object proc))
           (agent-start (pi-coding-agent-fake-pi-test--pop-object proc))
           (request (pi-coding-agent-fake-pi-test--pop-object proc)))
      (should (equal (plist-get prompt-response :command) "prompt"))
      (should (equal (plist-get agent-start :type) "agent_start"))
      (should (equal (plist-get request :type) "extension_ui_request"))
      (should (equal (plist-get request :method) "confirm"))
      (should (= (plist-get request :timeout) 500))
      (sleep-for 0.15)
      (pi-coding-agent-fake-pi-test--send
       proc
       (list :type "extension_ui_response"
             :id (plist-get request :id)
             :confirmed t))
      (let* ((events (pi-coding-agent-fake-pi-test--collect-until
                      proc
                      (lambda (obj) (equal (plist-get obj :type) "agent_end"))))
             (custom-end (seq-find
                          (lambda (obj)
                            (and (equal (plist-get obj :type) "message_end")
                                 (equal (plist-get (plist-get obj :message) :content)
                                        "CONFIRMED")))
                          events)))
        (should custom-end)))))

(provide 'pi-coding-agent-fake-pi-test)
;;; pi-coding-agent-fake-pi-test.el ends here
