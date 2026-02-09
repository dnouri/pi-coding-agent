;;; pi-coding-agent-core.el --- Core functionality for pi-coding-agent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pi-coding-agent

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Core functionality for pi-coding-agent: JSON parsing, line buffering, RPC communication.
;; This module provides the low-level plumbing for communicating with the
;; pi coding agent via JSON-over-stdio.

;;; Code:

(require 'cl-lib)
(require 'json)

;;;; JSON Parsing

(defun pi-coding-agent--parse-json-line (line)
  "Parse LINE as JSON, returning a plist.
Returns nil if LINE is not valid JSON."
  (condition-case nil
      (json-parse-string line :object-type 'plist)
    (json-error nil)))

;;;; Line Accumulation

(defun pi-coding-agent--accumulate-lines (accumulated chunk)
  ;; Note: Empty strings are filtered here because they're not valid JSON.
  ;; This couples line splitting with JSON semantics, but keeps the API simple.
  "Accumulate CHUNK into ACCUMULATED, extracting complete lines.
Returns a cons cell (COMPLETE-LINES . REMAINDER) where COMPLETE-LINES
is a list of complete newline-terminated lines (without the newlines)
and REMAINDER is any incomplete line fragment to save for next call."
  (let* ((combined (concat accumulated chunk))
         (parts (split-string combined "\n"))
         (complete-lines (butlast parts))
         (remainder (car (last parts))))
    (cons (seq-filter (lambda (s) (not (string-empty-p s))) complete-lines)
          remainder)))

;;;; JSON Encoding

(defun pi-coding-agent--encode-command (command)
  "Encode COMMAND plist as a JSON line for sending to pi.
COMMAND must be a valid plist with string/number/list values.
Returns a JSON string terminated with a newline."
  (concat (json-encode command) "\n"))

;;;; Request ID Management

(defvar pi-coding-agent--request-id-counter 0
  "Counter for generating unique request IDs.")

(defun pi-coding-agent--next-request-id ()
  "Generate the next unique request ID."
  (format "req_%d" (cl-incf pi-coding-agent--request-id-counter)))

(defun pi-coding-agent--get-pending-requests (process)
  "Get or create the pending requests hash table for PROCESS.
Each process has its own table stored as a process property."
  (or (process-get process 'pi-coding-agent-pending-requests)
      (let ((table (make-hash-table :test 'equal)))
        (process-put process 'pi-coding-agent-pending-requests table)
        table)))

(defun pi-coding-agent--rpc-async (process command callback)
  "Send COMMAND to pi PROCESS asynchronously.
COMMAND is a plist that will be augmented with a unique ID.
CALLBACK is called with the response plist when received."
  (let* ((id (pi-coding-agent--next-request-id))
         (full-command (plist-put (copy-sequence command) :id id))
         (pending (pi-coding-agent--get-pending-requests process)))
    (puthash id callback pending)
    (process-send-string process (pi-coding-agent--encode-command full-command))))

(defun pi-coding-agent--send-extension-ui-response (process response)
  "Send extension UI RESPONSE to pi PROCESS.
RESPONSE must include the original :id from the request, as pi uses
this to match responses to pending promises."
  (process-send-string process (pi-coding-agent--encode-command response)))

(defun pi-coding-agent--rpc-sync (process command &optional timeout)
  "Send COMMAND to pi PROCESS synchronously, returning the response.
Blocks until response is received or TIMEOUT seconds elapse.
TIMEOUT defaults to `pi-coding-agent-rpc-timeout' (or 30 seconds).
Returns nil on timeout."
  (let ((response nil)
        (timeout (or timeout
                     (and (boundp 'pi-coding-agent-rpc-timeout) pi-coding-agent-rpc-timeout)
                     30))
        (start-time (float-time)))
    (pi-coding-agent--rpc-async process command (lambda (r) (setq response r)))
    (while (and (null response)
                (< (- (float-time) start-time) timeout)
                (process-live-p process))
      (accept-process-output process 0.1))
    response))

;;;; Process Management

(defun pi-coding-agent--process-filter (proc output)
  "Handle OUTPUT from pi PROC.
Accumulates output and processes complete JSON lines.
Uses process property for per-process partial output storage."
  (let* ((partial (or (process-get proc 'pi-coding-agent-partial-output) ""))
         (result (pi-coding-agent--accumulate-lines partial output))
         (lines (car result)))
    (process-put proc 'pi-coding-agent-partial-output (cdr result))
    (dolist (line lines)
      (when-let ((json (pi-coding-agent--parse-json-line line)))
        (pi-coding-agent--dispatch-response proc json)))))

(defun pi-coding-agent--process-sentinel (proc event)
  "Handle process state change EVENT for PROC."
  (unless (process-live-p proc)
    (pi-coding-agent--handle-process-exit proc event)))

(defun pi-coding-agent--dispatch-response (proc json)
  "Dispatch JSON response from PROC to callback or event handler.
If JSON is a response with a known ID, call the stored callback.
Otherwise, treat it as an event and call the process's handler."
  (let ((type (plist-get json :type))
        (id (plist-get json :id)))
    (if (and (equal type "response") id)
        (let ((pending (pi-coding-agent--get-pending-requests proc)))
          (when-let ((callback (gethash id pending)))
            (remhash id pending)
            (funcall callback json)))
      ;; Call only this process's handler, not all handlers
      (pi-coding-agent--handle-event proc json))))

(defun pi-coding-agent--handle-event (proc event)
  "Handle an EVENT from pi PROC.
Calls only the handler registered for this specific process."
  ;; Call only this process's handler
  (when-let ((handler (process-get proc 'pi-coding-agent-display-handler)))
    (funcall handler event)))

(defun pi-coding-agent--handle-process-exit (proc event)
  "Clean up when pi process PROC exits with EVENT.
Calls pending request callbacks for this process with an error response
containing EVENT, then clears this process's pending requests table."
  (let ((pending (process-get proc 'pi-coding-agent-pending-requests))
        (error-response (list :type "response"
                              :success :false
                              :error (format "Process exited: %s" (string-trim event)))))
    (when pending
      (maphash (lambda (_id callback)
                 (funcall callback error-response))
               pending)
      (clrhash pending))))

(defvar pi-coding-agent-extra-args nil
  "Extra arguments to pass to the pi command.
A list of strings that will be appended to the base command.

Example: (setq pi-coding-agent-extra-args \\='(\"-e\" \"/path/to/ext.ts\"))

This is useful for testing extensions or passing additional flags.")

(defun pi-coding-agent--start-process (directory)
  "Start pi RPC process in DIRECTORY.
Returns the process object."
  (let ((default-directory directory))
    (make-process
     :name "pi"
     :command `("pi" "--mode" "rpc" ,@pi-coding-agent-extra-args)
     :connection-type 'pipe
     :filter #'pi-coding-agent--process-filter
     :sentinel #'pi-coding-agent--process-sentinel)))

;;;; State Management

(defvar-local pi-coding-agent--status 'idle
  "Current status of the pi session (buffer-local in chat buffer).
One of: `idle', `streaming', `compacting'.
This is the single source of truth for session activity state.

Status transitions are driven by events from pi:
- `idle' -> `streaming' on agent_start
- `streaming' -> `idle' on agent_end
- `idle' -> `compacting' on auto_compaction_start
- `compacting' -> `idle' on auto_compaction_end")

(defvar-local pi-coding-agent--state nil
  "Current state of the pi session (buffer-local in chat buffer).
A plist with keys like :model, :thinking-level, :messages, etc.")

(defvar-local pi-coding-agent--state-timestamp nil
  "Time when state was last updated (buffer-local in chat buffer).")

(defconst pi-coding-agent--state-verify-interval 30
  "Seconds between state verification checks.")

(defun pi-coding-agent--state-needs-verification-p ()
  "Return t if state should be verified with get_state.
Verification is needed when:
- State and timestamp exist
- Session is idle (not streaming or compacting)
- Timestamp is older than `pi-coding-agent--state-verify-interval' seconds."
  (and pi-coding-agent--state
       pi-coding-agent--state-timestamp
       (eq pi-coding-agent--status 'idle)
       (> (- (float-time) pi-coding-agent--state-timestamp)
          pi-coding-agent--state-verify-interval)))

(defun pi-coding-agent--json-false-p (value)
  "Return t if VALUE represents JSON false."
  (eq value :false))

(defun pi-coding-agent--json-null-p (value)
  "Return t if VALUE represents JSON null.
`json-parse-string' decodes JSON null as the keyword :null."
  (eq value :null))

(defun pi-coding-agent--normalize-boolean (value)
  "Convert JSON boolean VALUE to Elisp boolean.
JSON true (t) stays t, JSON false (:false) becomes nil."
  (if (pi-coding-agent--json-false-p value) nil value))

(defun pi-coding-agent--normalize-string-or-null (value)
  "Return VALUE if it's a string, nil otherwise.
Use when reading JSON fields that may be null or string.
JSON null (:null) and non-strings become nil."
  (and (stringp value) value))

(defun pi-coding-agent--update-state-from-event (event)
  "Update status and state based on EVENT.
Handles agent lifecycle, message events, and error/retry events.
Sets status to `streaming' on agent_start, `idle' on agent_end."
  (let ((type (plist-get event :type)))
    (pcase type
      ("agent_start"
       (setq pi-coding-agent--status 'streaming)
       (plist-put pi-coding-agent--state :is-retrying nil)
       (plist-put pi-coding-agent--state :last-error nil)
       (setq pi-coding-agent--state-timestamp (float-time)))
      ("agent_end"
       (setq pi-coding-agent--status 'idle)
       (plist-put pi-coding-agent--state :is-retrying nil)
       (plist-put pi-coding-agent--state :messages (plist-get event :messages))
       (setq pi-coding-agent--state-timestamp (float-time)))
      ("message_start"
       (plist-put pi-coding-agent--state :current-message (plist-get event :message)))
      ("message_end"
       (plist-put pi-coding-agent--state :current-message nil))
      ("tool_execution_start"
       (pi-coding-agent--handle-tool-start event))
      ("tool_execution_update"
       (pi-coding-agent--handle-tool-update event))
      ("tool_execution_end"
       (pi-coding-agent--handle-tool-end event))
      ("auto_retry_start"
       (plist-put pi-coding-agent--state :is-retrying t)
       (plist-put pi-coding-agent--state :retry-attempt (plist-get event :attempt))
       (plist-put pi-coding-agent--state :last-error (plist-get event :errorMessage)))
      ("auto_retry_end"
       (plist-put pi-coding-agent--state :is-retrying nil)
       (unless (eq (plist-get event :success) t)
         (plist-put pi-coding-agent--state :last-error (plist-get event :finalError))))
      ("extension_error"
       (plist-put pi-coding-agent--state :last-error (plist-get event :error))))))

(defun pi-coding-agent--ensure-active-tools ()
  "Ensure :active-tools hash table exists in state."
  (unless (plist-get pi-coding-agent--state :active-tools)
    (setq pi-coding-agent--state (plist-put pi-coding-agent--state :active-tools
                                (make-hash-table :test 'equal))))
  (plist-get pi-coding-agent--state :active-tools))

(defun pi-coding-agent--handle-tool-start (event)
  "Handle tool_execution_start EVENT."
  (let ((tools (pi-coding-agent--ensure-active-tools))
        (id (plist-get event :toolCallId))
        (name (plist-get event :toolName))
        (args (plist-get event :args)))
    (puthash id (list :name name :args args) tools)))

(defun pi-coding-agent--handle-tool-update (event)
  "Handle tool_execution_update EVENT."
  (let* ((tools (plist-get pi-coding-agent--state :active-tools))
         (id (plist-get event :toolCallId))
         (tool (and tools (gethash id tools))))
    (when tool
      (plist-put tool :partial-result (plist-get event :partialResult)))))

(defun pi-coding-agent--handle-tool-end (event)
  "Handle tool_execution_end EVENT."
  (let* ((tools (plist-get pi-coding-agent--state :active-tools))
         (id (plist-get event :toolCallId)))
    (when tools
      (remhash id tools))))

(defun pi-coding-agent--update-state-from-response (response)
  "Update state from a command RESPONSE.
Only processes successful responses for state-modifying commands."
  (when (eq (plist-get response :success) t)
    (let ((command (plist-get response :command))
          (data (plist-get response :data)))
      (pcase command
        ("set_model"
         (plist-put pi-coding-agent--state :model data)
         (setq pi-coding-agent--state-timestamp (float-time)))
        ("cycle_model"
         (when data
           (plist-put pi-coding-agent--state :model (plist-get data :model))
           (plist-put pi-coding-agent--state :thinking-level (plist-get data :thinkingLevel))
           (setq pi-coding-agent--state-timestamp (float-time))))
        ("cycle_thinking_level"
         (when data
           (plist-put pi-coding-agent--state :thinking-level (plist-get data :level))
           (setq pi-coding-agent--state-timestamp (float-time))))
        ("set_thinking_level"
         (setq pi-coding-agent--state-timestamp (float-time)))
        ("get_state"
         (let ((new-state (pi-coding-agent--extract-state-from-response response)))
           (setq pi-coding-agent--status (plist-get new-state :status)
                 pi-coding-agent--state new-state
                 pi-coding-agent--state-timestamp (float-time))))))))

(defun pi-coding-agent--extract-state-from-response (response)
  "Extract state plist from a get_state RESPONSE.
Converts camelCase keys to kebab-case and normalizes booleans.
Returns plist with :status key for setting `pi-coding-agent--status'."
  (when-let ((data (plist-get response :data)))
    (let ((is-streaming (pi-coding-agent--normalize-boolean (plist-get data :isStreaming)))
          (is-compacting (pi-coding-agent--normalize-boolean (plist-get data :isCompacting))))
      (list :status (cond (is-streaming 'streaming)
                          (is-compacting 'compacting)
                          (t 'idle))
            :model (plist-get data :model)
            :thinking-level (plist-get data :thinkingLevel)
            :session-id (plist-get data :sessionId)
            :session-file (plist-get data :sessionFile)
            :message-count (plist-get data :messageCount)
            :pending-message-count (plist-get data :pendingMessageCount)))))

(provide 'pi-coding-agent-core)
;;; pi-coding-agent-core.el ends here
