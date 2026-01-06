;;; pi-core.el --- Core functionality for pi.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pi.el
;; Keywords: ai llm ai-pair-programming tools
;; Version: 0.1.0

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

;; Core functionality for pi.el: JSON parsing, line buffering, RPC communication.
;; This module provides the low-level plumbing for communicating with the
;; pi coding agent via JSON-over-stdio.

;;; Code:

(require 'cl-lib)
(require 'json)

;;;; JSON Parsing

(defun pi--parse-json-line (line)
  "Parse LINE as JSON, returning a plist.
Returns nil if LINE is not valid JSON."
  (condition-case nil
      (json-parse-string line :object-type 'plist)
    (json-error nil)))

;;;; Line Accumulation

(defun pi--accumulate-lines (accumulated chunk)
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

(defun pi--encode-command (command)
  "Encode COMMAND plist as a JSON line for sending to pi.
COMMAND must be a valid plist with string/number/list values.
Returns a JSON string terminated with a newline."
  (concat (json-encode command) "\n"))

;;;; Request ID Management

(defvar pi--request-id-counter 0
  "Counter for generating unique request IDs.")

(defun pi--next-request-id ()
  "Generate the next unique request ID."
  (format "req_%d" (cl-incf pi--request-id-counter)))

(defun pi--get-pending-requests (process)
  "Get or create the pending requests hash table for PROCESS.
Each process has its own table stored as a process property."
  (or (process-get process 'pi-pending-requests)
      (let ((table (make-hash-table :test 'equal)))
        (process-put process 'pi-pending-requests table)
        table)))

(defun pi--rpc-async (process command callback)
  "Send COMMAND to pi PROCESS asynchronously.
COMMAND is a plist that will be augmented with a unique ID.
CALLBACK is called with the response plist when received."
  (let* ((id (pi--next-request-id))
         (full-command (plist-put (copy-sequence command) :id id))
         (pending (pi--get-pending-requests process)))
    (puthash id callback pending)
    (process-send-string process (pi--encode-command full-command))))

(defun pi--rpc-sync (process command &optional timeout)
  "Send COMMAND to pi PROCESS synchronously, returning the response.
Blocks until response is received or TIMEOUT seconds elapse.
TIMEOUT defaults to `pi-rpc-timeout' (or 30 seconds).  Returns nil on timeout."
  (let ((response nil)
        (timeout (or timeout
                     (and (boundp 'pi-rpc-timeout) pi-rpc-timeout)
                     30))
        (start-time (float-time)))
    (pi--rpc-async process command (lambda (r) (setq response r)))
    (while (and (null response)
                (< (- (float-time) start-time) timeout)
                (process-live-p process))
      (accept-process-output process 0.1))
    response))

;;;; Process Management

(defun pi--process-filter (proc output)
  "Handle OUTPUT from pi PROC.
Accumulates output and processes complete JSON lines.
Uses process property for per-process partial output storage."
  (let* ((partial (or (process-get proc 'pi-partial-output) ""))
         (result (pi--accumulate-lines partial output))
         (lines (car result)))
    (process-put proc 'pi-partial-output (cdr result))
    (dolist (line lines)
      (when-let ((json (pi--parse-json-line line)))
        (pi--dispatch-response proc json)))))

(defun pi--process-sentinel (proc event)
  "Handle process state change EVENT for PROC."
  (unless (process-live-p proc)
    (pi--handle-process-exit proc event)))

(defun pi--dispatch-response (proc json)
  "Dispatch JSON response from PROC to callback or event handler.
If JSON is a response with a known ID, call the stored callback.
Otherwise, treat it as an event and call the process's handler."
  (let ((type (plist-get json :type))
        (id (plist-get json :id)))
    (if (and (equal type "response") id)
        (let ((pending (pi--get-pending-requests proc)))
          (when-let ((callback (gethash id pending)))
            (remhash id pending)
            (funcall callback json)))
      ;; Call only this process's handler, not all handlers
      (pi--handle-event proc json))))

(defun pi--handle-event (proc event)
  "Handle an EVENT from pi PROC.
Calls only the handler registered for this specific process."
  ;; Call only this process's handler
  (when-let ((handler (process-get proc 'pi-display-handler)))
    (funcall handler event)))

(defun pi--handle-process-exit (proc event)
  "Clean up when pi process PROC exits with EVENT.
Calls pending request callbacks for this process with an error response
containing EVENT, then clears this process's pending requests table."
  (let ((pending (process-get proc 'pi-pending-requests))
        (error-response (list :type "response"
                              :success :false
                              :error (format "Process exited: %s" (string-trim event)))))
    (when pending
      (maphash (lambda (_id callback)
                 (funcall callback error-response))
               pending)
      (clrhash pending))))

(defun pi--start-process (directory)
  "Start pi RPC process in DIRECTORY.
Returns the process object."
  (let ((default-directory directory))
    (make-process
     :name "pi"
     :command '("pi" "--mode" "rpc")
     :connection-type 'pipe
     :filter #'pi--process-filter
     :sentinel #'pi--process-sentinel
     :noquery t)))

;;;; State Management

(defvar-local pi--status 'idle
  "Current status of the pi session (buffer-local in chat buffer).
One of: `idle', `sending', `streaming', `compacting'.
This is the single source of truth for session activity state.")

(defvar-local pi--state nil
  "Current state of the pi session (buffer-local in chat buffer).
A plist with keys like :model, :thinking-level, :messages, etc.")

(defvar-local pi--state-timestamp nil
  "Time when state was last updated (buffer-local in chat buffer).")

(defconst pi--state-verify-interval 30
  "Seconds between state verification checks.")

(defun pi--state-needs-verification-p ()
  "Return t if state should be verified with get_state.
Verification is needed when:
- State and timestamp exist
- Session is idle (not streaming, sending, or compacting)
- Timestamp is older than `pi--state-verify-interval' seconds."
  (and pi--state
       pi--state-timestamp
       (eq pi--status 'idle)
       (> (- (float-time) pi--state-timestamp)
          pi--state-verify-interval)))

(defun pi--json-false-p (value)
  "Return t if VALUE represents JSON false."
  (eq value :false))

(defun pi--normalize-boolean (value)
  "Convert JSON boolean VALUE to Elisp boolean.
JSON true (t) stays t, JSON false (:false) becomes nil."
  (if (pi--json-false-p value) nil value))

(defun pi--update-state-from-event (event)
  "Update `pi--status' and `pi--state' based on EVENT.
Handles agent lifecycle, message events, and error/retry events.
Sets `pi--status' to `streaming' on agent_start, `idle' on agent_end."
  (let ((type (plist-get event :type)))
    (pcase type
      ("agent_start"
       (setq pi--status 'streaming)
       (plist-put pi--state :is-retrying nil)
       (plist-put pi--state :last-error nil)
       (setq pi--state-timestamp (float-time)))
      ("agent_end"
       (setq pi--status 'idle)
       (plist-put pi--state :is-retrying nil)
       (plist-put pi--state :messages (plist-get event :messages))
       (setq pi--state-timestamp (float-time)))
      ("message_start"
       (plist-put pi--state :current-message (plist-get event :message)))
      ("message_update"
       (pi--handle-message-update event))
      ("message_end"
       (plist-put pi--state :current-message nil))
      ("tool_execution_start"
       (pi--handle-tool-start event))
      ("tool_execution_update"
       (pi--handle-tool-update event))
      ("tool_execution_end"
       (pi--handle-tool-end event))
      ("auto_retry_start"
       (plist-put pi--state :is-retrying t)
       (plist-put pi--state :retry-attempt (plist-get event :attempt))
       (plist-put pi--state :last-error (plist-get event :errorMessage)))
      ("auto_retry_end"
       (plist-put pi--state :is-retrying nil)
       (unless (eq (plist-get event :success) t)
         (plist-put pi--state :last-error (plist-get event :finalError))))
      ("hook_error"
       (plist-put pi--state :last-error (plist-get event :error))))))

(defun pi--handle-message-update (event)
  "Handle a message_update EVENT by accumulating text deltas."
  (let* ((msg-event (plist-get event :assistantMessageEvent))
         (event-type (plist-get msg-event :type))
         (current (plist-get pi--state :current-message)))
    (when (and current (equal event-type "text_delta"))
      (let* ((delta (plist-get msg-event :delta))
             (content (or (plist-get current :content) ""))
             (new-content (concat content delta)))
        (plist-put current :content new-content)))))

(defun pi--ensure-active-tools ()
  "Ensure :active-tools hash table exists in state."
  (unless (plist-get pi--state :active-tools)
    (setq pi--state (plist-put pi--state :active-tools
                                (make-hash-table :test 'equal))))
  (plist-get pi--state :active-tools))

(defun pi--handle-tool-start (event)
  "Handle tool_execution_start EVENT."
  (let ((tools (pi--ensure-active-tools))
        (id (plist-get event :toolCallId))
        (name (plist-get event :toolName))
        (args (plist-get event :args)))
    (puthash id (list :name name :args args) tools)))

(defun pi--handle-tool-update (event)
  "Handle tool_execution_update EVENT."
  (let* ((tools (plist-get pi--state :active-tools))
         (id (plist-get event :toolCallId))
         (tool (and tools (gethash id tools))))
    (when tool
      (plist-put tool :partial-result (plist-get event :partialResult)))))

(defun pi--handle-tool-end (event)
  "Handle tool_execution_end EVENT."
  (let* ((tools (plist-get pi--state :active-tools))
         (id (plist-get event :toolCallId)))
    (when tools
      (remhash id tools))))

(defun pi--update-state-from-response (response)
  "Update state from a command RESPONSE.
Only processes successful responses for state-modifying commands."
  (when (eq (plist-get response :success) t)
    (let ((command (plist-get response :command))
          (data (plist-get response :data)))
      (pcase command
        ("set_model"
         (plist-put pi--state :model data)
         (setq pi--state-timestamp (float-time)))
        ("cycle_model"
         (when data
           (plist-put pi--state :model (plist-get data :model))
           (plist-put pi--state :thinking-level (plist-get data :thinkingLevel))
           (setq pi--state-timestamp (float-time))))
        ("cycle_thinking_level"
         (when data
           (plist-put pi--state :thinking-level (plist-get data :level))
           (setq pi--state-timestamp (float-time))))
        ("set_thinking_level"
         (setq pi--state-timestamp (float-time)))
        ("get_state"
         (let ((new-state (pi--extract-state-from-response response)))
           (setq pi--status (plist-get new-state :status)
                 pi--state new-state
                 pi--state-timestamp (float-time))))))))

(defun pi--extract-state-from-response (response)
  "Extract state plist from a get_state RESPONSE.
Converts camelCase keys to kebab-case and normalizes booleans.
Returns plist with :status key for setting `pi--status'."
  (when-let ((data (plist-get response :data)))
    (let ((is-streaming (pi--normalize-boolean (plist-get data :isStreaming)))
          (is-compacting (pi--normalize-boolean (plist-get data :isCompacting))))
      (list :status (cond (is-streaming 'streaming)
                          (is-compacting 'compacting)
                          (t 'idle))
            :model (plist-get data :model)
            :thinking-level (plist-get data :thinkingLevel)
            :session-id (plist-get data :sessionId)
            :session-file (plist-get data :sessionFile)
            :message-count (plist-get data :messageCount)
            :queued-message-count (plist-get data :queuedMessageCount)))))

(provide 'pi-core)
;;; pi-core.el ends here
