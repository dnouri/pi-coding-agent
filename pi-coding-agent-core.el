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

(defun pi-coding-agent--line-chunks-string (chunks)
  "Return CHUNKS, stored newest first, as one string."
  (if chunks
      (apply #'concat (nreverse (copy-sequence chunks)))
    ""))

(defun pi-coding-agent--accumulate-line-chunks (chunks chunk)
  "Accumulate CHUNK into partial line CHUNKS.
CHUNKS stores fragments for an unfinished line newest first.  Return a cons
cell whose car is COMPLETE-LINES and cdr is REMAINDER-CHUNKS.  COMPLETE-LINES
are newline-terminated lines without newlines, and REMAINDER-CHUNKS is the
unfinished line state to keep for the next `process-filter' call.

The important property is that an unfinished long JSON line is kept as chunks
and materialized only when its newline arrives.  Large `get_messages' RPC
responses are one huge JSON line, so repeatedly concatenating the partial line
would otherwise become allocation-heavy and effectively quadratic."
  ;; Note: Empty strings are filtered here because they're not valid JSON.
  ;; This couples line splitting with JSON semantics, but keeps the API simple.
  (let ((lines nil)
        (start 0)
        newline)
    (while (setq newline (string-search "\n" chunk start))
      (let ((line (substring chunk start newline)))
        (when chunks
          (push line chunks)
          (setq line (pi-coding-agent--line-chunks-string chunks)
                chunks nil))
        (unless (string-empty-p line)
          (push line lines)))
      (setq start (1+ newline)))
    (when (< start (length chunk))
      (push (substring chunk start) chunks))
    (cons (nreverse lines) chunks)))

(defun pi-coding-agent--accumulate-lines (accumulated chunk)
  "Accumulate CHUNK into ACCUMULATED, extracting complete lines.
Returns a cons cell (COMPLETE-LINES . REMAINDER) where COMPLETE-LINES
is a list of complete newline-terminated lines (without the newlines)
and REMAINDER is any incomplete line fragment to save for next call.

This string API is kept for tests and small helpers.  The process filter uses
`pi-coding-agent--accumulate-line-chunks' directly so very large partial lines
are not repeatedly copied."
  (let* ((accumulated (or accumulated ""))
         (chunks (unless (string-empty-p accumulated)
                   (list accumulated)))
         (result (pi-coding-agent--accumulate-line-chunks chunks chunk)))
    (cons (car result)
          (pi-coding-agent--line-chunks-string (cdr result)))))

;;;; JSON Encoding

(defun pi-coding-agent--encode-command (command)
  "Encode COMMAND plist as a JSON line for sending to pi.
COMMAND must be a valid plist with string/number/list values.
Returns a JSON string terminated with a newline."
  (concat (json-encode command) "\n"))

;;;; Path Boundary Helpers

(defun pi-coding-agent--path-string-contains-nul-p (path)
  "Return non-nil if PATH is a string containing a NUL byte."
  (and (stringp path)
       (cl-position ?\0 path :test #'=)))

(defun pi-coding-agent--ensure-usable-path-string (path)
  "Signal `user-error' when PATH is a malformed file name string.
Non-string PATH values are ignored so callers can keep treating missing or
non-string path metadata as absent."
  (when (pi-coding-agent--path-string-contains-nul-p path)
    (user-error "Path contains NUL byte"))
  path)

(defun pi-coding-agent--syntactic-remote-prefix (path)
  "Return PATH's TRAMP-looking prefix, or nil.
This is a fallback for contexts that temporarily bind
`file-name-handler-alist' to nil; ordinary operation still delegates to
`file-remote-p'."
  (when (and (stringp path)
             (string-match "\\`\\(/[^/:]+:[^\n]*:\\)\\(?:/\\|~\\)" path))
    (match-string 1 path)))

(defun pi-coding-agent--remote-prefix-from-handler (path)
  "Return PATH's full TRAMP prefix using `file-remote-p', or nil."
  (when-let* ((localname (and (stringp path)
                              (file-remote-p path 'localname))))
    (when (string-suffix-p localname path)
      (substring path 0 (- (length path) (length localname))))))

(defun pi-coding-agent--remote-prefix (&optional anchor)
  "Return the full TRAMP remote prefix for ANCHOR, or nil.
When ANCHOR is nil, use `default-directory'.  This keeps multi-hop TRAMP
routes intact and falls back to a syntax check without requiring a remote
connection."
  (let ((anchor (or anchor default-directory)))
    (and (stringp anchor)
         (not (string-empty-p anchor))
         (or (pi-coding-agent--remote-prefix-from-handler anchor)
             (pi-coding-agent--syntactic-remote-prefix anchor)))))

(defun pi-coding-agent--remote-home-path-p (path)
  "Return non-nil when PATH is tilde-prefixed."
  (and (stringp path)
       (> (length path) 0)
       (eq (aref path 0) ?~)))

(defun pi-coding-agent--plain-remote-home-path-p (path)
  "Return non-nil when PATH is `~' or has the `~/' prefix."
  (and (stringp path)
       (or (equal path "~")
           (string-prefix-p "~/" path))))

(defun pi-coding-agent--named-remote-home-path-p (path)
  "Return non-nil when PATH has a named-user home prefix such as `~root'."
  (and (pi-coding-agent--remote-home-path-p path)
       (not (pi-coding-agent--plain-remote-home-path-p path))))

(defun pi-coding-agent--remote-prefix-for-path (path)
  "Return PATH's full TRAMP remote prefix, or nil."
  (and (stringp path)
       (not (string-empty-p path))
       (or (pi-coding-agent--remote-prefix-from-handler path)
           (pi-coding-agent--syntactic-remote-prefix path))))

(defun pi-coding-agent--route-preserving-path-op-p (&rest paths)
  "Return non-nil when any of PATHS should bypass TRAMP handlers.
This predicate is for pure file-name string transforms only.  TRAMP's file
name handlers canonicalize multi-hop routes like `/ssh:bastion|sudo:host:' to
just the final hop for generic operations such as `expand-file-name'."
  (cl-some #'pi-coding-agent--remote-prefix-for-path paths))

(defun pi-coding-agent--route-preserving-expand-file-name (path &optional anchor)
  "Expand PATH against ANCHOR without collapsing TRAMP route text.
This helper only changes pure string normalization.  Callers must not use it to
wrap real file I/O or process creation.  When PATH or ANCHOR is remote-looking,
file-name handlers are disabled for the `expand-file-name' call so multi-hop
TRAMP prefixes remain byte-for-byte intact."
  (let* ((anchor (or anchor default-directory))
         (prefix (pi-coding-agent--remote-prefix anchor)))
    (cond
     ((and prefix
           (pi-coding-agent--remote-home-path-p path)
           (not (pi-coding-agent--remote-prefix-for-path path)))
      (concat prefix path))
     ((pi-coding-agent--route-preserving-path-op-p path anchor)
      (let ((file-name-handler-alist nil))
        (expand-file-name path anchor)))
     (t
      (expand-file-name path anchor)))))

(defun pi-coding-agent--route-preserving-file-name-as-directory (path)
  "Return PATH as a directory without collapsing TRAMP route text.
This is a pure string helper; do not use it around real file I/O."
  (if (pi-coding-agent--route-preserving-path-op-p path)
      (let ((file-name-handler-alist nil))
        (file-name-as-directory path))
    (file-name-as-directory path)))

(defun pi-coding-agent--route-preserving-file-name-directory (path)
  "Return PATH's directory without collapsing TRAMP route text.
This is a pure string helper; do not use it around real file I/O."
  (if (pi-coding-agent--route-preserving-path-op-p path)
      (let ((file-name-handler-alist nil))
        (file-name-directory path))
    (file-name-directory path)))

(defun pi-coding-agent--route-preserving-abbreviate-file-name (path)
  "Abbreviate PATH for display without collapsing TRAMP route text.
This is a pure string helper; do not use it around real file I/O."
  (if (pi-coding-agent--route-preserving-path-op-p path)
      (let ((file-name-handler-alist nil))
        (abbreviate-file-name path))
    (abbreviate-file-name path)))

(defun pi-coding-agent--ensure-compatible-remote-path (path &optional anchor)
  "Signal `user-error' when remote PATH is incompatible with ANCHOR.
ANCHOR defaults to `default-directory'.  Remote PATH must use the same TRAMP
prefix as a remote ANCHOR, and is rejected when ANCHOR is local.  Malformed
file name strings are rejected before TRAMP prefix checks."
  (pi-coding-agent--ensure-usable-path-string path)
  (let ((path-prefix (pi-coding-agent--remote-prefix-for-path path))
        (anchor-prefix (pi-coding-agent--remote-prefix anchor)))
    (when path-prefix
      (cond
       ((not anchor-prefix)
        (user-error "Remote path cannot be used with local session: %s" path))
       ((not (equal path-prefix anchor-prefix))
        (user-error "Remote path is not on this session host: %s" path))))))

(defun pi-coding-agent--emacs-path (path &optional anchor)
  "Return inbound PATH as an Emacs-local file name.
PATH must be a nonempty string; otherwise return nil.  Already-remote PATHs
must match ANCHOR's TRAMP prefix, and are rejected when ANCHOR is local.  With
a remote ANCHOR (or remote `default-directory'), process-local absolute paths
like /x and home paths like ~/x are prefixed with that remote prefix.  Relative
paths are expanded under ANCHOR or `default-directory'.  Malformed file name
strings signal `user-error'."
  (pi-coding-agent--ensure-usable-path-string path)
  (when (and (stringp path) (not (string-empty-p path)))
    (let ((prefix (pi-coding-agent--remote-prefix anchor))
          (anchor (or anchor default-directory)))
      (cond
       ((pi-coding-agent--remote-prefix-for-path path)
        (pi-coding-agent--ensure-compatible-remote-path path anchor)
        path)
       ((and prefix (string-prefix-p "/" path))
        (concat prefix path))
       ((and prefix (pi-coding-agent--remote-home-path-p path))
        (concat prefix path))
       (t
        (pi-coding-agent--route-preserving-expand-file-name path anchor))))))

(defun pi-coding-agent--emacs-directory (path &optional anchor)
  "Return inbound PATH as an Emacs directory name.
Optional ANCHOR is forwarded to `pi-coding-agent--emacs-path'.  This adds a
trailing slash when PATH is usable."
  (when-let* ((emacs-path (pi-coding-agent--emacs-path path anchor)))
    (pi-coding-agent--route-preserving-file-name-as-directory emacs-path)))

(defun pi-coding-agent--passive-emacs-path (path &optional anchor)
  "Return inbound backend PATH as an Emacs path, or nil when unsafe.
Optional ANCHOR is forwarded to `pi-coding-agent--emacs-path'.  This is for
passive Pi-originated metadata only.  It intentionally keeps
`pi-coding-agent--emacs-path' strict for explicit navigation, validation, and
outbound boundaries, while preventing malformed backend metadata from escaping
process filters or callbacks."
  (condition-case nil
      (pi-coding-agent--emacs-path path anchor)
    (error nil)))

(defun pi-coding-agent--local-name-for-process (path)
  "Return PATH without its TRAMP prefix, even when handlers are disabled."
  (if-let* ((prefix (pi-coding-agent--remote-prefix-for-path path)))
      (substring path (length prefix))
    (file-local-name path)))

(defun pi-coding-agent--process-local-path (path &optional anchor)
  "Return outbound PATH as the process-local path Pi should receive.
PATH must be a nonempty string; otherwise return nil.  Emacs/TRAMP paths are
converted only when they match remote ANCHOR; TRAMP paths are rejected for local
ANCHOR.  The paths `~' and `~/x' are preserved for remote sessions because only
Pi on that host can expand the remote account home.  Named-user homes such as
`~root/x' are rejected for remote sessions because Pi does not implement that
shell expansion.  Local sessions expand home, absolute, and relative paths
through Emacs before sending them over JSON.  Malformed file name strings signal
`user-error'."
  (pi-coding-agent--ensure-compatible-remote-path path anchor)
  (when (and (stringp path) (not (string-empty-p path)))
    (let ((remote-anchor (pi-coding-agent--remote-prefix anchor)))
      (cond
       ((and remote-anchor
             (pi-coding-agent--plain-remote-home-path-p path)
             (not (pi-coding-agent--remote-prefix-for-path path)))
        path)
       ((and remote-anchor
             (pi-coding-agent--named-remote-home-path-p path)
             (not (pi-coding-agent--remote-prefix-for-path path)))
        (user-error "Remote Pi paths only support ~ or ~/..., not named homes: %s"
                    path))
       (t
        (when-let* ((emacs-path (pi-coding-agent--emacs-path path anchor)))
          (let ((localname (pi-coding-agent--local-name-for-process emacs-path)))
            (when (and remote-anchor
                       (pi-coding-agent--named-remote-home-path-p localname))
              (user-error "Remote Pi paths only support ~ or ~/..., not named homes: %s"
                          localname))
            localname)))))))

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

(defun pi-coding-agent--get-pending-command-types (process)
  "Get or create pending command type table for PROCESS.
Maps request IDs to command type strings."
  (or (process-get process 'pi-coding-agent-pending-command-types)
      (let ((table (make-hash-table :test 'equal)))
        (process-put process 'pi-coding-agent-pending-command-types table)
        table)))

(defconst pi-coding-agent--remote-ready-marker
  "__PI_CODING_AGENT_RPC_READY_V1__"
  "Exact stdout line that marks a remote Pi process ready for stdin.")

(defun pi-coding-agent--process-awaiting-ready-p (process)
  "Return non-nil when PROCESS must emit the ready marker before sends."
  (and (process-get process 'pi-coding-agent-awaiting-ready-marker)
       (not (process-get process 'pi-coding-agent-ready))))

(defun pi-coding-agent--enqueue-outbound-string (process string)
  "Queue STRING to be sent to PROCESS when its ready marker arrives."
  (process-put process 'pi-coding-agent-outbound-queue
               (append (process-get process 'pi-coding-agent-outbound-queue)
                       (list string))))

(defun pi-coding-agent--flush-outbound-queue (process)
  "Flush queued outbound strings to PROCESS in FIFO order."
  (let ((queue (process-get process 'pi-coding-agent-outbound-queue)))
    (when queue
      (process-put process 'pi-coding-agent-outbound-queue nil)
      (dolist (string queue)
        (process-send-string process string)))))

(defun pi-coding-agent--mark-process-ready (process)
  "Mark PROCESS ready for stdin and flush queued outbound strings."
  (process-put process 'pi-coding-agent-ready t)
  (process-put process 'pi-coding-agent-awaiting-ready-marker nil)
  (pi-coding-agent--flush-outbound-queue process))

(defun pi-coding-agent--send-string (process string)
  "Send STRING to PROCESS, or queue it until a remote process is ready."
  (if (pi-coding-agent--process-awaiting-ready-p process)
      (pi-coding-agent--enqueue-outbound-string process string)
    (process-send-string process string)))

(defun pi-coding-agent--rpc-async (process command callback)
  "Send COMMAND to pi PROCESS asynchronously.
COMMAND is a plist that will be augmented with a unique ID.
CALLBACK is called with the response plist when received."
  (let* ((id (pi-coding-agent--next-request-id))
         (full-command (plist-put (copy-sequence command) :id id))
         (pending (pi-coding-agent--get-pending-requests process))
         (pending-types (pi-coding-agent--get-pending-command-types process)))
    (puthash id callback pending)
    (puthash id (plist-get command :type) pending-types)
    (pi-coding-agent--send-string
     process (pi-coding-agent--encode-command full-command))))

(defun pi-coding-agent--send-extension-ui-response (process response)
  "Send extension UI RESPONSE to pi PROCESS.
RESPONSE must include the original :id from the request, as pi uses
this to match responses to pending promises."
  (pi-coding-agent--send-string process (pi-coding-agent--encode-command response)))

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
Accumulates output and dispatches complete JSON lines."
  (let* ((inhibit-redisplay t)
         (partial (process-get proc 'pi-coding-agent-partial-output-chunks))
         (result (pi-coding-agent--accumulate-line-chunks partial output))
         (lines (car result)))
    (process-put proc 'pi-coding-agent-partial-output-chunks (cdr result))
    (dolist (line lines)
      (if (equal line pi-coding-agent--remote-ready-marker)
          (pi-coding-agent--mark-process-ready proc)
        (when-let* ((json (pi-coding-agent--parse-json-line line)))
          (condition-case err
              (pi-coding-agent--dispatch-response proc json)
            (error
             (message "pi-coding-agent: error dispatching process response: %s"
                      (error-message-string err)))))))))

(defun pi-coding-agent--process-sentinel (proc event)
  "Handle process state change EVENT for PROC."
  (unless (process-live-p proc)
    (pi-coding-agent--handle-process-exit proc event)))

(defun pi-coding-agent--dispatch-response (proc json)
  "Dispatch JSON response from PROC to callback or event handler.
Response routing order: explicit ID, id-less `:command' match, then
id-less sole pending request. Non-response JSON is treated as an event."
  (let ((type (plist-get json :type))
        (id (plist-get json :id)))
    (if (equal type "response")
        (let* ((pending (pi-coding-agent--get-pending-requests proc))
               (pending-types (pi-coding-agent--get-pending-command-types proc))
               (dispatch-response
                (lambda (request-id callback)
                  (remhash request-id pending)
                  (remhash request-id pending-types)
                  (funcall callback json))))
          (cond
           ((and id (gethash id pending))
            (funcall dispatch-response id (gethash id pending)))
           ((null id)
            (let ((matched-id nil)
                  (matched-callback nil)
                  (matched-count 0)
                  (command (plist-get json :command)))
              (when command
                (maphash (lambda (request-id command-type)
                           (when (equal command-type command)
                             (setq matched-count (1+ matched-count))
                             (when (= matched-count 1)
                               (setq matched-id request-id
                                     matched-callback (gethash request-id pending)))))
                         pending-types))
              (cond
               ((and (= matched-count 1) matched-callback)
                (funcall dispatch-response matched-id matched-callback))
               ((= (hash-table-count pending) 1)
                (let (only-id only-callback)
                  (maphash (lambda (request-id callback)
                             (setq only-id request-id
                                   only-callback callback))
                           pending)
                  (when only-callback
                    (funcall dispatch-response only-id only-callback)))))))))
      ;; Call only this process's handler, not all handlers
      (pi-coding-agent--handle-event proc json))))

(defun pi-coding-agent--handle-event (proc event)
  "Handle an EVENT from pi PROC.
Calls only the handler registered for this specific process."
  ;; Call only this process's handler
  (when-let* ((handler (process-get proc 'pi-coding-agent-display-handler)))
    (funcall handler event)))

(defconst pi-coding-agent--process-stderr-max-chars 4000
  "Maximum number of stderr characters to keep in process exit excerpts.")

(defun pi-coding-agent--process-stderr-excerpt (proc)
  "Return a bounded stderr excerpt for PROC, or nil when stderr is empty."
  (when-let* ((stderr-buf (process-get proc 'pi-coding-agent-stderr-buf))
              ((buffer-live-p stderr-buf)))
    (let ((text (string-trim-right
                 (with-current-buffer stderr-buf
                   (buffer-substring-no-properties (point-min) (point-max))))))
      (unless (string-empty-p text)
        (if (<= (length text) pi-coding-agent--process-stderr-max-chars)
            text
          (let* ((head-chars (/ pi-coding-agent--process-stderr-max-chars 2))
                 (tail-chars (- pi-coding-agent--process-stderr-max-chars
                                head-chars)))
            (concat (substring text 0 head-chars)
                    "\n… [stderr truncated] …\n"
                    (substring text (- (length text) tail-chars)))))))))

(defun pi-coding-agent--cleanup-process-stderr-buffer (proc)
  "Kill PROC's stderr buffer, if any, and clear its process property."
  (when-let* ((stderr-buf (process-get proc 'pi-coding-agent-stderr-buf)))
    (process-put proc 'pi-coding-agent-stderr-buf nil)
    (when (buffer-live-p stderr-buf)
      (when-let* ((stderr-proc (get-buffer-process stderr-buf)))
        (set-process-query-on-exit-flag stderr-proc nil)
        (delete-process stderr-proc))
      (kill-buffer stderr-buf))))

(defun pi-coding-agent--handle-process-exit (proc event)
  "Clean up when pi process PROC exits with EVENT.
Calls pending request callbacks for this process with an error response
containing EVENT, then clears this process's pending request tables."
  (let* ((pending (process-get proc 'pi-coding-agent-pending-requests))
         (pending-types (process-get proc 'pi-coding-agent-pending-command-types))
         (stderr (pi-coding-agent--process-stderr-excerpt proc))
         (error-response
          (append (list :type "response"
                        :success :false
                        :error (format "Process exited: %s" (string-trim event)))
                  (when stderr
                    (list :stderr stderr)))))
    (unwind-protect
        (unwind-protect
            (progn
              (when pending
                (maphash (lambda (_id callback)
                           (funcall callback error-response))
                         pending)
                (clrhash pending))
              (when pending-types
                (clrhash pending-types)))
          (when-let* ((handler (process-get proc 'pi-coding-agent-exit-handler)))
            (funcall handler error-response)))
      (pi-coding-agent--cleanup-process-stderr-buffer proc))))

(defvar pi-coding-agent-executable)  ; forward decl — core.el cannot require ui.el
(defvar pi-coding-agent-project-trust-policy) ; forward decl — defined in ui.el

(defvar pi-coding-agent-extra-args nil
  "Extra arguments to pass to the pi command.
A list of strings that will be appended to the base command before the
project trust flag selected by `pi-coding-agent-project-trust-policy'.

Example: (setq pi-coding-agent-extra-args \\='(\"-e\" \"/path/to/ext.ts\"))

This is useful for testing extensions or passing additional flags.")

(defun pi-coding-agent--project-trust-args ()
  "Return Pi CLI arguments for `pi-coding-agent-project-trust-policy'."
  (let ((policy (if (boundp 'pi-coding-agent-project-trust-policy)
                    pi-coding-agent-project-trust-policy
                  'approve)))
    (pcase policy
      ('approve '("--approve"))
      ('default nil)
      ('no-approve '("--no-approve"))
      (_ (error "Invalid pi-coding-agent-project-trust-policy: %S" policy)))))

(defun pi-coding-agent--pi-command ()
  "Return the argv used to start Pi in RPC mode."
  (append pi-coding-agent-executable
          '("--mode" "rpc")
          pi-coding-agent-extra-args
          (pi-coding-agent--project-trust-args)))

(defun pi-coding-agent--remote-start-command (command)
  "Return a remote shell wrapper COMMAND that emits the ready marker.
COMMAND is passed as `$0' and `$@' to preserve the original argv exactly."
  (append (list "sh" "-c"
                (format "printf '%%s\\n' %s; exec \"$0\" \"$@\""
                        (shell-quote-argument
                         pi-coding-agent--remote-ready-marker)))
          command))

(defun pi-coding-agent--start-process (directory)
  "Start pi RPC process in DIRECTORY.
Returns the process object."
  (let* ((default-directory directory)
         (remote-start-p (pi-coding-agent--remote-prefix directory))
         (command (pi-coding-agent--pi-command))
         (process-command (if remote-start-p
                              (pi-coding-agent--remote-start-command command)
                            command))
         (stderr-buf (generate-new-buffer " *pi-coding-agent-stderr*")))
    (condition-case err
        (let ((proc (make-process
                     :name "pi"
                     :command process-command
                     :connection-type 'pipe
                     :noquery t
                     :file-handler t
                     :stderr stderr-buf
                     :filter #'pi-coding-agent--process-filter
                     :sentinel #'pi-coding-agent--process-sentinel)))
          (when (and remote-start-p
                     (not (process-get proc 'pi-coding-agent-ready)))
            (process-put proc 'pi-coding-agent-awaiting-ready-marker t))
          (process-put proc 'pi-coding-agent-stderr-buf stderr-buf)
          (when-let* ((stderr-proc (get-buffer-process stderr-buf)))
            (set-process-query-on-exit-flag stderr-proc nil))
          proc)
      (error
       (when (buffer-live-p stderr-buf)
         (kill-buffer stderr-buf))
       (signal (car err) (cdr err))))))

;;;; State Management

(defvar-local pi-coding-agent--status 'idle
  "Current status of the pi session (buffer-local in chat buffer).
One of: `idle', `sending', `streaming', `compacting'.
This is the single source of truth for session activity state.

Runtime status transitions are driven by events from pi:
- `idle' or `sending' -> `streaming' on agent_start
- `streaming' -> `sending' on agent_end with willRetry
- `streaming' -> `idle' on agent_end without retry
- `idle' -> `compacting' on compaction_start
- `compacting' -> `sending' on successful compaction_end with willRetry
- `compacting' -> `sending' on successful compaction_end that resumes
  prompt preflight
- `compacting' -> `idle' on compaction_end without retry, failure, or abort

Local commands may mark a session busy before the first event arrives,
for example normal prompt submission and manual compaction during the
RPC pre-event window.")

(defvar-local pi-coding-agent--pre-compaction-status nil
  "Status that was active before a compaction event sequence.
Used to restore local prompt submission state when Pi compacts during prompt
preflight before the agent turn has started.")

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
- Session status is `idle'
- Timestamp is older than `pi-coding-agent--state-verify-interval' seconds."
  (and pi-coding-agent--state
       pi-coding-agent--state-timestamp
       (eq pi-coding-agent--status 'idle)
       (> (- (float-time) pi-coding-agent--state-timestamp)
          pi-coding-agent--state-verify-interval)))

(defun pi-coding-agent--json-false-p (value)
  "Return t if VALUE represents JSON false.
`json-parse-string' yields `:false', while older helpers and tests may still
use `:json-false'.  Treat both as falsey JSON sentinels."
  (memq value '(:false :json-false)))

(defun pi-coding-agent--json-null-p (value)
  "Return t if VALUE represents JSON null.
`json-parse-string' decodes JSON null as the keyword :null."
  (eq value :null))

(defun pi-coding-agent--normalize-boolean (value)
  "Convert JSON boolean VALUE to Elisp boolean.
JSON true (t) stays t, and either supported false sentinel becomes nil."
  (if (pi-coding-agent--json-false-p value) nil value))

(defun pi-coding-agent--normalize-string-or-null (value)
  "Return VALUE if it's a string, nil otherwise.
Use when reading JSON fields that may be null or string.
JSON null (:null) and non-strings become nil."
  (and (stringp value) value))

(defun pi-coding-agent--compaction-result-from-event (event)
  "Return EVENT's successful compaction result, or nil when absent."
  (let ((result (plist-get event :result)))
    (unless (or (null result)
                (pi-coding-agent--json-null-p result))
      result)))

(defun pi-coding-agent--compaction-end-success-p (event)
  "Return non-nil when EVENT reports a completed, non-aborted compaction."
  (and (not (pi-coding-agent--normalize-boolean (plist-get event :aborted)))
       (not (null (pi-coding-agent--compaction-result-from-event event)))))

(defun pi-coding-agent--compaction-end-will-retry-p (event)
  "Return non-nil when EVENT indicates Pi will retry after compaction.
A retry is only considered pending for a successful compaction result;
failed or aborted compactions must not leave the session busy."
  (and (pi-coding-agent--normalize-boolean (plist-get event :willRetry))
       (pi-coding-agent--compaction-end-success-p event)))

(defun pi-coding-agent--compaction-end-resumes-preflight-p (event)
  "Return non-nil when EVENT can resume a pre-compaction prompt."
  (and (eq pi-coding-agent--pre-compaction-status 'sending)
       (pi-coding-agent--compaction-end-success-p event)))

(defun pi-coding-agent--update-state-from-event (event)
  "Update status and state based on EVENT.
Handles agent lifecycle, message events, compaction, and error/retry events."
  (let ((type (plist-get event :type)))
    (pcase type
      ("agent_start"
       (setq pi-coding-agent--status 'streaming)
       (plist-put pi-coding-agent--state :is-retrying nil)
       (plist-put pi-coding-agent--state :last-error nil)
       (setq pi-coding-agent--state-timestamp (float-time)))
      ("agent_end"
       (setq pi-coding-agent--status
             (if (pi-coding-agent--normalize-boolean (plist-get event :willRetry))
                 'sending
               'idle))
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
      ("compaction_start"
       (setq pi-coding-agent--pre-compaction-status
             (unless (eq pi-coding-agent--status 'compacting)
               pi-coding-agent--status))
       (setq pi-coding-agent--status 'compacting)
       (setq pi-coding-agent--state-timestamp (float-time)))
      ("compaction_end"
       (setq pi-coding-agent--status
             (cond
              ((pi-coding-agent--compaction-end-will-retry-p event)
               'sending)
              ((pi-coding-agent--compaction-end-resumes-preflight-p event)
               'sending)
              (t
               'idle)))
       (setq pi-coding-agent--pre-compaction-status nil)
       (setq pi-coding-agent--state-timestamp (float-time)))
      ("auto_retry_start"
       (setq pi-coding-agent--status 'sending)
       (plist-put pi-coding-agent--state :is-retrying t)
       (plist-put pi-coding-agent--state :retry-attempt (plist-get event :attempt))
       (plist-put pi-coding-agent--state :last-error (plist-get event :errorMessage)))
      ("auto_retry_end"
       (plist-put pi-coding-agent--state :is-retrying nil)
       (unless (eq (plist-get event :success) t)
         (when (eq pi-coding-agent--status 'sending)
           (setq pi-coding-agent--status 'idle))
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

(defun pi-coding-agent--extract-state-from-response (response &optional anchor)
  "Extract state plist from a get_state RESPONSE.
Converts camelCase keys to kebab-case, normalizes booleans, and converts
inbound sessionFile values to Emacs paths using ANCHOR or `default-directory'.
Returns plist with :status key for setting `pi-coding-agent--status'."
  (when-let* ((data (plist-get response :data)))
    (let ((is-streaming (pi-coding-agent--normalize-boolean (plist-get data :isStreaming)))
          (is-compacting (pi-coding-agent--normalize-boolean (plist-get data :isCompacting)))
          (session-file (pi-coding-agent--normalize-string-or-null
                         (plist-get data :sessionFile))))
      (list :status (cond (is-streaming 'streaming)
                          (is-compacting 'compacting)
                          (t 'idle))
            :model (plist-get data :model)
            :thinking-level (plist-get data :thinkingLevel)
            :session-id (plist-get data :sessionId)
            :session-file (pi-coding-agent--passive-emacs-path session-file anchor)
            :message-count (plist-get data :messageCount)
            :pending-message-count (plist-get data :pendingMessageCount)))))

(provide 'pi-coding-agent-core)
;;; pi-coding-agent-core.el ends here
