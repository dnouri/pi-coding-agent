;;; piem-gui-test-utils.el --- Utilities for piem GUI tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Shared utilities for deterministic GUI tests.
;;
;; Usage:
;;   (require 'piem-gui-test-utils)
;;   (piem-gui-test-with-fresh-session
;;     (:backend fake :fake-scenario "prompt-lifecycle")
;;     (piem-gui-test-send "Hello")
;;     (should (piem-gui-test-chat-contains "Fake reply for: Hello")))
;;
;; Session-entry helpers require a literal plist as the first form, including
;; an explicit `:backend'.  Once a session is active, inner helper calls may
;; reuse its options.
;; New GUI regressions should prefer fresh fake-backed sessions unless a shared
;; session is deliberately needed and justified.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'piem)
(require 'piem-test-common)
(require 'seq)

;; Disable "Buffer has running process" prompts in tests
(remove-hook 'kill-buffer-query-functions #'process-kill-buffer-query-function)

;;;; Configuration

(defvar piem-gui-test-model '(:provider "ollama" :modelId "qwen3:1.7b")
  "Frontend model state pushed at GUI-session startup on every backend.")

(defconst piem-gui-test-default-fake-scenario "prompt-lifecycle"
  "Default fake-pi scenario when a fake backend is chosen explicitly.")

;;;; Session State

(defvar piem-gui-test--session nil
  "Current test session plist with :chat-buffer, :input-buffer, :process.")

(defun piem-gui-test-session-active-p ()
  "Return t if a test session is active and healthy."
  (and piem-gui-test--session
       (buffer-live-p (plist-get piem-gui-test--session :chat-buffer))
       (process-live-p (plist-get piem-gui-test--session :process))))

;;;; Session Management

(defun piem-gui-test--normalize-backend (backend)
  "Return BACKEND normalized to either `real' or `fake'."
  (pcase backend
    ('real 'real)
    ('fake 'fake)
    (_ (error "GUI test sessions require explicit :backend, got: %S" backend))))

(defun piem-gui-test--backend-spec (backend &optional fake-scenario fake-extra-args)
  "Return backend plist for BACKEND.
FAKE-SCENARIO and FAKE-EXTRA-ARGS apply only to the fake backend."
  (piem-test-backend-spec
   (piem-gui-test--normalize-backend backend)
   piem-gui-test-default-fake-scenario
   fake-scenario
   fake-extra-args))

(defun piem-gui-test--normalize-session-options (options)
  "Return normalized GUI session OPTIONS plist.
OPTIONS must include an explicit `:backend'.  Fake sessions may omit
`:fake-scenario', which defaults to
`piem-gui-test-default-fake-scenario'."
  (let ((backend (piem-gui-test--normalize-backend
                  (plist-get options :backend))))
    (list :backend backend
          :fake-scenario (or (plist-get options :fake-scenario)
                             piem-gui-test-default-fake-scenario)
          :fake-extra-args (plist-get options :fake-extra-args))))

(defun piem-gui-test--current-session-options ()
  "Return the current session options.
Signal an error when no session is active, so test entry points must declare
an explicit backend instead of relying on a hidden default."
  (if (piem-gui-test-session-active-p)
      (plist-get piem-gui-test--session :options)
    (error "No active GUI test session; pass explicit options with :backend")))

(defun piem-gui-test--session-matches-p (options)
  "Return non-nil when current session already matches OPTIONS."
  (and (piem-gui-test-session-active-p)
       (equal (plist-get (plist-get piem-gui-test--session :options) :backend)
              (plist-get options :backend))
       (equal (plist-get (plist-get piem-gui-test--session :options) :fake-scenario)
              (plist-get options :fake-scenario))
       (equal (plist-get (plist-get piem-gui-test--session :options) :fake-extra-args)
              (plist-get options :fake-extra-args))))

(defun piem-gui-test--instrument-display-handler (proc)
  "Wrap PROC display handler with GUI-test event counters."
  (unless (process-get proc 'piem-gui-test-instrumented)
    (let ((handler (process-get proc 'piem-display-handler)))
      (process-put proc 'piem-gui-test-event-count 0)
      (process-put proc 'piem-gui-test-last-event nil)
      (process-put proc 'piem-gui-test-instrumented t)
      (process-put
       proc 'piem-display-handler
       (lambda (event)
         (process-put proc 'piem-gui-test-event-count
                      (1+ (or (process-get proc 'piem-gui-test-event-count) 0)))
         (process-put proc 'piem-gui-test-last-event event)
         (when handler
           (funcall handler event)))))))

(defun piem-gui-test-start-session (&optional dir options)
  "Start a new pi session in DIR with OPTIONS.
DIR defaults to /tmp.  OPTIONS must include an explicit `:backend' and may
also set `:fake-scenario' and `:fake-extra-args'.  Returns the session
plist."
  (let* ((options (piem-gui-test--normalize-session-options options))
         (backend (piem-gui-test--backend-spec
                   (plist-get options :backend)
                   (plist-get options :fake-scenario)
                   (plist-get options :fake-extra-args)))
         (default-directory (or dir "/tmp/"))
         (piem-executable (plist-get backend :executable))
         (piem-extra-args (plist-get backend :extra-args)))
    (delete-other-windows)
    (piem)
    (let* ((chat-buffer-name (format "*piem-chat:%s*" default-directory)))
      (should
       (piem-test-wait-until
        (lambda ()
          (let* ((chat-buf (get-buffer chat-buffer-name))
                 (input-buf (and chat-buf
                                 (with-current-buffer chat-buf
                                   piem--input-buffer)))
                 (proc (and chat-buf
                            (with-current-buffer chat-buf
                              piem--process))))
            (and (buffer-live-p chat-buf)
                 (buffer-live-p input-buf)
                 (process-live-p proc))))
        piem-test-gui-timeout
        piem-test-poll-interval))
      (let* ((chat-buf (get-buffer chat-buffer-name))
             (input-buf (and chat-buf
                             (with-current-buffer chat-buf
                               piem--input-buffer)))
             (proc (and chat-buf
                        (with-current-buffer chat-buf
                          piem--process))))
        (when (and chat-buf proc)
          (piem-gui-test--instrument-display-handler proc)
          ;; Keep GUI sessions on the normal frontend initialization path.
          (with-current-buffer chat-buf
            (piem--rpc-sync
             proc
             `(:type "set_model"
               :provider ,(plist-get piem-gui-test-model :provider)
               :modelId ,(plist-get piem-gui-test-model :modelId)))
            (piem--rpc-sync proc '(:type "set_thinking_level" :level "off")))
          (setq piem-gui-test--session
                (list :chat-buffer chat-buf
                      :input-buffer input-buf
                      :process proc
                      :directory default-directory
                      :options options
                      :backend backend)))))))

(defun piem-gui-test-end-session ()
  "End the current test session."
  (when piem-gui-test--session
    (let ((chat-buf (plist-get piem-gui-test--session :chat-buffer))
          (piem-quit-without-confirmation t))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))
    (setq piem-gui-test--session nil)))

(defun piem-gui-test-ensure-session (&optional options)
  "Ensure a test session matching OPTIONS is active.
When OPTIONS is nil, preserve the current session options if one is active.
Otherwise signal an error so the test entry point must declare an explicit
backend.  Also ensures proper window layout."
  (let ((options (if options
                     (piem-gui-test--normalize-session-options options)
                   (piem-gui-test--current-session-options))))
    (unless (piem-gui-test--session-matches-p options)
      (piem-gui-test-end-session)
      (piem-gui-test-start-session nil options))
    (piem-gui-test-ensure-layout)))

(defun piem-gui-test-ensure-layout ()
  "Ensure chat window is visible with proper layout."
  (when piem-gui-test--session
    (let ((chat-buf (plist-get piem-gui-test--session :chat-buffer))
          (input-buf (plist-get piem-gui-test--session :input-buffer)))
      (unless (get-buffer-window chat-buf)
        (delete-other-windows)
        (switch-to-buffer chat-buf)
        (when input-buf
          (let ((input-win (split-window nil -10 'below)))
            (set-window-buffer input-win input-buf)))))))

(defun piem-gui-test--macro-session-forms (macro-name forms)
  "Return (OPTIONS . BODY) from FORMS for MACRO-NAME.
Signal an error unless FORMS starts with a literal plist containing
an explicit `:backend'."
  (let ((options (car forms)))
    (unless (and (listp options)
                 (keywordp (car options))
                 (plist-member options :backend))
      (error "%s requires an explicit session options plist with :backend"
             macro-name))
    (cons options (cdr forms))))

;;;; Macros for Test Structure

(defmacro piem-gui-test-with-session (&rest forms)
  "Execute FORMS with an active pi session.
FORMS must start with a literal session options plist containing an explicit
`:backend'."
  (declare (indent 0) (debug t))
  (pcase-let* ((`(,options . ,body)
                (piem-gui-test--macro-session-forms
                 'piem-gui-test-with-session forms)))
    `(progn
       (piem-gui-test-ensure-session ',options)
       (ert-info ((format "backend: %s"
                          (plist-get (plist-get piem-gui-test--session :backend)
                                     :label)))
         ,@body))))

(defmacro piem-gui-test-with-fresh-session (&rest forms)
  "Execute FORMS with a fresh pi session.
FORMS must start with a literal session options plist containing an explicit
`:backend'."
  (declare (indent 0) (debug t))
  (pcase-let* ((`(,options . ,body)
                (piem-gui-test--macro-session-forms
                 'piem-gui-test-with-fresh-session forms)))
    `(progn
       (piem-gui-test-end-session)
       (piem-gui-test-start-session nil ',options)
       (unwind-protect
           (ert-info ((format "backend: %s"
                              (plist-get (plist-get piem-gui-test--session :backend)
                                         :label)))
             (progn ,@body))
         (piem-gui-test-end-session)))))

;;;; Waiting

(defun piem-gui-test-streaming-p ()
  "Return t if status is `streaming'."
  (when-let ((chat-buf (plist-get piem-gui-test--session :chat-buffer)))
    (with-current-buffer chat-buf
      (eq piem--status 'streaming))))

(defun piem-gui-test-wait-for-idle (&optional timeout)
  "Wait until streaming stops, up to TIMEOUT seconds."
  (let ((timeout (or timeout piem-test-gui-timeout))
        (proc (plist-get piem-gui-test--session :process)))
    (let ((done (piem-test-wait-until
                 (lambda () (not (piem-gui-test-streaming-p)))
                 timeout
                 piem-test-poll-interval
                 proc)))
      (when done
        (redisplay))
      done)))

(defun piem-gui-test-wait-for-chat-settled (&optional timeout)
  "Wait until the chat buffer stops changing.
Returns non-nil if the buffer is stable before TIMEOUT."
  (let* ((timeout (or timeout piem-test-rpc-timeout))
         (proc (plist-get piem-gui-test--session :process))
         (chat-buf (plist-get piem-gui-test--session :chat-buffer)))
    (when (buffer-live-p chat-buf)
      (let ((last-tick (with-current-buffer chat-buf
                         (buffer-chars-modified-tick))))
        (piem-test-wait-until
         (lambda ()
           (let ((tick (with-current-buffer chat-buf
                         (buffer-chars-modified-tick))))
             (if (= tick last-tick)
                 t
               (setq last-tick tick)
               nil)))
         timeout
         piem-test-poll-interval
         proc)))))

(defun piem-gui-test-wait-for-response-start (post-send-tick event-count &optional timeout)
  "Wait until backend activity starts after a send.
POST-SEND-TICK is the chat buffer tick captured immediately after the local
send path returns.  EVENT-COUNT is the process event counter captured before
sending."
  (let ((timeout (or timeout piem-test-rpc-timeout))
        (proc (plist-get piem-gui-test--session :process))
        (chat-buf (plist-get piem-gui-test--session :chat-buffer)))
    (piem-test-wait-until
     (lambda ()
       (or (piem-gui-test-streaming-p)
           (> (or (process-get proc 'piem-gui-test-event-count) 0)
              (or event-count 0))
           (and post-send-tick
                (buffer-live-p chat-buf)
                (> (with-current-buffer chat-buf
                     (buffer-chars-modified-tick))
                   post-send-tick))))
     timeout
     piem-test-poll-interval
     proc)))

;;;; Sending Messages

(defun piem-gui-test-send (text &optional no-wait)
  "Send TEXT to pi. Waits for response unless NO-WAIT is t."
  (piem-gui-test-ensure-session)
  (let* ((proc (plist-get piem-gui-test--session :process))
         (input-buf (plist-get piem-gui-test--session :input-buffer))
         (chat-buf (plist-get piem-gui-test--session :chat-buffer))
         (event-count (or (process-get proc 'piem-gui-test-event-count) 0))
         post-send-tick)
    (when input-buf
      (with-current-buffer input-buf
        (erase-buffer)
        (insert text)
        (piem-send)))
    (setq post-send-tick
          (and (buffer-live-p chat-buf)
               (with-current-buffer chat-buf
                 (buffer-chars-modified-tick))))
    (unless no-wait
      (should (piem-gui-test-wait-for-response-start
               post-send-tick event-count))
      (should (piem-gui-test-wait-for-idle))
      (should (piem-gui-test-wait-for-chat-settled))
      (redisplay))))

;;;; Window & Scroll Utilities

(defun piem-gui-test-chat-window ()
  "Get the chat window."
  (when-let ((buf (plist-get piem-gui-test--session :chat-buffer)))
    (get-buffer-window buf)))

(defun piem-gui-test-input-window ()
  "Get the input window."
  (when-let ((buf (plist-get piem-gui-test--session :input-buffer)))
    (get-buffer-window buf)))

(defun piem-gui-test-top-line-number ()
  "Get the line number at the top of the chat window.
This is stricter than window-start for detecting scroll drift."
  (when-let ((win (piem-gui-test-chat-window))
             (buf (plist-get piem-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (window-start win))
        (line-number-at-pos)))))

(defun piem-gui-test-at-end-p ()
  "Return t if chat window is scrolled to end."
  (when-let ((win (piem-gui-test-chat-window))
             (buf (plist-get piem-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (>= (window-end win t) (1- (point-max))))))

(defun piem-gui-test-window-point-at-end-p ()
  "Return t if chat window's point is at buffer end (following).
This checks window-point, not window-end.  Window-point being at end
is what determines if the window will auto-scroll during streaming."
  (when-let ((win (piem-gui-test-chat-window))
             (buf (plist-get piem-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (>= (window-point win) (1- (point-max))))))

(defun piem-gui-test-scroll-up (lines)
  "Scroll chat window up LINES lines (away from end)."
  (when-let ((win (piem-gui-test-chat-window))
             (buf (plist-get piem-gui-test--session :chat-buffer)))
    (with-selected-window win
      (with-current-buffer buf
        (goto-char (point-max))
        (scroll-down lines)
        (redisplay)))))

;;;; Buffer Content Utilities

(defun piem-gui-test-chat-content ()
  "Get chat buffer content as string."
  (when-let ((buf (plist-get piem-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun piem-gui-test-chat-contains (text)
  "Return t if chat buffer contains TEXT."
  (when-let ((content (piem-gui-test-chat-content)))
    (string-match-p (regexp-quote text) content)))

(defun piem-gui-test-chat-text-in-tool-block-p (text)
  "Return t if TEXT appears inside a tool block overlay."
  (when-let ((buf (plist-get piem-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let ((found nil))
          (while (and (not found) (search-forward text nil t))
            (let ((pos (match-beginning 0)))
              (setq found
                    (seq-some (lambda (ov) (overlay-get ov 'piem-tool-block))
                              (overlays-at pos)))))
          found)))))

(defun piem-gui-test-chat-lines ()
  "Get number of lines in chat buffer."
  (when-let ((buf (plist-get piem-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (count-lines (point-min) (point-max)))))

;;;; Layout Verification

(defun piem-gui-test-verify-layout ()
  "Verify window layout: chat on top, input on bottom.
Signals error if layout is wrong."
  (let ((chat-win (piem-gui-test-chat-window))
        (input-win (piem-gui-test-input-window)))
    (unless chat-win (error "Chat window not found"))
    (unless input-win (error "Input window not found"))
    (let ((chat-top (nth 1 (window-edges chat-win)))
          (input-top (nth 1 (window-edges input-win))))
      (unless (< chat-top input-top)
        (error "Layout wrong: chat-top=%s input-top=%s" chat-top input-top)))
    t))

;;;; Content Generation

(defun piem-gui-test-ensure-scrollable ()
  "Ensure chat has enough content to test scrolling.
Inserts dummy content directly for speed, without backend traffic."
  (piem-gui-test-ensure-session)
  (let* ((win (piem-gui-test-chat-window))
         (buf (plist-get piem-gui-test--session :chat-buffer))
         (win-height (and win (window-body-height win)))
         (target-lines (and win-height (* 3 win-height))))
    (when (and buf win target-lines
               (< (piem-gui-test-chat-lines) target-lines))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          ;; Insert dummy content to make buffer scrollable
          (dotimes (i (- target-lines (piem-gui-test-chat-lines)))
            (insert (format "Dummy line %d for scroll testing.\n" (1+ i))))
          (set-window-point win (point-max))))
      (redisplay))
    t))

(provide 'piem-gui-test-utils)
;;; piem-gui-test-utils.el ends here
