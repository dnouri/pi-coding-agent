;;; pi-coding-agent-gui-test-utils.el --- Utilities for pi-coding-agent GUI tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Shared utilities for deterministic GUI tests.
;;
;; Usage:
;;   (require 'pi-coding-agent-gui-test-utils)
;;   (pi-coding-agent-gui-test-with-fresh-session
;;     (:backend fake :fake-scenario "prompt-lifecycle")
;;     (pi-coding-agent-gui-test-send "Hello")
;;     (should (pi-coding-agent-gui-test-chat-contains "Fake reply for: Hello")))
;;
;; Session-entry helpers require a literal plist as the first form, including
;; an explicit `:backend'.  Once a session is active, inner helper calls may
;; reuse its options.
;; New GUI regressions should prefer fresh fake-backed sessions unless a shared
;; session is deliberately needed and justified.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'pi-coding-agent)
(require 'pi-coding-agent-test-common)
(require 'seq)

;; Disable "Buffer has running process" prompts in tests
(remove-hook 'kill-buffer-query-functions #'process-kill-buffer-query-function)

;;;; Configuration

(defvar pi-coding-agent-gui-test-model '(:provider "ollama" :modelId "qwen3:1.7b")
  "Frontend model state pushed at GUI-session startup on every backend.")

(defconst pi-coding-agent-gui-test-default-fake-scenario "prompt-lifecycle"
  "Default fake-pi scenario when a fake backend is chosen explicitly.")

;;;; Session State

(defvar pi-coding-agent-gui-test--session nil
  "Current test session plist with :chat-buffer, :input-buffer, :process.")

(defun pi-coding-agent-gui-test-session-active-p ()
  "Return t if a test session is active and healthy."
  (and pi-coding-agent-gui-test--session
       (buffer-live-p (plist-get pi-coding-agent-gui-test--session :chat-buffer))
       (process-live-p (plist-get pi-coding-agent-gui-test--session :process))))

;;;; Session Management

(defun pi-coding-agent-gui-test--normalize-backend (backend)
  "Return BACKEND normalized to either `real' or `fake'."
  (pcase backend
    ('real 'real)
    ('fake 'fake)
    (_ (error "GUI test sessions require explicit :backend, got: %S" backend))))

(defun pi-coding-agent-gui-test--backend-spec (backend &optional fake-scenario fake-extra-args)
  "Return backend plist for BACKEND.
FAKE-SCENARIO and FAKE-EXTRA-ARGS apply only to the fake backend."
  (pi-coding-agent-test-backend-spec
   (pi-coding-agent-gui-test--normalize-backend backend)
   pi-coding-agent-gui-test-default-fake-scenario
   fake-scenario
   fake-extra-args))

(defun pi-coding-agent-gui-test--normalize-session-options (options)
  "Return normalized GUI session OPTIONS plist.
OPTIONS must include an explicit `:backend'.  Fake sessions may omit
`:fake-scenario', which defaults to
`pi-coding-agent-gui-test-default-fake-scenario'."
  (let ((backend (pi-coding-agent-gui-test--normalize-backend
                  (plist-get options :backend))))
    (list :backend backend
          :fake-scenario (or (plist-get options :fake-scenario)
                             pi-coding-agent-gui-test-default-fake-scenario)
          :fake-extra-args (plist-get options :fake-extra-args))))

(defun pi-coding-agent-gui-test--current-session-options ()
  "Return the current session options.
Signal an error when no session is active, so test entry points must declare
an explicit backend instead of relying on a hidden default."
  (if (pi-coding-agent-gui-test-session-active-p)
      (plist-get pi-coding-agent-gui-test--session :options)
    (error "No active GUI test session; pass explicit options with :backend")))

(defun pi-coding-agent-gui-test--session-matches-p (options)
  "Return non-nil when current session already matches OPTIONS."
  (and (pi-coding-agent-gui-test-session-active-p)
       (equal (plist-get (plist-get pi-coding-agent-gui-test--session :options) :backend)
              (plist-get options :backend))
       (equal (plist-get (plist-get pi-coding-agent-gui-test--session :options) :fake-scenario)
              (plist-get options :fake-scenario))
       (equal (plist-get (plist-get pi-coding-agent-gui-test--session :options) :fake-extra-args)
              (plist-get options :fake-extra-args))))

(defun pi-coding-agent-gui-test--instrument-display-handler (proc)
  "Wrap PROC display handler with GUI-test event counters."
  (unless (process-get proc 'pi-coding-agent-gui-test-instrumented)
    (let ((handler (process-get proc 'pi-coding-agent-display-handler)))
      (process-put proc 'pi-coding-agent-gui-test-event-count 0)
      (process-put proc 'pi-coding-agent-gui-test-last-event nil)
      (process-put proc 'pi-coding-agent-gui-test-instrumented t)
      (process-put
       proc 'pi-coding-agent-display-handler
       (lambda (event)
         (process-put proc 'pi-coding-agent-gui-test-event-count
                      (1+ (or (process-get proc 'pi-coding-agent-gui-test-event-count) 0)))
         (process-put proc 'pi-coding-agent-gui-test-last-event event)
         (when handler
           (funcall handler event)))))))

(defun pi-coding-agent-gui-test-start-session (&optional dir options)
  "Start a new pi session in DIR with OPTIONS.
DIR defaults to /tmp.  OPTIONS must include an explicit `:backend' and may
also set `:fake-scenario' and `:fake-extra-args'.  Returns the session
plist."
  (let* ((options (pi-coding-agent-gui-test--normalize-session-options options))
         (backend (pi-coding-agent-gui-test--backend-spec
                   (plist-get options :backend)
                   (plist-get options :fake-scenario)
                   (plist-get options :fake-extra-args)))
         (default-directory (or dir "/tmp/"))
         (pi-coding-agent-executable (plist-get backend :executable))
         (pi-coding-agent-extra-args (plist-get backend :extra-args)))
    (delete-other-windows)
    (pi-coding-agent)
    (let* ((chat-buffer-name (format "*pi-coding-agent-chat:%s*" default-directory)))
      (should
       (pi-coding-agent-test-wait-until
        (lambda ()
          (let* ((chat-buf (get-buffer chat-buffer-name))
                 (input-buf (and chat-buf
                                 (with-current-buffer chat-buf
                                   pi-coding-agent--input-buffer)))
                 (proc (and chat-buf
                            (with-current-buffer chat-buf
                              pi-coding-agent--process))))
            (and (buffer-live-p chat-buf)
                 (buffer-live-p input-buf)
                 (process-live-p proc))))
        pi-coding-agent-test-gui-timeout
        pi-coding-agent-test-poll-interval))
      (let* ((chat-buf (get-buffer chat-buffer-name))
             (input-buf (and chat-buf
                             (with-current-buffer chat-buf
                               pi-coding-agent--input-buffer)))
             (proc (and chat-buf
                        (with-current-buffer chat-buf
                          pi-coding-agent--process))))
        (when (and chat-buf proc)
          (pi-coding-agent-gui-test--instrument-display-handler proc)
          ;; Keep GUI sessions on the normal frontend initialization path.
          (with-current-buffer chat-buf
            (pi-coding-agent--rpc-sync
             proc
             `(:type "set_model"
               :provider ,(plist-get pi-coding-agent-gui-test-model :provider)
               :modelId ,(plist-get pi-coding-agent-gui-test-model :modelId)))
            (pi-coding-agent--rpc-sync proc '(:type "set_thinking_level" :level "off")))
          (setq pi-coding-agent-gui-test--session
                (list :chat-buffer chat-buf
                      :input-buffer input-buf
                      :process proc
                      :directory default-directory
                      :options options
                      :backend backend)))))))

(defun pi-coding-agent-gui-test-end-session ()
  "End the current test session."
  (when pi-coding-agent-gui-test--session
    (let ((chat-buf (plist-get pi-coding-agent-gui-test--session :chat-buffer)))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))
    (setq pi-coding-agent-gui-test--session nil)))

(defun pi-coding-agent-gui-test-ensure-session (&optional options)
  "Ensure a test session matching OPTIONS is active.
When OPTIONS is nil, preserve the current session options if one is active.
Otherwise signal an error so the test entry point must declare an explicit
backend.  Also ensures proper window layout."
  (let ((options (if options
                     (pi-coding-agent-gui-test--normalize-session-options options)
                   (pi-coding-agent-gui-test--current-session-options))))
    (unless (pi-coding-agent-gui-test--session-matches-p options)
      (pi-coding-agent-gui-test-end-session)
      (pi-coding-agent-gui-test-start-session nil options))
    (pi-coding-agent-gui-test-ensure-layout)))

(defun pi-coding-agent-gui-test-ensure-layout ()
  "Ensure chat window is visible with proper layout."
  (when pi-coding-agent-gui-test--session
    (let ((chat-buf (plist-get pi-coding-agent-gui-test--session :chat-buffer))
          (input-buf (plist-get pi-coding-agent-gui-test--session :input-buffer)))
      (unless (get-buffer-window chat-buf)
        (delete-other-windows)
        (switch-to-buffer chat-buf)
        (when input-buf
          (let ((input-win (split-window nil -10 'below)))
            (set-window-buffer input-win input-buf)))))))

(defun pi-coding-agent-gui-test--macro-session-forms (macro-name forms)
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

(defmacro pi-coding-agent-gui-test-with-session (&rest forms)
  "Execute FORMS with an active pi session.
FORMS must start with a literal session options plist containing an explicit
`:backend'."
  (declare (indent 0) (debug t))
  (pcase-let* ((`(,options . ,body)
                (pi-coding-agent-gui-test--macro-session-forms
                 'pi-coding-agent-gui-test-with-session forms)))
    `(progn
       (pi-coding-agent-gui-test-ensure-session ',options)
       (ert-info ((format "backend: %s"
                          (plist-get (plist-get pi-coding-agent-gui-test--session :backend)
                                     :label)))
         ,@body))))

(defmacro pi-coding-agent-gui-test-with-fresh-session (&rest forms)
  "Execute FORMS with a fresh pi session.
FORMS must start with a literal session options plist containing an explicit
`:backend'."
  (declare (indent 0) (debug t))
  (pcase-let* ((`(,options . ,body)
                (pi-coding-agent-gui-test--macro-session-forms
                 'pi-coding-agent-gui-test-with-fresh-session forms)))
    `(progn
       (pi-coding-agent-gui-test-end-session)
       (pi-coding-agent-gui-test-start-session nil ',options)
       (unwind-protect
           (ert-info ((format "backend: %s"
                              (plist-get (plist-get pi-coding-agent-gui-test--session :backend)
                                         :label)))
             (progn ,@body))
         (pi-coding-agent-gui-test-end-session)))))

;;;; Waiting

(defun pi-coding-agent-gui-test-streaming-p ()
  "Return t if status is `streaming'."
  (when-let ((chat-buf (plist-get pi-coding-agent-gui-test--session :chat-buffer)))
    (with-current-buffer chat-buf
      (eq pi-coding-agent--status 'streaming))))

(defun pi-coding-agent-gui-test-wait-for-idle (&optional timeout)
  "Wait until streaming stops, up to TIMEOUT seconds."
  (let ((timeout (or timeout pi-coding-agent-test-gui-timeout))
        (proc (plist-get pi-coding-agent-gui-test--session :process)))
    (let ((done (pi-coding-agent-test-wait-until
                 (lambda () (not (pi-coding-agent-gui-test-streaming-p)))
                 timeout
                 pi-coding-agent-test-poll-interval
                 proc)))
      (when done
        (redisplay))
      done)))

(defun pi-coding-agent-gui-test-wait-for-chat-settled (&optional timeout)
  "Wait until the chat buffer stops changing.
Returns non-nil if the buffer is stable before TIMEOUT."
  (let* ((timeout (or timeout pi-coding-agent-test-rpc-timeout))
         (proc (plist-get pi-coding-agent-gui-test--session :process))
         (chat-buf (plist-get pi-coding-agent-gui-test--session :chat-buffer)))
    (when (buffer-live-p chat-buf)
      (let ((last-tick (with-current-buffer chat-buf
                         (buffer-chars-modified-tick))))
        (pi-coding-agent-test-wait-until
         (lambda ()
           (let ((tick (with-current-buffer chat-buf
                         (buffer-chars-modified-tick))))
             (if (= tick last-tick)
                 t
               (setq last-tick tick)
               nil)))
         timeout
         pi-coding-agent-test-poll-interval
         proc)))))

(defun pi-coding-agent-gui-test-wait-for-response-start (post-send-tick event-count &optional timeout)
  "Wait until backend activity starts after a send.
POST-SEND-TICK is the chat buffer tick captured immediately after the local
send path returns.  EVENT-COUNT is the process event counter captured before
sending."
  (let ((timeout (or timeout pi-coding-agent-test-rpc-timeout))
        (proc (plist-get pi-coding-agent-gui-test--session :process))
        (chat-buf (plist-get pi-coding-agent-gui-test--session :chat-buffer)))
    (pi-coding-agent-test-wait-until
     (lambda ()
       (or (pi-coding-agent-gui-test-streaming-p)
           (> (or (process-get proc 'pi-coding-agent-gui-test-event-count) 0)
              (or event-count 0))
           (and post-send-tick
                (buffer-live-p chat-buf)
                (> (with-current-buffer chat-buf
                     (buffer-chars-modified-tick))
                   post-send-tick))))
     timeout
     pi-coding-agent-test-poll-interval
     proc)))

;;;; Sending Messages

(defun pi-coding-agent-gui-test-send (text &optional no-wait)
  "Send TEXT to pi. Waits for response unless NO-WAIT is t."
  (pi-coding-agent-gui-test-ensure-session)
  (let* ((proc (plist-get pi-coding-agent-gui-test--session :process))
         (input-buf (plist-get pi-coding-agent-gui-test--session :input-buffer))
         (chat-buf (plist-get pi-coding-agent-gui-test--session :chat-buffer))
         (event-count (or (process-get proc 'pi-coding-agent-gui-test-event-count) 0))
         post-send-tick)
    (when input-buf
      (with-current-buffer input-buf
        (erase-buffer)
        (insert text)
        (pi-coding-agent-send)))
    (setq post-send-tick
          (and (buffer-live-p chat-buf)
               (with-current-buffer chat-buf
                 (buffer-chars-modified-tick))))
    (unless no-wait
      (should (pi-coding-agent-gui-test-wait-for-response-start
               post-send-tick event-count))
      (should (pi-coding-agent-gui-test-wait-for-idle))
      (should (pi-coding-agent-gui-test-wait-for-chat-settled))
      (redisplay))))

;;;; Window & Scroll Utilities

(defun pi-coding-agent-gui-test-chat-window ()
  "Get the chat window."
  (when-let ((buf (plist-get pi-coding-agent-gui-test--session :chat-buffer)))
    (get-buffer-window buf)))

(defun pi-coding-agent-gui-test-input-window ()
  "Get the input window."
  (when-let ((buf (plist-get pi-coding-agent-gui-test--session :input-buffer)))
    (get-buffer-window buf)))

(defun pi-coding-agent-gui-test-top-line-number ()
  "Get the line number at the top of the chat window.
This is stricter than window-start for detecting scroll drift."
  (when-let ((win (pi-coding-agent-gui-test-chat-window))
             (buf (plist-get pi-coding-agent-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (window-start win))
        (line-number-at-pos)))))

(defun pi-coding-agent-gui-test-at-end-p ()
  "Return t if chat window is scrolled to end."
  (when-let ((win (pi-coding-agent-gui-test-chat-window))
             (buf (plist-get pi-coding-agent-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (>= (window-end win t) (1- (point-max))))))

(defun pi-coding-agent-gui-test-window-point-at-end-p ()
  "Return t if chat window's point is at buffer end (following).
This checks window-point, not window-end.  Window-point being at end
is what determines if the window will auto-scroll during streaming."
  (when-let ((win (pi-coding-agent-gui-test-chat-window))
             (buf (plist-get pi-coding-agent-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (>= (window-point win) (1- (point-max))))))

(defun pi-coding-agent-gui-test-scroll-up (lines)
  "Scroll chat window up LINES lines (away from end)."
  (when-let ((win (pi-coding-agent-gui-test-chat-window))
             (buf (plist-get pi-coding-agent-gui-test--session :chat-buffer)))
    (with-selected-window win
      (with-current-buffer buf
        (goto-char (point-max))
        (scroll-down lines)
        (redisplay)))))

;;;; Buffer Content Utilities

(defun pi-coding-agent-gui-test-chat-content ()
  "Get chat buffer content as string."
  (when-let ((buf (plist-get pi-coding-agent-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun pi-coding-agent-gui-test-chat-contains (text)
  "Return t if chat buffer contains TEXT."
  (when-let ((content (pi-coding-agent-gui-test-chat-content)))
    (string-match-p (regexp-quote text) content)))

(defun pi-coding-agent-gui-test-chat-text-in-tool-block-p (text)
  "Return t if TEXT appears inside a tool block overlay."
  (when-let ((buf (plist-get pi-coding-agent-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let ((found nil))
          (while (and (not found) (search-forward text nil t))
            (let ((pos (match-beginning 0)))
              (setq found
                    (seq-some (lambda (ov) (overlay-get ov 'pi-coding-agent-tool-block))
                              (overlays-at pos)))))
          found)))))

(defun pi-coding-agent-gui-test-chat-lines ()
  "Get number of lines in chat buffer."
  (when-let ((buf (plist-get pi-coding-agent-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (count-lines (point-min) (point-max)))))

;;;; Layout Verification

(defun pi-coding-agent-gui-test-verify-layout ()
  "Verify window layout: chat on top, input on bottom.
Signals error if layout is wrong."
  (let ((chat-win (pi-coding-agent-gui-test-chat-window))
        (input-win (pi-coding-agent-gui-test-input-window)))
    (unless chat-win (error "Chat window not found"))
    (unless input-win (error "Input window not found"))
    (let ((chat-top (nth 1 (window-edges chat-win)))
          (input-top (nth 1 (window-edges input-win))))
      (unless (< chat-top input-top)
        (error "Layout wrong: chat-top=%s input-top=%s" chat-top input-top)))
    t))

;;;; Content Generation

(defun pi-coding-agent-gui-test-ensure-scrollable ()
  "Ensure chat has enough content to test scrolling.
Inserts dummy content directly for speed, without backend traffic."
  (pi-coding-agent-gui-test-ensure-session)
  (let* ((win (pi-coding-agent-gui-test-chat-window))
         (buf (plist-get pi-coding-agent-gui-test--session :chat-buffer))
         (win-height (and win (window-body-height win)))
         (target-lines (and win-height (* 3 win-height))))
    (when (and buf win target-lines
               (< (pi-coding-agent-gui-test-chat-lines) target-lines))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          ;; Insert dummy content to make buffer scrollable
          (dotimes (i (- target-lines (pi-coding-agent-gui-test-chat-lines)))
            (insert (format "Dummy line %d for scroll testing.\n" (1+ i))))
          (set-window-point win (point-max))))
      (redisplay))
    t))

(provide 'pi-coding-agent-gui-test-utils)
;;; pi-coding-agent-gui-test-utils.el ends here
