;;; pi-gui-test-utils.el --- Utilities for pi.el GUI tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Shared utilities for GUI integration tests.
;;
;; Usage:
;;   (require 'pi-gui-test-utils)
;;   (pi-gui-test-with-session
;;     (pi-gui-test-send "Hello")
;;     (should (pi-gui-test-chat-contains "Hello")))
;;
;; Tests can either:
;; - Share a session (fast, for related tests)
;; - Use `pi-gui-test-with-fresh-session` for isolation

;;; Code:

(require 'ert)
(require 'pi)
(require 'pi-test-common)

;;;; Configuration

(defvar pi-gui-test-model '(:provider "ollama" :modelId "qwen3:1.7b")
  "Model to use for tests. Supports tool calling.")

;;;; Session State

(defvar pi-gui-test--session nil
  "Current test session plist with :chat-buffer, :input-buffer, :process.")

(defun pi-gui-test-session-active-p ()
  "Return t if a test session is active and healthy."
  (and pi-gui-test--session
       (buffer-live-p (plist-get pi-gui-test--session :chat-buffer))
       (process-live-p (plist-get pi-gui-test--session :process))))

;;;; Session Management

(defun pi-gui-test-start-session (&optional dir)
  "Start a new pi session in DIR (default /tmp).
Returns session plist."
  (let ((default-directory (or dir "/tmp/")))
    (delete-other-windows)
    (pi)
    (sit-for 2)  ;; sit-for allows redisplay (sleep-for blocks it)
    (let* ((chat-buf (get-buffer (format "*pi-chat:%s*" default-directory)))
           (input-buf (and chat-buf (with-current-buffer chat-buf pi--input-buffer)))
           (proc (and chat-buf (with-current-buffer chat-buf pi--process))))
      (when (and chat-buf proc)
        ;; Set model and disable thinking for faster tests
        (with-current-buffer chat-buf
          (pi--rpc-sync proc
                        `(:type "set_model"
                          :provider ,(plist-get pi-gui-test-model :provider)
                          :modelId ,(plist-get pi-gui-test-model :modelId)))
          (pi--rpc-sync proc '(:type "set_thinking_level" :level "off")))
        (sit-for 1)
        (setq pi-gui-test--session
              (list :chat-buffer chat-buf
                    :input-buffer input-buf
                    :process proc
                    :directory default-directory))))))

(defun pi-gui-test-end-session ()
  "End the current test session."
  (when pi-gui-test--session
    (let ((chat-buf (plist-get pi-gui-test--session :chat-buffer)))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))
    (setq pi-gui-test--session nil)))

(defun pi-gui-test-ensure-session ()
  "Ensure a test session is active, starting one if needed.
Also ensures proper window layout."
  (unless (pi-gui-test-session-active-p)
    (pi-gui-test-start-session))
  (pi-gui-test-ensure-layout))

(defun pi-gui-test-ensure-layout ()
  "Ensure chat window is visible with proper layout."
  (when pi-gui-test--session
    (let ((chat-buf (plist-get pi-gui-test--session :chat-buffer))
          (input-buf (plist-get pi-gui-test--session :input-buffer)))
      (unless (get-buffer-window chat-buf)
        (delete-other-windows)
        (switch-to-buffer chat-buf)
        (when input-buf
          (let ((input-win (split-window nil -10 'below)))
            (set-window-buffer input-win input-buf)))))))

;;;; Macros for Test Structure

(defmacro pi-gui-test-with-session (&rest body)
  "Execute BODY with an active pi session.
Reuses existing session if available."
  (declare (indent 0) (debug t))
  `(progn
     (pi-gui-test-ensure-session)
     ,@body))

(defmacro pi-gui-test-with-fresh-session (&rest body)
  "Execute BODY with a fresh pi session.
Ends any existing session first, starts new one, cleans up after."
  (declare (indent 0) (debug t))
  `(progn
     (pi-gui-test-end-session)
     (pi-gui-test-start-session)
     (unwind-protect
         (progn ,@body)
       (pi-gui-test-end-session))))

;;;; Waiting

(defun pi-gui-test-streaming-p ()
  "Return t if currently streaming or sending.
Checks for both 'sending (waiting for response to start) and
'streaming (receiving response) to avoid race conditions."
  (when-let ((chat-buf (plist-get pi-gui-test--session :chat-buffer)))
    (with-current-buffer chat-buf
      (memq pi--status '(sending streaming)))))

(defun pi-gui-test-wait-for-idle (&optional timeout)
  "Wait until streaming stops, up to TIMEOUT seconds."
  (let ((timeout (or timeout pi-test-gui-timeout))
        (start (float-time)))
    (while (and (pi-gui-test-streaming-p)
                (< (- (float-time) start) timeout))
      (sit-for 0.5))
    (not (pi-gui-test-streaming-p))))

;;;; Sending Messages

(defun pi-gui-test-send (text &optional no-wait)
  "Send TEXT to pi. Waits for response unless NO-WAIT is t."
  (pi-gui-test-ensure-session)
  (let ((input-buf (plist-get pi-gui-test--session :input-buffer)))
    (when input-buf
      (with-current-buffer input-buf
        (erase-buffer)
        (insert text)
        (pi-send))))
  (unless no-wait
    (sit-for 1)
    (pi-gui-test-wait-for-idle)))

(defun pi-gui-test-send-no-tools (text)
  "Send TEXT asking the AI to respond without using tools."
  (pi-gui-test-send (concat "Without using any tools: " text)))

;;;; Window & Scroll Utilities

(defun pi-gui-test-chat-window ()
  "Get the chat window."
  (when-let ((buf (plist-get pi-gui-test--session :chat-buffer)))
    (get-buffer-window buf)))

(defun pi-gui-test-input-window ()
  "Get the input window."
  (when-let ((buf (plist-get pi-gui-test--session :input-buffer)))
    (get-buffer-window buf)))

(defun pi-gui-test-window-start ()
  "Get chat window's scroll position."
  (when-let ((win (pi-gui-test-chat-window)))
    (window-start win)))

(defun pi-gui-test-top-line-number ()
  "Get the line number at the top of the chat window.
This is stricter than window-start for detecting scroll drift."
  (when-let ((win (pi-gui-test-chat-window))
             (buf (plist-get pi-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (window-start win))
        (line-number-at-pos)))))

(defun pi-gui-test-at-end-p ()
  "Return t if chat window is scrolled to end."
  (when-let ((win (pi-gui-test-chat-window))
             (buf (plist-get pi-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (>= (window-end win t) (1- (point-max))))))

(defun pi-gui-test-scroll-up (lines)
  "Scroll chat window up LINES lines (away from end)."
  (when-let ((win (pi-gui-test-chat-window))
             (buf (plist-get pi-gui-test--session :chat-buffer)))
    (with-selected-window win
      (with-current-buffer buf
        (goto-char (point-max))
        (scroll-down lines)
        (sit-for 0.1)))))

(defun pi-gui-test-scroll-to-end ()
  "Scroll chat window to end."
  (when-let ((win (pi-gui-test-chat-window)))
    (with-selected-window win
      (goto-char (point-max))
      (recenter -1))))

;;;; Buffer Content Utilities

(defun pi-gui-test-chat-content ()
  "Get chat buffer content as string."
  (when-let ((buf (plist-get pi-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun pi-gui-test-chat-contains (text)
  "Return t if chat buffer contains TEXT."
  (when-let ((content (pi-gui-test-chat-content)))
    (string-match-p (regexp-quote text) content)))

(defun pi-gui-test-chat-matches (regexp)
  "Return t if chat buffer matches REGEXP."
  (when-let ((content (pi-gui-test-chat-content)))
    (string-match-p regexp content)))

(defun pi-gui-test-chat-lines ()
  "Get number of lines in chat buffer."
  (when-let ((buf (plist-get pi-gui-test--session :chat-buffer)))
    (with-current-buffer buf
      (count-lines (point-min) (point-max)))))

(defun pi-gui-test-input-content ()
  "Get input buffer content."
  (when-let ((buf (plist-get pi-gui-test--session :input-buffer)))
    (with-current-buffer buf
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;; Layout Verification

(defun pi-gui-test-verify-layout ()
  "Verify window layout: chat on top, input on bottom.
Signals error if layout is wrong."
  (let ((chat-win (pi-gui-test-chat-window))
        (input-win (pi-gui-test-input-window)))
    (unless chat-win (error "Chat window not found"))
    (unless input-win (error "Input window not found"))
    (let ((chat-top (nth 1 (window-edges chat-win)))
          (input-top (nth 1 (window-edges input-win))))
      (unless (< chat-top input-top)
        (error "Layout wrong: chat-top=%s input-top=%s" chat-top input-top)))
    t))

;;;; Content Generation

(defun pi-gui-test-ensure-scrollable ()
  "Ensure chat has enough content to test scrolling.
Inserts dummy content directly (no LLM calls) for speed."
  (pi-gui-test-ensure-session)
  (let* ((win (pi-gui-test-chat-window))
         (buf (plist-get pi-gui-test--session :chat-buffer))
         (win-height (and win (window-body-height win)))
         (target-lines (and win-height (* 3 win-height))))
    (when (and buf win target-lines
               (< (pi-gui-test-chat-lines) target-lines))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          ;; Insert dummy content to make buffer scrollable
          (dotimes (i (- target-lines (pi-gui-test-chat-lines)))
            (insert (format "Dummy line %d for scroll testing.\n" (1+ i))))
          (set-window-point win (point-max))))
      (sit-for 0.1))
    t))

;;;; File Utilities (for tool tests)

(defun pi-gui-test-create-temp-file (name content)
  "Create temp file NAME with CONTENT, return full path."
  (let ((path (expand-file-name name "/tmp/")))
    (with-temp-file path (insert content))
    path))

(defun pi-gui-test-delete-temp-file (path)
  "Delete temp file at PATH if it exists."
  (ignore-errors (delete-file path)))

(provide 'pi-gui-test-utils)
;;; pi-gui-test-utils.el ends here
