;;; pi-coding-agent-input-test.el --- Tests for pi-coding-agent-input -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>

;;; Commentary:

;; Tests for input history, history isearch, send/abort commands,
;; message queuing, file reference completion, path completion,
;; and slash command completion — the input buffer layer.

;;; Code:

(require 'ert)
(require 'pi-coding-agent)
(require 'pi-coding-agent-test-common)

;;; Sending Prompts

(ert-deftest pi-coding-agent-test-send-extracts-text ()
  "pi-coding-agent-send extracts text from input buffer and clears it."
  (let ((sent-text nil))
    (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-send1/"
      (cl-letf (((symbol-function 'pi-coding-agent--send-prompt)
                 (lambda (text) (setq sent-text text))))
        (with-current-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-send1/*"
          (insert "Hello, pi!")
          (pi-coding-agent-send)
          (should (equal sent-text "Hello, pi!"))
          (should (string-empty-p (buffer-string))))))))

(ert-deftest pi-coding-agent-test-send-empty-is-noop ()
  "pi-coding-agent-send with empty buffer does nothing."
  (let ((send-called nil))
    (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-send2/"
      (cl-letf (((symbol-function 'pi-coding-agent--send-prompt)
                 (lambda (_) (setq send-called t))))
        (with-current-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-send2/*"
          (pi-coding-agent-send)
          (should-not send-called))))))

(ert-deftest pi-coding-agent-test-send-whitespace-only-is-noop ()
  "pi-coding-agent-send with only whitespace does nothing."
  (let ((send-called nil))
    (pi-coding-agent-test-with-mock-session "/tmp/pi-coding-agent-test-send3/"
      (cl-letf (((symbol-function 'pi-coding-agent--send-prompt)
                 (lambda (_) (setq send-called t))))
        (with-current-buffer "*pi-coding-agent-input:/tmp/pi-coding-agent-test-send3/*"
          (insert "   \n\t  ")
          (pi-coding-agent-send)
          (should-not send-called))))))

(ert-deftest pi-coding-agent-test-send-queues-locally-while-streaming ()
  "pi-coding-agent-send adds to local queue while streaming, no RPC sent."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-stream*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-stream-input*"))
        (rpc-called nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf)
            (setq pi-coding-agent--followup-queue nil))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "My message")
            (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc _cmd _cb) (setq rpc-called t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "queued" (downcase fmt)))
                           (setq message-shown t)))))
              (pi-coding-agent-send))
            ;; Should NOT have called RPC (local queue instead)
            (should-not rpc-called)
            ;; Should have added to local queue in chat buffer
            (with-current-buffer chat-buf
              (should (equal pi-coding-agent--followup-queue '("My message"))))
            ;; Should have shown queued message
            (should message-shown)
            ;; Input should be cleared (message accepted)
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-send-queues-locally-while-compacting ()
  "pi-coding-agent-send adds to local queue while compacting, no RPC sent."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-compact*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-compact-input*"))
        (rpc-called nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'compacting)
            (setq pi-coding-agent--input-buffer input-buf)
            (setq pi-coding-agent--followup-queue nil))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "My message during compaction")
            (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc _cmd _cb) (setq rpc-called t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "queued" (downcase fmt)))
                           (setq message-shown t)))))
              (pi-coding-agent-send))
            ;; Should NOT have called RPC (local queue instead)
            (should-not rpc-called)
            ;; Should have added to local queue in chat buffer
            (with-current-buffer chat-buf
              (should (equal pi-coding-agent--followup-queue '("My message during compaction"))))
            ;; Should have shown queued message
            (should message-shown)
            ;; Input should be cleared (message accepted)
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-slash-compact-handled-locally-not-sent-as-prompt ()
  "/compact in input buffer invokes pi-coding-agent-compact locally, not sent to pi."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-slash-compact*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-slash-compact-input*"))
        (compact-called nil)
        (compact-args nil)
        (prompt-sent nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "/compact")
            (cl-letf (((symbol-function 'pi-coding-agent-compact)
                       (lambda (&optional args) (setq compact-called t compact-args args)))
                      ((symbol-function 'pi-coding-agent--send-prompt)
                       (lambda (_) (setq prompt-sent t))))
              (pi-coding-agent-send))
            ;; Should have called compact function with no args
            (should compact-called)
            (should (null compact-args))
            ;; Should NOT have sent as prompt
            (should-not prompt-sent)
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-slash-compact-with-args-passes-instructions ()
  "/compact with args passes custom instructions to compact function."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-slash-compact-args*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-slash-compact-args-input*"))
        (compact-called nil)
        (compact-args nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "/compact focus on the API design decisions")
            (cl-letf (((symbol-function 'pi-coding-agent-compact)
                       (lambda (&optional args) (setq compact-called t compact-args args))))
              (pi-coding-agent-send))
            ;; Should have called compact function with custom instructions
            (should compact-called)
            (should (equal compact-args "focus on the API design decisions"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-auto-compaction-success-sends-queued-messages ()
  "auto_compaction_end with aborted=false processes followup queue.
Uses :false (JSON false representation) to verify boolean normalization."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((sent-text nil))
      (setq pi-coding-agent--status 'compacting)
      (setq pi-coding-agent--followup-queue '("queued message"))
      (cl-letf (((symbol-function 'pi-coding-agent--send-prompt)
                 (lambda (text) (setq sent-text text))))
        (pi-coding-agent--handle-display-event
         '(:type "auto_compaction_end"
           :aborted :false
           :result (:tokensBefore 1000 :summary "Summary" :timestamp 1234567890000))))
      ;; Queue should be empty after processing
      (should (null pi-coding-agent--followup-queue))
      ;; The queued message should have been sent
      (should (equal sent-text "queued message")))))

(ert-deftest pi-coding-agent-test-auto-compaction-end-aborted-clears-queue ()
  "auto_compaction_end when aborted clears followup queue without sending."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((sent-text nil))
      (setq pi-coding-agent--status 'compacting)
      (setq pi-coding-agent--followup-queue '("queued message"))
      (cl-letf (((symbol-function 'pi-coding-agent--send-prompt)
                 (lambda (text) (setq sent-text text))))
        ;; Simulate auto_compaction_end event (aborted)
        (pi-coding-agent--handle-display-event
         '(:type "auto_compaction_end"
           :aborted t)))
      ;; Queue should be cleared (user cancelled)
      (should (null pi-coding-agent--followup-queue))
      ;; No message should have been sent
      (should (null sent-text)))))

;;; Abort Command

(ert-deftest pi-coding-agent-test-abort-sends-command ()
  "pi-coding-agent-abort sends abort command via RPC."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((sent-command nil)
          (pi-coding-agent--status 'streaming))
      (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                ((symbol-function 'pi-coding-agent--get-chat-buffer) (lambda () (current-buffer)))
                ((symbol-function 'pi-coding-agent--rpc-async)
                 (lambda (_proc cmd _cb) (setq sent-command cmd))))
        (pi-coding-agent-abort)
        (should (equal (plist-get sent-command :type) "abort"))))))

(ert-deftest pi-coding-agent-test-abort-noop-when-not-streaming ()
  "pi-coding-agent-abort does nothing when not streaming."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((sent-command nil)
          (pi-coding-agent--status 'idle))
      (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                ((symbol-function 'pi-coding-agent--get-chat-buffer) (lambda () (current-buffer)))
                ((symbol-function 'pi-coding-agent--rpc-async)
                 (lambda (_proc cmd _cb) (setq sent-command cmd))))
        (pi-coding-agent-abort)
        (should (null sent-command))))))

(ert-deftest pi-coding-agent-test-abort-clears-followup-queue ()
  "Aborting clears the follow-up queue so queued messages are not sent.
When user aborts, they want to stop everything - including queued messages."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((inhibit-read-only t)
          (message-was-sent nil))
      (insert "Some streaming content")
      ;; Set up state as if we're streaming with a queued message
      (setq pi-coding-agent--aborted t
            pi-coding-agent--followup-queue '("queued message that should be discarded"))
      ;; Mock send functions to detect if queue processing sends the message
      (cl-letf (((symbol-function 'pi-coding-agent--prepare-and-send)
                 (lambda (_text) (setq message-was-sent t)))
                ((symbol-function 'pi-coding-agent--fontify-timer-stop) #'ignore)
                ((symbol-function 'pi-coding-agent--refresh-header) #'ignore))
        ;; Simulate agent_end arriving after abort
        (pi-coding-agent--display-agent-end)
        ;; Queue should be empty (either cleared or not processed)
        (should (null pi-coding-agent--followup-queue))
        ;; Key assertion: queued message should NOT have been sent
        (should-not message-was-sent)))))

;;; Kill Buffer Protection

(ert-deftest pi-coding-agent-test-handler-removed-on-kill ()
  "Event handler is removed when chat buffer is killed."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((fake-proc (start-process "test" nil "true")))
      (unwind-protect
          (progn
            (setq pi-coding-agent--process fake-proc)
            (pi-coding-agent--register-display-handler fake-proc)
            (should (process-get fake-proc 'pi-coding-agent-display-handler))
            (pi-coding-agent--cleanup-on-kill)
            (should-not (process-get fake-proc 'pi-coding-agent-display-handler)))
        (when (process-live-p fake-proc)
          (delete-process fake-proc))))))

;;; Message Queuing

(ert-deftest pi-coding-agent-test-queue-steering-when-streaming-sends-steer ()
  "Queue steering sends steer RPC command when agent is streaming."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-steer*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-steer-input*"))
        (sent-command nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Please stop and focus on X")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc cmd _cb) (setq sent-command cmd))))
              (pi-coding-agent-queue-steering))
            ;; Should send steer command
            (should sent-command)
            (should (equal (plist-get sent-command :type) "steer"))
            (should (equal (plist-get sent-command :message) "Please stop and focus on X"))
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-queue-followup-uses-local-queue ()
  "Queue follow-up adds to local queue, no RPC sent."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-followup*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-followup-input*"))
        (rpc-called nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf)
            (setq pi-coding-agent--followup-queue nil))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "After you're done, also do Y")
            (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc _cmd _cb) (setq rpc-called t))))
              (pi-coding-agent-queue-followup))
            ;; Should NOT call RPC
            (should-not rpc-called)
            ;; Should add to local queue
            (with-current-buffer chat-buf
              (should (member "After you're done, also do Y" pi-coding-agent--followup-queue)))
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-queue-steering-when-idle-refuses ()
  "Queue steering refuses when agent is idle (nothing to interrupt)."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-steer-idle*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-steer-idle-input*"))
        (sent-anything nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Do something")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt)
                       (lambda (_) (setq sent-anything t)))
                      ((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc _cmd _cb) (setq sent-anything t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "nothing\\|idle\\|C-c C-c" (downcase fmt)))
                           (setq message-shown t)))))
              (pi-coding-agent-queue-steering))
            ;; Should NOT send anything
            (should-not sent-anything)
            ;; Should show message about using C-c C-c instead
            (should message-shown)
            ;; Input should be preserved (not accepted)
            (should (equal (buffer-string) "Do something"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-queue-followup-when-idle-sends-prompt ()
  "Queue follow-up sends as normal prompt when agent is idle."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-followup-idle*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-followup-idle-input*"))
        (sent-prompt nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Do something else")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt)
                       (lambda (text) (setq sent-prompt text))))
              (pi-coding-agent-queue-followup))
            ;; Should send as normal prompt
            (should (equal sent-prompt "Do something else"))
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-queue-steering-adds-to-history ()
  "Queue steering adds input to history."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-hist*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-hist-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (setq pi-coding-agent--input-ring (make-ring 10))
            (insert "History test message")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--rpc-async) #'ignore))
              (pi-coding-agent-queue-steering))
            ;; Should be in history
            (should (not (ring-empty-p pi-coding-agent--input-ring)))
            (should (equal (ring-ref pi-coding-agent--input-ring 0) "History test message"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-queue-empty-input-does-nothing ()
  "Queue with empty input does nothing."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-empty*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-empty-input*"))
        (command-sent nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            ;; Empty input (just whitespace)
            (insert "   \n  ")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc _cmd _cb) (setq command-sent t))))
              (pi-coding-agent-queue-steering))
            ;; Should not send anything
            (should-not command-sent)))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-steering-shows-minibuffer-message ()
  "Steering shows feedback in minibuffer but is NOT displayed locally.
Unlike normal sends, steering waits for pi's echo to display at the
correct position in the conversation."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-display*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-display-input*"))
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "My steering message")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--rpc-async) #'ignore)
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "steering\\|sent" (downcase fmt)))
                           (setq message-shown t)))))
              (pi-coding-agent-queue-steering)))
          ;; Should show minibuffer message
          (should message-shown)
          ;; Steering is NOT displayed locally - will be displayed when pi echoes it back
          (with-current-buffer chat-buf
            (should-not (string-match-p "My steering message" (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-input-mode-has-queue-keybindings ()
  "Input mode has C-c C-s for steering (C-c C-c handles follow-up)."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should (eq (key-binding (kbd "C-c C-s")) 'pi-coding-agent-queue-steering))
    ;; C-c C-c handles follow-up when streaming (no separate C-c C-q)
    (should (eq (key-binding (kbd "C-c C-c")) 'pi-coding-agent-send))))

(ert-deftest pi-coding-agent-test-queue-handles-rpc-error ()
  "Queue handles RPC error response by showing message to user."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-error*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-error-input*"))
        (captured-callback nil)
        (error-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Test message")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc _cmd cb) (setq captured-callback cb)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (when (and fmt (string-match-p "error\\|fail" (downcase fmt)))
                           (setq error-shown t)))))
              (pi-coding-agent-queue-steering)
              ;; Simulate error response from RPC
              (when captured-callback
                (funcall captured-callback '(:success :false :error "Queue limit reached")))))
          ;; Should have shown an error message
          (should error-shown))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-queue-with-dead-process-shows-error ()
  "Queue with dead process shows error message."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-queue-dead*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-queue-dead-input*"))
        (error-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Test message")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () nil))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (when (and fmt (string-match-p "process\\|unavailable\\|error" (downcase fmt)))
                           (setq error-shown t)))))
              (pi-coding-agent-queue-steering)))
          ;; Should have shown an error about unavailable process
          (should error-shown))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-send-when-idle-sends-literal-commands ()
  "C-c C-c when idle sends commands literally (pi expands)."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-send-slash*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-send-slash-input*"))
        (sent-prompt nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "/greet world")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt)
                       (lambda (text) (setq sent-prompt text))))
              (pi-coding-agent-send))
            ;; Should send literal command (pi handles expansion)
            (should (equal sent-prompt "/greet world"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

;; Note: pi-coding-agent-test-send-queues-locally-while-streaming covers this case

(ert-deftest pi-coding-agent-test-steering-when-idle-refuses ()
  "C-c C-s when idle shows message and does nothing."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-steer-idle*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-steer-idle-input*"))
        (send-called nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Steer message")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt)
                       (lambda (_) (setq send-called t)))
                      ((symbol-function 'pi-coding-agent--rpc-async)
                       (lambda (_proc _cmd _cb) (setq send-called t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "idle\\|nothing\\|use" (downcase fmt)))
                           (setq message-shown t)))))
              (pi-coding-agent-queue-steering))
            ;; Should NOT have sent anything
            (should-not send-called)
            ;; Should have shown a message
            (should message-shown)
            ;; Input should be preserved
            (should (equal (buffer-string) "Steer message"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-message-start-user-echo-ignored-when-displayed-locally ()
  "message_start role=user is ignored when we already displayed the same message locally.
Uses local-user-message to track what we displayed for comparison."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state nil)
          ;; Simulate that we displayed this message locally (normal send)
          (pi-coding-agent--local-user-message "Same message")
          (initial-content (buffer-string)))
      ;; Simulate receiving message_start for a user message (pi echoing back same text)
      (pi-coding-agent--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Same message")]
                   :timestamp 1704067200000)))
      ;; Buffer should be unchanged - pi's echo matches local display, so skip
      (should (equal (buffer-string) initial-content))
      (should-not (string-match-p "Same message" (buffer-string)))
      ;; Variable should be cleared
      (should-not pi-coding-agent--local-user-message))))

(ert-deftest pi-coding-agent-test-message-start-user-displayed-when-different ()
  "message_start role=user IS displayed when pi's text differs from local.
+This handles slash command expansion: user types '/greet', pi sends 'Hello!'."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state nil)
          (pi-coding-agent--local-user-message "/greet world")
          (initial-content (buffer-string)))
      (pi-coding-agent--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Hello world!")]
                   :timestamp 1704067200000)))
      ;; Should be displayed since text differs (expanded template)
      (should (string-match-p "Hello world!" (buffer-string)))
      (should-not pi-coding-agent--local-user-message))))

(ert-deftest pi-coding-agent-test-message-start-user-skipped-when-template-equals-command ()
  "Edge case: if template expands to exactly the command text, we skip display.
This is rare but possible - the local display is already correct."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state nil)
          (pi-coding-agent--local-user-message "/echo hello")
          (initial-content (buffer-string)))
      (pi-coding-agent--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "/echo hello")]
                   :timestamp 1704067200000)))
      ;; Should NOT be displayed - text matches what we displayed locally
      (should (equal (buffer-string) initial-content))
      (should-not pi-coding-agent--local-user-message))))

(ert-deftest pi-coding-agent-test-message-start-user-displayed-when-not-local ()
  "message_start role=user IS displayed when local-user-message is nil (steering case).
Steering messages are not displayed locally - they're displayed from the echo."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state nil)
          ;; Variable is nil - no locally displayed message pending
          (pi-coding-agent--local-user-message nil))
      ;; Simulate receiving message_start for a steering message
      (pi-coding-agent--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Steering message here")]
                   :timestamp 1704067200000)))
      ;; Should be displayed since local-user-message was nil
      (should (string-match-p "Steering message here" (buffer-string)))
      ;; Variable should still be nil
      (should-not pi-coding-agent--local-user-message))))

(ert-deftest pi-coding-agent-test-steering-display-not-interleaved ()
  "Steering message during streaming appears cleanly, not interleaved.
When user sends steering while assistant is streaming, the sequence is:
1. Current assistant output ends cleanly
2. User steering message with header appears
3. New assistant turn begins with its own header

This tests for a bug where user message header and assistant text got
mixed together like:
  > ...count from 1 to
  You · 01:32
  ===========
  STOP NOW
  10 slowly...  <- WRONG: '10 slowly' is assistant text after user msg!"
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--state nil)
          (pi-coding-agent--local-user-message nil)
          (pi-coding-agent--assistant-header-shown nil))
      ;; Simulate initial prompt response - assistant starts streaming
      (pi-coding-agent--handle-display-event '(:type "agent_start"))
      (pi-coding-agent--handle-display-event
       '(:type "message_start" :message (:role "assistant")))
      ;; Stream some content
      (pi-coding-agent--handle-display-event
       '(:type "message_update"
         :assistantMessageEvent (:type "text_delta" :delta "Counting: 1, 2, 3, ")))
      (pi-coding-agent--handle-display-event
       '(:type "message_update"
         :assistantMessageEvent (:type "text_delta" :delta "4, 5, 6, ")))

      ;; Now user sends steering - this comes as message_start with role=user
      ;; (steering messages are displayed from pi's echo, not locally)
      (pi-coding-agent--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "STOP-MARKER")]
                   :timestamp 1704067200000)))

      ;; Assistant continues with new turn after steering
      (setq pi-coding-agent--assistant-header-shown nil)  ; Reset for new turn
      (pi-coding-agent--handle-display-event '(:type "agent_start"))
      (pi-coding-agent--handle-display-event
       '(:type "message_start" :message (:role "assistant")))
      (pi-coding-agent--handle-display-event
       '(:type "message_update"
         :assistantMessageEvent (:type "text_delta" :delta "OK, stopping.")))
      (pi-coding-agent--handle-display-event '(:type "agent_end"))

      ;; Now verify the buffer structure
      (let ((content (buffer-string)))
        ;; All expected content should be present
        (should (string-match-p "Counting: 1, 2, 3, 4, 5, 6," content))
        (should (string-match-p "STOP-MARKER" content))
        (should (string-match-p "OK, stopping" content))

        ;; Find positions to verify order
        (let ((first-assistant-pos (string-match "Counting:" content))
              (steering-pos (string-match "STOP-MARKER" content))
              (second-response-pos (string-match "OK, stopping" content)))
          ;; Order must be: first-assistant < steering < second-response
          (should (< first-assistant-pos steering-pos))
          (should (< steering-pos second-response-pos))

          ;; "You" header must appear before the steering message
          (let ((you-header-pos (string-match "You" content)))
            (should you-header-pos)
            (should (< you-header-pos steering-pos)))

          ;; After STOP-MARKER, we should see "Assistant" header before second response
          (let* ((after-steering (substring content steering-pos))
                 (assistant-after-steering (string-match "Assistant" after-steering)))
            (should assistant-after-steering)))

        ;; Verify NO interleaving: counting text should NOT appear after STOP-MARKER
        (let* ((steering-pos (string-match "STOP-MARKER" content))
               (after-steering (substring content (+ steering-pos (length "STOP-MARKER")))))
          ;; Should NOT see counting continuation after the steering message
          (should-not (string-match-p "^[0-9]" (string-trim-left after-steering)))
          (should-not (string-match-p "^, [0-9]" (string-trim-left after-steering))))))))

(ert-deftest pi-coding-agent-test-local-user-message-tracks-display ()
  "The local-user-message variable tracks locally displayed messages.
- Normal send stores the text
- message_start role=user clears it to nil
- Steering doesn't set it (displayed from echo)
- agent_end clears it to nil"
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-echo-flag*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-echo-flag-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf)
            ;; Variable starts as nil
            (should-not pi-coding-agent--local-user-message))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "First message")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt) #'ignore))
              (pi-coding-agent-send)))
          ;; After normal send, variable should store the message text
          (with-current-buffer chat-buf
            (should (equal pi-coding-agent--local-user-message "First message"))
            ;; Simulate pi echo - variable clears to nil
            (pi-coding-agent--handle-display-event
             '(:type "message_start"
               :message (:role "user" :content [(:type "text" :text "First message")])))
            (should-not pi-coding-agent--local-user-message)
            ;; Now simulate steering (doesn't set it)
            (setq pi-coding-agent--status 'streaming))
          (with-current-buffer input-buf
            (erase-buffer)
            (insert "Steer this")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--rpc-async) #'ignore))
              (pi-coding-agent-queue-steering)))
          ;; Variable still nil (steering doesn't set it)
          (with-current-buffer chat-buf
            (should-not pi-coding-agent--local-user-message)
            ;; agent_end clears to nil (in case of edge cases)
            (setq pi-coding-agent--local-user-message "test")  ; Simulate weird state
            (pi-coding-agent--display-agent-end)
            (should-not pi-coding-agent--local-user-message)))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-normal-send-not-duplicated-by-message-start ()
  "Normal send should not be duplicated when message_start arrives.
When user sends a message normally (idle state), we display it immediately.
When pi echoes it back via message_start, we should NOT display it again."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-no-dup*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-no-dup-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'idle)
            (setq pi-coding-agent--input-buffer input-buf))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Hello pi")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt) #'ignore))
              (pi-coding-agent-send)))
          ;; Now simulate pi echoing the message back via message_start
          (with-current-buffer chat-buf
            (pi-coding-agent--handle-display-event
             '(:type "message_start"
               :message (:role "user"
                         :content [(:type "text" :text "Hello pi")]
                         :timestamp 1704067200000)))
            ;; Count occurrences of "Hello pi" - should be exactly 1
            (let ((count 0)
                  (start 0))
              (while (string-match "Hello pi" (buffer-string) start)
                (setq count (1+ count))
                (setq start (match-end 0)))
              (should (= count 1)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-agent-end-sends-queued-followup ()
  "agent_end pops from followup queue and sends as normal prompt.
When user queues a follow-up (busy state), it goes to local queue.
On agent_end, we pop from queue and send (which displays the message)."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-agent-end-queue*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-agent-end-queue-input*"))
        (sent-prompt nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf)
            (setq pi-coding-agent--followup-queue nil)
            ;; Simulate some prior content
            (let ((inhibit-read-only t))
              (insert "Assistant\n=========\nSome response...\n")))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "My follow-up question")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t)))
              (pi-coding-agent-send)))  ; Adds to local queue when streaming
          ;; Message should be in queue, not in chat yet
          (with-current-buffer chat-buf
            (should (equal pi-coding-agent--followup-queue '("My follow-up question")))
            (should-not (string-match-p "My follow-up question" (buffer-string))))
          ;; Now simulate agent_end - this should pop queue and send
          (with-current-buffer chat-buf
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt)
                       (lambda (text) (setq sent-prompt text)))
                      ((symbol-function 'pi-coding-agent--fontify-timer-stop) #'ignore)
                      ((symbol-function 'pi-coding-agent--refresh-header) #'ignore))
              (pi-coding-agent--handle-display-event '(:type "agent_end")))
            ;; Queue should be empty now
            (should (null pi-coding-agent--followup-queue))
            ;; Should have sent the queued message
            (should (equal sent-prompt "My follow-up question"))
            ;; Message should now be displayed in chat
            (should (string-match-p "My follow-up question" (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-followup-queue-fifo-order ()
  "Multiple follow-ups are processed in FIFO order."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-fifo*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-fifo-input*"))
        (sent-prompts nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf)
            (setq pi-coding-agent--followup-queue nil))
          ;; Queue three messages while busy
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (dolist (msg '("First message" "Second message" "Third message"))
              (erase-buffer)
              (insert msg)
              (pi-coding-agent-send)))
          ;; All three should be in queue
          (with-current-buffer chat-buf
            (should (= 3 (length pi-coding-agent--followup-queue))))
          ;; Now simulate agent_end three times, capturing what gets sent
          (with-current-buffer chat-buf
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--send-prompt)
                       (lambda (text) (push text sent-prompts)))
                      ((symbol-function 'pi-coding-agent--fontify-timer-stop) #'ignore)
                      ((symbol-function 'pi-coding-agent--refresh-header) #'ignore))
              ;; First agent_end
              (pi-coding-agent--handle-display-event '(:type "agent_end"))
              ;; Second agent_end
              (pi-coding-agent--handle-display-event '(:type "agent_end"))
              ;; Third agent_end
              (pi-coding-agent--handle-display-event '(:type "agent_end")))
            ;; Should have sent all three in FIFO order (sent-prompts is reversed)
            (should (equal (reverse sent-prompts)
                           '("First message" "Second message" "Third message")))
            ;; Queue should be empty
            (should (null pi-coding-agent--followup-queue))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-steering-displayed-from-echo ()
  "Steering is NOT displayed locally - it's displayed when pi echoes it back.
This ensures steering appears at the correct position in the conversation
(after the current assistant output completes)."
  (let ((chat-buf (get-buffer-create "*pi-coding-agent-test-steer-echo*"))
        (input-buf (get-buffer-create "*pi-coding-agent-test-steer-echo-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--status 'streaming)
            (setq pi-coding-agent--input-buffer input-buf)
            (let ((inhibit-read-only t))
              (insert "Assistant\n=========\nWorking on something...\n")))
          (with-current-buffer input-buf
            (pi-coding-agent-input-mode)
            (setq pi-coding-agent--chat-buffer chat-buf)
            (insert "Stop and do something else")
            (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'pi-coding-agent--rpc-async) #'ignore))
              (pi-coding-agent-queue-steering)))
          ;; Steering is NOT displayed when sent (unlike normal sends)
          (with-current-buffer chat-buf
            (should-not (string-match-p "Stop and do something else" (buffer-string)))
            ;; local-user-message should still be nil (steering doesn't set it)
            (should-not pi-coding-agent--local-user-message))
          ;; Simulate pi echoing the steering message back via message_start
          (with-current-buffer chat-buf
            (pi-coding-agent--handle-display-event
             '(:type "message_start"
               :message (:role "user"
                         :content [(:type "text" :text "Stop and do something else")]
                         :timestamp 1704067200000)))
            ;; NOW it should be displayed (from the echo)
            (should (string-match-p "Stop and do something else" (buffer-string)))
            ;; Should be displayed exactly once
            (let ((count 0)
                  (start 0))
              (while (string-match "Stop and do something else" (buffer-string) start)
                (setq count (1+ count))
                (setq start (match-end 0)))
              (should (= count 1)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest pi-coding-agent-test-steering-echo-followed-by-assistant-shows-header ()
  "After steering message, the next assistant message shows its header.
This tests the full flow: steering echo resets the flag, then the next
message_start role=assistant displays the 'Assistant' header."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((pi-coding-agent--status 'streaming)
          (pi-coding-agent--local-user-message nil)
          ;; Simulate that first assistant header was already shown
          (pi-coding-agent--assistant-header-shown t))
      ;; First, some assistant content is already in the buffer
      (let ((inhibit-read-only t))
        (insert "Assistant\n=========\nPrevious response...\n"))
      ;; Simulate steering message echo from pi
      (pi-coding-agent--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Stop it")]
                   :timestamp 1704067200000)))
      ;; Steering message should be displayed
      (should (string-match-p "Stop it" (buffer-string)))
      ;; Flag should be reset
      (should-not pi-coding-agent--assistant-header-shown)
      ;; Now simulate the assistant's response to steering
      (pi-coding-agent--handle-display-event
       '(:type "message_start"
         :message (:role "assistant")))
      ;; Now we should see TWO "Assistant" headers in the buffer
      (let ((count 0)
            (start 0)
            (content (buffer-string)))
        (while (string-match "Assistant\n=+" content start)
          (setq count (1+ count))
          (setq start (match-end 0)))
        (should (= count 2))))))


(ert-deftest pi-coding-agent-test-tool-toggle-expands-content ()
  "Toggle button expands collapsed tool output."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Initially collapsed - should have "... (N more lines)"
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    (should-not (string-match-p "L10" (buffer-string)))
    ;; Find and click the button
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (pi-coding-agent-toggle-tool-section)
    ;; Now should show all lines
    (should (string-match-p "L10" (buffer-string)))
    (should (string-match-p "\\[-\\]" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-toggle-collapses-content ()
  "Toggle button collapses expanded tool output."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Expand first
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (pi-coding-agent-toggle-tool-section)
    (should (string-match-p "L10" (buffer-string)))
    ;; Now collapse
    (goto-char (point-min))
    (search-forward "[-]" nil t)
    (backward-char 1)
    (pi-coding-agent-toggle-tool-section)
    ;; Should be collapsed again
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    (should-not (string-match-p "L10" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-toggle-expands-with-highlighting ()
  "Expanded tool output has syntax highlighting applied.
Regression test for issue #32: JIT font-lock doesn't fontify expanded content."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Create a read tool with Python content (>10 lines to trigger collapse)
    ;; The 'def' keyword is on line 11, hidden initially
    (pi-coding-agent--display-tool-start "read" '(:path "test.py"))
    (pi-coding-agent--display-tool-end "read" '(:path "test.py")
                          '((:type "text" :text "line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\ndef hello():\n    return 42"))
                          nil nil)
    ;; Initially collapsed - 'def' is hidden
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    (should-not (string-match-p "def hello" (buffer-string)))
    ;; Expand
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (pi-coding-agent-toggle-tool-section)
    ;; Now 'def' should be visible
    (should (string-match-p "def hello" (buffer-string)))
    ;; Find 'def' keyword and check for syntax highlighting
    (goto-char (point-min))
    (search-forward "def" nil t)
    (let ((face (get-text-property (match-beginning 0) 'face)))
      ;; Should have font-lock-keyword-face from python-mode
      (should (or (eq face 'font-lock-keyword-face)
                  (and (listp face) (memq 'font-lock-keyword-face face)))))))

(ert-deftest pi-coding-agent-test-tab-works-from-anywhere-in-block ()
  "TAB toggles tool output from any position within the block."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Go to the header line (not the button)
    (goto-char (point-min))
    (search-forward "$ ls" nil t)
    (beginning-of-line)
    ;; TAB should still expand
    (pi-coding-agent-toggle-tool-section)
    (should (string-match-p "L10" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tab-preserves-cursor-position ()
  "TAB toggle doesn't jump cursor unnecessarily."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Go to L3 line
    (goto-char (point-min))
    (search-forward "L3" nil t)
    (beginning-of-line)
    (let ((line-content (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
      ;; Expand
      (pi-coding-agent-toggle-tool-section)
      ;; Should still be on a line starting with L
      (should (string-match-p "^L[0-9]" 
                              (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position)))))))

(ert-deftest pi-coding-agent-test-toggle-preserves-window-scroll ()
  "Toggle collapse/expand should preserve window scroll when viewing content before tool.
When window shows content BEFORE the tool block, toggle should not jump away."
  (let ((buf (generate-new-buffer "*test-toggle-scroll*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (pi-coding-agent-chat-mode)
            ;; Add some content before the tool block
            (let ((inhibit-read-only t))
              (insert "Header line 1\nHeader line 2\nHeader line 3\n\n"))
            ;; Create tool output with many lines
            (pi-coding-agent--display-tool-start "read" '(:path "test.el"))
            (pi-coding-agent--display-tool-end
             "read" nil
             `((:type "text"
                :text ,(mapconcat (lambda (n) (format "Line %03d content" n))
                                  (number-sequence 1 50) "\n")))
             nil nil))
          ;; Display buffer in a window so we can test scroll
          (let ((win (display-buffer buf)))
            (when win
              (with-selected-window win
                ;; Position window at the header (before tool block)
                (goto-char (point-min))
                (recenter 0)
                (let ((start-before (window-start win)))
                  ;; Expand the tool content
                  (search-forward "..." nil t)
                  (pi-coding-agent-toggle-tool-section)
                  ;; Window should not have jumped
                  (should (= (window-start win) start-before))
                  ;; Now collapse
                  (search-forward "[-]" nil t)
                  (pi-coding-agent-toggle-tool-section)
                  ;; Window should still be at same position
                  (should (= (window-start win) start-before)))))))
      (kill-buffer buf))))

(ert-deftest pi-coding-agent-test-format-fork-message ()
  "Fork message formatted with index and preview."
  (let ((msg '(:entryId "abc-123" :text "Hello world, this is a test")))
    ;; With index
    (let ((result (pi-coding-agent--format-fork-message msg 2)))
      (should (string-match-p "2:" result))
      (should (string-match-p "Hello world" result)))
    ;; Without index
    (let ((result (pi-coding-agent--format-fork-message msg)))
      (should (string-match-p "Hello world" result))
      (should-not (string-match-p ":" result)))))

(ert-deftest pi-coding-agent-test-fork-detects-empty-messages-vector ()
  "Fork correctly detects empty messages vector from RPC.
JSON arrays are parsed as vectors, and (null []) is nil, not t.
The fork code must use seq-empty-p or length check."
  (let ((rpc-called nil)
        (message-shown nil))
    (cl-letf (((symbol-function 'pi-coding-agent--get-process) (lambda () 'mock-proc))
              ((symbol-function 'pi-coding-agent--rpc-async)
               (lambda (_proc cmd cb)
                 (setq rpc-called t)
                 ;; Simulate response with empty vector (no messages to fork from)
                 (funcall cb '(:success t :data (:messages [])))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when (string-match-p "No messages" fmt)
                   (setq message-shown t)))))
      (pi-coding-agent-fork)
      (should rpc-called)
      ;; Should show "No messages to fork from", not call completing-read
      (should message-shown))))

(ert-deftest pi-coding-agent-test-format-fork-message-handles-nil-text ()
  "Format fork message handles nil text gracefully."
  (let ((msg '(:entryId "abc-123" :text nil)))
    ;; Should not error, should return something displayable
    (let ((result (pi-coding-agent--format-fork-message msg 1)))
      (should (stringp result)))))

(ert-deftest pi-coding-agent-test-load-session-history-uses-provided-buffer ()
  "load-session-history uses provided chat buffer, not current buffer context.
This ensures history loads correctly when callback runs in arbitrary context."
  (let* ((chat-buf (generate-new-buffer "*pi-coding-agent-chat:test-history/*"))
         (rpc-callback nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode))
          ;; Mock RPC to capture callback
          (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc _cmd cb) (setq rpc-callback cb))))
            ;; Call with explicit buffer
            (pi-coding-agent--load-session-history 'mock-proc nil chat-buf))
          ;; Simulate callback from different buffer context
          (with-temp-buffer
            (funcall rpc-callback
                     '(:success t :data (:messages [(:role "user" :content "test")]))))
          ;; Chat buffer should have been updated (has startup header)
          (with-current-buffer chat-buf
            (should (string-match-p "C-c C-c" (buffer-string)))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pi-coding-agent-test-load-session-history-restores-context-usage ()
  "load-session-history sets last-usage from final assistant message.
This ensures context percentage is displayed correctly after resume/fork.
Regression test: context showed 0% after resuming because usage wasn't extracted."
  (let* ((chat-buf (generate-new-buffer "*pi-coding-agent-chat:test-usage/*"))
         (rpc-callback nil)
         ;; Simulate a Fortnite tips conversation with usage data
         (mock-messages
          [(:role "user"
            :content "How do I get better at Fortnite?"
            :timestamp 1704067200000)
           (:role "assistant"
            :content [(:type "text" :text "Here are some tips to improve at Fortnite:\n\n1. **Practice building** - Building is essential\n2. **Land at busy spots** - More combat practice\n3. **Watch pro players** - Learn advanced techniques")]
            :usage (:input 150 :output 80 :cacheRead 0 :cacheWrite 50)
            :stopReason "endTurn"
            :timestamp 1704067260000)
           (:role "user"
            :content "What about aiming?"
            :timestamp 1704067320000)
           (:role "assistant"
            :content [(:type "text" :text "For better aim:\n\n1. Lower your sensitivity\n2. Use aim trainers\n3. Practice tracking moving targets")]
            :usage (:input 280 :output 120 :cacheRead 50 :cacheWrite 30)
            :stopReason "endTurn"
            :timestamp 1704067380000)]))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            ;; Ensure usage starts as nil
            (should-not pi-coding-agent--last-usage))
          ;; Mock RPC to capture callback
          (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc _cmd cb) (setq rpc-callback cb))))
            (pi-coding-agent--load-session-history 'mock-proc nil chat-buf))
          ;; Simulate RPC response with our mock messages
          (funcall rpc-callback
                   `(:success t :data (:messages ,mock-messages)))
          ;; Verify last-usage was extracted from final assistant message
          (with-current-buffer chat-buf
            (should pi-coding-agent--last-usage)
            (should (equal (plist-get pi-coding-agent--last-usage :input) 280))
            (should (equal (plist-get pi-coding-agent--last-usage :output) 120))
            (should (equal (plist-get pi-coding-agent--last-usage :cacheRead) 50))
            (should (equal (plist-get pi-coding-agent--last-usage :cacheWrite) 30))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pi-coding-agent-test-load-session-history-skips-aborted-usage ()
  "load-session-history skips aborted messages when extracting usage.
Aborted messages may have incomplete usage data."
  (let* ((chat-buf (generate-new-buffer "*pi-coding-agent-chat:test-aborted/*"))
         (rpc-callback nil)
         ;; Session where last message was aborted
         (mock-messages
          [(:role "user"
            :content "Tell me about Fortnite"
            :timestamp 1704067200000)
           (:role "assistant"
            :content [(:type "text" :text "Fortnite is a battle royale game...")]
            :usage (:input 100 :output 50 :cacheRead 0 :cacheWrite 20)
            :stopReason "endTurn"
            :timestamp 1704067260000)
           (:role "user"
            :content "More details"
            :timestamp 1704067320000)
           (:role "assistant"
            :content [(:type "text" :text "Well...")]
            :usage (:input 0 :output 0 :cacheRead 0 :cacheWrite 0)
            :stopReason "aborted"
            :timestamp 1704067380000)]))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode))
          (cl-letf (((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc _cmd cb) (setq rpc-callback cb))))
            (pi-coding-agent--load-session-history 'mock-proc nil chat-buf))
          (funcall rpc-callback
                   `(:success t :data (:messages ,mock-messages)))
          ;; Should use the non-aborted assistant message's usage
          (with-current-buffer chat-buf
            (should pi-coding-agent--last-usage)
            (should (equal (plist-get pi-coding-agent--last-usage :input) 100))
            (should (equal (plist-get pi-coding-agent--last-usage :output) 50))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pi-coding-agent-test-session-dir-name ()
  "Session directory name derived from project path."
  (should (equal (pi-coding-agent--session-dir-name "/home/daniel/co/pi-coding-agent")
                 "--home-daniel-co-pi-coding-agent--"))
  (should (equal (pi-coding-agent--session-dir-name "/tmp/test")
                 "--tmp-test--")))

(ert-deftest pi-coding-agent-test-list-sessions-sorted-by-mtime ()
  "Sessions are sorted by modification time, most recent first.
Regression test for #25: sessions were sorted by filename (creation time)
and then re-sorted alphabetically by completing-read."
  (let* ((temp-base (make-temp-file "pi-coding-agent-sessions-" t))
         (session-dir (expand-file-name "--test-project--" temp-base))
         ;; Create files with names that would sort differently alphabetically
         (old-file (expand-file-name "2024-01-01_10-00-00.jsonl" session-dir))
         (new-file (expand-file-name "2024-01-01_09-00-00.jsonl" session-dir)))
    (unwind-protect
        (progn
          (make-directory session-dir t)
          (let* ((now (current-time))
                 (old-time (time-subtract now (seconds-to-time 10)))
                 (new-time (time-subtract now (seconds-to-time 5))))
            ;; Create "old" file first
            (with-temp-file old-file (insert "{}"))
            (set-file-times old-file old-time)
            ;; Create "new" file second (more recent mtime despite earlier filename)
            (with-temp-file new-file (insert "{}"))
            (set-file-times new-file new-time))
          ;; Directly call directory-files and sort logic to test sorting
          (let* ((files (directory-files session-dir t "\\.jsonl$"))
                 (sorted (sort files
                               (lambda (a b)
                                 (time-less-p
                                  (file-attribute-modification-time (file-attributes b))
                                  (file-attribute-modification-time (file-attributes a)))))))
            ;; new-file should be first (most recent mtime)
            ;; even though "09-00-00" < "10-00-00" alphabetically
            (should (equal (length sorted) 2))
            (should (string-suffix-p "09-00-00.jsonl" (car sorted)))))
      ;; Cleanup
      (delete-directory temp-base t))))

(ert-deftest pi-coding-agent-test-session-metadata-extracts-first-message ()
  "pi-coding-agent--session-metadata extracts first user message text."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n")
            (insert "{\"type\":\"message\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Hello world\"}]}}\n")
            (insert "{\"type\":\"message\",\"message\":{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"Hi\"}]}}\n"))
          (let ((metadata (pi-coding-agent--session-metadata temp-file)))
            (should metadata)
            (should (equal (plist-get metadata :first-message) "Hello world"))
            (should (equal (plist-get metadata :message-count) 2))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-session-metadata-returns-nil-for-empty-file ()
  "pi-coding-agent--session-metadata returns nil for empty or invalid files."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          ;; Empty file
          (should (null (pi-coding-agent--session-metadata temp-file))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-session-metadata-handles-missing-first-message ()
  "pi-coding-agent--session-metadata handles session with only header."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n"))
          (let ((metadata (pi-coding-agent--session-metadata temp-file)))
            (should metadata)
            (should (null (plist-get metadata :first-message)))
            (should (equal (plist-get metadata :message-count) 0))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-session-metadata-extracts-session-name ()
  "pi-coding-agent--session-metadata extracts session name from session_info entry."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n")
            (insert "{\"type\":\"message\",\"id\":\"m1\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Hello\"}]}}\n")
            (insert "{\"type\":\"session_info\",\"id\":\"si1\",\"name\":\"Refactor auth module\"}\n"))
          (let ((metadata (pi-coding-agent--session-metadata temp-file)))
            (should metadata)
            (should (equal (plist-get metadata :session-name) "Refactor auth module"))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-session-metadata-uses-latest-session-name ()
  "pi-coding-agent--session-metadata uses the most recent session_info name."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n")
            (insert "{\"type\":\"session_info\",\"id\":\"si1\",\"name\":\"Old name\"}\n")
            (insert "{\"type\":\"message\",\"id\":\"m1\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Hello\"}]}}\n")
            (insert "{\"type\":\"session_info\",\"id\":\"si2\",\"name\":\"New name\"}\n"))
          (let ((metadata (pi-coding-agent--session-metadata temp-file)))
            (should metadata)
            (should (equal (plist-get metadata :session-name) "New name"))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-session-metadata-ignores-null-name ()
  "pi-coding-agent--session-metadata treats null name as cleared (no name)."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n")
            (insert "{\"type\":\"session_info\",\"id\":\"si1\",\"name\":\"My Session\"}\n")
            (insert "{\"type\":\"message\",\"id\":\"m1\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Hello\"}]}}\n")
            ;; User cleared the session name - null means no name
            (insert "{\"type\":\"session_info\",\"id\":\"si2\",\"name\":null}\n"))
          (let ((metadata (pi-coding-agent--session-metadata temp-file)))
            (should metadata)
            ;; Should be nil, not :null
            (should (null (plist-get metadata :session-name)))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-format-session-choice-fallback-on-cleared-name ()
  "pi-coding-agent--format-session-choice falls back to message when name cleared."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n")
            (insert "{\"type\":\"message\",\"id\":\"m1\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Hello world\"}]}}\n")
            (insert "{\"type\":\"session_info\",\"id\":\"si1\",\"name\":\"My Project\"}\n")
            ;; Name was cleared
            (insert "{\"type\":\"session_info\",\"id\":\"si2\",\"name\":null}\n"))
          (let ((choice (pi-coding-agent--format-session-choice temp-file)))
            ;; Should fall back to first message, not crash
            (should (string-match-p "Hello world" (car choice)))
            (should-not (string-match-p "My Project" (car choice)))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-format-session-choice-prefers-name ()
  "pi-coding-agent--format-session-choice uses session name when available."
  (let ((temp-file (make-temp-file "pi-coding-agent-test-session" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "{\"type\":\"session\",\"id\":\"test\"}\n")
            (insert "{\"type\":\"message\",\"id\":\"m1\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Hello world\"}]}}\n")
            (insert "{\"type\":\"session_info\",\"id\":\"si1\",\"name\":\"My Project\"}\n"))
          (let ((choice (pi-coding-agent--format-session-choice temp-file)))
            ;; Should show session name, not first message
            (should (string-match-p "My Project" (car choice)))
            (should-not (string-match-p "Hello world" (car choice)))))
      (delete-file temp-file))))

(ert-deftest pi-coding-agent-test-header-line-includes-session-name ()
  "pi-coding-agent--header-line-string includes session name when set."
  (let ((chat-buf (get-buffer-create "*pi-test-header-session-name*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (pi-coding-agent-chat-mode)
          (setq pi-coding-agent--state '(:model (:name "test-model") :thinking-level "high"))
          ;; Without session name
          (setq pi-coding-agent--session-name nil)
          (let ((header (pi-coding-agent--header-line-string)))
            (should-not (string-match-p "My Session" header)))
          ;; With session name
          (setq pi-coding-agent--session-name "My Session")
          (let ((header (pi-coding-agent--header-line-string)))
            (should (string-match-p "My Session" header))
            ;; Should have separator before session name
            (should (string-match-p "│" header))))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-header-line-truncates-long-session-name ()
  "pi-coding-agent--header-line-string truncates long session names."
  (let ((chat-buf (get-buffer-create "*pi-test-header-truncate*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (pi-coding-agent-chat-mode)
          (setq pi-coding-agent--state '(:model (:name "test-model")))
          ;; Set a very long session name (longer than 30 chars)
          (setq pi-coding-agent--session-name "This is a very long session name that should be truncated")
          (let ((header (pi-coding-agent--header-line-string)))
            ;; Should contain truncated version with ellipsis
            (should (string-match-p "This is a very long session" header))
            (should (string-match-p "…" header))
            ;; Should NOT contain the full name
            (should-not (string-match-p "truncated$" header))))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-set-session-name-empty-shows-current ()
  "pi-coding-agent-set-session-name with empty string shows current name."
  (let ((chat-buf (get-buffer-create "*pi-test-show-name*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--session-name "My Session"))
          ;; Capture message output
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (pi-coding-agent-set-session-name "")))
          ;; Should show current name, not change it
          (should (equal (buffer-local-value 'pi-coding-agent--session-name chat-buf)
                         "My Session"))
          (should (member "Pi: Session name: My Session" messages)))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-set-session-name-empty-no-name-shows-message ()
  "pi-coding-agent-set-session-name with empty string and no name shows message."
  (let ((chat-buf (get-buffer-create "*pi-test-no-name*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--session-name nil))
          ;; Capture message output
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (pi-coding-agent-set-session-name "")))
          (should (member "Pi: No session name set" messages)))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-set-session-name-no-process-errors ()
  "pi-coding-agent-set-session-name errors when no process is running."
  (let ((chat-buf (get-buffer-create "*pi-test-no-proc*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode))
          ;; Mock pi-coding-agent--get-process to return nil
          (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                     (lambda () nil)))
            (should-error
             (with-current-buffer chat-buf
               (pi-coding-agent-set-session-name "New Name"))
             :type 'user-error)))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-set-session-name-sends-rpc ()
  "pi-coding-agent-set-session-name sends correct RPC command."
  (let ((chat-buf (get-buffer-create "*pi-test-rpc*"))
        (pi-coding-agent--request-id-counter 0)
        (output-buffer (generate-new-buffer " *test-output*")))
    (unwind-protect
        (let ((fake-proc (start-process "cat" output-buffer "cat")))
          (unwind-protect
              (progn
                (with-current-buffer chat-buf
                  (pi-coding-agent-chat-mode))
                ;; Mock get-process and get-chat-buffer
                (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                           (lambda () fake-proc))
                          ((symbol-function 'pi-coding-agent--get-chat-buffer)
                           (lambda () chat-buf)))
                  (pi-coding-agent-set-session-name "Test Session"))
                ;; Wait for output
                (pi-coding-agent-test-wait-until
                 (lambda ()
                   (with-current-buffer output-buffer
                     (> (buffer-size) 0)))
                 1.0 0.05 fake-proc)
                ;; Verify JSON sent
                (with-current-buffer output-buffer
                  (let* ((sent (buffer-string))
                         (json (json-parse-string (string-trim sent) :object-type 'plist)))
                    (should (equal (plist-get json :type) "set_session_name"))
                    (should (equal (plist-get json :name) "Test Session")))))
            (delete-process fake-proc)))
      (kill-buffer output-buffer)
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-set-session-name-trims-whitespace ()
  "pi-coding-agent-set-session-name trims whitespace from name."
  (let ((chat-buf (get-buffer-create "*pi-test-trim*"))
        (pi-coding-agent--request-id-counter 0)
        (output-buffer (generate-new-buffer " *test-output*")))
    (unwind-protect
        (let ((fake-proc (start-process "cat" output-buffer "cat")))
          (unwind-protect
              (progn
                (with-current-buffer chat-buf
                  (pi-coding-agent-chat-mode))
                ;; Mock get-process and get-chat-buffer
                (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                           (lambda () fake-proc))
                          ((symbol-function 'pi-coding-agent--get-chat-buffer)
                           (lambda () chat-buf)))
                  (pi-coding-agent-set-session-name "  Trimmed Name  "))
                ;; Wait for output
                (pi-coding-agent-test-wait-until
                 (lambda ()
                   (with-current-buffer output-buffer
                     (> (buffer-size) 0)))
                 1.0 0.05 fake-proc)
                ;; Verify JSON sent has trimmed name
                (with-current-buffer output-buffer
                  (let* ((sent (buffer-string))
                         (json (json-parse-string (string-trim sent) :object-type 'plist)))
                    (should (equal (plist-get json :name) "Trimmed Name")))))
            (delete-process fake-proc)))
      (kill-buffer output-buffer)
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-set-session-name-whitespace-only-shows-current ()
  "pi-coding-agent-set-session-name with whitespace-only shows current name."
  (let ((chat-buf (get-buffer-create "*pi-test-ws*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--session-name "Existing Name"))
          ;; Capture message output
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (pi-coding-agent-set-session-name "   ")))  ; whitespace only
          ;; Should show current name, not try to set
          (should (equal (buffer-local-value 'pi-coding-agent--session-name chat-buf)
                         "Existing Name"))
          (should (member "Pi: Session name: Existing Name" messages)))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-set-session-name-rpc-failure-shows-error ()
  "pi-coding-agent-set-session-name shows error on RPC failure."
  (let ((chat-buf (get-buffer-create "*pi-test-fail*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--session-name "Old Name"))
          ;; Mock RPC to call callback with failure
          (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                     (lambda () 'fake-proc))
                    ((symbol-function 'pi-coding-agent--get-chat-buffer)
                     (lambda () chat-buf))
                    ((symbol-function 'pi-coding-agent--rpc-async)
                     (lambda (_proc _cmd callback)
                       (funcall callback '(:success nil :error "test error"))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (pi-coding-agent-set-session-name "New Name")))
          ;; Name should NOT be updated
          (should (equal (buffer-local-value 'pi-coding-agent--session-name chat-buf)
                         "Old Name"))
          ;; Error message should be shown
          (should (member "Pi: Failed to set session name: test error" messages)))
      (kill-buffer chat-buf))))

;;; Input History

(ert-deftest pi-coding-agent-test-history-add-to-ring ()
  "pi-coding-agent--history-add adds input to ring."
  (let ((pi-coding-agent--input-ring nil))
    (pi-coding-agent--history-add "first")
    (pi-coding-agent--history-add "second")
    (should (equal (ring-ref (pi-coding-agent--input-ring) 0) "second"))
    (should (equal (ring-ref (pi-coding-agent--input-ring) 1) "first"))))

(ert-deftest pi-coding-agent-test-history-no-duplicate ()
  "pi-coding-agent--history-add skips duplicates of last entry."
  (let ((pi-coding-agent--input-ring nil))
    (pi-coding-agent--history-add "first")
    (pi-coding-agent--history-add "first")
    (should (= (ring-length (pi-coding-agent--input-ring)) 1))))

(ert-deftest pi-coding-agent-test-history-skip-empty ()
  "pi-coding-agent--history-add skips empty input."
  (let ((pi-coding-agent--input-ring nil))
    (pi-coding-agent--history-add "")
    (pi-coding-agent--history-add "   ")
    (should (ring-empty-p (pi-coding-agent--input-ring)))))

(ert-deftest pi-coding-agent-test-history-previous-input ()
  "pi-coding-agent-previous-input navigates backward through history."
  (let ((pi-coding-agent--input-ring nil))
    (pi-coding-agent--history-add "first")
    (pi-coding-agent--history-add "second")
    (with-temp-buffer
      (pi-coding-agent-input-mode)
      (insert "current")
      (pi-coding-agent-previous-input)
      (should (equal (buffer-string) "second"))
      (should (equal pi-coding-agent--input-saved "current"))
      (pi-coding-agent-previous-input)
      (should (equal (buffer-string) "first")))))

(ert-deftest pi-coding-agent-test-history-next-input ()
  "pi-coding-agent-next-input navigates forward and restores saved input."
  (let ((pi-coding-agent--input-ring nil))
    (pi-coding-agent--history-add "first")
    (pi-coding-agent--history-add "second")
    (with-temp-buffer
      (pi-coding-agent-input-mode)
      (insert "current")
      (pi-coding-agent-previous-input)
      (pi-coding-agent-previous-input)
      (should (equal (buffer-string) "first"))
      (pi-coding-agent-next-input)
      (should (equal (buffer-string) "second"))
      (pi-coding-agent-next-input)
      (should (equal (buffer-string) "current")))))

(ert-deftest pi-coding-agent-test-history-keys-bound ()
  "History keys are bound in pi-coding-agent-input-mode."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should (eq (key-binding (kbd "M-p")) 'pi-coding-agent-previous-input))
    (should (eq (key-binding (kbd "M-n")) 'pi-coding-agent-next-input))
    (should (eq (key-binding (kbd "C-r")) 'pi-coding-agent-history-isearch-backward))))

(ert-deftest pi-coding-agent-test-history-isolated-per-buffer ()
  "Input history is isolated per buffer, not shared globally.
Regression test for #27: history was shared across all sessions."
  (let ((buf1 (generate-new-buffer "*pi-coding-agent-input:project-a*"))
        (buf2 (generate-new-buffer "*pi-coding-agent-input:project-b*")))
    (unwind-protect
        (progn
          ;; Add history in buffer 1
          (with-current-buffer buf1
            (pi-coding-agent-input-mode)
            (pi-coding-agent--history-add "project-a-query"))
          ;; Add different history in buffer 2
          (with-current-buffer buf2
            (pi-coding-agent-input-mode)
            (pi-coding-agent--history-add "project-b-query"))
          ;; Buffer 1 should only see its own history
          (with-current-buffer buf1
            (should (= (ring-length (pi-coding-agent--input-ring)) 1))
            (should (equal (ring-ref (pi-coding-agent--input-ring) 0) "project-a-query")))
          ;; Buffer 2 should only see its own history
          (with-current-buffer buf2
            (should (= (ring-length (pi-coding-agent--input-ring)) 1))
            (should (equal (ring-ref (pi-coding-agent--input-ring) 0) "project-b-query"))))
      ;; Cleanup
      (kill-buffer buf1)
      (kill-buffer buf2))))

;;; History Isearch (C-r incremental search)

(ert-deftest pi-coding-agent-test-history-isearch-empty-history-errors ()
  "pi-coding-agent-history-isearch-backward errors with empty history."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should-error (pi-coding-agent-history-isearch-backward) :type 'user-error)))

(ert-deftest pi-coding-agent-test-history-isearch-saves-current-input ()
  "pi-coding-agent-history-isearch-backward saves current buffer content."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (pi-coding-agent--history-add "old command")
    (insert "my current input")
    ;; Mock isearch-backward to avoid actually starting isearch
    (cl-letf (((symbol-function 'isearch-backward) #'ignore))
      (pi-coding-agent-history-isearch-backward))
    (should (equal pi-coding-agent--history-isearch-saved-input "my current input"))))

(ert-deftest pi-coding-agent-test-history-isearch-sets-active-flag ()
  "pi-coding-agent-history-isearch-backward sets the active flag."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (pi-coding-agent--history-add "old command")
    (cl-letf (((symbol-function 'isearch-backward) #'ignore))
      (pi-coding-agent-history-isearch-backward))
    (should pi-coding-agent--history-isearch-active)))

(ert-deftest pi-coding-agent-test-history-isearch-end-restores-on-quit ()
  "pi-coding-agent--history-isearch-end restores input when isearch is quit."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (setq pi-coding-agent--history-isearch-active t)
    (setq pi-coding-agent--history-isearch-saved-input "original input")
    (erase-buffer)
    (insert "some history item")
    ;; Simulate isearch quit
    (let ((isearch-mode-end-hook-quit t))
      (pi-coding-agent--history-isearch-end))
    (should (equal (buffer-string) "original input"))
    (should-not pi-coding-agent--history-isearch-active)))

(ert-deftest pi-coding-agent-test-history-isearch-end-keeps-on-accept ()
  "pi-coding-agent--history-isearch-end keeps history item when accepted."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (setq pi-coding-agent--history-isearch-active t)
    (setq pi-coding-agent--history-isearch-saved-input "original input")
    (erase-buffer)
    (insert "chosen history item")
    ;; Simulate isearch accept (quit is nil)
    (let ((isearch-mode-end-hook-quit nil))
      (pi-coding-agent--history-isearch-end))
    (should (equal (buffer-string) "chosen history item"))
    (should-not pi-coding-agent--history-isearch-active)))

(ert-deftest pi-coding-agent-test-history-isearch-goto-index ()
  "pi-coding-agent--history-isearch-goto loads history item into buffer."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (pi-coding-agent--history-add "first")
    (pi-coding-agent--history-add "second")
    (pi-coding-agent--history-add "third")
    (insert "current")
    (pi-coding-agent--history-isearch-goto 1)  ; "second" (0=third, 1=second)
    (should (equal (buffer-string) "second"))
    (should (= pi-coding-agent--history-isearch-index 1))))

(ert-deftest pi-coding-agent-test-history-isearch-hook-added ()
  "isearch-mode-hook is set up in pi-coding-agent-input-mode."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should (memq 'pi-coding-agent--history-isearch-setup isearch-mode-hook))))

(ert-deftest pi-coding-agent-test-history-isearch-goto-nil-restores-saved ()
  "pi-coding-agent--history-isearch-goto with nil index restores saved input."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (pi-coding-agent--history-add "history item")
    (setq pi-coding-agent--history-isearch-saved-input "my original input")
    (insert "something else")
    (pi-coding-agent--history-isearch-goto nil)
    (should (equal (buffer-string) "my original input"))
    (should (null pi-coding-agent--history-isearch-index))))

(ert-deftest pi-coding-agent-test-history-isearch-goto-empty-saved-input ()
  "pi-coding-agent--history-isearch-goto with nil index and empty saved input."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (pi-coding-agent--history-add "history item")
    (setq pi-coding-agent--history-isearch-saved-input "")
    (insert "something else")
    (pi-coding-agent--history-isearch-goto nil)
    (should (equal (buffer-string) ""))
    (should (null pi-coding-agent--history-isearch-index))))

;;; Input Buffer Slash Completion

(ert-deftest pi-coding-agent-test-command-capf-returns-nil-without-slash ()
  "Completion returns nil when not after slash."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "hello")
    (should-not (pi-coding-agent--command-capf))))

(ert-deftest pi-coding-agent-test-command-capf-returns-nil-at-line-start ()
  "Completion returns nil when point is at beginning of line."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "/test")
    (goto-char (line-beginning-position))
    (should-not (pi-coding-agent--command-capf))))

(ert-deftest pi-coding-agent-test-command-capf-returns-completion-data ()
  "Completion returns data when after slash at start of buffer."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (setq pi-coding-agent--commands '((:name "test-cmd" :description "Test")))
    (insert "/te")
    (let ((result (pi-coding-agent--command-capf)))
      (should result)
      (should (= (nth 0 result) 2))  ; Start after /
      (should (= (nth 1 result) 4))  ; End at point
      (should (member "test-cmd" (nth 2 result))))))

(ert-deftest pi-coding-agent-test-command-capf-ignores-slash-on-later-lines ()
  "Completion ignores / on lines after the first (pi only expands at buffer start)."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (setq pi-coding-agent--commands '((:name "test-cmd" :description "Test")))
    (insert "Some context:\n/te")
    (should-not (pi-coding-agent--command-capf))))



(ert-deftest pi-coding-agent-test-send-prompt-sends-literal ()
  "pi-coding-agent--send-prompt sends text literally (no expansion).
Pi handles command expansion on the server side."
  (let* ((rpc-message nil)
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'pi-coding-agent--get-process)
                   (lambda () fake-proc))
                  ((symbol-function 'pi-coding-agent--rpc-async)
                   (lambda (_proc msg _cb) (setq rpc-message msg))))
          (pi-coding-agent--send-prompt "/greet world")
          ;; Should send literal /greet world, NOT expanded
          (should (equal (plist-get rpc-message :message) "/greet world")))
      (delete-process fake-proc))))

(ert-deftest pi-coding-agent-test-format-session-stats ()
  "Format session stats returns readable string with cache details."
  (let ((stats '(:tokens (:input 50000 :output 10000 :total 60000
                         :cacheRead 123000 :cacheWrite 4567)
                 :cost 0.45
                 :userMessages 5
                 :toolCalls 12)))
    (let ((result (pi-coding-agent--format-session-stats stats)))
      (should (string-match-p "50,000" result))
      (should (string-match-p "10,000" result))
      (should (string-match-p "60,000" result))
      (should (string-match-p "123,000" result))
      (should (string-match-p "4,567" result))
      (should (string-match-p "\\$0.45" result))
      (should (string-match-p "Messages: 5" result))
      (should (string-match-p "Tools: 12" result)))))

(ert-deftest pi-coding-agent-test-header-line-shows-model ()
  "Header line displays current model."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model "claude-sonnet-4"))
    (let ((header (pi-coding-agent--header-line-string)))
      (should (string-match-p "sonnet-4" header)))))

(ert-deftest pi-coding-agent-test-header-line-shows-thinking ()
  "Header line displays thinking level."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model "claude-sonnet-4" :thinking-level "high"))
    (let ((header (pi-coding-agent--header-line-string)))
      (should (string-match-p "high" header)))))

(ert-deftest pi-coding-agent-test-header-line-shows-activity-phase ()
  "Header line shows the current activity phase label."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model "claude-sonnet-4" :thinking-level "high")
          pi-coding-agent--activity-phase "thinking")
    (let ((header (pi-coding-agent--header-line-string)))
      (should (string-match-p "thinking" header)))))

(ert-deftest pi-coding-agent-test-header-line-shows-idle ()
  "Header line shows idle activity phase with fixed-width padding."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model "claude-sonnet-4" :thinking-level "high")
          pi-coding-agent--activity-phase "idle")
    (let ((header (substring-no-properties (pi-coding-agent--header-line-string))))
      (should (string-match-p "idle    " header)))))

(ert-deftest pi-coding-agent-test-header-line-phase-is-padded ()
  "Header line activity phase slot is always 8 characters wide."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model "claude-sonnet-4" :thinking-level "high")
          pi-coding-agent--activity-phase "running")
    (let* ((header (substring-no-properties (pi-coding-agent--header-line-string)))
           (pos (string-match "running" header)))
      (should pos)
      (should (equal (substring header pos (+ pos 8)) "running ")))))

(ert-deftest pi-coding-agent-test-header-line-shows-thinking-activity-phase ()
  "Header line shows semantic activity label during streaming."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model "claude-sonnet-4")
          pi-coding-agent--activity-phase "thinking")
    (let ((header (pi-coding-agent--header-line-string)))
      (should (string-match-p "thinking" header)))))

(ert-deftest pi-coding-agent-test-abort-send-resets-activity-phase ()
  "Abort send resets activity phase and status to idle in CHAT-BUF."
  (let ((chat-buf (generate-new-buffer "*pi-coding-agent-chat:test-abort-send/*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--activity-phase "running"
                  pi-coding-agent--status 'streaming))
          ;; Simulate callback/sentinel context by calling from other buffer
          (with-temp-buffer
            (pi-coding-agent--abort-send chat-buf))
          (with-current-buffer chat-buf
            (should (equal pi-coding-agent--activity-phase "idle"))
            (should (eq pi-coding-agent--status 'idle))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest pi-coding-agent-test-working-message-in-header ()
  "Header line includes transient working message when set."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model "claude-sonnet-4")
          pi-coding-agent--working-message "📖 Skimming…")
    (let ((header (substring-no-properties (pi-coding-agent--header-line-string))))
      (should (string-match-p "Skimming" header)))))

(ert-deftest pi-coding-agent-test-header-no-pipes-when-minimal ()
  "Header has no pipe separators when only identity group is present."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model "claude-sonnet-4")
          pi-coding-agent--cached-stats nil
          pi-coding-agent--session-name nil
          pi-coding-agent--extension-status nil
          pi-coding-agent--working-message nil)
    (let ((header (substring-no-properties (pi-coding-agent--header-line-string))))
      (should-not (string-match-p "│" header)))))

(ert-deftest pi-coding-agent-test-header-pipes-collapse-correctly ()
  "Header renders only needed pipes when stats and context groups are set."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model (:name "claude-sonnet-4" :contextWindow 200000))
          pi-coding-agent--cached-stats '(:tokens (:input 1000 :output 500 :cacheRead 0 :cacheWrite 0)
                                   :cost 0.05)
          pi-coding-agent--last-usage '(:input 100 :output 50 :cacheRead 0 :cacheWrite 0)
          pi-coding-agent--session-name "My Session"
          pi-coding-agent--extension-status nil
          pi-coding-agent--working-message nil)
    (let ((header (substring-no-properties (pi-coding-agent--header-line-string)))
          (count 0)
          (start 0))
      (while (string-match "│" header start)
        (setq count (1+ count)
              start (match-end 0)))
      (should (= count 2)))))

(ert-deftest pi-coding-agent-test-header-all-groups-present ()
  "Header shows three group separators when all groups have content."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model (:name "claude-sonnet-4" :contextWindow 200000))
          pi-coding-agent--cached-stats '(:tokens (:input 1000 :output 500 :cacheRead 0 :cacheWrite 0)
                                   :cost 0.05)
          pi-coding-agent--last-usage '(:input 100 :output 50 :cacheRead 0 :cacheWrite 0)
          pi-coding-agent--session-name "My Session"
          pi-coding-agent--extension-status '(("ext" . "Git: synced"))
          pi-coding-agent--working-message "📖 Skimming…")
    (let ((header (substring-no-properties (pi-coding-agent--header-line-string)))
          (count 0)
          (start 0))
      (while (string-match "│" header start)
        (setq count (1+ count)
              start (match-end 0)))
      (should (= count 3))
      (should (string-match-p "My Session" header))
      (should (string-match-p "Git: synced · 📖 Skimming…" header)))))

(ert-deftest pi-coding-agent-test-header-session-name-in-context-group ()
  "Context group shows session name when set, collapses when nil."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model "claude-sonnet-4")
          pi-coding-agent--session-name "Refactor auth")
    (let ((header (substring-no-properties (pi-coding-agent--header-line-string))))
      (should (string-match-p "Refactor auth" header))
      (should (string-match-p "│" header)))
    (setq pi-coding-agent--session-name nil)
    (let ((header (substring-no-properties (pi-coding-agent--header-line-string))))
      (should-not (string-match-p "│" header)))))

(ert-deftest pi-coding-agent-test-format-tokens-compact ()
  "Tokens formatted compactly."
  (should (equal "500" (pi-coding-agent--format-tokens-compact 500)))
  (should (equal "5k" (pi-coding-agent--format-tokens-compact 5000)))
  (should (equal "50k" (pi-coding-agent--format-tokens-compact 50000)))
  (should (equal "1.2M" (pi-coding-agent--format-tokens-compact 1200000))))

(ert-deftest pi-coding-agent-test-input-mode-has-header-line ()
  "Input mode sets up header-line-format."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should header-line-format)))

(ert-deftest pi-coding-agent-test-header-line-handles-model-plist ()
  "Header line handles model as plist with :name."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model (:name "claude-sonnet-4" :id "model-123")))
    (let ((header (pi-coding-agent--header-line-string)))
      (should (string-match-p "sonnet-4" header)))))

(ert-deftest pi-coding-agent-test-menu-model-description-buffer-local ()
  "Menu model description uses buffer-local model."
  (let ((buf-a (generate-new-buffer "*pi-coding-agent-chat:model-a*"))
        (buf-b (generate-new-buffer "*pi-coding-agent-chat:model-b*")))
    (unwind-protect
        (let (desc-a desc-b)
          (with-current-buffer buf-a
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--state '(:model (:name "Alpha")))
            (setq desc-a (pi-coding-agent--menu-model-description)))
          (with-current-buffer buf-b
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--state '(:model (:name "Beta")))
            (setq desc-b (pi-coding-agent--menu-model-description)))
          (should (equal (list desc-a desc-b)
                         '("Model: Alpha" "Model: Beta"))))
      (mapc #'kill-buffer (list buf-a buf-b)))))

(ert-deftest pi-coding-agent-test-select-model-updates-current-session-only ()
  "Selecting a model updates only the current session."
  (let* ((buf-a (generate-new-buffer "*pi-coding-agent-chat:model-select-a*"))
         (buf-b (generate-new-buffer "*pi-coding-agent-chat:model-select-b*"))
         (available-models (list (list :id "model-a" :name "Model A" :provider "test")
                                 (list :id "model-b" :name "Model B" :provider "test")))
         (selected-model (list :id "model-b" :name "Model B" :provider "test")))
    (unwind-protect
        (cl-letf (((symbol-function 'pi-coding-agent--rpc-sync)
                   (lambda (&rest _) (list :success t :data (list :models available-models))))
                  ((symbol-function 'pi-coding-agent--rpc-async)
                   (lambda (_proc _cmd callback)
                     (funcall callback (list :success t :command "set_model" :data selected-model))))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "Model B")))
          (with-current-buffer buf-a
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--process :proc-a)
            (setq pi-coding-agent--state '(:model (:name "Model A" :id "model-a"))))
          (with-current-buffer buf-b
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--process :proc-b)
            (setq pi-coding-agent--state '(:model (:name "Model B-old" :id "model-b-old"))))
          (with-current-buffer buf-a
            (pi-coding-agent-select-model))
          (let ((model-a (with-current-buffer buf-a
                           (plist-get (plist-get pi-coding-agent--state :model) :name)))
                (model-b (with-current-buffer buf-b
                           (plist-get (plist-get pi-coding-agent--state :model) :name))))
            (should (equal (list model-a model-b)
                           '("Model B" "Model B-old")))))
      (mapc (lambda (buf)
              (with-current-buffer buf
                (setq pi-coding-agent--process nil))
              (kill-buffer buf))
            (list buf-a buf-b)))))

(ert-deftest pi-coding-agent-test-update-state-refreshes-header ()
  "Updating state should trigger header-line refresh."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model (:name "old-model") :thinking-level "low"))
    (let ((header-before (pi-coding-agent--header-line-string)))
      ;; Simulate state update
      (setq pi-coding-agent--state '(:model (:name "new-model") :thinking-level "high"))
      (let ((header-after (pi-coding-agent--header-line-string)))
        ;; Header string should reflect new state
        (should (string-match-p "new-model" header-after))
        (should (string-match-p "high" header-after))))))

(ert-deftest pi-coding-agent-test-apply-state-response-updates-buffer ()
  "Apply state response updates buffer-local variables in correct buffer."
  (let ((chat-buf (generate-new-buffer "*test-apply-state*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (pi-coding-agent-chat-mode)
            (setq pi-coding-agent--state nil
                  pi-coding-agent--status nil))
          ;; Call from a different buffer to verify buffer context handling
          (with-temp-buffer
            (pi-coding-agent--apply-state-response
             chat-buf
             '(:success t :data (:isStreaming :false
                                 :sessionFile "/tmp/test.jsonl"
                                 :model "test-model"))))
          ;; Verify state was updated in chat-buf, not temp buffer
          (with-current-buffer chat-buf
            (should (eq pi-coding-agent--status 'idle))
            (should (equal (plist-get pi-coding-agent--state :session-file)
                           "/tmp/test.jsonl"))))
      (kill-buffer chat-buf))))

(ert-deftest pi-coding-agent-test-apply-state-response-handles-dead-buffer ()
  "Apply state response handles dead buffer gracefully."
  (let ((chat-buf (generate-new-buffer "*test-dead-buf*")))
    (kill-buffer chat-buf)
    ;; Should not error when buffer is dead
    (pi-coding-agent--apply-state-response
     chat-buf
     '(:success t :data (:sessionFile "/tmp/test.jsonl")))))

(ert-deftest pi-coding-agent-test-header-line-model-is-clickable ()
  "Model name in header-line has click properties."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model (:name "claude-sonnet-4")))
    (let ((header (pi-coding-agent--header-line-string)))
      ;; Should have local-map property
      (should (get-text-property 0 'local-map header))
      ;; Should have mouse-face for highlight
      (should (get-text-property 0 'mouse-face header)))))

(ert-deftest pi-coding-agent-test-header-line-thinking-is-clickable ()
  "Thinking level in header-line has click properties."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--state '(:model (:name "test") :thinking-level "high"))
    (let* ((header (pi-coding-agent--header-line-string))
           ;; Find position of "high" in header
           (pos (string-match "high" header)))
      (should pos)
      ;; Should have local-map at that position
      (should (get-text-property pos 'local-map header))
      ;; Should have mouse-face for highlight
      (should (get-text-property pos 'mouse-face header)))))

(ert-deftest pi-coding-agent-test-header-format-context-returns-nil-when-no-window ()
  "Context format returns nil when context window is 0."
  (should (null (pi-coding-agent--header-format-context 1000 0))))

(ert-deftest pi-coding-agent-test-header-format-context-shows-percentage ()
  "Context format shows percentage and window size."
  (let ((result (pi-coding-agent--header-format-context 50000 200000)))
    (should (string-match-p "25.0%%" result))
    (should (string-match-p "200k" result))))

(ert-deftest pi-coding-agent-test-header-format-context-shows-unknown-when-tokens-missing ()
  "Context format shows unknown usage when token count is unavailable."
  (let ((result (pi-coding-agent--header-format-context nil 200000)))
    (should (string-match-p "\\?/200k" result))))

(ert-deftest pi-coding-agent-test-header-format-context-warning-over-70 ()
  "Context format uses warning face over 70%."
  (let ((result (pi-coding-agent--header-format-context 150000 200000)))
    (should (eq (get-text-property 0 'face result) 'warning))))

(ert-deftest pi-coding-agent-test-header-format-context-error-over-90 ()
  "Context format uses error face over 90%."
  (let ((result (pi-coding-agent--header-format-context 190000 200000)))
    (should (eq (get-text-property 0 'face result) 'error))))

(ert-deftest pi-coding-agent-test-message-end-updates-usage-for-normal-completion ()
  "message_end with stopReason=stop updates pi-coding-agent--last-usage."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--last-usage nil)
    (pi-coding-agent--handle-display-event
     '(:type "message_end"
       :message (:role "assistant"
                 :stopReason "stop"
                 :usage (:input 1000 :output 500 :cacheRead 200 :cacheWrite 50))))
    (should pi-coding-agent--last-usage)
    (should (equal (plist-get pi-coding-agent--last-usage :input) 1000))))

(ert-deftest pi-coding-agent-test-message-end-refreshes-header-for-assistant ()
  "Assistant message_end refreshes header stats for fresher cost updates."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((refresh-count 0))
      (cl-letf (((symbol-function 'pi-coding-agent--refresh-header)
                 (lambda () (setq refresh-count (1+ refresh-count)))))
        (pi-coding-agent--handle-display-event
         '(:type "message_end"
           :message (:role "assistant"
                     :stopReason "stop"
                     :usage (:input 100 :output 50 :cacheRead 10 :cacheWrite 5)))))
      (should (= refresh-count 1)))))

(ert-deftest pi-coding-agent-test-message-end-does-not-refresh-header-for-user ()
  "User message_end does not trigger header stats refresh."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (let ((refresh-count 0))
      (cl-letf (((symbol-function 'pi-coding-agent--refresh-header)
                 (lambda () (setq refresh-count (1+ refresh-count)))))
        (pi-coding-agent--handle-display-event
         '(:type "message_end"
           :message (:role "user" :content "hello"))))
      (should (= refresh-count 0)))))

(ert-deftest pi-coding-agent-test-message-end-updates-usage-for-tool-use ()
  "message_end with stopReason=toolUse updates pi-coding-agent--last-usage."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--last-usage nil)
    (pi-coding-agent--handle-display-event
     '(:type "message_end"
       :message (:role "assistant"
                 :stopReason "toolUse"
                 :usage (:input 2000 :output 300 :cacheRead 100 :cacheWrite 25))))
    (should pi-coding-agent--last-usage)
    (should (equal (plist-get pi-coding-agent--last-usage :input) 2000))))

(ert-deftest pi-coding-agent-test-message-end-skips-usage-for-aborted ()
  "message_end with stopReason=aborted does NOT update pi-coding-agent--last-usage.
This preserves the previous valid usage for context percentage display."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Set up previous valid usage
    (setq pi-coding-agent--last-usage '(:input 5000 :output 1000 :cacheRead 500 :cacheWrite 100))
    ;; Process an aborted message with zeroed/incomplete usage
    (pi-coding-agent--handle-display-event
     '(:type "message_end"
       :message (:role "assistant"
                 :stopReason "aborted"
                 :usage (:input 0 :output 0 :cacheRead 0 :cacheWrite 0))))
    ;; Should preserve previous usage, not overwrite with zeros
    (should (equal (plist-get pi-coding-agent--last-usage :input) 5000))))

(ert-deftest pi-coding-agent-test-message-end-updates-usage-for-error ()
  "message_end with stopReason=error updates pi-coding-agent--last-usage.
Errors still consume context, so their usage data is valid for display."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (setq pi-coding-agent--last-usage nil)
    (pi-coding-agent--handle-display-event
     '(:type "message_end"
       :message (:role "assistant"
                 :stopReason "error"
                 :usage (:input 3000 :output 100 :cacheRead 400 :cacheWrite 0))))
    (should pi-coding-agent--last-usage)
    (should (equal (plist-get pi-coding-agent--last-usage :input) 3000))))

(ert-deftest pi-coding-agent-test-header-format-stats-returns-nil-when-no-stats ()
  "Stats format returns nil when stats is nil."
  (should (null (pi-coding-agent--header-format-stats nil nil nil))))

(ert-deftest pi-coding-agent-test-header-format-stats-shows-cost-and-context ()
  "Header stats format shows only cost and context usage."
  (let* ((stats '(:tokens (:input 1000 :output 500 :cacheRead 2000 :cacheWrite 100)
                  :cost 0.05))
         (last-usage '(:input 3000 :output 100 :cacheRead 400 :cacheWrite 0))
         (model-obj '(:contextWindow 200000))
         (result (pi-coding-agent--header-format-stats stats last-usage model-obj)))
    (should (string-match-p "\\$0.05" result))
    (should (string-match-p "1.8%%/200k" result))
    (should-not (string-match-p "↑" result))
    (should-not (string-match-p "↓" result))
    (should-not (string-match-p "R" result))
    (should-not (string-match-p "W" result))))

(ert-deftest pi-coding-agent-test-header-format-stats-shows-unknown-context-without-usage ()
  "Header stats show unknown context when no last assistant usage is available."
  (let* ((stats '(:tokens (:input 1000 :output 500 :cacheRead 2000 :cacheWrite 100)
                  :cost 0.05))
         (model-obj '(:contextWindow 200000))
         (result (pi-coding-agent--header-format-stats stats nil model-obj)))
    (should (string-match-p "\\$0.05" result))
    (should (string-match-p "\\?/200k" result))))

;;; File Reference Completion (@)

(ert-deftest pi-coding-agent-test-at-trigger-context ()
  "@ completion should only trigger at word boundaries, not in emails."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    ;; @ at start of buffer - should trigger
    (erase-buffer)
    (insert "@")
    (should (pi-coding-agent--at-trigger-p))
    ;; @ after space - should trigger
    (erase-buffer)
    (insert "hello @")
    (should (pi-coding-agent--at-trigger-p))
    ;; @ after newline - should trigger
    (erase-buffer)
    (insert "hello\n@")
    (should (pi-coding-agent--at-trigger-p))
    ;; @ after punctuation - should trigger
    (erase-buffer)
    (insert "see:@")
    (should (pi-coding-agent--at-trigger-p))
    ;; @ after alphanumeric (email) - should NOT trigger
    (erase-buffer)
    (insert "user@")
    (should-not (pi-coding-agent--at-trigger-p))
    ;; @ in middle of email - should NOT trigger
    (erase-buffer)
    (insert "test123@")
    (should-not (pi-coding-agent--at-trigger-p))))

(ert-deftest pi-coding-agent-test-file-reference-capf-returns-nil-without-at ()
  "File reference completion returns nil when not after @."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "hello world")
    (should-not (pi-coding-agent--file-reference-capf))))

(ert-deftest pi-coding-agent-test-file-reference-capf-returns-data-after-at ()
  "File reference completion returns data when point is after @."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    ;; Mock project files
    (setq pi-coding-agent--project-files-cache '("file1.el" "file2.py" "dir/file3.ts"))
    (setq pi-coding-agent--project-files-cache-time (float-time))
    (insert "Check @fi")
    (let ((result (pi-coding-agent--file-reference-capf)))
      (should result)
      ;; Start should be after @
      (should (= (nth 0 result) (- (point) 2)))  ; Position after @
      ;; End should be at point
      (should (= (nth 1 result) (point)))
      ;; Candidates should include matching files
      (should (member "file1.el" (nth 2 result)))
      (should (member "file2.py" (nth 2 result))))))

(ert-deftest pi-coding-agent-test-file-reference-capf-empty-prefix ()
  "File reference completion returns all files when no prefix after @."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (setq pi-coding-agent--project-files-cache '("a.el" "b.py" "c.ts"))
    (setq pi-coding-agent--project-files-cache-time (float-time))
    (insert "See @")
    (let ((result (pi-coding-agent--file-reference-capf)))
      (should result)
      ;; Should return all files when prefix is empty
      (should (= (length (nth 2 result)) 3)))))

(ert-deftest pi-coding-agent-test-file-reference-capf-mid-line ()
  "File reference completion works in the middle of a line."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (setq pi-coding-agent--project-files-cache '("test.el"))
    (setq pi-coding-agent--project-files-cache-time (float-time))
    (insert "Look at @te and also")
    (goto-char 11)  ; Position right after "@te"
    (let ((result (pi-coding-agent--file-reference-capf)))
      (should result)
      (should (member "test.el" (nth 2 result))))))

;;; Path Completion (Tab)

(ert-deftest pi-coding-agent-test-path-capf-returns-nil-for-non-path ()
  "Path completion returns nil for text that doesn't look like a path."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "hello world")
    (should-not (pi-coding-agent--path-capf))))

(ert-deftest pi-coding-agent-test-path-capf-returns-nil-for-non-prefixed-path ()
  "Path completion returns nil for paths without ./ ../ ~/ or / prefix."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "src/file.el")
    (should-not (pi-coding-agent--path-capf))))

(ert-deftest pi-coding-agent-test-path-capf-triggers-for-dot-slash ()
  "Path completion triggers for paths starting with ./"
  (let* ((temp-dir (make-temp-file "pi-coding-agent-path-test-" t))
         (test-file (expand-file-name "test.txt" temp-dir)))
    (unwind-protect
        (progn
          (with-temp-file test-file (insert "test"))
          (let ((default-directory temp-dir))
            (with-temp-buffer
              (pi-coding-agent-input-mode)
              (setq pi-coding-agent--chat-buffer (current-buffer))
              ;; Mock session directory
              (cl-letf (((symbol-function 'pi-coding-agent--session-directory)
                         (lambda () temp-dir)))
                (insert "./te")
                (let ((result (pi-coding-agent--path-capf)))
                  (should result)
                  ;; Should have candidates
                  (should (> (length (nth 2 result)) 0)))))))
      (delete-directory temp-dir t))))

(ert-deftest pi-coding-agent-test-path-capf-triggers-for-tilde ()
  "Path completion triggers for paths starting with ~/"
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "~/")
    ;; Just verify it doesn't error and returns something
    ;; (actual completions depend on user's home directory)
    (let ((result (pi-coding-agent--path-capf)))
      ;; May return nil if ~ directory doesn't exist or has no completions
      ;; but should not error
      (should (or (null result) (listp result))))))

(ert-deftest pi-coding-agent-test-path-capf-triggers-for-absolute ()
  "Path completion triggers for absolute paths not at buffer start."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "see /tmp/")
    (let ((result (pi-coding-agent--path-capf)))
      (when result
        (should (listp (nth 2 result)))))))

(ert-deftest pi-coding-agent-test-path-completions-excludes-dot-entries ()
  "Path completions should not include ./ or ../ entries."
  (let* ((temp-dir (make-temp-file "pi-coding-agent-path-test-" t))
         (subdir (expand-file-name "subdir" temp-dir)))
    (unwind-protect
        (progn
          (make-directory subdir)
          (cl-letf (((symbol-function 'pi-coding-agent--session-directory)
                     (lambda () temp-dir)))
            (let ((completions (pi-coding-agent--path-completions "./")))
              ;; Should have the subdir
              (should (member "./subdir/" completions))
              ;; Should NOT have ./ or ../
              (should-not (member "./" completions))
              (should-not (member "./../" completions))
              (should-not (member "././" completions)))))
      (delete-directory temp-dir t))))

(ert-deftest pi-coding-agent-test-complete-command-exists ()
  "pi-coding-agent-complete should be an interactive command."
  (should (commandp 'pi-coding-agent-complete)))

(ert-deftest pi-coding-agent-test-path-capf-skips-slash-at-buffer-start ()
  "Path completion skips / at buffer start to allow slash commands."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "/tmp")
    (should-not (pi-coding-agent--path-capf))))

(ert-deftest pi-coding-agent-test-path-capf-allows-slash-on-later-lines ()
  "Path completion works for / on lines after the first."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (insert "Check this file:\n/tmp/")
    (let ((result (pi-coding-agent--path-capf)))
      (when result
        (should (listp (nth 2 result)))))))

(ert-deftest pi-coding-agent-test-tool-start-creates-overlay ()
  "tool_execution_start creates an overlay with pending face."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    ;; Should have an overlay with pi-coding-agent-tool-block property
    (goto-char (point-min))
    (let* ((overlays (overlays-at (point)))
           (tool-ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block)) overlays)))
      (should tool-ov)
      (should (eq (overlay-get tool-ov 'face) 'pi-coding-agent-tool-block))
      (should (equal (overlay-get tool-ov 'pi-coding-agent-tool-name) "bash")))))

(ert-deftest pi-coding-agent-test-tool-start-header-format ()
  "tool_execution_start uses simple header format, not drawer syntax."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls -la"))
    ;; Should have "$ ls -la" header
    (should (string-match-p "\\$ ls -la" (buffer-string)))
    ;; Should NOT have drawer syntax
    (should-not (string-match-p ":BASH:" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-end-keeps-overlay-face ()
  "tool_execution_end keeps base face on success."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    ;; Initially base face
    (let ((ov (car (overlays-at (point-min)))))
      (should (eq (overlay-get ov 'face) 'pi-coding-agent-tool-block)))
    ;; After success — face stays the same
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file.txt"))
                          nil nil)
    (let ((ov (car (overlays-at (point-min)))))
      (should (eq (overlay-get ov 'face) 'pi-coding-agent-tool-block)))))

(ert-deftest pi-coding-agent-test-tool-end-error-face ()
  "tool_execution_end sets error face on failure."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "bad"))
    (pi-coding-agent--display-tool-end "bash" '(:command "bad")
                          '((:type "text" :text "error"))
                          nil t)  ; is-error = t
    (let ((ov (car (overlays-at (point-min)))))
      (should (eq (overlay-get ov 'face) 'pi-coding-agent-tool-block-error)))))

(ert-deftest pi-coding-agent-test-tool-end-no-drawer-syntax ()
  "tool_execution_end does not insert :END: marker."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-tool-start "bash" '(:command "ls"))
    (pi-coding-agent--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "output"))
                          nil nil)
    (should-not (string-match-p ":END:" (buffer-string)))))

(ert-deftest pi-coding-agent-test-tool-overlay-does-not-extend-to-subsequent-content ()
  "Tool overlay should not extend when content is inserted after tool block.
Regression test: overlay with rear-advance was extending to subsequent content."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Create a complete tool block
    (pi-coding-agent--display-tool-start "write" '(:path "/tmp/test.txt" :content "hello"))
    (pi-coding-agent--display-tool-end "write" '(:path "/tmp/test.txt" :content "hello")
                          '((:type "text" :text "Written to /tmp/test.txt"))
                          nil nil)
    ;; Simulate inserting more content after the tool (like next message)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "AFTER_TOOL_CONTENT\n"))
    ;; The new content should NOT be inside any tool overlay
    (let* ((new-content-pos (- (point-max) 10))  ; somewhere in AFTER_TOOL_CONTENT
           (overlays (overlays-at new-content-pos))
           (tool-overlay (seq-find
                          (lambda (ov) (overlay-get ov 'pi-coding-agent-tool-block))
                          overlays)))
      (should-not tool-overlay))))

(ert-deftest pi-coding-agent-test-abort-mid-tool-cleans-up-overlay ()
  "Aborting mid-tool should clean up the pending overlay.
When abort happens during tool execution, tool_execution_end never arrives.
display-agent-end must finalize the pending overlay with error face."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    ;; Start a tool (creates pending overlay)
    (pi-coding-agent--display-tool-start "bash" '(:command "sleep 100"))
    ;; Verify overlay is pending
    (should pi-coding-agent--pending-tool-overlay)
    (should (eq (overlay-get pi-coding-agent--pending-tool-overlay 'face)
                'pi-coding-agent-tool-block))
    ;; Simulate abort - display-agent-end is called WITHOUT tool-end
    (setq pi-coding-agent--aborted t)
    (pi-coding-agent--display-agent-end)
    ;; Pending overlay variable should be nil
    (should-not pi-coding-agent--pending-tool-overlay)
    ;; But there should still be a finalized overlay with error face
    (goto-char (point-min))
    (let* ((overlays (overlays-at (point)))
           (tool-ov (seq-find (lambda (o) (overlay-get o 'pi-coding-agent-tool-block)) overlays)))
      (should tool-ov)
      (should (eq (overlay-get tool-ov 'face) 'pi-coding-agent-tool-block-error)))
    ;; Content inserted after should NOT be inside the overlay
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "AFTER_ABORT_CONTENT\n"))
    (let* ((new-content-pos (- (point-max) 10))
           (overlays (overlays-at new-content-pos))
           (tool-overlay (seq-find
                          (lambda (ov) (overlay-get ov 'pi-coding-agent-tool-block))
                          overlays)))
      (should-not tool-overlay))))

(ert-deftest pi-coding-agent-test-delta-no-transform-inside-code-block ()
  "Hash inside fenced code block should NOT be transformed."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "```python\n# This is a comment\n```")
    ;; The # inside code block should stay as single #
    (should (string-match-p "^# This is a comment$" (buffer-string)))))

(ert-deftest pi-coding-agent-test-delta-transform-resumes-after-code-block ()
  "Headings after code block closes should be transformed."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "```\n# comment\n```\n# Real Heading")
    ;; Inside block: stays #
    (should (string-match-p "^# comment$" (buffer-string)))
    ;; After block: becomes ##
    (should (string-match-p "^## Real Heading" (buffer-string)))))

(ert-deftest pi-coding-agent-test-delta-code-fence-split-across-deltas ()
  "Code fence split across deltas still detected."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "``")
    (pi-coding-agent--display-message-delta "`python\n# comment\n```")
    ;; Should recognize the split ``` and not transform inside
    (should (string-match-p "^# comment$" (buffer-string)))))

(ert-deftest pi-coding-agent-test-delta-backticks-mid-line-not-fence ()
  "Backticks mid-line don't trigger code block state."
  (with-temp-buffer
    (pi-coding-agent-chat-mode)
    (pi-coding-agent--display-agent-start)
    (pi-coding-agent--display-message-delta "Use ```code``` inline\n# Heading")
    ;; Inline backticks shouldn't affect heading transform
    (should (string-match-p "^## Heading" (buffer-string)))))

;;; Input Mode — Markdown Highlighting (opt-in)

(ert-deftest pi-coding-agent-test-input-mode-gfm-when-enabled ()
  "With markdown highlighting enabled, input mode has GFM font-lock."
  (with-temp-buffer
    (let ((pi-coding-agent-input-markdown-highlighting t))
      (pi-coding-agent-input-mode)
      (should (derived-mode-p 'pi-coding-agent-input-mode))
      (insert "some **bold** text")
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "bold")
      (should (memq 'markdown-bold-face
                    (let ((f (get-text-property (1- (point)) 'face)))
                      (if (listp f) f (list f))))))))

(ert-deftest pi-coding-agent-test-input-mode-no-metadata-face ()
  "With markdown highlighting, lines ending with colon have no metadata face."
  (with-temp-buffer
    (let ((pi-coding-agent-input-markdown-highlighting t))
      (pi-coding-agent-input-mode)
      (insert "Fix the bug:\n- item\n")
      (font-lock-ensure)
      (goto-char (point-min))
      (let ((f (get-text-property (point) 'face)))
        (should-not (memq 'markdown-metadata-key-face
                          (if (listp f) f (list f))))))))

(ert-deftest pi-coding-agent-test-input-mode-no-hidden-markup ()
  "Input mode does NOT hide markup, even when user customizes it globally."
  (with-temp-buffer
    (let ((pi-coding-agent-input-markdown-highlighting t)
          (markdown-hide-markup t))
      (pi-coding-agent-input-mode)
      (should-not markdown-hide-markup))))

(ert-deftest pi-coding-agent-test-input-mode-no-fontification-without-gfm ()
  "Without markdown highlighting, bold text gets no markdown face."
  (with-temp-buffer
    (let ((pi-coding-agent-input-markdown-highlighting nil))
      (pi-coding-agent-input-mode)
      (insert "some **bold** text")
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "bold")
      (should-not (memq 'markdown-bold-face
                        (let ((f (get-text-property (1- (point)) 'face)))
                          (if (listp f) f (list f))))))))

(ert-deftest pi-coding-agent-test-input-mode-keybindings ()
  "Pi input keybindings are active in input mode."
  (with-temp-buffer
    (pi-coding-agent-input-mode)
    (should (eq (key-binding (kbd "C-c C-c")) 'pi-coding-agent-send))
    (should (eq (key-binding (kbd "C-c C-k")) 'pi-coding-agent-abort))
    (should (eq (key-binding (kbd "C-c C-p")) 'pi-coding-agent-menu))
    (should (eq (key-binding (kbd "M-p")) 'pi-coding-agent-previous-input))
    (should (eq (key-binding (kbd "M-n")) 'pi-coding-agent-next-input))
    (should (eq (key-binding (kbd "TAB")) 'pi-coding-agent-complete))
    (should (eq (key-binding (kbd "C-c C-s")) 'pi-coding-agent-queue-steering))))

(provide 'pi-coding-agent-input-test)
;;; pi-coding-agent-input-test.el ends here
