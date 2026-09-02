;;; piem-input-test.el --- Tests for piem-input -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>

;;; Commentary:

;; Tests for input history, history isearch, send/abort commands,
;; message queuing, file reference completion, path completion,
;; and slash command completion — the input buffer layer.

;;; Code:

(require 'ert)
(require 'piem)
(require 'piem-test-common)

;;; Sending Prompts

(ert-deftest piem-test-send-extracts-text ()
  "piem-send extracts text from input buffer and clears it."
  (let ((sent-text nil))
    (piem-test-with-mock-session "/tmp/piem-test-send1/"
      (cl-letf (((symbol-function 'piem--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (setq sent-text text)
                   (when on-success (funcall on-success)))))
        (with-current-buffer "*piem-input:/tmp/piem-test-send1/*"
          (insert "Hello, pi!")
          (piem-send)
          (should (equal sent-text "Hello, pi!"))
          (should (string-empty-p (buffer-string))))))))

(ert-deftest piem-test-send-empty-is-noop ()
  "piem-send with empty buffer does nothing."
  (let ((send-called nil))
    (piem-test-with-mock-session "/tmp/piem-test-send2/"
      (cl-letf (((symbol-function 'piem--send-prompt)
                 (lambda (_) (setq send-called t))))
        (with-current-buffer "*piem-input:/tmp/piem-test-send2/*"
          (piem-send)
          (should-not send-called))))))

(ert-deftest piem-test-send-whitespace-only-is-noop ()
  "piem-send with only whitespace does nothing."
  (let ((send-called nil))
    (piem-test-with-mock-session "/tmp/piem-test-send3/"
      (cl-letf (((symbol-function 'piem--send-prompt)
                 (lambda (_) (setq send-called t))))
        (with-current-buffer "*piem-input:/tmp/piem-test-send3/*"
          (insert "   \n\t  ")
          (piem-send)
          (should-not send-called))))))

(ert-deftest piem-test-send-queues-locally-while-streaming ()
  "piem-send adds to local queue while streaming, no RPC sent."
  (let ((chat-buf (get-buffer-create "*piem-test-queue-stream*"))
        (input-buf (get-buffer-create "*piem-test-queue-stream-input*"))
        (rpc-called nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming)
            (setq piem--input-buffer input-buf)
            (setq piem--followup-queue nil))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "My message")
            (cl-letf (((symbol-function 'piem--rpc-async)
                       (lambda (_proc _cmd _cb) (setq rpc-called t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "queued" (downcase fmt)))
                           (setq message-shown t)))))
              (piem-send))
            ;; Should NOT have called RPC (local queue instead)
            (should-not rpc-called)
            ;; Should have added to local queue in chat buffer
            (with-current-buffer chat-buf
              (should (equal piem--followup-queue '("My message"))))
            ;; Should have shown queued message
            (should message-shown)
            ;; Input should be cleared (message accepted)
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-send-queues-locally-while-sending ()
  "piem-send adds to local queue while waiting for agent_start."
  (let ((chat-buf (get-buffer-create "*piem-test-queue-sending*"))
        (input-buf (get-buffer-create "*piem-test-queue-sending-input*"))
        (rpc-called nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'sending)
            (setq piem--input-buffer input-buf)
            (setq piem--followup-queue nil))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "Queued while retry is starting")
            (cl-letf (((symbol-function 'piem--rpc-async)
                       (lambda (_proc _cmd _cb) (setq rpc-called t)))
                      ((symbol-function 'message) #'ignore))
              (piem-send))
            (should-not rpc-called)
            (with-current-buffer chat-buf
              (should (equal piem--followup-queue
                             '("Queued while retry is starting"))))
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-send-refuses-during-session-transition ()
  "A prompt typed while switching sessions is not queued into either session."
  (let ((chat-buf (get-buffer-create "*piem-test-send-transition*"))
        (input-buf (get-buffer-create "*piem-test-send-transition-input*"))
        (shown-message nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle
                  piem--input-buffer input-buf
                  piem--followup-queue nil)
            (piem--begin-session-transition 'mock-proc))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "not yet")
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq shown-message (apply #'format fmt args)))))
              (piem-send))
            (should (equal (buffer-string) "not yet")))
          (with-current-buffer chat-buf
            (should (null piem--followup-queue)))
          (should (equal shown-message
                         "Pi: Cannot send while session is switching")))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-send-refuses-builtin-command-while-busy ()
  "Client-side slash commands are not hidden in the follow-up queue."
  (let ((chat-buf (get-buffer-create "*piem-test-builtin-busy*"))
        (input-buf (get-buffer-create "*piem-test-builtin-busy-input*"))
        (shown-message nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming
                  piem--input-buffer input-buf
                  piem--followup-queue nil))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "/new")
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq shown-message (apply #'format fmt args)))))
              (piem-send))
            (should (equal (buffer-string) "/new")))
          (with-current-buffer chat-buf
            (should (null piem--followup-queue)))
          (should (equal shown-message "Pi: Cannot queue /new while Pi is busy")))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-send-queues-locally-while-compacting ()
  "piem-send adds to local queue while compacting, no RPC sent."
  (let ((chat-buf (get-buffer-create "*piem-test-queue-compact*"))
        (input-buf (get-buffer-create "*piem-test-queue-compact-input*"))
        (rpc-called nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'compacting)
            (setq piem--input-buffer input-buf)
            (setq piem--followup-queue nil))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "My message during compaction")
            (cl-letf (((symbol-function 'piem--rpc-async)
                       (lambda (_proc _cmd _cb) (setq rpc-called t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "queued" (downcase fmt)))
                           (setq message-shown t)))))
              (piem-send))
            ;; Should NOT have called RPC (local queue instead)
            (should-not rpc-called)
            ;; Should have added to local queue in chat buffer
            (with-current-buffer chat-buf
              (should (equal piem--followup-queue '("My message during compaction"))))
            ;; Should have shown queued message
            (should message-shown)
            ;; Input should be cleared (message accepted)
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-slash-compact-handled-locally-not-sent-as-prompt ()
  "/compact in input buffer invokes piem-compact locally, not sent to pi."
  (let ((chat-buf (get-buffer-create "*piem-test-slash-compact*"))
        (input-buf (get-buffer-create "*piem-test-slash-compact-input*"))
        (compact-called nil)
        (compact-args nil)
        (prompt-sent nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle)
            (setq piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "/compact")
            (cl-letf (((symbol-function 'piem-compact)
                       (lambda (&optional args) (setq compact-called t compact-args args)))
                      ((symbol-function 'piem--send-prompt)
                       (lambda (_) (setq prompt-sent t))))
              (piem-send))
            ;; Should have called compact function with no args
            (should compact-called)
            (should (null compact-args))
            ;; Should NOT have sent as prompt
            (should-not prompt-sent)
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-slash-compact-with-args-passes-instructions ()
  "/compact with args passes custom instructions to compact function."
  (let ((chat-buf (get-buffer-create "*piem-test-slash-compact-args*"))
        (input-buf (get-buffer-create "*piem-test-slash-compact-args-input*"))
        (compact-called nil)
        (compact-args nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle)
            (setq piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "/compact focus on the API design decisions")
            (cl-letf (((symbol-function 'piem-compact)
                       (lambda (&optional args) (setq compact-called t compact-args args))))
              (piem-send))
            ;; Should have called compact function with custom instructions
            (should compact-called)
            (should (equal compact-args "focus on the API design decisions"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-compaction-success-without-retry-sends-queued-message ()
  "Successful non-retry compaction schedules the oldest queued follow-up.
Uses :false (JSON false representation) to verify boolean normalization."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((sent-text nil)
          drain-callback
          drain-args)
      (setq piem--status 'compacting)
      (setq piem--followup-queue '("queued message"))
      (cl-letf (((symbol-function 'piem--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (setq sent-text text)
                   (when on-success (funcall on-success))))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest args)
                   (setq drain-callback fn
                         drain-args args)
                   'fake-drain-timer)))
        (piem--handle-display-event
         '(:type "compaction_end"
           :reason "threshold"
           :aborted :false
           :willRetry :false
           :result (:tokensBefore 1000 :summary "Summary" :timestamp 1234567890000)))
        (should (eq piem--status 'idle))
        (should (functionp drain-callback))
        (should (equal piem--followup-queue '("queued message")))
        (should (null sent-text))
        (apply drain-callback drain-args))
      ;; Queue should be empty after processing.
      (should (null piem--followup-queue))
      ;; The queued message should have been sent.
      (should (equal sent-text "queued message")))))

(ert-deftest piem-test-compaction-success-without-retry-preserves-fifo ()
  "Successful non-retry compaction schedules one follow-up in FIFO order."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((sent-text nil)
          drain-callback
          drain-args)
      (setq piem--status 'compacting)
      (dolist (text '("First" "Second" "Third"))
        (piem--push-followup text))
      (cl-letf (((symbol-function 'piem--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (setq sent-text text)
                   (when on-success (funcall on-success))))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest args)
                   (setq drain-callback fn
                         drain-args args)
                   'fake-drain-timer)))
        (piem--handle-display-event
         '(:type "compaction_end"
           :reason "manual"
           :aborted :false
           :willRetry :false
           :result (:tokensBefore 1000 :summary "Summary")))
        (should (equal piem--followup-queue '("Third" "Second" "First")))
        (should (null sent-text))
        (apply drain-callback drain-args))
      (should (equal sent-text "First"))
      (should (equal piem--followup-queue '("Third" "Second"))))))

(ert-deftest piem-test-overflow-compaction-will-retry-preserves-followup-queue ()
  "Overflow compaction with automatic retry keeps local follow-ups queued."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((sent-text nil))
      (setq piem--status 'compacting)
      (setq piem--followup-queue '("queued behind retry"))
      (cl-letf (((symbol-function 'piem--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (setq sent-text text)
                   (when on-success (funcall on-success)))))
        (piem--handle-display-event
         '(:type "compaction_end"
           :reason "overflow"
           :aborted :false
           :willRetry t
           :result (:tokensBefore 1000 :summary "Summary"))))
      (should (eq piem--status 'sending))
      (should (equal piem--followup-queue '("queued behind retry")))
      (should (null sent-text)))))

(ert-deftest piem-test-overflow-compaction-retry-drains-queue-after-agent-end ()
  "Queued follow-ups wait for Pi's automatic retry turn to finish."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((sent-prompts nil)
          drain-callback
          drain-args)
      (setq piem--status 'compacting)
      (setq piem--followup-queue '("queued behind retry"))
      (cl-letf (((symbol-function 'piem--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (push text sent-prompts)
                   (when on-success (funcall on-success))))
                ((symbol-function 'piem--refresh-header) #'ignore)
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest args)
                   (setq drain-callback fn
                         drain-args args)
                   'fake-drain-timer)))
        (piem--handle-display-event
         '(:type "compaction_end"
           :reason "overflow"
           :aborted :false
           :willRetry t
           :result (:tokensBefore 1000 :summary "Summary")))
        (should (null sent-prompts))
        (should (equal piem--followup-queue '("queued behind retry")))
        (should (null drain-callback))
        (piem--handle-display-event '(:type "agent_start"))
        (should (eq piem--status 'streaming))
        (piem--handle-display-event '(:type "agent_end" :messages []))
        (should (functionp drain-callback))
        (apply drain-callback drain-args))
      (should (equal (reverse sent-prompts) '("queued behind retry")))
      (should (null piem--followup-queue)))))

(ert-deftest piem-test-compaction-end-before-agent-start-defers-followup-drain ()
  "Queued follow-ups do not overtake Pi work that starts after compaction_end."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((sent-prompts nil)
          first-drain-callback
          first-drain-args
          second-drain-callback
          second-drain-args)
      (setq piem--status 'compacting
            piem--followup-queue '("local follow-up after Pi queue"))
      (cl-letf (((symbol-function 'piem--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (push text sent-prompts)
                   (when on-success (funcall on-success))))
                ((symbol-function 'piem--refresh-header) #'ignore)
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest args)
                   (if first-drain-callback
                       (setq second-drain-callback fn
                             second-drain-args args)
                     (setq first-drain-callback fn
                           first-drain-args args))
                   'fake-drain-timer)))
        ;; Pi can emit compaction_end and then immediately begin work it already
        ;; owns.  Local follow-ups must wait for that visible agent turn.
        (piem--handle-display-event
         '(:type "compaction_end"
           :reason "threshold"
           :aborted :false
           :willRetry :false
           :result (:tokensBefore 1000 :summary "Summary")))
        (should (eq piem--status 'idle))
        (should (functionp first-drain-callback))
        (should (null sent-prompts))
        (should (equal piem--followup-queue
                       '("local follow-up after Pi queue")))
        (piem--handle-display-event '(:type "agent_start"))
        ;; A stale scheduled drain may still fire, but streaming status makes it
        ;; a no-op.
        (apply first-drain-callback first-drain-args)
        (should (null sent-prompts))
        (should (equal piem--followup-queue
                       '("local follow-up after Pi queue")))
        (piem--handle-display-event '(:type "agent_end" :messages []))
        (should (functionp second-drain-callback))
        (apply second-drain-callback second-drain-args))
      (should (equal (reverse sent-prompts)
                     '("local follow-up after Pi queue")))
      (should (null piem--followup-queue)))))

(ert-deftest piem-test-agent-end-before-compaction-defers-followup-drain ()
  "Queued follow-ups do not overtake compaction that starts after agent_end."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((sent-prompts nil)
          drain-callback
          drain-args)
      (setq piem--status 'streaming
            piem--followup-queue '("queued until compaction settles"))
      (cl-letf (((symbol-function 'piem--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (push text sent-prompts)
                   (when on-success (funcall on-success))))
                ((symbol-function 'piem--refresh-header) #'ignore)
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest args)
                   (setq drain-callback fn
                         drain-args args)
                   'fake-drain-timer)))
        ;; Pi emits agent_end before post-run compaction_start.  The local
        ;; queue must wait long enough for that compaction event to claim the
        ;; next ordering slot.
        (piem--handle-display-event '(:type "agent_end" :messages []))
        (should (eq piem--status 'idle))
        (should (functionp drain-callback))
        (should (null sent-prompts))
        (should (equal piem--followup-queue
                       '("queued until compaction settles")))
        (piem--handle-display-event '(:type "compaction_start" :reason "threshold"))
        ;; A stale scheduled drain may still fire, but compacting status makes
        ;; it a no-op.
        (apply drain-callback drain-args)
        (should (null sent-prompts))
        (should (equal piem--followup-queue
                       '("queued until compaction settles")))
        (setq drain-callback nil
              drain-args nil)
        (piem--handle-display-event
         '(:type "compaction_end"
           :reason "threshold"
           :aborted :false
           :willRetry :false
           :result (:tokensBefore 1000 :summary "Summary")))
        (should (functionp drain-callback))
        (apply drain-callback drain-args))
      (should (equal (reverse sent-prompts)
                     '("queued until compaction settles")))
      (should (null piem--followup-queue)))))

(ert-deftest piem-test-preflight-compaction-keeps-followups-behind-prompt ()
  "Compaction during prompt preflight must not drain local follow-ups."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((sent-prompts nil)
          drain-callback)
      (setq piem--status 'sending
            piem--pre-compaction-status nil
            piem--followup-queue '("queued behind original prompt"))
      (cl-letf (((symbol-function 'piem--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (push text sent-prompts)
                   (when on-success (funcall on-success))))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest _args)
                   (setq drain-callback fn)
                   'fake-drain-timer)))
        (piem--handle-display-event
         '(:type "compaction_start" :reason "threshold"))
        (piem--handle-display-event
         '(:type "compaction_end"
           :reason "threshold"
           :aborted :false
           :willRetry :false
           :result (:tokensBefore 1000 :summary "Summary")))
        (should (eq piem--status 'sending))
        (should (null drain-callback))
        (should (null sent-prompts))
        (should (equal piem--followup-queue
                       '("queued behind original prompt")))))))

(ert-deftest piem-test-failed-preflight-compaction-restores-followups-before-prompt-failure ()
  "Failed preflight compaction restores follow-ups while prompt failure is pending."
  (let ((chat-buf (get-buffer-create "*piem-test-preflight-compaction-failure-chat*"))
        (input-buf (get-buffer-create "*piem-test-preflight-compaction-failure-input*"))
        (sent-text nil))
    (unwind-protect
        (progn
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf))
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (let ((generation (piem--begin-prompt-start-wait)))
              (setq piem--status 'sending
                    piem--input-buffer input-buf
                    piem--pre-compaction-status nil
                    piem--followup-queue '("follow-up after prompt"))
              (cl-letf (((symbol-function 'piem--send-prompt)
                         (lambda (text &optional on-success &rest _)
                           (setq sent-text text)
                           (when on-success (funcall on-success))))
                        ((symbol-function 'message) #'ignore))
                (piem--handle-display-event
                 '(:type "compaction_start" :reason "threshold"))
                (piem--handle-display-event
                 '(:type "compaction_end"
                   :reason "threshold"
                   :aborted :false
                   :willRetry :false
                   :result :null
                   :errorMessage "quota exceeded")))
              (should (eq piem--status 'idle))
              (should (equal piem--activity-phase "thinking"))
              (should (piem--prompt-start-current-p generation))
              (should (null piem--followup-queue))
              (should (null sent-text))))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "follow-up after prompt"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-aborted-preflight-compaction-clears-followups-before-prompt-failure ()
  "Aborted preflight compaction clears follow-ups while prompt failure is pending."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((sent-text nil))
      (piem--begin-prompt-start-wait)
      (setq piem--status 'sending
            piem--pre-compaction-status nil
            piem--followup-queue '("discarded follow-up"))
      (cl-letf (((symbol-function 'piem--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (setq sent-text text)
                   (when on-success (funcall on-success))))
                ((symbol-function 'message) #'ignore))
        (piem--handle-display-event
         '(:type "compaction_start" :reason "threshold"))
        (piem--handle-display-event
         '(:type "compaction_end"
           :reason "threshold"
           :aborted t
           :willRetry :false
           :result nil)))
      (should (eq piem--status 'idle))
      (should (equal piem--activity-phase "thinking"))
      (should (null piem--followup-queue))
      (should (null sent-text)))))

(ert-deftest piem-test-failed-preflight-compaction-prompt-failure-restores-original ()
  "Prompt RPC failure clears preflight wait after compaction failure."
  (let ((chat-buf (get-buffer-create "*piem-test-preflight-prompt-failure-chat*"))
        (input-buf (get-buffer-create "*piem-test-preflight-prompt-failure-input*"))
        (rpc-callback nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle
                  piem--activity-phase "idle"
                  piem--input-buffer input-buf
                  piem--followup-queue '("queued follow-up")))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "original prompt"))
          (cl-letf (((symbol-function 'piem--get-process)
                     (lambda () 'mock-proc))
                    ((symbol-function 'process-live-p) (lambda (_) t))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc _cmd callback)
                       (setq rpc-callback callback)))
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer input-buf
              (piem-send))
            (with-current-buffer chat-buf
              (piem--handle-display-event
               '(:type "compaction_start" :reason "threshold"))
              (piem--handle-display-event
               '(:type "compaction_end"
                 :reason "threshold"
                 :aborted :false
                 :willRetry :false
                 :result :null
                 :errorMessage "quota exceeded"))
              (should (piem--session-busy-p chat-buf))
              (funcall rpc-callback '(:success :false :error "quota exceeded"))
              (should (eq piem--status 'idle))
              (should (equal piem--activity-phase "idle"))
              (should-not (piem--session-busy-p chat-buf))))
          (with-current-buffer input-buf
            (should (equal (buffer-string)
                           "original prompt\n\nqueued follow-up"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-agent-end-will-retry-preserves-followup-queue ()
  "agent_end with willRetry keeps local follow-ups behind Pi's retry."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((sent-prompts nil)
          drain-callback
          drain-args)
      (setq piem--status 'streaming
            piem--followup-queue '("queued behind transient retry"))
      (cl-letf (((symbol-function 'piem--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (push text sent-prompts)
                   (when on-success (funcall on-success))))
                ((symbol-function 'piem--refresh-header) #'ignore)
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat fn &rest args)
                   (setq drain-callback fn
                         drain-args args)
                   'fake-drain-timer)))
        (piem--handle-display-event
         '(:type "agent_end" :messages [] :willRetry t))
        (should (eq piem--status 'sending))
        (should (null drain-callback))
        (should (null sent-prompts))
        (should (equal piem--followup-queue
                       '("queued behind transient retry")))
        (piem--handle-display-event '(:type "agent_start"))
        (piem--handle-display-event '(:type "agent_end" :messages []))
        (should (functionp drain-callback))
        (apply drain-callback drain-args))
      (should (equal (reverse sent-prompts)
                     '("queued behind transient retry")))
      (should (null piem--followup-queue)))))

(ert-deftest piem-test-compaction-failure-restores-queued-followups ()
  "Failed compaction surfaces queued follow-ups for user recovery."
  (let ((chat-buf (get-buffer-create "*piem-test-compaction-failure-chat*"))
        (input-buf (get-buffer-create "*piem-test-compaction-failure-input*"))
        (sent-text nil))
    (unwind-protect
        (progn
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf))
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'compacting
                  piem--input-buffer input-buf
                  piem--followup-queue '("recover me later"))
            (cl-letf (((symbol-function 'piem--send-prompt)
                       (lambda (text &optional on-success &rest _)
                         (setq sent-text text)
                         (when on-success (funcall on-success))))
                      ((symbol-function 'message) #'ignore))
              (piem--handle-display-event
               '(:type "compaction_end"
                 :reason "threshold"
                 :aborted :false
                 :willRetry :false
                 :result :null
                 :errorMessage "quota exceeded")))
            (should (eq piem--status 'idle))
            (should (null piem--followup-queue))
            (should (null sent-text)))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "recover me later"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-auto-retry-failure-restores-queued-followups ()
  "Final auto-retry failure surfaces queued follow-ups for user recovery."
  (let ((chat-buf (get-buffer-create "*piem-test-retry-failure-chat*"))
        (input-buf (get-buffer-create "*piem-test-retry-failure-input*")))
    (unwind-protect
        (progn
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf))
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'sending
                  piem--activity-phase "thinking"
                  piem--input-buffer input-buf
                  piem--followup-queue '("queued behind failed retry"))
            (piem--handle-display-event
             '(:type "auto_retry_end"
               :success :false
               :attempt 3
               :finalError "overloaded"))
            (should (eq piem--status 'idle))
            (should (equal piem--activity-phase "idle"))
            (should (null piem--followup-queue))
            (should (string-match-p "Retry failed" (buffer-string))))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "queued behind failed retry"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-compaction-end-aborted-clears-queue ()
  "compaction_end when aborted clears followup queue without sending."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((sent-text nil))
      (setq piem--status 'compacting)
      (setq piem--followup-queue '("queued message"))
      (cl-letf (((symbol-function 'piem--send-prompt)
                 (lambda (text &optional on-success &rest _)
                   (setq sent-text text)
                   (when on-success (funcall on-success)))))
        ;; Simulate compaction_end event (aborted)
        (piem--handle-display-event
         '(:type "compaction_end"
           :reason "threshold"
           :aborted t)))
      ;; Queue should be cleared (user cancelled)
      (should (null piem--followup-queue))
      ;; No message should have been sent
      (should (null sent-text)))))

;;; Abort Command

(ert-deftest piem-test-abort-sends-command ()
  "piem-abort sends abort command while streaming."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((sent-command nil)
          (piem--status 'streaming))
      (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                ((symbol-function 'piem--get-chat-buffer) (lambda () (current-buffer)))
                ((symbol-function 'piem--rpc-async)
                 (lambda (_proc cmd _cb) (setq sent-command cmd))))
        (piem-abort)
        (should (equal (plist-get sent-command :type) "abort"))
        (should piem--aborted)))))

(ert-deftest piem-test-abort-sends-command-while-compacting ()
  "piem-abort sends abort command while compacting."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((sent-command nil)
          (piem--status 'compacting))
      (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                ((symbol-function 'piem--get-chat-buffer) (lambda () (current-buffer)))
                ((symbol-function 'piem--rpc-async)
                 (lambda (_proc cmd _cb) (setq sent-command cmd))))
        (piem-abort)
        (should (equal (plist-get sent-command :type) "abort"))
        (should-not piem--aborted)))))

(ert-deftest piem-test-abort-noop-when-idle ()
  "piem-abort does nothing when idle."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((sent-command nil)
          (piem--status 'idle))
      (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                ((symbol-function 'piem--get-chat-buffer) (lambda () (current-buffer)))
                ((symbol-function 'piem--rpc-async)
                 (lambda (_proc cmd _cb) (setq sent-command cmd))))
        (piem-abort)
        (should (null sent-command))))))

(ert-deftest piem-test-abort-clears-followup-queue ()
  "Aborting clears the follow-up queue so queued messages are not sent.
When user aborts, they want to stop everything - including queued messages."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((inhibit-read-only t)
          (message-was-sent nil))
      (insert "Some streaming content")
      ;; Set up state as if we're streaming with a queued message
      (setq piem--aborted t
            piem--followup-queue '("queued message that should be discarded"))
      ;; Mock send functions to detect if queue processing sends the message
      (cl-letf (((symbol-function 'piem--prepare-and-send)
                 (lambda (_text) (setq message-was-sent t)))
                ((symbol-function 'piem--refresh-header) #'ignore))
        ;; Simulate agent_end arriving after abort
        (piem--display-agent-end)
        ;; Queue should be empty (either cleared or not processed)
        (should (null piem--followup-queue))
        ;; Key assertion: queued message should NOT have been sent
        (should-not message-was-sent)))))

;;; Kill Buffer Protection

(ert-deftest piem-test-handler-removed-on-kill ()
  "Event handler is removed when chat buffer is killed."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((fake-proc (start-process "test" nil "true")))
      (unwind-protect
          (progn
            (setq piem--process fake-proc)
            (piem--register-display-handler fake-proc)
            (should (process-get fake-proc 'piem-display-handler))
            (should (process-get fake-proc 'piem-exit-handler))
            (piem--cleanup-on-kill)
            (should-not (process-get fake-proc 'piem-display-handler))
            (should-not (process-get fake-proc 'piem-exit-handler)))
        (when (process-live-p fake-proc)
          (delete-process fake-proc))))))

;;; Message Queuing

(ert-deftest piem-test-queue-steering-when-streaming-sends-steer ()
  "Queue steering sends steer RPC command when agent is streaming."
  (let ((chat-buf (get-buffer-create "*piem-test-queue-steer*"))
        (input-buf (get-buffer-create "*piem-test-queue-steer-input*"))
        (sent-command nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming)
            (setq piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "Please stop and focus on X")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--rpc-async)
                       (lambda (_proc cmd _cb) (setq sent-command cmd))))
              (piem-queue-steering))
            ;; Should send steer command
            (should sent-command)
            (should (equal (plist-get sent-command :type) "steer"))
            (should (equal (plist-get sent-command :message) "Please stop and focus on X"))
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-queue-steering-when-sending-sends-steer ()
  "Queue steering sends steer RPC while waiting for agent_start."
  (let ((chat-buf (get-buffer-create "*piem-test-queue-steer-sending*"))
        (input-buf (get-buffer-create "*piem-test-queue-steer-sending-input*"))
        (sent-command nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'sending)
            (setq piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "Steer the pending retry")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--rpc-async)
                       (lambda (_proc cmd _cb) (setq sent-command cmd))))
              (piem-queue-steering))
            (should sent-command)
            (should (equal (plist-get sent-command :type) "steer"))
            (should (equal (plist-get sent-command :message) "Steer the pending retry"))
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-queue-steering-refuses-during-session-transition ()
  "Steering must not bypass the session-transition send guard."
  (let ((chat-buf (get-buffer-create "*piem-test-steer-transition*"))
        (input-buf (get-buffer-create "*piem-test-steer-transition-input*"))
        (steer-sent nil)
        (shown-message nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming
                  piem--input-buffer input-buf
                  piem--followup-queue nil)
            (piem--begin-session-transition 'mock-proc))
          (with-current-buffer input-buf
            (piem-input-mode)
            (piem--set-chat-buffer chat-buf)
            (insert "do not steer during switch")
            (cl-letf (((symbol-function 'piem--send-steer-message)
                       (lambda (_text)
                         (setq steer-sent t)
                         t))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq shown-message (apply #'format fmt args)))))
              (piem-queue-steering))
            (should-not steer-sent)
            (should (equal (buffer-string) "do not steer during switch")))
          (with-current-buffer chat-buf
            (should (null piem--followup-queue)))
          (should (equal shown-message
                         "Pi: Cannot send steering while session is switching")))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-queue-steering-send-failure-preserves-input ()
  "Steering send failures keep input text for retry and avoid success feedback."
  (let ((chat-buf (get-buffer-create "*piem-test-queue-steer-fail*"))
        (input-buf (get-buffer-create "*piem-test-queue-steer-fail-input*"))
        (success-message nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming)
            (setq piem--input-buffer input-buf)
            (setq piem--followup-queue nil))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "Retry this steer")
            (cl-letf (((symbol-function 'piem--send-steer-message)
                       (lambda (_text) nil))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "steering message sent" (downcase fmt)))
                           (setq success-message t)))))
              (piem-queue-steering))
            ;; Failed send should keep input so user can retry.
            (should (equal (buffer-string) "Retry this steer"))
            ;; Should not claim success.
            (should-not success-message)
            ;; Failed send should not enqueue a normal follow-up.
            (with-current-buffer chat-buf
              (should (null piem--followup-queue)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-queue-steering-while-compacting-queues-locally ()
  "Queue steering during compaction should queue locally instead of sending steer now."
  (let ((chat-buf (get-buffer-create "*piem-test-queue-steer-compacting*"))
        (input-buf (get-buffer-create "*piem-test-queue-steer-compacting-input*"))
        (steer-sent nil)
        (shown-message nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'compacting)
            (setq piem--input-buffer input-buf)
            (setq piem--followup-queue nil))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "Steer during compaction")
            (cl-letf (((symbol-function 'piem--send-steer-message)
                       (lambda (_text)
                         (setq steer-sent t)
                         t))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq shown-message (apply #'format fmt args)))))
              (piem-queue-steering))
            ;; Should NOT send steer immediately
            (should-not steer-sent)
            ;; Should queue in local follow-up queue
            (with-current-buffer chat-buf
              (should (equal piem--followup-queue '("Steer during compaction"))))
            ;; Should tell user it was queued without over-promising timing.
            (should (equal shown-message "Pi: Steering queued (will send when Pi is ready)"))
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-steering-and-send-during-compaction-preserve-fifo ()
  "Steering and normal sends queued during compaction keep FIFO order."
  (let ((chat-buf (get-buffer-create "*piem-test-compaction-fifo*"))
        (input-buf (get-buffer-create "*piem-test-compaction-fifo-input*"))
        (sent-prompts nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'compacting)
            (setq piem--input-buffer input-buf)
            (setq piem--followup-queue nil))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (cl-letf (((symbol-function 'message) #'ignore))
              (insert "Steering first")
              (piem-queue-steering)
              (insert "Normal send second")
              (piem-send)))
          (with-current-buffer chat-buf
            (should (equal piem--followup-queue
                           '("Normal send second" "Steering first")))
            (let (drain-callback
                  drain-args)
              (cl-letf (((symbol-function 'piem--send-prompt)
                         (lambda (text &optional on-success &rest _)
                           (push text sent-prompts)
                           (when on-success (funcall on-success))))
                        ((symbol-function 'piem--refresh-header) #'ignore)
                        ((symbol-function 'run-at-time)
                         (lambda (_secs _repeat fn &rest args)
                           (setq drain-callback fn
                                 drain-args args)
                           'fake-drain-timer)))
                (piem--handle-display-event
                 '(:type "compaction_end"
                   :reason "threshold"
                   :aborted :false
                   :willRetry :false
                   :result (:tokensBefore 1000 :summary "Summary")))
                (should (functionp drain-callback))
                (apply drain-callback drain-args)
                (setq drain-callback nil
                      drain-args nil)
                (piem--handle-display-event '(:type "agent_end" :messages []))
                (should (functionp drain-callback))
                (apply drain-callback drain-args)))
            (should (equal (reverse sent-prompts)
                           '("Steering first" "Normal send second")))
            (should (null piem--followup-queue))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-queued-builtin-command-restores-input-without-running ()
  "Stale queued built-in commands are surfaced instead of run from a timer."
  (let ((chat-buf (get-buffer-create "*piem-test-queued-builtin-chat*"))
        (input-buf (get-buffer-create "*piem-test-queued-builtin-input*"))
        (new-session-called nil)
        (shown-message nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle
                  piem--input-buffer input-buf
                  piem--followup-queue '("/new")))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf))
          (with-current-buffer chat-buf
            (cl-letf (((symbol-function 'piem-new-session)
                       (lambda () (setq new-session-called t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq shown-message (apply #'format fmt args)))))
              (piem--process-followup-queue))
            (should-not new-session-called)
            (should (null piem--followup-queue)))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "/new")))
          (should (equal shown-message
                         "Pi: Cannot run queued /new command automatically")))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-queue-followup-uses-local-queue ()
  "Queue follow-up adds to local queue, no RPC sent."
  (let ((chat-buf (get-buffer-create "*piem-test-queue-followup*"))
        (input-buf (get-buffer-create "*piem-test-queue-followup-input*"))
        (rpc-called nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming)
            (setq piem--input-buffer input-buf)
            (setq piem--followup-queue nil))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "After you're done, also do Y")
            (cl-letf (((symbol-function 'piem--rpc-async)
                       (lambda (_proc _cmd _cb) (setq rpc-called t))))
              (piem-queue-followup))
            ;; Should NOT call RPC
            (should-not rpc-called)
            ;; Should add to local queue
            (with-current-buffer chat-buf
              (should (member "After you're done, also do Y" piem--followup-queue)))
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-queue-steering-when-idle-refuses ()
  "Queue steering refuses when agent is idle (nothing to interrupt)."
  (let ((chat-buf (get-buffer-create "*piem-test-queue-steer-idle*"))
        (input-buf (get-buffer-create "*piem-test-queue-steer-idle-input*"))
        (sent-anything nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle)
            (setq piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "Do something")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--send-prompt)
                       (lambda (_) (setq sent-anything t)))
                      ((symbol-function 'piem--rpc-async)
                       (lambda (_proc _cmd _cb) (setq sent-anything t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "nothing\\|idle\\|C-c C-c" (downcase fmt)))
                           (setq message-shown t)))))
              (piem-queue-steering))
            ;; Should NOT send anything
            (should-not sent-anything)
            ;; Should show message about using C-c C-c instead
            (should message-shown)
            ;; Input should be preserved (not accepted)
            (should (equal (buffer-string) "Do something"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-queue-followup-when-idle-sends-prompt ()
  "Queue follow-up sends as normal prompt when agent is idle."
  (let ((chat-buf (get-buffer-create "*piem-test-queue-followup-idle*"))
        (input-buf (get-buffer-create "*piem-test-queue-followup-idle-input*"))
        (sent-prompt nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle)
            (setq piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "Do something else")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--send-prompt)
                       (lambda (text &optional on-success &rest _)
                         (setq sent-prompt text)
                         (when on-success (funcall on-success)))))
              (piem-queue-followup))
            ;; Should send as normal prompt
            (should (equal sent-prompt "Do something else"))
            ;; Input should be cleared
            (should (string-empty-p (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-queue-steering-adds-to-history ()
  "Queue steering adds input to history."
  (let ((chat-buf (get-buffer-create "*piem-test-queue-hist*"))
        (input-buf (get-buffer-create "*piem-test-queue-hist-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming)
            (setq piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (setq piem--input-ring (make-ring 10))
            (insert "History test message")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--rpc-async) #'ignore))
              (piem-queue-steering))
            ;; Should be in history
            (should (not (ring-empty-p piem--input-ring)))
            (should (equal (ring-ref piem--input-ring 0) "History test message"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-queue-empty-input-does-nothing ()
  "Queue with empty input does nothing."
  (let ((chat-buf (get-buffer-create "*piem-test-queue-empty*"))
        (input-buf (get-buffer-create "*piem-test-queue-empty-input*"))
        (command-sent nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming)
            (setq piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            ;; Empty input (just whitespace)
            (insert "   \n  ")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--rpc-async)
                       (lambda (_proc _cmd _cb) (setq command-sent t))))
              (piem-queue-steering))
            ;; Should not send anything
            (should-not command-sent)))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-steering-shows-minibuffer-message ()
  "Steering shows feedback in minibuffer but is NOT displayed locally.
Unlike normal sends, steering waits for pi's echo to display at the
correct position in the conversation."
  (let ((chat-buf (get-buffer-create "*piem-test-queue-display*"))
        (input-buf (get-buffer-create "*piem-test-queue-display-input*"))
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming)
            (setq piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "My steering message")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--rpc-async) #'ignore)
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "steering\\|sent" (downcase fmt)))
                           (setq message-shown t)))))
              (piem-queue-steering)))
          ;; Should show minibuffer message
          (should message-shown)
          ;; Steering is NOT displayed locally - will be displayed when pi echoes it back
          (with-current-buffer chat-buf
            (should-not (string-match-p "My steering message" (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-input-mode-has-queue-keybindings ()
  "Input mode has C-c C-s for steering (C-c C-c handles follow-up)."
  (with-temp-buffer
    (piem-input-mode)
    (should (eq (key-binding (kbd "C-c C-s")) 'piem-queue-steering))
    ;; C-c C-c handles follow-up when streaming (no separate C-c C-q)
    (should (eq (key-binding (kbd "C-c C-c")) 'piem-send))))

(ert-deftest piem-test-queue-handles-rpc-error ()
  "Queue handles RPC error response by showing message to user."
  (let ((chat-buf (get-buffer-create "*piem-test-queue-error*"))
        (input-buf (get-buffer-create "*piem-test-queue-error-input*"))
        (captured-callback nil)
        (error-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming)
            (setq piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "Test message")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--rpc-async)
                       (lambda (_proc _cmd cb) (setq captured-callback cb)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (when (and fmt (string-match-p "error\\|fail" (downcase fmt)))
                           (setq error-shown t)))))
              (piem-queue-steering)
              ;; Simulate error response from RPC
              (when captured-callback
                (funcall captured-callback '(:success :false :error "Queue limit reached")))))
          ;; Should have shown an error message
          (should error-shown))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-queue-with-dead-process-shows-error ()
  "Queue with dead process shows error message."
  (let ((chat-buf (get-buffer-create "*piem-test-queue-dead*"))
        (input-buf (get-buffer-create "*piem-test-queue-dead-input*"))
        (error-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming)
            (setq piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "Test message")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () nil))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (when (and fmt (string-match-p "process\\|unavailable\\|error" (downcase fmt)))
                           (setq error-shown t)))))
              (piem-queue-steering)))
          ;; Should have shown an error about unavailable process
          (should error-shown))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-send-when-idle-sends-literal-commands ()
  "C-c C-c when idle sends commands literally (pi expands)."
  (let ((chat-buf (get-buffer-create "*piem-test-send-slash*"))
        (input-buf (get-buffer-create "*piem-test-send-slash-input*"))
        (sent-prompt nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle)
            (setq piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "/greet world")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--send-prompt)
                       (lambda (text &optional on-success &rest _)
                         (setq sent-prompt text)
                         (when on-success (funcall on-success)))))
              (piem-send))
            ;; Should send literal command (pi handles expansion)
            (should (equal sent-prompt "/greet world"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

;; Note: piem-test-send-queues-locally-while-streaming covers this case

(ert-deftest piem-test-steering-when-idle-refuses ()
  "C-c C-s when idle shows message and does nothing."
  (let ((chat-buf (get-buffer-create "*piem-test-steer-idle*"))
        (input-buf (get-buffer-create "*piem-test-steer-idle-input*"))
        (send-called nil)
        (message-shown nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle)
            (setq piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "Steer message")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--send-prompt)
                       (lambda (_) (setq send-called t)))
                      ((symbol-function 'piem--rpc-async)
                       (lambda (_proc _cmd _cb) (setq send-called t)))
                      ((symbol-function 'message)
                       (lambda (fmt &rest _)
                         (when (and fmt (string-match-p "idle\\|nothing\\|use" (downcase fmt)))
                           (setq message-shown t)))))
              (piem-queue-steering))
            ;; Should NOT have sent anything
            (should-not send-called)
            ;; Should have shown a message
            (should message-shown)
            ;; Input should be preserved
            (should (equal (buffer-string) "Steer message"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-message-start-user-echo-ignored-when-displayed-locally ()
  "message_start role=user is ignored when we already displayed the same message locally.
Uses local-user-message to track what we displayed for comparison."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((piem--status 'streaming)
          (piem--state nil)
          ;; Simulate that we displayed this message locally (normal send)
          (piem--local-user-message "Same message")
          (initial-content (buffer-string)))
      ;; Simulate receiving message_start for a user message (pi echoing back same text)
      (piem--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Same message")]
                   :timestamp 1704067200000)))
      ;; Buffer should be unchanged - pi's echo matches local display, so skip
      (should (equal (buffer-string) initial-content))
      (should-not (string-match-p "Same message" (buffer-string)))
      ;; Variable should be cleared
      (should-not piem--local-user-message))))

(ert-deftest piem-test-message-start-user-displayed-when-different ()
  "message_start role=user IS displayed when pi's text differs from local.
+This handles slash command expansion: user types '/greet', pi sends 'Hello!'."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((piem--status 'streaming)
          (piem--state nil)
          (piem--local-user-message "/greet world")
          (initial-content (buffer-string)))
      (piem--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Hello world!")]
                   :timestamp 1704067200000)))
      ;; Should be displayed since text differs (expanded template)
      (should (string-match-p "Hello world!" (buffer-string)))
      (should-not piem--local-user-message))))

(ert-deftest piem-test-message-start-user-skipped-when-template-equals-command ()
  "Edge case: if template expands to exactly the command text, we skip display.
This is rare but possible - the local display is already correct."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((piem--status 'streaming)
          (piem--state nil)
          (piem--local-user-message "/echo hello")
          (initial-content (buffer-string)))
      (piem--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "/echo hello")]
                   :timestamp 1704067200000)))
      ;; Should NOT be displayed - text matches what we displayed locally
      (should (equal (buffer-string) initial-content))
      (should-not piem--local-user-message))))

(ert-deftest piem-test-message-start-user-displayed-when-not-local ()
  "message_start role=user IS displayed when local-user-message is nil (steering case).
Steering messages are not displayed locally - they're displayed from the echo."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((piem--status 'streaming)
          (piem--state nil)
          ;; Variable is nil - no locally displayed message pending
          (piem--local-user-message nil))
      ;; Simulate receiving message_start for a steering message
      (piem--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Steering message here")]
                   :timestamp 1704067200000)))
      ;; Should be displayed since local-user-message was nil
      (should (string-match-p "Steering message here" (buffer-string)))
      ;; Variable should still be nil
      (should-not piem--local-user-message))))

(ert-deftest piem-test-steering-display-not-interleaved ()
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
    (piem-chat-mode)
    (let ((piem--status 'streaming)
          (piem--state nil)
          (piem--local-user-message nil)
          (piem--assistant-header-shown nil))
      ;; Simulate initial prompt response - assistant starts streaming
      (piem--handle-display-event '(:type "agent_start"))
      (piem--handle-display-event
       '(:type "message_start" :message (:role "assistant")))
      ;; Stream some content
      (piem--handle-display-event
       '(:type "message_update"
         :assistantMessageEvent (:type "text_delta" :delta "Counting: 1, 2, 3, ")))
      (piem--handle-display-event
       '(:type "message_update"
         :assistantMessageEvent (:type "text_delta" :delta "4, 5, 6, ")))

      ;; Now user sends steering - this comes as message_start with role=user
      ;; (steering messages are displayed from pi's echo, not locally)
      (piem--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "STOP-MARKER")]
                   :timestamp 1704067200000)))

      ;; Assistant continues with new turn after steering
      (setq piem--assistant-header-shown nil)  ; Reset for new turn
      (piem--handle-display-event '(:type "agent_start"))
      (piem--handle-display-event
       '(:type "message_start" :message (:role "assistant")))
      (piem--handle-display-event
       '(:type "message_update"
         :assistantMessageEvent (:type "text_delta" :delta "OK, stopping.")))
      (piem--handle-display-event '(:type "agent_end"))

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

(ert-deftest piem-test-local-user-message-tracks-display ()
  "The local-user-message variable tracks locally displayed messages.
- Normal send stores the text
- message_start role=user clears it to nil
- Steering doesn't set it (displayed from echo)
- agent_end clears it to nil"
  (let ((chat-buf (get-buffer-create "*piem-test-echo-flag*"))
        (input-buf (get-buffer-create "*piem-test-echo-flag-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle)
            (setq piem--input-buffer input-buf)
            ;; Variable starts as nil
            (should-not piem--local-user-message))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "First message")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--send-prompt)
                       (lambda (_text &optional on-success &rest _)
                         (when on-success
                           (funcall on-success)))))
              (piem-send)))
          ;; After accepted normal send, variable should store the message text
          (with-current-buffer chat-buf
            (should (equal piem--local-user-message "First message"))
            ;; Simulate pi echo - variable clears to nil
            (piem--handle-display-event
             '(:type "message_start"
               :message (:role "user" :content [(:type "text" :text "First message")])))
            (should-not piem--local-user-message)
            ;; Now simulate steering (doesn't set it)
            (setq piem--status 'streaming))
          (with-current-buffer input-buf
            (erase-buffer)
            (insert "Steer this")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--rpc-async) #'ignore))
              (piem-queue-steering)))
          ;; Variable still nil (steering doesn't set it)
          (with-current-buffer chat-buf
            (should-not piem--local-user-message)
            ;; agent_end clears to nil (in case of edge cases)
            (setq piem--local-user-message "test")  ; Simulate weird state
            (piem--display-agent-end)
            (should-not piem--local-user-message)))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-normal-send-not-duplicated-by-message-start ()
  "Accepted normal sends should not be duplicated when message_start arrives.
When prompt preflight succeeds, we display the user text locally.  When pi
echoes it back via message_start, we should NOT display it again."
  (let ((chat-buf (get-buffer-create "*piem-test-no-dup*"))
        (input-buf (get-buffer-create "*piem-test-no-dup-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle)
            (setq piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "Hello pi")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--send-prompt)
                       (lambda (_text &optional on-success &rest _)
                         (when on-success
                           (funcall on-success)))))
              (piem-send)))
          ;; Now simulate pi echoing the message back via message_start
          (with-current-buffer chat-buf
            (piem--handle-display-event
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

(ert-deftest piem-test-agent-end-sends-queued-followup ()
  "agent_end schedules the queued follow-up after post-run events settle.
When user queues a follow-up (busy state), it goes to the local queue.
After an agent_end with no retry or compaction, the scheduled drain pops
from the queue and sends it as a normal prompt."
  (let ((chat-buf (get-buffer-create "*piem-test-agent-end-queue*"))
        (input-buf (get-buffer-create "*piem-test-agent-end-queue-input*"))
        (sent-prompt nil)
        drain-callback
        drain-args)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming)
            (setq piem--input-buffer input-buf)
            (setq piem--followup-queue nil)
            ;; Simulate some prior content
            (let ((inhibit-read-only t))
              (insert "Assistant\n=========\nSome response...\n")))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "My follow-up question")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t)))
              (piem-send)))  ; Adds to local queue when streaming
          ;; Message should be in queue, not in chat yet
          (with-current-buffer chat-buf
            (should (equal piem--followup-queue '("My follow-up question")))
            (should-not (string-match-p "My follow-up question" (buffer-string))))
          ;; Now simulate agent_end.  It should not drain synchronously, so an
          ;; immediately following compaction_start would still be able to
          ;; preserve ordering.
          (with-current-buffer chat-buf
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--send-prompt)
                       (lambda (text &optional on-success &rest _)
                         (setq sent-prompt text)
                         (when on-success (funcall on-success))))
                      ((symbol-function 'piem--refresh-header) #'ignore)
                      ((symbol-function 'run-at-time)
                       (lambda (_secs _repeat fn &rest args)
                         (setq drain-callback fn
                               drain-args args)
                         'fake-drain-timer)))
              (piem--handle-display-event '(:type "agent_end"))
              (should (functionp drain-callback))
              (should (equal piem--followup-queue '("My follow-up question")))
              (should (null sent-prompt))
              (apply drain-callback drain-args))
            ;; Queue should be empty now
            (should (null piem--followup-queue))
            ;; Should have sent the queued message
            (should (equal sent-prompt "My follow-up question"))
            ;; Message should now be displayed in chat
            (should (string-match-p "My follow-up question" (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-send-queues-while-followup-drain-pending ()
  "New input waits behind an older queued follow-up drain."
  (let ((chat-buf (get-buffer-create "*piem-test-drain-pending-send*"))
        (input-buf (get-buffer-create "*piem-test-drain-pending-send-input*"))
        (sent-prompt nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle
                  piem--input-buffer input-buf
                  piem--followup-queue '("older queued follow-up")
                  piem--followup-drain-timer 'fake-drain-timer))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "new prompt during drain window")
            (cl-letf (((symbol-function 'piem--send-prompt)
                       (lambda (text &optional on-success &rest _)
                         (setq sent-prompt text)
                         (when on-success (funcall on-success))))
                      ((symbol-function 'message) #'ignore))
              (piem-send))
            (should (string-empty-p (buffer-string))))
          (with-current-buffer chat-buf
            (should (null sent-prompt))
            (should (equal piem--followup-queue
                           '("new prompt during drain window"
                             "older queued follow-up")))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-send-queues-when-stale-state-arrives-before-agent-start ()
  "A stale idle get_state response must not open a second-send gap."
  (let ((chat-buf (get-buffer-create "*piem-test-stale-state-send-chat*"))
        (input-buf (get-buffer-create "*piem-test-stale-state-send-input*"))
        (sent-prompts nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle
                  piem--input-buffer input-buf
                  piem--state '(:session-id "same-session")))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf))
          (cl-letf (((symbol-function 'piem--get-process)
                     (lambda () 'mock-proc))
                    ((symbol-function 'process-live-p) (lambda (_) t))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd _callback)
                       (push (plist-get cmd :message) sent-prompts))))
            (with-current-buffer input-buf
              (insert "first prompt")
              (piem-send))
            (with-current-buffer chat-buf
              (piem--apply-state-response
               chat-buf
               '(:success t :data (:isStreaming :false
                                   :isCompacting :false
                                   :sessionId "same-session"
                                   :sessionFile "/tmp/same.jsonl"))))
            (with-current-buffer input-buf
              (insert "second prompt")
              (piem-send)))
          (with-current-buffer chat-buf
            (should (equal (reverse sent-prompts) '("first prompt")))
            (should (equal piem--followup-queue
                           '("second prompt")))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-process-followup-queue-waits-until-ready ()
  "Direct queue draining keeps its own safe-to-send guard."
  (dolist (case '((sending nil nil)
                  (streaming nil nil)
                  (compacting nil nil)
                  (idle "pending local echo" nil)
                  (idle nil prompt-wait)
                  (idle nil transition)))
    (with-temp-buffer
      (piem-chat-mode)
      (let ((sent-prompts nil))
        (setq piem--status (nth 0 case)
              piem--local-user-message (nth 1 case)
              piem--followup-queue '("queued follow-up"))
        (pcase (nth 2 case)
          ('prompt-wait
           (piem--begin-prompt-start-wait)
           (setq piem--status 'idle))
          ('transition
           (piem--begin-session-transition 'mock-proc)))
        (cl-letf (((symbol-function 'piem--send-prompt)
                   (lambda (text &optional on-success &rest _)
                     (push text sent-prompts)
                     (when on-success (funcall on-success)))))
          (piem--process-followup-queue))
        (should (null sent-prompts))
        (should (equal piem--followup-queue
                       '("queued follow-up")))))))

(ert-deftest piem-test-followup-queue-fifo-order ()
  "Multiple follow-ups are processed in FIFO order."
  (let ((chat-buf (get-buffer-create "*piem-test-fifo*"))
        (input-buf (get-buffer-create "*piem-test-fifo-input*"))
        (sent-prompts nil)
        drain-callback
        drain-args)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming)
            (setq piem--input-buffer input-buf)
            (setq piem--followup-queue nil))
          ;; Queue three messages while busy
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (dolist (msg '("First message" "Second message" "Third message"))
              (erase-buffer)
              (insert msg)
              (piem-send)))
          ;; All three should be in queue
          (with-current-buffer chat-buf
            (should (= 3 (length piem--followup-queue))))
          ;; Simulate each completed queued turn and then run the scheduled drain.
          (with-current-buffer chat-buf
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--send-prompt)
                       (lambda (text &optional on-success &rest _)
                   (push text sent-prompts)
                   (when on-success (funcall on-success))))
                      ((symbol-function 'piem--refresh-header) #'ignore)
                      ((symbol-function 'run-at-time)
                       (lambda (_secs _repeat fn &rest args)
                         (setq drain-callback fn
                               drain-args args)
                         'fake-drain-timer)))
              (dotimes (_ 3)
                (setq drain-callback nil
                      drain-args nil)
                (piem--handle-display-event '(:type "agent_end"))
                (should (functionp drain-callback))
                (apply drain-callback drain-args))))
          ;; Should have sent all three in FIFO order (sent-prompts is reversed)
          (should (equal (reverse sent-prompts)
                         '("First message" "Second message" "Third message")))
          ;; Queue should be empty
          (with-current-buffer chat-buf
            (should (null piem--followup-queue))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-steering-displayed-from-echo ()
  "Steering is NOT displayed locally - it's displayed when pi echoes it back.
This ensures steering appears at the correct position in the conversation
(after the current assistant output completes)."
  (let ((chat-buf (get-buffer-create "*piem-test-steer-echo*"))
        (input-buf (get-buffer-create "*piem-test-steer-echo-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming)
            (setq piem--input-buffer input-buf)
            (let ((inhibit-read-only t))
              (insert "Assistant\n=========\nWorking on something...\n")))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "Stop and do something else")
            (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--rpc-async) #'ignore))
              (piem-queue-steering)))
          ;; Steering is NOT displayed when sent (unlike normal sends)
          (with-current-buffer chat-buf
            (should-not (string-match-p "Stop and do something else" (buffer-string)))
            ;; local-user-message should still be nil (steering doesn't set it)
            (should-not piem--local-user-message))
          ;; Simulate pi echoing the steering message back via message_start
          (with-current-buffer chat-buf
            (piem--handle-display-event
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

(ert-deftest piem-test-steering-echo-followed-by-assistant-shows-header ()
  "After steering message, the next assistant message shows its header.
This tests the full flow: steering echo resets the flag, then the next
message_start role=assistant displays the 'Assistant' header."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((piem--status 'streaming)
          (piem--local-user-message nil)
          ;; Simulate that first assistant header was already shown
          (piem--assistant-header-shown t))
      ;; First, some assistant content is already in the buffer
      (let ((inhibit-read-only t))
        (insert "Assistant\n=========\nPrevious response...\n"))
      ;; Simulate steering message echo from pi
      (piem--handle-display-event
       '(:type "message_start"
         :message (:role "user"
                   :content [(:type "text" :text "Stop it")]
                   :timestamp 1704067200000)))
      ;; Steering message should be displayed
      (should (string-match-p "Stop it" (buffer-string)))
      ;; Flag should be reset
      (should-not piem--assistant-header-shown)
      ;; Now simulate the assistant's response to steering
      (piem--handle-display-event
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


(ert-deftest piem-test-tool-toggle-expands-content ()
  "Toggle button expands collapsed tool output."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--display-tool-start "bash" '(:command "ls"))
    (piem--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Initially collapsed - should have "... (N more lines)"
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    (should-not (string-match-p "L10" (buffer-string)))
    ;; Find and click the button
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (piem-toggle-tool-section)
    ;; Now should show all lines
    (should (string-match-p "L10" (buffer-string)))
    (should (string-match-p "\\[-\\]" (buffer-string)))))

(ert-deftest piem-test-tool-toggle-collapses-content ()
  "Toggle button collapses expanded tool output."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--display-tool-start "bash" '(:command "ls"))
    (piem--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Expand first
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (piem-toggle-tool-section)
    (should (string-match-p "L10" (buffer-string)))
    ;; Now collapse
    (goto-char (point-min))
    (search-forward "[-]" nil t)
    (backward-char 1)
    (piem-toggle-tool-section)
    ;; Should be collapsed again
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    (should-not (string-match-p "L10" (buffer-string)))))

(ert-deftest piem-test-tool-toggle-re-expand-after-collapse-from-button ()
  "TAB re-expands after collapsing from the [-] button position.
Regression: collapsing from the [-] button placed cursor at the overlay
boundary where overlays-at returns nil, making the next TAB fall through
to outline-cycle instead of toggling.  Uses enough lines so the [-]
button position in the expanded state exceeds the collapsed overlay end."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--display-tool-start "bash" '(:command "ls"))
    (piem--display-tool-end "bash" nil
                          '((:type "text" :text "L01\nL02\nL03\nL04\nL05\nL06\nL07\nL08\nL09\nL10\nL11\nL12\nL13\nL14\nL15"))
                          nil nil)
    ;; Expand
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (piem-toggle-tool-section)
    (should (string-match-p "L15" (buffer-string)))
    ;; Navigate to the [-] button (near end of expanded block)
    (goto-char (point-min))
    (search-forward "[-]" nil t)
    (beginning-of-line)
    ;; Collapse from the button position
    (piem-toggle-tool-section)
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    ;; Verify cursor landed inside the tool block overlay, not at its boundary
    (should (seq-find (lambda (o) (overlay-get o 'piem-tool-block))
                      (overlays-at (point))))
    ;; The critical assertion: TAB must still work to re-expand
    (piem-toggle-tool-section)
    (should (string-match-p "L15" (buffer-string)))))

(ert-deftest piem-test-tool-toggle-expands-with-highlighting ()
  "Expanded tool output has syntax highlighting applied.
With tree-sitter, code blocks get `font-lock-string-face' from
the markdown grammar.  Tool output face also applies."
  (with-temp-buffer
    (piem-chat-mode)
    ;; Create a read tool with Python content (>10 lines to trigger collapse)
    ;; The 'def' keyword is on line 11, hidden initially
    (piem--display-tool-start "read" '(:path "test.py"))
    (piem--display-tool-end "read" '(:path "test.py")
                          '((:type "text" :text "line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\ndef hello():\n    return 42"))
                          nil nil)
    ;; Initially collapsed - 'def' is hidden
    (should (string-match-p "\\.\\.\\..*more lines" (buffer-string)))
    (should-not (string-match-p "def hello" (buffer-string)))
    ;; Expand
    (goto-char (point-min))
    (search-forward "..." nil t)
    (backward-char 1)
    (piem-toggle-tool-section)
    ;; Now 'def' should be visible
    (should (string-match-p "def hello" (buffer-string)))
    ;; Re-fontify after expansion (in GUI, jit-lock handles this)
    (font-lock-ensure)
    ;; Find 'def' keyword and check for some face being applied
    (goto-char (point-min))
    (search-forward "def" nil t)
    (let ((face (get-text-property (match-beginning 0) 'face)))
      ;; With embedded language support, 'def' gets font-lock-keyword-face
      ;; from the Python grammar.  Without it, font-lock-string-face.
      (should face))))

(ert-deftest piem-test-tab-works-from-anywhere-in-block ()
  "TAB toggles tool output from any position within the block."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--display-tool-start "bash" '(:command "ls"))
    (piem--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Go to the header line (not the button)
    (goto-char (point-min))
    (search-forward "$ ls" nil t)
    (beginning-of-line)
    ;; TAB should still expand
    (piem-toggle-tool-section)
    (should (string-match-p "L10" (buffer-string)))))

(ert-deftest piem-test-tab-preserves-cursor-position ()
  "TAB toggle doesn't jump cursor unnecessarily."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--display-tool-start "bash" '(:command "ls"))
    (piem--display-tool-end "bash" nil
                          '((:type "text" :text "L1\nL2\nL3\nL4\nL5\nL6\nL7\nL8\nL9\nL10"))
                          nil nil)
    ;; Go to L3 line
    (goto-char (point-min))
    (search-forward "L3" nil t)
    (beginning-of-line)
    (let ((line-content (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
      ;; Expand
      (piem-toggle-tool-section)
      ;; Should still be on a line starting with L
      (should (string-match-p "^L[0-9]" 
                              (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position)))))))

(ert-deftest piem-test-toggle-preserves-window-scroll ()
  "Toggle collapse/expand should preserve window scroll when viewing content before tool.
When window shows content BEFORE the tool block, toggle should not jump away."
  (let ((buf (generate-new-buffer "*test-toggle-scroll*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (piem-chat-mode)
            ;; Add some content before the tool block
            (let ((inhibit-read-only t))
              (insert "Header line 1\nHeader line 2\nHeader line 3\n\n"))
            ;; Create tool output with many lines
            (piem--display-tool-start "read" '(:path "test.el"))
            (piem--display-tool-end
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
                  (piem-toggle-tool-section)
                  ;; Window should not have jumped
                  (should (= (window-start win) start-before))
                  ;; Now collapse
                  (search-forward "[-]" nil t)
                  (piem-toggle-tool-section)
                  ;; Window should still be at same position
                  (should (= (window-start win) start-before)))))))
      (kill-buffer buf))))

(ert-deftest piem-test-format-fork-message ()
  "Fork message formatted with index and preview."
  (let ((msg '(:entryId "abc-123" :text "Hello world, this is a test")))
    ;; With index
    (let ((result (piem--format-fork-message msg 2)))
      (should (string-match-p "2:" result))
      (should (string-match-p "Hello world" result)))
    ;; Without index
    (let ((result (piem--format-fork-message msg)))
      (should (string-match-p "Hello world" result))
      (should-not (string-match-p ":" result)))))

(ert-deftest piem-test-fork-detects-empty-messages-vector ()
  "Fork correctly detects empty messages vector from RPC.
JSON arrays are parsed as vectors, and (null []) is nil, not t.
The fork code must use seq-empty-p or length check."
  (let ((rpc-called nil)
        (message-shown nil))
    (with-temp-buffer
      (piem-chat-mode)
      (setq piem--status 'idle)
      (cl-letf (((symbol-function 'piem--get-process) (lambda () 'mock-proc))
                ((symbol-function 'piem--get-chat-buffer)
                 (lambda () (current-buffer)))
                ((symbol-function 'piem--rpc-async)
                 (lambda (_proc cmd cb)
                   (setq rpc-called t)
                   ;; Simulate response with empty vector (no messages to fork from)
                   (funcall cb '(:success t :data (:messages [])))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when (string-match-p "No messages" fmt)
                     (setq message-shown t)))))
        (piem-fork)
        (should rpc-called)
        ;; Should show "No messages to fork from", not call completing-read
        (should message-shown)))))

(ert-deftest piem-test-format-fork-message-handles-nil-text ()
  "Format fork message handles nil text gracefully."
  (let ((msg '(:entryId "abc-123" :text nil)))
    ;; Should not error, should return something displayable
    (let ((result (piem--format-fork-message msg 1)))
      (should (stringp result)))))

(ert-deftest piem-test-load-session-history-uses-provided-buffer ()
  "load-session-history uses provided chat buffer, not current buffer context.
This ensures history loads correctly when callback runs in arbitrary context."
  (let* ((chat-buf (generate-new-buffer "*piem-chat:test-history/*"))
         (rpc-callback nil)
         (proc (start-process "test-history-load-provided-buffer" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--process proc))
          ;; Mock RPC to capture callback
          (cl-letf (((symbol-function 'piem--rpc-async)
                     (lambda (_proc _cmd cb) (setq rpc-callback cb))))
            ;; Call with explicit buffer
            (piem--load-session-history proc nil chat-buf))
          ;; Simulate callback from different buffer context
          (with-temp-buffer
            (funcall rpc-callback
                     '(:success t :data (:messages [(:role "user" :content "test")]))))
          ;; Chat buffer should have been updated (has startup header)
          (with-current-buffer chat-buf
            (should (string-match-p "C-c C-c" (buffer-string)))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-header-line-includes-session-name ()
  "piem--header-line-string includes session name when set."
  (let ((chat-buf (get-buffer-create "*pi-test-header-session-name*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (piem-chat-mode)
          (setq piem--state '(:model (:name "test-model") :thinking-level "high"))
          ;; Without session name
          (setq piem--session-name nil)
          (let ((header (piem--header-line-string)))
            (should-not (string-match-p "My Session" header)))
          ;; With session name
          (setq piem--session-name "My Session")
          (let ((header (piem--header-line-string)))
            (should (string-match-p "My Session" header))
            ;; Should have separator before session name
            (should (string-match-p "│" header))))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-header-line-truncates-long-session-name ()
  "piem--header-line-string truncates long session names."
  (let ((chat-buf (get-buffer-create "*pi-test-header-truncate*")))
    (unwind-protect
        (with-current-buffer chat-buf
          (piem-chat-mode)
          (setq piem--state '(:model (:name "test-model")))
          ;; Set a very long session name (longer than 30 chars)
          (setq piem--session-name "This is a very long session name that should be truncated")
          (let ((header (piem--header-line-string)))
            ;; Should contain truncated version with ellipsis
            (should (string-match-p "This is a very long session" header))
            (should (string-match-p "…" header))
            ;; Should NOT contain the full name
            (should-not (string-match-p "truncated$" header))))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-set-session-name-empty-shows-current ()
  "piem-set-session-name with empty string shows current name."
  (let ((chat-buf (get-buffer-create "*pi-test-show-name*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--session-name "My Session"))
          ;; Capture message output
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (piem-set-session-name "")))
          ;; Should show current name, not change it
          (should (equal (buffer-local-value 'piem--session-name chat-buf)
                         "My Session"))
          (should (member "Pi: Session name: My Session" messages)))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-set-session-name-empty-no-name-shows-message ()
  "piem-set-session-name with empty string and no name shows message."
  (let ((chat-buf (get-buffer-create "*pi-test-no-name*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--session-name nil))
          ;; Capture message output
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (piem-set-session-name "")))
          (should (member "Pi: No session name set" messages)))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-set-session-name-no-process-errors ()
  "piem-set-session-name errors when no process is running."
  (let ((chat-buf (get-buffer-create "*pi-test-no-proc*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode))
          ;; Mock piem--get-process to return nil
          (cl-letf (((symbol-function 'piem--get-process)
                     (lambda () nil)))
            (should-error
             (with-current-buffer chat-buf
               (piem-set-session-name "New Name"))
             :type 'user-error)))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-set-session-name-sends-rpc ()
  "piem-set-session-name sends correct RPC command."
  (let ((chat-buf (get-buffer-create "*pi-test-rpc*"))
        (piem--request-id-counter 0)
        (output-buffer (generate-new-buffer " *test-output*")))
    (unwind-protect
        (let ((fake-proc (start-process "cat" output-buffer "cat")))
          (unwind-protect
              (progn
                (with-current-buffer chat-buf
                  (piem-chat-mode))
                ;; Mock get-process and get-chat-buffer
                (cl-letf (((symbol-function 'piem--get-process)
                           (lambda () fake-proc))
                          ((symbol-function 'piem--get-chat-buffer)
                           (lambda () chat-buf)))
                  (piem-set-session-name "Test Session"))
                ;; Wait for output
                (piem-test-wait-until
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

(ert-deftest piem-test-set-session-name-trims-whitespace ()
  "piem-set-session-name trims whitespace from name."
  (let ((chat-buf (get-buffer-create "*pi-test-trim*"))
        (piem--request-id-counter 0)
        (output-buffer (generate-new-buffer " *test-output*")))
    (unwind-protect
        (let ((fake-proc (start-process "cat" output-buffer "cat")))
          (unwind-protect
              (progn
                (with-current-buffer chat-buf
                  (piem-chat-mode))
                ;; Mock get-process and get-chat-buffer
                (cl-letf (((symbol-function 'piem--get-process)
                           (lambda () fake-proc))
                          ((symbol-function 'piem--get-chat-buffer)
                           (lambda () chat-buf)))
                  (piem-set-session-name "  Trimmed Name  "))
                ;; Wait for output
                (piem-test-wait-until
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

(ert-deftest piem-test-set-session-name-whitespace-only-shows-current ()
  "piem-set-session-name with whitespace-only shows current name."
  (let ((chat-buf (get-buffer-create "*pi-test-ws*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--session-name "Existing Name"))
          ;; Capture message output
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (piem-set-session-name "   ")))  ; whitespace only
          ;; Should show current name, not try to set
          (should (equal (buffer-local-value 'piem--session-name chat-buf)
                         "Existing Name"))
          (should (member "Pi: Session name: Existing Name" messages)))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-set-session-name-rpc-failure-shows-error ()
  "piem-set-session-name shows error on RPC failure."
  (let ((chat-buf (get-buffer-create "*pi-test-fail*"))
        (messages nil))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--session-name "Old Name"))
          ;; Mock RPC to call callback with failure
          (cl-letf (((symbol-function 'piem--get-process)
                     (lambda () 'fake-proc))
                    ((symbol-function 'piem--get-chat-buffer)
                     (lambda () chat-buf))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc _cmd callback)
                       (funcall callback '(:success nil :error "test error"))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (with-current-buffer chat-buf
              (piem-set-session-name "New Name")))
          ;; Name should NOT be updated
          (should (equal (buffer-local-value 'piem--session-name chat-buf)
                         "Old Name"))
          ;; Error message should be shown
          (should (member "Pi: Failed to set session name: test error" messages)))
      (kill-buffer chat-buf))))

;;; Input History

(ert-deftest piem-test-history-add-to-ring ()
  "piem--history-add adds input to ring."
  (let ((piem--input-ring nil))
    (piem--history-add "first")
    (piem--history-add "second")
    (should (equal (ring-ref (piem--input-ring) 0) "second"))
    (should (equal (ring-ref (piem--input-ring) 1) "first"))))

(ert-deftest piem-test-history-no-duplicate ()
  "piem--history-add skips duplicates of last entry."
  (let ((piem--input-ring nil))
    (piem--history-add "first")
    (piem--history-add "first")
    (should (= (ring-length (piem--input-ring)) 1))))

(ert-deftest piem-test-history-skip-empty ()
  "piem--history-add skips empty input."
  (let ((piem--input-ring nil))
    (piem--history-add "")
    (piem--history-add "   ")
    (should (ring-empty-p (piem--input-ring)))))

(ert-deftest piem-test-history-previous-input ()
  "piem-previous-input navigates backward through history."
  (let ((piem--input-ring nil))
    (piem--history-add "first")
    (piem--history-add "second")
    (with-temp-buffer
      (piem-input-mode)
      (insert "current")
      (piem-previous-input)
      (should (equal (buffer-string) "second"))
      (should (equal piem--input-saved "current"))
      (piem-previous-input)
      (should (equal (buffer-string) "first")))))

(ert-deftest piem-test-history-next-input ()
  "piem-next-input navigates forward and restores saved input."
  (let ((piem--input-ring nil))
    (piem--history-add "first")
    (piem--history-add "second")
    (with-temp-buffer
      (piem-input-mode)
      (insert "current")
      (piem-previous-input)
      (piem-previous-input)
      (should (equal (buffer-string) "first"))
      (piem-next-input)
      (should (equal (buffer-string) "second"))
      (piem-next-input)
      (should (equal (buffer-string) "current")))))

(ert-deftest piem-test-history-keys-bound ()
  "History keys are bound in piem-input-mode."
  (with-temp-buffer
    (piem-input-mode)
    (should (eq (key-binding (kbd "M-p")) 'piem-previous-input))
    (should (eq (key-binding (kbd "M-n")) 'piem-next-input))
    (should (eq (key-binding (kbd "C-r")) 'piem-history-isearch-backward))))

(ert-deftest piem-test-history-isolated-per-buffer ()
  "Input history is isolated per buffer, not shared globally.
Regression test for #27: history was shared across all sessions."
  (let ((buf1 (generate-new-buffer "*piem-input:project-a*"))
        (buf2 (generate-new-buffer "*piem-input:project-b*")))
    (unwind-protect
        (progn
          ;; Add history in buffer 1
          (with-current-buffer buf1
            (piem-input-mode)
            (piem--history-add "project-a-query"))
          ;; Add different history in buffer 2
          (with-current-buffer buf2
            (piem-input-mode)
            (piem--history-add "project-b-query"))
          ;; Buffer 1 should only see its own history
          (with-current-buffer buf1
            (should (= (ring-length (piem--input-ring)) 1))
            (should (equal (ring-ref (piem--input-ring) 0) "project-a-query")))
          ;; Buffer 2 should only see its own history
          (with-current-buffer buf2
            (should (= (ring-length (piem--input-ring)) 1))
            (should (equal (ring-ref (piem--input-ring) 0) "project-b-query"))))
      ;; Cleanup
      (kill-buffer buf1)
      (kill-buffer buf2))))

;;; History Isearch (C-r incremental search)

(ert-deftest piem-test-history-isearch-empty-history-errors ()
  "piem-history-isearch-backward errors with empty history."
  (with-temp-buffer
    (piem-input-mode)
    (should-error (piem-history-isearch-backward) :type 'user-error)))

(ert-deftest piem-test-history-isearch-saves-current-input ()
  "piem-history-isearch-backward saves current buffer content."
  (with-temp-buffer
    (piem-input-mode)
    (piem--history-add "old command")
    (insert "my current input")
    ;; Mock isearch-backward to avoid actually starting isearch
    (cl-letf (((symbol-function 'isearch-backward) #'ignore))
      (piem-history-isearch-backward))
    (should (equal piem--history-isearch-saved-input "my current input"))))

(ert-deftest piem-test-history-isearch-sets-active-flag ()
  "piem-history-isearch-backward sets the active flag."
  (with-temp-buffer
    (piem-input-mode)
    (piem--history-add "old command")
    (cl-letf (((symbol-function 'isearch-backward) #'ignore))
      (piem-history-isearch-backward))
    (should piem--history-isearch-active)))

(ert-deftest piem-test-history-isearch-end-restores-on-quit ()
  "piem--history-isearch-end restores input when isearch is quit."
  (with-temp-buffer
    (piem-input-mode)
    (setq piem--history-isearch-active t)
    (setq piem--history-isearch-saved-input "original input")
    (erase-buffer)
    (insert "some history item")
    ;; Simulate isearch quit
    (let ((isearch-mode-end-hook-quit t))
      (piem--history-isearch-end))
    (should (equal (buffer-string) "original input"))
    (should-not piem--history-isearch-active)))

(ert-deftest piem-test-history-isearch-end-keeps-on-accept ()
  "piem--history-isearch-end keeps history item when accepted."
  (with-temp-buffer
    (piem-input-mode)
    (setq piem--history-isearch-active t)
    (setq piem--history-isearch-saved-input "original input")
    (erase-buffer)
    (insert "chosen history item")
    ;; Simulate isearch accept (quit is nil)
    (let ((isearch-mode-end-hook-quit nil))
      (piem--history-isearch-end))
    (should (equal (buffer-string) "chosen history item"))
    (should-not piem--history-isearch-active)))

(ert-deftest piem-test-history-isearch-goto-index ()
  "piem--history-isearch-goto loads history item into buffer."
  (with-temp-buffer
    (piem-input-mode)
    (piem--history-add "first")
    (piem--history-add "second")
    (piem--history-add "third")
    (insert "current")
    (piem--history-isearch-goto 1)  ; "second" (0=third, 1=second)
    (should (equal (buffer-string) "second"))
    (should (= piem--history-isearch-index 1))))

(ert-deftest piem-test-history-isearch-hook-added ()
  "isearch-mode-hook is set up in piem-input-mode."
  (with-temp-buffer
    (piem-input-mode)
    (should (memq 'piem--history-isearch-setup isearch-mode-hook))))

(ert-deftest piem-test-history-isearch-goto-nil-restores-saved ()
  "piem--history-isearch-goto with nil index restores saved input."
  (with-temp-buffer
    (piem-input-mode)
    (piem--history-add "history item")
    (setq piem--history-isearch-saved-input "my original input")
    (insert "something else")
    (piem--history-isearch-goto nil)
    (should (equal (buffer-string) "my original input"))
    (should (null piem--history-isearch-index))))

(ert-deftest piem-test-history-isearch-goto-empty-saved-input ()
  "piem--history-isearch-goto with nil index and empty saved input."
  (with-temp-buffer
    (piem-input-mode)
    (piem--history-add "history item")
    (setq piem--history-isearch-saved-input "")
    (insert "something else")
    (piem--history-isearch-goto nil)
    (should (equal (buffer-string) ""))
    (should (null piem--history-isearch-index))))

;;; Input Buffer Completion

(ert-deftest piem-test-input-mode-has-only-own-capfs ()
  "Input mode should only include our own completion functions.
`text-mode' adds `ispell-completion-at-point' by default, which pollutes
the completion candidates with dictionary words.  Our input buffer should
only offer our own capfs (slash commands, file references, paths)."
  (with-temp-buffer
    (piem-input-mode)
    (should (equal completion-at-point-functions
                   '(piem--path-capf
                     piem--file-reference-capf
                     piem--command-capf)))))

(ert-deftest piem-test-input-mode-has-only-own-capfs-with-markdown ()
  "Only our capfs present even with markdown highlighting enabled."
  (let ((piem-input-markdown-highlighting t))
    (with-temp-buffer
      (piem-input-mode)
      (should (equal completion-at-point-functions
                     '(piem--path-capf
                       piem--file-reference-capf
                       piem--command-capf))))))

;;; Input Buffer Slash Completion

(ert-deftest piem-test-command-capf-returns-nil-without-slash ()
  "Completion returns nil when not after slash."
  (with-temp-buffer
    (piem-input-mode)
    (insert "hello")
    (should-not (piem--command-capf))))

(ert-deftest piem-test-command-capf-returns-nil-at-line-start ()
  "Completion returns nil when point is at beginning of line."
  (with-temp-buffer
    (piem-input-mode)
    (insert "/test")
    (goto-char (line-beginning-position))
    (should-not (piem--command-capf))))

(ert-deftest piem-test-command-capf-returns-completion-data ()
  "Completion returns data when after slash at start of buffer."
  (with-temp-buffer
    (piem-input-mode)
    (setq piem--commands '((:name "test-cmd" :description "Test")))
    (insert "/te")
    (let ((result (piem--command-capf)))
      (should result)
      (should (= (nth 0 result) 2))  ; Start after /
      (should (= (nth 1 result) 4))  ; End at point
      (should (member "test-cmd" (nth 2 result))))))

(ert-deftest piem-test-command-capf-ignores-slash-on-later-lines ()
  "Completion ignores / on lines after the first (pi only expands at buffer start)."
  (with-temp-buffer
    (piem-input-mode)
    (setq piem--commands '((:name "test-cmd" :description "Test")))
    (insert "Some context:\n/te")
    (should-not (piem--command-capf))))

(ert-deftest piem-test-command-capf-includes-builtins ()
  "Completion includes built-in commands even when RPC returns nothing."
  (with-temp-buffer
    (piem-input-mode)
    (setq piem--commands nil)
    (insert "/co")
    (let ((result (piem--command-capf)))
      (should result)
      (should (member "compact" (nth 2 result)))
      (should (member "new" (nth 2 result)))
      (should (member "model" (nth 2 result))))))

(ert-deftest piem-test-command-capf-merges-builtins-and-rpc ()
  "Completion merges built-in and RPC commands without duplicates."
  (with-temp-buffer
    (piem-input-mode)
    (setq piem--commands '((:name "my-ext" :description "Extension")))
    (insert "/")
    (let* ((result (piem--command-capf))
           (names (nth 2 result)))
      ;; Has built-in
      (should (member "compact" names))
      ;; Has RPC command
      (should (member "my-ext" names))
      ;; No duplicates
      (should (= (length (seq-filter (lambda (n) (equal n "compact")) names)) 1)))))

(ert-deftest piem-test-send-prompt-sends-literal ()
  "piem--send-prompt sends text literally (no expansion).
Pi handles command expansion on the server side."
  (let* ((rpc-message nil)
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'piem--get-process)
                   (lambda () fake-proc))
                  ((symbol-function 'piem--rpc-async)
                   (lambda (_proc msg _cb) (setq rpc-message msg))))
          (piem--send-prompt "/greet world")
          ;; Should send literal /greet world, NOT expanded
          (should (equal (plist-get rpc-message :message) "/greet world")))
      (delete-process fake-proc))))

(ert-deftest piem-test-image-preview-does-not-add-prompt-images ()
  "Display previews do not add an images field to outgoing prompts."
  (let* ((rpc-message nil)
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (cl-letf (((symbol-function 'piem--get-process)
                   (lambda () fake-proc))
                  ((symbol-function 'piem--rpc-async)
                   (lambda (_proc message _callback)
                     (setq rpc-message message))))
          (piem--send-prompt "/tmp/screenshot.png")
          (should (equal "/tmp/screenshot.png"
                         (plist-get rpc-message :message)))
          (should-not (plist-member rpc-message :images)))
      (delete-process fake-proc))))

(ert-deftest piem-test-prompt-image-png-end-to-end ()
  "C-c C-a content-sniffs a misleadingly named PNG for exact RPC content."
  (piem-test-with-prompt-image-session (dir chat-buf input-buf)
    (let* ((path (piem-test--write-prompt-image
                  (expand-file-name "pixel.txt" dir) 'png))
           (data (piem-test--prompt-image-base64 'png))
           rpc-message)
      (with-current-buffer input-buf
        (piem-test--attach-image-via-key path)
        (should (string-match-p "pixel.txt" (piem-test--input-header)))
        (piem-test--attach-image-via-key path 'clear)
        (should-not (string-match-p "pixel.txt" (piem-test--input-header)))
        (piem-test--attach-image-via-key path)
        (delete-file path)
        (insert "Describe the pixel")
        (cl-letf (((symbol-function 'piem--get-process)
                   (lambda () 'image-process))
                  ((symbol-function 'process-live-p) (lambda (_) t))
                  ((symbol-function 'piem--rpc-async)
                   (lambda (_process command _callback)
                     (setq rpc-message command))))
          (piem-send))
        (should (equal rpc-message
                       (list :type "prompt" :message "Describe the pixel" :images
                             (vector (list :type "image" :data data :mimeType "image/png")))))
        (should (equal (ring-ref piem--input-ring 0) "Describe the pixel"))))))

(ert-deftest piem-test-prompt-image-sync-rpc-error-restores-draft ()
  "A synchronous RPC error restores the exact pending image draft."
  (piem-test-with-prompt-image-session (dir chat-buf input-buf)
    (let* ((path (piem-test--write-prompt-image
                  (expand-file-name "sync-error.png" dir) 'png))
           (text "Keep this image prompt")
           attached-image
           chat-before)
      (with-current-buffer input-buf
        (piem-test--attach-image path)
        (setq attached-image (piem--get-prompt-image))
        (insert text))
      (setq chat-before (with-current-buffer chat-buf (buffer-string)))
      (cl-letf (((symbol-function 'piem--get-process)
                 (lambda () 'image-process))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'piem--rpc-async)
                 (lambda (&rest _) (error "synchronous RPC failure")))
                ((symbol-function 'message) #'ignore))
        (with-current-buffer input-buf
          (condition-case nil
              (piem-send)
            (error nil))))
      (with-current-buffer input-buf
        (should (equal (buffer-string) text))
        (should (eq (piem--get-prompt-image) attached-image)))
      (with-current-buffer chat-buf
        (should-not (piem--prompt-start-wait-active-p))
        (should (eq piem--status 'idle))
        (should-not piem--local-user-message)
        (should (equal (buffer-string) chat-before))))))

(ert-deftest piem-test-prompt-image-signatures-and-rejections ()
  "Other raster signatures attach; non-images and over-cap sources do not."
  (piem-test-with-prompt-image-session (dir _chat-buf input-buf)
    (with-current-buffer input-buf
      (let (previous)
        (dolist (spec '((jpeg "photo.jpg") (gif "pixel.gif")
                        (webp "pixel.webp")))
          (when previous
            (piem-test--attach-image-via-key previous 'clear))
          (setq previous
                (piem-test--write-prompt-image
                 (expand-file-name (cadr spec) dir) (car spec)))
          (piem-test--attach-image previous)
          (should (string-match-p
                   (regexp-quote (file-name-nondirectory previous))
                   (piem-test--input-header))))
        (piem-test--attach-image-via-key previous 'clear))
      (let* ((not-image (expand-file-name "not-image.txt" dir))
             (too-large (piem-test--write-prompt-image
                         (expand-file-name "too-large.png" dir) 'png)))
        (with-temp-file not-image (insert "not an image"))
        (dolist (case `((,not-image nil "image\\|format")
                        (,too-large 1 "large\\|limit\\|byte\\|size")))
          (let (feedback)
            (cl-letf (((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (when format-string
                           (setq feedback (apply #'format format-string args))))))
              (condition-case error-data
                  (let ((piem-prompt-image-max-bytes
                         (or (cadr case) most-positive-fixnum)))
                    (piem-test--attach-image (car case)))
                (user-error (setq feedback (error-message-string error-data)))))
            (should (string-match-p (caddr case) (downcase (or feedback ""))))
            (should-not (string-match-p
                         (regexp-quote (file-name-nondirectory (car case)))
                         (piem-test--input-header)))))))))

(ert-deftest piem-test-prompt-image-refusal-matrix-preserves-draft ()
  "Capability, busy, slash, steering, and empty refusals retain the draft."
  (piem-test-with-prompt-image-session (dir chat-buf input-buf)
    (let ((path (piem-test--write-prompt-image
                 (expand-file-name "guard.png" dir) 'png)))
      (dolist (case '((text-model "Describe" idle send "model\\|support")
                      (missing-input "Missing metadata" idle send "model\\|support\\|load")
                      (unknown-model "Unknown model" idle send "model\\|support\\|load")
                      (busy "Wait" streaming send "busy\\|stream")
                      (slash "/new" idle send "slash\\|command")
                      (steering "Change" streaming steer "steer")
                      (empty "" idle send "empty\\|text\\|prompt")))
        (pcase-let ((`(,kind ,text ,status ,action ,reason) case))
          (with-current-buffer input-buf
            (erase-buffer)
            (piem-test--attach-image path)
            (insert text))
          (with-current-buffer chat-buf
            (setq piem--status status
                  piem--state
                  (pcase kind
                    ('text-model '(:model (:name "Text" :input ["text"])))
                    ('missing-input '(:model (:name "Loading")))
                    ('unknown-model nil)
                    (_ '(:model (:name "Vision" :input ["text" "image"]))))))
          (let (feedback rpc-called builtin-called)
            (cl-letf (((symbol-function 'piem--get-process)
                       (lambda () 'image-process))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'piem--rpc-async)
                       (lambda (&rest _) (setq rpc-called t)))
                      ((symbol-function 'piem-new-session)
                       (lambda () (setq builtin-called t)))
                      ((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (when format-string
                           (setq feedback (apply #'format format-string args))))))
              (with-current-buffer input-buf
                (pcase action
                  ('send (piem-send))
                  ('steer (piem-queue-steering)))
                (should (equal (buffer-string) text))
                (should (string-match-p "guard.png"
                                        (piem-test--input-header))))
              (should-not rpc-called)
              (should-not builtin-called)
              (should (string-match-p reason (downcase (or feedback "")))))
          (with-current-buffer input-buf
            (piem-test--attach-image-via-key path 'clear))))))))

(ert-deftest piem-test-prompt-image-waits-for-model-change ()
  "Image send waits for model selection, then uses the accepted model state."
  (piem-test-with-prompt-image-session (dir chat-buf input-buf)
    (let* ((path (piem-test--write-prompt-image
                  (expand-file-name "model-change.png" dir) 'png))
           (old-model '(:id "vision-old" :name "Vision Old"
                        :provider "fake" :input ["text" "image"]))
           (new-model '(:id "vision-next" :name "Vision Next"
                        :provider "fake" :input ["text" "image"]))
           (text "Wait for the selected model")
           attached-image
           model-callback
           prompt-command)
      (with-current-buffer chat-buf
        (setq piem--process 'image-process
              piem--state (list :model old-model)))
      (with-current-buffer input-buf
        (piem-test--attach-image path)
        (setq attached-image (piem--get-prompt-image))
        (insert text))
      (cl-letf (((symbol-function 'piem--get-process)
                 (lambda () 'image-process))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'piem--rpc-sync)
                 (lambda (&rest _)
                   (list :success t :data
                         (list :models (vector old-model new-model)))))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (or (seq-find
                        (lambda (candidate)
                          (string-match-p "Vision Next" candidate))
                        collection)
                       (ert-fail "Missing Vision Next model choice"))))
                ((symbol-function 'piem--rpc-async)
                 (lambda (_process command callback)
                   (pcase (plist-get command :type)
                     ("set_model" (setq model-callback callback))
                     ("prompt" (setq prompt-command command)))))
                ((symbol-function 'message) #'ignore))
        (with-current-buffer input-buf
          (piem-select-model)
          (should (functionp model-callback))
          (piem-send)
          (should (equal (buffer-string) text))
          (should (eq (piem--get-prompt-image) attached-image)))
        (should-not prompt-command)
        (funcall model-callback
                 (list :success t :command "set_model" :data new-model))
        (with-current-buffer input-buf
          (piem-send)
          (should (string-empty-p (buffer-string)))
          (should-not (piem--get-prompt-image)))
        (should (equal (plist-get prompt-command :message) text))
        (should (plist-member prompt-command :images))))))

(ert-deftest piem-test-model-change-refuses-image-preflight ()
  "Model selection cannot overlap an image prompt awaiting acceptance."
  (piem-test-with-prompt-image-session (_dir chat-buf _input-buf)
    (let (rpc-called feedback)
      (with-current-buffer chat-buf
        (setq piem--process 'image-process
              piem--status 'sending
              piem--prompt-start-wait-active t))
      (cl-letf (((symbol-function 'piem--get-process)
                 (lambda () 'image-process))
                ((symbol-function 'piem--get-chat-buffer)
                 (lambda () chat-buf))
                ((symbol-function 'piem--rpc-sync)
                 (lambda (&rest _)
                   (setq rpc-called t)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (setq feedback (apply #'format format-string args))))))
        (with-current-buffer chat-buf
          (piem-select-model)))
      (should-not rpc-called)
      (should (string-match-p "Cannot change models"
                              (or feedback ""))))))

(ert-deftest piem-test-model-change-aborts-if-process-changes-during-selection ()
  "A selector cannot acquire a model gate for a process that was replaced."
  (piem-test-with-prompt-image-session (_dir chat-buf _input-buf)
    (let* ((old-model '(:id "old" :name "Old" :provider "fake"))
           (new-model '(:id "new" :name "New" :provider "fake"))
           rpc-called
           feedback)
      (with-current-buffer chat-buf
        (setq piem--process 'old-process
              piem--state (list :model old-model)))
      (cl-letf (((symbol-function 'piem--get-process)
                 (lambda () 'old-process))
                ((symbol-function 'piem--get-chat-buffer)
                 (lambda () chat-buf))
                ((symbol-function 'piem--rpc-sync)
                 (lambda (&rest _)
                   (list :success t :data
                         (list :models (vector old-model new-model)))))
                ((symbol-function 'completing-read)
                 (lambda (&rest _)
                   (with-current-buffer chat-buf
                     (setq piem--process 'new-process))
                   "New [fake]"))
                ((symbol-function 'piem--rpc-async)
                 (lambda (&rest _)
                   (setq rpc-called t)))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (setq feedback (apply #'format format-string args))))))
        (with-current-buffer chat-buf
          (piem-select-model)))
      (should-not rpc-called)
      (with-current-buffer chat-buf
        (should-not (piem--model-change-pending-p)))
      (should (equal feedback
                     "Pi: Process changed while selecting a model; try again")))))

(ert-deftest piem-test-model-cancellation-restores-gated-queue ()
  "Cancelling a model change makes text queued behind it visible."
  (piem-test-with-prompt-image-session (_dir chat-buf input-buf)
    (with-current-buffer chat-buf
      (setq piem--process 'old-process)
      (should (piem--begin-model-change
               'old-process chat-buf))
      (piem--push-followup "do not strand me")
      (piem--cancel-model-change-and-restore-followups chat-buf)
      (piem--set-process 'new-process)
      (should-not (piem--model-change-pending-p))
      (should-not piem--followup-queue))
    (with-current-buffer input-buf
      (should (equal (buffer-string) "do not strand me")))))

(ert-deftest piem-test-failed-model-change-restores-queued-text ()
  "A failed model change must not send queued text under the old model."
  (piem-test-with-prompt-image-session (_dir chat-buf input-buf)
    (let* ((old-model '(:id "old" :name "Old" :provider "fake"))
           (new-model '(:id "new" :name "New" :provider "fake"))
           model-callback
           prompt-called
           feedback)
      (with-current-buffer chat-buf
        (setq piem--process 'image-process
              piem--state (list :model old-model)))
      (cl-letf (((symbol-function 'piem--get-process)
                 (lambda () 'image-process))
                ((symbol-function 'piem--get-chat-buffer)
                 (lambda () chat-buf))
                ((symbol-function 'piem--rpc-sync)
                 (lambda (&rest _)
                   (list :success t :data
                         (list :models (vector old-model new-model)))))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "New [fake]"))
                ((symbol-function 'piem--rpc-async)
                 (lambda (_process command callback)
                   (pcase (plist-get command :type)
                     ("set_model" (setq model-callback callback))
                     ("prompt" (setq prompt-called t)))))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (when format-string
                     (setq feedback (apply #'format format-string args))))))
        (with-current-buffer chat-buf
          (piem-select-model))
        (with-current-buffer input-buf
          (insert "keep this queued")
          (piem-send)
          (should (string-empty-p (buffer-string))))
        (should (functionp model-callback))
        (funcall model-callback '(:success :false :error "model unavailable")))
      (should-not prompt-called)
      (with-current-buffer chat-buf
        (should-not (piem--model-change-pending-p))
        (should-not piem--followup-queue)
        (should (equal (plist-get piem--state :model) old-model)))
      (with-current-buffer input-buf
        (should (equal (buffer-string) "keep this queued")))
      (should (equal feedback
                     "Pi: Failed to set model: model unavailable")))))

(ert-deftest piem-test-prompt-image-stale-model-callback-stays-gated ()
  "A replaced process's model callback cannot release the current gate."
  (piem-test-with-prompt-image-session (dir chat-buf input-buf)
    (let* ((path (piem-test--write-prompt-image
                  (expand-file-name "stale-model.png" dir) 'png))
           (old-model '(:id "vision-old" :name "Vision Old"
                        :provider "fake" :input ["text" "image"]))
           (model-a '(:id "vision-a" :name "Vision A"
                      :provider "fake" :input ["text" "image"]))
           (model-b '(:id "vision-b" :name "Vision B"
                      :provider "fake" :input ["text" "image"]))
           (text "Keep gating this image")
           (current-process 'image-process)
           choice-name attached-image model-callbacks prompt-command)
      (with-current-buffer chat-buf
        (setq piem--process current-process
              piem--state (list :model old-model)))
      (with-current-buffer input-buf
        (piem-test--attach-image path)
        (setq attached-image (piem--get-prompt-image))
        (insert text))
      (cl-letf (((symbol-function 'piem--get-process)
                 (lambda () current-process))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'piem--rpc-sync)
                 (lambda (&rest _)
                   (list :success t :data
                         (list :models (vector old-model model-a model-b)))))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (or (seq-find
                        (lambda (candidate)
                          (string-match-p choice-name candidate))
                        collection)
                       (ert-fail "Missing requested model choice"))))
                ((symbol-function 'piem--rpc-async)
                 (lambda (_process command callback)
                   (pcase (plist-get command :type)
                     ("set_model"
                      (push (cons (plist-get command :modelId) callback)
                            model-callbacks))
                     ("prompt" (setq prompt-command command)))))
                ((symbol-function 'message) #'ignore))
        (setq choice-name "Vision A")
        (with-current-buffer input-buf
          (piem-select-model))
        (setq current-process 'replacement-process)
        (with-current-buffer chat-buf
          (piem--set-process current-process))
        (setq choice-name "Vision B")
        (with-current-buffer input-buf
          (piem-select-model))
        (let ((callback-a (alist-get "vision-a" model-callbacks
                                     nil nil #'equal))
              (callback-b (alist-get "vision-b" model-callbacks
                                     nil nil #'equal)))
          (should (functionp callback-a))
          (should (functionp callback-b))
          (funcall callback-a
                   (list :success t :command "set_model" :data model-a))
          (with-current-buffer input-buf
            (piem-send)
            (should (equal (buffer-string) text))
            (should (eq (piem--get-prompt-image) attached-image)))
          (should-not prompt-command)
          (funcall callback-b
                   (list :success t :command "set_model" :data model-b))
          (with-current-buffer chat-buf
            (should (equal (plist-get (plist-get piem--state :model)
                                      :id)
                           "vision-b")))
          (with-current-buffer input-buf
            (piem-send)
            (should (string-empty-p (buffer-string)))
            (should-not (piem--get-prompt-image)))
          (should (equal (plist-get prompt-command :message) text))
          (should (plist-member prompt-command :images)))))))

(ert-deftest piem-test-prompt-image-preflight-ownership ()
  "No-process and rejected sends restore image bytes; acceptance consumes them."
  (piem-test-with-prompt-image-session (dir chat-buf input-buf)
    (let* ((path (piem-test--write-prompt-image
                  (expand-file-name "restored.png" dir) 'png))
           (data (piem-test--prompt-image-base64 'png))
           rpc-message rpc-callback attached-image)
      (with-current-buffer input-buf
        (piem-test--attach-image path)
        (setq attached-image (piem--get-prompt-image))
        (delete-file path)
        (insert "Recover this turn")
        (cl-letf (((symbol-function 'piem--get-process) (lambda () nil))
                  ((symbol-function 'message) #'ignore))
          (piem-send))
        (should (equal (buffer-string) "Recover this turn"))
        (should (string-match-p "restored.png"
                                (piem-test--input-header)))
        (cl-labels ((send ()
                      (cl-letf (((symbol-function 'piem--get-process)
                                 (lambda () 'image-process))
                                ((symbol-function 'process-live-p) (lambda (_) t))
                                ((symbol-function 'piem--rpc-async)
                                 (lambda (_process command callback)
                                   (setq rpc-message command
                                         rpc-callback callback)))
                                ((symbol-function 'message) #'ignore))
                        (piem-send))))
          (send)
          (let ((pending-block (aref (plist-get rpc-message :images) 0)))
            (should (equal (plist-get pending-block :data) data))
            (should-error (piem-attach-image 'clear)
                          :type 'user-error)
            (funcall rpc-callback '(:success nil :error "rejected"))
            (should (eq (piem--get-prompt-image) attached-image))
            (should (equal
                     (piem--prompt-image-content-block
                      (piem--get-prompt-image))
                     pending-block)))
          (should (equal (buffer-string) "Recover this turn"))
          (should (string-match-p "restored.png"
                                  (piem-test--input-header)))
          (setq rpc-message nil rpc-callback nil)
          (send)
          (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
            (funcall rpc-callback '(:success t))))
        (should (string-empty-p (buffer-string)))
        (should-not (string-match-p "restored.png"
                                    (piem-test--input-header))))
      (with-current-buffer chat-buf
        (should (string-match-p "Recover this turn" (buffer-string)))
        (should (string-match-p "Image: image/png" (buffer-string)))))))

(ert-deftest piem-test-prompt-image-authoritative-echo-compares-content ()
  "A same-text echo with a different image renders authoritative content."
  (piem-test-with-prompt-image-session (dir chat-buf input-buf)
    (let* ((path (piem-test--write-prompt-image
                  (expand-file-name "original.png" dir) 'png))
           (text "Inspect this image")
           (jpeg-data (piem-test--prompt-image-base64 'jpeg))
           rpc-callback)
      (with-current-buffer input-buf
        (piem-test--attach-image path)
        (insert text)
        (cl-letf (((symbol-function 'piem--get-process)
                   (lambda () 'image-process))
                  ((symbol-function 'process-live-p) (lambda (_) t))
                  ((symbol-function 'piem--rpc-async)
                   (lambda (_process _command callback)
                     (setq rpc-callback callback))))
          (piem-send)))
      (should (functionp rpc-callback))
      (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
        (funcall rpc-callback '(:success t))
        (with-current-buffer chat-buf
          (piem--handle-display-event '(:type "agent_start"))
          (should (string-match-p "Image: image/png" (buffer-string)))
          (piem--handle-display-event
           (list :type "message_start"
                 :message
                 (list :role "user" :timestamp 1704067200000
                       :content
                       (vector (list :type "text" :text text)
                               (list :type "image" :data jpeg-data
                                     :mimeType "image/jpeg")))))
          (should (string-match-p "Image: image/jpeg" (buffer-string))))))))

(ert-deftest piem-test-prompt-image-no-turn-success-retracts-local-echo ()
  "An extension-handled image prompt leaves no phantom user turn."
  (piem-test-with-prompt-image-session (dir chat-buf input-buf)
    (let* ((path (piem-test--write-prompt-image
                  (expand-file-name "handled.png" dir) 'png))
           rpc-callback state-callback fallback-callback fallback-args)
      (with-current-buffer chat-buf
        (setq piem--process 'image-process))
      (with-current-buffer input-buf
        (piem-test--attach-image path)
        (insert "Handle this without a turn"))
      (cl-letf (((symbol-function 'piem--get-process)
                 (lambda () 'image-process))
                ((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'piem--rpc-async)
                 (lambda (_process command callback)
                   (pcase (plist-get command :type)
                     ("prompt" (setq rpc-callback callback))
                     ("get_state" (setq state-callback callback)))))
                ((symbol-function 'run-at-time)
                 (lambda (_secs _repeat function &rest args)
                   (if (eq function
                           'piem--clear-sending-if-no-agent-start)
                       (setq fallback-callback function
                             fallback-args args)
                     'fake-drain-timer)
                   'fake-prompt-start-timer))
                ((symbol-function 'display-images-p) (lambda (&rest _) nil))
                ((symbol-function 'message) #'ignore))
        (with-current-buffer input-buf
          (piem-send))
        (funcall rpc-callback '(:success t))
        (with-current-buffer chat-buf
          (should piem--local-user-message)
          (should (string-match-p "Handle this without a turn"
                                  (buffer-string)))
          (narrow-to-region (1+ (point-min)) (point-max)))
        (apply fallback-callback fallback-args)
        (should (functionp state-callback))
        (funcall state-callback
                 '(:success t
                   :data (:isStreaming :false :isCompacting :false))))
      (with-current-buffer chat-buf
        (widen)
        (should (eq piem--status 'idle))
        (should-not piem--local-user-message)
        (should-not piem--local-user-message-region)
        (should-not (string-match-p "Handle this without a turn"
                                    (buffer-string)))))))

(ert-deftest piem-test-no-turn-fallback-keeps-server-active-prompt ()
  "A delayed agent_start must not be mistaken for an extension-handled prompt."
  (let ((fake-proc (start-process "test-active-prompt" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (piem-chat-mode)
          (setq piem--process fake-proc
                piem--status 'sending
                piem--local-user-message "slow prompt"
                piem--followup-queue '("wait behind it"))
          (setq piem--local-user-message-region
                (piem--display-user-message
                 "slow prompt" (current-time) nil t))
          (let ((generation (piem--begin-prompt-start-wait)))
            (cl-letf (((symbol-function 'piem--rpc-async)
                       (lambda (_process command callback)
                         (should (equal (plist-get command :type) "get_state"))
                         (funcall callback
                                  '(:success t
                                    :data (:isStreaming t
                                           :isCompacting :false)))))
                      ((symbol-function 'run-at-time)
                       (lambda (&rest _) 'fake-prompt-start-timer)))
              (piem--clear-sending-if-no-agent-start
               (current-buffer) generation
               #'piem--handle-no-turn-local-prompt))
            (should (piem--prompt-start-current-p generation))
            (should (equal piem--local-user-message "slow prompt"))
            (should piem--local-user-message-region)
            (should (equal piem--followup-queue '("wait behind it")))
            (should (string-match-p "slow prompt" (buffer-string)))))
      (when (process-live-p fake-proc)
        (delete-process fake-proc)))))

(ert-deftest piem-test-no-turn-prompt-retracts-local-echo ()
  "An extension-handled prompt leaves no speculative user turn behind."
  (let ((chat-buf (generate-new-buffer "*pi-no-turn-retract-chat*"))
        (input-buf (generate-new-buffer "*pi-no-turn-retract-input*"))
        (fake-proc (start-process "test-no-turn-retract" nil "cat"))
        prompt-callback state-callback fallback-callback fallback-args)
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--process fake-proc
                  piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "Handle this without a turn"))
          (cl-letf (((symbol-function 'piem--get-process)
                     (lambda () fake-proc))
                    ((symbol-function 'piem--get-chat-buffer)
                     (lambda () chat-buf))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_process command callback)
                       (pcase (plist-get command :type)
                         ("prompt" (setq prompt-callback callback))
                         ("get_state" (setq state-callback callback)))))
                    ((symbol-function 'run-at-time)
                     (lambda (_seconds _repeat function &rest args)
                       (if (eq function
                               'piem--clear-sending-if-no-agent-start)
                           (setq fallback-callback function
                                 fallback-args args)
                         'fake-drain-timer)
                       'fake-prompt-start-timer))
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer input-buf
              (piem-send))
            (funcall prompt-callback '(:success t))
            (with-current-buffer chat-buf
              (should (equal piem--local-user-message
                             "Handle this without a turn"))
              (narrow-to-region (1+ (point-min)) (point-max)))
            (apply fallback-callback fallback-args)
            (funcall state-callback
                     '(:success t
                       :data (:isStreaming :false :isCompacting :false))))
          (with-current-buffer chat-buf
            (widen)
            (should (eq piem--status 'idle))
            (should-not piem--local-user-message)
            (should-not piem--local-user-message-region)
            (should-not (string-match-p "Handle this without a turn"
                                        (buffer-string)))))
      (when (process-live-p fake-proc)
        (delete-process fake-proc))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-send-prompt-marks-sending-until-preflight-fails ()
  "piem--send-prompt closes the local pre-agent_start idle gap."
  (let* ((rpc-callback nil)
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (piem-chat-mode)
          (setq piem--status 'idle
                piem--activity-phase "idle")
          (cl-letf (((symbol-function 'piem--get-process)
                     (lambda () fake-proc))
                    ((symbol-function 'piem--get-chat-buffer)
                     (lambda () (current-buffer)))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc _msg cb) (setq rpc-callback cb)))
                    ((symbol-function 'message) #'ignore))
            (piem--send-prompt "hello")
            (should (eq piem--status 'sending))
            (should (equal piem--activity-phase "thinking"))
            (should (functionp rpc-callback))
            (funcall rpc-callback '(:success :false :error "preflight failed"))
            (should (eq piem--status 'idle))
            (should (equal piem--activity-phase "idle"))))
      (delete-process fake-proc))))

(ert-deftest piem-test-normal-send-preflight-failure-restores-input ()
  "Rejected normal sends do not leave ghost chat text or lose input."
  (let ((chat-buf (get-buffer-create "*piem-test-send-fail-echo*"))
        (input-buf (get-buffer-create "*piem-test-send-fail-echo-input*"))
        (rpc-callback nil)
        (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle
                  piem--activity-phase "idle"
                  piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "this send will fail")
            (cl-letf (((symbol-function 'piem--get-process)
                       (lambda () fake-proc))
                      ((symbol-function 'piem--get-chat-buffer)
                       (lambda () chat-buf))
                      ((symbol-function 'piem--rpc-async)
                       (lambda (_proc _msg cb) (setq rpc-callback cb)))
                      ((symbol-function 'message) #'ignore))
              (piem-send)))
          (with-current-buffer chat-buf
            (should (null piem--local-user-message))
            (should-not (string-match-p "this send will fail" (buffer-string)))
            (funcall rpc-callback '(:success :false :error "preflight failed"))
            (should (eq piem--status 'idle))
            (should (equal piem--activity-phase "idle"))
            (should (null piem--local-user-message))
            (should-not (string-match-p "this send will fail" (buffer-string))))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "this send will fail"))))
      (delete-process fake-proc)
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-slash-prompt-preflight-failure-restores-input ()
  "Rejected slash prompts are restored instead of being lost silently."
  (let ((chat-buf (get-buffer-create "*piem-test-slash-fail-chat*"))
        (input-buf (get-buffer-create "*piem-test-slash-fail-input*"))
        (rpc-callback nil)
        (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle
                  piem--activity-phase "idle"
                  piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "/unknown command")
            (cl-letf (((symbol-function 'piem--get-process)
                       (lambda () fake-proc))
                      ((symbol-function 'piem--get-chat-buffer)
                       (lambda () chat-buf))
                      ((symbol-function 'piem--rpc-async)
                       (lambda (_proc _msg cb) (setq rpc-callback cb)))
                      ((symbol-function 'message) #'ignore))
              (piem-send)))
          (with-current-buffer chat-buf
            (should-not (string-match-p "/unknown command" (buffer-string)))
            (funcall rpc-callback '(:success :false :error "preflight failed"))
            (should (eq piem--status 'idle)))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "/unknown command"))))
      (delete-process fake-proc)
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-normal-send-without-process-restores-input ()
  "Direct sends without a process do not create ghost chat text."
  (let ((chat-buf (get-buffer-create "*piem-test-send-no-process-chat*"))
        (input-buf (get-buffer-create "*piem-test-send-no-process-input*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle
                  piem--activity-phase "idle"
                  piem--process nil
                  piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "no process prompt")
            (cl-letf (((symbol-function 'message) #'ignore))
              (piem-send))
            (should (equal (buffer-string) "no process prompt")))
          (with-current-buffer chat-buf
            (should (eq piem--status 'idle))
            (should (null piem--local-user-message))
            (should-not (string-match-p "no process prompt" (buffer-string)))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-restore-followups-prepends-existing-draft ()
  "Recovered queued text stays before any newer input draft."
  (let ((chat-buf (get-buffer-create "*piem-test-restore-before-draft-chat*"))
        (input-buf (get-buffer-create "*piem-test-restore-before-draft-input*")))
    (unwind-protect
        (progn
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "newer draft"))
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--input-buffer input-buf
                  piem--followup-queue '("second" "first"))
            (piem--restore-followup-queue-to-input)
            (should (null piem--followup-queue)))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "first\n\nsecond\n\nnewer draft"))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-queued-followup-send-failure-restores-queue-to-input ()
  "Rejected queued follow-ups become visible input instead of hidden work."
  (let ((chat-buf (get-buffer-create "*piem-test-queued-fail-chat*"))
        (input-buf (get-buffer-create "*piem-test-queued-fail-input*"))
        (rpc-callback nil)
        (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle
                  piem--activity-phase "idle"
                  piem--input-buffer input-buf
                  piem--followup-queue '("newer queued item" "queued send will fail")))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf))
          (cl-letf (((symbol-function 'piem--get-process)
                     (lambda () fake-proc))
                    ((symbol-function 'piem--get-chat-buffer)
                     (lambda () chat-buf))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc _msg cb) (setq rpc-callback cb)))
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer chat-buf
              (piem--process-followup-queue)
              (should (eq piem--status 'sending))
              (should (equal piem--followup-queue
                             '("newer queued item" "queued send will fail")))
              (should-not (string-match-p "queued send will fail" (buffer-string)))
              (funcall rpc-callback '(:success :false :error "preflight failed"))
              (should (eq piem--status 'idle))
              (should (null piem--followup-queue))
              (should-not (string-match-p "queued send will fail" (buffer-string)))))
          (with-current-buffer input-buf
            (should (equal (buffer-string)
                           "queued send will fail\n\nnewer queued item"))))
      (delete-process fake-proc)
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-send-prompt-success-without-agent-start-returns-idle ()
  "Successful no-turn slash commands do not leave the frontend sending."
  (let* ((rpc-callback nil)
         (fallback-callback nil)
         (fallback-args nil)
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (piem-chat-mode)
          (setq piem--status 'idle
                piem--activity-phase "idle"
                piem--process fake-proc)
          (cl-letf (((symbol-function 'piem--get-process)
                     (lambda () fake-proc))
                    ((symbol-function 'piem--get-chat-buffer)
                     (lambda () (current-buffer)))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc command cb)
                       (pcase (plist-get command :type)
                         ("prompt" (setq rpc-callback cb))
                         ("get_state"
                          (funcall cb
                                   '(:success t
                                     :data (:isStreaming :false
                                            :isCompacting :false)))))))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args)
                       (setq fallback-callback fn
                             fallback-args args)
                       'fake-prompt-start-timer)))
            (piem--send-prompt "/test-noop")
            (should (eq piem--status 'sending))
            (funcall rpc-callback '(:success t))
            (should (functionp fallback-callback))
            (apply fallback-callback fallback-args)
            (should (eq piem--status 'idle))
            (should (equal piem--activity-phase "idle"))))
      (delete-process fake-proc))))

(ert-deftest piem-test-no-turn-prompt-fallback-drains-queued-followup ()
  "Successful no-turn commands release follow-ups queued while sending."
  (let* ((rpc-callbacks nil)
         (prompt-fallback-callback nil)
         (prompt-fallback-args nil)
         (drain-callback nil)
         (drain-args nil)
         (sent-messages nil)
         (chat-buf (get-buffer-create "*piem-test-no-turn-drain*"))
         (input-buf (get-buffer-create "*piem-test-no-turn-drain-input*"))
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle
                  piem--activity-phase "idle"
                  piem--process fake-proc
                  piem--input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf))
          (cl-letf (((symbol-function 'piem--get-process)
                     (lambda () fake-proc))
                    ((symbol-function 'piem--get-chat-buffer)
                     (lambda () chat-buf))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc cmd cb)
                       (pcase (plist-get cmd :type)
                         ("prompt"
                          (push (plist-get cmd :message) sent-messages)
                          (push cb rpc-callbacks))
                         ("get_state"
                          (funcall cb
                                   '(:success t
                                     :data (:isStreaming :false
                                            :isCompacting :false)))))))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args)
                       (cond
                        ((eq fn 'piem--clear-sending-if-no-agent-start)
                         (setq prompt-fallback-callback fn
                               prompt-fallback-args args)
                         'fake-prompt-start-timer)
                        ((eq fn 'piem--drain-followup-queue-if-idle)
                         (setq drain-callback fn
                               drain-args args)
                         'fake-drain-timer)
                        (t 'fake-timer))))
                    ((symbol-function 'message) #'ignore))
            (with-current-buffer chat-buf
              (piem--prepare-and-send "/test-noop"))
            (with-current-buffer input-buf
              (insert "follow-up after noop")
              (piem-send))
            (with-current-buffer chat-buf
              (should (eq piem--status 'sending))
              (should (equal piem--followup-queue
                             '("follow-up after noop"))))
            (funcall (car rpc-callbacks) '(:success t))
            (should (functionp prompt-fallback-callback))
            (apply prompt-fallback-callback prompt-fallback-args)
            (should (functionp drain-callback))
            (apply drain-callback drain-args)
            (should (equal (reverse sent-messages)
                           '("/test-noop" "follow-up after noop")))))
      (delete-process fake-proc)
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-stale-prompt-start-fallback-does-not-clear-newer-send ()
  "A stale no-turn fallback timer cannot clear a newer sending prompt."
  (let* ((rpc-callbacks nil)
         (fallbacks nil)
         (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (piem-chat-mode)
          (setq piem--status 'idle
                piem--activity-phase "idle")
          (cl-letf (((symbol-function 'piem--get-process)
                     (lambda () fake-proc))
                    ((symbol-function 'piem--get-chat-buffer)
                     (lambda () (current-buffer)))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc _msg cb) (push cb rpc-callbacks)))
                    ((symbol-function 'run-at-time)
                     (lambda (_secs _repeat fn &rest args)
                       (push (cons fn args) fallbacks)
                       'fake-prompt-start-timer)))
            (piem--send-prompt "/first-noop")
            (funcall (car rpc-callbacks) '(:success t))
            (let ((first-fallback (car fallbacks)))
              (piem--send-prompt "/second-command")
              (should (eq piem--status 'sending))
              (apply (car first-fallback) (cdr first-fallback))
              (should (eq piem--status 'sending))
              (should (equal piem--activity-phase "thinking")))))
      (delete-process fake-proc))))

(ert-deftest piem-test-agent-start-invalidates-prompt-success-fallback ()
  "A prompt response arriving after agent_start cannot schedule stale idle reset."
  (let ((rpc-callback nil)
        (fallback-scheduled nil)
        (fake-proc (start-process "test" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (piem-chat-mode)
          (setq piem--status 'idle
                piem--activity-phase "idle")
          (cl-letf (((symbol-function 'piem--get-process)
                     (lambda () fake-proc))
                    ((symbol-function 'piem--get-chat-buffer)
                     (lambda () (current-buffer)))
                    ((symbol-function 'piem--rpc-async)
                     (lambda (_proc _msg cb) (setq rpc-callback cb)))
                    ((symbol-function 'run-at-time)
                     (lambda (&rest _)
                       (setq fallback-scheduled t)
                       'fake-prompt-start-timer)))
            (piem--send-prompt "/starts-fast")
            (piem--handle-display-event '(:type "agent_start"))
            (funcall rpc-callback '(:success t))
            (should-not fallback-scheduled)
            (should (eq piem--status 'streaming))))
      (delete-process fake-proc))))

(ert-deftest piem-test-agent-start-cancels-prompt-start-fallback ()
  "A real agent_start cancels the no-turn prompt fallback timer."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((cancelled nil))
      (setq piem--status 'sending
            piem--prompt-start-timer 'fake-timer)
      (cl-letf (((symbol-function 'timerp) (lambda (timer) (eq timer 'fake-timer)))
                ((symbol-function 'cancel-timer) (lambda (_timer) (setq cancelled t))))
        (piem--handle-display-event '(:type "agent_start")))
      (should cancelled)
      (should (null piem--prompt-start-timer))
      (should (eq piem--status 'streaming)))))

(ert-deftest piem-test-format-session-stats ()
  "Format session stats returns readable string with cache details."
  (let ((stats '(:tokens (:input 50000 :output 10000 :total 60000
                         :cacheRead 123000 :cacheWrite 4567)
                 :cost 0.45
                 :userMessages 5
                 :toolCalls 12)))
    (let ((result (piem--format-session-stats stats)))
      (should (string-match-p "50,000" result))
      (should (string-match-p "10,000" result))
      (should (string-match-p "60,000" result))
      (should (string-match-p "123,000" result))
      (should (string-match-p "4,567" result))
      (should (string-match-p "\\$0.45" result))
      (should (string-match-p "Messages: 5" result))
      (should (string-match-p "Tools: 12" result)))))

(ert-deftest piem-test-header-line-shows-model ()
  "Header line displays current model."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model "claude-sonnet-4"))
    (let ((header (piem--header-line-string)))
      (should (string-match-p "sonnet-4" header)))))

(ert-deftest piem-test-header-line-shows-thinking ()
  "Header line displays thinking level."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model "claude-sonnet-4" :thinking-level "high"))
    (let ((header (piem--header-line-string)))
      (should (string-match-p "high" header)))))

(ert-deftest piem-test-header-line-shows-activity-phase ()
  "Header line shows the current activity phase label."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model "claude-sonnet-4" :thinking-level "high")
          piem--activity-phase "thinking")
    (let ((header (piem--header-line-string)))
      (should (string-match-p "thinking" header)))))

(ert-deftest piem-test-header-line-shows-idle ()
  "Header line shows idle activity phase with fixed-width padding."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model "claude-sonnet-4" :thinking-level "high")
          piem--activity-phase "idle")
    (let ((header (substring-no-properties (piem--header-line-string))))
      (should (string-match-p "idle    " header)))))

(ert-deftest piem-test-header-line-phase-is-padded ()
  "Header line activity phase slot is always 8 characters wide."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model "claude-sonnet-4" :thinking-level "high")
          piem--activity-phase "running")
    (let* ((header (substring-no-properties (piem--header-line-string)))
           (pos (string-match "running" header)))
      (should pos)
      (should (equal (substring header pos (+ pos 8)) "running ")))))

(ert-deftest piem-test-header-line-shows-thinking-activity-phase ()
  "Header line shows semantic activity label during streaming."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model "claude-sonnet-4")
          piem--activity-phase "thinking")
    (let ((header (piem--header-line-string)))
      (should (string-match-p "thinking" header)))))

(ert-deftest piem-test-quit-prompts-even-when-process-noquery ()
  "Explicit quit still asks before killing a live noquery process."
  (let ((chat-buf (generate-new-buffer "*piem-test-quit-chat*"))
        (input-buf (generate-new-buffer "*piem-test-quit-input*"))
        (proc (start-process "test-quit-noquery" nil "cat"))
        (asked nil))
    (unwind-protect
        (progn
          (set-process-query-on-exit-flag proc nil)
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--process proc)
            (piem--set-input-buffer input-buf))
          (with-current-buffer input-buf
            (piem-input-mode)
            (piem--set-chat-buffer chat-buf))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (_prompt)
                       (setq asked t)
                       nil)))
            (with-current-buffer chat-buf
              (should-error (piem-quit) :type 'user-error)))
          (should asked)
          (should (buffer-live-p chat-buf))
          (should (buffer-live-p input-buf))
          (should (process-live-p proc)))
      (when (process-live-p proc)
        (delete-process proc))
      (piem-test--kill-live-buffers input-buf chat-buf))))

(ert-deftest piem-test-process-exit-resets-busy-chat-and-restores-queue ()
  "Process exit leaves the frontend idle, visible, and restores queued work."
  (let ((chat-buf (get-buffer-create "*piem-test-process-exit-chat*"))
        (input-buf (get-buffer-create "*piem-test-process-exit-input*"))
        (stderr-buf (generate-new-buffer
                     " *piem-test-process-exit-stderr*"))
        (proc (start-process "test-process-exit" nil "sh" "-c" "exit 1")))
    (unwind-protect
        (progn
          (set-process-sentinel proc nil)
          (set-process-query-on-exit-flag proc nil)
          (should (piem-test-wait-for-process-exit proc))
          (with-current-buffer stderr-buf
            (insert "ECOMPROMISED: lock was compromised\n"
                    (make-string 5000 ?x)
                    "\nEND provider stack\n"))
          (process-put proc 'piem-stderr-buf stderr-buf)
          (process-put proc 'piem-chat-buffer chat-buf)
          (piem--register-display-handler proc)
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf))
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming
                  piem--activity-phase "replying"
                  piem--process proc
                  piem--input-buffer input-buf
                  piem--local-user-message "pending echo"
                  piem--followup-queue '("queued after crash")
                  piem--prompt-start-generation 7)
            (piem--handle-process-exit proc
                                                  "exited abnormally with code 1\n")
            (should (eq piem--status 'idle))
            (should (equal piem--activity-phase "idle"))
            (should (null piem--process))
            (should (null piem--local-user-message))
            (should (null piem--followup-queue))
            (should (> piem--prompt-start-generation 7))
            (should (equal (plist-get piem--state :last-error)
                           "Process exited: exited abnormally with code 1"))
            (let ((chat-text (buffer-string)))
              (should (string-match-p "pi process exited" chat-text))
              (should (string-match-p
                       "Process exited: exited abnormally with code 1"
                       chat-text))
              (should (string-match-p "Exit code: 1" chat-text))
              (should (string-match-p "ECOMPROMISED" chat-text))
              (should (string-match-p "stderr truncated" chat-text))
              (should (string-match-p "END provider stack" chat-text))
              (should (< (length chat-text) 4500))))
          (should-not (buffer-live-p stderr-buf))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "queued after crash"))))
      (when (buffer-live-p stderr-buf)
        (kill-buffer stderr-buf))
      (when (process-live-p proc)
        (delete-process proc))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-process-exit-without-stderr-is-visible ()
  "A current process exit remains visible when no stderr was captured."
  (let ((chat-buf (get-buffer-create
                   "*piem-test-process-exit-no-stderr-chat*"))
        (proc (start-process "test-process-exit-no-stderr" nil
                             "sh" "-c" "exit 2")))
    (unwind-protect
        (progn
          (set-process-sentinel proc nil)
          (set-process-query-on-exit-flag proc nil)
          (should (piem-test-wait-for-process-exit proc))
          (process-put proc 'piem-chat-buffer chat-buf)
          (piem--register-display-handler proc)
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--process proc)
            (piem--handle-process-exit proc
                                                  "exited abnormally with code 2\n")
            (let ((chat-text (buffer-string)))
              (should (string-match-p "pi process exited" chat-text))
              (should (string-match-p "Process exited" chat-text))
              (should (string-match-p "Exit code: 2" chat-text))
              (should-not (string-match-p "stderr:" chat-text)))))
      (when (process-live-p proc)
        (delete-process proc))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-process-exit-cleans-up-when-display-errors ()
  "Current process cleanup and queue restoration survive a display error."
  (let ((chat-buf (generate-new-buffer
                   "*piem-test-process-exit-display-error-chat*"))
        (input-buf (generate-new-buffer
                    "*piem-test-process-exit-display-error-input*"))
        (proc (start-process "test-process-exit-display-error" nil "cat"))
        (mode-line-updates 0))
    (unwind-protect
        (progn
          (set-process-query-on-exit-flag proc nil)
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf))
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'streaming
                  piem--activity-phase "replying"
                  piem--process proc
                  piem--input-buffer input-buf
                  piem--local-user-message "pending echo"
                  piem--pre-compaction-status 'streaming
                  piem--followup-queue '("restore after error")
                  piem--prompt-start-generation 11)
            (cl-letf (((symbol-function
                        'piem--display-process-exit-error)
                       (lambda (&rest _)
                         (error "display failed")))
                      ((symbol-function 'force-mode-line-update)
                       (lambda (&optional _all)
                         (setq mode-line-updates (1+ mode-line-updates)))))
              (should-error
               (piem--mark-process-exited
                proc '(:success :false
                       :error "Process exited: display failure"
                       :exitCode 1))
               :type 'error)
              (should (eq piem--status 'idle))
              (should (equal piem--activity-phase "idle"))
              (should (null piem--process))
              (should (null piem--local-user-message))
              (should (null piem--pre-compaction-status))
              (should (null piem--followup-queue))
              (should (> piem--prompt-start-generation 11))
              (should (>= mode-line-updates 2))))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "restore after error"))))
      (when (process-live-p proc)
        (delete-process proc))
      (piem-test--kill-live-buffers input-buf chat-buf))))

(ert-deftest piem-test-stale-process-exit-is-not-rendered ()
  "A registered process that is not current must not show a crash block."
  (let ((current-proc (start-process "test-current-process" nil "cat"))
        (stale-proc (start-process "test-stale-process" nil "cat")))
    (unwind-protect
        (with-temp-buffer
          (piem-chat-mode)
          (setq piem--process current-proc
                piem--status 'streaming
                piem--activity-phase "replying"
                piem--state '(:last-error "keep me")
                piem--local-user-message "in flight"
                piem--followup-queue '("still queued"))
          (process-put stale-proc 'piem-chat-buffer
                       (current-buffer))
          (piem--register-display-handler stale-proc)
          (funcall (process-get stale-proc 'piem-exit-handler)
                   '(:success :false
                     :error "Process exited: stale transition failed"
                     :exitCode 1
                     :stderr "ECOMPROMISED: stale process"))
          (should (eq piem--process current-proc))
          (should (eq piem--status 'streaming))
          (should (equal piem--activity-phase "replying"))
          (should (equal (plist-get piem--state :last-error)
                         "keep me"))
          (should (equal piem--local-user-message "in flight"))
          (should (equal piem--followup-queue '("still queued")))
          (should-not (string-match-p "pi process exited" (buffer-string))))
      (piem--unregister-display-handler stale-proc)
      (when (process-live-p current-proc)
        (delete-process current-proc))
      (when (process-live-p stale-proc)
        (delete-process stale-proc)))))

(ert-deftest piem-test-process-exit-restores-direct-pending-prompt ()
  "Process exit restores a direct prompt whose RPC preflight never completed."
  (let ((chat-buf (get-buffer-create "*piem-test-process-exit-direct-chat*"))
        (input-buf (get-buffer-create "*piem-test-process-exit-direct-input*"))
        (proc (start-process "test-process-exit-direct" nil "cat")))
    (unwind-protect
        (progn
          (set-process-sentinel proc nil)
          (set-process-query-on-exit-flag proc nil)
          (process-put proc 'piem-chat-buffer chat-buf)
          (piem--register-display-handler proc)
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--status 'idle
                  piem--process proc
                  piem--input-buffer input-buf
                  piem--prompt-start-generation 3))
          (with-current-buffer input-buf
            (piem-input-mode)
            (setq piem--chat-buffer chat-buf)
            (insert "please survive the crash")
            (piem-send)
            (should (string-empty-p (buffer-string))))
          (with-current-buffer chat-buf
            (should (eq piem--status 'sending))
            (should-not (string-match-p "please survive the crash" (buffer-string)))
            (piem--handle-process-exit proc "exited abnormally with code 1\n")
            (should (eq piem--status 'idle))
            (should (null piem--process))
            (should (string-match-p "Process exited"
                                    (plist-get piem--state :last-error))))
          (with-current-buffer input-buf
            (should (equal (buffer-string) "please survive the crash"))))
      (when (process-live-p proc)
        (delete-process proc))
      (kill-buffer chat-buf)
      (kill-buffer input-buf))))

(ert-deftest piem-test-abort-send-resets-activity-phase ()
  "Abort send resets activity phase and status to idle in CHAT-BUF."
  (let ((chat-buf (generate-new-buffer "*piem-chat:test-abort-send/*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--activity-phase "running"
                  piem--status 'streaming))
          ;; Simulate callback/sentinel context by calling from other buffer
          (with-temp-buffer
            (piem--abort-send chat-buf))
          (with-current-buffer chat-buf
            (should (equal piem--activity-phase "idle"))
            (should (eq piem--status 'idle))))
      (when (buffer-live-p chat-buf)
        (kill-buffer chat-buf)))))

(ert-deftest piem-test-working-message-in-header ()
  "Header line includes transient working message when set."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model "claude-sonnet-4")
          piem--working-message "📖 Skimming…")
    (let ((header (substring-no-properties (piem--header-line-string))))
      (should (string-match-p "Skimming" header)))))

(ert-deftest piem-test-header-no-pipes-when-minimal ()
  "Header has no pipe separators when only identity group is present."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model "claude-sonnet-4")
          piem--cached-stats nil
          piem--session-name nil
          piem--extension-status nil
          piem--working-message nil)
    (let ((header (substring-no-properties (piem--header-line-string))))
      (should-not (string-match-p "│" header)))))

(ert-deftest piem-test-header-pipes-collapse-correctly ()
  "Header renders only needed pipes when stats and context groups are set."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model (:name "claude-sonnet-4" :contextWindow 200000))
          piem--cached-stats '(:cost 0.05
                                   :contextUsage (:tokens 150 :contextWindow 200000 :percent 0.075))
          piem--session-name "My Session"
          piem--extension-status nil
          piem--working-message nil)
    (let ((header (substring-no-properties (piem--header-line-string)))
          (count 0)
          (start 0))
      (while (string-match "│" header start)
        (setq count (1+ count)
              start (match-end 0)))
      (should (= count 2)))))

(ert-deftest piem-test-header-all-groups-present ()
  "Header shows three group separators when all groups have content."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model (:name "claude-sonnet-4" :contextWindow 200000))
          piem--cached-stats '(:cost 0.05
                                   :contextUsage (:tokens 150 :contextWindow 200000 :percent 0.075))
          piem--session-name "My Session"
          piem--extension-status '(("ext" . "Git: synced"))
          piem--working-message "📖 Skimming…")
    (let ((header (substring-no-properties (piem--header-line-string)))
          (count 0)
          (start 0))
      (while (string-match "│" header start)
        (setq count (1+ count)
              start (match-end 0)))
      (should (= count 3))
      (should (string-match-p "My Session" header))
      (should (string-match-p "Git: synced · 📖 Skimming…" header)))))

(ert-deftest piem-test-header-extension-group-escapes-percent-signs ()
  "Extension header text escapes percent signs for header-line display."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model (:name "gpt-5.4" :contextWindow 200000))
          piem--extension-status '(("sub-status:usage" . "5h 4% · Week 3% · degraded"))
          piem--working-message "refresh 50%")
    (let ((header (substring-no-properties (piem--header-line-string))))
      (should (string-match-p "5h 4%% · Week 3%% · degraded" header))
      (should (string-match-p "refresh 50%%" header)))))

(ert-deftest piem-test-header-session-name-in-context-group ()
  "Context group shows session name when set, collapses when nil."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model "claude-sonnet-4")
          piem--session-name "Refactor auth")
    (let ((header (substring-no-properties (piem--header-line-string))))
      (should (string-match-p "Refactor auth" header))
      (should (string-match-p "│" header)))
    (setq piem--session-name nil)
    (let ((header (substring-no-properties (piem--header-line-string))))
      (should-not (string-match-p "│" header)))))

(ert-deftest piem-test-format-tokens-compact ()
  "Tokens formatted compactly."
  (should (equal "500" (piem--format-tokens-compact 500)))
  (should (equal "5k" (piem--format-tokens-compact 5000)))
  (should (equal "50k" (piem--format-tokens-compact 50000)))
  (should (equal "1.2M" (piem--format-tokens-compact 1200000))))

(ert-deftest piem-test-input-mode-has-header-line ()
  "Input mode sets up header-line-format."
  (with-temp-buffer
    (piem-input-mode)
    (should header-line-format)))

(ert-deftest piem-test-header-line-handles-model-plist ()
  "Header line handles model as plist with :name."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model (:name "claude-sonnet-4" :id "model-123")))
    (let ((header (piem--header-line-string)))
      (should (string-match-p "sonnet-4" header)))))

(ert-deftest piem-test-menu-model-description-buffer-local ()
  "Menu model description uses buffer-local model."
  (let ((buf-a (generate-new-buffer "*piem-chat:model-a*"))
        (buf-b (generate-new-buffer "*piem-chat:model-b*")))
    (unwind-protect
        (let (desc-a desc-b)
          (with-current-buffer buf-a
            (piem-chat-mode)
            (setq piem--state '(:model (:name "Alpha")))
            (setq desc-a (piem--menu-model-description)))
          (with-current-buffer buf-b
            (piem-chat-mode)
            (setq piem--state '(:model (:name "Beta")))
            (setq desc-b (piem--menu-model-description)))
          (should (equal (list desc-a desc-b)
                         '("Model: Alpha" "Model: Beta"))))
      (mapc #'kill-buffer (list buf-a buf-b)))))

(ert-deftest piem-test-select-model-updates-current-session-only ()
  "Selecting a model updates only the current session."
  (let* ((buf-a (generate-new-buffer "*piem-chat:model-select-a*"))
         (buf-b (generate-new-buffer "*piem-chat:model-select-b*"))
         (available-models (list (list :id "model-a" :name "Model A" :provider "test")
                                 (list :id "model-b" :name "Model B" :provider "test")))
         (selected-model (list :id "model-b" :name "Model B" :provider "test")))
    (unwind-protect
        (cl-letf (((symbol-function 'piem--rpc-sync)
                   (lambda (&rest _) (list :success t :data (list :models available-models))))
                  ((symbol-function 'piem--rpc-async)
                   (lambda (_proc _cmd callback)
                     (funcall callback (list :success t :command "set_model" :data selected-model))))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "Model B")))
          (with-current-buffer buf-a
            (piem-chat-mode)
            (setq piem--process :proc-a)
            (setq piem--state '(:model (:name "Model A" :id "model-a"))))
          (with-current-buffer buf-b
            (piem-chat-mode)
            (setq piem--process :proc-b)
            (setq piem--state '(:model (:name "Model B-old" :id "model-b-old"))))
          (with-current-buffer buf-a
            (piem-select-model))
          (let ((model-a (with-current-buffer buf-a
                           (plist-get (plist-get piem--state :model) :name)))
                (model-b (with-current-buffer buf-b
                           (plist-get (plist-get piem--state :model) :name))))
            (should (equal (list model-a model-b)
                           '("Model B" "Model B-old")))))
      (mapc (lambda (buf)
              (with-current-buffer buf
                (setq piem--process nil))
              (kill-buffer buf))
            (list buf-a buf-b)))))

(ert-deftest piem-test-update-state-refreshes-header ()
  "Updating state should trigger header-line refresh."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model (:name "old-model") :thinking-level "low"))
    (let ((header-before (piem--header-line-string)))
      ;; Simulate state update
      (setq piem--state '(:model (:name "new-model") :thinking-level "high"))
      (let ((header-after (piem--header-line-string)))
        ;; Header string should reflect new state
        (should (string-match-p "new-model" header-after))
        (should (string-match-p "high" header-after))))))

(ert-deftest piem-test-apply-state-response-updates-buffer ()
  "Apply state response updates buffer-local variables in correct buffer."
  (let ((chat-buf (generate-new-buffer "*test-apply-state*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (setq piem--state nil
                  piem--status nil))
          ;; Call from a different buffer to verify buffer context handling
          (with-temp-buffer
            (piem--apply-state-response
             chat-buf
             '(:success t :data (:isStreaming :false
                                 :sessionFile "/tmp/test.jsonl"
                                 :model "test-model"))))
          ;; Verify state was updated in chat-buf, not temp buffer
          (with-current-buffer chat-buf
            (should (eq piem--status 'idle))
            (should (equal (plist-get piem--state :session-file)
                           "/tmp/test.jsonl"))))
      (kill-buffer chat-buf))))

(ert-deftest piem-test-apply-state-response-handles-dead-buffer ()
  "Apply state response handles dead buffer gracefully."
  (let ((chat-buf (generate-new-buffer "*test-dead-buf*")))
    (kill-buffer chat-buf)
    ;; Should not error when buffer is dead
    (piem--apply-state-response
     chat-buf
     '(:success t :data (:sessionFile "/tmp/test.jsonl")))))

(ert-deftest piem-test-header-line-model-is-clickable ()
  "Model name in header-line has click properties."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model (:name "claude-sonnet-4")))
    (let ((header (piem--header-line-string)))
      ;; Should have local-map property
      (should (get-text-property 0 'local-map header))
      ;; Should have mouse-face for highlight
      (should (get-text-property 0 'mouse-face header)))))

(ert-deftest piem-test-header-line-thinking-is-clickable ()
  "Thinking level in header-line cycles on mouse click."
  (with-temp-buffer
    (piem-chat-mode)
    (setq piem--state '(:model (:name "test") :thinking-level "high"))
    (let* ((header (piem--header-line-string))
           ;; Find position of "high" in header
           (pos (string-match "high" header))
           (map (and pos (get-text-property pos 'local-map header))))
      (should pos)
      ;; Should have local-map at that position
      (should map)
      ;; Should have mouse-face for highlight
      (should (get-text-property pos 'mouse-face header))
      (should (eq (lookup-key map [header-line mouse-1])
                  #'piem-cycle-thinking))
      (should (eq (lookup-key map [header-line mouse-2])
                  #'piem-cycle-thinking)))))

(ert-deftest piem-test-header-format-context-returns-nil-when-no-window ()
  "Context format returns nil when context window is 0."
  (should (null (piem--header-format-context 25.0 0))))

(ert-deftest piem-test-header-format-context-shows-percentage ()
  "Context format shows percentage and window size."
  (let ((result (piem--header-format-context 25.0 200000)))
    (should (string-match-p "25.0%%" result))
    (should (string-match-p "200k" result))))

(ert-deftest piem-test-header-format-context-shows-unknown-when-percent-nil ()
  "Context format shows unknown usage when percentage is unavailable."
  (let ((result (piem--header-format-context nil 200000)))
    (should (string-match-p "\\?/200k" result))))

(ert-deftest piem-test-header-format-context-warning-over-70 ()
  "Context format uses warning face over 70%."
  (let ((result (piem--header-format-context 75.0 200000)))
    (should (eq (get-text-property 0 'face result) 'warning))))

(ert-deftest piem-test-header-format-context-error-over-90 ()
  "Context format uses error face over 90%."
  (let ((result (piem--header-format-context 95.0 200000)))
    (should (eq (get-text-property 0 'face result) 'error))))

(ert-deftest piem-test-message-end-refreshes-header-for-assistant ()
  "Assistant message_end refreshes header stats for fresher cost updates."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((refresh-count 0))
      (cl-letf (((symbol-function 'piem--refresh-header)
                 (lambda () (setq refresh-count (1+ refresh-count)))))
        (piem--handle-display-event
         '(:type "message_end"
           :message (:role "assistant"
                     :stopReason "stop"
                     :usage (:input 100 :output 50 :cacheRead 10 :cacheWrite 5)))))
      (should (= refresh-count 1)))))

(ert-deftest piem-test-message-end-does-not-refresh-header-for-user ()
  "User message_end does not trigger header stats refresh."
  (with-temp-buffer
    (piem-chat-mode)
    (let ((refresh-count 0))
      (cl-letf (((symbol-function 'piem--refresh-header)
                 (lambda () (setq refresh-count (1+ refresh-count)))))
        (piem--handle-display-event
         '(:type "message_end"
           :message (:role "user" :content "hello"))))
      (should (= refresh-count 0)))))

(ert-deftest piem-test-header-format-stats-returns-nil-when-no-stats ()
  "Stats format returns nil when stats is nil."
  (should (null (piem--header-format-stats nil))))

(ert-deftest piem-test-header-format-stats-shows-cost-and-context ()
  "Header stats shows cost and context percentage from contextUsage."
  (let* ((stats '(:cost 0.05
                  :contextUsage (:tokens 3500 :contextWindow 200000 :percent 1.75)))
         (result (piem--header-format-stats stats)))
    (should (string-match-p "\\$0.05" result))
    (should (string-match-p "1.8%%/200k" result))))

(ert-deftest piem-test-header-format-stats-no-context-without-context-usage ()
  "Header stats omit context display when contextUsage is absent.
Without contextUsage there is no context window to display against."
  (let* ((stats '(:cost 0.05))
         (result (piem--header-format-stats stats)))
    (should (string-match-p "\\$0.05" result))
    (should-not (string-match-p "\\?" result))))

(ert-deftest piem-test-header-format-stats-shows-unknown-when-tokens-null ()
  "Header stats show ? for context when contextUsage.tokens is :null.
This occurs after compaction before the next assistant message."
  (let* ((stats '(:cost 0.12
                  :contextUsage (:tokens :null :contextWindow 200000 :percent 0)))
         (result (piem--header-format-stats stats)))
    (should (string-match-p "\\$0.12" result))
    (should (string-match-p "\\?/200k" result))))

;;; File Reference Completion (@)

(ert-deftest piem-test-at-trigger-context ()
  "@ completion should only trigger at word boundaries, not in emails."
  (with-temp-buffer
    (piem-input-mode)
    ;; @ at start of buffer - should trigger
    (erase-buffer)
    (insert "@")
    (should (piem--at-trigger-p))
    ;; @ after space - should trigger
    (erase-buffer)
    (insert "hello @")
    (should (piem--at-trigger-p))
    ;; @ after newline - should trigger
    (erase-buffer)
    (insert "hello\n@")
    (should (piem--at-trigger-p))
    ;; @ after punctuation - should trigger
    (erase-buffer)
    (insert "see:@")
    (should (piem--at-trigger-p))
    ;; @ after alphanumeric (email) - should NOT trigger
    (erase-buffer)
    (insert "user@")
    (should-not (piem--at-trigger-p))
    ;; @ in middle of email - should NOT trigger
    (erase-buffer)
    (insert "test123@")
    (should-not (piem--at-trigger-p))))

(ert-deftest piem-test-file-reference-capf-returns-nil-without-at ()
  "File reference completion returns nil when not after @."
  (with-temp-buffer
    (piem-input-mode)
    (insert "hello world")
    (should-not (piem--file-reference-capf))))

(ert-deftest piem-test-file-reference-capf-returns-data-after-at ()
  "File reference completion returns data when point is after @."
  (with-temp-buffer
    (piem-input-mode)
    ;; Mock project files
    (setq piem--project-files-cache '("file1.el" "file2.py" "dir/file3.ts"))
    (setq piem--project-files-cache-time (float-time))
    (insert "Check @fi")
    (let ((result (piem--file-reference-capf)))
      (should result)
      ;; Start should be after @
      (should (= (nth 0 result) (- (point) 2)))  ; Position after @
      ;; End should be at point
      (should (= (nth 1 result) (point)))
      ;; Candidates should include matching files
      (should (member "file1.el" (nth 2 result)))
      (should (member "file2.py" (nth 2 result))))))

(ert-deftest piem-test-file-reference-capf-empty-prefix ()
  "File reference completion returns all files when no prefix after @."
  (with-temp-buffer
    (piem-input-mode)
    (setq piem--project-files-cache '("a.el" "b.py" "c.ts"))
    (setq piem--project-files-cache-time (float-time))
    (insert "See @")
    (let ((result (piem--file-reference-capf)))
      (should result)
      ;; Should return all files when prefix is empty
      (should (= (length (nth 2 result)) 3)))))

(ert-deftest piem-test-file-reference-capf-mid-line ()
  "File reference completion works in the middle of a line."
  (with-temp-buffer
    (piem-input-mode)
    (setq piem--project-files-cache '("test.el"))
    (setq piem--project-files-cache-time (float-time))
    (insert "Look at @te and also")
    (goto-char 11)  ; Position right after "@te"
    (let ((result (piem--file-reference-capf)))
      (should result)
      (should (member "test.el" (nth 2 result))))))

;;; Path Completion (Tab)

(ert-deftest piem-test-path-capf-returns-nil-for-non-path ()
  "Path completion returns nil for text that doesn't look like a path."
  (with-temp-buffer
    (piem-input-mode)
    (insert "hello world")
    (should-not (piem--path-capf))))

(ert-deftest piem-test-path-capf-returns-nil-for-non-prefixed-path ()
  "Path completion returns nil for paths without ./ ../ ~/ or / prefix."
  (with-temp-buffer
    (piem-input-mode)
    (insert "src/file.el")
    (should-not (piem--path-capf))))

(ert-deftest piem-test-path-capf-triggers-for-dot-slash ()
  "Path completion triggers for paths starting with ./"
  (let* ((temp-dir (make-temp-file "piem-path-test-" t))
         (test-file (expand-file-name "test.txt" temp-dir)))
    (unwind-protect
        (progn
          (with-temp-file test-file (insert "test"))
          (let ((default-directory temp-dir))
            (with-temp-buffer
              (piem-input-mode)
              (setq piem--chat-buffer (current-buffer))
              ;; Mock session directory
              (cl-letf (((symbol-function 'piem--session-directory)
                         (lambda () temp-dir)))
                (insert "./te")
                (let ((result (piem--path-capf)))
                  (should result)
                  ;; Should have candidates
                  (should (> (length (nth 2 result)) 0)))))))
      (delete-directory temp-dir t))))

(ert-deftest piem-test-path-capf-triggers-for-tilde ()
  "Path completion triggers for paths starting with ~/"
  (with-temp-buffer
    (piem-input-mode)
    (insert "~/")
    ;; Just verify it doesn't error and returns something
    ;; (actual completions depend on user's home directory)
    (let ((result (piem--path-capf)))
      ;; May return nil if ~ directory doesn't exist or has no completions
      ;; but should not error
      (should (or (null result) (listp result))))))

(ert-deftest piem-test-path-capf-triggers-for-absolute ()
  "Path completion triggers for absolute paths not at buffer start."
  (with-temp-buffer
    (piem-input-mode)
    (insert "see /tmp/")
    (let ((result (piem--path-capf)))
      (when result
        (should (listp (nth 2 result)))))))

(ert-deftest piem-test-path-capf-remote-absolute-uses-remote-root ()
  "Remote absolute path completion checks remote / but inserts /-style paths."
  (let ((default-directory "/ssh:pi-host:/home/pi/project/")
        (completion-base nil)
        (completion-dir nil)
        (directory-checks nil))
    (with-temp-buffer
      (piem-input-mode)
      (setq default-directory "/ssh:pi-host:/home/pi/project/")
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (path)
                   (push path directory-checks)
                   (string-prefix-p "/ssh:pi-host:" path)))
                ((symbol-function 'file-name-all-completions)
                 (lambda (base dir)
                   (setq completion-base base
                         completion-dir dir)
                   '("log/" "local"))))
        (insert "see /var/lo")
        (let* ((result (piem--path-capf))
               (candidates (nth 2 result))
               (annotation (plist-get (nthcdr 3 result) :annotation-function)))
          (should result)
          (should (equal completion-base "lo"))
          (should (equal completion-dir "/ssh:pi-host:/var/"))
          (should (equal candidates '("/var/log/" "/var/local")))
          (should-not (cl-some #'file-remote-p candidates))
          (should (equal (funcall annotation "/var/log/") " (dir)"))
          (should (equal (funcall annotation "/var/local") " (file)"))
          (should (member "/ssh:pi-host:/var/" directory-checks))
          (should-not (member "/ssh:pi-host:/var/log/" directory-checks))
          (should-not (member "/var/" directory-checks)))))))

(ert-deftest piem-test-path-capf-multi-hop-absolute-keeps-full-route ()
  "Remote absolute path completion checks the full multi-hop TRAMP route."
  (let ((completion-base nil)
        (completion-dir nil)
        (directory-checks nil)
        (session-dir "/ssh:bastion|sudo:root@pi-host:/home/pi/project/"))
    (with-temp-buffer
      (piem-input-mode)
      (cl-letf (((symbol-function 'piem--session-directory)
                 (lambda () session-dir))
                ((symbol-function 'file-directory-p)
                 (lambda (path)
                   (push path directory-checks)
                   (equal path "/ssh:bastion|sudo:root@pi-host:/var/")))
                ((symbol-function 'file-name-all-completions)
                 (lambda (base dir)
                   (setq completion-base base
                         completion-dir dir)
                   '("log/" "local"))))
        (insert "see /var/lo")
        (let* ((result (piem--path-capf))
               (candidates (nth 2 result)))
          (should result)
          (should (equal completion-base "lo"))
          (should (equal completion-dir
                         "/ssh:bastion|sudo:root@pi-host:/var/"))
          (should (equal candidates '("/var/log/" "/var/local")))
          (should (member "/ssh:bastion|sudo:root@pi-host:/var/"
                          directory-checks)))))))

(ert-deftest piem-test-path-completions-returns-nil-for-unsafe-input ()
  "Path completion treats malformed path text as no completions."
  (let ((bad-path (concat "/tmp/bad" (string ?\0) "name")))
    (should-not (piem--path-completions bad-path))))

(ert-deftest piem-test-path-completions-excludes-dot-entries ()
  "Path completions should not include ./ or ../ entries."
  (let* ((temp-dir (make-temp-file "piem-path-test-" t))
         (subdir (expand-file-name "subdir" temp-dir)))
    (unwind-protect
        (progn
          (make-directory subdir)
          (cl-letf (((symbol-function 'piem--session-directory)
                     (lambda () temp-dir)))
            (let ((completions (piem--path-completions "./")))
              ;; Should have the subdir
              (should (member "./subdir/" completions))
              ;; Should NOT have ./ or ../
              (should-not (member "./" completions))
              (should-not (member "./../" completions))
              (should-not (member "././" completions)))))
      (delete-directory temp-dir t))))

(ert-deftest piem-test-complete-command-exists ()
  "piem-complete should be an interactive command."
  (should (commandp 'piem-complete)))

(ert-deftest piem-test-path-capf-skips-slash-at-buffer-start ()
  "Path completion skips / at buffer start to allow slash commands."
  (with-temp-buffer
    (piem-input-mode)
    (insert "/tmp")
    (should-not (piem--path-capf))))

(ert-deftest piem-test-path-capf-allows-slash-on-later-lines ()
  "Path completion works for / on lines after the first."
  (with-temp-buffer
    (piem-input-mode)
    (insert "Check this file:\n/tmp/")
    (let ((result (piem--path-capf)))
      (when result
        (should (listp (nth 2 result)))))))

(ert-deftest piem-test-tool-start-creates-overlay ()
  "tool_execution_start creates an overlay with pending face."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--display-tool-start "bash" '(:command "ls"))
    ;; Should have an overlay with piem-tool-block property
    (goto-char (point-min))
    (let* ((overlays (overlays-at (point)))
           (tool-ov (seq-find (lambda (o) (overlay-get o 'piem-tool-block)) overlays)))
      (should tool-ov)
      (should (eq (overlay-get tool-ov 'face) 'piem-tool-block))
      (should (equal (overlay-get tool-ov 'piem-tool-name) "bash")))))

(ert-deftest piem-test-tool-start-header-format ()
  "tool_execution_start uses simple header format, not drawer syntax."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--display-tool-start "bash" '(:command "ls -la"))
    ;; Should have "$ ls -la" header
    (should (string-match-p "\\$ ls -la" (buffer-string)))
    ;; Should NOT have drawer syntax
    (should-not (string-match-p ":BASH:" (buffer-string)))))

(ert-deftest piem-test-tool-end-keeps-overlay-face ()
  "tool_execution_end keeps base face on success."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--display-tool-start "bash" '(:command "ls"))
    ;; Initially base face
    (let ((ov (car (overlays-at (point-min)))))
      (should (eq (overlay-get ov 'face) 'piem-tool-block)))
    ;; After success — face stays the same
    (piem--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "file.txt"))
                          nil nil)
    (let ((ov (car (overlays-at (point-min)))))
      (should (eq (overlay-get ov 'face) 'piem-tool-block)))))

(ert-deftest piem-test-tool-end-error-face ()
  "tool_execution_end sets error face on failure."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--display-tool-start "bash" '(:command "bad"))
    (piem--display-tool-end "bash" '(:command "bad")
                          '((:type "text" :text "error"))
                          nil t)  ; is-error = t
    (let ((ov (car (overlays-at (point-min)))))
      (should (eq (overlay-get ov 'face) 'piem-tool-block-error)))))

(ert-deftest piem-test-tool-end-no-drawer-syntax ()
  "tool_execution_end does not insert :END: marker."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--display-tool-start "bash" '(:command "ls"))
    (piem--display-tool-end "bash" '(:command "ls")
                          '((:type "text" :text "output"))
                          nil nil)
    (should-not (string-match-p ":END:" (buffer-string)))))

(ert-deftest piem-test-tool-overlay-does-not-extend-to-subsequent-content ()
  "Tool overlay should not extend when content is inserted after tool block.
Regression test: overlay with rear-advance was extending to subsequent content."
  (with-temp-buffer
    (piem-chat-mode)
    ;; Create a complete tool block
    (piem--display-tool-start "write" '(:path "/tmp/test.txt" :content "hello"))
    (piem--display-tool-end "write" '(:path "/tmp/test.txt" :content "hello")
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
                          (lambda (ov) (overlay-get ov 'piem-tool-block))
                          overlays)))
      (should-not tool-overlay))))

(ert-deftest piem-test-abort-mid-tool-cleans-up-overlay ()
  "Aborting mid-tool should clean up the pending overlay.
When abort happens during tool execution, tool_execution_end never arrives.
display-agent-end must finalize the pending overlay with error face."
  (with-temp-buffer
    (piem-chat-mode)
    ;; Start a tool (creates pending overlay)
    (piem--display-tool-start "bash" '(:command "sleep 100"))
    ;; Verify overlay is pending
    (should piem--pending-tool-overlay)
    (should (eq (overlay-get piem--pending-tool-overlay 'face)
                'piem-tool-block))
    ;; Simulate abort - display-agent-end is called WITHOUT tool-end
    (setq piem--aborted t)
    (piem--display-agent-end)
    ;; Pending overlay variable should be nil
    (should-not piem--pending-tool-overlay)
    ;; But there should still be a finalized overlay with error face
    (goto-char (point-min))
    (let* ((overlays (overlays-at (point)))
           (tool-ov (seq-find (lambda (o) (overlay-get o 'piem-tool-block)) overlays)))
      (should tool-ov)
      (should (eq (overlay-get tool-ov 'face) 'piem-tool-block-error)))
    ;; Content inserted after should NOT be inside the overlay
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "AFTER_ABORT_CONTENT\n"))
    (let* ((new-content-pos (- (point-max) 10))
           (overlays (overlays-at new-content-pos))
           (tool-overlay (seq-find
                          (lambda (ov) (overlay-get ov 'piem-tool-block))
                          overlays)))
      (should-not tool-overlay))))

(ert-deftest piem-test-delta-no-transform-inside-code-block ()
  "Hash inside fenced code block should NOT be transformed."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--display-agent-start)
    (piem--display-message-delta "```python\n# This is a comment\n```")
    ;; The # inside code block should stay as single #
    (should (string-match-p "^# This is a comment$" (buffer-string)))))

(ert-deftest piem-test-delta-transform-resumes-after-code-block ()
  "Headings after code block closes should be transformed."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--display-agent-start)
    (piem--display-message-delta "```\n# comment\n```\n# Real Heading")
    ;; Inside block: stays #
    (should (string-match-p "^# comment$" (buffer-string)))
    ;; After block: becomes ##
    (should (string-match-p "^## Real Heading" (buffer-string)))))

(ert-deftest piem-test-delta-code-fence-split-across-deltas ()
  "Code fence split across deltas still detected."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--display-agent-start)
    (piem--display-message-delta "``")
    (piem--display-message-delta "`python\n# comment\n```")
    ;; Should recognize the split ``` and not transform inside
    (should (string-match-p "^# comment$" (buffer-string)))))

(ert-deftest piem-test-delta-backticks-mid-line-not-fence ()
  "Backticks mid-line don't trigger code block state."
  (with-temp-buffer
    (piem-chat-mode)
    (piem--display-agent-start)
    (piem--display-message-delta "Use ```code``` inline\n# Heading")
    ;; Inline backticks shouldn't affect heading transform
    (should (string-match-p "^## Heading" (buffer-string)))))

;;; Input Mode — Markdown Highlighting

(ert-deftest piem-test-input-mode-md-ts-by-default ()
  "By default, input mode has tree-sitter markdown font-lock."
  (with-temp-buffer
    (piem-input-mode)
    (should (derived-mode-p 'piem-input-mode))
    (insert "some **bold** text")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "bold")
    (should (memq 'bold
                  (let ((f (get-text-property (1- (point)) 'face)))
                    (if (listp f) f (list f)))))))

(ert-deftest piem-test-input-mode-no-metadata-face ()
  "With markdown highlighting, lines ending with colon have no metadata face.
Tree-sitter markdown doesn't have metadata face, so this verifies
no spurious faces are applied to plain colon-ending lines."
  (with-temp-buffer
    (piem-input-mode)
    (insert "Fix the bug:\n- item\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (let ((f (get-text-property (point) 'face)))
      ;; No heading, bold, or other markdown face on plain text
      (should-not (and f (not (eq f 'default)))))))

(ert-deftest piem-test-input-mode-no-hidden-markup ()
  "Input mode does NOT hide markup, even when user customizes it globally."
  (with-temp-buffer
    (let ((old-default (default-value 'md-ts-hide-markup)))
      (unwind-protect
          (progn
            (setq-default md-ts-hide-markup t)
            (piem-input-mode)
            (should-not md-ts-hide-markup)
            (insert "some **bold** text")
            (font-lock-ensure)
            (goto-char (point-min))
            (search-forward "**")
            (should-not (get-text-property (1- (point)) 'invisible)))
        (setq-default md-ts-hide-markup old-default)))))

(ert-deftest piem-test-input-mode-no-fontification-without-markdown ()
  "Without markdown highlighting, bold text gets no bold face."
  (with-temp-buffer
    (let ((piem-input-markdown-highlighting nil))
      (piem-input-mode)
      (insert "some **bold** text")
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "bold")
      (should-not (memq 'bold
                        (let ((f (get-text-property (1- (point)) 'face)))
                          (if (listp f) f (list f))))))))

(ert-deftest piem-test-input-mode-keybindings ()
  "Pi input keybindings are active in input mode."
  (with-temp-buffer
    (piem-input-mode)
    (should (eq (key-binding (kbd "C-c C-c")) 'piem-send))
    (should (eq (key-binding (kbd "C-c C-k")) 'piem-abort))
    (should (eq (key-binding (kbd "C-c C-p")) 'piem-menu))
    (should (eq (key-binding (kbd "C-c C-r"))
                'piem-session-browser))
    (should (eq (key-binding (kbd "M-p")) 'piem-previous-input))
    (should (eq (key-binding (kbd "M-n")) 'piem-next-input))
    (should (eq (key-binding (kbd "TAB")) 'piem-complete))
    (should (eq (key-binding (kbd "C-c C-s")) 'piem-queue-steering))))

;;; Input-Buffer Chat Navigation

(ert-deftest piem-test-input-next-message-moves-chat ()
  "Input-side next-message moves the linked chat and keeps focus."
  (let ((chat-buf (generate-new-buffer "*test-chat*"))
        (input-buf (generate-new-buffer "*test-input*")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer chat-buf)
          (with-current-buffer chat-buf
            (piem-chat-mode)
            (let ((inhibit-read-only t))
              (piem-test--insert-chat-turns))
            (piem--set-input-buffer input-buf)
            (goto-char (point-min)))
          (let ((input-win (split-window nil -10 'below)))
            (set-window-buffer input-win input-buf)
            (with-current-buffer input-buf
              (piem-input-mode)
              (piem--set-chat-buffer chat-buf))
            (select-window input-win)
            (piem-input-next-message)
            (with-current-buffer chat-buf
              (should (looking-at "You · 10:00")))
            (should (eq (window-buffer (selected-window)) input-buf))))
      (kill-buffer chat-buf)
      (kill-buffer input-buf)
      (delete-other-windows))))

(ert-deftest piem-test-input-previous-message-moves-linked-chat ()
  "Input-side previous-message uses the linked chat, not scroll state."
  (let ((chat-a (generate-new-buffer "*test-chat-a*"))
        (chat-b (generate-new-buffer "*test-chat-b*"))
        (input-buf (generate-new-buffer "*test-input*")))
    (unwind-protect
        (progn
          (delete-other-windows)
          (switch-to-buffer chat-a)
          (let* ((chat-win-a (selected-window))
                 (input-win (split-window chat-win-a -10 'below))
                 (chat-win-b (split-window chat-win-a nil 'right)))
            (set-window-buffer input-win input-buf)
            (set-window-buffer chat-win-b chat-b)
            (with-current-buffer chat-a
              (piem-chat-mode)
              (let ((inhibit-read-only t))
                (piem-test--insert-chat-turns))
              (goto-char (point-max))
              (re-search-backward "^You · 10:10$" nil t))
            (with-current-buffer chat-b
              (piem-chat-mode)
              (let ((inhibit-read-only t))
                (piem-test--insert-chat-turns))
              (goto-char (point-max))
              (re-search-backward "^You · 10:10$" nil t))
            (with-current-buffer input-buf
              (piem-input-mode)
              (piem--set-chat-buffer chat-a)
              (setq-local other-window-scroll-buffer chat-b))
            (select-window input-win)
            (piem-input-previous-message)
            (with-current-buffer chat-a
              (should (looking-at "You · 10:05")))
            (with-current-buffer chat-b
              (should (looking-at "You · 10:10")))
            (should (eq (window-buffer (selected-window)) input-buf))))
      (kill-buffer chat-a)
      (kill-buffer chat-b)
      (kill-buffer input-buf)
      (delete-other-windows))))

(ert-deftest piem-test-input-previous-message-no-chat-window-errors ()
  "Navigating from input without a visible linked chat signals error."
  (let ((chat-buf (generate-new-buffer "*test-chat-hidden*")))
    (unwind-protect
        (with-temp-buffer
          (piem-input-mode)
          (piem--set-chat-buffer chat-buf)
          (should-error (piem-input-previous-message)
                        :type 'user-error))
      (kill-buffer chat-buf))))

(provide 'piem-input-test)
;;; piem-input-test.el ends here
