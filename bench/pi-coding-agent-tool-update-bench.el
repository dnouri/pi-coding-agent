;;; pi-coding-agent-tool-update-bench.el --- Tool-update storm benchmarks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Deterministic tool_execution_update storm benchmarks for pi-coding-agent.
;; A real session backed by `bench/fake-pi-tool-update-storm.py' renders a
;; synthetic long-session fill phase and then a burst-patterned storm of
;; subagent progress updates.  The harness measures how the stock frontend
;; copes: per-event handling time via around-advice on
;; `pi-coding-agent--handle-display-event', and user-perceived main-thread
;; blocking via a 50 ms probe timer's lateness, and tool block re-render
;; counts via advice on `pi-coding-agent--tool-block-replace-body' and
;; `pi-coding-agent--display-tool-end' (the environment-independent
;; coalescing metric).  All content is synthetic; no private session files
;; are read.
;;
;; Run with:
;;
;;   make bench-tool-update            # GUI via xvfb, primary lane
;;   make bench-tool-update-batch      # --batch, secondary lane
;;   make bench-tool-update-smoke      # cheap correctness smoke
;;
;; or directly through `bench/run-tool-update-bench.sh'.
;;
;; The primary lane is GUI/xvfb because the measured cost is dominated by
;; buffer mutation plus redisplay/fontification, which batch mode cannot
;; reproduce.  Batch numbers are useful for CI trend artifacts.  The run
;; fails on correctness violations (lost or duplicated tool blocks and
;; events), not on timing thresholds.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(defconst pi-coding-agent-tu-bench-repo-root
  (file-name-as-directory
   (expand-file-name ".."
                     (file-name-directory
                      (or load-file-name buffer-file-name default-directory))))
  "Repository root containing the tool-update benchmark files.")

(add-to-list 'load-path pi-coding-agent-tu-bench-repo-root)
(require 'pi-coding-agent)

(defun pi-coding-agent-tu-bench--env (name default)
  "Return environment variable NAME, or DEFAULT when it is unset or empty."
  (let ((value (getenv name)))
    (if (and value (not (string-empty-p value))) value default)))

(defun pi-coding-agent-tu-bench--env-int (name default)
  "Return environment variable NAME as an integer, or DEFAULT."
  (string-to-number (pi-coding-agent-tu-bench--env
                     name (number-to-string default))))

(defun pi-coding-agent-tu-bench--env-float (name default)
  "Return environment variable NAME as a float, or DEFAULT."
  (string-to-number (pi-coding-agent-tu-bench--env
                     name (number-to-string default))))

(defun pi-coding-agent-tu-bench--truthy-env-p (name default)
  "Return non-nil when environment variable NAME is truthy.
DEFAULT is used when NAME is unset."
  (let ((value (downcase (pi-coding-agent-tu-bench--env name default))))
    (and (member value '("1" "true" "yes" "on")) t)))

(defun pi-coding-agent-tu-bench--json-bool (value)
  "Return VALUE encoded as a JSON boolean sentinel."
  (if value t :json-false))

(defvar pi-coding-agent-tu-bench-scenario
  (pi-coding-agent-tu-bench--env "PI_TU_BENCH_SCENARIO" "standalone")
  "Scenario label written into tool-update benchmark artifacts.")

(defvar pi-coding-agent-tu-bench-iteration
  (pi-coding-agent-tu-bench--env-int "PI_TU_BENCH_ITERATION" 1)
  "Iteration number written into tool-update benchmark artifacts.")

(defvar pi-coding-agent-tu-bench-fill-bash
  (pi-coding-agent-tu-bench--env-int "PI_TU_BENCH_FILL_BASH" 58)
  "Number of completed bash tool executions in the synthetic fill phase.")

(defvar pi-coding-agent-tu-bench-fill-read
  (pi-coding-agent-tu-bench--env-int "PI_TU_BENCH_FILL_READ" 5)
  "Number of completed read tool executions in the synthetic fill phase.")

(defvar pi-coding-agent-tu-bench-fill-write
  (pi-coding-agent-tu-bench--env-int "PI_TU_BENCH_FILL_WRITE" 2)
  "Number of completed write tool executions in the synthetic fill phase.")

(defvar pi-coding-agent-tu-bench-fill-edit
  (pi-coding-agent-tu-bench--env-int "PI_TU_BENCH_FILL_EDIT" 1)
  "Number of completed edit tool executions in the synthetic fill phase.")

(defvar pi-coding-agent-tu-bench-fill-output-lines
  (pi-coding-agent-tu-bench--env-int "PI_TU_BENCH_FILL_OUTPUT_LINES" 20)
  "Number of synthetic output lines per fill phase tool result.")

(defvar pi-coding-agent-tu-bench-updates
  (pi-coding-agent-tu-bench--env-int "PI_TU_BENCH_UPDATES" 400)
  "Number of storm phase tool_execution_update events.")

(defvar pi-coding-agent-tu-bench-parallel-tools
  (pi-coding-agent-tu-bench--env-int "PI_TU_BENCH_PARALLEL_TOOLS" 3)
  "Number of parallel subagent tool executions in the storm phase.")

(defvar pi-coding-agent-tu-bench-gap-scale
  (pi-coding-agent-tu-bench--env-float "PI_TU_BENCH_GAP_SCALE" 1.0)
  "Gap scale factor applied to storm pauses; recorded for artifacts only.
The fake backend applies the scale when scheduling events.")

(defvar pi-coding-agent-tu-bench-seed
  (pi-coding-agent-tu-bench--env-int "PI_TU_BENCH_SEED" 20240817)
  "PRNG seed for the fake backend's gap pattern; recorded for artifacts.")

(defvar pi-coding-agent-tu-bench-timeout-seconds
  (pi-coding-agent-tu-bench--env-int "PI_TU_BENCH_TIMEOUT_SECONDS" 240)
  "Timeout in seconds for waiting on the storm to settle.")

(defvar pi-coding-agent-tu-bench-display-buffers
  (pi-coding-agent-tu-bench--truthy-env-p "PI_TU_BENCH_DISPLAY" "0")
  "Whether GUI benchmark runs should display chat and input windows.")

(defvar pi-coding-agent-tu-bench-probe-interval
  0.05
  "Probe timer interval in seconds for measuring main thread blocking.")

(defvar pi-coding-agent-tu-bench-out-dir
  (file-name-as-directory
   (expand-file-name (pi-coding-agent-tu-bench--env
                      "PI_TU_BENCH_OUT_DIR"
                      "tmp/tool-update-bench/standalone")
                     pi-coding-agent-tu-bench-repo-root))
  "Output directory for one tool-update benchmark iteration.")

(defvar pi-coding-agent-tu-bench-runner-out-dir
  (file-name-as-directory
   (expand-file-name (pi-coding-agent-tu-bench--env
                      "PI_TU_BENCH_RUNNER_OUT_DIR"
                      pi-coding-agent-tu-bench-out-dir)
                     pi-coding-agent-tu-bench-repo-root))
  "Top-level runner output directory for reproduction commands.")

(defvar pi-coding-agent-tu-bench-fake-pi
  (expand-file-name "bench/fake-pi-tool-update-storm.py"
                    pi-coding-agent-tu-bench-repo-root)
  "Fake pi RPC executable used by tool-update benchmark runs.")

(defvar pi-coding-agent-tu-bench-fake-log
  (expand-file-name "fake-pi.jsonl" pi-coding-agent-tu-bench-out-dir)
  "Content-free fake RPC log path for one benchmark run.")

(defvar pi-coding-agent-tu-bench-result-file
  (expand-file-name "result.json" pi-coding-agent-tu-bench-out-dir)
  "JSON result artifact path for one benchmark run.")

(defvar pi-coding-agent-tu-bench-report-file
  (expand-file-name "report.md" pi-coding-agent-tu-bench-out-dir)
  "Markdown report artifact path for one benchmark run.")

(defvar pi-coding-agent-tu-bench-times-file
  (expand-file-name "times.tsv" pi-coding-agent-tu-bench-out-dir)
  "Per-event-type timing artifact path for one benchmark run.")

(defvar pi-coding-agent-tu-bench--event-log nil
  "List of (TYPE . ELAPSED-MS) entries for handled display events.")

(defvar pi-coding-agent-tu-bench--prompt-time nil
  "Float time when the storm prompt was sent.")

(defvar pi-coding-agent-tu-bench--agent-end-time nil
  "Float time when the agent_end event finished handling, or nil.")

(defvar pi-coding-agent-tu-bench--probe-expected nil
  "Float time the next probe firing was expected, or nil.")

(defvar pi-coding-agent-tu-bench--probe-lateness nil
  "List of probe firing lateness values in seconds.")

(defvar pi-coding-agent-tu-bench--probe-timer nil
  "The probe timer object, or nil.")

(defvar pi-coding-agent-tu-bench--render-log nil
  "Hash table of render operation metrics.
Keys are \"operation\\ttool-call-id\" strings; values are (COUNT TOTAL-MS
MAX-MS) lists.  This is the environment-independent coalescing metric: it
counts how often the frontend re-renders tool block bodies, independent of
how expensive each render is on the host.")

(defun pi-coding-agent-tu-bench--around-handle-event (orig event)
  "Around advice recording handling time for display EVENT.
Calls ORIG with EVENT and appends to the benchmark event log."
  (let ((start (float-time)))
    (prog1 (funcall orig event)
      (let ((type (or (plist-get event :type) "unknown")))
        (push (cons type (* 1000.0 (- (float-time) start)))
              pi-coding-agent-tu-bench--event-log)
        (when (equal type "agent_end")
          (setq pi-coding-agent-tu-bench--agent-end-time (float-time)))))))

(defun pi-coding-agent-tu-bench--install-advice ()
  "Install the per-event timing and render counting advice."
  (advice-add 'pi-coding-agent--handle-display-event
              :around #'pi-coding-agent-tu-bench--around-handle-event)
  (advice-add 'pi-coding-agent--tool-block-replace-body
              :around #'pi-coding-agent-tu-bench--around-replace-body)
  (advice-add 'pi-coding-agent--display-tool-end
              :around #'pi-coding-agent-tu-bench--around-display-tool-end))

(defun pi-coding-agent-tu-bench--remove-advice ()
  "Remove all benchmark advice."
  (advice-remove 'pi-coding-agent--handle-display-event
                 #'pi-coding-agent-tu-bench--around-handle-event)
  (advice-remove 'pi-coding-agent--tool-block-replace-body
                 #'pi-coding-agent-tu-bench--around-replace-body)
  (advice-remove 'pi-coding-agent--display-tool-end
                 #'pi-coding-agent-tu-bench--around-display-tool-end))

(defun pi-coding-agent-tu-bench--record-render (operation tool-call-id
                                                          elapsed-ms)
  "Record one OPERATION render for TOOL-CALL-ID taking ELAPSED-MS."
  (unless (hash-table-p pi-coding-agent-tu-bench--render-log)
    (setq pi-coding-agent-tu-bench--render-log (make-hash-table :test 'equal)))
  (let* ((key (format "%s\t%s" operation (or tool-call-id "(unkeyed)")))
         (row (gethash key pi-coding-agent-tu-bench--render-log)))
    (if row
        (progn
          (cl-incf (car row))
          (cl-incf (cadr row) elapsed-ms)
          (setf (caddr row) (max (caddr row) elapsed-ms)))
      (puthash key (list 1 elapsed-ms elapsed-ms)
               pi-coding-agent-tu-bench--render-log))))

(defun pi-coding-agent-tu-bench--around-replace-body (orig block &rest args)
  "Around advice counting and timing a body replacement for BLOCK.
Calls ORIG with BLOCK and ARGS."
  (let ((start (float-time)))
    (prog1 (apply orig block args)
      (pi-coding-agent-tu-bench--record-render
       "replace-body"
       (and block (pi-coding-agent--tool-block-tool-call-id block))
       (* 1000.0 (- (float-time) start))))))

(defun pi-coding-agent-tu-bench--around-display-tool-end (orig &rest args)
  "Around advice counting and timing one tool end display.
Calls ORIG with ARGS; the block comes from ARGS or the current block."
  (let ((start (float-time))
        (block (or (nth 5 args) (pi-coding-agent--current-tool-block))))
    (prog1 (apply orig args)
      (pi-coding-agent-tu-bench--record-render
       "display-tool-end"
       (and block (pi-coding-agent--tool-block-tool-call-id block))
       (* 1000.0 (- (float-time) start))))))

(defun pi-coding-agent-tu-bench--render-rows ()
  "Return render metric rows sorted by operation, then descending count.
Each row is a plist with :operation :toolCallId :count :totalMs :meanMs
and :maxMs."
  (let (rows)
    (maphash
     (lambda (key cell)
       (let ((parts (split-string key "\t")))
         (push (list :operation (car parts)
                     :toolCallId (cadr parts)
                     :count (car cell)
                     :totalMs (cadr cell)
                     :meanMs (/ (cadr cell) (car cell))
                     :maxMs (caddr cell))
               rows)))
     pi-coding-agent-tu-bench--render-log)
    (sort rows (lambda (a b)
                 (if (equal (plist-get a :operation) (plist-get b :operation))
                     (> (plist-get a :count) (plist-get b :count))
                   (string< (plist-get a :operation)
                            (plist-get b :operation)))))))

(defun pi-coding-agent-tu-bench--render-count (operation tool-call-id)
  "Return how often OPERATION was recorded for TOOL-CALL-ID."
  (let ((row (and (hash-table-p pi-coding-agent-tu-bench--render-log)
                  (gethash (format "%s\t%s" operation tool-call-id)
                           pi-coding-agent-tu-bench--render-log))))
    (if row (car row) 0)))

(defun pi-coding-agent-tu-bench--render-total (operation)
  "Return the total number of recorded OPERATION renders."
  (cl-loop for row in (pi-coding-agent-tu-bench--render-rows)
           when (equal (plist-get row :operation) operation)
           sum (plist-get row :count)))

(defun pi-coding-agent-tu-bench--render-operation-json (operation)
  "Return aggregate and per-tool-call-id metrics for OPERATION as a plist."
  (let ((rows (seq-filter (lambda (row)
                            (equal (plist-get row :operation) operation))
                          (pi-coding-agent-tu-bench--render-rows))))
    (list :total (cl-loop for row in rows sum (plist-get row :count))
          :totalMs (cl-loop for row in rows sum (plist-get row :totalMs))
          :maxMs (if rows
                     (apply #'max (mapcar (lambda (row)
                                            (plist-get row :maxMs))
                                          rows))
                   0.0)
          :perToolCallId (mapcar (lambda (row)
                                   (cons (plist-get row :toolCallId)
                                         (plist-get row :count)))
                                 rows))))

(defun pi-coding-agent-tu-bench--probe-tick ()
  "Record how late this probe firing is relative to its expected time."
  (let ((now (float-time)))
    (when pi-coding-agent-tu-bench--probe-expected
      (push (max 0.0 (- now pi-coding-agent-tu-bench--probe-expected))
            pi-coding-agent-tu-bench--probe-lateness))
    (setq pi-coding-agent-tu-bench--probe-expected
          (+ now pi-coding-agent-tu-bench-probe-interval))))

(defun pi-coding-agent-tu-bench--start-probe ()
  "Start the probe timer and reset its state."
  (setq pi-coding-agent-tu-bench--probe-expected nil
        pi-coding-agent-tu-bench--probe-lateness nil)
  (setq pi-coding-agent-tu-bench--probe-timer
        (run-with-timer 0 pi-coding-agent-tu-bench-probe-interval
                        #'pi-coding-agent-tu-bench--probe-tick)))

(defun pi-coding-agent-tu-bench--stop-probe ()
  "Cancel the probe timer when it is running."
  (when pi-coding-agent-tu-bench--probe-timer
    (cancel-timer pi-coding-agent-tu-bench--probe-timer)
    (setq pi-coding-agent-tu-bench--probe-timer nil)))

(defun pi-coding-agent-tu-bench--event-count (type)
  "Return the number of handled display events of TYPE."
  (cl-count type pi-coding-agent-tu-bench--event-log
            :key #'car :test #'equal))

(defun pi-coding-agent-tu-bench--event-stats ()
  "Return per-event-type handling stats sorted by descending total ms.
Each row is a plist with :type :count :totalMs :meanMs and :maxMs."
  (let ((table (make-hash-table :test 'equal))
        (rows nil))
    (dolist (entry pi-coding-agent-tu-bench--event-log)
      (let ((cell (gethash (car entry) table)))
        (if cell
            (progn
              (cl-incf (car cell))
              (cl-incf (cadr cell) (cdr entry))
              (setf (caddr cell) (max (caddr cell) (cdr entry))))
          (puthash (car entry) (list 1 (cdr entry) (cdr entry)) table))))
    (maphash (lambda (type cell)
               (push (list :type type
                           :count (car cell)
                           :totalMs (cadr cell)
                           :meanMs (/ (cadr cell) (car cell))
                           :maxMs (caddr cell))
                     rows))
             table)
    (sort rows (lambda (a b) (> (plist-get a :totalMs)
                                (plist-get b :totalMs))))))

(defun pi-coding-agent-tu-bench--percentile (samples p)
  "Return the P percentile of SAMPLES as a fraction between 0 and 1."
  (let* ((sorted (sort (copy-sequence samples) #'<))
         (n (length sorted)))
    (if (zerop n)
        0.0
      (nth (min (1- n) (floor (* p n))) sorted))))

(defun pi-coding-agent-tu-bench--probe-stats ()
  "Return probe timer statistics as a plist of millisecond values."
  (let ((lateness pi-coding-agent-tu-bench--probe-lateness))
    (list :intervalMs (* 1000.0 pi-coding-agent-tu-bench-probe-interval)
          :fires (length lateness)
          :p50Ms (* 1000.0 (pi-coding-agent-tu-bench--percentile lateness 0.50))
          :p95Ms (* 1000.0 (pi-coding-agent-tu-bench--percentile lateness 0.95))
          :maxMs (if lateness
                     (* 1000.0 (cl-reduce #'max lateness))
                   0.0)
          :over100Ms (cl-count-if (lambda (x) (> x 0.1)) lateness)
          :over250Ms (cl-count-if (lambda (x) (> x 0.25)) lateness))))

(defun pi-coding-agent-tu-bench--storm-tool-ids ()
  "Return the expected storm phase subagent tool call IDs."
  (cl-loop for index below pi-coding-agent-tu-bench-parallel-tools
           collect (format "call-storm-%02d" index)))

(defun pi-coding-agent-tu-bench--fill-tool-count ()
  "Return the total number of fill phase tool executions."
  (+ pi-coding-agent-tu-bench-fill-bash
     pi-coding-agent-tu-bench-fill-read
     pi-coding-agent-tu-bench-fill-write
     pi-coding-agent-tu-bench-fill-edit))

(defun pi-coding-agent-tu-bench--wait-until (predicate timeout)
  "Wait for PREDICATE to become non-nil, or TIMEOUT seconds to elapse."
  (let ((start (float-time))
        result)
    (while (and (not (setq result (funcall predicate)))
                (< (- (float-time) start) timeout))
      (accept-process-output nil 0.01)
      (when (and pi-coding-agent-tu-bench-display-buffers (not noninteractive))
        (redisplay t)))
    result))

(defun pi-coding-agent-tu-bench--pending-requests-count (proc)
  "Return the number of pending RPC requests for PROC."
  (let ((pending (and (processp proc)
                      (process-get proc 'pi-coding-agent-pending-requests))))
    (if (hash-table-p pending) (hash-table-count pending) 0)))

(defun pi-coding-agent-tu-bench--count-occurrences (buffer text)
  "Return the number of times TEXT occurs in BUFFER."
  (if (not (buffer-live-p buffer))
      0
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((count 0))
          (while (search-forward text nil t)
            (setq count (1+ count)))
          count)))))

(defun pi-coding-agent-tu-bench--tool-block-overlay-count (buffer tool-call-id)
  "Return the number of tool block overlays in BUFFER for TOOL-CALL-ID."
  (if (not (buffer-live-p buffer))
      0
    (with-current-buffer buffer
      (cl-count-if
       (lambda (ov)
         (when-let* ((record (and (overlay-get ov 'pi-coding-agent-tool-block)
                                  (overlay-get
                                   ov 'pi-coding-agent-tool-block-record))))
           (equal (pi-coding-agent--tool-block-tool-call-id record)
                  tool-call-id)))
       (overlays-in (point-min) (point-max))))))

(defun pi-coding-agent-tu-bench--live-tool-block-count (buffer)
  "Return the number of entries in BUFFER's live tool block registry."
  (if (not (buffer-live-p buffer))
      -1
    (with-current-buffer buffer
      (if (hash-table-p pi-coding-agent--live-tool-blocks)
          (hash-table-count pi-coding-agent--live-tool-blocks)
        0))))

(defun pi-coding-agent-tu-bench--check (name ok detail)
  "Return a correctness check entry for NAME with OK flag and DETAIL text."
  (list :name name
        :ok (pi-coding-agent-tu-bench--json-bool ok)
        :detail detail))

(defun pi-coding-agent-tu-bench--collect-checks (chat-buf)
  "Return correctness check entries for the settled run in CHAT-BUF.
Every entry must have a true :ok for the benchmark run to pass."
  (let* ((storm-ids (pi-coding-agent-tu-bench--storm-tool-ids))
         (expected-executions (+ (pi-coding-agent-tu-bench--fill-tool-count)
                                 pi-coding-agent-tu-bench-parallel-tools))
         (start-count (pi-coding-agent-tu-bench--event-count
                       "tool_execution_start"))
         (end-count (pi-coding-agent-tu-bench--event-count "tool_execution_end"))
         (update-count (pi-coding-agent-tu-bench--event-count
                        "tool_execution_update"))
         (checks nil))
    (push (pi-coding-agent-tu-bench--check
           "agent-end-received"
           (and pi-coding-agent-tu-bench--agent-end-time t)
           (if pi-coding-agent-tu-bench--agent-end-time
               "agent_end handled"
             "agent_end never handled"))
          checks)
    (push (pi-coding-agent-tu-bench--check
           "tool-execution-start-count"
           (= start-count expected-executions)
           (format "expected %d, handled %d" expected-executions start-count))
          checks)
    (push (pi-coding-agent-tu-bench--check
           "tool-execution-end-count"
           (= end-count expected-executions)
           (format "expected %d, handled %d" expected-executions end-count))
          checks)
    (push (pi-coding-agent-tu-bench--check
           "tool-execution-update-count"
           (= update-count pi-coding-agent-tu-bench-updates)
           (format "expected %d, handled %d"
                   pi-coding-agent-tu-bench-updates update-count))
          checks)
    (let ((bad-blocks
           (seq-filter
            (lambda (tool-call-id)
              (/= 1 (pi-coding-agent-tu-bench--tool-block-overlay-count
                     chat-buf tool-call-id)))
            storm-ids)))
      (push (pi-coding-agent-tu-bench--check
             "subagent-blocks-present-once"
             (null bad-blocks)
             (if bad-blocks
                 (format "ids without exactly one finalized block: %s"
                         (string-join bad-blocks ", "))
               (format "each of %d subagent blocks present exactly once"
                       (length storm-ids))))
            checks))
    (let ((bad-texts
           (seq-filter
            (lambda (tool-call-id)
              (/= 1 (pi-coding-agent-tu-bench--count-occurrences
                     chat-buf (format "STORM-FINAL-RESULT %s" tool-call-id))))
            storm-ids)))
      (push (pi-coding-agent-tu-bench--check
             "subagent-final-text-exactly-once"
             (null bad-texts)
             (if bad-texts
                 (format "ids whose final text is not exactly once: %s"
                         (string-join bad-texts ", "))
               "every subagent final result text appears exactly once"))
            checks))
    (let ((live-count (pi-coding-agent-tu-bench--live-tool-block-count
                       chat-buf)))
      (push (pi-coding-agent-tu-bench--check
             "no-live-tool-blocks-remain"
             (= live-count 0)
             (format "%d live tool block registry entries remain" live-count))
            checks))
    (nreverse checks)))

(defun pi-coding-agent-tu-bench--cleanup-session (chat-buf)
  "Kill CHAT-BUF, its input buffer, and their fake pi process."
  (when (buffer-live-p chat-buf)
    (let ((input-buf (buffer-local-value 'pi-coding-agent--input-buffer
                                         chat-buf)))
      (with-current-buffer chat-buf
        (when (and (boundp 'pi-coding-agent--process)
                   (processp pi-coding-agent--process)
                   (process-live-p pi-coding-agent--process))
          (set-process-query-on-exit-flag pi-coding-agent--process nil)
          (delete-process pi-coding-agent--process)))
      (kill-buffer chat-buf)
      (when (buffer-live-p input-buf)
        (kill-buffer input-buf)))))

(defun pi-coding-agent-tu-bench--run-session ()
  "Run one storm session and return a metrics plist.
Sets up a fake-backed session in a temporary directory, sends one prompt,
waits for agent_end, then collects buffer metrics and correctness checks."
  (let* ((session-dir (make-temp-file
                       (expand-file-name "session-"
                                         pi-coding-agent-tu-bench-out-dir)
                       t))
         (chat-buf nil)
         (ok nil)
         (error-text nil)
         (gc-before nil)
         (gc-time-before nil)
         (start nil))
    (setq pi-coding-agent-executable
          (list (or (executable-find "python3") (error "Python3 not found"))
                pi-coding-agent-tu-bench-fake-pi))
    (setq pi-coding-agent-extra-args
          (list "--log-file" pi-coding-agent-tu-bench-fake-log))
    ;; Never prompt about grammars or versions from a benchmark run.
    (setq pi-coding-agent-essential-grammar-action 'warn)
    (unwind-protect
        (condition-case err
            (progn
              ;; Keep the asynchronous `pi --version' probe out of the run;
              ;; it would spawn an extra fake process in the GUI lane.
              (let ((pi-coding-agent--version-probe-delay 3600))
                (setq chat-buf (pi-coding-agent--setup-session session-dir)))
              (when (and pi-coding-agent-tu-bench-display-buffers
                         (not noninteractive))
                (pi-coding-agent--show-session-buffers
                 chat-buf
                 (buffer-local-value 'pi-coding-agent--input-buffer chat-buf))
                (redisplay t))
              (garbage-collect)
              (setq gc-before gcs-done
                    gc-time-before gc-elapsed
                    start (float-time))
              (pi-coding-agent-tu-bench--start-probe)
              (setq pi-coding-agent-tu-bench--prompt-time (float-time))
              (with-current-buffer chat-buf
                (pi-coding-agent--prepare-and-send
                 "run the synthetic tool update storm"))
              (setq ok
                    (pi-coding-agent-tu-bench--wait-until
                     (lambda ()
                       (let ((proc (and (buffer-live-p chat-buf)
                                        (with-current-buffer chat-buf
                                          pi-coding-agent--process))))
                         (unless (and (processp proc) (process-live-p proc))
                           (error "Fake pi process exited before the storm settled"))
                         (and pi-coding-agent-tu-bench--agent-end-time
                              (= 0 (pi-coding-agent-tu-bench--pending-requests-count
                                    proc)))))
                     pi-coding-agent-tu-bench-timeout-seconds))
              (unless ok
                (error "Timed out waiting for agent_end after %d seconds"
                       pi-coding-agent-tu-bench-timeout-seconds)))
          (error (setq error-text (error-message-string err))))
      (pi-coding-agent-tu-bench--stop-probe))
    (prog1
        (list :ok (pi-coding-agent-tu-bench--json-bool ok)
              :error error-text
              :wallMs (when pi-coding-agent-tu-bench--agent-end-time
                        (* 1000.0 (- pi-coding-agent-tu-bench--agent-end-time
                                     pi-coding-agent-tu-bench--prompt-time)))
              :seconds (and start (- (float-time) start))
              :gcs (and gc-before (- gcs-done gc-before))
              :gcSeconds (and gc-time-before (- gc-elapsed gc-time-before))
              :bufferBytes (and (buffer-live-p chat-buf)
                                (with-current-buffer chat-buf (buffer-size)))
              :bufferLines (and (buffer-live-p chat-buf)
                                (with-current-buffer chat-buf
                                  (count-lines (point-min) (point-max))))
              :overlays (and (buffer-live-p chat-buf)
                             (with-current-buffer chat-buf
                               (length (overlays-in (point-min) (point-max)))))
              :checks (pi-coding-agent-tu-bench--collect-checks chat-buf))
      (pi-coding-agent-tu-bench--cleanup-session chat-buf))))

(defun pi-coding-agent-tu-bench--git-string (&rest args)
  "Run git with ARGS in the repository root and return trimmed output."
  (string-trim
   (with-temp-buffer
     (let ((default-directory pi-coding-agent-tu-bench-repo-root))
       (if (zerop (apply #'process-file "git" nil t nil args))
           (buffer-string)
         "")))))

(defun pi-coding-agent-tu-bench--workload-json ()
  "Return the configured workload as a JSON-encodable plist."
  (list :fillBash pi-coding-agent-tu-bench-fill-bash
        :fillRead pi-coding-agent-tu-bench-fill-read
        :fillWrite pi-coding-agent-tu-bench-fill-write
        :fillEdit pi-coding-agent-tu-bench-fill-edit
        :fillOutputLines pi-coding-agent-tu-bench-fill-output-lines
        :updates pi-coding-agent-tu-bench-updates
        :parallelTools pi-coding-agent-tu-bench-parallel-tools
        :gapScale pi-coding-agent-tu-bench-gap-scale
        :seed pi-coding-agent-tu-bench-seed))

(defun pi-coding-agent-tu-bench--write-times-tsv ()
  "Write event timing and render rows to `pi-coding-agent-tu-bench-times-file'."
  (with-temp-file pi-coding-agent-tu-bench-times-file
    (insert "series\tname\tcount\ttotal_ms\tmean_ms\tmax_ms\n")
    (dolist (row (pi-coding-agent-tu-bench--event-stats))
      (insert (format "event\t%s\t%d\t%.3f\t%.3f\t%.3f\n"
                      (plist-get row :type)
                      (plist-get row :count)
                      (plist-get row :totalMs)
                      (plist-get row :meanMs)
                      (plist-get row :maxMs))))
    (dolist (row (pi-coding-agent-tu-bench--render-rows))
      (insert (format "render\t%s:%s\t%d\t%.3f\t%.3f\t%.3f\n"
                      (plist-get row :operation)
                      (plist-get row :toolCallId)
                      (plist-get row :count)
                      (plist-get row :totalMs)
                      (plist-get row :meanMs)
                      (plist-get row :maxMs))))))

(defun pi-coding-agent-tu-bench--checks-json (entries)
  "Encode correctness check ENTRIES as a JSON vector."
  (vconcat
   (mapcar (lambda (check)
             (list :name (plist-get check :name)
                   :ok (plist-get check :ok)
                   :detail (plist-get check :detail)))
           entries)))

(defun pi-coding-agent-tu-bench--write-result-json (metrics)
  "Write run METRICS to `pi-coding-agent-tu-bench-result-file'."
  (let* ((dirty (not (string-empty-p
                      (pi-coding-agent-tu-bench--git-string
                       "status" "--porcelain" "--untracked-files=no"))))
         (probe (pi-coding-agent-tu-bench--probe-stats))
         (object
          (list :scenario pi-coding-agent-tu-bench-scenario
                :iteration pi-coding-agent-tu-bench-iteration
                :commit (pi-coding-agent-tu-bench--git-string
                         "rev-parse" "--short" "HEAD")
                :dirty (pi-coding-agent-tu-bench--json-bool dirty)
                :display (pi-coding-agent-tu-bench--json-bool
                          pi-coding-agent-tu-bench-display-buffers)
                :emacsVersion emacs-version
                :markdownGrammar
                (pi-coding-agent-tu-bench--json-bool
                 (treesit-language-available-p 'markdown))
                :workload (pi-coding-agent-tu-bench--workload-json)
                :ok (plist-get metrics :ok)
                :error (or (plist-get metrics :error) :json-null)
                :wallMs (or (plist-get metrics :wallMs) :json-null)
                :seconds (plist-get metrics :seconds)
                :gcs (plist-get metrics :gcs)
                :gcSeconds (plist-get metrics :gcSeconds)
                :bufferBytes (plist-get metrics :bufferBytes)
                :bufferLines (plist-get metrics :bufferLines)
                :overlays (plist-get metrics :overlays)
                :eventStats
                (vconcat
                 (mapcar (lambda (row)
                           (list :type (plist-get row :type)
                                 :count (plist-get row :count)
                                 :totalMs (plist-get row :totalMs)
                                 :meanMs (plist-get row :meanMs)
                                 :maxMs (plist-get row :maxMs)))
                         (pi-coding-agent-tu-bench--event-stats)))
                :probe (list :intervalMs (plist-get probe :intervalMs)
                             :fires (plist-get probe :fires)
                             :p50Ms (plist-get probe :p50Ms)
                             :p95Ms (plist-get probe :p95Ms)
                             :maxMs (plist-get probe :maxMs)
                             :over100Ms (plist-get probe :over100Ms)
                             :over250Ms (plist-get probe :over250Ms)
                             :latenessMs
                             (vconcat
                              (mapcar (lambda (x) (* 1000.0 x))
                                      (nreverse
                                       (copy-sequence
                                        pi-coding-agent-tu-bench--probe-lateness)))))
                :renders (list :replaceBody
                               (pi-coding-agent-tu-bench--render-operation-json
                                "replace-body")
                               :displayToolEnd
                               (pi-coding-agent-tu-bench--render-operation-json
                                "display-tool-end"))
                :checks (pi-coding-agent-tu-bench--checks-json
                         (plist-get metrics :checks)))))
    (with-temp-file pi-coding-agent-tu-bench-result-file
      (insert (json-encode object) "\n"))))

(defun pi-coding-agent-tu-bench--write-report (metrics run-ok)
  "Write a Markdown report for METRICS; RUN-OK is the overall verdict."
  (let ((dirty (not (string-empty-p
                     (pi-coding-agent-tu-bench--git-string
                      "status" "--porcelain" "--untracked-files=no"))))
        (probe (pi-coding-agent-tu-bench--probe-stats)))
    (with-temp-file pi-coding-agent-tu-bench-report-file
      (insert "# Tool-update storm benchmark\n\n")
      (insert "Synthetic deterministic workload only; no private session content is read or stored.\n\n")
      (insert (format "- Verdict: `%s`\n" (if run-ok "pass" "FAIL")))
      (insert (format "- Scenario: `%s`\n" pi-coding-agent-tu-bench-scenario))
      (insert (format "- Iteration: `%d`\n" pi-coding-agent-tu-bench-iteration))
      (insert (format "- Commit: `%s`%s\n"
                      (pi-coding-agent-tu-bench--git-string
                       "rev-parse" "--short" "HEAD")
                      (if dirty " (dirty)" "")))
      (insert (format "- Emacs: `%s`\n" emacs-version))
      (insert (format "- Visible GUI buffers: `%s`\n"
                      (if pi-coding-agent-tu-bench-display-buffers
                          "yes" "no")))
      (insert (format "- Markdown tree-sitter grammar: `%s`\n\n"
                      (if (treesit-language-available-p 'markdown)
                          "available" "MISSING")))
      (insert "## Reproduction command shape\n\n")
      (insert "```sh\n")
      (insert (format "./bench/run-tool-update-bench.sh %s --scenario %s -c 1 --out-dir %s\n"
                      (if pi-coding-agent-tu-bench-display-buffers
                          "" "--batch")
                      pi-coding-agent-tu-bench-scenario
                      pi-coding-agent-tu-bench-runner-out-dir))
      (insert "```\n\n")
      (insert "## Workload\n\n")
      (insert (format "- Fill: `%d` bash, `%d` read, `%d` write, `%d` edit completed tool executions x `%d` output lines\n"
                      pi-coding-agent-tu-bench-fill-bash
                      pi-coding-agent-tu-bench-fill-read
                      pi-coding-agent-tu-bench-fill-write
                      pi-coding-agent-tu-bench-fill-edit
                      pi-coding-agent-tu-bench-fill-output-lines))
      (insert (format "- Storm: `%d` updates across `%d` parallel subagent tools, gap scale `%.2f`, seed `%d`\n\n"
                      pi-coding-agent-tu-bench-updates
                      pi-coding-agent-tu-bench-parallel-tools
                      pi-coding-agent-tu-bench-gap-scale
                      pi-coding-agent-tu-bench-seed))
      (insert "## Wall-clock and buffer metrics\n\n")
      (insert (format "- Prompt to agent_end: `%s` ms\n"
                      (if-let* ((wall (plist-get metrics :wallMs)))
                          (format "%.0f" wall) "n/a")))
      (insert (format "- Total run seconds: `%.3f`; GCs: `%s`; GC seconds: `%s`\n"
                      (or (plist-get metrics :seconds) 0.0)
                      (or (plist-get metrics :gcs) "n/a")
                      (if-let* ((gcs (plist-get metrics :gcSeconds)))
                          (format "%.3f" gcs) "n/a")))
      (insert (format "- Chat buffer: `%s` chars, `%s` lines, `%s` overlays\n\n"
                      (or (plist-get metrics :bufferBytes) "n/a")
                      (or (plist-get metrics :bufferLines) "n/a")
                      (or (plist-get metrics :overlays) "n/a")))
      (insert "## Probe timer (main-thread blocking)\n\n")
      (insert (format "- Interval: `%.0f` ms; fires: `%d`\n"
                      (plist-get probe :intervalMs) (plist-get probe :fires)))
      (insert (format "- Lateness p50: `%.1f` ms; p95: `%.1f` ms; max: `%.1f` ms\n"
                      (plist-get probe :p50Ms)
                      (plist-get probe :p95Ms)
                      (plist-get probe :maxMs)))
      (insert (format "- Fires >100 ms late: `%d`; >250 ms late: `%d`\n\n"
                      (plist-get probe :over100Ms)
                      (plist-get probe :over250Ms)))
      (insert "## Per-event-type handling\n\n")
      (insert "| event type | count | total ms | mean ms | max ms |\n")
      (insert "|---|---:|---:|---:|---:|\n")
      (dolist (row (pi-coding-agent-tu-bench--event-stats))
        (insert (format "| `%s` | %d | %.1f | %.2f | %.1f |\n"
                        (plist-get row :type)
                        (plist-get row :count)
                        (plist-get row :totalMs)
                        (plist-get row :meanMs)
                        (plist-get row :maxMs))))
      (insert "\n## Tool block render counts\n\n")
      (insert "Environment-independent coalescing metric: how often the frontend re-renders tool block bodies.  ")
      (insert "The WP2 coalescing renderer should drop `replace-body` calls towards (storm seconds / 0.25) x parallel tools.\n\n")
      (insert "| operation | total calls | total ms | mean ms | max ms |\n")
      (insert "|---|---:|---:|---:|---:|\n")
      (dolist (operation '("replace-body" "display-tool-end"))
        (let ((aggregate (pi-coding-agent-tu-bench--render-operation-json
                          operation))
              (rows (seq-filter
                     (lambda (row)
                       (equal (plist-get row :operation) operation))
                     (pi-coding-agent-tu-bench--render-rows))))
          (insert (format "| `%s` | %d | %.1f | %.2f | %.1f |\n"
                          operation
                          (plist-get aggregate :total)
                          (plist-get aggregate :totalMs)
                          (if rows
                              (/ (plist-get aggregate :totalMs)
                                 (plist-get aggregate :total))
                            0.0)
                          (plist-get aggregate :maxMs)))))
      (insert "\nPer storm tool call ID:\n\n")
      (insert "| tool call id | replace-body calls | display-tool-end calls |\n")
      (insert "|---|---:|---:|\n")
      (dolist (tool-call-id (pi-coding-agent-tu-bench--storm-tool-ids))
        (insert (format "| `%s` | %d | %d |\n"
                        tool-call-id
                        (pi-coding-agent-tu-bench--render-count
                         "replace-body" tool-call-id)
                        (pi-coding-agent-tu-bench--render-count
                         "display-tool-end" tool-call-id))))
      (insert "\nThe full per-tool-call-id breakdown (including fill phase ids) is in `result.json` and `times.tsv`.\n")
      (insert "\n## Correctness checks\n\n")
      (insert "| check | ok | detail |\n")
      (insert "|---|---|---|\n")
      (dolist (check (plist-get metrics :checks))
        (insert (format "| `%s` | %s | %s |\n"
                        (plist-get check :name)
                        (if (eq (plist-get check :ok) t) "yes" "NO")
                        (plist-get check :detail))))
      (insert "\n## Raw artifacts\n\n")
      (insert (format "- Result JSON: `%s`\n"
                      pi-coding-agent-tu-bench-result-file))
      (insert (format "- Timing TSV: `%s`\n"
                      pi-coding-agent-tu-bench-times-file))
      (insert (format "- Fake RPC log without content: `%s`\n"
                      pi-coding-agent-tu-bench-fake-log)))))

(defun pi-coding-agent-tu-bench--metrics-ok-p (metrics)
  "Return non-nil when METRICS report a settled run with every check green."
  (and (eq (plist-get metrics :ok) t)
       (seq-every-p (lambda (check) (eq (plist-get check :ok) t))
                    (plist-get metrics :checks))))

;;;###autoload
(defun pi-coding-agent-tu-bench-run ()
  "Run one tool-update benchmark iteration and write artifacts.
Return non-nil when the storm settled and all correctness checks passed.
Timing thresholds are not enforced."
  (when (and pi-coding-agent-tu-bench-display-buffers
             (not noninteractive)
             (not (display-graphic-p)))
    (error "GUI benchmark lane requires a graphic display; use xvfb-run"))
  (make-directory pi-coding-agent-tu-bench-out-dir t)
  (ignore-errors (delete-file pi-coding-agent-tu-bench-fake-log))
  (setq pi-coding-agent-tu-bench--event-log nil
        pi-coding-agent-tu-bench--prompt-time nil
        pi-coding-agent-tu-bench--agent-end-time nil
        pi-coding-agent-tu-bench--probe-expected nil
        pi-coding-agent-tu-bench--probe-lateness nil
        pi-coding-agent-tu-bench--render-log (make-hash-table :test 'equal))
  (pi-coding-agent-tu-bench--install-advice)
  (unwind-protect
      (let* ((metrics (pi-coding-agent-tu-bench--run-session))
             (run-ok (pi-coding-agent-tu-bench--metrics-ok-p metrics)))
        (pi-coding-agent-tu-bench--write-times-tsv)
        (pi-coding-agent-tu-bench--write-result-json metrics)
        (pi-coding-agent-tu-bench--write-report metrics run-ok)
        (princ (format "Wrote %s\n" pi-coding-agent-tu-bench-result-file))
        (princ (format "Wrote %s\n" pi-coding-agent-tu-bench-times-file))
        (princ (format "Wrote %s\n" pi-coding-agent-tu-bench-report-file))
        run-ok)
    (pi-coding-agent-tu-bench--remove-advice)))

(defun pi-coding-agent-tu-bench-run-batch ()
  "Run one tool-update benchmark iteration in batch mode and exit."
  (let ((standard-output #'external-debugging-output))
    (kill-emacs (if (pi-coding-agent-tu-bench-run) 0 1))))

(provide 'pi-coding-agent-tool-update-bench)
;;; pi-coding-agent-tool-update-bench.el ends here
