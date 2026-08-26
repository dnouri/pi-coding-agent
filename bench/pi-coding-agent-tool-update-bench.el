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
;; coalescing metric).  The agent-end-cooling scenario reuses the same fake
;; process and runner to queue the `PI_TU_BENCH_FILL_*'-sized tool cohort at
;; final agent_end, observe real one-shot cooling callbacks, and route scroll
;; heartbeats through unread command events.  All content is synthetic; no
;; private session files are read.
;;
;; Run with:
;;
;;   make bench-tool-update             # GUI via xvfb, primary lane
;;   make bench-tool-update-batch       # --batch, secondary lane
;;   make bench-tool-update-smoke       # cheap correctness smoke
;;   make bench-agent-end-cooling       # deferred cooling GUI lane
;;   make bench-agent-end-cooling-batch # deferred cooling batch lane
;;   make bench-agent-end-cooling-smoke # cheap cooling correctness smoke
;;
;; or directly through `bench/run-tool-update-bench.sh'.
;;
;; The primary lane is GUI/xvfb because the measured cost is dominated by
;; buffer mutation plus redisplay/fontification, which batch mode cannot
;; reproduce.  Batch numbers are useful for CI trend artifacts.  The run
;; fails on correctness violations (lost or duplicated tool blocks and
;; events), not on timing thresholds.
;;
;; The runner deliberately uses `-Q'.  Cooling slice/root timings in this lane
;; are structural diagnostics, and zero tree-root calls is a valid result.
;; They must not be cited as evidence that md-ts root cost was reduced; this
;; benchmark proves deferred scheduling and final correctness only.

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

(defvar pi-coding-agent-tu-bench-hot-tail-turns
  (pi-coding-agent-tu-bench--env-int "PI_TU_BENCH_HOT_TAIL_TURNS" 1)
  "Number of headed turns kept hot by the cooling benchmark scenario.")

(defvar pi-coding-agent-tu-bench-command-interval
  (/ (pi-coding-agent-tu-bench--env-float
      "PI_TU_BENCH_COMMAND_INTERVAL_MS" 100.0)
     1000.0)
  "Seconds between routed scroll-heartbeat commands during cooling.")

(defconst pi-coding-agent-tu-bench-cooling-live-id "call-cooling-live"
  "Tool call ID of the live hot-tail sentinel fixture.")

(defconst pi-coding-agent-tu-bench-command-event
  'pi-coding-agent-tu-bench-scroll-heartbeat
  "Synthetic benchmark-safe command event routed during natural drain.")

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

(defvar pi-coding-agent-tu-bench-cooling-slices-file
  (expand-file-name "cooling-slices.tsv" pi-coding-agent-tu-bench-out-dir)
  "Per-callback deferred cooling artifact path for one benchmark run.")

(defvar pi-coding-agent-tu-bench-commands-file
  (expand-file-name "commands.tsv" pi-coding-agent-tu-bench-out-dir)
  "Per-command scroll-heartbeat artifact path for one benchmark run.")

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

(defvar pi-coding-agent-tu-bench--current-filter-id nil
  "Dynamically bound `process-filter' invocation ID, or nil.")

(defvar pi-coding-agent-tu-bench--filter-sequence 0
  "Monotonic `process-filter' invocation counter.")

(defvar pi-coding-agent-tu-bench--filter-log nil
  "Process-filter timing rows, newest first.")

(defvar pi-coding-agent-tu-bench--agent-end-observation nil
  "Structural state captured immediately around the real agent_end handler.")

(defvar pi-coding-agent-tu-bench--agent-end-filter nil
  "Timing row for the `process-filter' invocation enclosing agent_end.")

(defvar pi-coding-agent-tu-bench--drain-start-time nil
  "Time when production agent_end returned and deferred drain began.")

(defvar pi-coding-agent-tu-bench--cooling-slice-log nil
  "Deferred cooling callback rows, newest first.")

(defvar pi-coding-agent-tu-bench--cooling-slice-sequence 0
  "Monotonic deferred cooling callback counter.")

(defvar pi-coding-agent-tu-bench--scheduler-errors nil
  "Deferred cooling warning strings observed during a run.")

(defvar pi-coding-agent-tu-bench--tree-root-phase nil
  "Dynamically bound tree-root measurement phase, or nil.")

(defvar pi-coding-agent-tu-bench--tree-root-count 0
  "Dynamically accumulated tree-root call count.")

(defvar pi-coding-agent-tu-bench--tree-root-ms 0.0
  "Dynamically accumulated tree-root call wall time in milliseconds.")

(defvar pi-coding-agent-tu-bench--tree-root-max-ms 0.0
  "Dynamically accumulated maximum tree-root call wall time.")

(defvar pi-coding-agent-tu-bench--cooling-chat-buffer nil
  "Chat buffer owned by the active cooling benchmark command route.")

(defvar pi-coding-agent-tu-bench--command-timer nil
  "One-shot timer scheduling the next scroll-heartbeat command event.")

(defvar pi-coding-agent-tu-bench--command-pending nil
  "FIFO list of scroll-heartbeat command jobs awaiting command routing.")

(defvar pi-coding-agent-tu-bench--command-log nil
  "Routed command timing rows, newest first.")

(defvar pi-coding-agent-tu-bench--command-sequence 0
  "Monotonic scroll-heartbeat command counter.")

(defvar pi-coding-agent-tu-bench--command-current-job nil
  "Job currently moving through pre-command and post-command hooks.")

(defvar pi-coding-agent-tu-bench--command-drain-active nil
  "Non-nil while command heartbeats may be scheduled for a cooling drain.")

(defvar pi-coding-agent-tu-bench--command-last-route "follow"
  "Last logical scroll route, either \"sentinel\" or \"follow\".")

(defvar pi-coding-agent-tu-bench--window-sentinel-marker nil
  "Marker tracking the logical scroll sentinel during cooling.")

(defun pi-coding-agent-tu-bench--cooling-scenario-p ()
  "Return non-nil for an agent-end cooling benchmark scenario."
  (string-prefix-p "agent-end-cooling"
                   pi-coding-agent-tu-bench-scenario))

(defun pi-coding-agent-tu-bench--tool-overlays (&optional buffer)
  "Return tool block overlays in BUFFER, or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (seq-filter
     (lambda (overlay)
       (overlay-get overlay 'pi-coding-agent-tool-block))
     (overlays-in (point-min) (point-max)))))

(defun pi-coding-agent-tu-bench--overlay-tool-call-id (overlay)
  "Return OVERLAY's tool call ID, or nil."
  (when-let* ((record
               (overlay-get overlay 'pi-coding-agent-tool-block-record)))
    (pi-coding-agent--tool-block-tool-call-id record)))

(defun pi-coding-agent-tu-bench--overlay-count-for-id (tool-call-id)
  "Return the current buffer's tool overlay count for TOOL-CALL-ID."
  (cl-count tool-call-id (pi-coding-agent-tu-bench--tool-overlays)
            :key #'pi-coding-agent-tu-bench--overlay-tool-call-id
            :test #'equal))

(defun pi-coding-agent-tu-bench--cold-tool-metadata ()
  "Return cold tool metadata segments in `current-buffer' order."
  (let ((position (point-min))
        metadata)
    (while (< position (point-max))
      (let* ((value (get-text-property
                     position 'pi-coding-agent-cold-tool-block))
             (next (or (next-single-property-change
                        position 'pi-coding-agent-cold-tool-block
                        nil (point-max))
                       (point-max))))
        (when value
          (push value metadata))
        (setq position (max (1+ position) next))))
    (nreverse metadata)))

(defun pi-coding-agent-tu-bench--button-count ()
  "Return the number of distinct text buttons in the current buffer."
  (let ((position (point-min))
        (count 0))
    (while (< position (point-max))
      (if-let* ((button (button-at position)))
          (progn
            (setq count (1+ count))
            (setq position (max (1+ position) (button-end button))))
        (setq position (1+ position))))
    count))

(defun pi-coding-agent-tu-bench--collapsed-tool-count ()
  "Return how many current tool overlays carry a collapse button."
  (cl-count-if
   (lambda (overlay)
     (pi-coding-agent--find-toggle-button-in-region
      (overlay-start overlay) (overlay-end overlay)))
   (pi-coding-agent-tu-bench--tool-overlays)))

(defun pi-coding-agent-tu-bench--semantic-lines ()
  "Return deterministic cooling semantic sentinel lines in buffer order."
  (save-excursion
    (goto-char (point-min))
    (let (lines)
      (while (re-search-forward "^.*COOLING-SEMANTIC.*$" nil t)
        (push (buffer-substring-no-properties
               (match-beginning 0) (match-end 0))
              lines))
      (nreverse lines))))

(defun pi-coding-agent-tu-bench--expected-semantic-lines ()
  "Return exact semantic sentinel lines expected from the cooling fixture."
  (append
   (cl-loop for index below (pi-coding-agent-tu-bench--fill-tool-count)
            collect (format "COOLING-SEMANTIC call-cooling-%04d" index))
   '("| COOLING-SEMANTIC-HOT-TABLE | hot | remains decorated and outside the completed cooling cohort |"
     "$ cooling-live --sentinel COOLING-SEMANTIC-LIVE")))

(defun pi-coding-agent-tu-bench--expected-semantic-hash ()
  "Return SHA-256 of the exact expected cooling semantic projection."
  (secure-hash 'sha256
               (string-join
                (pi-coding-agent-tu-bench--expected-semantic-lines) "\n")))

(defun pi-coding-agent-tu-bench--tree-root-row ()
  "Return tree-root counters from the current dynamic measurement phase."
  (list :phase pi-coding-agent-tu-bench--tree-root-phase
        :count pi-coding-agent-tu-bench--tree-root-count
        :totalMs pi-coding-agent-tu-bench--tree-root-ms
        :maxMs pi-coding-agent-tu-bench--tree-root-max-ms))

(defun pi-coding-agent-tu-bench--around-tree-root (orig &rest args)
  "Call ORIG with ARGS and record time in the active lightweight phase."
  (if (not pi-coding-agent-tu-bench--tree-root-phase)
      (apply orig args)
    (let ((start (float-time))
          value)
      (unwind-protect
          (setq value (apply orig args))
        (let ((elapsed (* 1000.0 (- (float-time) start))))
          (setq pi-coding-agent-tu-bench--tree-root-count
                (1+ pi-coding-agent-tu-bench--tree-root-count)
                pi-coding-agent-tu-bench--tree-root-ms
                (+ pi-coding-agent-tu-bench--tree-root-ms elapsed)
                pi-coding-agent-tu-bench--tree-root-max-ms
                (max pi-coding-agent-tu-bench--tree-root-max-ms elapsed))))
      value)))

(defun pi-coding-agent-tu-bench--around-process-filter (orig proc output)
  "Call process filter ORIG for PROC and OUTPUT while timing the real path."
  (let* ((id (cl-incf pi-coding-agent-tu-bench--filter-sequence))
         (start (float-time))
         (pi-coding-agent-tu-bench--current-filter-id id)
         value)
    (setq value (funcall orig proc output))
    (let* ((elapsed (* 1000.0 (- (float-time) start)))
           (agent-end
            (and pi-coding-agent-tu-bench--agent-end-observation
                 (= id (plist-get pi-coding-agent-tu-bench--agent-end-observation
                                  :filterId))))
           (row (list :id id
                      :wallMs elapsed
                      :bytes (string-bytes output)
                      :lines (cl-count ?\n output)
                      :agentEnd (and agent-end t))))
      (push row pi-coding-agent-tu-bench--filter-log)
      (when agent-end
        (setq pi-coding-agent-tu-bench--agent-end-filter row)))
    value))

(defun pi-coding-agent-tu-bench--time-display-event (orig event type)
  "Call the real display-event handler ORIG with EVENT, timed as TYPE.
Append (TYPE . elapsed-ms) to the event log and return a plist with
:VALUE, :END, and :ELAPSED-MS.  Observation only: the production
handler runs unmodified, with no delays or state overrides."
  (let* ((start (float-time))
         (value (funcall orig event))
         (end (float-time)))
    (push (cons type (* 1000.0 (- end start)))
          pi-coding-agent-tu-bench--event-log)
    (list :value value :end end :elapsedMs (* 1000.0 (- end start)))))

(defun pi-coding-agent-tu-bench--cooling-state-counts ()
  "Return the shared deferred-cooling state counts for the current buffer.
The agent_end boundary observation (before and after the real handler)
and the settled final state both read these fields through this one
helper, so the two views cannot drift apart.  Pure observation."
  (list :hotTailBoundary
        (and (markerp pi-coding-agent--hot-tail-start)
             (marker-position pi-coding-agent--hot-tail-start))
        :queueLength (length pi-coding-agent--tool-cooling-queue)
        :timer (and pi-coding-agent--tool-cooling-timer t)
        :coldTools (length (pi-coding-agent-tu-bench--cold-tool-metadata))
        :toolOverlays (length (pi-coding-agent-tu-bench--tool-overlays))
        :liveRegistry
        (if (hash-table-p pi-coding-agent--live-tool-blocks)
            (hash-table-count pi-coding-agent--live-tool-blocks)
          0)
        :currentOverlays
        (pi-coding-agent-tu-bench--overlay-count-for-id
         pi-coding-agent-tu-bench-cooling-live-id)))

(defun pi-coding-agent-tu-bench--semantic-summary ()
  "Return (LINE-COUNT . SHA-256) of the current semantic projection."
  (let ((lines (pi-coding-agent-tu-bench--semantic-lines)))
    (cons (length lines)
          (secure-hash 'sha256 (string-join lines "\n")))))

(defun pi-coding-agent-tu-bench--around-handle-event (orig event)
  "Around advice recording handling time for display EVENT.
Calls ORIG with EVENT and appends to the benchmark event log."
  (let ((type (or (plist-get event :type) "unknown")))
    (if (and (equal type "agent_end")
             (pi-coding-agent-tu-bench--cooling-scenario-p))
        (let* ((before (pi-coding-agent-tu-bench--cooling-state-counts))
               (collapsed-before
                (pi-coding-agent-tu-bench--collapsed-tool-count))
               (gc-before gcs-done)
               (gc-time-before gc-elapsed)
               timed tree-row)
          (let ((pi-coding-agent-tu-bench--tree-root-phase "agent_end")
                (pi-coding-agent-tu-bench--tree-root-count 0)
                (pi-coding-agent-tu-bench--tree-root-ms 0.0)
                (pi-coding-agent-tu-bench--tree-root-max-ms 0.0))
            (setq timed (pi-coding-agent-tu-bench--time-display-event
                         orig event type)
                  tree-row (pi-coding-agent-tu-bench--tree-root-row)))
          (let* ((after (pi-coding-agent-tu-bench--cooling-state-counts))
                 (semantic (pi-coding-agent-tu-bench--semantic-summary)))
            (setq pi-coding-agent-tu-bench--agent-end-time
                  (plist-get timed :end)
                  pi-coding-agent-tu-bench--drain-start-time
                  (plist-get timed :end)
                  pi-coding-agent-tu-bench--agent-end-observation
                  (list
                   :wallMs (plist-get timed :elapsedMs)
                   :filterId pi-coding-agent-tu-bench--current-filter-id
                   :boundaryBefore (plist-get before :hotTailBoundary)
                   :boundaryAfter (plist-get after :hotTailBoundary)
                   :queueBefore (plist-get before :queueLength)
                   :queueAfter (plist-get after :queueLength)
                   :timerBefore (plist-get before :timer)
                   :timerAfter (plist-get after :timer)
                   :coldBefore (plist-get before :coldTools)
                   :coldAfter (plist-get after :coldTools)
                   :toolOverlaysBefore (plist-get before :toolOverlays)
                   :toolOverlaysAfter (plist-get after :toolOverlays)
                   :collapsedToolOverlaysBefore collapsed-before
                   :liveRegistryBefore (plist-get before :liveRegistry)
                   :liveRegistryAfter (plist-get after :liveRegistry)
                   :currentOverlayBefore (plist-get before :currentOverlays)
                   :currentOverlayAfter (plist-get after :currentOverlays)
                   :semanticLinesAfter (car semantic)
                   :semanticHashAfter (cdr semantic)
                   :gcs (- gcs-done gc-before)
                   :gcMs (* 1000.0 (- gc-elapsed gc-time-before))
                   :treeRoots tree-row))
            (plist-get timed :value)))
      (let ((timed (pi-coding-agent-tu-bench--time-display-event
                    orig event type)))
        (when (equal type "agent_end")
          (setq pi-coding-agent-tu-bench--agent-end-time
                (plist-get timed :end)))
        (plist-get timed :value)))))

(defun pi-coding-agent-tu-bench--around-cooling-slice
    (orig buffer generation)
  "Call cooling worker ORIG for BUFFER and GENERATION and record one slice."
  (if (not (pi-coding-agent-tu-bench--cooling-scenario-p))
      (funcall orig buffer generation)
    (let* ((index (cl-incf pi-coding-agent-tu-bench--cooling-slice-sequence))
           (buffer-live-before (buffer-live-p buffer))
           (queue-before
            (if buffer-live-before
                (with-current-buffer buffer
                  (length pi-coding-agent--tool-cooling-queue))
              -1))
           (candidate
            (and buffer-live-before
                 (with-current-buffer buffer
                   (car pi-coding-agent--tool-cooling-queue))))
           (candidate-id
            (and candidate
                 (pi-coding-agent-tu-bench--overlay-tool-call-id candidate)))
           (candidate-eligible
            (and candidate buffer-live-before
                 (with-current-buffer buffer
                   (when-let* ((boundary
                                (pi-coding-agent--tool-cooling-boundary)))
                     (pi-coding-agent--completed-tool-overlay-before-p
                      candidate boundary)))))
           (timer-before
            (and buffer-live-before
                 (with-current-buffer buffer
                   (and pi-coding-agent--tool-cooling-timer t))))
           (gc-before gcs-done)
           (gc-time-before gc-elapsed)
           (start (float-time))
           (pi-coding-agent-tu-bench--tree-root-phase "cooling_slice")
           (pi-coding-agent-tu-bench--tree-root-count 0)
           (pi-coding-agent-tu-bench--tree-root-ms 0.0)
           (pi-coding-agent-tu-bench--tree-root-max-ms 0.0)
           value end)
      (unwind-protect
          (setq value (funcall orig buffer generation))
        (setq end (float-time))
        (let* ((buffer-live-after (buffer-live-p buffer))
               (queue-after
                (if buffer-live-after
                    (with-current-buffer buffer
                      (length pi-coding-agent--tool-cooling-queue))
                  -1))
               (timer-after
                (and buffer-live-after
                     (with-current-buffer buffer
                       (and pi-coding-agent--tool-cooling-timer t))))
               (delta (and (>= queue-before 0) (>= queue-after 0)
                           (- queue-before queue-after)))
               (row
                (list
                 :index index
                 :queueBefore queue-before
                 :queueAfter queue-after
                 :queueDelta delta
                 :candidateId (or candidate-id :json-null)
                 :candidateEligible
                 (pi-coding-agent-tu-bench--json-bool candidate-eligible)
                 :candidateOverlayAliveAfter
                 (pi-coding-agent-tu-bench--json-bool
                  (and candidate (overlay-buffer candidate)))
                 :timerBefore
                 (pi-coding-agent-tu-bench--json-bool timer-before)
                 :timerAfter
                 (pi-coding-agent-tu-bench--json-bool timer-after)
                 :wallMs (* 1000.0 (- end start))
                 :gcs (- gcs-done gc-before)
                 :gcMs (* 1000.0 (- gc-elapsed gc-time-before))
                 :treeRoots (pi-coding-agent-tu-bench--tree-root-row)
                 :window (pi-coding-agent-tu-bench--capture-window-state))))
          (push row pi-coding-agent-tu-bench--cooling-slice-log)))
      value)))

(defun pi-coding-agent-tu-bench--around-display-warning (orig &rest args)
  "Call `display-warning' ORIG with ARGS and record scheduler errors."
  (let ((text (format "%s" (nth 1 args))))
    (when (and (pi-coding-agent-tu-bench--cooling-scenario-p)
               (string-match-p "Deferred tool block cooling failed" text))
      (push text pi-coding-agent-tu-bench--scheduler-errors)))
  (apply orig args))

(defun pi-coding-agent-tu-bench--install-advice ()
  "Install event, render, and scenario-specific cooling measurement advice."
  (advice-add 'pi-coding-agent--handle-display-event
              :around #'pi-coding-agent-tu-bench--around-handle-event)
  (advice-add 'pi-coding-agent--tool-block-replace-body
              :around #'pi-coding-agent-tu-bench--around-replace-body)
  (advice-add 'pi-coding-agent--display-tool-end
              :around #'pi-coding-agent-tu-bench--around-display-tool-end)
  (when (pi-coding-agent-tu-bench--cooling-scenario-p)
    (advice-add 'pi-coding-agent--process-filter
                :around #'pi-coding-agent-tu-bench--around-process-filter)
    (advice-add 'pi-coding-agent--run-tool-cooling-slice
                :around #'pi-coding-agent-tu-bench--around-cooling-slice)
    (advice-add 'display-warning
                :around #'pi-coding-agent-tu-bench--around-display-warning)
    (when (fboundp 'treesit-parser-root-node)
      (advice-add 'treesit-parser-root-node
                  :around #'pi-coding-agent-tu-bench--around-tree-root))))

(defun pi-coding-agent-tu-bench--remove-advice ()
  "Remove all benchmark advice."
  (advice-remove 'pi-coding-agent--process-filter
                 #'pi-coding-agent-tu-bench--around-process-filter)
  (advice-remove 'pi-coding-agent--handle-display-event
                 #'pi-coding-agent-tu-bench--around-handle-event)
  (advice-remove 'pi-coding-agent--tool-block-replace-body
                 #'pi-coding-agent-tu-bench--around-replace-body)
  (advice-remove 'pi-coding-agent--display-tool-end
                 #'pi-coding-agent-tu-bench--around-display-tool-end)
  (advice-remove 'pi-coding-agent--run-tool-cooling-slice
                 #'pi-coding-agent-tu-bench--around-cooling-slice)
  (advice-remove 'display-warning
                 #'pi-coding-agent-tu-bench--around-display-warning)
  (when (fboundp 'treesit-parser-root-node)
    (advice-remove 'treesit-parser-root-node
                   #'pi-coding-agent-tu-bench--around-tree-root)))

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

(defun pi-coding-agent-tu-bench--window-line-at (buffer position)
  "Return BUFFER's source line at POSITION without text properties."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (min (max (point-min) position) (point-max)))
      (buffer-substring-no-properties
       (line-beginning-position) (line-end-position)))))

(defun pi-coding-agent-tu-bench--capture-window-state ()
  "Return the cooling chat window's current logical sentinel/follow state."
  (let* ((buffer pi-coding-agent-tu-bench--cooling-chat-buffer)
         (window (and (buffer-live-p buffer)
                      (get-buffer-window buffer t))))
    (if (not (and (buffer-live-p buffer) (window-live-p window)))
        (list :available :json-false
              :expectedRoute pi-coding-agent-tu-bench--command-last-route
              :viewOk t)
      (with-current-buffer buffer
        (let* ((start (window-start window))
               (window-point-position (window-point window))
               (start-line
                (pi-coding-agent-tu-bench--window-line-at buffer start))
               (point-line
                (pi-coding-agent-tu-bench--window-line-at
                 buffer window-point-position))
               (following (pi-coding-agent--window-following-p window))
               (sentinel-p
                (and (string-prefix-p "COOLING-WINDOW-SENTINEL" start-line)
                     (string-prefix-p "COOLING-WINDOW-SENTINEL" point-line)))
               (view-ok
                (if (equal pi-coding-agent-tu-bench--command-last-route
                           "sentinel")
                    sentinel-p
                  following)))
          (list :available t
                :expectedRoute pi-coding-agent-tu-bench--command-last-route
                :windowStart start
                :windowPoint window-point-position
                :pointMax (point-max)
                :startLine start-line
                :pointLine point-line
                :following
                (pi-coding-agent-tu-bench--json-bool following)
                :sentinel
                (pi-coding-agent-tu-bench--json-bool sentinel-p)
                :viewOk
                (pi-coding-agent-tu-bench--json-bool view-ok)))))))

(defun pi-coding-agent-tu-bench--command-pre-hook ()
  "Record entry through the benchmark command's pre-command hook."
  (when (eq this-command
            'pi-coding-agent-tu-bench--scroll-heartbeat-command)
    (setq pi-coding-agent-tu-bench--command-current-job
          (plist-put pi-coding-agent-tu-bench--command-current-job
                     :preHookAt (float-time)))))

(defun pi-coding-agent-tu-bench--command-post-hook ()
  "Record exit through the benchmark command's post-command hook."
  (when (eq this-command
            'pi-coding-agent-tu-bench--scroll-heartbeat-command)
    (setq pi-coding-agent-tu-bench--command-current-job
          (plist-put pi-coding-agent-tu-bench--command-current-job
                     :postHookAt (float-time)))))

(defun pi-coding-agent-tu-bench--scroll-heartbeat-command ()
  "Route one deterministic sentinel/follow scroll heartbeat."
  (interactive)
  (let* ((job pi-coding-agent-tu-bench--command-current-job)
         (route (plist-get job :route))
         (buffer pi-coding-agent-tu-bench--cooling-chat-buffer)
         (window (and (buffer-live-p buffer)
                      (get-buffer-window buffer t))))
    (unless (buffer-live-p buffer)
      (error "Cooling command route lost its chat buffer"))
    (with-current-buffer buffer
      (let ((sentinel
             (and (markerp pi-coding-agent-tu-bench--window-sentinel-marker)
                  (marker-position
                   pi-coding-agent-tu-bench--window-sentinel-marker))))
        (unless sentinel
          (error "Cooling command route lost its logical sentinel"))
        (if (window-live-p window)
            (with-selected-window window
              (if (equal route "sentinel")
                  (progn
                    (goto-char sentinel)
                    (set-window-start window sentinel t)
                    (set-window-point window sentinel))
                (goto-char (point-max))
                (set-window-point window (point-max))))
          ;; Batch is a secondary lane without a window, but still traverses
          ;; the same unread-event/key-binding/command-hook route.
          (goto-char (if (equal route "sentinel")
                         sentinel
                       (point-max))))))
    (setq pi-coding-agent-tu-bench--command-last-route route)))

(defun pi-coding-agent-tu-bench--enqueue-command-event
    (sequence due route)
  "Enqueue scroll command SEQUENCE for DUE using logical ROUTE."
  (setq pi-coding-agent-tu-bench--command-timer nil)
  (when pi-coding-agent-tu-bench--command-drain-active
    (setq pi-coding-agent-tu-bench--command-pending
          (nconc pi-coding-agent-tu-bench--command-pending
                 (list (list :sequence sequence
                             :dueAt due
                             :enqueuedAt (float-time)
                             :route route))))
    (setq unread-command-events
          (append unread-command-events
                  (list pi-coding-agent-tu-bench-command-event)))))

(defun pi-coding-agent-tu-bench--schedule-command-event ()
  "Arm one benchmark command event when the cooling drain is active."
  (when (and pi-coding-agent-tu-bench--command-drain-active
             (not pi-coding-agent-tu-bench--command-timer)
             (null pi-coding-agent-tu-bench--command-pending))
    (let* ((sequence
            (cl-incf pi-coding-agent-tu-bench--command-sequence))
           (route (if (cl-oddp sequence) "sentinel" "follow"))
           (due (+ (float-time)
                   pi-coding-agent-tu-bench-command-interval)))
      (setq pi-coding-agent-tu-bench--command-timer
            (run-at-time pi-coding-agent-tu-bench-command-interval nil
                         #'pi-coding-agent-tu-bench--enqueue-command-event
                         sequence due route)))))

(defun pi-coding-agent-tu-bench--route-command-event (event)
  "Route unread EVENT through key lookup, hooks, and `command-execute'."
  (when (eq event pi-coding-agent-tu-bench-command-event)
    (let* ((job (pop pi-coding-agent-tu-bench--command-pending))
           (buffer pi-coding-agent-tu-bench--cooling-chat-buffer)
           (command
            (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (key-binding (vector event) t))))
           (started (float-time))
           error-text)
      (if (not job)
          (setq error-text "Command event had no scheduled job")
        (setq job (plist-put job :startedAt started)
              pi-coding-agent-tu-bench--command-current-job job)
        (condition-case error-data
            (if (not (eq command
                         'pi-coding-agent-tu-bench--scroll-heartbeat-command))
                (error "Command event resolved to %S" command)
              (with-current-buffer buffer
                (let ((this-command command)
                      (real-this-command command))
                  (run-hooks 'pre-command-hook)
                  (unwind-protect
                      (command-execute command)
                    (run-hooks 'post-command-hook)))))
          (error
           (setq error-text (error-message-string error-data))))
        (setq job pi-coding-agent-tu-bench--command-current-job))
      (let* ((ended (float-time))
             (row
              (append
               job
               (list
                :command (and command (symbol-name command))
                :endedAt ended
                :latenessMs
                (if-let* ((due (plist-get job :dueAt)))
                    (* 1000.0 (max 0.0 (- started due)))
                  0.0)
                :queueDelayMs
                (if-let* ((enqueued (plist-get job :enqueuedAt)))
                    (* 1000.0 (max 0.0 (- started enqueued)))
                  0.0)
                :durationMs (* 1000.0 (- ended started))
                :preHook
                (pi-coding-agent-tu-bench--json-bool
                 (plist-get job :preHookAt))
                :postHook
                (pi-coding-agent-tu-bench--json-bool
                 (plist-get job :postHookAt))
                :error (or error-text :json-null)
                :window (pi-coding-agent-tu-bench--capture-window-state)))))
        (push row pi-coding-agent-tu-bench--command-log))
      (setq pi-coding-agent-tu-bench--command-current-job nil)
      (pi-coding-agent-tu-bench--schedule-command-event))
    t))

(defun pi-coding-agent-tu-bench--install-command-route (chat-buf)
  "Install the benchmark-safe unread command route in CHAT-BUF."
  (setq pi-coding-agent-tu-bench--cooling-chat-buffer chat-buf
        pi-coding-agent-tu-bench--command-last-route "follow")
  (with-current-buffer chat-buf
    (use-local-map (copy-keymap (current-local-map)))
    (define-key (current-local-map)
                (vector pi-coding-agent-tu-bench-command-event)
                #'pi-coding-agent-tu-bench--scroll-heartbeat-command)
    (add-hook 'pre-command-hook
              #'pi-coding-agent-tu-bench--command-pre-hook nil t)
    (add-hook 'post-command-hook
              #'pi-coding-agent-tu-bench--command-post-hook nil t)
    (save-excursion
      (goto-char (point-min))
      (unless (search-forward "COOLING-WINDOW-SENTINEL" nil t)
        (error "Cooling fixture did not render its window sentinel"))
      (setq pi-coding-agent-tu-bench--window-sentinel-marker
            (copy-marker (line-beginning-position) nil)))
    (goto-char (point-max)))
  (when-let* ((window (get-buffer-window chat-buf t)))
    (select-window window)
    (set-window-point window
                      (with-current-buffer chat-buf (point-max)))))

(defun pi-coding-agent-tu-bench--start-command-route ()
  "Start one-shot command scheduling for a natural cooling drain."
  (setq pi-coding-agent-tu-bench--command-drain-active t)
  (pi-coding-agent-tu-bench--schedule-command-event))

(defun pi-coding-agent-tu-bench--stop-command-route ()
  "Stop command scheduling and remove unread benchmark events."
  (setq pi-coding-agent-tu-bench--command-drain-active nil)
  (when (timerp pi-coding-agent-tu-bench--command-timer)
    (cancel-timer pi-coding-agent-tu-bench--command-timer))
  (setq pi-coding-agent-tu-bench--command-timer nil
        pi-coding-agent-tu-bench--command-pending nil
        unread-command-events
        (delq pi-coding-agent-tu-bench-command-event
              unread-command-events)))

(defun pi-coding-agent-tu-bench--cooling-drained-p (chat-buf)
  "Return non-nil when CHAT-BUF owns no cooling queue or timer."
  (and (buffer-live-p chat-buf)
       (with-current-buffer chat-buf
         (and (null pi-coding-agent--tool-cooling-queue)
              (null pi-coding-agent--tool-cooling-timer)))))

(defun pi-coding-agent-tu-bench--wait-for-natural-cooling
    (chat-buf timeout)
  "Wait up to TIMEOUT for CHAT-BUF's real one-shot cooling timers to drain."
  (let ((start (or pi-coding-agent-tu-bench--drain-start-time
                   (float-time)))
        (gc-before gcs-done)
        (gc-time-before gc-elapsed)
        (deadline (+ (float-time) timeout))
        settled end)
    (pi-coding-agent-tu-bench--install-command-route chat-buf)
    (pi-coding-agent-tu-bench--start-probe)
    (pi-coding-agent-tu-bench--start-command-route)
    (unwind-protect
        (while (and (not settled) (< (float-time) deadline))
          ;; `read-event' gives Emacs its ordinary timer/redisplay opportunity;
          ;; no explicit redisplay is forced in this timed interval.
          (when-let* ((event (read-event nil nil 0.01)))
            (pi-coding-agent-tu-bench--route-command-event event))
          (accept-process-output nil 0.005)
          (when (and (pi-coding-agent-tu-bench--cooling-drained-p chat-buf)
                     (null pi-coding-agent-tu-bench--command-pending))
            (setq settled t)))
      (setq end (float-time))
      (pi-coding-agent-tu-bench--stop-command-route)
      (pi-coding-agent-tu-bench--stop-probe))
    (list :settled (pi-coding-agent-tu-bench--json-bool settled)
          :timeoutSeconds timeout
          :wallMs (* 1000.0 (- end start))
          :activeMs
          (cl-loop for row in pi-coding-agent-tu-bench--cooling-slice-log
                   sum (plist-get row :wallMs))
          :callbacks (length pi-coding-agent-tu-bench--cooling-slice-log)
          :gcs (- gcs-done gc-before)
          :gcMs (* 1000.0 (- gc-elapsed gc-time-before))
          :finalWindow (pi-coding-agent-tu-bench--capture-window-state))))

(defun pi-coding-agent-tu-bench--command-stats ()
  "Return command scheduling and duration summary statistics."
  (let* ((rows pi-coding-agent-tu-bench--command-log)
         (lateness (mapcar (lambda (row) (plist-get row :latenessMs)) rows))
         (durations (mapcar (lambda (row) (plist-get row :durationMs)) rows)))
    (list :count (length rows)
          :sentinelRoutes
          (cl-count "sentinel" rows :key (lambda (row) (plist-get row :route))
                    :test #'equal)
          :followRoutes
          (cl-count "follow" rows :key (lambda (row) (plist-get row :route))
                    :test #'equal)
          :latenessP50Ms
          (pi-coding-agent-tu-bench--percentile lateness 0.50)
          :latenessP95Ms
          (pi-coding-agent-tu-bench--percentile lateness 0.95)
          :latenessMaxMs (if lateness (apply #'max lateness) 0.0)
          :durationP50Ms
          (pi-coding-agent-tu-bench--percentile durations 0.50)
          :durationP95Ms
          (pi-coding-agent-tu-bench--percentile durations 0.95)
          :durationMaxMs (if durations (apply #'max durations) 0.0))))

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

(defun pi-coding-agent-tu-bench--parser-font-lock-state (chat-buf)
  "Return final parser-root and font-lock usability metrics for CHAT-BUF."
  (if (not (buffer-live-p chat-buf))
      (list :parserOk :json-false
            :fontLockOk :json-false
            :error "chat buffer is not live")
    (with-current-buffer chat-buf
      (let ((pi-coding-agent-tu-bench--tree-root-phase "final_check")
            (pi-coding-agent-tu-bench--tree-root-count 0)
            (pi-coding-agent-tu-bench--tree-root-ms 0.0)
            (pi-coding-agent-tu-bench--tree-root-max-ms 0.0)
            parser root root-start root-end root-type
            parser-error font-lock-error
            (font-lock-start (float-time))
            font-lock-ms)
        (condition-case error-data
            (progn
              (setq parser (pi-coding-agent--markdown-parser))
              (unless parser
                (error "No Markdown tree-sitter parser"))
              (setq root (treesit-parser-root-node parser)
                    root-start (treesit-node-start root)
                    root-end (treesit-node-end root)
                    root-type (treesit-node-type root)))
          (error
           (setq parser-error (error-message-string error-data))))
        (setq font-lock-start (float-time))
        (condition-case error-data
            (font-lock-ensure (point-min) (point-max))
          (error
           (setq font-lock-error (error-message-string error-data))))
        (setq font-lock-ms (* 1000.0 (- (float-time) font-lock-start)))
        (list
         :parserOk
         (pi-coding-agent-tu-bench--json-bool
          (and root
               (<= root-start (point-min))
               (>= root-end (1- (point-max)))))
         :fontLockOk
         (pi-coding-agent-tu-bench--json-bool (null font-lock-error))
         :parserCount (length (treesit-parser-list))
         :rootType (or root-type :json-null)
         :rootStart (or root-start :json-null)
         :rootEnd (or root-end :json-null)
         :fontLockMs font-lock-ms
         :parserError (or parser-error :json-null)
         :fontLockError (or font-lock-error :json-null)
         :treeRoots (pi-coding-agent-tu-bench--tree-root-row))))))

(defun pi-coding-agent-tu-bench--tool-name-counts (metadata)
  "Return cooling tool-name counts from cold target METADATA."
  (list :bash (cl-count "bash" metadata
                        :key (lambda (entry) (plist-get entry :tool-name))
                        :test #'equal)
        :read (cl-count "read" metadata
                        :key (lambda (entry) (plist-get entry :tool-name))
                        :test #'equal)
        :write (cl-count "write" metadata
                         :key (lambda (entry) (plist-get entry :tool-name))
                         :test #'equal)
        :edit (cl-count "edit" metadata
                        :key (lambda (entry) (plist-get entry :tool-name))
                        :test #'equal)))

(defun pi-coding-agent-tu-bench--cooling-final-state (chat-buf parser-state)
  "Return exact settled cooling state for CHAT-BUF and PARSER-STATE."
  (if (not (buffer-live-p chat-buf))
      (list :bufferLive :json-false :parser parser-state)
    (with-current-buffer chat-buf
      (let* ((counts (pi-coding-agent-tu-bench--cooling-state-counts))
             (semantic (pi-coding-agent-tu-bench--semantic-summary))
             (metadata (pi-coding-agent-tu-bench--cold-tool-metadata))
             (tool-overlays (pi-coding-agent-tu-bench--tool-overlays))
             (current
              (seq-find
               (lambda (overlay)
                 (equal (pi-coding-agent-tu-bench--overlay-tool-call-id
                         overlay)
                        pi-coding-agent-tu-bench-cooling-live-id))
               tool-overlays))
             (boundary (plist-get counts :hotTailBoundary))
             (window (get-buffer-window chat-buf t)))
        (list
         :bufferLive t
         :coldTools (plist-get counts :coldTools)
         :coldToolNames
         (pi-coding-agent-tu-bench--tool-name-counts metadata)
         :pathBearingColdTools
         (cl-count-if (lambda (entry) (plist-get entry :path)) metadata)
         :toolOverlays (plist-get counts :toolOverlays)
         :toolOverlayIds
         (vconcat
          (sort (delq nil
                      (mapcar
                       #'pi-coding-agent-tu-bench--overlay-tool-call-id
                       tool-overlays))
                #'string<))
         :currentOverlayStart
         (if current (overlay-start current) :json-null)
         :currentOverlayEnd
         (if current (overlay-end current) :json-null)
         :currentInsideHotTail
         (pi-coding-agent-tu-bench--json-bool
          (and current boundary (>= (overlay-start current) boundary)))
         :currentColdProperty
         (pi-coding-agent-tu-bench--json-bool
          (and current
               (get-text-property
                (overlay-start current)
                'pi-coding-agent-cold-tool-block)))
         :hotTailBoundary (or boundary :json-null)
         :buttons (pi-coding-agent-tu-bench--button-count)
         :diffOverlays
         (cl-count-if
          (lambda (overlay)
            (overlay-get overlay 'pi-coding-agent-diff-overlay))
          (overlays-in (point-min) (point-max)))
         :tableOverlays
         (cl-count-if
          (lambda (overlay)
            (overlay-get overlay 'pi-coding-agent-table-display))
          (overlays-in (point-min) (point-max)))
         :liveToolRegistry (plist-get counts :liveRegistry)
         :pendingToolOverlay
         (pi-coding-agent-tu-bench--json-bool
          pi-coding-agent--pending-tool-overlay)
         :queueLength (plist-get counts :queueLength)
         :timerOwned
         (pi-coding-agent-tu-bench--json-bool
          (plist-get counts :timer))
         :semanticLineCount (car semantic)
         :semanticHash (cdr semantic)
         :expectedSemanticHash
         (pi-coding-agent-tu-bench--expected-semantic-hash)
         :window (pi-coding-agent-tu-bench--capture-window-state)
         :geometry
         (if (and pi-coding-agent-tu-bench-display-buffers
                  (not noninteractive)
                  (window-live-p window))
             (list :frameColumns (frame-width (window-frame window))
                   :frameLines (frame-height (window-frame window))
                   :chatColumns (window-width window)
                   :chatLines (window-height window))
           :json-null)
         :parser parser-state)))))

(defun pi-coding-agent-tu-bench--cooling-slices-in-order ()
  "Return cooling slice rows in callback order."
  (nreverse (copy-sequence pi-coding-agent-tu-bench--cooling-slice-log)))

(defun pi-coding-agent-tu-bench--commands-in-order ()
  "Return command rows in scheduling order."
  (nreverse (copy-sequence pi-coding-agent-tu-bench--command-log)))

(defun pi-coding-agent-tu-bench--tree-root-summary ()
  "Return lightweight tree-root metrics split across agent_end and slices."
  (let* ((agent-row
          (plist-get pi-coding-agent-tu-bench--agent-end-observation
                     :treeRoots))
         (slice-rows (pi-coding-agent-tu-bench--cooling-slices-in-order)))
    (list
     :agentEnd (or agent-row
                   (list :phase "agent_end" :count 0
                         :totalMs 0.0 :maxMs 0.0))
     :coolingSlices
     (list
      :count (cl-loop for row in slice-rows
                      sum (plist-get (plist-get row :treeRoots) :count))
      :totalMs (cl-loop for row in slice-rows
                        sum (plist-get (plist-get row :treeRoots) :totalMs))
      :maxMs (if slice-rows
                 (apply #'max
                        (mapcar
                         (lambda (row)
                           (plist-get (plist-get row :treeRoots) :maxMs))
                         slice-rows))
               0.0)))))

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

(defun pi-coding-agent-tu-bench--collect-cooling-checks
    (chat-buf drain final-state)
  "Build structural check rows for CHAT-BUF cooling DRAIN and FINAL-STATE."
  (let* ((expected (pi-coding-agent-tu-bench--fill-tool-count))
         (observation pi-coding-agent-tu-bench--agent-end-observation)
         (slices (pi-coding-agent-tu-bench--cooling-slices-in-order))
         (commands (pi-coding-agent-tu-bench--commands-in-order))
         (names (plist-get final-state :coldToolNames))
         (parser (plist-get final-state :parser))
         (expected-paths (+ pi-coding-agent-tu-bench-fill-read
                            pi-coding-agent-tu-bench-fill-write
                            pi-coding-agent-tu-bench-fill-edit))
         (expected-semantic-lines (+ expected 2))
         (expected-hash (pi-coding-agent-tu-bench--expected-semantic-hash))
         (slice-deltas
          (mapcar (lambda (row) (plist-get row :queueDelta)) slices))
         (progress-slices
          (seq-filter (lambda (row)
                        (= 1 (or (plist-get row :queueDelta) -1)))
                      slices))
         checks)
    (push
     (pi-coding-agent-tu-bench--check
      "agent-end-real-process-filter-path"
      (and pi-coding-agent-tu-bench--agent-end-time
           observation
           pi-coding-agent-tu-bench--agent-end-filter)
      (if pi-coding-agent-tu-bench--agent-end-filter
          (format "agent_end event %.3f ms inside filter %d (%.3f ms)"
                  (plist-get observation :wallMs)
                  (plist-get pi-coding-agent-tu-bench--agent-end-filter :id)
                  (plist-get pi-coding-agent-tu-bench--agent-end-filter :wallMs))
        "agent_end had no enclosing real process-filter observation"))
     checks)
    (push
     (pi-coding-agent-tu-bench--check
      "hot-tail-boundary-advances-only-at-agent-end"
      (and (= (or (plist-get observation :boundaryBefore) -1) 1)
           (> (or (plist-get observation :boundaryAfter) 0) 1))
      (format "boundary %s -> %s"
              (plist-get observation :boundaryBefore)
              (plist-get observation :boundaryAfter)))
     checks)
    (push
     (pi-coding-agent-tu-bench--check
      "current-tool-live-on-agent-end-entry"
      (and (= (or (plist-get observation :liveRegistryBefore) -1) 1)
           (= (or (plist-get observation :currentOverlayBefore) -1) 1))
      (format "live registry %s; current overlays %s"
              (plist-get observation :liveRegistryBefore)
              (plist-get observation :currentOverlayBefore)))
     checks)
    (push
     (pi-coding-agent-tu-bench--check
      "agent-end-returns-with-whole-cohort-deferred"
      (and (= (or (plist-get observation :queueAfter) -1) expected)
           (eq (plist-get observation :timerAfter) t)
           (= (or (plist-get observation :coldAfter) -1) 0)
           (= (or (plist-get observation :toolOverlaysAfter) -1)
              (1+ expected)))
      (format "queue=%s timer=%s cold=%s hot-overlays=%s expected cohort=%d"
              (plist-get observation :queueAfter)
              (plist-get observation :timerAfter)
              (plist-get observation :coldAfter)
              (plist-get observation :toolOverlaysAfter)
              expected))
     checks)
    (push
     (pi-coding-agent-tu-bench--check
      "cohort-has-collapsed-and-short-tools"
      (let ((collapsed
             (or (plist-get observation :collapsedToolOverlaysBefore) 0)))
        (and (> collapsed 0) (< collapsed expected)))
      (format "%s collapsed overlays among %d completed fixture tools"
              (plist-get observation :collapsedToolOverlaysBefore) expected))
     checks)
    (push
     (pi-coding-agent-tu-bench--check
      "one-candidate-maximum-per-natural-callback"
      (and (>= (length slices) expected)
           (cl-every (lambda (delta)
                       (and (integerp delta) (memq delta '(0 1))))
                     slice-deltas)
           (= expected (apply #'+ (or slice-deltas '(0))))
           (= expected (length progress-slices))
           (seq-every-p
            (lambda (row)
              (and (eq (plist-get row :candidateEligible) t)
                   (eq (plist-get row :candidateOverlayAliveAfter)
                       :json-false)))
            progress-slices))
      (format "%d callbacks, %d one-overlay progress slices, deltas in [0,1]"
              (length slices) (length progress-slices)))
     checks)
    (push
     (pi-coding-agent-tu-bench--check
      "natural-drain-settles-before-timeout"
      (eq (plist-get drain :settled) t)
      (format "wall %.3f ms, active %.3f ms, timeout %s s"
              (or (plist-get drain :wallMs) 0.0)
              (or (plist-get drain :activeMs) 0.0)
              (plist-get drain :timeoutSeconds)))
     checks)
    (push
     (pi-coding-agent-tu-bench--check
      "exact-final-cold-hot-counts"
      (and (= (or (plist-get final-state :coldTools) -1) expected)
           (= (or (plist-get final-state :toolOverlays) -1) 1)
           (equal (plist-get final-state :toolOverlayIds)
                  (vector pi-coding-agent-tu-bench-cooling-live-id)))
      (format "cold=%s hot-overlays=%s ids=%S"
              (plist-get final-state :coldTools)
              (plist-get final-state :toolOverlays)
              (plist-get final-state :toolOverlayIds)))
     checks)
    (push
     (pi-coding-agent-tu-bench--check
      "exact-cold-tool-cohort-composition"
      (and (= (or (plist-get names :bash) -1)
              pi-coding-agent-tu-bench-fill-bash)
           (= (or (plist-get names :read) -1)
              pi-coding-agent-tu-bench-fill-read)
           (= (or (plist-get names :write) -1)
              pi-coding-agent-tu-bench-fill-write)
           (= (or (plist-get names :edit) -1)
              pi-coding-agent-tu-bench-fill-edit)
           (= (or (plist-get final-state :pathBearingColdTools) -1)
              expected-paths))
      (format "names=%S path-bearing=%s/%d"
              names (plist-get final-state :pathBearingColdTools)
              expected-paths))
     checks)
    (push
     (pi-coding-agent-tu-bench--check
      "current-inside-hot-tail-stays-hot"
      (and (eq (plist-get final-state :currentInsideHotTail) t)
           (eq (plist-get final-state :currentColdProperty) :json-false)
           (= 1 (pi-coding-agent-tu-bench--count-occurrences
                 chat-buf "COOLING-SEMANTIC-LIVE")))
      (format "inside=%s cold-property=%s live sentinel occurrences=%d"
              (plist-get final-state :currentInsideHotTail)
              (plist-get final-state :currentColdProperty)
              (pi-coding-agent-tu-bench--count-occurrences
               chat-buf "COOLING-SEMANTIC-LIVE")))
     checks)
    (push
     (pi-coding-agent-tu-bench--check
      "semantic-sentinel-projection-hash-holds"
      (and (= (or (plist-get observation :semanticLinesAfter) -1)
              expected-semantic-lines)
           (= (or (plist-get final-state :semanticLineCount) -1)
              expected-semantic-lines)
           (equal (plist-get observation :semanticHashAfter) expected-hash)
           (equal (plist-get final-state :semanticHash) expected-hash))
      (format "agent_end=%s final=%s expected=%s lines=%s/%d"
              (plist-get observation :semanticHashAfter)
              (plist-get final-state :semanticHash)
              expected-hash
              (plist-get final-state :semanticLineCount)
              expected-semantic-lines))
     checks)
    (let ((table-pos nil)
          (current-pos (plist-get final-state :currentOverlayStart)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (save-excursion
            (goto-char (point-min))
            (when (search-forward "COOLING-SEMANTIC-HOT-TABLE" nil t)
              (setq table-pos (match-beginning 0))))))
      (push
       (pi-coding-agent-tu-bench--check
        "adjacent-hot-tail-table-remains-usable"
        (and table-pos
             (integerp current-pos)
             (< table-pos current-pos)
             (or (not pi-coding-agent-tu-bench-display-buffers)
                 (> (or (plist-get final-state :tableOverlays) 0) 0)))
        (format "table-pos=%s current-pos=%s display-overlays=%s"
                table-pos current-pos
                (plist-get final-state :tableOverlays)))
       checks))
    (push
     (pi-coding-agent-tu-bench--check
      "no-stale-buttons-diffs-or-tool-overlays"
      (and (= (or (plist-get final-state :buttons) -1) 0)
           (= (or (plist-get final-state :diffOverlays) -1) 0)
           (= (or (plist-get final-state :toolOverlays) -1) 1))
      (format "buttons=%s diffs=%s tool-overlays=%s"
              (plist-get final-state :buttons)
              (plist-get final-state :diffOverlays)
              (plist-get final-state :toolOverlays)))
     checks)
    (push
     (pi-coding-agent-tu-bench--check
      "cooling-scheduler-and-live-registry-clean"
      (and (= (or (plist-get final-state :queueLength) -1) 0)
           (eq (plist-get final-state :timerOwned) :json-false)
           (= (or (plist-get final-state :liveToolRegistry) -1) 0)
           (eq (plist-get final-state :pendingToolOverlay) :json-false)
           (null pi-coding-agent-tu-bench--scheduler-errors))
      (format "queue=%s timer=%s live=%s pending=%s scheduler-errors=%S"
              (plist-get final-state :queueLength)
              (plist-get final-state :timerOwned)
              (plist-get final-state :liveToolRegistry)
              (plist-get final-state :pendingToolOverlay)
              pi-coding-agent-tu-bench--scheduler-errors))
     checks)
    (push
     (pi-coding-agent-tu-bench--check
      "parser-root-and-font-lock-usable"
      (and (eq (plist-get parser :parserOk) t)
           (eq (plist-get parser :fontLockOk) t))
      (format "parser=%s font-lock=%s root=%s parser-error=%s font-lock-error=%s"
              (plist-get parser :parserOk)
              (plist-get parser :fontLockOk)
              (plist-get parser :rootType)
              (plist-get parser :parserError)
              (plist-get parser :fontLockError)))
     checks)
    (push
     (pi-coding-agent-tu-bench--check
      "command-loop-scroll-heartbeats-routed"
      (and (>= (length commands) 2)
           (seq-some (lambda (row)
                       (equal (plist-get row :route) "sentinel"))
                     commands)
           (seq-some (lambda (row)
                       (equal (plist-get row :route) "follow"))
                     commands)
           (seq-every-p
            (lambda (row)
              (and (eq (plist-get row :preHook) t)
                   (eq (plist-get row :postHook) t)
                   (eq (plist-get row :error) :json-null)
                   (eq (plist-get (plist-get row :window) :viewOk) t)))
            commands)
           (seq-every-p
            (lambda (row)
              (eq (plist-get (plist-get row :window) :viewOk) t))
            slices)
           (eq (plist-get (plist-get drain :finalWindow) :viewOk) t))
      (format "%d commands (%d sentinel/%d follow); final route=%s view-ok=%s"
              (length commands)
              (cl-count "sentinel" commands
                        :key (lambda (row) (plist-get row :route))
                        :test #'equal)
              (cl-count "follow" commands
                        :key (lambda (row) (plist-get row :route))
                        :test #'equal)
              (plist-get (plist-get drain :finalWindow) :expectedRoute)
              (plist-get (plist-get drain :finalWindow) :viewOk)))
     checks)
    (push
     (pi-coding-agent-tu-bench--check
      "cooling-wire-event-counts"
      (and (= (pi-coding-agent-tu-bench--event-count
               "tool_execution_start")
              (1+ expected))
           (= (pi-coding-agent-tu-bench--event-count
               "tool_execution_end")
              expected)
           (= (pi-coding-agent-tu-bench--event-count
               "tool_execution_update")
              0)
           (= (pi-coding-agent-tu-bench--event-count "agent_end") 1))
      (format "starts=%d ends=%d updates=%d agent_end=%d"
              (pi-coding-agent-tu-bench--event-count
               "tool_execution_start")
              (pi-coding-agent-tu-bench--event-count
               "tool_execution_end")
              (pi-coding-agent-tu-bench--event-count
               "tool_execution_update")
              (pi-coding-agent-tu-bench--event-count "agent_end")))
     checks)
    (when (and pi-coding-agent-tu-bench-display-buffers
               (not noninteractive))
      (let ((geometry (plist-get final-state :geometry)))
        (push
         (pi-coding-agent-tu-bench--check
          "gui-frame-is-120x40"
          (and (= (or (plist-get geometry :frameColumns) -1) 120)
               (= (or (plist-get geometry :frameLines) -1) 40))
          (format "frame=%sx%s chat=%sx%s"
                  (plist-get geometry :frameColumns)
                  (plist-get geometry :frameLines)
                  (plist-get geometry :chatColumns)
                  (plist-get geometry :chatLines)))
         checks)))
    (nreverse checks)))

(defun pi-coding-agent-tu-bench--cleanup-session-directory (session-dir)
  "Delete generated SESSION-DIR when it belongs to this iteration output."
  (when (and (stringp session-dir)
             (file-directory-p session-dir)
             (file-in-directory-p session-dir
                                  pi-coding-agent-tu-bench-out-dir))
    (delete-directory session-dir t)))

(defun pi-coding-agent-tu-bench--cleanup-session (chat-buf session-dir)
  "Kill CHAT-BUF and its fake process, then delete generated SESSION-DIR."
  (unwind-protect
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
            (kill-buffer input-buf))))
    (pi-coding-agent-tu-bench--cleanup-session-directory session-dir)))

(defun pi-coding-agent-tu-bench--run-session ()
  "Run one fake-backed tool benchmark session and return a metrics plist.
The cooling scenario waits for production one-shot timers to drain naturally;
the storm scenarios retain their existing prompt-to-agent_end settle path."
  (let* ((session-dir (make-temp-file
                       (expand-file-name "session-"
                                         pi-coding-agent-tu-bench-out-dir)
                       t))
         (cooling (pi-coding-agent-tu-bench--cooling-scenario-p))
         (chat-buf nil)
         (ok nil)
         (error-text nil)
         (drain nil)
         (parser-state nil)
         (final-state nil)
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
    (when cooling
      (setq pi-coding-agent-hot-tail-turn-count
            pi-coding-agent-tu-bench-hot-tail-turns))
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
                ;; This setup redisplay is deliberately before the timed drain.
                (redisplay t))
              (garbage-collect)
              (setq gc-before gcs-done
                    gc-time-before gc-elapsed
                    start (float-time))
              (unless cooling
                (pi-coding-agent-tu-bench--start-probe))
              (setq pi-coding-agent-tu-bench--prompt-time (float-time))
              (with-current-buffer chat-buf
                (pi-coding-agent--prepare-and-send
                 (if cooling
                     "run the synthetic deferred agent-end cooling cohort"
                   "run the synthetic tool update storm")))
              (if cooling
                  (progn
                    ;; Suppress the legacy benchmark's explicit redisplay while
                    ;; receiving the final event.  The drain below uses ordinary
                    ;; read-event opportunities and production timers only.
                    (unless
                        (let ((pi-coding-agent-tu-bench-display-buffers nil))
                          (pi-coding-agent-tu-bench--wait-until
                           (lambda ()
                             (let ((proc
                                    (and (buffer-live-p chat-buf)
                                         (with-current-buffer chat-buf
                                           pi-coding-agent--process))))
                               (unless (and (processp proc)
                                            (process-live-p proc))
                                 (error
                                  "Fake pi exited before agent_end cooling"))
                               pi-coding-agent-tu-bench--agent-end-time))
                           pi-coding-agent-tu-bench-timeout-seconds))
                      (error "Timed out waiting for cooling agent_end after %d seconds"
                             pi-coding-agent-tu-bench-timeout-seconds))
                    (setq drain
                          (pi-coding-agent-tu-bench--wait-for-natural-cooling
                           chat-buf pi-coding-agent-tu-bench-timeout-seconds))
                    (let ((proc (with-current-buffer chat-buf
                                  pi-coding-agent--process)))
                      (setq ok
                            (and (eq (plist-get drain :settled) t)
                                 (= 0
                                    (pi-coding-agent-tu-bench--pending-requests-count
                                     proc)))))
                    (unless ok
                      (error "Cooling drain did not settle cleanly within %d seconds"
                             pi-coding-agent-tu-bench-timeout-seconds)))
                (setq ok
                      (pi-coding-agent-tu-bench--wait-until
                       (lambda ()
                         (let ((proc (and (buffer-live-p chat-buf)
                                          (with-current-buffer chat-buf
                                            pi-coding-agent--process))))
                           (unless (and (processp proc) (process-live-p proc))
                             (error
                              "Fake pi process exited before the storm settled"))
                           (and pi-coding-agent-tu-bench--agent-end-time
                                (= 0
                                   (pi-coding-agent-tu-bench--pending-requests-count
                                    proc)))))
                       pi-coding-agent-tu-bench-timeout-seconds))
                (unless ok
                  (error "Timed out waiting for agent_end after %d seconds"
                         pi-coding-agent-tu-bench-timeout-seconds))))
          (error (setq error-text (error-message-string err))))
      (pi-coding-agent-tu-bench--stop-command-route)
      (pi-coding-agent-tu-bench--stop-probe)
      ;; The synthetic cwd is not an advertised artifact.  Delete it in this
      ;; unwind path even when event handling quits or errors.
      (pi-coding-agent-tu-bench--cleanup-session-directory session-dir))
    (when cooling
      (when (and ok (buffer-live-p chat-buf))
        (setq parser-state
              (pi-coding-agent-tu-bench--parser-font-lock-state chat-buf)
              final-state
              (pi-coding-agent-tu-bench--cooling-final-state
               chat-buf parser-state)))
      (unless parser-state
        (setq parser-state
              (list :parserOk :json-false
                    :fontLockOk :json-false
                    :error (or error-text "cooling did not settle"))))
      (unless final-state
        (setq final-state
              (pi-coding-agent-tu-bench--cooling-final-state
               chat-buf parser-state))))
    (unwind-protect
        (let ((checks
               (if cooling
                   (pi-coding-agent-tu-bench--collect-cooling-checks
                    chat-buf drain final-state)
                 (pi-coding-agent-tu-bench--collect-checks chat-buf))))
          (list :settled (pi-coding-agent-tu-bench--json-bool ok)
                :error error-text
                :wallMs (when pi-coding-agent-tu-bench--agent-end-time
                          (* 1000.0
                             (- pi-coding-agent-tu-bench--agent-end-time
                                pi-coding-agent-tu-bench--prompt-time)))
                :seconds (and start (- (float-time) start))
                :gcs (and gc-before (- gcs-done gc-before))
                :gcSeconds
                (and gc-time-before (- gc-elapsed gc-time-before))
                :bufferBytes
                (and (buffer-live-p chat-buf)
                     (with-current-buffer chat-buf (buffer-size)))
                :bufferLines
                (and (buffer-live-p chat-buf)
                     (with-current-buffer chat-buf
                       (count-lines (point-min) (point-max))))
                :overlays
                (and (buffer-live-p chat-buf)
                     (with-current-buffer chat-buf
                       (length (overlays-in (point-min) (point-max)))))
                :drain drain
                :final final-state
                :checks checks))
      (pi-coding-agent-tu-bench--cleanup-session chat-buf session-dir))))

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
        :seed pi-coding-agent-tu-bench-seed
        :hotTailTurns pi-coding-agent-tu-bench-hot-tail-turns
        :commandIntervalMs
        (* 1000.0 pi-coding-agent-tu-bench-command-interval)
        :expectedCoolingCohort
        (if (pi-coding-agent-tu-bench--cooling-scenario-p)
            (pi-coding-agent-tu-bench--fill-tool-count)
          0)))

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

(defun pi-coding-agent-tu-bench--write-cooling-tsv ()
  "Write detailed cooling callback and command TSV artifacts."
  (when (pi-coding-agent-tu-bench--cooling-scenario-p)
    (with-temp-file pi-coding-agent-tu-bench-cooling-slices-file
      (insert (concat
               "index\tqueue_before\tqueue_after\tqueue_delta\t"
               "candidate_id\teligible\toverlay_alive_after\twall_ms\t"
               "tree_root_count\ttree_root_ms\tgcs\tgc_ms\ttimer_after\t"
               "window_route\twindow_ok\n"))
      (dolist (row (pi-coding-agent-tu-bench--cooling-slices-in-order))
        (let ((tree (plist-get row :treeRoots))
              (window (plist-get row :window)))
          (insert
           (format "%d\t%d\t%d\t%d\t%s\t%s\t%s\t%.3f\t%d\t%.3f\t%d\t%.3f\t%s\t%s\t%s\n"
                   (plist-get row :index)
                   (plist-get row :queueBefore)
                   (plist-get row :queueAfter)
                   (plist-get row :queueDelta)
                   (plist-get row :candidateId)
                   (if (eq (plist-get row :candidateEligible) t) 1 0)
                   (if (eq (plist-get row :candidateOverlayAliveAfter) t) 1 0)
                   (plist-get row :wallMs)
                   (plist-get tree :count)
                   (plist-get tree :totalMs)
                   (plist-get row :gcs)
                   (plist-get row :gcMs)
                   (if (eq (plist-get row :timerAfter) t) 1 0)
                   (plist-get window :expectedRoute)
                   (if (eq (plist-get window :viewOk) t) 1 0))))))
    (with-temp-file pi-coding-agent-tu-bench-commands-file
      (insert (concat
               "sequence\troute\tdue_at\tenqueued_at\tstarted_at\tended_at\t"
               "lateness_ms\tqueue_delay_ms\tduration_ms\tpre_hook\tpost_hook\t"
               "window_route\twindow_ok\terror\n"))
      (dolist (row (pi-coding-agent-tu-bench--commands-in-order))
        (let ((window (plist-get row :window)))
          (insert
           (format "%d\t%s\t%.6f\t%.6f\t%.6f\t%.6f\t%.3f\t%.3f\t%.3f\t%s\t%s\t%s\t%s\t%s\n"
                   (plist-get row :sequence)
                   (plist-get row :route)
                   (plist-get row :dueAt)
                   (plist-get row :enqueuedAt)
                   (plist-get row :startedAt)
                   (plist-get row :endedAt)
                   (plist-get row :latenessMs)
                   (plist-get row :queueDelayMs)
                   (plist-get row :durationMs)
                   (if (eq (plist-get row :preHook) t) 1 0)
                   (if (eq (plist-get row :postHook) t) 1 0)
                   (plist-get window :expectedRoute)
                   (if (eq (plist-get window :viewOk) t) 1 0)
                   (if (eq (plist-get row :error) :json-null)
                       ""
                     (plist-get row :error)))))))))

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
         (cooling (pi-coding-agent-tu-bench--cooling-scenario-p))
         (filters (nreverse
                   (copy-sequence pi-coding-agent-tu-bench--filter-log)))
         (command-stats (pi-coding-agent-tu-bench--command-stats))
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
                :timingGuidance
                (list :policy "diagnostic-only"
                      :targetUnderMs 100
                      :concernOverMs 250
                      :severeOverMs 1000
                      :hardFailure :json-false)
                :ok (plist-get metrics :ok)
                :settled (plist-get metrics :settled)
                :error (or (plist-get metrics :error) :json-null)
                :wallMs (or (plist-get metrics :wallMs) :json-null)
                :seconds (plist-get metrics :seconds)
                :gcs (plist-get metrics :gcs)
                :gcSeconds (plist-get metrics :gcSeconds)
                :bufferBytes (plist-get metrics :bufferBytes)
                :bufferLines (plist-get metrics :bufferLines)
                :overlays (plist-get metrics :overlays)
                :agentEnd
                (or pi-coding-agent-tu-bench--agent-end-observation
                    :json-null)
                :processFilters
                (list :count (length filters)
                      :maxMs
                      (if filters
                          (apply #'max
                                 (mapcar (lambda (row)
                                           (plist-get row :wallMs))
                                         filters))
                        0.0)
                      :agentEnd
                      (or pi-coding-agent-tu-bench--agent-end-filter
                          :json-null))
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
                :cooling
                (if cooling
                    (list
                     :drain (or (plist-get metrics :drain) :json-null)
                     :slices
                     (vconcat
                      (pi-coding-agent-tu-bench--cooling-slices-in-order))
                     :commands
                     (list :stats command-stats
                           :rows
                           (vconcat
                            (pi-coding-agent-tu-bench--commands-in-order)))
                     :treeRoots
                     (pi-coding-agent-tu-bench--tree-root-summary)
                     :schedulerErrors
                     (vconcat
                      (nreverse
                       (copy-sequence
                        pi-coding-agent-tu-bench--scheduler-errors)))
                     :final (plist-get metrics :final)
                     :artifacts
                     (list :slicesTsv
                           pi-coding-agent-tu-bench-cooling-slices-file
                           :commandsTsv
                           pi-coding-agent-tu-bench-commands-file))
                  :json-null)
                :checks (pi-coding-agent-tu-bench--checks-json
                         (plist-get metrics :checks)))))
    (with-temp-file pi-coding-agent-tu-bench-result-file
      (insert (json-encode object) "\n"))))

(defun pi-coding-agent-tu-bench--write-report (metrics run-ok)
  "Write a Markdown report for METRICS; RUN-OK is the overall verdict."
  (let ((dirty (not (string-empty-p
                     (pi-coding-agent-tu-bench--git-string
                      "status" "--porcelain" "--untracked-files=no"))))
        (probe (pi-coding-agent-tu-bench--probe-stats))
        (cooling (pi-coding-agent-tu-bench--cooling-scenario-p)))
    (with-temp-file pi-coding-agent-tu-bench-report-file
      (insert (if cooling
                  "# Deferred agent_end cooling benchmark\n\n"
                "# Tool-update storm benchmark\n\n"))
      (insert "Synthetic deterministic workload only; no private session content is read or stored.\n\n")
      (insert (format "- Verdict: `%s`\n"
                      (pi-coding-agent-tu-bench--verdict-label run-ok)))
      (insert (format "- Session/drain settled: `%s`\n"
                      (if (eq (plist-get metrics :settled) t) "yes" "no")))
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
      (if cooling
          (insert (format "- Cooling: `%d` completed overlays cross at final agent_end; `%d` hot-tail turn; command interval `%.0f` ms\n\n"
                          (pi-coding-agent-tu-bench--fill-tool-count)
                          pi-coding-agent-tu-bench-hot-tail-turns
                          (* 1000.0
                             pi-coding-agent-tu-bench-command-interval)))
        (insert (format "- Storm: `%d` updates across `%d` parallel subagent tools, gap scale `%.2f`, seed `%d`\n\n"
                        pi-coding-agent-tu-bench-updates
                        pi-coding-agent-tu-bench-parallel-tools
                        pi-coding-agent-tu-bench-gap-scale
                        pi-coding-agent-tu-bench-seed)))
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
      (insert "Timing guidance is diagnostic only: `<100 ms` target, `>250 ms` concern, `>1 s` severe.  No timing threshold fails the run.\n\n")
      (when cooling
        (insert "## Measurement-fidelity caveat\n\n")
        (insert "This runner uses `-Q`.  Slice and tree-root timings are structural diagnostics, and zero root calls is valid.  Do not cite this benchmark as evidence that md-ts root cost was reduced; it proves deferred scheduling and final correctness only.\n\n")
        (let* ((agent-end pi-coding-agent-tu-bench--agent-end-observation)
               (filter pi-coding-agent-tu-bench--agent-end-filter)
               (drain (plist-get metrics :drain))
               (final (plist-get metrics :final))
               (commands (pi-coding-agent-tu-bench--command-stats))
               (slices (pi-coding-agent-tu-bench--cooling-slices-in-order))
               (slice-max (if slices
                              (apply #'max
                                     (mapcar (lambda (row)
                                               (plist-get row :wallMs))
                                             slices))
                            0.0))
               (tree (pi-coding-agent-tu-bench--tree-root-summary)))
          (insert "## Deferred cooling metrics\n\n")
          (insert (format "- agent_end event: `%.3f` ms; enclosing filter: `%s` ms\n"
                          (or (plist-get agent-end :wallMs) 0.0)
                          (if filter
                              (format "%.3f" (plist-get filter :wallMs))
                            "n/a")))
          (insert (format "- Return state: queue `%s`, timer `%s`, cold `%s`; boundary `%s` → `%s`\n"
                          (plist-get agent-end :queueAfter)
                          (plist-get agent-end :timerAfter)
                          (plist-get agent-end :coldAfter)
                          (plist-get agent-end :boundaryBefore)
                          (plist-get agent-end :boundaryAfter)))
          (insert (format "- Natural drain: `%.3f` ms wall, `%.3f` ms callback-active, `%d` callbacks, max slice `%.3f` ms\n"
                          (or (plist-get drain :wallMs) 0.0)
                          (or (plist-get drain :activeMs) 0.0)
                          (length slices) slice-max))
          (insert (format "- Drain GC: `%s` collections, `%.3f` ms; final cold/hot: `%s`/`%s`\n"
                          (plist-get drain :gcs)
                          (or (plist-get drain :gcMs) 0.0)
                          (plist-get final :coldTools)
                          (plist-get final :toolOverlays)))
          (insert (format "- Commands: `%d` (`%d` sentinel / `%d` follow); lateness p95/max `%.3f`/`%.3f` ms; duration p95/max `%.3f`/`%.3f` ms\n"
                          (plist-get commands :count)
                          (plist-get commands :sentinelRoutes)
                          (plist-get commands :followRoutes)
                          (plist-get commands :latenessP95Ms)
                          (plist-get commands :latenessMaxMs)
                          (plist-get commands :durationP95Ms)
                          (plist-get commands :durationMaxMs)))
          (insert (format "- Tree roots: agent_end `%s` calls / `%.3f` ms; slices `%s` calls / `%.3f` ms\n"
                          (plist-get (plist-get tree :agentEnd) :count)
                          (plist-get (plist-get tree :agentEnd) :totalMs)
                          (plist-get (plist-get tree :coolingSlices) :count)
                          (plist-get (plist-get tree :coolingSlices) :totalMs)))
          (insert (format "- Semantic SHA-256: `%s`\n\n"
                          (plist-get final :semanticHash)))))
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
      (insert "Environment-independent metric: how often the frontend re-renders tool block bodies.  ")
      (unless cooling
        (insert "The WP2 coalescing renderer should drop `replace-body` calls towards (storm seconds / 0.25) x parallel tools."))
      (insert "\n\n")
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
      (unless cooling
        (insert "\nPer storm tool call ID:\n\n")
        (insert "| tool call id | replace-body calls | display-tool-end calls |\n")
        (insert "|---|---:|---:|\n")
        (dolist (tool-call-id (pi-coding-agent-tu-bench--storm-tool-ids))
          (insert (format "| `%s` | %d | %d |\n"
                          tool-call-id
                          (pi-coding-agent-tu-bench--render-count
                           "replace-body" tool-call-id)
                          (pi-coding-agent-tu-bench--render-count
                           "display-tool-end" tool-call-id)))))
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
      (when cooling
        (insert (format "- Cooling slices TSV: `%s`\n"
                        pi-coding-agent-tu-bench-cooling-slices-file))
        (insert (format "- Command route TSV: `%s`\n"
                        pi-coding-agent-tu-bench-commands-file)))
      (insert (format "- Fake RPC log without content: `%s`\n"
                      pi-coding-agent-tu-bench-fake-log)))))

(defun pi-coding-agent-tu-bench--metrics-ok-p (metrics)
  "Return non-nil when METRICS settled with every structural check green."
  (and (eq (plist-get metrics :settled) t)
       (seq-every-p (lambda (check) (eq (plist-get check :ok) t))
                    (plist-get metrics :checks))))

(defun pi-coding-agent-tu-bench--metrics-with-verdict (metrics)
  "Return METRICS with :ok set from its complete structural verdict."
  (plist-put metrics :ok
             (pi-coding-agent-tu-bench--json-bool
              (pi-coding-agent-tu-bench--metrics-ok-p metrics))))

(defun pi-coding-agent-tu-bench--verdict-label (run-ok)
  "Return the report verdict label for RUN-OK."
  (if run-ok "pass" "FAIL"))

(defun pi-coding-agent-tu-bench--exit-status (run-ok)
  "Return the benchmark process exit status for RUN-OK."
  (if run-ok 0 1))

(defun pi-coding-agent-tu-bench--validate-verdict-contract ()
  "Smoke-check failed assertions across JSON, report, and exit verdicts.
This intentionally uses an in-memory failed check rather than a fault-injected
benchmark scenario.  It prevents settled-only JSON success from regressing."
  (let* ((metrics
          (pi-coding-agent-tu-bench--metrics-with-verdict
           (list :settled t
                 :checks
                 (list (list :name "synthetic-failed-cooling-check"
                             :ok :json-false)))))
         (run-ok (eq (plist-get metrics :ok) t))
         (encoded (json-encode
                   (list :ok (plist-get metrics :ok)
                         :settled (plist-get metrics :settled)))))
    (unless (and (eq (plist-get metrics :ok) :json-false)
                 (string-match-p "\\\"ok\\\":false" encoded)
                 (equal (pi-coding-agent-tu-bench--verdict-label run-ok)
                        "FAIL")
                 (= (pi-coding-agent-tu-bench--exit-status run-ok) 1))
      (error "Benchmark verdict contract self-check failed: %s" encoded))))

;;;###autoload
(defun pi-coding-agent-tu-bench-run ()
  "Run one tool-update benchmark iteration and write artifacts.
Return non-nil when the scenario settled and all correctness checks passed.
Timing thresholds are diagnostic only and are never enforced."
  (pi-coding-agent-tu-bench--validate-verdict-contract)
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
        pi-coding-agent-tu-bench--render-log (make-hash-table :test 'equal)
        pi-coding-agent-tu-bench--current-filter-id nil
        pi-coding-agent-tu-bench--filter-sequence 0
        pi-coding-agent-tu-bench--filter-log nil
        pi-coding-agent-tu-bench--agent-end-observation nil
        pi-coding-agent-tu-bench--agent-end-filter nil
        pi-coding-agent-tu-bench--drain-start-time nil
        pi-coding-agent-tu-bench--cooling-slice-log nil
        pi-coding-agent-tu-bench--cooling-slice-sequence 0
        pi-coding-agent-tu-bench--scheduler-errors nil
        pi-coding-agent-tu-bench--cooling-chat-buffer nil
        pi-coding-agent-tu-bench--command-timer nil
        pi-coding-agent-tu-bench--command-pending nil
        pi-coding-agent-tu-bench--command-log nil
        pi-coding-agent-tu-bench--command-sequence 0
        pi-coding-agent-tu-bench--command-current-job nil
        pi-coding-agent-tu-bench--command-drain-active nil
        pi-coding-agent-tu-bench--command-last-route "follow"
        pi-coding-agent-tu-bench--window-sentinel-marker nil)
  (pi-coding-agent-tu-bench--install-advice)
  (unwind-protect
      (let* ((metrics
              (pi-coding-agent-tu-bench--metrics-with-verdict
               (pi-coding-agent-tu-bench--run-session)))
             (run-ok (eq (plist-get metrics :ok) t)))
        (pi-coding-agent-tu-bench--write-times-tsv)
        (pi-coding-agent-tu-bench--write-cooling-tsv)
        (pi-coding-agent-tu-bench--write-result-json metrics)
        (pi-coding-agent-tu-bench--write-report metrics run-ok)
        (princ (format "Wrote %s\n" pi-coding-agent-tu-bench-result-file))
        (princ (format "Wrote %s\n" pi-coding-agent-tu-bench-times-file))
        (when (pi-coding-agent-tu-bench--cooling-scenario-p)
          (princ (format "Wrote %s\n"
                         pi-coding-agent-tu-bench-cooling-slices-file))
          (princ (format "Wrote %s\n"
                         pi-coding-agent-tu-bench-commands-file)))
        (princ (format "Wrote %s\n" pi-coding-agent-tu-bench-report-file))
        run-ok)
    (pi-coding-agent-tu-bench--remove-advice)))

(defun pi-coding-agent-tu-bench-run-batch ()
  "Run one tool-update benchmark iteration in batch mode and exit."
  (let ((standard-output #'external-debugging-output))
    (kill-emacs
     (pi-coding-agent-tu-bench--exit-status
      (pi-coding-agent-tu-bench-run)))))

(provide 'pi-coding-agent-tool-update-bench)
;;; pi-coding-agent-tool-update-bench.el ends here
