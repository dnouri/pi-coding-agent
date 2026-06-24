;;; pi-coding-agent-reload-resume-bench.el --- Reload/resume benchmarks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Deterministic reload/resume benchmarks for pi-coding-agent.
;; The benchmark generates synthetic pi JSONL session files, drives the Emacs
;; UI through a fake JSON-over-stdio backend, and records only metrics.  No
;; private session files are read.
;;
;; Run with:
;;
;;   make bench-reload-resume            # GUI via xvfb, primary lane
;;   make bench-reload-resume-batch      # --batch, secondary lane
;;   make bench-reload-resume-smoke      # cheap correctness smoke
;;
;; or directly through `bench/run-reload-resume-bench.sh'.
;;
;; The primary lane is GUI/xvfb because reload/resume includes redisplay,
;; tree-sitter, overlays, and window-visible rendering.  Batch numbers are
;; useful for CI trend artifacts but less representative of interactive use.
;; Timing advice is diagnostic only; pass --timings when inclusive function
;; timings are more useful than purer wall-clock numbers.  No timing threshold
;; is enforced by this benchmark.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(defconst pi-coding-agent-rr-bench-repo-root
  (file-name-as-directory
   (expand-file-name ".."
                     (file-name-directory
                      (or load-file-name buffer-file-name default-directory))))
  "Repository root containing the reload/resume benchmark files.")

(add-to-list 'load-path pi-coding-agent-rr-bench-repo-root)
(require 'pi-coding-agent)

(defun pi-coding-agent-rr-bench--env (name default)
  "Return environment variable NAME, or DEFAULT when it is unset or empty."
  (let ((value (getenv name)))
    (if (and value (not (string-empty-p value))) value default)))

(defun pi-coding-agent-rr-bench--env-int (name default)
  "Return environment variable NAME as an integer, or DEFAULT."
  (string-to-number (pi-coding-agent-rr-bench--env
                     name (number-to-string default))))

(defun pi-coding-agent-rr-bench--truthy-env-p (name default)
  "Return non-nil when environment variable NAME is truthy.
DEFAULT is used when NAME is unset."
  (let ((value (downcase (pi-coding-agent-rr-bench--env name default))))
    (and (member value '("1" "true" "yes" "on")) t)))

(defun pi-coding-agent-rr-bench--json-bool (value)
  "Return VALUE encoded as a JSON boolean sentinel."
  (if value t :json-false))

(defvar pi-coding-agent-rr-bench-scenario
  (pi-coding-agent-rr-bench--env "PI_RR_BENCH_SCENARIO" "standalone")
  "Scenario label written into reload/resume benchmark artifacts.")

(defvar pi-coding-agent-rr-bench-variant
  (pi-coding-agent-rr-bench--env "PI_RR_BENCH_VARIANT" "unknown")
  "Variant label written into reload/resume benchmark artifacts.")

(defvar pi-coding-agent-rr-bench-iteration
  (pi-coding-agent-rr-bench--env-int "PI_RR_BENCH_ITERATION" 1)
  "Iteration number written into reload/resume benchmark artifacts.")

(defvar pi-coding-agent-rr-bench-turns
  (pi-coding-agent-rr-bench--env-int "PI_RR_BENCH_TURNS" 350)
  "Number of synthetic turns in the current and target sessions.")

(defvar pi-coding-agent-rr-bench-other-sessions
  (pi-coding-agent-rr-bench--env-int "PI_RR_BENCH_OTHER_SESSIONS" 60)
  "Number of extra session files in the synthetic resume directory.")

(defvar pi-coding-agent-rr-bench-other-turns
  (pi-coding-agent-rr-bench--env-int "PI_RR_BENCH_OTHER_TURNS" 40)
  "Number of turns in each extra synthetic session file.")

(defvar pi-coding-agent-rr-bench-tool-every
  (pi-coding-agent-rr-bench--env-int "PI_RR_BENCH_TOOL_EVERY" 1)
  "Cadence for synthetic tool calls; zero disables tool calls.")

(defvar pi-coding-agent-rr-bench-table-every
  (pi-coding-agent-rr-bench--env-int "PI_RR_BENCH_TABLE_EVERY" 10)
  "Cadence for synthetic Markdown tables; zero disables tables.")

(defvar pi-coding-agent-rr-bench-thinking-every
  (pi-coding-agent-rr-bench--env-int "PI_RR_BENCH_THINKING_EVERY" 5)
  "Cadence for synthetic thinking blocks; zero disables thinking blocks.")

(defvar pi-coding-agent-rr-bench-text-bytes
  (pi-coding-agent-rr-bench--env-int "PI_RR_BENCH_TEXT_BYTES" 0)
  "Approximate extra bytes to append to each synthetic text block.")

(defvar pi-coding-agent-rr-bench-wire-bytes
  (pi-coding-agent-rr-bench--env-int "PI_RR_BENCH_WIRE_BYTES" 0)
  "Ignored bytes to add to each synthetic message object.
This grows the JSON-over-stdio `get_messages' response without growing the
rendered transcript, which keeps the RPC framing benchmark focused.")

(defvar pi-coding-agent-rr-bench-tool-output-lines
  (pi-coding-agent-rr-bench--env-int "PI_RR_BENCH_TOOL_OUTPUT_LINES" 36)
  "Number of synthetic output lines per tool result.")

(defvar pi-coding-agent-rr-bench-timeout-seconds
  (pi-coding-agent-rr-bench--env-int "PI_RR_BENCH_TIMEOUT_SECONDS" 300)
  "Timeout in seconds for each asynchronous benchmark operation.")

(defvar pi-coding-agent-rr-bench-display-buffers
  (pi-coding-agent-rr-bench--truthy-env-p "PI_RR_BENCH_DISPLAY" "0")
  "Whether GUI benchmark iterations should display chat and input windows.")

(defvar pi-coding-agent-rr-bench-timings-enabled
  (pi-coding-agent-rr-bench--truthy-env-p "PI_RR_BENCH_TIMINGS" "0")
  "Whether to collect diagnostic inclusive timing advice data.")

(defvar pi-coding-agent-rr-bench-out-dir
  (file-name-as-directory
   (expand-file-name (pi-coding-agent-rr-bench--env
                      "PI_RR_BENCH_OUT_DIR"
                      "tmp/reload-resume-bench/standalone")
                     pi-coding-agent-rr-bench-repo-root))
  "Output directory for one reload/resume benchmark iteration.")

(defvar pi-coding-agent-rr-bench-runner-out-dir
  (file-name-as-directory
   (expand-file-name (pi-coding-agent-rr-bench--env
                      "PI_RR_BENCH_RUNNER_OUT_DIR"
                      pi-coding-agent-rr-bench-out-dir)
                     pi-coding-agent-rr-bench-repo-root))
  "Top-level runner output directory for reproduction commands.")

(defvar pi-coding-agent-rr-bench-fixture-root
  (file-name-as-directory
   (expand-file-name (pi-coding-agent-rr-bench--env
                      "PI_RR_BENCH_FIXTURE_ROOT"
                      (expand-file-name "fixtures"
                                        pi-coding-agent-rr-bench-out-dir))
                     pi-coding-agent-rr-bench-repo-root))
  "Root directory for generated synthetic fixture files.")

(defvar pi-coding-agent-rr-bench-data-dir
  (expand-file-name "sessions" pi-coding-agent-rr-bench-fixture-root)
  "Directory containing generated synthetic session JSONL files.")

(defvar pi-coding-agent-rr-bench-project-dir
  (file-name-as-directory
   (expand-file-name "project" pi-coding-agent-rr-bench-fixture-root))
  "Synthetic project directory recorded in generated session files.")

(defvar pi-coding-agent-rr-bench-fake-pi
  (expand-file-name "bench/fake-pi-reload-resume.py"
                    pi-coding-agent-rr-bench-repo-root)
  "Fake pi RPC executable used by reload/resume benchmark iterations.")

(defvar pi-coding-agent-rr-bench-fake-log
  (expand-file-name "fake-pi.jsonl" pi-coding-agent-rr-bench-out-dir)
  "Content-free fake RPC log path for one benchmark iteration.")

(defvar pi-coding-agent-rr-bench-result-file
  (expand-file-name "result.json" pi-coding-agent-rr-bench-out-dir)
  "JSON result artifact path for one benchmark iteration.")

(defvar pi-coding-agent-rr-bench-report-file
  (expand-file-name "report.md" pi-coding-agent-rr-bench-out-dir)
  "Markdown report artifact path for one benchmark iteration.")

(defvar pi-coding-agent-rr-bench-times-file
  (expand-file-name "times.tsv" pi-coding-agent-rr-bench-out-dir)
  "Diagnostic inclusive timing artifact path for one benchmark iteration.")

(defvar pi-coding-agent-rr-bench--phase nil
  "Current operation phase used as a prefix for timing rows.")

(defvar pi-coding-agent-rr-bench--timings (make-hash-table :test 'equal)
  "Hash table of diagnostic inclusive timing rows keyed by phase and name.")

(defvar pi-coding-agent-rr-bench--advice-handles nil
  "List of installed timing advice functions for cleanup.")

(defun pi-coding-agent-rr-bench--json-line (object)
  "Encode OBJECT as one JSONL line."
  (concat (json-encode object) "\n"))

(defun pi-coding-agent-rr-bench--timestamp (turn &optional offset)
  "Return a deterministic millisecond timestamp for TURN plus OFFSET."
  (+ 1704067200000 (* 60000 turn) (or offset 0)))

(defun pi-coding-agent-rr-bench--payload (turn label)
  "Return deterministic synthetic payload text for TURN and LABEL."
  (if (<= pi-coding-agent-rr-bench-text-bytes 0)
      ""
    (let* ((alphabet "abcdefghijklmnopqrstuvwxyz0123456789")
           (ch (aref alphabet (% (+ turn (length label)) (length alphabet))))
           (prefix (format "\nSynthetic payload %s turn %d: " label turn))
           (payload-len (max 0 (- pi-coding-agent-rr-bench-text-bytes
                                  (length prefix)))))
      (concat prefix (make-string payload-len ch)))))

(defun pi-coding-agent-rr-bench--wire-payload (turn label)
  "Return ignored synthetic wire payload for TURN and LABEL, or nil."
  (when (> pi-coding-agent-rr-bench-wire-bytes 0)
    (let* ((alphabet "abcdefghijklmnopqrstuvwxyz0123456789")
           (ch (aref alphabet (% (+ turn (* 3 (length label)))
                                  (length alphabet))))
           (prefix (format "ignored wire payload %s turn %d: " label turn))
           (payload-len (max 0 (- pi-coding-agent-rr-bench-wire-bytes
                                  (length prefix)))))
      (concat prefix (make-string payload-len ch)))))

(defun pi-coding-agent-rr-bench--message-with-wire-payload
    (message turn label)
  "Return MESSAGE plus ignored wire payload for TURN and LABEL when enabled."
  (if-let* ((payload (pi-coding-agent-rr-bench--wire-payload turn label)))
      (append message (list :benchmarkPayload payload))
    message))

(defun pi-coding-agent-rr-bench--table-text (turn)
  "Return deterministic Markdown table text for TURN."
  (mapconcat
   #'identity
   (append
    (list (format "| turn %d | status | value | note |" turn)
          "|---:|---|---:|---|")
    (cl-loop for i from 1 to 8
             collect (format "| %d.%d | **ok** | %d | `cell-%d-%d` wraps with extra words |"
                             turn i (* turn i) turn i)))
   "\n"))

(defun pi-coding-agent-rr-bench--assistant-text (turn short)
  "Return deterministic assistant text for TURN.
When SHORT is non-nil, omit tables and large optional payloads."
  (concat
   (format "Assistant answer for synthetic turn %d. This deterministic paragraph gives history replay real insertion work.\n\n" turn)
   (unless short
     (when (and (> pi-coding-agent-rr-bench-table-every 0)
                (zerop (% turn pi-coding-agent-rr-bench-table-every)))
       (concat (pi-coding-agent-rr-bench--table-text turn) "\n\n")))
   "```elisp\n"
   (format "(message \"synthetic turn %d\")\n" turn)
   "```\n"
   (pi-coding-agent-rr-bench--payload turn "assistant")))

(defun pi-coding-agent-rr-bench--thinking-text (turn)
  "Return deterministic thinking text for TURN."
  (concat (format "Synthetic thinking for turn %d. Keep render path deterministic."
                  turn)
          (pi-coding-agent-rr-bench--payload turn "thinking")))

(defun pi-coding-agent-rr-bench--tool-output (turn)
  "Return deterministic synthetic tool output for TURN."
  (mapconcat
   (lambda (i)
     (format "line %03d from tool on turn %03d: %s" i turn
             (make-string 72 (aref "abcdefghijklmnopqrstuvwxyz"
                                    (% (+ i turn) 26)))))
   (number-sequence 1 pi-coding-agent-rr-bench-tool-output-lines)
   "\n"))

(defun pi-coding-agent-rr-bench--tool-kind (turn)
  "Return the synthetic tool name for TURN."
  (pcase (% turn 4)
    (0 "read")
    (1 "bash")
    (2 "edit")
    (_ "profile_tool")))

(defun pi-coding-agent-rr-bench--tool-args (turn tool-name)
  "Return synthetic arguments for TOOL-NAME on TURN."
  (pcase tool-name
    ("read" (list :path (format "src/file-%03d.el" (% turn 37))
                  :offset (* 10 (% turn 20))))
    ("bash" (list :command (format "printf 'turn %d' && sleep 0" turn)))
    ("edit" (list :path (format "src/file-%03d.el" (% turn 37))
                  :oldText (format "old-%d" turn)
                  :newText (format "new-%d" turn)))
    (_ (list :path (format "src/file-%03d.el" (% turn 37))
             :payload (vconcat (cl-loop for i below 8
                                         collect (list :key (format "k%d" i)
                                                       :value (format "v%d-%d" turn i))))))))

(defun pi-coding-agent-rr-bench--tool-details (turn tool-name)
  "Return synthetic tool result details for TOOL-NAME on TURN."
  (pcase tool-name
    ("edit" (list :diff (format "- old-%d\n+ new-%d\n" turn turn)
                  :truncation nil
                  :fullOutputPath nil))
    (_ (list :truncation nil :fullOutputPath nil))))

(defun pi-coding-agent-rr-bench--message-record (entry-id message)
  "Return a JSONL session record for ENTRY-ID containing MESSAGE."
  (list :type "message" :entryId entry-id :message message))

(defun pi-coding-agent-rr-bench--write-session (path name turns mtime-index
                                                     &optional short)
  "Write synthetic session PATH.
NAME labels the session; TURNS controls its length; MTIME-INDEX controls its
synthetic modification time.  When SHORT is non-nil, omit expensive assistant
extras.  Return a metrics plist for the generated session."
  (make-directory (file-name-directory path) t)
  (let ((message-count 0)
        (tool-count 0))
    (with-temp-file path
      (insert (pi-coding-agent-rr-bench--json-line
               (list :type "session"
                     :id (file-name-base path)
                     :cwd pi-coding-agent-rr-bench-project-dir)))
      (insert (pi-coding-agent-rr-bench--json-line
               (list :type "session_info"
                     :id (concat (file-name-base path) "-name")
                     :name name)))
      (cl-loop for turn from 1 to turns do
               (let ((user-text
                      (concat
                       (format "Session %s asks for synthetic reload/resume detail on turn %d."
                               name turn)
                       (pi-coding-agent-rr-bench--payload turn "user"))))
                 (insert
                  (pi-coding-agent-rr-bench--json-line
                   (pi-coding-agent-rr-bench--message-record
                    (format "user-%d" turn)
                    (pi-coding-agent-rr-bench--message-with-wire-payload
                     (list :role "user"
                           :content (vector (list :type "text" :text user-text))
                           :timestamp (pi-coding-agent-rr-bench--timestamp turn))
                     turn "user"))))
                 (setq message-count (1+ message-count)))
               (let* ((tool-p (and (not short)
                                   (> pi-coding-agent-rr-bench-tool-every 0)
                                   (zerop (% turn pi-coding-agent-rr-bench-tool-every))))
                      (tool-name (and tool-p
                                      (pi-coding-agent-rr-bench--tool-kind turn)))
                      (tool-id (and tool-p (format "tool-%d" turn)))
                      (assistant-content
                       (vconcat
                        (delq nil
                              (list
                               (list :type "text"
                                     :text (pi-coding-agent-rr-bench--assistant-text
                                            turn short))
                               (when (and (not short)
                                          (> pi-coding-agent-rr-bench-thinking-every 0)
                                          (zerop (% turn pi-coding-agent-rr-bench-thinking-every)))
                                 (list :type "thinking"
                                       :thinking (pi-coding-agent-rr-bench--thinking-text turn)))
                               (when tool-p
                                 (list :type "toolCall"
                                       :id tool-id
                                       :name tool-name
                                       :arguments (pi-coding-agent-rr-bench--tool-args
                                                   turn tool-name)))
                               (list :type "text"
                                     :text (format "\nTail sentinel for turn %d.\n"
                                                   turn)))))))
                 (insert
                  (pi-coding-agent-rr-bench--json-line
                   (pi-coding-agent-rr-bench--message-record
                    (format "assistant-%d" turn)
                    (pi-coding-agent-rr-bench--message-with-wire-payload
                     (list :role "assistant"
                           :content assistant-content
                           :timestamp (pi-coding-agent-rr-bench--timestamp turn 1000)
                           :stopReason "stop")
                     turn "assistant"))))
                 (setq message-count (1+ message-count))
                 (when tool-p
                   (insert
                    (pi-coding-agent-rr-bench--json-line
                     (pi-coding-agent-rr-bench--message-record
                      (format "tool-result-%d" turn)
                      (pi-coding-agent-rr-bench--message-with-wire-payload
                       (list :role "toolResult"
                             :toolCallId tool-id
                             :content (vector (list :type "text"
                                                    :text (pi-coding-agent-rr-bench--tool-output turn)))
                             :details (pi-coding-agent-rr-bench--tool-details
                                       turn tool-name)
                             :isError :json-false
                             :timestamp (pi-coding-agent-rr-bench--timestamp
                                         turn 2000))
                       turn "toolResult"))))
                   (setq message-count (1+ message-count)
                         tool-count (1+ tool-count))))))
    (set-file-times path (seconds-to-time (+ 1704067200 mtime-index)))
    (let ((bytes (file-attribute-size (file-attributes path))))
      (list :path path :name name :turns turns :messages message-count
            :tools tool-count :bytes bytes))))

(defun pi-coding-agent-rr-bench--prepare-data ()
  "Create deterministic fixtures and return a workload summary plist."
  (when (file-directory-p pi-coding-agent-rr-bench-fixture-root)
    (delete-directory pi-coding-agent-rr-bench-fixture-root t))
  (make-directory pi-coding-agent-rr-bench-data-dir t)
  (make-directory pi-coding-agent-rr-bench-project-dir t)
  (let* ((current (expand-file-name "current-long.jsonl"
                                    pi-coding-agent-rr-bench-data-dir))
         (target (expand-file-name "target-long.jsonl"
                                   pi-coding-agent-rr-bench-data-dir))
         (current-summary (pi-coding-agent-rr-bench--write-session
                           current "Current long session"
                           pi-coding-agent-rr-bench-turns 1000))
         (target-summary (pi-coding-agent-rr-bench--write-session
                          target "Target long session"
                          pi-coding-agent-rr-bench-turns 1001))
         (other-summaries nil))
    (cl-loop for i from 1 to pi-coding-agent-rr-bench-other-sessions do
             (push (pi-coding-agent-rr-bench--write-session
                    (expand-file-name (format "other-%03d.jsonl" i)
                                      pi-coding-agent-rr-bench-data-dir)
                    (format "Other profiling session %03d" i)
                    pi-coding-agent-rr-bench-other-turns i t)
                   other-summaries))
    (let* ((all (append (list current-summary target-summary)
                        (nreverse other-summaries)))
           (total-bytes (apply #'+ (mapcar (lambda (row)
                                             (plist-get row :bytes))
                                           all))))
      (list :current current-summary
            :target target-summary
            :other-count pi-coding-agent-rr-bench-other-sessions
            :session-file-count (length all)
            :total-bytes total-bytes
            :fixture-root pi-coding-agent-rr-bench-fixture-root
            :session-dir pi-coding-agent-rr-bench-data-dir
            :project-dir pi-coding-agent-rr-bench-project-dir))))

(defun pi-coding-agent-rr-bench--timing-key (name)
  "Return the hash key for timing NAME in the current phase."
  (format "%s\t%s" (or pi-coding-agent-rr-bench--phase "global") name))

(defun pi-coding-agent-rr-bench--add-time (name seconds)
  "Add SECONDS to diagnostic timing row NAME."
  (let* ((key (pi-coding-agent-rr-bench--timing-key name))
         (row (gethash key pi-coding-agent-rr-bench--timings)))
    (if row
        (setcdr row (list (1+ (cadr row))
                          (+ seconds (cl-caddr row))
                          (max seconds (cl-cadddr row))))
      (puthash key (list name 1 seconds seconds)
               pi-coding-agent-rr-bench--timings))))

(defun pi-coding-agent-rr-bench--timing-advice (name)
  "Return around advice that records inclusive time under NAME."
  (lambda (orig &rest args)
    (let ((start (float-time)))
      (unwind-protect
          (apply orig args)
        (pi-coding-agent-rr-bench--add-time name (- (float-time) start))))))

(defun pi-coding-agent-rr-bench--install-timing-advices ()
  "Install diagnostic inclusive timing advice for reload/resume paths."
  (when pi-coding-agent-rr-bench-timings-enabled
    (let ((symbols '(;; RPC and JSON framing.
                     pi-coding-agent--rpc-async
                     pi-coding-agent--process-filter
                     pi-coding-agent--accumulate-lines
                     pi-coding-agent--accumulate-line-chunks
                     pi-coding-agent--dispatch-response
                     pi-coding-agent--parse-json-line
                     json-parse-string
                     ;; Session picker and metadata.
                     pi-coding-agent--session-list-directory
                     pi-coding-agent--with-session-list-directory
                     pi-coding-agent--session-metadata
                     pi-coding-agent--session-file-cwd-or-error
                     pi-coding-agent--update-session-name-from-file
                     pi-coding-agent--list-session-entries
                     pi-coding-agent--list-sessions
                     pi-coding-agent--format-session-choice
                     pi-coding-agent--format-session-entry-choice
                     directory-files
                     insert-file-contents
                     file-attributes
                     ;; Transition control flow.
                     pi-coding-agent-reload
                     pi-coding-agent-resume-session
                     pi-coding-agent--resume-session-from-directory
                     pi-coding-agent--resume-selected-session
                     pi-coding-agent--refresh-session-state
                     pi-coding-agent--load-session-history
                     ;; History rendering.
                     pi-coding-agent--display-session-history
                     pi-coding-agent--clear-render-artifacts
                     pi-coding-agent--display-history-messages
                     pi-coding-agent--build-tool-result-index
                     pi-coding-agent--display-user-message
                     pi-coding-agent--render-history-assistant-content
                     pi-coding-agent--render-history-text
                     pi-coding-agent--render-history-thinking
                     pi-coding-agent--render-history-tool
                     pi-coding-agent--append-to-chat
                     pi-coding-agent--update-hot-tail-boundary
                     pi-coding-agent--cool-completed-tool-blocks-outside-hot-tail
                     pi-coding-agent--cool-completed-tool-blocks
                     pi-coding-agent--cool-tool-overlay
                     pi-coding-agent--postprocess-history-buffer
                     pi-coding-agent--history-table-candidate-p
                     pi-coding-agent--decorate-tables-in-region
                     pi-coding-agent--treesit-table-regions
                     pi-coding-agent--decorate-table
                     pi-coding-agent--table-display-groups
                     ;; Tool rendering / overlay pressure.
                     pi-coding-agent--display-tool-start
                     pi-coding-agent--display-tool-end
                     pi-coding-agent--tool-block-create
                     pi-coding-agent--tool-overlay-finalize
                     pi-coding-agent--tool-block-finalize
                     pi-coding-agent--truncate-to-visual-lines
                     pi-coding-agent--insert-tool-content-with-toggle
                     pi-coding-agent--insert-rendered-tool-content
                     pi-coding-agent--pretty-print-json
                     make-overlay
                     overlays-in
                     remove-overlays
                     delete-overlay
                     font-lock-ensure
                     redisplay)))
      (dolist (sym symbols)
        (when (and (fboundp sym)
                   (not (assq sym pi-coding-agent-rr-bench--advice-handles)))
          (let ((fn (pi-coding-agent-rr-bench--timing-advice sym)))
            (advice-add sym :around fn)
            (push (cons sym fn)
                  pi-coding-agent-rr-bench--advice-handles)))))))

(defun pi-coding-agent-rr-bench--remove-timing-advices ()
  "Remove all diagnostic timing advice installed by the benchmark."
  (dolist (entry pi-coding-agent-rr-bench--advice-handles)
    (ignore-errors (advice-remove (car entry) (cdr entry))))
  (setq pi-coding-agent-rr-bench--advice-handles nil))

(defun pi-coding-agent-rr-bench--timing-rows (&optional phase)
  "Return diagnostic timing rows, optionally filtered to PHASE.
Rows are sorted by descending inclusive total time."
  (let (rows)
    (maphash
     (lambda (key row)
       (let ((parts (split-string key "\t")))
         (when (or (null phase) (equal phase (car parts)))
           (push (list :phase (car parts)
                       :name (car row)
                       :count (cadr row)
                       :total (cl-caddr row)
                       :max (cl-cadddr row))
                 rows))))
     pi-coding-agent-rr-bench--timings)
    (sort rows (lambda (a b) (> (plist-get a :total)
                                (plist-get b :total))))))

(defun pi-coding-agent-rr-bench--write-times-tsv ()
  "Write diagnostic timing rows to `pi-coding-agent-rr-bench-times-file'."
  (with-temp-file pi-coding-agent-rr-bench-times-file
    (insert "phase\tname\tcount\ttotal_seconds\tmax_seconds\n")
    (dolist (row (pi-coding-agent-rr-bench--timing-rows))
      (insert (format "%s\t%s\t%d\t%.6f\t%.6f\n"
                      (plist-get row :phase)
                      (plist-get row :name)
                      (plist-get row :count)
                      (plist-get row :total)
                      (plist-get row :max))))))

(defun pi-coding-agent-rr-bench--read-session-messages (path)
  "Read message payloads from synthetic session file PATH."
  (let (messages)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((obj (json-parse-string
                    (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))
                    :object-type 'plist)))
          (when (equal (plist-get obj :type) "message")
            (push (plist-get obj :message) messages)))
        (forward-line 1)))
    (vconcat (nreverse messages))))

(defun pi-coding-agent-rr-bench--preload-history (chat session-file)
  "Pre-render SESSION-FILE into CHAT before the timed operation."
  (when (buffer-live-p chat)
    (with-current-buffer chat
      (pi-coding-agent--display-session-history
       (pi-coding-agent-rr-bench--read-session-messages session-file)
       chat))))

(defun pi-coding-agent-rr-bench--buffer-contains-p (buffer text)
  "Return non-nil if TEXT is present in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (search-forward text nil t)))))

(defun pi-coding-agent-rr-bench--make-session (session-file &optional
                                                            backend-session-file
                                                            preload-session-file)
  "Create a fake-backed Emacs session whose cached file is SESSION-FILE.
BACKEND-SESSION-FILE, when non-nil, is the fake backend's initial session.
PRELOAD-SESSION-FILE, when non-nil, is rendered before timing starts."
  (setq pi-coding-agent-executable (list (or (executable-find "python3")
                                             (error "Python3 not found"))
                                         pi-coding-agent-rr-bench-fake-pi))
  (setq pi-coding-agent-extra-args
        (list "--initial-session" (or backend-session-file session-file)
              "--log-file" pi-coding-agent-rr-bench-fake-log))
  (let* ((chat (generate-new-buffer " *pi-coding-agent-rr-bench-chat*"))
         (input (generate-new-buffer " *pi-coding-agent-rr-bench-input*"))
         proc)
    (with-current-buffer chat
      (pi-coding-agent-chat-mode)
      (pi-coding-agent--set-chat-session-identity
       pi-coding-agent-rr-bench-project-dir)
      (pi-coding-agent--set-input-buffer input)
      (setq default-directory pi-coding-agent-rr-bench-project-dir)
      (setq pi-coding-agent--state
            (list :model (list :name "Fake Model" :provider "fake")
                  :thinking-level "medium"
                  :status 'idle
                  :session-id (file-name-base session-file)
                  :session-file session-file
                  :message-count 0
                  :pending-message-count 0))
      (setq pi-coding-agent--status 'idle)
      (setq proc (pi-coding-agent--start-process
                  pi-coding-agent-rr-bench-project-dir))
      ;; Version probes are unrelated to reload/resume and would spawn an
      ;; extra fake process in the GUI lane.  Delay them beyond the benchmark.
      (let ((pi-coding-agent--version-probe-delay 3600))
        (pi-coding-agent--set-process proc))
      (set-process-buffer proc chat)
      (process-put proc 'pi-coding-agent-chat-buffer chat)
      (pi-coding-agent--register-display-handler proc))
    (with-current-buffer input
      (pi-coding-agent-input-mode)
      (setq default-directory pi-coding-agent-rr-bench-project-dir)
      (pi-coding-agent--set-chat-buffer chat))
    (when preload-session-file
      (pi-coding-agent-rr-bench--preload-history chat preload-session-file))
    (when (and pi-coding-agent-rr-bench-display-buffers (not noninteractive))
      (delete-other-windows)
      (switch-to-buffer chat)
      (let ((input-window (split-window-vertically -8)))
        (set-window-buffer input-window input)
        (select-window (get-buffer-window chat)))
      (redisplay t))
    (list :chat chat :input input :proc proc)))

(defun pi-coding-agent-rr-bench--cleanup-session (session)
  "Kill buffers and processes belonging to benchmark SESSION."
  (dolist (buf (list (plist-get session :chat) (plist-get session :input)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (boundp 'pi-coding-agent--process)
                   (processp pi-coding-agent--process)
                   (process-live-p pi-coding-agent--process))
          (set-process-query-on-exit-flag pi-coding-agent--process nil)
          (delete-process pi-coding-agent--process)))
      (kill-buffer buf))))

(defun pi-coding-agent-rr-bench--pending-requests-count (proc)
  "Return the number of pending RPC requests for PROC."
  (let ((pending (and (processp proc)
                      (process-get proc 'pi-coding-agent-pending-requests))))
    (if (hash-table-p pending) (hash-table-count pending) 0)))

(defun pi-coding-agent-rr-bench--canonical-message-count (chat)
  "Return CHAT's canonical message count, or nil if unavailable."
  (when (buffer-live-p chat)
    (with-current-buffer chat
      (when (and (boundp 'pi-coding-agent--canonical-messages)
                 (vectorp pi-coding-agent--canonical-messages))
        (length pi-coding-agent--canonical-messages)))))

(defun pi-coding-agent-rr-bench--state-session-file (chat)
  "Return CHAT's current state session file, or nil if unavailable."
  (when (buffer-live-p chat)
    (with-current-buffer chat
      (when (boundp 'pi-coding-agent--state)
        (plist-get pi-coding-agent--state :session-file)))))

(defun pi-coding-agent-rr-bench--wait-until (predicate timeout)
  "Wait for PREDICATE to become non-nil, or TIMEOUT seconds to elapse."
  (let ((start (float-time))
        result)
    (while (and (not (setq result (funcall predicate)))
                (< (- (float-time) start) timeout))
      (accept-process-output nil 0.01)
      (when (and pi-coding-agent-rr-bench-display-buffers (not noninteractive))
        (redisplay t)))
    result))

(defun pi-coding-agent-rr-bench--run-operation (name session thunk done-p)
  "Run operation NAME for SESSION by calling THUNK.
DONE-P must return non-nil once asynchronous UI state has settled.  Return a
result plist containing correctness and wall-clock metrics."
  (setq pi-coding-agent-rr-bench--phase name)
  (garbage-collect)
  (let* ((chat (plist-get session :chat))
         (gc-before gcs-done)
         (gc-time-before gc-elapsed)
         (start (float-time))
         (ok nil)
         (error-text nil))
    (condition-case err
        (progn
          ;; `pi-coding-agent-reload' installs a fresh process and, in GUI
          ;; Emacs, normally schedules an unrelated `pi --version' probe.
          ;; Keep that probe out of the timed reload/resume window.
          (let ((pi-coding-agent--version-probe-delay 3600))
            (funcall thunk))
          (setq ok (pi-coding-agent-rr-bench--wait-until
                    (lambda ()
                      (let ((proc (and (buffer-live-p chat)
                                       (with-current-buffer chat
                                         pi-coding-agent--process))))
                        (unless (and (processp proc) (process-live-p proc))
                          (error "Fake pi process exited before %s settled" name))
                        (and (funcall done-p)
                             (= 0 (pi-coding-agent-rr-bench--pending-requests-count
                                   proc)))))
                    pi-coding-agent-rr-bench-timeout-seconds)))
      (error (setq error-text (error-message-string err))))
    (when (and pi-coding-agent-rr-bench-display-buffers (not noninteractive))
      (redisplay t))
    (prog1
        (list :name name
              :ok (pi-coding-agent-rr-bench--json-bool ok)
              :error error-text
              :seconds (- (float-time) start)
              :gcs (- gcs-done gc-before)
              :gcSeconds (- gc-elapsed gc-time-before)
              :bufferBytes (and (buffer-live-p chat)
                                (with-current-buffer chat (buffer-size)))
              :bufferLines (and (buffer-live-p chat)
                                (with-current-buffer chat
                                  (count-lines (point-min) (point-max)))))
      (setq pi-coding-agent-rr-bench--phase nil))))

(defun pi-coding-agent-rr-bench--target-choice (collection)
  "Return the synthetic target session display string from COLLECTION."
  (let ((choices (all-completions "" collection)))
    (or (seq-find (lambda (choice)
                    (string-prefix-p "Target long session" choice))
                  choices)
        (car choices))))

(defun pi-coding-agent-rr-bench--run-resume (current-session target-session
                                                            target-count)
  "Benchmark resume from CURRENT-SESSION to TARGET-SESSION.
TARGET-COUNT is the expected canonical message count after resume."
  (let* ((session (pi-coding-agent-rr-bench--make-session
                   current-session nil current-session))
         (chat (plist-get session :chat)))
    (unwind-protect
        (pi-coding-agent-rr-bench--run-operation
         "resume"
         session
         (lambda ()
           (with-current-buffer chat
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt collection &rest _args)
                          (pi-coding-agent-rr-bench--target-choice collection))))
               (pi-coding-agent-resume-session))))
         (lambda ()
           (and (= (or (pi-coding-agent-rr-bench--canonical-message-count chat)
                       -1)
                   target-count)
                (equal (pi-coding-agent-rr-bench--state-session-file chat)
                       target-session)
                (pi-coding-agent-rr-bench--buffer-contains-p
                 chat "Session Target long session asks")
                (not (pi-coding-agent-rr-bench--buffer-contains-p
                      chat "Session Current long session asks")))))
      (pi-coding-agent-rr-bench--cleanup-session session))))

(defun pi-coding-agent-rr-bench--run-reload (current-session target-session
                                                           target-count)
  "Benchmark reload from CURRENT-SESSION to TARGET-SESSION.
TARGET-COUNT is the expected canonical message count after reload."
  (let* ((session (pi-coding-agent-rr-bench--make-session
                   target-session current-session target-session))
         (chat (plist-get session :chat)))
    (unwind-protect
        (pi-coding-agent-rr-bench--run-operation
         "reload"
         session
         (lambda () (with-current-buffer chat (pi-coding-agent-reload)))
         (lambda ()
           (and (= (or (pi-coding-agent-rr-bench--canonical-message-count chat)
                       -1)
                   target-count)
                (equal (pi-coding-agent-rr-bench--state-session-file chat)
                       target-session)
                (pi-coding-agent-rr-bench--buffer-contains-p
                 chat "Session Target long session asks")
                (not (pi-coding-agent-rr-bench--buffer-contains-p
                      chat "Session Current long session asks")))))
      (pi-coding-agent-rr-bench--cleanup-session session))))

(defun pi-coding-agent-rr-bench--fake-rpc-summary ()
  "Return a content-free summary of fake RPC traffic for this iteration."
  (let ((get-messages nil)
        (commands nil))
    (when (file-readable-p pi-coding-agent-rr-bench-fake-log)
      (with-temp-buffer
        (insert-file-contents pi-coding-agent-rr-bench-fake-log)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                                       (line-end-position)))
                 (obj (ignore-errors
                        (json-parse-string line :object-type 'plist))))
            (when (plist-get obj :direction)
              (let ((command (plist-get obj :command)))
                (when command (push command commands))
                (when (and (equal (plist-get obj :direction) "out")
                           (equal command "get_messages"))
                  (push (plist-get obj :bytes) get-messages)))))
          (forward-line 1))))
    (list :getMessagesBytes (vconcat (nreverse get-messages))
          :commands (vconcat (nreverse commands)))))

(defun pi-coding-agent-rr-bench--top-timings-json (phase n)
  "Return the top N diagnostic timing rows for PHASE as a JSON vector."
  (vconcat
   (mapcar
    (lambda (row)
      (list :phase (plist-get row :phase)
            :name (plist-get row :name)
            :count (plist-get row :count)
            :totalSeconds (plist-get row :total)
            :maxSeconds (plist-get row :max)))
    (seq-take (pi-coding-agent-rr-bench--timing-rows phase) n))))

(defun pi-coding-agent-rr-bench--git-string (&rest args)
  "Run git with ARGS in the repository root and return trimmed output."
  (string-trim
   (with-temp-buffer
     (let ((default-directory pi-coding-agent-rr-bench-repo-root))
       (if (zerop (apply #'process-file "git" nil t nil args))
           (buffer-string)
         "")))))

(defun pi-coding-agent-rr-bench--workload-json (data)
  "Return workload DATA as a JSON-encodable plist."
  (let* ((current (plist-get data :current))
         (target (plist-get data :target)))
    (list :turns pi-coding-agent-rr-bench-turns
          :otherSessions pi-coding-agent-rr-bench-other-sessions
          :otherTurns pi-coding-agent-rr-bench-other-turns
          :toolEvery pi-coding-agent-rr-bench-tool-every
          :tableEvery pi-coding-agent-rr-bench-table-every
          :thinkingEvery pi-coding-agent-rr-bench-thinking-every
          :textBytes pi-coding-agent-rr-bench-text-bytes
          :wireBytes pi-coding-agent-rr-bench-wire-bytes
          :toolOutputLines pi-coding-agent-rr-bench-tool-output-lines
          :sessionFileCount (plist-get data :session-file-count)
          :totalBytes (plist-get data :total-bytes)
          :fixtureRoot (plist-get data :fixture-root)
          :sessionDir (plist-get data :session-dir)
          :projectDir (plist-get data :project-dir)
          :current (list :path (plist-get current :path)
                         :bytes (plist-get current :bytes)
                         :messages (plist-get current :messages)
                         :tools (plist-get current :tools))
          :target (list :path (plist-get target :path)
                        :bytes (plist-get target :bytes)
                        :messages (plist-get target :messages)
                        :tools (plist-get target :tools)))))

(defun pi-coding-agent-rr-bench--write-result-json (results data)
  "Write RESULTS and workload DATA to `pi-coding-agent-rr-bench-result-file'."
  (let* ((dirty (not (string-empty-p
                      (pi-coding-agent-rr-bench--git-string
                       "status" "--porcelain" "--untracked-files=no"))))
         (object (list :scenario pi-coding-agent-rr-bench-scenario
                       :variant pi-coding-agent-rr-bench-variant
                       :iteration pi-coding-agent-rr-bench-iteration
                       :commit (pi-coding-agent-rr-bench--git-string
                                "rev-parse" "--short" "HEAD")
                       :dirty (pi-coding-agent-rr-bench--json-bool dirty)
                       :display (pi-coding-agent-rr-bench--json-bool
                                 pi-coding-agent-rr-bench-display-buffers)
                       :timingsEnabled (pi-coding-agent-rr-bench--json-bool
                                        pi-coding-agent-rr-bench-timings-enabled)
                       :emacsVersion emacs-version
                       :workload (pi-coding-agent-rr-bench--workload-json data)
                       :results (vconcat results)
                       :rpc (pi-coding-agent-rr-bench--fake-rpc-summary)
                       :topTimings
                       (list :resume (pi-coding-agent-rr-bench--top-timings-json
                                      "resume" 20)
                             :reload (pi-coding-agent-rr-bench--top-timings-json
                                      "reload" 20)))))
    (with-temp-file pi-coding-agent-rr-bench-result-file
      (insert (json-encode object) "\n"))))

(defun pi-coding-agent-rr-bench--operation-summary-table (results)
  "Return a Markdown table summarizing operation RESULTS."
  (concat
   "| operation | ok | wall seconds | GCs | GC seconds | buffer bytes | buffer lines | error |\n"
   "|---|---:|---:|---:|---:|---:|---:|---|\n"
   (mapconcat
    (lambda (result)
      (format "| %s | %s | %.3f | %d | %.3f | %s | %s | %s |"
              (plist-get result :name)
              (if (eq (plist-get result :ok) t) "yes" "no")
              (plist-get result :seconds)
              (plist-get result :gcs)
              (plist-get result :gcSeconds)
              (or (plist-get result :bufferBytes) "")
              (or (plist-get result :bufferLines) "")
              (or (plist-get result :error) "")))
    results "\n")))

(defun pi-coding-agent-rr-bench--top-lines (phase &optional n)
  "Return Markdown rows for the top N timing rows in PHASE."
  (let ((rows (seq-take (pi-coding-agent-rr-bench--timing-rows phase)
                        (or n 18))))
    (if rows
        (mapconcat
         (lambda (row)
           (format "| `%s` | %d | %.3f | %.3f |"
                   (plist-get row :name)
                   (plist-get row :count)
                   (plist-get row :total)
                   (plist-get row :max)))
         rows "\n")
      "| _(no timing data)_ | 0 | 0.000 | 0.000 |")))

(defun pi-coding-agent-rr-bench--write-report (results data)
  "Write a Markdown report for RESULTS and workload DATA."
  (let ((dirty (not (string-empty-p
                     (pi-coding-agent-rr-bench--git-string
                      "status" "--porcelain" "--untracked-files=no")))))
    (with-temp-file pi-coding-agent-rr-bench-report-file
      (insert "# Deterministic reload/resume benchmark\n\n")
      (insert "Synthetic fixture only; no private session content is read or stored.\n\n")
      (insert (format "- Scenario: `%s`\n" pi-coding-agent-rr-bench-scenario))
      (insert (format "- Variant: `%s`\n" pi-coding-agent-rr-bench-variant))
      (insert (format "- Iteration: `%d`\n" pi-coding-agent-rr-bench-iteration))
      (insert (format "- Commit: `%s`%s\n"
                      (pi-coding-agent-rr-bench--git-string
                       "rev-parse" "--short" "HEAD")
                      (if dirty " (dirty)" "")))
      (insert (format "- Emacs: `%s`\n" emacs-version))
      (insert (format "- Visible GUI buffers: `%s`\n"
                      (if pi-coding-agent-rr-bench-display-buffers
                          "yes" "no")))
      (insert (format "- Diagnostic timing advice: `%s`\n"
                      (if pi-coding-agent-rr-bench-timings-enabled
                          "enabled" "disabled")))
      (insert "- Existing transcript pre-rendered before timed operation: `yes`\n\n")
      (insert "## Reproduction command shape\n\n")
      (insert "```sh\n")
      (insert (format "./bench/run-reload-resume-bench.sh %s --scenario %s -c 1 --out-dir %s\n"
                      (if pi-coding-agent-rr-bench-display-buffers
                          "" "--batch")
                      pi-coding-agent-rr-bench-scenario
                      pi-coding-agent-rr-bench-runner-out-dir))
      (insert "```\n\n")
      (insert "## Workload\n\n")
      (let* ((current (plist-get data :current))
             (target (plist-get data :target)))
        (insert (format "- Fixture root: `%s`\n" (plist-get data :fixture-root)))
        (insert (format "- Session files: `%d`; total JSONL bytes: `%d`\n"
                        (plist-get data :session-file-count)
                        (plist-get data :total-bytes)))
        (insert (format "- Current: `%d` bytes, `%d` messages\n"
                        (plist-get current :bytes)
                        (plist-get current :messages)))
        (insert (format "- Target: `%d` bytes, `%d` messages\n"
                        (plist-get target :bytes)
                        (plist-get target :messages)))
        (insert (format "- Other sessions: `%d` x `%d` turns\n"
                        pi-coding-agent-rr-bench-other-sessions
                        pi-coding-agent-rr-bench-other-turns))
        (insert (format "- Tool/table/thinking cadence: `%d`/`%d`/`%d`; text bytes per text block: `%d`; ignored wire bytes per message: `%d`\n\n"
                        pi-coding-agent-rr-bench-tool-every
                        pi-coding-agent-rr-bench-table-every
                        pi-coding-agent-rr-bench-thinking-every
                        pi-coding-agent-rr-bench-text-bytes
                        pi-coding-agent-rr-bench-wire-bytes)))
      (insert "## Wall-clock results\n\n")
      (insert (pi-coding-agent-rr-bench--operation-summary-table results))
      (insert "\n\n")
      (insert "## Fake RPC payload evidence\n\n")
      (insert (format "- `get_messages` response byte sizes: `%S`\n\n"
                      (append (plist-get (pi-coding-agent-rr-bench--fake-rpc-summary)
                                         :getMessagesBytes)
                              nil)))
      (dolist (phase '("resume" "reload"))
        (insert (format "## Top inclusive timings: %s\n\n" phase))
        (insert "| function/feature | calls | total seconds | max call seconds |\n")
        (insert "|---|---:|---:|---:|\n")
        (insert (pi-coding-agent-rr-bench--top-lines phase 20))
        (insert "\n\n"))
      (insert "## Raw artifacts\n\n")
      (insert (format "- Result JSON: `%s`\n" pi-coding-agent-rr-bench-result-file))
      (insert (format "- Timing TSV: `%s`\n" pi-coding-agent-rr-bench-times-file))
      (insert (format "- Fake RPC log without content: `%s`\n"
                      pi-coding-agent-rr-bench-fake-log)))))

(defun pi-coding-agent-rr-bench--results-ok-p (results)
  "Return non-nil when every operation in RESULTS completed correctly."
  (and results
       (seq-every-p (lambda (result) (eq (plist-get result :ok) t))
                    results)))

(defun pi-coding-agent-rr-bench-run ()
  "Run one reload/resume benchmark iteration and write artifacts.
Return non-nil when all correctness checks passed.  Timing thresholds are not
enforced."
  (make-directory pi-coding-agent-rr-bench-out-dir t)
  (ignore-errors (delete-file pi-coding-agent-rr-bench-fake-log))
  (clrhash pi-coding-agent-rr-bench--timings)
  (pi-coding-agent-rr-bench--install-timing-advices)
  (unwind-protect
      (let* ((data (pi-coding-agent-rr-bench--prepare-data))
             (current-session (plist-get (plist-get data :current) :path))
             (target-session (plist-get (plist-get data :target) :path))
             (target-count (plist-get (plist-get data :target) :messages))
             (results nil))
        (push (pi-coding-agent-rr-bench--run-resume
               current-session target-session target-count)
              results)
        (push (pi-coding-agent-rr-bench--run-reload
               current-session target-session target-count)
              results)
        (setq results (nreverse results))
        (pi-coding-agent-rr-bench--write-times-tsv)
        (pi-coding-agent-rr-bench--write-result-json results data)
        (pi-coding-agent-rr-bench--write-report results data)
        (princ (format "Wrote %s\n" pi-coding-agent-rr-bench-result-file))
        (princ (format "Wrote %s\n" pi-coding-agent-rr-bench-times-file))
        (princ (format "Wrote %s\n" pi-coding-agent-rr-bench-report-file))
        (pi-coding-agent-rr-bench--results-ok-p results))
    (pi-coding-agent-rr-bench--remove-timing-advices)))

(defun pi-coding-agent-rr-bench-run-batch ()
  "Run one reload/resume benchmark iteration in batch mode and exit."
  (let ((standard-output #'external-debugging-output))
    (kill-emacs (if (pi-coding-agent-rr-bench-run) 0 1))))

(provide 'pi-coding-agent-reload-resume-bench)
;;; pi-coding-agent-reload-resume-bench.el ends here
