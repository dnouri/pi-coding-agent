;;; piem-reload-resume-bench.el --- Reload/resume benchmarks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Deterministic reload/resume benchmarks for piem.
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

(defconst piem-rr-bench-repo-root
  (file-name-as-directory
   (expand-file-name ".."
                     (file-name-directory
                      (or load-file-name buffer-file-name default-directory))))
  "Repository root containing the reload/resume benchmark files.")

(add-to-list 'load-path piem-rr-bench-repo-root)
(require 'piem)

(defun piem-rr-bench--env (name default)
  "Return environment variable NAME, or DEFAULT when it is unset or empty."
  (let ((value (getenv name)))
    (if (and value (not (string-empty-p value))) value default)))

(defun piem-rr-bench--env-int (name default)
  "Return environment variable NAME as an integer, or DEFAULT."
  (string-to-number (piem-rr-bench--env
                     name (number-to-string default))))

(defun piem-rr-bench--truthy-env-p (name default)
  "Return non-nil when environment variable NAME is truthy.
DEFAULT is used when NAME is unset."
  (let ((value (downcase (piem-rr-bench--env name default))))
    (and (member value '("1" "true" "yes" "on")) t)))

(defun piem-rr-bench--json-bool (value)
  "Return VALUE encoded as a JSON boolean sentinel."
  (if value t :json-false))

(defvar piem-rr-bench-scenario
  (piem-rr-bench--env "PI_RR_BENCH_SCENARIO" "standalone")
  "Scenario label written into reload/resume benchmark artifacts.")

(defvar piem-rr-bench-variant
  (piem-rr-bench--env "PI_RR_BENCH_VARIANT" "unknown")
  "Variant label written into reload/resume benchmark artifacts.")

(defvar piem-rr-bench-iteration
  (piem-rr-bench--env-int "PI_RR_BENCH_ITERATION" 1)
  "Iteration number written into reload/resume benchmark artifacts.")

(defvar piem-rr-bench-turns
  (piem-rr-bench--env-int "PI_RR_BENCH_TURNS" 350)
  "Number of synthetic turns in the current and target sessions.")

(defvar piem-rr-bench-other-sessions
  (piem-rr-bench--env-int "PI_RR_BENCH_OTHER_SESSIONS" 60)
  "Number of extra session files in the synthetic resume directory.")

(defvar piem-rr-bench-other-turns
  (piem-rr-bench--env-int "PI_RR_BENCH_OTHER_TURNS" 40)
  "Number of turns in each extra synthetic session file.")

(defvar piem-rr-bench-tool-every
  (piem-rr-bench--env-int "PI_RR_BENCH_TOOL_EVERY" 1)
  "Cadence for synthetic tool calls; zero disables tool calls.")

(defvar piem-rr-bench-table-every
  (piem-rr-bench--env-int "PI_RR_BENCH_TABLE_EVERY" 10)
  "Cadence for synthetic Markdown tables; zero disables tables.")

(defvar piem-rr-bench-thinking-every
  (piem-rr-bench--env-int "PI_RR_BENCH_THINKING_EVERY" 5)
  "Cadence for synthetic thinking blocks; zero disables thinking blocks.")

(defvar piem-rr-bench-text-bytes
  (piem-rr-bench--env-int "PI_RR_BENCH_TEXT_BYTES" 0)
  "Approximate extra bytes to append to each synthetic text block.")

(defvar piem-rr-bench-wire-bytes
  (piem-rr-bench--env-int "PI_RR_BENCH_WIRE_BYTES" 0)
  "Ignored bytes to add to each synthetic message object.
This grows the JSON-over-stdio `get_messages' response without growing the
rendered transcript, which keeps the RPC framing benchmark focused.")

(defvar piem-rr-bench-tool-output-lines
  (piem-rr-bench--env-int "PI_RR_BENCH_TOOL_OUTPUT_LINES" 36)
  "Number of synthetic output lines per tool result.")

(defvar piem-rr-bench-timeout-seconds
  (piem-rr-bench--env-int "PI_RR_BENCH_TIMEOUT_SECONDS" 300)
  "Timeout in seconds for each asynchronous benchmark operation.")

(defvar piem-rr-bench-display-buffers
  (piem-rr-bench--truthy-env-p "PI_RR_BENCH_DISPLAY" "0")
  "Whether GUI benchmark iterations should display chat and input windows.")

(defvar piem-rr-bench-timings-enabled
  (piem-rr-bench--truthy-env-p "PI_RR_BENCH_TIMINGS" "0")
  "Whether to collect diagnostic inclusive timing advice data.")

(defvar piem-rr-bench-out-dir
  (file-name-as-directory
   (expand-file-name (piem-rr-bench--env
                      "PI_RR_BENCH_OUT_DIR"
                      "tmp/reload-resume-bench/standalone")
                     piem-rr-bench-repo-root))
  "Output directory for one reload/resume benchmark iteration.")

(defvar piem-rr-bench-runner-out-dir
  (file-name-as-directory
   (expand-file-name (piem-rr-bench--env
                      "PI_RR_BENCH_RUNNER_OUT_DIR"
                      piem-rr-bench-out-dir)
                     piem-rr-bench-repo-root))
  "Top-level runner output directory for reproduction commands.")

(defvar piem-rr-bench-fixture-root
  (file-name-as-directory
   (expand-file-name (piem-rr-bench--env
                      "PI_RR_BENCH_FIXTURE_ROOT"
                      (expand-file-name "fixtures"
                                        piem-rr-bench-out-dir))
                     piem-rr-bench-repo-root))
  "Root directory for generated synthetic fixture files.")

(defvar piem-rr-bench-data-dir
  (expand-file-name "sessions" piem-rr-bench-fixture-root)
  "Directory containing generated synthetic session JSONL files.")

(defvar piem-rr-bench-project-dir
  (file-name-as-directory
   (expand-file-name "project" piem-rr-bench-fixture-root))
  "Synthetic project directory recorded in generated session files.")

(defvar piem-rr-bench-fake-pi
  (expand-file-name "bench/fake-pi-reload-resume.py"
                    piem-rr-bench-repo-root)
  "Fake pi RPC executable used by reload/resume benchmark iterations.")

(defvar piem-rr-bench-fake-log
  (expand-file-name "fake-pi.jsonl" piem-rr-bench-out-dir)
  "Content-free fake RPC log path for one benchmark iteration.")

(defvar piem-rr-bench-result-file
  (expand-file-name "result.json" piem-rr-bench-out-dir)
  "JSON result artifact path for one benchmark iteration.")

(defvar piem-rr-bench-report-file
  (expand-file-name "report.md" piem-rr-bench-out-dir)
  "Markdown report artifact path for one benchmark iteration.")

(defvar piem-rr-bench-times-file
  (expand-file-name "times.tsv" piem-rr-bench-out-dir)
  "Diagnostic inclusive timing artifact path for one benchmark iteration.")

(defvar piem-rr-bench--phase nil
  "Current operation phase used as a prefix for timing rows.")

(defvar piem-rr-bench--timings (make-hash-table :test 'equal)
  "Hash table of diagnostic inclusive timing rows keyed by phase and name.")

(defvar piem-rr-bench--advice-handles nil
  "List of installed timing advice functions for cleanup.")

(defun piem-rr-bench--json-line (object)
  "Encode OBJECT as one JSONL line."
  (concat (json-encode object) "\n"))

(defun piem-rr-bench--timestamp (turn &optional offset)
  "Return a deterministic millisecond timestamp for TURN plus OFFSET."
  (+ 1704067200000 (* 60000 turn) (or offset 0)))

(defun piem-rr-bench--payload (turn label)
  "Return deterministic synthetic payload text for TURN and LABEL."
  (if (<= piem-rr-bench-text-bytes 0)
      ""
    (let* ((alphabet "abcdefghijklmnopqrstuvwxyz0123456789")
           (ch (aref alphabet (% (+ turn (length label)) (length alphabet))))
           (prefix (format "\nSynthetic payload %s turn %d: " label turn))
           (payload-len (max 0 (- piem-rr-bench-text-bytes
                                  (length prefix)))))
      (concat prefix (make-string payload-len ch)))))

(defun piem-rr-bench--wire-payload (turn label)
  "Return ignored synthetic wire payload for TURN and LABEL, or nil."
  (when (> piem-rr-bench-wire-bytes 0)
    (let* ((alphabet "abcdefghijklmnopqrstuvwxyz0123456789")
           (ch (aref alphabet (% (+ turn (* 3 (length label)))
                                  (length alphabet))))
           (prefix (format "ignored wire payload %s turn %d: " label turn))
           (payload-len (max 0 (- piem-rr-bench-wire-bytes
                                  (length prefix)))))
      (concat prefix (make-string payload-len ch)))))

(defun piem-rr-bench--message-with-wire-payload
    (message turn label)
  "Return MESSAGE plus ignored wire payload for TURN and LABEL when enabled."
  (if-let* ((payload (piem-rr-bench--wire-payload turn label)))
      (append message (list :benchmarkPayload payload))
    message))

(defun piem-rr-bench--table-text (turn)
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

(defun piem-rr-bench--assistant-text (turn short)
  "Return deterministic assistant text for TURN.
When SHORT is non-nil, omit tables and large optional payloads."
  (concat
   (format "Assistant answer for synthetic turn %d. This deterministic paragraph gives history replay real insertion work.\n\n" turn)
   (unless short
     (when (and (> piem-rr-bench-table-every 0)
                (zerop (% turn piem-rr-bench-table-every)))
       (concat (piem-rr-bench--table-text turn) "\n\n")))
   "```elisp\n"
   (format "(message \"synthetic turn %d\")\n" turn)
   "```\n"
   (piem-rr-bench--payload turn "assistant")))

(defun piem-rr-bench--thinking-text (turn)
  "Return deterministic thinking text for TURN."
  (concat (format "Synthetic thinking for turn %d. Keep render path deterministic."
                  turn)
          (piem-rr-bench--payload turn "thinking")))

(defun piem-rr-bench--tool-output (turn)
  "Return deterministic synthetic tool output for TURN."
  (mapconcat
   (lambda (i)
     (format "line %03d from tool on turn %03d: %s" i turn
             (make-string 72 (aref "abcdefghijklmnopqrstuvwxyz"
                                    (% (+ i turn) 26)))))
   (number-sequence 1 piem-rr-bench-tool-output-lines)
   "\n"))

(defun piem-rr-bench--tool-kind (turn)
  "Return the synthetic tool name for TURN."
  (pcase (% turn 4)
    (0 "read")
    (1 "bash")
    (2 "edit")
    (_ "profile_tool")))

(defun piem-rr-bench--tool-args (turn tool-name)
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

(defun piem-rr-bench--tool-details (turn tool-name)
  "Return synthetic tool result details for TOOL-NAME on TURN."
  (pcase tool-name
    ("edit" (list :diff (format "- old-%d\n+ new-%d\n" turn turn)
                  :truncation nil
                  :fullOutputPath nil))
    (_ (list :truncation nil :fullOutputPath nil))))

(defun piem-rr-bench--message-record (entry-id message)
  "Return a JSONL session record for ENTRY-ID containing MESSAGE."
  (list :type "message" :entryId entry-id :message message))

(defun piem-rr-bench--write-session (path name turns mtime-index
                                                     &optional short)
  "Write synthetic session PATH.
NAME labels the session; TURNS controls its length; MTIME-INDEX controls its
synthetic modification time.  When SHORT is non-nil, omit expensive assistant
extras.  Return a metrics plist for the generated session."
  (make-directory (file-name-directory path) t)
  (let ((message-count 0)
        (tool-count 0))
    (with-temp-file path
      (insert (piem-rr-bench--json-line
               (list :type "session"
                     :id (file-name-base path)
                     :cwd piem-rr-bench-project-dir)))
      (insert (piem-rr-bench--json-line
               (list :type "session_info"
                     :id (concat (file-name-base path) "-name")
                     :name name)))
      (cl-loop for turn from 1 to turns do
               (let ((user-text
                      (concat
                       (format "Session %s asks for synthetic reload/resume detail on turn %d."
                               name turn)
                       (piem-rr-bench--payload turn "user"))))
                 (insert
                  (piem-rr-bench--json-line
                   (piem-rr-bench--message-record
                    (format "user-%d" turn)
                    (piem-rr-bench--message-with-wire-payload
                     (list :role "user"
                           :content (vector (list :type "text" :text user-text))
                           :timestamp (piem-rr-bench--timestamp turn))
                     turn "user"))))
                 (setq message-count (1+ message-count)))
               (let* ((tool-p (and (not short)
                                   (> piem-rr-bench-tool-every 0)
                                   (zerop (% turn piem-rr-bench-tool-every))))
                      (tool-name (and tool-p
                                      (piem-rr-bench--tool-kind turn)))
                      (tool-id (and tool-p (format "tool-%d" turn)))
                      (assistant-content
                       (vconcat
                        (delq nil
                              (list
                               (list :type "text"
                                     :text (piem-rr-bench--assistant-text
                                            turn short))
                               (when (and (not short)
                                          (> piem-rr-bench-thinking-every 0)
                                          (zerop (% turn piem-rr-bench-thinking-every)))
                                 (list :type "thinking"
                                       :thinking (piem-rr-bench--thinking-text turn)))
                               (when tool-p
                                 (list :type "toolCall"
                                       :id tool-id
                                       :name tool-name
                                       :arguments (piem-rr-bench--tool-args
                                                   turn tool-name)))
                               (list :type "text"
                                     :text (format "\nTail sentinel for turn %d.\n"
                                                   turn)))))))
                 (insert
                  (piem-rr-bench--json-line
                   (piem-rr-bench--message-record
                    (format "assistant-%d" turn)
                    (piem-rr-bench--message-with-wire-payload
                     (list :role "assistant"
                           :content assistant-content
                           :timestamp (piem-rr-bench--timestamp turn 1000)
                           :stopReason "stop")
                     turn "assistant"))))
                 (setq message-count (1+ message-count))
                 (when tool-p
                   (insert
                    (piem-rr-bench--json-line
                     (piem-rr-bench--message-record
                      (format "tool-result-%d" turn)
                      (piem-rr-bench--message-with-wire-payload
                       (list :role "toolResult"
                             :toolCallId tool-id
                             :content (vector (list :type "text"
                                                    :text (piem-rr-bench--tool-output turn)))
                             :details (piem-rr-bench--tool-details
                                       turn tool-name)
                             :isError :json-false
                             :timestamp (piem-rr-bench--timestamp
                                         turn 2000))
                       turn "toolResult"))))
                   (setq message-count (1+ message-count)
                         tool-count (1+ tool-count))))))
    (set-file-times path (seconds-to-time (+ 1704067200 mtime-index)))
    (let ((bytes (file-attribute-size (file-attributes path))))
      (list :path path :name name :turns turns :messages message-count
            :tools tool-count :bytes bytes))))

(defun piem-rr-bench--prepare-data ()
  "Create deterministic fixtures and return a workload summary plist."
  (when (file-directory-p piem-rr-bench-fixture-root)
    (delete-directory piem-rr-bench-fixture-root t))
  (make-directory piem-rr-bench-data-dir t)
  (make-directory piem-rr-bench-project-dir t)
  (let* ((current (expand-file-name "current-long.jsonl"
                                    piem-rr-bench-data-dir))
         (target (expand-file-name "target-long.jsonl"
                                   piem-rr-bench-data-dir))
         (current-summary (piem-rr-bench--write-session
                           current "Current long session"
                           piem-rr-bench-turns 1000))
         (target-summary (piem-rr-bench--write-session
                          target "Target long session"
                          piem-rr-bench-turns 1001))
         (other-summaries nil))
    (cl-loop for i from 1 to piem-rr-bench-other-sessions do
             (push (piem-rr-bench--write-session
                    (expand-file-name (format "other-%03d.jsonl" i)
                                      piem-rr-bench-data-dir)
                    (format "Other profiling session %03d" i)
                    piem-rr-bench-other-turns i t)
                   other-summaries))
    (let* ((all (append (list current-summary target-summary)
                        (nreverse other-summaries)))
           (total-bytes (apply #'+ (mapcar (lambda (row)
                                             (plist-get row :bytes))
                                           all))))
      (list :current current-summary
            :target target-summary
            :other-count piem-rr-bench-other-sessions
            :session-file-count (length all)
            :total-bytes total-bytes
            :fixture-root piem-rr-bench-fixture-root
            :session-dir piem-rr-bench-data-dir
            :project-dir piem-rr-bench-project-dir))))

(defun piem-rr-bench--timing-key (name)
  "Return the hash key for timing NAME in the current phase."
  (format "%s\t%s" (or piem-rr-bench--phase "global") name))

(defun piem-rr-bench--add-time (name seconds)
  "Add SECONDS to diagnostic timing row NAME."
  (let* ((key (piem-rr-bench--timing-key name))
         (row (gethash key piem-rr-bench--timings)))
    (if row
        (setcdr row (list (1+ (cadr row))
                          (+ seconds (cl-caddr row))
                          (max seconds (cl-cadddr row))))
      (puthash key (list name 1 seconds seconds)
               piem-rr-bench--timings))))

(defun piem-rr-bench--timing-advice (name)
  "Return around advice that records inclusive time under NAME."
  (lambda (orig &rest args)
    (let ((start (float-time)))
      (unwind-protect
          (apply orig args)
        (piem-rr-bench--add-time name (- (float-time) start))))))

(defun piem-rr-bench--install-timing-advices ()
  "Install diagnostic inclusive timing advice for reload/resume paths."
  (when piem-rr-bench-timings-enabled
    (let ((symbols '(;; RPC and JSON framing.
                     piem--rpc-async
                     piem--process-filter
                     piem--accumulate-lines
                     piem--accumulate-line-chunks
                     piem--dispatch-response
                     piem--parse-json-line
                     json-parse-string
                     ;; Session browser and disk metadata.
                     piem-session-browser
                     piem--get-or-create-session-browser
                     piem--session-browser-fetch-and-render
                     piem--browse-load-sessions
                     piem--browse-scan-session-files
                     piem--browse-session-directories
                     piem--browse-session-files
                     piem-jsonl-read-session-info
                     piem--session-browser-render
                     piem--session-browser-rerender
                     piem--session-browser-insert-session
                     piem-session-browser-switch
                     piem--browse-switch-session
                     piem--session-list-directory
                     piem--session-file-cwd-or-error
                     piem--update-session-name-from-file
                     directory-files
                     insert-file-contents
                     file-attributes
                     ;; Transition control flow.
                     piem-reload
                     piem--resume-selected-session
                     piem--refresh-session-state
                     piem--load-session-history
                     ;; History rendering.
                     piem--display-session-history
                     piem--clear-render-artifacts
                     piem--display-history-messages
                     piem--build-tool-result-index
                     piem--display-user-message
                     piem--render-history-assistant-content
                     piem--render-history-text
                     piem--render-history-thinking
                     piem--render-history-tool
                     piem--append-to-chat
                     piem--update-hot-tail-boundary
                     piem--cool-completed-tool-blocks-outside-hot-tail
                     piem--cool-completed-tool-blocks
                     piem--cool-tool-overlay
                     piem--postprocess-history-buffer
                     piem--history-table-candidate-p
                     piem--decorate-tables-in-region
                     piem--treesit-table-regions
                     piem--decorate-table
                     piem--table-display-groups
                     ;; Tool rendering / overlay pressure.
                     piem--display-tool-start
                     piem--display-tool-end
                     piem--tool-block-create
                     piem--tool-overlay-finalize
                     piem--tool-block-finalize
                     piem--truncate-to-visual-lines
                     piem--insert-tool-content-with-toggle
                     piem--insert-rendered-tool-content
                     piem--pretty-print-json
                     make-overlay
                     overlays-in
                     remove-overlays
                     delete-overlay
                     font-lock-ensure
                     redisplay)))
      (dolist (sym symbols)
        (when (and (fboundp sym)
                   (not (assq sym piem-rr-bench--advice-handles)))
          (let ((fn (piem-rr-bench--timing-advice sym)))
            (advice-add sym :around fn)
            (push (cons sym fn)
                  piem-rr-bench--advice-handles)))))))

(defun piem-rr-bench--remove-timing-advices ()
  "Remove all diagnostic timing advice installed by the benchmark."
  (dolist (entry piem-rr-bench--advice-handles)
    (ignore-errors (advice-remove (car entry) (cdr entry))))
  (setq piem-rr-bench--advice-handles nil))

(defun piem-rr-bench--timing-rows (&optional phase)
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
     piem-rr-bench--timings)
    (sort rows (lambda (a b) (> (plist-get a :total)
                                (plist-get b :total))))))

(defun piem-rr-bench--write-times-tsv ()
  "Write diagnostic timing rows to `piem-rr-bench-times-file'."
  (with-temp-file piem-rr-bench-times-file
    (insert "phase\tname\tcount\ttotal_seconds\tmax_seconds\n")
    (dolist (row (piem-rr-bench--timing-rows))
      (insert (format "%s\t%s\t%d\t%.6f\t%.6f\n"
                      (plist-get row :phase)
                      (plist-get row :name)
                      (plist-get row :count)
                      (plist-get row :total)
                      (plist-get row :max))))))

(defun piem-rr-bench--read-session-messages (path)
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

(defun piem-rr-bench--preload-history (chat session-file)
  "Pre-render SESSION-FILE into CHAT before the timed operation."
  (when (buffer-live-p chat)
    (with-current-buffer chat
      (piem--display-session-history
       (piem-rr-bench--read-session-messages session-file)
       chat))))

(defun piem-rr-bench--buffer-contains-p (buffer text)
  "Return non-nil if TEXT is present in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (search-forward text nil t)))))

(defun piem-rr-bench--make-session (session-file &optional
                                                            backend-session-file
                                                            preload-session-file)
  "Create a fake-backed Emacs session whose cached file is SESSION-FILE.
BACKEND-SESSION-FILE, when non-nil, is the fake backend's initial session.
PRELOAD-SESSION-FILE, when non-nil, is rendered before timing starts."
  (setq piem-executable (list (or (executable-find "python3")
                                             (error "Python3 not found"))
                                         piem-rr-bench-fake-pi))
  (setq piem-extra-args
        (list "--initial-session" (or backend-session-file session-file)
              "--log-file" piem-rr-bench-fake-log))
  (let* ((chat (generate-new-buffer " *piem-rr-bench-chat*"))
         (input (generate-new-buffer " *piem-rr-bench-input*"))
         proc)
    (with-current-buffer chat
      (piem-chat-mode)
      (piem--set-chat-session-identity
       piem-rr-bench-project-dir)
      (piem--set-input-buffer input)
      (setq default-directory piem-rr-bench-project-dir)
      (setq piem--state
            (list :model (list :name "Fake Model" :provider "fake")
                  :thinking-level "medium"
                  :status 'idle
                  :session-id (file-name-base session-file)
                  :session-file session-file
                  :message-count 0
                  :pending-message-count 0))
      (setq piem--status 'idle)
      (setq proc (piem--start-process
                  piem-rr-bench-project-dir))
      ;; Version probes are unrelated to reload/resume and would spawn an
      ;; extra fake process in the GUI lane.  Delay them beyond the benchmark.
      (let ((piem--version-probe-delay 3600))
        (piem--set-process proc))
      (set-process-buffer proc chat)
      (process-put proc 'piem-chat-buffer chat)
      (piem--register-display-handler proc))
    (with-current-buffer input
      (piem-input-mode)
      (setq default-directory piem-rr-bench-project-dir)
      (piem--set-chat-buffer chat))
    (when preload-session-file
      (piem-rr-bench--preload-history chat preload-session-file))
    (when (and piem-rr-bench-display-buffers (not noninteractive))
      (delete-other-windows)
      (switch-to-buffer chat)
      (let ((input-window (split-window-vertically -8)))
        (set-window-buffer input-window input)
        (select-window (get-buffer-window chat)))
      (redisplay t))
    (list :chat chat :input input :proc proc)))

(defun piem-rr-bench--cleanup-session (session)
  "Kill buffers and processes belonging to benchmark SESSION."
  (dolist (buf (list (plist-get session :chat) (plist-get session :input)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (boundp 'piem--process)
                   (processp piem--process)
                   (process-live-p piem--process))
          (set-process-query-on-exit-flag piem--process nil)
          (delete-process piem--process)))
      (kill-buffer buf))))

(defun piem-rr-bench--pending-requests-count (proc)
  "Return the number of pending RPC requests for PROC."
  (let ((pending (and (processp proc)
                      (process-get proc 'piem-pending-requests))))
    (if (hash-table-p pending) (hash-table-count pending) 0)))

(defun piem-rr-bench--canonical-message-count (chat)
  "Return CHAT's canonical message count, or nil if unavailable."
  (when (buffer-live-p chat)
    (with-current-buffer chat
      (when (and (boundp 'piem--canonical-messages)
                 (vectorp piem--canonical-messages))
        (length piem--canonical-messages)))))

(defun piem-rr-bench--state-session-file (chat)
  "Return CHAT's current state session file, or nil if unavailable."
  (when (buffer-live-p chat)
    (with-current-buffer chat
      (when (boundp 'piem--state)
        (plist-get piem--state :session-file)))))

(defun piem-rr-bench--wait-until (predicate timeout)
  "Wait for PREDICATE to become non-nil, or TIMEOUT seconds to elapse."
  (let ((start (float-time))
        result)
    (while (and (not (setq result (funcall predicate)))
                (< (- (float-time) start) timeout))
      (accept-process-output nil 0.01)
      (when (and piem-rr-bench-display-buffers (not noninteractive))
        (redisplay t)))
    result))

(defun piem-rr-bench--run-operation (name session thunk done-p)
  "Run operation NAME for SESSION by calling THUNK.
DONE-P must return non-nil once asynchronous UI state has settled.  Return a
result plist containing correctness and wall-clock metrics."
  (setq piem-rr-bench--phase name)
  (garbage-collect)
  (let* ((chat (plist-get session :chat))
         (gc-before gcs-done)
         (gc-time-before gc-elapsed)
         (start (float-time))
         (ok nil)
         (error-text nil))
    (condition-case err
        (progn
          ;; `piem-reload' installs a fresh process and, in GUI
          ;; Emacs, normally schedules an unrelated `pi --version' probe.
          ;; Keep that probe out of the timed reload/resume window.
          (let ((piem--version-probe-delay 3600))
            (funcall thunk))
          (setq ok (piem-rr-bench--wait-until
                    (lambda ()
                      (let ((proc (and (buffer-live-p chat)
                                       (with-current-buffer chat
                                         piem--process))))
                        (unless (and (processp proc) (process-live-p proc))
                          (error "Fake pi process exited before %s settled" name))
                        (and (funcall done-p)
                             (= 0 (piem-rr-bench--pending-requests-count
                                   proc)))))
                    piem-rr-bench-timeout-seconds))
          (unless ok
            (setq error-text (format "Timed out waiting for %s to settle" name))))
      (error (setq error-text (error-message-string err))))
    (when (and piem-rr-bench-display-buffers (not noninteractive))
      (redisplay t))
    (prog1
        (list :name name
              :ok (piem-rr-bench--json-bool ok)
              :error error-text
              :seconds (- (float-time) start)
              :gcs (- gcs-done gc-before)
              :gcSeconds (- gc-elapsed gc-time-before)
              :bufferBytes (and (buffer-live-p chat)
                                (with-current-buffer chat (buffer-size)))
              :bufferLines (and (buffer-live-p chat)
                                (with-current-buffer chat
                                  (count-lines (point-min) (point-max)))))
      (setq piem-rr-bench--phase nil))))

(defun piem-rr-bench--open-session-browser (chat)
  "Open the real async session browser linked to CHAT and return its buffer."
  (let (dir)
    (with-current-buffer chat
      (setq dir (piem--session-directory))
      (piem-session-browser))
    (let ((browser
           (get-buffer (piem--session-browser-buffer-name dir))))
      (unless (buffer-live-p browser)
        (error "Session browser did not create a buffer for %s" dir))
      (unless (eq (buffer-local-value 'piem--chat-buffer browser)
                  chat)
        (error "Session browser is not linked to the benchmark chat"))
      browser)))

(defun piem-rr-bench--wait-for-session-browser (browser)
  "Wait boundedly for BROWSER to finish loading, or signal."
  (unless (piem-rr-bench--wait-until
           (lambda ()
             (and (buffer-live-p browser)
                  (with-current-buffer browser
                    (not piem--session-browser-loading))))
           piem-rr-bench-timeout-seconds)
    (error "Timed out waiting for the session browser to load"))
  (with-current-buffer browser
    (when piem--session-browser-error
      (error "Session browser failed: %s"
             piem--session-browser-error))))

(defun piem-rr-bench--select-session-path (browser path)
  "Move point in BROWSER to the session section whose value equals PATH."
  (with-current-buffer browser
    (let ((pending (oref magit-root-section children))
          (section nil))
      (while (and pending (not section))
        (let ((candidate (pop pending)))
          (if (and (eq (oref candidate type) 'session)
                   (equal (oref candidate value) path))
              (setq section candidate)
            (setq pending (append (oref candidate children) pending)))))
      (unless section
        (error "Session browser did not render target path: %s" path))
      (goto-char (oref section start))
      (dolist (window (get-buffer-window-list browser nil t))
        (set-window-point window (point))))))

(defun piem-rr-bench--run-resume (current-session target-session
                                                            target-count)
  "Benchmark browser-backed resume from CURRENT-SESSION to TARGET-SESSION.
TARGET-COUNT is the expected canonical message count after resume."
  (let* ((session (piem-rr-bench--make-session
                   current-session nil current-session))
         (chat (plist-get session :chat))
         (browser nil))
    (unwind-protect
        (piem-rr-bench--run-operation
         "resume"
         session
         (lambda ()
           (setq browser
                 (piem-rr-bench--open-session-browser chat))
           (piem-rr-bench--wait-for-session-browser browser)
           (piem-rr-bench--select-session-path
            browser target-session)
           (with-current-buffer browser
             (piem-session-browser-switch)))
         (lambda ()
           (and (not (with-current-buffer chat
                       (piem--session-transition-active-p)))
                (= (or (piem-rr-bench--canonical-message-count chat)
                       -1)
                   target-count)
                (equal (piem-rr-bench--state-session-file chat)
                       target-session)
                (piem-rr-bench--buffer-contains-p
                 chat "Session Target long session asks")
                (not (piem-rr-bench--buffer-contains-p
                      chat "Session Current long session asks")))))
      (when (buffer-live-p browser)
        (kill-buffer browser))
      (piem-rr-bench--cleanup-session session))))

(defun piem-rr-bench--run-reload (current-session target-session
                                                           target-count)
  "Benchmark reload from CURRENT-SESSION to TARGET-SESSION.
TARGET-COUNT is the expected canonical message count after reload."
  (let* ((session (piem-rr-bench--make-session
                   target-session current-session target-session))
         (chat (plist-get session :chat)))
    (unwind-protect
        (piem-rr-bench--run-operation
         "reload"
         session
         (lambda () (with-current-buffer chat (piem-reload)))
         (lambda ()
           (and (not (with-current-buffer chat
                       (piem--session-transition-active-p)))
                (= (or (piem-rr-bench--canonical-message-count chat)
                       -1)
                   target-count)
                (equal (piem-rr-bench--state-session-file chat)
                       target-session)
                (piem-rr-bench--buffer-contains-p
                 chat "Session Target long session asks")
                (not (piem-rr-bench--buffer-contains-p
                      chat "Session Current long session asks")))))
      (piem-rr-bench--cleanup-session session))))

(defun piem-rr-bench--fake-rpc-summary ()
  "Return a content-free summary of fake RPC traffic for this iteration."
  (let ((get-messages nil)
        (commands nil))
    (when (file-readable-p piem-rr-bench-fake-log)
      (with-temp-buffer
        (insert-file-contents piem-rr-bench-fake-log)
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

(defun piem-rr-bench--top-timings-json (phase n)
  "Return the top N diagnostic timing rows for PHASE as a JSON vector."
  (vconcat
   (mapcar
    (lambda (row)
      (list :phase (plist-get row :phase)
            :name (plist-get row :name)
            :count (plist-get row :count)
            :totalSeconds (plist-get row :total)
            :maxSeconds (plist-get row :max)))
    (seq-take (piem-rr-bench--timing-rows phase) n))))

(defun piem-rr-bench--git-string (&rest args)
  "Run git with ARGS in the repository root and return trimmed output."
  (string-trim
   (with-temp-buffer
     (let ((default-directory piem-rr-bench-repo-root))
       (if (zerop (apply #'process-file "git" nil t nil args))
           (buffer-string)
         "")))))

(defun piem-rr-bench--workload-json (data)
  "Return workload DATA as a JSON-encodable plist."
  (let* ((current (plist-get data :current))
         (target (plist-get data :target)))
    (list :turns piem-rr-bench-turns
          :otherSessions piem-rr-bench-other-sessions
          :otherTurns piem-rr-bench-other-turns
          :toolEvery piem-rr-bench-tool-every
          :tableEvery piem-rr-bench-table-every
          :thinkingEvery piem-rr-bench-thinking-every
          :textBytes piem-rr-bench-text-bytes
          :wireBytes piem-rr-bench-wire-bytes
          :toolOutputLines piem-rr-bench-tool-output-lines
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

(defun piem-rr-bench--write-result-json (results data)
  "Write RESULTS and workload DATA to `piem-rr-bench-result-file'."
  (let* ((dirty (not (string-empty-p
                      (piem-rr-bench--git-string
                       "status" "--porcelain" "--untracked-files=no"))))
         (object (list :scenario piem-rr-bench-scenario
                       :variant piem-rr-bench-variant
                       :iteration piem-rr-bench-iteration
                       :commit (piem-rr-bench--git-string
                                "rev-parse" "--short" "HEAD")
                       :dirty (piem-rr-bench--json-bool dirty)
                       :display (piem-rr-bench--json-bool
                                 piem-rr-bench-display-buffers)
                       :timingsEnabled (piem-rr-bench--json-bool
                                        piem-rr-bench-timings-enabled)
                       :emacsVersion emacs-version
                       :workload (piem-rr-bench--workload-json data)
                       :results (vconcat results)
                       :rpc (piem-rr-bench--fake-rpc-summary)
                       :topTimings
                       (list :resume (piem-rr-bench--top-timings-json
                                      "resume" 20)
                             :reload (piem-rr-bench--top-timings-json
                                      "reload" 20)))))
    (with-temp-file piem-rr-bench-result-file
      (insert (json-encode object) "\n"))))

(defun piem-rr-bench--operation-summary-table (results)
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

(defun piem-rr-bench--top-lines (phase &optional n)
  "Return Markdown rows for the top N timing rows in PHASE."
  (let ((rows (seq-take (piem-rr-bench--timing-rows phase)
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

(defun piem-rr-bench--write-report (results data)
  "Write a Markdown report for RESULTS and workload DATA."
  (let ((dirty (not (string-empty-p
                     (piem-rr-bench--git-string
                      "status" "--porcelain" "--untracked-files=no")))))
    (with-temp-file piem-rr-bench-report-file
      (insert "# Deterministic reload/resume benchmark\n\n")
      (insert "Synthetic fixture only; no private session content is read or stored.\n\n")
      (insert (format "- Scenario: `%s`\n" piem-rr-bench-scenario))
      (insert (format "- Variant: `%s`\n" piem-rr-bench-variant))
      (insert (format "- Iteration: `%d`\n" piem-rr-bench-iteration))
      (insert (format "- Commit: `%s`%s\n"
                      (piem-rr-bench--git-string
                       "rev-parse" "--short" "HEAD")
                      (if dirty " (dirty)" "")))
      (insert (format "- Emacs: `%s`\n" emacs-version))
      (insert (format "- Visible GUI buffers: `%s`\n"
                      (if piem-rr-bench-display-buffers
                          "yes" "no")))
      (insert (format "- Diagnostic timing advice: `%s`\n"
                      (if piem-rr-bench-timings-enabled
                          "enabled" "disabled")))
      (insert "- Existing transcript pre-rendered before timed operation: `yes`\n\n")
      (insert "## Reproduction command shape\n\n")
      (insert "```sh\n")
      (insert (format "./bench/run-reload-resume-bench.sh %s --scenario %s -c 1 --out-dir %s\n"
                      (if piem-rr-bench-display-buffers
                          "" "--batch")
                      piem-rr-bench-scenario
                      piem-rr-bench-runner-out-dir))
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
                        piem-rr-bench-other-sessions
                        piem-rr-bench-other-turns))
        (insert (format "- Tool/table/thinking cadence: `%d`/`%d`/`%d`; text bytes per text block: `%d`; ignored wire bytes per message: `%d`\n\n"
                        piem-rr-bench-tool-every
                        piem-rr-bench-table-every
                        piem-rr-bench-thinking-every
                        piem-rr-bench-text-bytes
                        piem-rr-bench-wire-bytes)))
      (insert "## Wall-clock results\n\n")
      (insert (piem-rr-bench--operation-summary-table results))
      (insert "\n\n")
      (insert "## Fake RPC payload evidence\n\n")
      (insert (format "- `get_messages` response byte sizes: `%S`\n\n"
                      (append (plist-get (piem-rr-bench--fake-rpc-summary)
                                         :getMessagesBytes)
                              nil)))
      (dolist (phase '("resume" "reload"))
        (insert (format "## Top inclusive timings: %s\n\n" phase))
        (insert "| function/feature | calls | total seconds | max call seconds |\n")
        (insert "|---|---:|---:|---:|\n")
        (insert (piem-rr-bench--top-lines phase 20))
        (insert "\n\n"))
      (insert "## Raw artifacts\n\n")
      (insert (format "- Result JSON: `%s`\n" piem-rr-bench-result-file))
      (insert (format "- Timing TSV: `%s`\n" piem-rr-bench-times-file))
      (insert (format "- Fake RPC log without content: `%s`\n"
                      piem-rr-bench-fake-log)))))

(defun piem-rr-bench--results-ok-p (results)
  "Return non-nil when every operation in RESULTS completed correctly."
  (and results
       (seq-every-p (lambda (result) (eq (plist-get result :ok) t))
                    results)))

(defun piem-rr-bench-run ()
  "Run one reload/resume benchmark iteration and write artifacts.
Return non-nil when all correctness checks passed.  Timing thresholds are not
enforced."
  (make-directory piem-rr-bench-out-dir t)
  (ignore-errors (delete-file piem-rr-bench-fake-log))
  (clrhash piem-rr-bench--timings)
  (piem-rr-bench--install-timing-advices)
  (unwind-protect
      (let* ((data (piem-rr-bench--prepare-data))
             (current-session (plist-get (plist-get data :current) :path))
             (target-session (plist-get (plist-get data :target) :path))
             (target-count (plist-get (plist-get data :target) :messages))
             (results nil))
        (push (piem-rr-bench--run-resume
               current-session target-session target-count)
              results)
        (push (piem-rr-bench--run-reload
               current-session target-session target-count)
              results)
        (setq results (nreverse results))
        (piem-rr-bench--write-times-tsv)
        (piem-rr-bench--write-result-json results data)
        (piem-rr-bench--write-report results data)
        (princ (format "Wrote %s\n" piem-rr-bench-result-file))
        (princ (format "Wrote %s\n" piem-rr-bench-times-file))
        (princ (format "Wrote %s\n" piem-rr-bench-report-file))
        (piem-rr-bench--results-ok-p results))
    (piem-rr-bench--remove-timing-advices)))

(defun piem-rr-bench-run-batch ()
  "Run one reload/resume benchmark iteration in batch mode and exit."
  (let ((standard-output #'external-debugging-output))
    (kill-emacs (if (piem-rr-bench-run) 0 1))))

(provide 'piem-reload-resume-bench)
;;; piem-reload-resume-bench.el ends here
