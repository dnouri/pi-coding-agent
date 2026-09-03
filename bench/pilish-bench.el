;;; pilish-bench.el --- Benchmarks for chat-buffer table rendering -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Performance benchmarks for display-only table rendering in pilish.
;; Exercises the full rendering pipeline: tree-sitter detection, visible-text
;; extraction, cell wrapping, overlay creation, streaming refresh, and
;; hot-tail resize.
;;
;; Run with:
;;
;;   make bench            # GUI via xvfb (matches real Emacs)
;;   make bench-batch      # --batch (no font engine, faster but less realistic)
;;
;; or directly:
;;
;;   xvfb-run -a emacs -Q -L . -l bench/pilish-bench.el \
;;         -f pilish-bench-run-batch
;;
;; The primary lane is GUI/xvfb because `string-width' and font-lock
;; consult font metrics in GUI mode.  Batch numbers serve only as a
;; directional secondary comparison.
;;
;; Each scenario prints: NAME  MIN  MEDIAN  MAX  GCs  GC-TIME
;; Stage-level decomposition follows the scenario suite.

;;; Code:

(require 'benchmark)
(require 'pilish-render)

;;;; Fixture Loading

(defvar pilish-bench--tables nil
  "List of table strings extracted from the fixture file.")

(defvar pilish-bench--fixture-dir
  (expand-file-name "fixtures/"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Directory containing benchmark fixture files.")

(defun pilish-bench--extract-tables (file)
  "Extract pipe tables from FILE, returning a list of table strings."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((lines (split-string (buffer-string) "\n"))
          tables current)
      (dolist (line lines)
        (if (string-match-p "^[> ]*|" (string-trim line))
            (push line current)
          (when current
            (push (mapconcat #'identity (nreverse current) "\n") tables)
            (setq current nil))))
      (when current
        (push (mapconcat #'identity (nreverse current) "\n") tables))
      (nreverse tables))))

(defun pilish-bench--load-fixtures ()
  "Load fixture tables from the bench fixtures directory."
  (unless pilish-bench--tables
    (let ((file (expand-file-name "tables.md"
                                  pilish-bench--fixture-dir)))
      (unless (file-exists-p file)
        (error "Fixture not found: %s" file))
      (setq pilish-bench--tables
            (pilish-bench--extract-tables file)))))

;;;; Chat Buffer Builders

(defun pilish-bench--make-chat-buffer (content-fn)
  "Create a chat buffer populated by CONTENT-FN.
CONTENT-FN is called with the buffer current and `inhibit-read-only'
set to t.  Returns the buffer."
  (let ((buf (generate-new-buffer " *bench-chat*")))
    (with-current-buffer buf
      (pilish-chat-mode)
      (let ((inhibit-read-only t))
        (funcall content-fn)))
    buf))

(defun pilish-bench--insert-turn (role text)
  "Insert a chat turn for ROLE containing TEXT."
  (let ((heading (if (string= role "user") "You" "Assistant"))
        (underline (if (string= role "user") "===" "---------")))
    (insert heading "\n" underline "\n\n" text "\n\n")))

(defun pilish-bench--build-mixed-buffer (tables &optional turns-per-table)
  "Build a mixed chat buffer with TABLES interleaved with prose turns.
TURNS-PER-TABLE controls how many assistant turns contain a table
before a prose-only user/assistant exchange.  Returns the buffer."
  (let ((tpt (or turns-per-table 1)))
    (pilish-bench--make-chat-buffer
     (lambda ()
       (pilish-bench--insert-turn
        "user" "Analyze these datasets and give me a summary.")
       (let ((i 0))
         (dolist (tbl tables)
           (pilish-bench--insert-turn
            "assistant"
            (concat "Here is the analysis:\n\n" tbl
                    "\n\nThe data above shows significant trends."))
           (setq i (1+ i))
           (when (zerop (mod i tpt))
             ;; Interleave prose-only and tool-output turns
             (pilish-bench--insert-turn
              "user" "Can you elaborate on that?")
             (pilish-bench--insert-turn
              "assistant"
              (concat "Let me dig deeper into the methodology.\n\n"
                      "The approach uses standard statistical methods "
                      "including regression analysis, hypothesis testing, "
                      "and confidence intervals to establish significance.")))))))))

;;;; Stage-Level Instrumentation

(defvar pilish-bench--instrumenting nil
  "Non-nil while `pilish-bench--with-instrumentation' is active.")

(defvar pilish-bench--stage-times nil
  "Alist of (STAGE-NAME . TOTAL-SECONDS) accumulated during instrumented runs.")

(defvar pilish-bench--stage-calls nil
  "Alist of (STAGE-NAME . CALL-COUNT) accumulated during instrumented runs.")

(defmacro pilish-bench--with-instrumentation (&rest body)
  "Execute BODY with stage-level timing instrumentation active.
Binds `pilish-bench--stage-times' and `--stage-calls',
installs timing advice around key functions, and removes it afterward."
  (declare (indent 0))
  `(let ((pilish-bench--instrumenting t)
         (pilish-bench--stage-times nil)
         (pilish-bench--stage-calls nil))
     (pilish-bench--install-timing-advice)
     (unwind-protect
         (progn ,@body)
       (pilish-bench--remove-timing-advice))
     (cons pilish-bench--stage-times
           pilish-bench--stage-calls)))

(defun pilish-bench--record-stage (name elapsed)
  "Record ELAPSED seconds for stage NAME."
  (if-let* ((cell (assq name pilish-bench--stage-times)))
      (setcdr cell (+ (cdr cell) elapsed))
    (push (cons name elapsed) pilish-bench--stage-times))
  (if-let* ((cell (assq name pilish-bench--stage-calls)))
      (setcdr cell (1+ (cdr cell)))
    (push (cons name 1) pilish-bench--stage-calls)))

(defun pilish-bench--make-timing-advice (name)
  "Return an :around advice function that records timing for stage NAME."
  (lambda (orig-fn &rest args)
    (if pilish-bench--instrumenting
        (let ((t0 (float-time)))
          (prog1 (apply orig-fn args)
            (pilish-bench--record-stage
             name (- (float-time) t0))))
      (apply orig-fn args))))

(defconst pilish-bench--instrumented-functions
  '((treesit-detect   . pilish--treesit-table-regions)
    (visible-string   . pilish--markdown-visible-string)
    (compute-widths   . markdown-table-wrap-compute-widths)
    (wrap-cell        . markdown-table-wrap-cell)
    (render-row-lines . pilish--render-table-row-lines)
    (display-groups   . pilish--table-display-groups)
    (decorate-table   . pilish--decorate-table)
    (remove-overlays  . pilish--remove-table-overlays)
    (split-row        . markdown-table-wrap--split-table-row))
  "Alist of (STAGE-NAME . FUNCTION) to instrument.")

(defun pilish-bench--install-timing-advice ()
  "Install :around timing advice on all instrumented functions."
  (dolist (entry pilish-bench--instrumented-functions)
    (advice-add (cdr entry) :around
                (pilish-bench--make-timing-advice (car entry))
                '((name . pilish-bench-timing)))))

(defun pilish-bench--remove-timing-advice ()
  "Remove timing advice from all instrumented functions."
  (dolist (entry pilish-bench--instrumented-functions)
    (advice-remove (cdr entry) 'pilish-bench-timing)))

;;;; Scenario Definitions

(defun pilish-bench--stable-decorate ()
  "Decorate all fixture tables in a mixed chat buffer at width 80."
  (pilish-bench--load-fixtures)
  (let ((buf (pilish-bench--build-mixed-buffer
              pilish-bench--tables)))
    (unwind-protect
        (benchmark-run 1
          (with-current-buffer buf
            (pilish--decorate-tables-in-region
             (point-min) (point-max) 80)))
      (kill-buffer buf))))

(defun pilish-bench--stable-decorate-narrow ()
  "Decorate all fixture tables at narrow width 40 (maximum wrapping)."
  (pilish-bench--load-fixtures)
  (let ((buf (pilish-bench--build-mixed-buffer
              pilish-bench--tables)))
    (unwind-protect
        (benchmark-run 1
          (with-current-buffer buf
            (pilish--decorate-tables-in-region
             (point-min) (point-max) 40)))
      (kill-buffer buf))))

(defun pilish-bench--streaming-refresh ()
  "Simulate streaming: decorate only the last table after a newline delta."
  (pilish-bench--load-fixtures)
  (let ((buf (pilish-bench--build-mixed-buffer
              pilish-bench--tables)))
    (unwind-protect
        (with-current-buffer buf
          ;; Pre-decorate all tables
          (pilish--decorate-tables-in-region
           (point-min) (point-max) 80)
          ;; Now measure redecoration of just the last table
          (let* ((regions (pilish--treesit-table-regions
                           (point-min) (point-max)))
                 (last-region (car (last regions)))
                 (lbeg (car last-region))
                 (lend (cdr last-region)))
            (benchmark-run 1
              (pilish--remove-table-overlays lbeg lend)
              (pilish--decorate-table lbeg lend 80))))
      (kill-buffer buf))))

(defun pilish-bench--resize-full ()
  "Full-buffer resize: redecorate all tables at a new width."
  (pilish-bench--load-fixtures)
  (let ((buf (pilish-bench--build-mixed-buffer
              pilish-bench--tables)))
    (unwind-protect
        (with-current-buffer buf
          (pilish--decorate-tables-in-region
           (point-min) (point-max) 80)
          (benchmark-run 1
            (pilish--decorate-tables-in-region
             (point-min) (point-max) 60)))
      (kill-buffer buf))))

(defun pilish-bench--resize-hot-tail ()
  "Hot-tail resize: redecorate only the last 2 tables at a new width."
  (pilish-bench--load-fixtures)
  (let ((buf (pilish-bench--build-mixed-buffer
              pilish-bench--tables)))
    (unwind-protect
        (with-current-buffer buf
          (pilish--decorate-tables-in-region
           (point-min) (point-max) 80)
          (let* ((regions (pilish--treesit-table-regions
                           (point-min) (point-max)))
                 (n (length regions))
                 (hot-start (car (nth (max 0 (- n 2)) regions))))
            (benchmark-run 1
              (pilish--decorate-tables-in-region
               hot-start (point-max) 60))))
      (kill-buffer buf))))

(defun pilish-bench--resize-sweep ()
  "Resize sweep: redecorate all tables at 10 different widths."
  (pilish-bench--load-fixtures)
  (let ((buf (pilish-bench--build-mixed-buffer
              pilish-bench--tables))
        (widths (number-sequence 40 130 10)))
    (unwind-protect
        (benchmark-run 1
          (with-current-buffer buf
            (dolist (w widths)
              (pilish--decorate-tables-in-region
               (point-min) (point-max) w))))
      (kill-buffer buf))))

(defun pilish-bench--visible-string-extraction ()
  "Measure visible-string extraction for diverse cell content.
No caching: each call uses a fresh cell to avoid hash-table hits."
  (pilish-bench--load-fixtures)
  (let ((cells '("plain text"
                 "**bold text**"
                 "*italic text*"
                 "`code text`"
                 "***bold italic***"
                 "**bold** and *italic* and `code`"
                 "Jacobo **Árbenz Guzmán**, president"
                 "[link](https://example.com/review)"
                 "***Democracy for the Few***"
                 "`978-0-87286-298-2`"
                 "**北京大学**和`复旦大学`列为参考"
                 "**東京大学**の*研究センター*")))
    (benchmark-run 1
      (dolist (cell cells)
        (pilish--markdown-visible-string cell)))))

(defun pilish-bench--inline-fontification-cost ()
  "Measure the inline fontification step alone (without mode setup).
Uses a pre-existing chat-mode buffer to isolate font-lock-ensure cost."
  (let ((buf (pilish-bench--make-chat-buffer #'ignore))
        (cells '("**bold** and *italic* and `code`"
                 "Jacobo **Árbenz Guzmán**, president"
                 "***Democracy for the Few***"
                 "**北京大学**和`复旦大学`列为参考")))
    (unwind-protect
        (benchmark-run 1
          (dolist (cell cells)
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert cell)
                (font-lock-ensure)
                (pilish--visible-text
                 (point-min) (point-max))))))
      (kill-buffer buf))))

;;;; Runner

(defconst pilish-bench--suite
  '(("stable-all"       . pilish-bench--stable-decorate)
    ("stable-narrow"     . pilish-bench--stable-decorate-narrow)
    ("stream-last"       . pilish-bench--streaming-refresh)
    ("resize-full"       . pilish-bench--resize-full)
    ("resize-hot-tail"   . pilish-bench--resize-hot-tail)
    ("resize-sweep"      . pilish-bench--resize-sweep)
    ("visible-string"    . pilish-bench--visible-string-extraction)
    ("fontify-only"      . pilish-bench--inline-fontification-cost))
  "Alist of (NAME . FUNCTION) for the benchmark suite.")

(defvar pilish-bench--repetitions 5
  "Number of repetitions for each benchmark.")

(defun pilish-bench-run (&optional repetitions)
  "Run all benchmarks REPETITIONS times, print results.
Default is 5 repetitions."
  (let ((k (or repetitions pilish-bench--repetitions)))
    (pilish-bench--load-fixtures)

    ;; Header
    (princ (format "\npilish table rendering benchmarks (%d reps)\n" k))
    (princ (format "Fixture: %d tables, %s rows\n"
                   (length pilish-bench--tables)
                   (mapconcat (lambda (tbl)
                                (number-to-string
                                 (length (split-string tbl "\n" t))))
                              pilish-bench--tables "/")))
    (princ (format "Emacs %s, %s\n\n"
                   emacs-version
                   (if noninteractive "batch (no font engine)"
                     (format "GUI (font: %s)"
                             (face-attribute 'default :family)))))

    ;; Column headers
    (princ (format "%-18s %9s %9s %9s  %4s %9s\n"
                   "scenario" "min" "median" "max" "GCs" "GC-time"))
    (princ (format "%-18s %9s %9s %9s  %4s %9s\n"
                   "--------" "---" "------" "---" "---" "-------"))

    ;; Run suite
    (dolist (entry pilish-bench--suite)
      (let ((name (car entry))
            (fn (cdr entry))
            (times nil)
            (total-gcs 0)
            (total-gc-time 0.0))
        (dotimes (_ k)
          (garbage-collect)
          (let ((result (funcall fn)))
            (push (car result) times)
            (setq total-gcs (+ total-gcs (nth 1 result)))
            (setq total-gc-time (+ total-gc-time (nth 2 result)))))
        (let* ((sorted (sort (copy-sequence times) #'<))
               (mn (car sorted))
               (mx (car (last sorted)))
               (med (nth (/ k 2) sorted)))
          (princ (format "%-18s %8.1fms %8.1fms %8.1fms  %4d %8.1fms\n"
                         name
                         (* 1000 mn) (* 1000 med) (* 1000 mx)
                         total-gcs (* 1000 total-gc-time))))))

    ;; Stage decomposition for the stable-all scenario.
    ;; Stages are nested: decorate-table > display-groups > visible-string,
    ;; so totals overlap.  The report shows both inclusive time (what a stage
    ;; and its children cost) and leaf-only breakdown.
    (princ "\n--- Stage decomposition (stable-all, single run) ---\n")
    (princ "Note: stages are nested; inclusive times overlap.\n\n")
    (let* ((result (pilish-bench--with-instrumentation
                     (pilish-bench--stable-decorate)))
           (stage-times (car result))
           (stage-calls (cdr result)))
      (princ (format "%-22s %9s %7s %7s\n"
                     "stage" "inclusive" "calls" "avg"))
      (princ (format "%-22s %9s %7s %7s\n"
                     "-----" "---------" "-----" "---"))
      (dolist (stage (sort (copy-sequence stage-times)
                           (lambda (a b) (> (cdr a) (cdr b)))))
        (let ((calls (or (cdr (assq (car stage) stage-calls)) 1)))
          (princ (format "%-22s %8.1fms %7d %6.2fms\n"
                         (car stage)
                         (* 1000 (cdr stage))
                         calls
                         (/ (* 1000 (cdr stage)) (float calls)))))))

    (princ "\n")))

(defun pilish-bench-run-batch ()
  "Entry point for batch execution.  Run benchmarks and exit."
  (let ((standard-output #'external-debugging-output)
        (args command-line-args-left)
        (k pilish-bench--repetitions))
    (while args
      (cond ((string= (car args) "-c")
             (setq args (cdr args))
             (when args
               (setq k (string-to-number (pop args)))))
            (t (pop args))))
    (pilish-bench-run k)
    (kill-emacs 0)))

(provide 'pilish-bench)
;;; pilish-bench.el ends here
