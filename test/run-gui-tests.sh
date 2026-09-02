#!/usr/bin/env bash
# run-gui-tests.sh - Run the deterministic fake-backed GUI ERT suite
#
# No Docker, Ollama, or real pi install is required.  The suite still uses a
# real Emacs frame/window environment and the normal subprocess seam.
#
# Usage:
#   ./test/run-gui-tests.sh [options] [test-selector]
#
# Options:
#   --headless   Force headless mode (uses xvfb-run)
#
# Environment:
#   PI_HEADLESS=1   Same as --headless
#
# Examples:
#   ./test/run-gui-tests.sh                        # Run with display
#   ./test/run-gui-tests.sh --headless             # Run headless
#   ./test/run-gui-tests.sh piem-gui-test-session    # Run one GUI test

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

HEADLESS="${PI_HEADLESS:-0}"
EMACS_BIN="${EMACS:-emacs}"
if [ -z "${PACKAGE_USER_DIR:-}" ]; then
    EMACS_MAJOR_VERSION=$("$EMACS_BIN" --batch -Q \
        --eval '(princ emacs-major-version)')
    export PACKAGE_USER_DIR="$PROJECT_DIR/.cache/elpa/$EMACS_MAJOR_VERSION"
fi
SELECTOR="\"piem-gui-test-\""

# Parse args
while [[ $# -gt 0 ]]; do
    case "$1" in
        --headless) HEADLESS=1; shift ;;
        *) SELECTOR="\"$1\""; shift ;;
    esac
done

# Auto-detect headless if no display available
if [ "$HEADLESS" != "1" ] && [ -z "$DISPLAY" ]; then
    HEADLESS=1
fi

WIDTH=80
HEIGHT=22
RESULTS_FILE=$(mktemp /tmp/ert-results.XXXXXX)
RUNNER_FILE=$(mktemp /tmp/run-gui-tests.XXXXXX.el)
trap 'rm -f "$RESULTS_FILE" "$RUNNER_FILE"' EXIT

echo "=== piem GUI Tests ==="
echo "Project: $PROJECT_DIR"
echo "Selector: $SELECTOR"
if [ "$HEADLESS" = "1" ]; then
    echo "Mode: headless (xvfb)"
else
    echo "Mode: display"
fi
echo ""

# Create elisp to run tests
cat > "$RUNNER_FILE" << 'ELISP_END'
;; Setup
(setq inhibit-startup-screen t)
(setq load-prefer-newer t)
ELISP_END

cat >> "$RUNNER_FILE" << EOF
(set-frame-size (selected-frame) $WIDTH $HEIGHT)
(add-to-list 'load-path "$PROJECT_DIR")
(add-to-list 'load-path "$PROJECT_DIR/test")

;; Initialize the same project-local packages as the parent test lane.
(require 'package)
(let ((dir (getenv "PACKAGE_USER_DIR")))
  (when dir
    (setq package-user-dir
          (directory-file-name (expand-file-name dir)))))
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)

;; Load test utilities and tests
(require 'piem-gui-test-utils)
(require 'piem-gui-tests)

;; Redirect messages to file for results
(defvar piem-gui-test--output-file "$RESULTS_FILE")

;; Helper to print to stderr immediately (unbuffered, works in non-batch mode)
(defun piem-gui-test--log (fmt &rest args)
  "Print formatted message to stderr immediately."
  (princ (concat (apply #'format fmt args) "\n") #'external-debugging-output))

;; Run tests and collect results
;; Order tests: session-starts first, then alphabetically
(defun piem-gui-test--order-tests (tests)
  "Order TESTS so session-starts runs first."
  (let (first-test rest-tests)
    (dolist (test tests)
      (if (eq (ert-test-name test) 'piem-gui-test-session-starts)
          (setq first-test test)
        (push test rest-tests)))
    (if first-test
        (cons first-test (nreverse rest-tests))
      (nreverse rest-tests))))

(let* ((selector $SELECTOR)
       (passed 0)
       (skipped 0)
       (failed 0)
       (total 0)
       (test-list (piem-gui-test--order-tests (ert-select-tests selector t))))
  (setq total (length test-list))
  (piem-gui-test--log "Running %d GUI tests..." total)
  (with-temp-buffer
    (insert "=== GUI Test Results ===\n\n")
    (let ((n 0))
      (dolist (test test-list)
        (let* ((name (ert-test-name test))
               (start (float-time))
               (result (ert-run-test test))
               (elapsed (piem-test-format-elapsed (- (float-time) start))))
          (setq n (1+ n))
          (cond
           ((ert-test-passed-p result)
            (setq passed (1+ passed))
            (piem-gui-test--log "  [%d/%d] PASS: %s (%s)" n total name elapsed)
            (insert (format "  PASS: %s (%s)\n" name elapsed)))
           ((ert-test-skipped-p result)
            (setq skipped (1+ skipped))
            (let ((reason
                   (error-message-string
                    (ert-test-result-with-condition-condition result))))
              (piem-gui-test--log
               "  [%d/%d] SKIP: %s (%s): %s" n total name elapsed reason)
              (insert (format "  SKIP: %s (%s): %s\n" name elapsed reason))))
           (t
            (setq failed (1+ failed))
            (piem-gui-test--log "  [%d/%d] FAIL: %s (%s)" n total name elapsed)
            (insert (format "  FAIL: %s (%s)\n" name elapsed))
            (when (ert-test-failed-p result)
              (insert (format "        %S\n" (ert-test-result-with-condition-condition result))))
            ;; Debug: print diagnostic info on failure
            (when (and (boundp 'piem-gui-test--session) piem-gui-test--session)
              (piem-gui-test--log "  --- Session config ---")
              (piem-gui-test--log "  Backend: %s"
                                (plist-get (plist-get piem-gui-test--session :backend)
                                           :label))
              (piem-gui-test--log "  Options: %S"
                                (plist-get piem-gui-test--session :options))
              ;; Process status
              (when-let ((proc (plist-get piem-gui-test--session :process)))
                (piem-gui-test--log "  --- Process status ---")
                (piem-gui-test--log "  Status: %s, Exit: %s"
                                  (process-status proc) (process-exit-status proc))
                (piem-gui-test--log "  Events: %s, Last event: %S"
                                  (or (process-get proc 'piem-gui-test-event-count) 0)
                                  (process-get proc 'piem-gui-test-last-event)))
              ;; Chat buffer content
              (when-let ((chat-buf (plist-get piem-gui-test--session :chat-buffer)))
                (when (buffer-live-p chat-buf)
                  (piem-gui-test--log "  --- Chat buffer content ---")
                  (piem-gui-test--log "%s" (with-current-buffer chat-buf (buffer-string)))
                  (piem-gui-test--log "  --- End chat buffer ---")))))))))
    (insert (format "\n=== %d tests: %d passed, %d skipped, %d failed ===\n"
                    total passed skipped failed))
    (write-region (point-min) (point-max) piem-gui-test--output-file))
  (piem-gui-test--log
   "=== %d tests: %d passed, %d skipped, %d failed ==="
   total passed skipped failed)
  (kill-emacs (if (> failed 0) 1 0)))
EOF

# Run Emacs (not in batch mode - we need GUI)
# Temporarily disable set -e since Emacs returns non-zero on test failure
set +e
if [ "$HEADLESS" = "1" ]; then
    # GDK_BACKEND=x11 forces GTK/PGTK to use X11 instead of auto-detecting Wayland
    # </dev/null prevents "standard input is not a tty" error in CI
    xvfb-run -a env GDK_BACKEND=x11 PATH="$PATH" \
        "$EMACS_BIN" -Q -l "$RUNNER_FILE" </dev/null 2>&1
else
    "$EMACS_BIN" -Q -l "$RUNNER_FILE" 2>&1
fi
EXIT_CODE=$?
set -e

# Show results
if [[ -f "$RESULTS_FILE" ]]; then
    cat "$RESULTS_FILE"
fi

exit $EXIT_CODE
