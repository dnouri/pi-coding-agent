#!/bin/bash
# run-gui-tests.sh - Run ERT tests that require a GUI
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
#   ./test/run-gui-tests.sh pi-coding-agent-gui-test-session    # Run specific test

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

HEADLESS="${PI_HEADLESS:-0}"
SELECTOR="\"pi-coding-agent-gui-test-\""

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

echo "=== Pi.el GUI Tests ==="
echo "Project: $PROJECT_DIR"
echo "Selector: $SELECTOR"
if [ "$HEADLESS" = "1" ]; then
    echo "Mode: headless (xvfb)"
else
    echo "Mode: display"
fi
echo ""

# Create elisp to run tests
cat > /tmp/run-gui-tests.el << 'ELISP_END'
;; Setup
(setq inhibit-startup-screen t)
ELISP_END

cat >> /tmp/run-gui-tests.el << EOF
(set-frame-size (selected-frame) $WIDTH $HEIGHT)
(add-to-list 'load-path "$PROJECT_DIR")
(add-to-list 'load-path "$PROJECT_DIR/test")

;; Initialize packages to find markdown-mode
(require 'package)
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(package-initialize)

;; Load test utilities and tests
(require 'pi-coding-agent-gui-test-utils)
(require 'pi-coding-agent-gui-tests)

;; Redirect messages to file for results
(defvar pi-coding-agent-gui-test--output-file "$RESULTS_FILE")

;; Helper to print to stderr immediately (unbuffered, works in non-batch mode)
(defun pi-coding-agent-gui-test--log (fmt &rest args)
  "Print formatted message to stderr immediately."
  (princ (concat (apply #'format fmt args) "\n") #'external-debugging-output))

;; Run tests and collect results
;; Order tests: session-starts first, then alphabetically
(defun pi-coding-agent-gui-test--order-tests (tests)
  "Order TESTS so session-starts runs first."
  (let (first-test rest-tests)
    (dolist (test tests)
      (if (eq (ert-test-name test) 'pi-coding-agent-gui-test-session-starts)
          (setq first-test test)
        (push test rest-tests)))
    (if first-test
        (cons first-test (nreverse rest-tests))
      (nreverse rest-tests))))

(let* ((selector $SELECTOR)
       (passed 0)
       (failed 0)
       (total 0)
       (test-list (pi-coding-agent-gui-test--order-tests (ert-select-tests selector t))))
  (setq total (length test-list))
  (pi-coding-agent-gui-test--log "Running %d GUI tests..." total)
  (with-temp-buffer
    (insert "=== GUI Test Results ===\n\n")
    (let ((n 0))
      (dolist (test test-list)
        (let* ((name (ert-test-name test))
               (start (float-time))
               (result (ert-run-test test))
               (elapsed (pi-coding-agent-test-format-elapsed (- (float-time) start))))
          (setq n (1+ n))
          (cond
           ((ert-test-passed-p result)
            (setq passed (1+ passed))
            (pi-coding-agent-gui-test--log "  [%d/%d] PASS: %s (%s)" n total name elapsed)
            (insert (format "  PASS: %s (%s)\n" name elapsed)))
           (t
            (setq failed (1+ failed))
            (pi-coding-agent-gui-test--log "  [%d/%d] FAIL: %s (%s)" n total name elapsed)
            (insert (format "  FAIL: %s (%s)\n" name elapsed))
            (when (ert-test-failed-p result)
              (insert (format "        %S\n" (ert-test-result-with-condition-condition result))))
            ;; Debug: print diagnostic info on failure
            (when (and (boundp 'pi-coding-agent-gui-test--session) pi-coding-agent-gui-test--session)
              ;; Process status
              (when-let ((proc (plist-get pi-coding-agent-gui-test--session :process)))
                (pi-coding-agent-gui-test--log "  --- Process status ---")
                (pi-coding-agent-gui-test--log "  Status: %s, Exit: %s"
                                  (process-status proc) (process-exit-status proc)))
              ;; Session state
              (pi-coding-agent-gui-test--log "  --- Session state ---")
              (pi-coding-agent-gui-test--log "  Model: %s, Streaming: %s"
                                (plist-get pi-coding-agent-gui-test--session :model)
                                (plist-get pi-coding-agent-gui-test--session :streaming))
              ;; Chat buffer content
              (when-let ((chat-buf (plist-get pi-coding-agent-gui-test--session :chat-buffer)))
                (when (buffer-live-p chat-buf)
                  (pi-coding-agent-gui-test--log "  --- Chat buffer content ---")
                  (pi-coding-agent-gui-test--log "%s" (with-current-buffer chat-buf (buffer-string)))
                  (pi-coding-agent-gui-test--log "  --- End chat buffer ---")))))))))
    (insert (format "\n=== %d tests: %d passed, %d failed ===\n" total passed failed))
    (write-region (point-min) (point-max) pi-coding-agent-gui-test--output-file))
  (pi-coding-agent-gui-test--log "=== %d tests: %d passed, %d failed ===" total passed failed)
  (kill-emacs (if (> failed 0) 1 0)))
EOF

# Run Emacs (not in batch mode - we need GUI)
# Temporarily disable set -e since Emacs returns non-zero on test failure
set +e
if [ "$HEADLESS" = "1" ]; then
    # GDK_BACKEND=x11 forces GTK/PGTK to use X11 instead of auto-detecting Wayland
    # PATH must be passed explicitly so Emacs can find 'pi'
    # </dev/null prevents "standard input is not a tty" error in CI
    xvfb-run -a env GDK_BACKEND=x11 PATH="$PATH" emacs -Q -l /tmp/run-gui-tests.el </dev/null 2>&1
else
    emacs -Q -l /tmp/run-gui-tests.el 2>&1
fi
EXIT_CODE=$?
set -e

# Show results
if [[ -f "$RESULTS_FILE" ]]; then
    cat "$RESULTS_FILE"
    rm -f "$RESULTS_FILE"
fi
rm -f /tmp/run-gui-tests.el

exit $EXIT_CODE
