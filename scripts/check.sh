#!/bin/bash
# Quality checks for pi-coding-agent - run before commits
#
# Usage:
#   ./scripts/check.sh              # Unit tests only (fast)
#   ./scripts/check.sh --integration # Include integration tests (slow, needs pi)
#   ./scripts/check.sh -i            # Short form

set -e

cd "$(dirname "$0")/.."

# Parse arguments
RUN_INTEGRATION=false
for arg in "$@"; do
    case $arg in
        --integration|-i)
            RUN_INTEGRATION=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [--integration|-i]"
            echo ""
            echo "Options:"
            echo "  --integration, -i  Run integration tests (requires pi installed)"
            echo "  --help, -h         Show this help"
            exit 0
            ;;
    esac
done

echo "=== Byte-compiling ==="
emacs --batch \
    -L . \
    --eval "(require 'package)" \
    --eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)" \
    --eval "(package-initialize)" \
    --eval "(setq byte-compile-error-on-warn t)" \
    -f batch-byte-compile \
    pi-coding-agent-core.el pi-coding-agent.el

echo ""
echo "=== Checking docstrings ==="
# Capture warnings from checkdoc and fail if any are found
CHECKDOC_OUTPUT=$(emacs --batch \
    -L . \
    --eval "(require 'checkdoc)" \
    --eval "(setq sentence-end-double-space nil)" \
    --eval "(checkdoc-file \"pi-coding-agent-core.el\")" \
    --eval "(checkdoc-file \"pi-coding-agent.el\")" 2>&1)

if echo "$CHECKDOC_OUTPUT" | grep -q "^Warning"; then
    echo "$CHECKDOC_OUTPUT" | grep "^Warning"
    echo "Checkdoc failed!"
    exit 1
fi
echo "Checkdoc: Done."

echo ""
echo "=== Package-lint ==="
emacs --batch \
    -L . \
    --eval "(require 'package)" \
    --eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)" \
    --eval "(package-initialize)" \
    --eval "(unless (package-installed-p 'package-lint) \
              (package-refresh-contents) \
              (package-install 'package-lint))" \
    --eval "(require 'package-lint)" \
    --eval "(setq package-lint-main-file \"pi-coding-agent.el\")" \
    -f package-lint-batch-and-exit pi-coding-agent.el pi-coding-agent-core.el
echo "Package-lint: Done."

echo ""
echo "=== Running unit tests ==="
emacs --batch \
    -L . \
    -L test \
    --eval "(require 'package)" \
    --eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)" \
    --eval "(package-initialize)" \
    -l pi-coding-agent \
    -l pi-coding-agent-core-test \
    -l pi-coding-agent-test \
    -f ert-run-tests-batch-and-exit

if [ "$RUN_INTEGRATION" = true ]; then
    echo ""
    echo "=== Running integration tests ==="
    
    # Check if pi is available
    if ! command -v pi &> /dev/null; then
        echo "Error: pi executable not found in PATH"
        exit 1
    fi
    
    PI_RUN_INTEGRATION=1 emacs --batch \
        -L . \
        -L test \
        --eval "(require 'package)" \
        --eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)" \
        --eval "(package-initialize)" \
        -l pi-coding-agent \
        -l pi-coding-agent-integration-test \
        -f ert-run-tests-batch-and-exit
fi

echo ""
echo "=== All checks passed ==="

# Clean up .elc files (we don't commit them)
rm -f *.elc
