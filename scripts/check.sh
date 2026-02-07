#!/bin/bash
# Quality checks for pi-coding-agent - run before commits
#
# Usage:
#   ./scripts/check.sh              # Unit tests only (fast)
#   ./scripts/check.sh --integration # Include integration tests (slow, needs pi)
#   ./scripts/check.sh -i            # Short form

set -e

cd "$(dirname "$0")/.."

for arg in "$@"; do
    case $arg in
        --integration|-i)
            RUN_INTEGRATION=true
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [--integration|-i]"
            echo "  --integration, -i  Include integration tests (requires pi)"
            exit 0
            ;;
    esac
done

make check

if [ "${RUN_INTEGRATION:-false}" = true ]; then
    make test-integration
fi

rm -f *.elc
