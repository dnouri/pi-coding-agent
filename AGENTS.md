# pi-coding-agent — Development Guide

Emacs frontend for the [pi coding agent](https://picodingagent.ai/).
Two-window UI: markdown chat buffer + prompt composition buffer.
Communicates with the pi CLI via JSON-over-stdio (RPC).

## Source Files

| File | Purpose |
|------|---------|
| `pi-coding-agent.el` | Main UI: chat rendering, tool display, event dispatch, transient menus (~4300 lines) |
| `pi-coding-agent-core.el` | Low-level: JSON parsing, line buffering, RPC protocol (~375 lines) |
| `test/pi-coding-agent-test.el` | Unit tests for the UI layer (~390 tests) |
| `test/pi-coding-agent-core-test.el` | Unit tests for core (~57 tests) |
| `test/pi-coding-agent-test-common.el` | Shared test utilities and fixtures |
| `test/pi-coding-agent-integration-test.el` | Integration tests (require running pi + Ollama) |
| `test/pi-coding-agent-gui-tests.el` | GUI tests (require display or xvfb) |
| `Makefile` | Build, test, lint targets |
| `scripts/check.sh` | Pre-commit hook: byte-compile + lint + tests |

## Running Tests

Run all unit tests (~446 tests, ~10s):
```bash
make test
```

Run a filtered subset by ERT pattern (~0.5s):
```bash
make test SELECTOR=fontify-buffer-tail
make test SELECTOR=toolcall-delta
make test SELECTOR=pi-coding-agent-test-abort-clears-followup-queue
```

The `SELECTOR` value is an ERT selector string — a substring match against test names.
Use `\|` for OR: `make test SELECTOR='abort\|followup'`

## Linting

```bash
make lint              # checkdoc + package-lint
make lint-checkdoc     # docstring warnings only
make lint-package      # MELPA package conventions only
make check             # byte-compile + lint + all tests (= pre-commit hook)
```

## Dependencies

`make test` auto-installs Emacs package deps (markdown-mode, transient) on first
run and caches via `.deps-stamp`. To force reinstall: `make clean` then `make test`.

## Pre-commit Hook

The git pre-commit hook runs `scripts/check.sh` (byte-compile + checkdoc +
package-lint + all unit tests, ~12s). Install with `make install-hooks`.

To skip for WIP commits: `git commit --no-verify`

## Key Conventions

- All public symbols are prefixed `pi-coding-agent-`
- Internal symbols use `pi-coding-agent--` (double dash)
- Tests are named `pi-coding-agent-test-<description>`
- Test files require `pi-coding-agent-test-common` for shared fixtures
