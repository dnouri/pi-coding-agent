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

## Paren Balance Check

After editing `.el` files, verify parentheses are balanced:
```bash
make check-parens
```

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

## Tmux Testing (Spike Scripts)

For reproducing visual bugs or testing interactive behavior, write a spike
script in `./tmp/` (gitignored) and load it into Emacs inside tmux.

Every spike script needs this boilerplate at the top (use the actual
absolute path to your checkout):
```elisp
(setq inhibit-startup-screen t)
(add-to-list 'load-path "/absolute/path/to/pi-coding-agent")
(require 'package)
(package-initialize)
(require 'pi-coding-agent)
```

Launch with (from the project root):
```bash
tmux new-session -d -s test -x 120 -y 40 \
  "emacs -nw -Q -l $PWD/tmp/spike.el 2>tmp/spike.log"
sleep 2 && tmux capture-pane -t test -p
```

To start a full interactive pi session in tmux:
```bash
tmux new-session -d -s test -x 120 -y 40 \
  "emacs -nw -Q --eval \"(progn (require 'package) (package-initialize) \
    (add-to-list 'load-path \\\"$PWD\\\") \
    (require 'pi-coding-agent) (pi))\""
```

Common gotchas:
- **`-Q` is required** but skips package init — the boilerplate above fixes that
- **Sleep timing**: use `sleep 2` for UI ops, `sleep 10`+ for LLM responses
- **Buffer names** follow `*pi-coding-agent-{chat,input}:<dir>*` (abbreviated),
  e.g. `*pi-coding-agent-chat:~/co/pi-coding-agent/*`
- **Window focus**: the pi layout has two windows; `C-x o` switches between them.
  Prefer spike scripts over interactive `tmux send-keys` when possible —
  they're reproducible, debuggable, and don't require tracking focus state

## Reference: pi CLI and RPC Protocol

The pi CLI (TypeScript) is the reference implementation. When implementing
new RPC commands, understanding event formats, or checking how the TUI
handles something, consult its source.

**Finding the checkout:** Look for a local clone, or clone one:
```bash
PI_MONO=$(find ~/co ~/src ~/projects /tmp -maxdepth 2 -name "pi-mono" -type d 2>/dev/null | head -1)
if [ -z "$PI_MONO" ]; then
  git clone git@github.com:badlogic/pi-mono.git /tmp/pi-mono
  PI_MONO=/tmp/pi-mono
fi
```

**Key files** (under `$PI_MONO/packages/coding-agent/`):

| File | When to consult |
|------|-----------------|
| `docs/rpc.md` | RPC command/event format, protocol overview |
| `src/modes/rpc/rpc-types.ts` | Type definitions for all RPC commands and events |
| `src/modes/rpc/rpc-mode.ts` | How RPC commands are dispatched and events emitted |
| `src/modes/interactive/interactive-mode.ts` | How the TUI handles events, tool display, streaming |
| `src/modes/interactive/components/tool-execution.ts` | TUI tool output rendering |
| `src/core/agent-session.ts` | Session lifecycle, forking, message handling |

**Other useful packages:**

| Path | Contents |
|------|----------|
| `$PI_MONO/packages/agent/src/agent-loop.ts` | Core agent loop, tool execution |
| `$PI_MONO/packages/agent/src/types.ts` | Agent-level type definitions |
| `$PI_MONO/packages/ai/src/types.ts` | AI provider types (messages, tools, content blocks) |

**When to look:** Before implementing a new RPC command, when an event format
is unclear, when the Emacs behavior should match the TUI, or when debugging
protocol mismatches.

## Git Hygiene

Always use `git add <specific-files>` instead of `git add -A`. The latter
stages everything including spike scripts, test artifacts, and `.pi/` files.
Run `git status` before committing to verify what's staged.

## Key Conventions

- All public symbols are prefixed `pi-coding-agent-`
- Internal symbols use `pi-coding-agent--` (double dash)
- Tests are named `pi-coding-agent-test-<description>`
- Test files require `pi-coding-agent-test-common` for shared fixtures
