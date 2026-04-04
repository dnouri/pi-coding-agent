# Fake pi contract for deterministic frontend tests

This note defines the smallest fake-pi surface that is worth building.
The fake is a protocol double for the RPC subprocess boundary, not a mock
of internal Emacs functions.

## Scope and seam

The Emacs frontend already has the right seam:

- `pi-coding-agent-executable`
- `pi-coding-agent-extra-args`
- `pi-coding-agent--start-process`
- the real process filter / sentinel / display handler path

The fake must enter through that seam unchanged. Test helpers may bind the
executable and extra args, but production startup code should not grow a
special fake-only branch.

## File layout

- Harness executable: `test/support/fake_pi.py`
- Harness contract note: `test/support/fake-pi-contract.md`
- Scenario fixtures / transcripts: `test/fixtures/fake-pi/`
- Scenario fixture format notes: `test/fixtures/fake-pi/README.md`
- One-off experiments: `tmp/`

The harness should speak strict JSONL on the wire while using a simpler,
more expressive internal scenario DSL.

## Why this fake exists

Two distinct risks need coverage:

1. The Emacs frontend must keep working against a pi-like RPC subprocess.
2. The real pi CLI may drift from the double.

So the fake is for deterministic GUI and integration scenarios, while a
thinner real-backend suite remains as a compatibility backstop.

## Current slow-test value review

### Still boundary-valuable

These are still worth covering at the real subprocess boundary:

- Integration RPC smoke:
  - process spawn / lifecycle
  - `get_state`
  - `get_commands`
  - `new_session`
  - `get_fork_messages`
- Integration prompt lifecycle:
  - immediate `prompt` success plus delayed streamed events
  - `agent_start` / `message_start` / `message_update` / `message_end` / `agent_end`
  - idle state after completion
  - persisted message count change
- Integration distinct behaviors:
  - `abort`
  - `steer`
  - session-name persistence through a real session file
- GUI-only regressions:
  - follow-scroll when the window is already at end
  - preserve scroll while scrolled up
  - visible tool rendering / overlay boundaries
  - extension UI round-trips in a real chat buffer

### Already strongly shadowed by unit coverage

These have strong direct coverage outside slow suites and should only stay in
GUI/integration form when they still prove a real boundary risk:

- linked chat/input buffer kill behavior
- many markdown / fence / blank-line rendering rules
- most extension UI method dispatch details
- menu / command list shaping logic after `get_commands`

## Wire-level rules the fake must obey

- Strict JSONL with `\n` as the record delimiter
- Accept optional trailing `\r` on input lines
- Flush each output record promptly
- `prompt` must return an immediate success response before later events
- Events are id-less; responses use `type: "response"`
- Unsupported commands should fail loudly with `success: false`

## V1 command surface

Required now because the current frontend and shared contract read it:

- `get_state`
- `get_commands`
- `prompt`
- `abort`
- `new_session`
- `get_fork_messages`
- `set_session_name`
- `set_model`
- `set_thinking_level`
- `extension_ui_response`

Also required by the shared integration contract:

- `steer`

Still out of scope:

- `follow_up`
- `get_messages`
- tree/navigation RPC
- compaction / retry / bash RPC
- session listing / export / HTML

## V1 event surface

Required now:

- `agent_start`
- `agent_end`
- `message_start`
- `message_update`
- `message_end`
- `tool_execution_start`
- `tool_execution_update`
- `tool_execution_end`
- `extension_ui_request`

The fake does not need `turn_start`, `turn_end`, retry, compaction, or other
higher-level events until a test genuinely needs them.

## Required fields by surface

### `get_state`

Fields the current Emacs code or assertions actively read:

- `model`
- `thinkingLevel`
- `isStreaming`
- `isCompacting`
- `sessionId`
- `sessionFile`
- `messageCount`
- `pendingMessageCount`

Useful for fidelity but not currently required by the Emacs frontend:

- `sessionName`
- `steeringMode`
- `followUpMode`
- `autoCompactionEnabled`

### `get_commands`

Required shape:

- response `data.commands` must be a JSON array
- each command used by assertions needs at least:
  - `name`
  - `source`

`description` may be omitted unless a test needs it.  Command
metadata uses `sourceInfo` with `scope` and `path` sub-fields.
The Emacs normalizer lifts these to top-level `:location` and `:path`.

### `prompt` happy path

Required behavior:

1. send success response immediately
2. later emit `agent_start`
3. emit `message_start`
4. emit one or more `message_update` events with
   `assistantMessageEvent.type: "text_delta"`
5. emit `message_end`
6. emit `agent_end`
7. update `get_state.isStreaming` and `messageCount`
8. persist enough session data to back session-file assertions

### Tool execution path

For deterministic GUI tests, the fake must be able to emit:

- `message_update` with `assistantMessageEvent.type: "toolcall_start"`
- optional `toolcall_delta`
- `tool_execution_start`
- `tool_execution_update` with accumulated `partialResult`
- `tool_execution_end`

Required fields currently consumed by Emacs rendering:

- `toolCallId`
- `toolName`
- `args`
- `partialResult`
- `result`
- `isError`

### Fork messages

Required shape:

- response `data.messages` is a JSON array
- each entry includes `entryId`
- `text` is enough for current picker formatting tests

### Session naming

Required behavior:

- `set_session_name` succeeds for non-empty names
- fake writes a real session file on disk
- file starts with a `session` header line
- file appends `session_info` entries that Emacs can parse

The fake only needs the minimum JSONL structure that current Emacs session
metadata parsing reads.

### Extension UI

Required request methods for v1 test coverage:

- `confirm`
- `input`
- `select`
- `editor`
- fire-and-forget methods the frontend already handles, especially
  `notify`, `setStatus`, and `set_editor_text`

Required response shape:

- `type: "extension_ui_response"`
- matching request `id`
- one of `confirmed`, `value`, or `cancelled`

Timeouts for dialog requests should be explicit scenario data, not hidden magic
constants in the harness. Fast defaults are good for automated tests, but the
manual-debugging path should be able to extend or disable those timeouts from
the CLI so a human can inspect the UI before responding.

## Real session-file minimum

The fake must create real temporary files, not invented paths. The minimum
useful on-disk shape is:

- one `session` header line
- zero or more `message` lines
- zero or more `session_info` lines

That is enough for:

- `sessionFile` existence checks
- session-name persistence checks
- resume metadata parsing in Emacs

## Backend helper API

Keep backend choice explicit in tests.

The shared helper returns a backend plist with:

- backend symbol: `real` or `fake`
- backend label for failure output
- executable command list
- extra args
- optional fake scenario name

The important design point is visibility: a failing test should say which
backend and which scenario was running.

## Intentionally out of scope for v1

The fake should not try to model all of pi.

Out of scope until a concrete test needs it:

- full prompt/template/skill expansion fidelity
- tree browsing / branch summary / navigation RPC
- compaction and retry flows
- bash RPC command semantics
- session listing across projects
- provider/model discovery parity with the real backend
- extension runtime behavior beyond the RPC UI sub-protocol
- every event and field documented in upstream `rpc.md`

