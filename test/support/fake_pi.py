#!/usr/bin/env -S uv run --no-project --script
# /// script
# requires-python = ">=3.13"
# ///
"""Fake pi RPC harness for deterministic frontend tests.

This script is a small protocol double for pi's JSONL RPC mode.  It keeps the
wire contract strict while offering a tiny, data-driven scenario layer.

Manual usage examples:

    uv run --script test/support/fake_pi.py --scenario prompt-lifecycle
    ./test/support/fake_pi.py --scenario extension-confirm \
        --extension-timeout-ms 10000 --log-file /tmp/fake-pi.log

Scenario files live in ``test/fixtures/fake-pi/`` and currently support four
prompt behaviors:

``text_stream``
    Streams a simple assistant text reply in chunks and supports queued
    ``steer`` messages plus mid-stream ``abort``.

``extension_dialog``
    Emits an ``extension_ui_request`` and waits for a matching
    ``extension_ui_response``.  The timeout is scenario data and can be
    overridden (or disabled with ``--extension-timeout-ms 0``) for manual tmux
    debugging.

``custom_message``
    A slash command that optionally emits one visible custom message without a
    full assistant turn.

``tool_stream``
    Emits the streamed tool-call and tool-execution event surface, then ends
    with optional assistant text.
"""

from __future__ import annotations

import argparse
import json
import sys
import tempfile
import threading
import time
import uuid
from dataclasses import dataclass, field
from pathlib import Path
from collections.abc import Callable, Iterator
from typing import Any, BinaryIO, Literal

JsonDict = dict[str, Any]
DialogMethod = Literal["confirm", "input", "select", "editor"]


@dataclass(frozen=True)
class SlashCommand:
    """A slash command returned by ``get_commands``."""

    name: str
    source: Literal["extension", "prompt", "skill"]
    description: str | None = None
    path: str | None = None
    location: str | None = None

    def to_rpc(self) -> JsonDict:
        """Return this command in RPC response shape.

        Emits ``sourceInfo`` with ``scope`` and ``path`` sub-fields.
        """
        data: JsonDict = {"name": self.name, "source": self.source}
        if self.description is not None:
            data["description"] = self.description
        if self.path is not None or self.location is not None:
            source_info: JsonDict = {}
            if self.location is not None:
                source_info["scope"] = self.location
            if self.path is not None:
                source_info["path"] = self.path
            data["sourceInfo"] = source_info
        return data


@dataclass(frozen=True)
class TextStreamPrompt:
    """Scenario data for a simple streamed text reply."""

    type: Literal["text_stream"]
    assistant_text: str
    chunk_count: int = 4
    delay_ms: int = 30
    echo_user: bool = True
    steer_assistant_text: str | None = None


@dataclass(frozen=True)
class ExtensionDialogPrompt:
    """Scenario data for an extension dialog round-trip."""

    type: Literal["extension_dialog"]
    command_name: str
    method: DialogMethod
    title: str
    message: str | None = None
    placeholder: str | None = None
    options: list[str] = field(default_factory=list)
    prefill: str | None = None
    timeout_ms: int | None = None
    response_messages: dict[str, str] = field(default_factory=dict)


@dataclass(frozen=True)
class CustomMessagePrompt:
    """Scenario data for a slash command that may emit one custom message."""

    type: Literal["custom_message"]
    command_name: str
    message_text: str | None = None


@dataclass(frozen=True)
class ToolStreamPrompt:
    """Scenario data for a streamed tool execution flow."""

    type: Literal["tool_stream"]
    tool_name: str
    tool_args: JsonDict
    partial_result_text: str = ""
    result_text: str = ""
    assistant_text: str = ""
    delay_ms: int = 30
    echo_user: bool = True


PromptBehavior = (
    TextStreamPrompt | ExtensionDialogPrompt | CustomMessagePrompt | ToolStreamPrompt
)


@dataclass(frozen=True)
class Scenario:
    """Fully parsed scenario fixture."""

    name: str
    description: str
    commands: list[SlashCommand]
    prompt: PromptBehavior


@dataclass
class SessionState:
    """Mutable fake session state exposed by ``get_state``."""

    model: JsonDict
    thinking_level: str = "off"
    is_streaming: bool = False
    is_compacting: bool = False
    steering_mode: Literal["all", "one-at-a-time"] = "one-at-a-time"
    follow_up_mode: Literal["all", "one-at-a-time"] = "one-at-a-time"
    session_id: str = ""
    session_file: str = ""
    session_name: str | None = None
    auto_compaction_enabled: bool = False
    message_count: int = 0
    pending_message_count: int = 0

    def to_rpc(self) -> JsonDict:
        """Return this state in RPC response shape."""
        data: JsonDict = {
            "model": self.model,
            "thinkingLevel": self.thinking_level,
            "isStreaming": self.is_streaming,
            "isCompacting": self.is_compacting,
            "steeringMode": self.steering_mode,
            "followUpMode": self.follow_up_mode,
            "sessionFile": self.session_file,
            "sessionId": self.session_id,
            "autoCompactionEnabled": self.auto_compaction_enabled,
            "messageCount": self.message_count,
            "pendingMessageCount": self.pending_message_count,
        }
        if self.session_name is not None:
            data["sessionName"] = self.session_name
        return data


def now_ms() -> int:
    """Return the current Unix timestamp in milliseconds."""
    return int(time.time() * 1000)


def iter_jsonl_commands(stream: BinaryIO) -> Iterator[JsonDict]:
    """Yield strict LF-delimited JSON objects from ``stream``.

    This intentionally reads bytes and splits on ``b"\n"`` only, because
    Python's text-mode line iteration treats lone ``\r`` as a newline and would
    drift from pi's RPC framing contract. EOF does not terminate an incomplete
    record: without a final LF, the trailing bytes are ignored.
    """
    buffer = b""
    while chunk := stream.read1(4096):
        buffer += chunk
        while True:
            newline_index = buffer.find(b"\n")
            if newline_index == -1:
                break
            line = buffer[:newline_index]
            buffer = buffer[newline_index + 1 :]
            if line.endswith(b"\r"):
                line = line[:-1]
            if not line:
                continue
            yield json.loads(line.decode("utf-8"))


def default_scenario_dir() -> Path:
    """Return the default fixture directory for fake-pi scenarios."""
    return Path(__file__).resolve().parent.parent / "fixtures" / "fake-pi"


def load_scenario(path: Path, name: str) -> Scenario:
    """Load and validate a scenario fixture from ``path``."""
    data = json.loads(path.read_text(encoding="utf-8"))
    commands = []
    for item in data.get("commands", []):
        si = item.get("sourceInfo", {})
        commands.append(SlashCommand(
            name=item["name"],
            source=item["source"],
            description=item.get("description"),
            path=si.get("path"),
            location=si.get("scope"),
        ))
    prompt_data = data["prompt"]
    prompt_type = prompt_data["type"]
    if prompt_type == "text_stream":
        prompt: PromptBehavior = TextStreamPrompt(
            type="text_stream",
            assistant_text=prompt_data["assistant_text"],
            chunk_count=int(prompt_data.get("chunk_count", 4)),
            delay_ms=int(prompt_data.get("delay_ms", 30)),
            echo_user=bool(prompt_data.get("echo_user", True)),
            steer_assistant_text=prompt_data.get("steer_assistant_text"),
        )
    elif prompt_type == "extension_dialog":
        prompt = ExtensionDialogPrompt(
            type="extension_dialog",
            command_name=prompt_data["command_name"],
            method=prompt_data["method"],
            title=prompt_data["title"],
            message=prompt_data.get("message"),
            placeholder=prompt_data.get("placeholder"),
            options=list(prompt_data.get("options", [])),
            prefill=prompt_data.get("prefill"),
            timeout_ms=(
                int(prompt_data["timeout_ms"])
                if prompt_data.get("timeout_ms") is not None
                else None
            ),
            response_messages=dict(prompt_data.get("response_messages", {})),
        )
    elif prompt_type == "custom_message":
        prompt = CustomMessagePrompt(
            type="custom_message",
            command_name=prompt_data["command_name"],
            message_text=prompt_data.get("message_text"),
        )
    elif prompt_type == "tool_stream":
        prompt = ToolStreamPrompt(
            type="tool_stream",
            tool_name=prompt_data["tool_name"],
            tool_args=dict(prompt_data["tool_args"]),
            partial_result_text=prompt_data.get("partial_result_text", ""),
            result_text=prompt_data.get("result_text", ""),
            assistant_text=prompt_data.get("assistant_text", ""),
            delay_ms=int(prompt_data.get("delay_ms", 30)),
            echo_user=bool(prompt_data.get("echo_user", True)),
        )
    else:
        raise ValueError(f"Unsupported prompt type: {prompt_type}")
    return Scenario(
        name=name,
        description=data.get("description", name),
        commands=commands,
        prompt=prompt,
    )


class FakePiHarness:
    """Protocol double that speaks pi's JSONL RPC protocol."""

    def __init__(
        self,
        *,
        scenario: Scenario,
        session_dir: str | None,
        log_file: str | None,
        extension_timeout_ms: int | None,
        split_responses: dict[str, int],
    ) -> None:
        self.scenario = scenario
        self.log_file = Path(log_file) if log_file else None
        self.extension_timeout_ms = extension_timeout_ms
        self.split_responses = split_responses
        self._write_lock = threading.Lock()
        self._abort_requested = threading.Event()
        self._extension_waiter = threading.Event()
        self._extension_response: JsonDict | None = None
        self._pending_extension_id: str | None = None
        self._pending_steer_message: str | None = None
        self._run_thread: threading.Thread | None = None
        self._message_serial = 0
        self._session_root_dir = tempfile.TemporaryDirectory(
            prefix="fake-pi-", dir=session_dir
        )
        self._session_root = Path(self._session_root_dir.name)
        model = {
            "id": "fake-model",
            "name": "Fake Model",
            "provider": "fake",
            "api": "fake-api",
            "contextWindow": 8192,
            "maxTokens": 1024,
        }
        self.state = SessionState(model=model)
        self.user_messages: list[dict[str, str]] = []
        self._reset_session_file()

    def run(self) -> int:
        """Process stdin commands until EOF."""
        try:
            for command in iter_jsonl_commands(sys.stdin.buffer):
                self.handle(command)
            return 0
        finally:
            self._stop_active_run()
            self._session_root_dir.cleanup()

    def handle(self, command: JsonDict) -> None:
        """Handle a single RPC command."""
        self._log("in", command)
        command_type = command["type"]
        match command_type:
            case "get_state":
                self._respond(command, data=self.state.to_rpc())
            case "get_commands":
                self._respond(
                    command,
                    data={"commands": [item.to_rpc() for item in self.scenario.commands]},
                )
            case "prompt":
                self._handle_prompt(command)
            case "abort":
                self._abort_requested.set()
                self._respond(command)
            case "steer":
                self._handle_steer(command)
            case "new_session":
                self._handle_new_session(command)
            case "get_fork_messages":
                self._respond(command, data={"messages": self.user_messages})
            case "set_session_name":
                self._handle_set_session_name(command)
            case "set_model":
                self._handle_set_model(command)
            case "set_thinking_level":
                self.state.thinking_level = str(command["level"])
                self._respond(command)
            case "extension_ui_response":
                self._handle_extension_ui_response(command)
            case "follow_up":
                self._fail(command, "follow_up is intentionally out of scope for this fake")
            case _:
                self._fail(command, f"Unsupported fake-pi command: {command_type}")

    def _handle_prompt(self, command: JsonDict) -> None:
        """Start the scenario-specific prompt behavior."""
        if self.state.is_streaming:
            self._fail(command, "Fake pi is already streaming")
            return
        self._abort_requested.clear()
        message = str(command["message"])
        match self.scenario.prompt:
            case TextStreamPrompt() as behavior:
                self._respond(command)
                self._start_run(
                    name=f"fake-pi-text-stream-{self.scenario.name}",
                    target=lambda: self._run_text_prompt(message, behavior),
                )
            case ExtensionDialogPrompt() as behavior:
                if message != behavior.command_name:
                    self._fail(
                        command,
                        f"Scenario {self.scenario.name} only supports {behavior.command_name}",
                    )
                    return
                self._respond(command)
                self._start_run(
                    name=f"fake-pi-dialog-{self.scenario.name}",
                    target=lambda: self._run_extension_dialog(message, behavior),
                )
            case CustomMessagePrompt() as behavior:
                if message != behavior.command_name:
                    self._fail(
                        command,
                        f"Scenario {self.scenario.name} only supports {behavior.command_name}",
                    )
                    return
                self._respond(command)
                self._run_custom_message_prompt(message, behavior)
            case ToolStreamPrompt() as behavior:
                self._respond(command)
                self._start_run(
                    name=f"fake-pi-tool-stream-{self.scenario.name}",
                    target=lambda: self._run_tool_prompt(message, behavior),
                )
            case _:
                raise AssertionError("Unknown prompt behavior")

    def _handle_steer(self, command: JsonDict) -> None:
        """Queue a steering message for the active text stream."""
        if not self.state.is_streaming:
            self._fail(command, "Cannot steer when no prompt is streaming")
            return
        if not isinstance(self.scenario.prompt, TextStreamPrompt):
            self._fail(command, "Current fake scenario does not support steer")
            return
        self._pending_steer_message = str(command["message"])
        self._respond(command)

    def _handle_new_session(self, command: JsonDict) -> None:
        """Reset the fake to a fresh session."""
        self._stop_active_run()
        self._reset_session_file()
        self.state.is_streaming = False
        self.state.session_name = None
        self.state.message_count = 0
        self.state.pending_message_count = 0
        self.user_messages = []
        self._abort_requested.clear()
        self._respond(command, data={"cancelled": False})

    def _handle_set_session_name(self, command: JsonDict) -> None:
        """Persist a session name to the real session file."""
        name = str(command.get("name", "")).strip()
        if not name:
            self._fail(command, "Session name must be non-empty")
            return
        self.state.session_name = name
        self._append_session_line(
            {"type": "session_info", "id": self._entry_id("session-info"), "name": name}
        )
        self._respond(command)

    def _handle_set_model(self, command: JsonDict) -> None:
        """Update the fake model in place."""
        self.state.model = {
            **self.state.model,
            "provider": command["provider"],
            "id": command["modelId"],
            "name": command["modelId"],
        }
        self._respond(command, data=self.state.model)

    def _handle_extension_ui_response(self, command: JsonDict) -> None:
        """Resume a pending extension dialog request if the IDs match."""
        if self._pending_extension_id == command.get("id"):
            self._extension_response = command
            self._extension_waiter.set()
        self._log("extension-response", command)

    def _run_text_prompt(self, message: str, behavior: TextStreamPrompt) -> None:
        """Run a streamed-text prompt scenario."""
        self.state.is_streaming = True
        emitted_messages: list[JsonDict] = []
        current_message = message
        self._write_json({"type": "agent_start"})
        while True:
            completed, assistant_message = self._emit_text_turn(
                current_message,
                behavior=behavior,
                assistant_text_template=(
                    behavior.assistant_text
                    if current_message == message or behavior.steer_assistant_text is None
                    else behavior.steer_assistant_text
                ),
            )
            if not completed:
                self._finish_aborted_run()
                return
            emitted_messages.append(assistant_message)
            pending_steer = self._take_pending_steer()
            if pending_steer is None:
                break
            current_message = pending_steer
        self._finish_run(emitted_messages)

    def _emit_text_turn(
        self,
        user_text: str,
        *,
        behavior: TextStreamPrompt,
        assistant_text_template: str,
    ) -> tuple[bool, JsonDict]:
        """Emit one user->assistant text exchange.

        Returns ``(completed, assistant_message)``.  When ``completed`` is
        false, the assistant message is undefined because the run was aborted.
        """
        user_message = self._build_user_message(user_text)
        self._persist_user_message(user_message)
        if behavior.echo_user:
            self._write_json({"type": "message_start", "message": user_message})
            self._write_json({"type": "message_end", "message": user_message})
        assistant_text = assistant_text_template.format(message=user_text)
        assistant_placeholder: JsonDict = {"role": "assistant", "content": []}
        if not self._sleep_ms(behavior.delay_ms, abortable=True):
            return False, {}
        self._write_json({"type": "message_start", "message": assistant_placeholder})
        for chunk in self._chunk_text(assistant_text, behavior.chunk_count):
            if self._abort_requested.is_set():
                return False, {}
            self._write_json(
                {
                    "type": "message_update",
                    "message": assistant_placeholder,
                    "assistantMessageEvent": {
                        "type": "text_delta",
                        "contentIndex": 0,
                        "delta": chunk,
                    },
                }
            )
            if not self._sleep_ms(behavior.delay_ms, abortable=True):
                return False, {}
        assistant_message = self._build_assistant_message(assistant_text)
        self._persist_assistant_message(assistant_message)
        self._write_json({"type": "message_end", "message": assistant_message})
        return True, assistant_message

    def _run_extension_dialog(
        self,
        command_text: str,
        behavior: ExtensionDialogPrompt,
    ) -> None:
        """Run an extension dialog scenario until it resolves or times out."""
        self.state.is_streaming = True
        self._persist_user_message(self._build_user_message(command_text))
        self._write_json({"type": "agent_start"})
        request_id = f"ext-{uuid.uuid4().hex[:8]}"
        request = self._build_extension_request(request_id, behavior)
        self._write_json(request)
        response = self._wait_for_extension_response(request_id, self._dialog_timeout_ms(behavior))
        result_key = self._dialog_result_key(behavior.method, response)
        message_text = behavior.response_messages.get(
            result_key,
            behavior.response_messages.get("default", result_key.upper()),
        )
        followup = self._build_custom_message(message_text)
        self._persist_custom_message(followup)
        self._write_json({"type": "message_start", "message": followup})
        self._write_json({"type": "message_end", "message": followup})
        self._finish_run([followup])

    def _run_custom_message_prompt(
        self,
        command_text: str,
        behavior: CustomMessagePrompt,
    ) -> None:
        """Run a slash command that may emit one visible custom message."""
        self._persist_user_message(self._build_user_message(command_text))
        if not behavior.message_text:
            return
        followup = self._build_custom_message(
            behavior.message_text.format(message=command_text)
        )
        self._persist_custom_message(followup)
        self._write_json({"type": "message_start", "message": followup})
        self._write_json({"type": "message_end", "message": followup})

    def _run_tool_prompt(self, message: str, behavior: ToolStreamPrompt) -> None:
        """Run a prompt that emits tool-call and tool-execution events."""
        self.state.is_streaming = True
        self._write_json({"type": "agent_start"})
        user_message = self._build_user_message(message)
        self._persist_user_message(user_message)
        if behavior.echo_user:
            self._write_json({"type": "message_start", "message": user_message})
            self._write_json({"type": "message_end", "message": user_message})
        if self._abort_requested.is_set():
            self._finish_aborted_run()
            return
        tool_call_id = f"call-{uuid.uuid4().hex[:8]}"
        tool_call = {
            "type": "toolCall",
            "id": tool_call_id,
            "name": behavior.tool_name,
            "arguments": behavior.tool_args,
        }
        assistant_message: JsonDict = {"role": "assistant", "content": [tool_call]}
        if not self._sleep_ms(behavior.delay_ms, abortable=True):
            self._finish_aborted_run()
            return
        self._write_json({"type": "message_start", "message": assistant_message})
        self._write_json(
            {
                "type": "message_update",
                "message": assistant_message,
                "assistantMessageEvent": {
                    "type": "toolcall_start",
                    "contentIndex": 0,
                },
            }
        )
        self._write_json(
            {
                "type": "message_update",
                "message": assistant_message,
                "assistantMessageEvent": {
                    "type": "toolcall_delta",
                    "contentIndex": 0,
                    "delta": json.dumps(behavior.tool_args, ensure_ascii=False),
                },
            }
        )
        if not self._sleep_ms(behavior.delay_ms, abortable=True):
            self._finish_aborted_run()
            return
        self._write_json(
            {
                "type": "tool_execution_start",
                "toolCallId": tool_call_id,
                "toolName": behavior.tool_name,
                "args": behavior.tool_args,
            }
        )
        if behavior.partial_result_text:
            self._write_json(
                {
                    "type": "tool_execution_update",
                    "toolCallId": tool_call_id,
                    "toolName": behavior.tool_name,
                    "args": behavior.tool_args,
                    "partialResult": self._tool_result_payload(behavior.partial_result_text),
                }
            )
        if not self._sleep_ms(behavior.delay_ms, abortable=True):
            self._finish_aborted_run()
            return
        self._write_json(
            {
                "type": "tool_execution_end",
                "toolCallId": tool_call_id,
                "toolName": behavior.tool_name,
                "result": self._tool_result_payload(behavior.result_text),
                "isError": False,
            }
        )
        final_message = self._build_assistant_message(behavior.assistant_text)
        if behavior.assistant_text:
            for chunk in self._chunk_text(behavior.assistant_text, 2):
                self._write_json(
                    {
                        "type": "message_update",
                        "message": final_message,
                        "assistantMessageEvent": {
                            "type": "text_delta",
                            "contentIndex": 0,
                            "delta": chunk,
                        },
                    }
                )
        self._persist_assistant_message(final_message)
        self._write_json({"type": "message_end", "message": final_message})
        self._finish_run([final_message])

    def _build_extension_request(
        self, request_id: str, behavior: ExtensionDialogPrompt
    ) -> JsonDict:
        """Return the RPC event for an extension dialog request."""
        request: JsonDict = {
            "type": "extension_ui_request",
            "id": request_id,
            "method": behavior.method,
            "title": behavior.title,
        }
        timeout_ms = self._dialog_timeout_ms(behavior)
        if timeout_ms is not None:
            request["timeout"] = timeout_ms
        if behavior.method == "confirm":
            request["message"] = behavior.message or ""
        elif behavior.method == "input":
            if behavior.placeholder is not None:
                request["placeholder"] = behavior.placeholder
        elif behavior.method == "select":
            request["options"] = behavior.options
        elif behavior.method == "editor":
            if behavior.prefill is not None:
                request["prefill"] = behavior.prefill
        else:
            raise AssertionError(f"Unsupported dialog method: {behavior.method}")
        return request

    def _wait_for_extension_response(
        self, request_id: str, timeout_ms: int | None
    ) -> JsonDict | None:
        """Wait for a matching extension dialog response."""
        self._pending_extension_id = request_id
        self._extension_response = None
        self._extension_waiter.clear()
        try:
            if timeout_ms is None:
                while not self._extension_waiter.wait(0.01):
                    if self._abort_requested.is_set():
                        return None
            else:
                deadline = time.monotonic() + (timeout_ms / 1000)
                while time.monotonic() < deadline:
                    if self._extension_waiter.wait(0.01):
                        break
                    if self._abort_requested.is_set():
                        return None
                else:
                    return None
            return self._extension_response
        finally:
            self._pending_extension_id = None
            self._extension_response = None
            self._extension_waiter.clear()

    def _dialog_timeout_ms(self, behavior: ExtensionDialogPrompt) -> int | None:
        """Return the effective extension dialog timeout for ``behavior``."""
        if self.extension_timeout_ms is not None:
            return None if self.extension_timeout_ms <= 0 else self.extension_timeout_ms
        return behavior.timeout_ms

    def _dialog_result_key(
        self, method: DialogMethod, response: JsonDict | None
    ) -> str:
        """Map a dialog response to a scenario result key."""
        if response is None:
            return "cancelled" if self._abort_requested.is_set() else "timeout"
        if response.get("cancelled") is True:
            return "cancelled"
        if method == "confirm":
            return "confirmed" if response.get("confirmed") is True else "declined"
        if "value" in response:
            return "value"
        return "cancelled"

    def _stop_active_run(self) -> None:
        """Stop and join the active worker thread, if any."""
        thread = self._run_thread
        if thread is None:
            self.state.is_streaming = False
            self._pending_steer_message = None
            self._abort_requested.clear()
            return
        self._abort_requested.set()
        self._extension_waiter.set()
        if thread.is_alive():
            thread.join(timeout=5)
        if self._run_thread is thread:
            self._run_thread = None
        self.state.is_streaming = False
        self._pending_steer_message = None
        self._abort_requested.clear()

    def _start_run(self, *, name: str, target: Callable[[], None]) -> None:
        """Start a daemon worker for prompt playback."""
        def runner() -> None:
            try:
                target()
            finally:
                if self._run_thread is thread:
                    self._run_thread = None

        thread = threading.Thread(target=runner, name=name, daemon=True)
        self._run_thread = thread
        thread.start()

    def _take_pending_steer(self) -> str | None:
        """Return and clear the queued steering message, if any."""
        message = self._pending_steer_message
        self._pending_steer_message = None
        return message

    def _finish_run(self, messages: list[JsonDict]) -> None:
        """Emit agent_end and reset transient run state."""
        self._write_json({"type": "agent_end", "messages": messages})
        self.state.is_streaming = False
        self._abort_requested.clear()
        self._pending_steer_message = None

    def _finish_aborted_run(self) -> None:
        """Finish the current run as aborted."""
        self._finish_run([])

    def _sleep_ms(self, delay_ms: int, *, abortable: bool) -> bool:
        """Sleep for ``delay_ms`` milliseconds.

        Returns ``False`` when an abort interrupt was observed.
        """
        if delay_ms <= 0:
            return not self._abort_requested.is_set()
        deadline = time.monotonic() + (delay_ms / 1000)
        while time.monotonic() < deadline:
            if abortable and self._abort_requested.is_set():
                return False
            time.sleep(min(0.01, max(0.0, deadline - time.monotonic())))
        return not (abortable and self._abort_requested.is_set())

    @staticmethod
    def _tool_result_payload(text: str) -> JsonDict:
        """Return a minimal tool result payload for tool execution events."""
        return {
            "content": [{"type": "text", "text": text}],
            "details": {"truncation": None, "fullOutputPath": None},
        }

    def _build_user_message(self, text: str) -> JsonDict:
        """Return a user message payload."""
        return {
            "role": "user",
            "content": [{"type": "text", "text": text}],
            "timestamp": now_ms(),
        }

    def _build_assistant_message(self, text: str) -> JsonDict:
        """Return an assistant message payload."""
        return {
            "role": "assistant",
            "content": [{"type": "text", "text": text}],
            "timestamp": now_ms(),
            "stopReason": "stop",
        }

    def _build_custom_message(self, text: str) -> JsonDict:
        """Return a displayable custom message payload."""
        return {
            "role": "custom",
            "display": True,
            "content": text,
            "timestamp": now_ms(),
        }

    def _persist_user_message(self, message: JsonDict) -> None:
        """Append a user message to the real session file and fork list."""
        entry_id = self._entry_id("user")
        text = message["content"][0]["text"]
        self.user_messages.append({"entryId": entry_id, "text": text})
        self._append_session_line({"type": "message", "entryId": entry_id, "message": message})
        self.state.message_count += 1

    def _persist_assistant_message(self, message: JsonDict) -> None:
        """Append an assistant message to the real session file."""
        self._append_session_line(
            {"type": "message", "entryId": self._entry_id("assistant"), "message": message}
        )
        self.state.message_count += 1

    def _persist_custom_message(self, message: JsonDict) -> None:
        """Append a custom message to the real session file."""
        self._append_session_line(
            {"type": "message", "entryId": self._entry_id("custom"), "message": message}
        )
        self.state.message_count += 1

    def _reset_session_file(self) -> None:
        """Create a fresh real session file with a session header."""
        self._message_serial = 0
        self.state.session_id = f"fake-{uuid.uuid4().hex[:8]}"
        self.state.session_file = str(self._session_root / f"{self.state.session_id}.jsonl")
        Path(self.state.session_file).write_text("", encoding="utf-8")
        self._append_session_line({"type": "session", "id": self.state.session_id})

    def _append_session_line(self, payload: JsonDict) -> None:
        """Append ``payload`` as one JSONL line to the current session file."""
        session_path = Path(self.state.session_file)
        with session_path.open("a", encoding="utf-8") as handle:
            handle.write(json.dumps(payload, ensure_ascii=False) + "\n")

    def _entry_id(self, prefix: str) -> str:
        """Return a deterministic-ish entry ID for session records."""
        self._message_serial += 1
        return f"{prefix}-{self._message_serial}"

    def _respond(self, command: JsonDict, *, data: JsonDict | None = None) -> None:
        """Emit a successful RPC response for ``command``."""
        response: JsonDict = {
            "type": "response",
            "command": command["type"],
            "success": True,
        }
        if "id" in command:
            response["id"] = command["id"]
        if data is not None:
            response["data"] = data
        self._write_json(response, split_at=self.split_responses.get(str(command["type"])))

    def _fail(self, command: JsonDict, error: str) -> None:
        """Emit a failed RPC response for ``command``."""
        response: JsonDict = {
            "type": "response",
            "command": command["type"],
            "success": False,
            "error": error,
        }
        if "id" in command:
            response["id"] = command["id"]
        self._write_json(response)

    def _write_json(self, payload: JsonDict, *, split_at: int | None = None) -> None:
        """Write one JSONL record to stdout and flush promptly."""
        line = json.dumps(payload, separators=(",", ":"), ensure_ascii=False) + "\n"
        self._log("out", payload)
        with self._write_lock:
            if split_at is not None and 0 < split_at < len(line):
                sys.stdout.write(line[:split_at])
                sys.stdout.flush()
                time.sleep(0.01)
                sys.stdout.write(line[split_at:])
                sys.stdout.flush()
            else:
                sys.stdout.write(line)
                sys.stdout.flush()

    def _log(self, direction: str, payload: JsonDict) -> None:
        """Append a debug log line when ``--log-file`` is enabled."""
        if self.log_file is None:
            return
        with self.log_file.open("a", encoding="utf-8") as handle:
            handle.write(
                json.dumps(
                    {"direction": direction, "payload": payload}, ensure_ascii=False
                )
                + "\n"
            )

    @staticmethod
    def _chunk_text(text: str, chunk_count: int) -> list[str]:
        """Split ``text`` into ``chunk_count`` non-empty chunks."""
        chunk_count = max(1, chunk_count)
        if len(text) <= chunk_count:
            return [char for char in text if char] or [text]
        base, extra = divmod(len(text), chunk_count)
        chunks: list[str] = []
        start = 0
        for index in range(chunk_count):
            size = base + (1 if index < extra else 0)
            piece = text[start : start + size]
            if piece:
                chunks.append(piece)
            start += size
        return chunks or [text]


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    """Parse command-line arguments for the fake harness."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode", default="rpc", choices=["rpc"])
    parser.add_argument("--scenario", required=True)
    parser.add_argument("--scenario-dir", default=str(default_scenario_dir()))
    parser.add_argument("--session-dir")
    parser.add_argument("--log-file")
    parser.add_argument(
        "--extension-timeout-ms",
        type=int,
        help="Override dialog timeout in milliseconds; 0 disables timeout",
    )
    parser.add_argument(
        "--split-response",
        action="append",
        default=[],
        metavar="COMMAND:INDEX",
        help="Split a response line at INDEX bytes for newline-framing tests",
    )
    return parser.parse_args(argv)


def parse_split_responses(items: list[str]) -> dict[str, int]:
    """Parse ``--split-response`` values into a command->index map."""
    result: dict[str, int] = {}
    for item in items:
        command, _, index = item.partition(":")
        if not command or not index:
            raise ValueError(f"Invalid --split-response value: {item}")
        result[command] = int(index)
    return result


def main(argv: list[str] | None = None) -> int:
    """Entry point for the fake-pi harness."""
    args = parse_args(argv)
    scenario_path = Path(args.scenario_dir) / f"{args.scenario}.json"
    try:
        scenario = load_scenario(scenario_path, args.scenario)
    except FileNotFoundError:
        print(f"fake-pi: scenario not found: {args.scenario}", file=sys.stderr)
        return 2
    except json.JSONDecodeError as exc:
        print(
            f"fake-pi: invalid JSON in scenario {args.scenario}: {exc}",
            file=sys.stderr,
        )
        return 2
    except (KeyError, TypeError, ValueError) as exc:
        print(f"fake-pi: invalid scenario {args.scenario}: {exc}", file=sys.stderr)
        return 2
    try:
        split_responses = parse_split_responses(args.split_response)
    except ValueError as exc:
        print(f"fake-pi: {exc}", file=sys.stderr)
        return 2
    harness = FakePiHarness(
        scenario=scenario,
        session_dir=args.session_dir,
        log_file=args.log_file,
        extension_timeout_ms=args.extension_timeout_ms,
        split_responses=split_responses,
    )
    return harness.run()


if __name__ == "__main__":
    raise SystemExit(main())
