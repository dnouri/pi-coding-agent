#!/usr/bin/env bash
# run-reload-resume-bench.sh - Run pi-coding-agent reload/resume benchmarks
#
# Usage:
#   ./bench/run-reload-resume-bench.sh                         # GUI via xvfb (primary lane)
#   ./bench/run-reload-resume-bench.sh --batch                 # batch mode (secondary lane)
#   ./bench/run-reload-resume-bench.sh -c 3                    # 3 repetitions per scenario
#   ./bench/run-reload-resume-bench.sh --scenario smoke -c 1   # cheap correctness smoke
#   ./bench/run-reload-resume-bench.sh --scenarios a,b         # comma-separated scenarios
#   ./bench/run-reload-resume-bench.sh --out-dir tmp/rr-bench  # write artifacts elsewhere
#   ./bench/run-reload-resume-bench.sh --timings               # add diagnostic advice data
#
# The primary lane uses xvfb-run for GUI Emacs, so interactive buffers render
# under a virtual display instead of popping up locally.  Batch numbers are a
# faster secondary lane, but GUI is closer to real reload/resume behavior.
# The script fails on correctness failures, not on timing thresholds.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

BATCH=0
REPS=5
TIMINGS=0
OUT_DIR="$PROJECT_DIR/tmp/reload-resume-bench"
SCENARIOS=()

usage() {
    sed -n '2,16p' "$0" | sed 's/^# \{0,1\}//'
}

require_arg() {
    local opt="$1"
    if [[ $# -lt 2 || -z "${2:-}" ]]; then
        echo "ERROR: $opt requires an argument" >&2
        usage >&2
        exit 1
    fi
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --batch) BATCH=1; shift ;;
        --timings) TIMINGS=1; shift ;;
        --no-timings) TIMINGS=0; shift ;;
        -c|--count)
            require_arg "$1" "${2:-}"
            REPS="$2"
            shift 2
            ;;
        --out-dir)
            require_arg "$1" "${2:-}"
            OUT_DIR="$2"
            shift 2
            ;;
        --scenario)
            require_arg "$1" "${2:-}"
            SCENARIOS+=("$2")
            shift 2
            ;;
        --scenarios)
            require_arg "$1" "${2:-}"
            IFS=',' read -r -a SCENARIOS <<< "$2"
            shift 2
            ;;
        -h|--help) usage; exit 0 ;;
        *) echo "Unknown argument: $1" >&2; usage >&2; exit 1 ;;
    esac
done

if ! [[ "$REPS" =~ ^[0-9]+$ ]] || [[ "$REPS" -lt 1 ]]; then
    echo "ERROR: repetition count must be a positive integer: $REPS" >&2
    exit 1
fi

if [[ ${#SCENARIOS[@]} -eq 0 ]]; then
    SCENARIOS=(rpc-xlarge metadata-heavy balanced-render)
fi

case "$OUT_DIR" in
    /*) ;;
    *) OUT_DIR="$PWD/$OUT_DIR" ;;
esac
if [[ -z "$OUT_DIR" || "$OUT_DIR" == "/" ]]; then
    echo "ERROR: refusing unsafe output directory: $OUT_DIR" >&2
    exit 1
fi

BENCH_MARKER="$OUT_DIR/.pi-coding-agent-reload-resume-bench"
if [[ -e "$OUT_DIR" && ! -d "$OUT_DIR" ]]; then
    echo "ERROR: refusing to replace non-directory output path: $OUT_DIR" >&2
    exit 1
fi
if [[ -d "$OUT_DIR" && ! -f "$BENCH_MARKER" ]] && find "$OUT_DIR" -mindepth 1 -maxdepth 1 -print -quit | grep -q .; then
    echo "ERROR: refusing to remove non-empty output directory without benchmark marker: $OUT_DIR" >&2
    exit 1
fi

scenario_env() {
    case "$1" in
        smoke)
            cat <<'EOF'
PI_RR_BENCH_TURNS=12
PI_RR_BENCH_OTHER_SESSIONS=3
PI_RR_BENCH_OTHER_TURNS=4
PI_RR_BENCH_TOOL_EVERY=2
PI_RR_BENCH_TABLE_EVERY=6
PI_RR_BENCH_THINKING_EVERY=6
PI_RR_BENCH_TEXT_BYTES=0
PI_RR_BENCH_WIRE_BYTES=0
PI_RR_BENCH_TOOL_OUTPUT_LINES=12
PI_RR_BENCH_TIMEOUT_SECONDS=30
EOF
            ;;
        rpc-xlarge)
            cat <<'EOF'
PI_RR_BENCH_TURNS=180
PI_RR_BENCH_OTHER_SESSIONS=0
PI_RR_BENCH_OTHER_TURNS=0
PI_RR_BENCH_TOOL_EVERY=0
PI_RR_BENCH_TABLE_EVERY=0
PI_RR_BENCH_THINKING_EVERY=0
PI_RR_BENCH_TEXT_BYTES=0
PI_RR_BENCH_WIRE_BYTES=40000
PI_RR_BENCH_TOOL_OUTPUT_LINES=0
PI_RR_BENCH_TIMEOUT_SECONDS=300
EOF
            ;;
        metadata-heavy)
            cat <<'EOF'
PI_RR_BENCH_TURNS=80
PI_RR_BENCH_OTHER_SESSIONS=80
PI_RR_BENCH_OTHER_TURNS=300
PI_RR_BENCH_TOOL_EVERY=0
PI_RR_BENCH_TABLE_EVERY=0
PI_RR_BENCH_THINKING_EVERY=0
PI_RR_BENCH_TEXT_BYTES=1024
PI_RR_BENCH_WIRE_BYTES=0
PI_RR_BENCH_TOOL_OUTPUT_LINES=0
PI_RR_BENCH_TIMEOUT_SECONDS=360
EOF
            ;;
        balanced-render)
            cat <<'EOF'
PI_RR_BENCH_TURNS=120
PI_RR_BENCH_OTHER_SESSIONS=20
PI_RR_BENCH_OTHER_TURNS=20
PI_RR_BENCH_TOOL_EVERY=1
PI_RR_BENCH_TABLE_EVERY=10
PI_RR_BENCH_THINKING_EVERY=5
PI_RR_BENCH_TEXT_BYTES=0
PI_RR_BENCH_WIRE_BYTES=0
PI_RR_BENCH_TOOL_OUTPUT_LINES=24
PI_RR_BENCH_TIMEOUT_SECONDS=180
EOF
            ;;
        *) echo "Unknown scenario: $1" >&2; exit 1 ;;
    esac
}

EMACS_INIT=(
    -Q -L "$PROJECT_DIR"
    --eval "(setq inhibit-startup-screen t)"
    --eval "(require 'package)"
    --eval "(package-initialize)"
    --eval "(setq load-path (cons (expand-file-name \"$PROJECT_DIR\") load-path))"
    -l "$SCRIPT_DIR/pi-coding-agent-reload-resume-bench.el"
)

rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"
touch "$BENCH_MARKER"

printf '=== pi-coding-agent Reload/Resume Benchmarks ===\n'
printf 'Project: %s\n' "$PROJECT_DIR"
if [[ "$BATCH" = "1" ]]; then
    MODE="batch"
    printf 'Mode: batch (secondary lane), %s reps\n' "$REPS"
else
    MODE="gui-xvfb"
    printf 'Mode: GUI via xvfb (primary lane), %s reps\n' "$REPS"
    if ! command -v xvfb-run >/dev/null 2>&1; then
        echo "ERROR: xvfb-run not found. Install xvfb or use --batch." >&2
        exit 1
    fi
fi
printf 'Diagnostic timings: %s\n' "$([[ "$TIMINGS" = "1" ]] && echo enabled || echo disabled)"
printf 'Scenarios: %s\n\n' "${SCENARIOS[*]}"

for scenario in "${SCENARIOS[@]}"; do
    for ((iter = 1; iter <= REPS; iter++)); do
        run_dir="$OUT_DIR/$scenario/iter-$(printf '%02d' "$iter")"
        fixture_root="$run_dir/fixtures"
        mkdir -p "$run_dir"
        env_file="$run_dir/env"
        scenario_env "$scenario" > "$env_file"
        set -a
        # shellcheck disable=SC1090
        source "$env_file"
        set +a
        export PI_RR_BENCH_SCENARIO="$scenario"
        export PI_RR_BENCH_VARIANT="current"
        export PI_RR_BENCH_ITERATION="$iter"
        export PI_RR_BENCH_OUT_DIR="$run_dir"
        export PI_RR_BENCH_RUNNER_OUT_DIR="$OUT_DIR"
        export PI_RR_BENCH_FIXTURE_ROOT="$fixture_root"
        export PI_RR_BENCH_DISPLAY=$([[ "$BATCH" = "1" ]] && echo 0 || echo 1)
        export PI_RR_BENCH_TIMINGS="$TIMINGS"

        printf '[%s/%s] running\n' "$scenario" "$iter"
        if [[ "$BATCH" = "1" ]]; then
            if ! emacs --batch "${EMACS_INIT[@]}" \
                -f pi-coding-agent-rr-bench-run-batch \
                > "$run_dir/stdout.log" 2> "$run_dir/stderr.log"; then
                cat "$run_dir/stdout.log"
                cat "$run_dir/stderr.log" >&2
                exit 1
            fi
        else
            if ! xvfb-run -a env GDK_BACKEND=x11 PATH="$PATH" \
                emacs --geometry 120x40 "${EMACS_INIT[@]}" \
                --eval "(let ((standard-output #'external-debugging-output)) (kill-emacs (if (pi-coding-agent-rr-bench-run) 0 1)))" \
                </dev/null > "$run_dir/stdout.log" 2> "$run_dir/stderr.log"; then
                cat "$run_dir/stdout.log"
                cat "$run_dir/stderr.log" >&2
                exit 1
            fi
        fi
    done
done

python3 - "$OUT_DIR" "$MODE" "$REPS" "$TIMINGS" "${SCENARIOS[*]}" <<'PY'
from __future__ import annotations

import csv
import json
import statistics
import sys
from pathlib import Path
from typing import Any

out = Path(sys.argv[1])
mode = sys.argv[2]
reps = sys.argv[3]
timings = sys.argv[4] == "1"
scenarios_arg = sys.argv[5]
rows: list[dict[str, Any]] = []

for result_path in sorted(out.glob("*/iter-*/result.json")):
    with result_path.open(encoding="utf-8") as handle:
        result = json.load(handle)
    workload = result.get("workload", {})
    rpc_bytes = result.get("rpc", {}).get("getMessagesBytes", [])
    get_messages_bytes = rpc_bytes[0] if rpc_bytes else None
    for op in result.get("results", []):
        rows.append({
            "scenario": result.get("scenario"),
            "iteration": result.get("iteration"),
            "operation": op.get("name"),
            "ok": op.get("ok") is True,
            "seconds": op.get("seconds"),
            "gcs": op.get("gcs"),
            "gcSeconds": op.get("gcSeconds"),
            "bufferBytes": op.get("bufferBytes"),
            "bufferLines": op.get("bufferLines"),
            "targetBytes": workload.get("target", {}).get("bytes"),
            "sessionFileCount": workload.get("sessionFileCount"),
            "getMessagesBytes": get_messages_bytes,
            "error": op.get("error") or "",
            "resultPath": str(result_path),
        })

csv_path = out / "summary.csv"
if rows:
    with csv_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)

summary_lines: list[str] = []
summary_lines.append("# pi-coding-agent reload/resume benchmark summary")
summary_lines.append("")
summary_lines.append("Synthetic deterministic fixtures only; no private session content is used.")
summary_lines.append("")
summary_lines.append(f"- Mode: `{mode}`")
summary_lines.append(f"- Repetitions per scenario: `{reps}`")
summary_lines.append(f"- Scenarios: `{scenarios_arg}`")
summary_lines.append(f"- Diagnostic timing advice: `{'enabled' if timings else 'disabled'}`")
summary_lines.append("- Timing thresholds: `none` (correctness failures still fail the run)")
summary_lines.append("")
summary_lines.append("| scenario | operation | min | median | max | GC time | target bytes | sessions | successful runs |")
summary_lines.append("|---|---|---:|---:|---:|---:|---:|---:|---:|")

print("\nsummary")
print("scenario          operation     min      median       max  GC-time  target bytes sessions ok")
print("--------          ---------     ---      ------       ---  -------  ------------ -------- --")

failed = [row for row in rows if not row["ok"]]
for scenario in sorted({str(row["scenario"]) for row in rows}):
    for operation in ("resume", "reload"):
        subset_all = [row for row in rows if str(row["scenario"]) == scenario and row["operation"] == operation]
        subset = [row for row in subset_all if row["ok"]]
        if not subset:
            print(f"{scenario:<17} {operation:<9} no successful runs")
            summary_lines.append(f"| {scenario} | {operation} | n/a | n/a | n/a | n/a | n/a | n/a | 0/{len(subset_all)} |")
            continue
        times = sorted(float(row["seconds"]) for row in subset)
        gc_time = sum(float(row["gcSeconds"] or 0.0) for row in subset)
        median = statistics.median(times)
        target = subset[0]["targetBytes"]
        sessions = subset[0]["sessionFileCount"]
        ok_count = f"{len(subset)}/{len(subset_all)}"
        print(
            f"{scenario:<17} {operation:<9} "
            f"{times[0]*1000:7.1f}ms {median*1000:9.1f}ms {times[-1]*1000:7.1f}ms "
            f"{gc_time*1000:7.1f}ms {target:12} {sessions:8} {ok_count}"
        )
        summary_lines.append(
            f"| {scenario} | {operation} | {times[0]:.3f}s | {median:.3f}s | {times[-1]:.3f}s | "
            f"{gc_time:.3f}s | {target} | {sessions} | {ok_count} |"
        )

summary_lines.append("")
summary_lines.append("## Artifacts")
summary_lines.append("")
summary_lines.append(f"- CSV: `{csv_path}`")
summary_lines.append("- Per-run reports: `SCENARIO/iter-NN/report.md`")
summary_lines.append("- Per-run JSON: `SCENARIO/iter-NN/result.json`")
summary_lines.append("- Per-run timing TSV: `SCENARIO/iter-NN/times.tsv`")

if failed:
    summary_lines.append("")
    summary_lines.append("## Correctness failures")
    summary_lines.append("")
    for row in failed:
        summary_lines.append(
            f"- {row['scenario']} iter {row['iteration']} {row['operation']}: {row['error'] or 'operation did not settle'} "
            f"({row['resultPath']})"
        )

summary_path = out / "summary.md"
summary_path.write_text("\n".join(summary_lines) + "\n", encoding="utf-8")

print(f"\nWrote {csv_path}")
print(f"Wrote {summary_path}")

if not rows:
    print("ERROR: no benchmark result rows found", file=sys.stderr)
    raise SystemExit(1)
if failed:
    print("ERROR: one or more reload/resume correctness checks failed", file=sys.stderr)
    raise SystemExit(1)
PY
