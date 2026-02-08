# Decompose pi-coding-agent.el into Modules

*Design rationale for the module decomposition.  For the current
architecture and file layout, see AGENTS.md.*

## Problem

A single 4,271-line file (`pi-coding-agent.el`) containing 190 functions,
63 variables, and 11 faces was the entire UI layer.  A new developer
saw two `.el` files and had no structural cue about what does what.

The code was well-sectioned internally (31 `;;;;` sections), but those
sections were invisible from the outside.

## Design Principles

1. **File names are documentation** — a newcomer should guess correctly
2. **No circular requires** — strict dependency direction
3. **Cut at the seams** — split where coupling is already low
4. **Tests mirror source** — test files parallel the source structure
5. **MELPA-compatible** — subsidiary files follow Emacs multi-file
   package conventions (like magit does)
6. **Purely structural** — no behavioral changes; every test must pass
   at every step
7. **Explicit state boundaries** — variables mutated from multiple
   modules get accessor functions in `ui.el`

## Static Analysis

A call-graph analysis revealed the natural module boundaries:

- **Buffer-Local Session Variables** are the gravity well: referenced
  39× from Response Display, 23× from Transient Menu, 15× from
  Streaming Fontification
- **Response Display** (34 funcs) and **Tool Output** (23 funcs) are
  deeply coupled: 9 cross-calls each way, sharing markers and overlays
- **Transient Menu** (38 funcs, 786 lines) is nearly self-contained
- **Editor features** (history, isearch, @-completion, path completion,
  queuing) are input-buffer concerns with minimal inbound dependencies
- **Streaming Fontification** section was misnamed: it also contained
  header-line formatting, spinner, stats display — really "UI infrastructure"

## MELPA Conventions for Subsidiary Files

Following magit's pattern:

- **Main file** (`pi-coding-agent.el`): Has `Package-Requires`, `Version`,
  `;;;###autoload` cookies
- **Subsidiary files**: Have `;;; file.el --- Description` line, copyright,
  `Commentary`, proper `(provide)`, but no `Package-Requires` or `Version`
- **package-lint**: Configured with
  `package-lint-main-file "pi-coding-agent.el"` in the Makefile
