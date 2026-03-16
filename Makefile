# pi-coding-agent Makefile

EMACS ?= emacs
BATCH = $(EMACS) --batch -Q -L . \
	--eval "(add-to-list 'treesit-extra-load-path (expand-file-name \"~/.emacs.d/tree-sitter\"))"
# Keep this checkout first in load-path even after package-initialize.
LOCAL_LOAD_PATH = --eval "(setq load-path (cons (expand-file-name \".\") load-path))"

# Pi CLI version — single source of truth (workflows extract this automatically)
PI_VERSION ?= 0.52.9
PI_BIN ?= .cache/pi/node_modules/.bin/pi
PI_BIN_DIR = $(abspath $(dir $(PI_BIN)))

# Test selector: run a subset of tests by ERT pattern
# Example: make test SELECTOR=fontify-buffer-tail
SELECTOR ?=

# Verbose output for tests (show full ERT output, including passed lines)
# Example: make test VERBOSE=1
VERBOSE ?=

.PHONY: test test-unit test-core test-ui test-render test-table test-input test-menu test-build
.PHONY: test-integration test-integration-fake test-integration-real test-integration-ci test-integration-ci-real test-gui test-gui-ci test-all
.PHONY: bench bench-batch
.PHONY: check check-parens compile lint lint-checkdoc lint-package clean clean-cache help
.PHONY: ollama-start ollama-stop ollama-status setup-pi install-hooks

help:
	@echo "Targets:"
	@echo "  make test             All unit tests (SELECTOR=pattern, VERBOSE=1 for full output)"
	@echo "  make test-core        Core/RPC tests only"
	@echo "  make test-ui          UI foundation tests only"
	@echo "  make test-render      Render tests only"
	@echo "  make test-table       Table decoration tests only"
	@echo "  make test-input       Input buffer tests only"
	@echo "  make test-menu        Menu/session tests only"
	@echo "  make test-build       Build/dependency helper tests only"
	@echo "  make test-unit        Compile + all unit tests"
	@echo "  make test-integration Shared integration tests (fake first, then real; local target starts Ollama for the real lane)"
	@echo "  make test-integration-fake Shared integration tests against fake backend only"
	@echo "  make test-integration-real Shared integration tests against real backend only (local target starts Ollama)"
	@echo "  make test-gui         Deterministic fake-backed GUI tests (SELECTOR=pattern; no Docker)"
	@echo "  make bench            Table rendering benchmarks (GUI via xvfb)"
	@echo "  make bench-batch      Table rendering benchmarks (batch, secondary lane)"
	@echo "  make lint             Checkdoc + package-lint"
	@echo "  make check            Compile, lint, unit tests (pre-commit)"
	@echo "  make install-hooks    Set up git pre-commit hook"
	@echo "  make clean            Remove generated files"
	@echo ""
	@echo "CI targets:"
	@echo "  make test-unit              (used by Unit Tests workflow)"
	@echo "  make lint                   (used by Lint workflow)"
	@echo "  make test-integration-ci    (CI-shaped integration run: fake lane, then real lane; expects Ollama already running)"
	@echo "  make test-integration-ci-real (real integration lane with Ollama already running)"
	@echo "  make test-gui-ci            (fake-backed GUI lane under xvfb/headless)"

# ============================================================
# Dependencies
# ============================================================

# Install package dependencies (sentinel file avoids re-running every time).
# Requirements come from pi-coding-agent.el's Package-Requires header.
# The helper upgrades built-in packages when Emacs ships an older version
# than the package requires (for example transient on Emacs 29/30).
.deps-stamp: Makefile scripts/install-deps.el scripts/pi-coding-agent-build.el pi-coding-agent.el
	@$(BATCH) -L scripts -l scripts/install-deps.el
	@touch $@

deps: .deps-stamp

# ============================================================
# Unit tests
# ============================================================

SHELL = /bin/bash
MAKE_SELECTOR = $(if $(SELECTOR),SELECTOR='$(SELECTOR)',)
GUI_SELECTOR_ARG = $(if $(SELECTOR),$(SELECTOR),)

test: .deps-stamp
	@echo "=== Unit Tests ==="
	@set -o pipefail; \
	OUTPUT=$$(mktemp); \
	$(BATCH) -L test \
		--eval "(setq load-prefer-newer t)" \
		--eval "(require 'package)" \
		--eval "(package-initialize)" \
		$(LOCAL_LOAD_PATH) \
		-l pi-coding-agent \
		-l pi-coding-agent-core-test \
		-l pi-coding-agent-ui-test \
		-l pi-coding-agent-render-test \
		-l pi-coding-agent-table-test \
		-l pi-coding-agent-input-test \
		-l pi-coding-agent-menu-test \
		-l pi-coding-agent-build-test \
		-l pi-coding-agent-fake-pi-test \
		-l pi-coding-agent-gui-test-utils-test \
		-l pi-coding-agent-integration-test-common-test \
		-l pi-coding-agent-test \
		$(if $(SELECTOR),--eval '(ert-run-tests-batch-and-exit "$(SELECTOR)")',-f ert-run-tests-batch-and-exit) \
		>$$OUTPUT 2>&1; \
	STATUS=$$?; \
	if [ "$(VERBOSE)" = "1" ] || [ $$STATUS -ne 0 ]; then \
		cat $$OUTPUT; \
	else \
		grep -v "^   passed\|^Pi: \|^Running [0-9]\|^$$" $$OUTPUT; \
	fi; \
	rm -f $$OUTPUT; \
	exit $$STATUS

# Per-module test targets: run tests for a single module in isolation.
# Usage: make test-render (much faster than `make test` during development)
BATCH_TEST = $(BATCH) -L test --eval "(setq load-prefer-newer t)" \
	--eval "(require 'package)" --eval "(package-initialize)" \
	$(LOCAL_LOAD_PATH) \
	-l pi-coding-agent

test-core: .deps-stamp
	@$(BATCH_TEST) -l pi-coding-agent-core-test -f ert-run-tests-batch-and-exit
test-ui: .deps-stamp
	@$(BATCH_TEST) -l pi-coding-agent-ui-test -f ert-run-tests-batch-and-exit
test-render: .deps-stamp
	@$(BATCH_TEST) -l pi-coding-agent-render-test -f ert-run-tests-batch-and-exit
test-table: .deps-stamp
	@$(BATCH_TEST) -l pi-coding-agent-table-test -f ert-run-tests-batch-and-exit
test-input: .deps-stamp
	@$(BATCH_TEST) -l pi-coding-agent-input-test -f ert-run-tests-batch-and-exit
test-menu: .deps-stamp
	@$(BATCH_TEST) -l pi-coding-agent-menu-test -f ert-run-tests-batch-and-exit

test-build: .deps-stamp
	@$(BATCH_TEST) -l pi-coding-agent-build-test -f ert-run-tests-batch-and-exit

test-unit: compile test

# ============================================================
# Setup helpers
# ============================================================

install-hooks:
	@git config core.hooksPath hooks
	@echo "Git hooks installed (using hooks/)"

setup-pi:
	@if [ -x "$(PI_BIN)" ]; then \
		CURRENT=$$($(PI_BIN) --version 2>/dev/null); \
		if [ "$$CURRENT" != "$(PI_VERSION)" ] && [ "$(PI_VERSION)" != "latest" ]; then \
			echo "Cached pi@$$CURRENT differs from requested $(PI_VERSION), reinstalling..."; \
			rm -rf .cache/pi; \
		fi; \
	fi
	@if [ ! -x "$(PI_BIN)" ]; then \
		echo "Installing pi@$(PI_VERSION) to .cache/pi/..."; \
		rm -rf .cache/pi; \
		npm install --prefix .cache/pi @mariozechner/pi-coding-agent@$(PI_VERSION) --silent; \
	fi
	@echo "Using pi: $(PI_BIN)"
	@$(PI_BIN) --version || (echo "ERROR: pi not working"; exit 1)

# ============================================================
# Integration tests
# ============================================================

INTEGRATION_BATCH = $(BATCH) -L test \
	--eval "(setq load-prefer-newer t)" \
	--eval "(require 'package)" \
	--eval "(package-initialize)" \
	$(LOCAL_LOAD_PATH) \
	-l pi-coding-agent -l pi-coding-agent-integration-test \
	$(if $(SELECTOR),--eval '(ert-run-tests-batch-and-exit "$(SELECTOR)")',-f ert-run-tests-batch-and-exit)
# Reuse CI's session directory when provided, but stay locally runnable by
# creating and cleaning up a temporary session directory otherwise.
REAL_INTEGRATION_RUN = \
	SESSION_DIR="$$PI_CODING_AGENT_DIR"; \
	CLEANUP_SESSION_DIR=0; \
	if [ -z "$$SESSION_DIR" ]; then \
		SESSION_DIR=$$(mktemp -d); \
		CLEANUP_SESSION_DIR=1; \
	else \
		mkdir -p "$$SESSION_DIR"; \
	fi; \
	cp test/fixtures/ollama-models.json "$$SESSION_DIR/models.json"; \
	env PATH="$(PI_BIN_DIR):$$PATH" PI_CODING_AGENT_DIR="$$SESSION_DIR" PI_RUN_INTEGRATION=1 PI_INTEGRATION_BACKENDS=real \
		$(INTEGRATION_BATCH); \
	status=$$?; \
	if [ "$$CLEANUP_SESSION_DIR" = "1" ]; then rm -rf "$$SESSION_DIR"; fi; \
	exit $$status

# Local default: fake lane first, then the slower real compatibility lane.
test-integration:
	@$(MAKE) --no-print-directory test-integration-fake $(MAKE_SELECTOR)
	@$(MAKE) --no-print-directory test-integration-real $(MAKE_SELECTOR)

# Local: fake backend only (no pi install or Ollama needed)
test-integration-fake: .deps-stamp
	@echo "=== Integration Tests (fake backend only) ==="
	@env PI_RUN_INTEGRATION=1 PI_INTEGRATION_BACKENDS=fake \
		$(INTEGRATION_BATCH)

# Local: real backend only
test-integration-real: .deps-stamp setup-pi
	@echo "=== Integration Tests (real backend only, pi@$(PI_VERSION)) ==="
	@./scripts/ollama.sh start
	@$(REAL_INTEGRATION_RUN)

# CI-shaped default: fast fake lane first, then the real backend lane.
test-integration-ci:
	@$(MAKE) --no-print-directory test-integration-fake $(MAKE_SELECTOR)
	@$(MAKE) --no-print-directory test-integration-ci-real $(MAKE_SELECTOR)

# CI: Ollama already running via services block for the real lane.
test-integration-ci-real: .deps-stamp setup-pi
	@echo "=== Integration Tests CI (real backend only, pi@$(PI_VERSION)) ==="
	@$(REAL_INTEGRATION_RUN)

# ============================================================
# GUI tests
# ============================================================

# Local: deterministic fake-backed GUI regressions (no Docker or pi install).
test-gui: .deps-stamp
	@echo "=== GUI Tests (fake backend only) ==="
	@./test/run-gui-tests.sh $(GUI_SELECTOR_ARG)

# CI: same fake-backed suite under xvfb/headless.
test-gui-ci: .deps-stamp
	@echo "=== GUI Tests CI (fake backend only) ==="
	@PI_HEADLESS=1 ./test/run-gui-tests.sh --headless $(GUI_SELECTOR_ARG)

# ============================================================
# All tests
# ============================================================

test-all: test test-integration test-gui

# ============================================================
# Benchmarks
# ============================================================

# Primary lane: GUI via xvfb (realistic string-width / font metrics).
bench: .deps-stamp
	@./bench/run-bench.sh

# Secondary lane: batch mode (faster, no font engine).
bench-batch: .deps-stamp
	@./bench/run-bench.sh --batch

# ============================================================
# Ollama management (local development)
# ============================================================

ollama-start:
	@./scripts/ollama.sh start

ollama-stop:
	@./scripts/ollama.sh stop

ollama-status:
	@./scripts/ollama.sh status

# ============================================================
# Code quality
# ============================================================

check-parens:
	@echo "=== Check Parens ==="
	@OUTPUT=$$($(BATCH) --eval '(condition-case err (dolist (f (list "scripts/pi-coding-agent-build.el" "scripts/install-deps.el" "scripts/install-ts-grammars.el" "pi-coding-agent-core.el" "pi-coding-agent-grammars.el" "pi-coding-agent-ui.el" "pi-coding-agent-table.el" "pi-coding-agent-render.el" "pi-coding-agent-input.el" "pi-coding-agent-menu.el" "pi-coding-agent.el")) (with-current-buffer (find-file-noselect f) (check-parens) (message "%s OK" f))) (user-error (message "FAIL: %s" (error-message-string err)) (kill-emacs 1)))' 2>&1); \
	echo "$$OUTPUT" | grep -E "OK$$|FAIL:"; \
	echo "$$OUTPUT" | grep -q "FAIL:" && exit 1 || true

compile: .deps-stamp
	@rm -f *.elc scripts/*.elc
	@echo "=== Byte-compile ==="
	@$(BATCH) -L scripts \
		--eval "(require 'package)" \
		--eval "(package-initialize)" \
		$(LOCAL_LOAD_PATH) \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile scripts/pi-coding-agent-build.el scripts/install-deps.el scripts/install-ts-grammars.el pi-coding-agent-core.el pi-coding-agent-grammars.el pi-coding-agent-ui.el pi-coding-agent-table.el pi-coding-agent-render.el pi-coding-agent-input.el pi-coding-agent-menu.el pi-coding-agent.el

lint: lint-checkdoc lint-package

lint-checkdoc:
	@echo "=== Checkdoc ==="
	@OUTPUT=$$($(BATCH) \
		--eval "(require 'checkdoc)" \
		--eval "(setq sentence-end-double-space nil)" \
		--eval "(checkdoc-file \"scripts/pi-coding-agent-build.el\")" \
		--eval "(checkdoc-file \"scripts/install-deps.el\")" \
		--eval "(checkdoc-file \"scripts/install-ts-grammars.el\")" \
		--eval "(checkdoc-file \"pi-coding-agent-core.el\")" \
		--eval "(checkdoc-file \"pi-coding-agent-grammars.el\")" \
		--eval "(checkdoc-file \"pi-coding-agent-ui.el\")" \
		--eval "(checkdoc-file \"pi-coding-agent-table.el\")" \
		--eval "(checkdoc-file \"pi-coding-agent-render.el\")" \
		--eval "(checkdoc-file \"pi-coding-agent-input.el\")" \
		--eval "(checkdoc-file \"pi-coding-agent-menu.el\")" \
		--eval "(checkdoc-file \"pi-coding-agent.el\")" 2>&1); \
	WARNINGS=$$(echo "$$OUTPUT" | grep -A1 "^Warning" | grep -v "^Warning\|^--$$"); \
	if [ -n "$$WARNINGS" ]; then echo "$$WARNINGS"; exit 1; else echo "OK"; fi

lint-package:
	@echo "=== Package-lint ==="
	@$(BATCH) \
		--eval "(require 'package)" \
		--eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)" \
		--eval "(package-initialize)" \
		--eval "(unless (package-installed-p 'package-lint) \
		          (package-refresh-contents) \
		          (package-install 'package-lint))" \
		--eval "(require 'package-lint)" \
		--eval "(setq package-lint-main-file \"pi-coding-agent.el\")" \
		-f package-lint-batch-and-exit pi-coding-agent.el pi-coding-agent-ui.el pi-coding-agent-table.el pi-coding-agent-render.el pi-coding-agent-input.el pi-coding-agent-menu.el pi-coding-agent-core.el pi-coding-agent-grammars.el

check: compile lint test

# ============================================================
# Cleanup
# ============================================================

clean:
	@rm -f *.elc scripts/*.elc test/*.elc .deps-stamp

clean-cache:
	@./scripts/ollama.sh stop 2>/dev/null || true
	@rm -rf .cache
