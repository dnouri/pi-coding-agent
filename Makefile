# pi-coding-agent Makefile

EMACS ?= emacs
BATCH = $(EMACS) --batch -Q -L .
# Keep this checkout first in load-path even after package-initialize.
LOCAL_LOAD_PATH = --eval "(setq load-path (cons (expand-file-name \".\") load-path))"

# Pi CLI version — single source of truth (workflows extract this automatically)
PI_VERSION ?= 0.52.9
PI_BIN ?= .cache/pi/node_modules/.bin/pi
PI_BIN_DIR = $(abspath $(dir $(PI_BIN)))

# Test selector: run a subset of tests by ERT pattern
# Example: make test SELECTOR=fontify-buffer-tail
SELECTOR ?=

.PHONY: test test-unit test-core test-ui test-render test-input test-menu
.PHONY: test-integration test-integration-ci test-gui test-gui-ci test-all
.PHONY: check check-parens compile lint lint-checkdoc lint-package clean clean-cache help
.PHONY: ollama-start ollama-stop ollama-status setup-pi setup-models install-hooks

help:
	@echo "Targets:"
	@echo "  make test             All unit tests (use SELECTOR=pattern to filter)"
	@echo "  make test-core        Core/RPC tests only"
	@echo "  make test-ui          UI foundation tests only"
	@echo "  make test-render      Render tests only"
	@echo "  make test-input       Input buffer tests only"
	@echo "  make test-menu        Menu/session tests only"
	@echo "  make test-unit        Compile + all unit tests"
	@echo "  make test-integration Integration tests (local, starts Ollama)"
	@echo "  make test-gui         GUI tests (local, starts Ollama)"
	@echo "  make lint             Checkdoc + package-lint"
	@echo "  make check            Compile, lint, unit tests (pre-commit)"
	@echo "  make install-hooks    Set up git pre-commit hook"
	@echo "  make clean            Remove generated files"
	@echo ""
	@echo "CI targets:"
	@echo "  make test-unit           (used by Unit Tests workflow)"
	@echo "  make lint                (used by Lint workflow)"
	@echo "  make test-integration-ci (Ollama already running)"
	@echo "  make test-gui-ci         (Ollama already running)"

# ============================================================
# Dependencies
# ============================================================

# Install package dependencies (sentinel file avoids re-running every time).
# Note: We need transient 0.9.0+ (3-element group vector format).
# Emacs 30 ships 0.7.2, so all Emacs versions need MELPA transient.
.deps-stamp: Makefile
	@$(BATCH) \
		--eval "(require 'package)" \
		--eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(unless (package-installed-p 'markdown-mode) \
		          (package-install 'markdown-mode))" \
		--eval "(package-install (cadr (assq 'transient package-archive-contents)))" \
		--eval "(message \"Dependencies installed\")" 2>&1 \
		| grep -E "^Dependencies installed$$|^Error:" || true
	@touch $@

deps: .deps-stamp

# ============================================================
# Unit tests
# ============================================================

SHELL = /bin/bash

test: .deps-stamp
	@echo "=== Unit Tests ==="
	@set -o pipefail; $(BATCH) -L test \
		--eval "(setq load-prefer-newer t)" \
		--eval "(require 'package)" \
		--eval "(package-initialize)" \
		$(LOCAL_LOAD_PATH) \
		-l pi-coding-agent \
		-l pi-coding-agent-core-test \
		-l pi-coding-agent-ui-test \
		-l pi-coding-agent-render-test \
		-l pi-coding-agent-input-test \
		-l pi-coding-agent-menu-test \
		-l pi-coding-agent-test \
		$(if $(SELECTOR),--eval '(ert-run-tests-batch-and-exit "$(SELECTOR)")',-f ert-run-tests-batch-and-exit) 2>&1 \
		| grep -v "^   passed\|^Pi: \|^Running [0-9]\|^$$"

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
test-input: .deps-stamp
	@$(BATCH_TEST) -l pi-coding-agent-input-test -f ert-run-tests-batch-and-exit
test-menu: .deps-stamp
	@$(BATCH_TEST) -l pi-coding-agent-menu-test -f ert-run-tests-batch-and-exit

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

# Setup models.json - uses PI_CODING_AGENT_DIR if set, else temp dir
setup-models:
	@if [ -z "$$PI_CODING_AGENT_DIR" ]; then \
		export PI_CODING_AGENT_DIR=$$(mktemp -d); \
		echo "PI_CODING_AGENT_DIR=$$PI_CODING_AGENT_DIR"; \
	fi; \
	mkdir -p "$$PI_CODING_AGENT_DIR"; \
	cp test/fixtures/ollama-models.json "$$PI_CODING_AGENT_DIR/models.json"

# ============================================================
# Integration tests
# ============================================================

# Local: starts Ollama via Docker
test-integration: clean .deps-stamp setup-pi
	@echo "=== Integration Tests (pi@$(PI_VERSION)) ==="
	@./scripts/ollama.sh start
	@PI_CODING_AGENT_DIR=$$(mktemp -d) && \
		cp test/fixtures/ollama-models.json "$$PI_CODING_AGENT_DIR/models.json" && \
		env PATH="$(PI_BIN_DIR):$$PATH" PI_CODING_AGENT_DIR="$$PI_CODING_AGENT_DIR" PI_RUN_INTEGRATION=1 \
		$(BATCH) -L test \
			--eval "(require 'package)" \
			--eval "(package-initialize)" \
			$(LOCAL_LOAD_PATH) \
			-l pi-coding-agent -l pi-coding-agent-integration-test -f ert-run-tests-batch-and-exit; \
		status=$$?; rm -rf "$$PI_CODING_AGENT_DIR"; exit $$status

# CI: Ollama already running via services block
test-integration-ci: clean .deps-stamp setup-pi
	@echo "=== Integration Tests CI (pi@$(PI_VERSION)) ==="
	@mkdir -p "$$PI_CODING_AGENT_DIR"
	@cp test/fixtures/ollama-models.json "$$PI_CODING_AGENT_DIR/models.json"
	env PATH="$(PI_BIN_DIR):$$PATH" PI_RUN_INTEGRATION=1 \
	$(BATCH) -L test \
		--eval "(require 'package)" \
		--eval "(package-initialize)" \
		$(LOCAL_LOAD_PATH) \
		-l pi-coding-agent -l pi-coding-agent-integration-test -f ert-run-tests-batch-and-exit

# ============================================================
# GUI tests
# ============================================================

# Local: starts Ollama via Docker
test-gui: clean .deps-stamp setup-pi
	@echo "=== GUI Tests (pi@$(PI_VERSION)) ==="
	@./scripts/ollama.sh start
	@PI_CODING_AGENT_DIR=$$(mktemp -d) && \
		cp test/fixtures/ollama-models.json "$$PI_CODING_AGENT_DIR/models.json" && \
		env PATH="$(PI_BIN_DIR):$$PATH" PI_CODING_AGENT_DIR="$$PI_CODING_AGENT_DIR" \
		./test/run-gui-tests.sh; \
		status=$$?; rm -rf "$$PI_CODING_AGENT_DIR"; exit $$status

# CI: Ollama already running via services block
test-gui-ci: clean .deps-stamp setup-pi
	@echo "=== GUI Tests CI (pi@$(PI_VERSION)) ==="
	@mkdir -p "$$PI_CODING_AGENT_DIR"
	@cp test/fixtures/ollama-models.json "$$PI_CODING_AGENT_DIR/models.json"
	env PATH="$(PI_BIN_DIR):$$PATH" ./test/run-gui-tests.sh

# ============================================================
# All tests
# ============================================================

test-all: test test-integration test-gui

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
	@OUTPUT=$$($(BATCH) --eval '(condition-case err (dolist (f (list "pi-coding-agent-core.el" "pi-coding-agent-ui.el" "pi-coding-agent-render.el" "pi-coding-agent-input.el" "pi-coding-agent-menu.el" "pi-coding-agent.el")) (with-current-buffer (find-file-noselect f) (check-parens) (message "%s OK" f))) (user-error (message "FAIL: %s" (error-message-string err)) (kill-emacs 1)))' 2>&1); \
	echo "$$OUTPUT" | grep -E "OK$$|FAIL:"; \
	echo "$$OUTPUT" | grep -q "FAIL:" && exit 1 || true

compile: .deps-stamp
	@rm -f *.elc
	@echo "=== Byte-compile ==="
	@$(BATCH) \
		--eval "(require 'package)" \
		--eval "(package-initialize)" \
		$(LOCAL_LOAD_PATH) \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile pi-coding-agent-core.el pi-coding-agent-ui.el pi-coding-agent-render.el pi-coding-agent-input.el pi-coding-agent-menu.el pi-coding-agent.el

lint: lint-checkdoc lint-package

lint-checkdoc:
	@echo "=== Checkdoc ==="
	@OUTPUT=$$($(BATCH) \
		--eval "(require 'checkdoc)" \
		--eval "(setq sentence-end-double-space nil)" \
		--eval "(checkdoc-file \"pi-coding-agent-core.el\")" \
		--eval "(checkdoc-file \"pi-coding-agent-ui.el\")" \
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
		-f package-lint-batch-and-exit pi-coding-agent.el pi-coding-agent-ui.el pi-coding-agent-render.el pi-coding-agent-input.el pi-coding-agent-menu.el pi-coding-agent-core.el

check: compile lint test

# ============================================================
# Cleanup
# ============================================================

clean:
	@rm -f *.elc test/*.elc .deps-stamp

clean-cache:
	@./scripts/ollama.sh stop 2>/dev/null || true
	@rm -rf .cache
