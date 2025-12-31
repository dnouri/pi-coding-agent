# pi.el Makefile

EMACS ?= emacs
BATCH = $(EMACS) --batch -L .

# Pi CLI version - update in workflows too when changing
PI_VERSION ?= 0.30.2
PI_BIN ?= .cache/pi/node_modules/.bin/pi
PI_BIN_DIR = $(abspath $(dir $(PI_BIN)))

.PHONY: test test-integration test-integration-ci test-gui test-gui-ci test-all
.PHONY: check compile lint clean clean-cache help
.PHONY: ollama-start ollama-stop ollama-status setup-pi setup-models

help:
	@echo "Targets:"
	@echo "  make test             Unit tests (fast)"
	@echo "  make test-integration Integration tests (local, starts Ollama)"
	@echo "  make test-gui         GUI tests (local, starts Ollama)"
	@echo "  make check            Compile, lint, unit tests"
	@echo "  make clean            Remove generated files"
	@echo ""
	@echo "CI targets (Ollama already running):"
	@echo "  make test-integration-ci"
	@echo "  make test-gui-ci"

# ============================================================
# Unit tests
# ============================================================

test: clean
	@echo "=== Unit Tests ==="
	$(BATCH) -L test -l pi -l pi-core-test -l pi-test -f ert-run-tests-batch-and-exit

# ============================================================
# Setup helpers
# ============================================================

setup-pi:
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
test-integration: clean setup-pi
	@echo "=== Integration Tests (pi@$(PI_VERSION)) ==="
	@./scripts/ollama.sh start
	@PI_CODING_AGENT_DIR=$$(mktemp -d) && \
		cp test/fixtures/ollama-models.json "$$PI_CODING_AGENT_DIR/models.json" && \
		env PATH="$(PI_BIN_DIR):$$PATH" PI_CODING_AGENT_DIR="$$PI_CODING_AGENT_DIR" PI_RUN_INTEGRATION=1 \
		$(BATCH) -L test -l pi -l pi-integration-test -f ert-run-tests-batch-and-exit; \
		status=$$?; rm -rf "$$PI_CODING_AGENT_DIR"; exit $$status

# CI: Ollama already running via services block
test-integration-ci: clean setup-pi
	@echo "=== Integration Tests CI (pi@$(PI_VERSION)) ==="
	@mkdir -p "$$PI_CODING_AGENT_DIR"
	@cp test/fixtures/ollama-models.json "$$PI_CODING_AGENT_DIR/models.json"
	env PATH="$(PI_BIN_DIR):$$PATH" PI_RUN_INTEGRATION=1 \
	$(BATCH) -L test -l pi -l pi-integration-test -f ert-run-tests-batch-and-exit

# ============================================================
# GUI tests
# ============================================================

# Local: starts Ollama via Docker
test-gui: clean setup-pi
	@echo "=== GUI Tests (pi@$(PI_VERSION)) ==="
	@./scripts/ollama.sh start
	@PI_CODING_AGENT_DIR=$$(mktemp -d) && \
		cp test/fixtures/ollama-models.json "$$PI_CODING_AGENT_DIR/models.json" && \
		env PATH="$(PI_BIN_DIR):$$PATH" PI_CODING_AGENT_DIR="$$PI_CODING_AGENT_DIR" \
		./test/run-gui-tests.sh; \
		status=$$?; rm -rf "$$PI_CODING_AGENT_DIR"; exit $$status

# CI: Ollama already running via services block
test-gui-ci: clean setup-pi
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

compile: clean
	@echo "=== Byte-compile ==="
	$(BATCH) --eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile pi-core.el pi.el

lint:
	@echo "=== Checkdoc ==="
	@$(BATCH) \
		--eval "(require 'checkdoc)" \
		--eval "(setq sentence-end-double-space nil)" \
		--eval "(checkdoc-file \"pi-core.el\")" \
		--eval "(checkdoc-file \"pi.el\")" 2>&1 | \
		{ grep -q "^Warning" && { grep "^Warning"; exit 1; } || echo "OK"; }

check: compile lint test

# ============================================================
# Cleanup
# ============================================================

clean:
	@rm -f *.elc test/*.elc

clean-cache:
	@./scripts/ollama.sh stop 2>/dev/null || true
	@rm -rf .cache
