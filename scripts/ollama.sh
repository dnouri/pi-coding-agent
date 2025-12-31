#!/bin/bash
# scripts/ollama.sh - Manage Ollama Docker container for testing
#
# This script provides isolated Ollama setup for pi.el integration tests.
# All data stays in .ollama/ within the project directory.
#
# Usage:
#   ./scripts/ollama.sh start   # Start container, pull model if needed
#   ./scripts/ollama.sh stop    # Stop and remove container
#   ./scripts/ollama.sh status  # Check if running and show models
#   ./scripts/ollama.sh logs    # Show container logs (for debugging)
#
# Environment variables:
#   PI_TEST_MODEL  - Model to use (default: qwen3:1.7b)
#   PI_TEST_PORT   - Port for Ollama API (default: 11434)

set -e

CONTAINER_NAME="pi-test-ollama"
MODEL="${PI_TEST_MODEL:-qwen3:1.7b}"
PORT="${PI_TEST_PORT:-11434}"
PROJECT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
CACHE_DIR="$PROJECT_DIR/.cache"
OLLAMA_DIR="$CACHE_DIR/ollama"
OLLAMA_URL="http://localhost:$PORT"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() { echo -e "${GREEN}✓${NC} $1"; }
log_warn() { echo -e "${YELLOW}⚠${NC} $1"; }
log_error() { echo -e "${RED}✗${NC} $1"; }

check_docker() {
    if ! command -v docker &>/dev/null; then
        log_error "Docker not found. Install from https://docker.com"
        exit 1
    fi
    if ! docker info &>/dev/null; then
        log_error "Docker daemon not running. Start Docker and try again."
        exit 1
    fi
}

is_container_running() {
    docker ps --format '{{.Names}}' 2>/dev/null | grep -q "^${CONTAINER_NAME}$"
}

wait_for_api() {
    local max_attempts=30
    local attempt=0
    while [ $attempt -lt $max_attempts ]; do
        if curl -s "$OLLAMA_URL/api/tags" &>/dev/null; then
            return 0
        fi
        attempt=$((attempt + 1))
        sleep 1
    done
    return 1
}

ensure_model() {
    if ! curl -s "$OLLAMA_URL/api/tags" | grep -q "\"name\":\"$MODEL\""; then
        log_warn "Pulling model $MODEL (this may take a few minutes)..."
        docker exec "$CONTAINER_NAME" ollama pull "$MODEL"
        log_info "Model $MODEL ready"
    else
        log_info "Model $MODEL already available"
    fi
}

cmd_start() {
    check_docker

    # Check if already running
    if is_container_running; then
        log_info "Ollama container already running"
        ensure_model
        return 0
    fi

    # Remove stopped container if exists
    docker rm -f "$CONTAINER_NAME" 2>/dev/null || true

    # Create local storage directory
    mkdir -p "$OLLAMA_DIR"

    # Start container with project-local storage
    log_warn "Starting Ollama container..."
    docker run -d \
        --name "$CONTAINER_NAME" \
        -v "$OLLAMA_DIR:/root/.ollama" \
        -p "$PORT:11434" \
        ollama/ollama >/dev/null

    # Wait for API to be ready
    log_warn "Waiting for Ollama API..."
    if ! wait_for_api; then
        log_error "Ollama failed to start. Check logs with: $0 logs"
        exit 1
    fi

    ensure_model
    log_info "Ollama ready at $OLLAMA_URL"
}

cmd_stop() {
    if is_container_running; then
        log_warn "Stopping Ollama container..."
        docker stop "$CONTAINER_NAME" >/dev/null
        docker rm "$CONTAINER_NAME" >/dev/null
        log_info "Ollama stopped"
    else
        log_info "Ollama not running"
    fi
}

cmd_status() {
    check_docker
    
    if is_container_running; then
        log_info "Ollama container: running"
        echo ""
        echo "Available models:"
        curl -s "$OLLAMA_URL/api/tags" 2>/dev/null | \
            grep -oP '"name":"\K[^"]+' | \
            while read -r model; do echo "  - $model"; done
        echo ""
        echo "API endpoint: $OLLAMA_URL"
        echo "Storage: $OLLAMA_DIR"
    else
        log_error "Ollama container: not running"
        echo ""
        echo "Start with: $0 start"
        exit 1
    fi
}

cmd_logs() {
    if docker ps -a --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
        docker logs "$CONTAINER_NAME" --tail 50
    else
        log_error "Container $CONTAINER_NAME not found"
        exit 1
    fi
}

cmd_help() {
    echo "Ollama Docker management for pi.el testing"
    echo ""
    echo "Usage: $0 <command>"
    echo ""
    echo "Commands:"
    echo "  start   Start Ollama container (pulls model if needed)"
    echo "  stop    Stop and remove Ollama container"
    echo "  status  Show container status and available models"
    echo "  logs    Show container logs"
    echo "  help    Show this help message"
    echo ""
    echo "Environment:"
    echo "  PI_TEST_MODEL=$MODEL"
    echo "  PI_TEST_PORT=$PORT"
    echo ""
    echo "Storage: $OLLAMA_DIR"
}

# Main dispatch
case "${1:-help}" in
    start)  cmd_start ;;
    stop)   cmd_stop ;;
    status) cmd_status ;;
    logs)   cmd_logs ;;
    help)   cmd_help ;;
    *)
        log_error "Unknown command: $1"
        cmd_help
        exit 1
        ;;
esac
