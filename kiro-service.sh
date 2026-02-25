#!/usr/bin/env bash
# Kiro Emacs Service Manager - ITIL/ISO9001 Compliant
# Service: kiro-emacs-spool
# Version: 1.0.0
# Owner: Kiro Team

set -euo pipefail

SERVICE_NAME="kiro-emacs-spool"
TMUX_SESSION="kiro-emacs"
SPOOL_DIR="${HOME}/kiro-spool"
LOG_DIR="${HOME}/.kiro/logs"
PID_FILE="${HOME}/.kiro/kiro-emacs.pid"
CONFIG_DIR="${HOME}/.emacs.d/kiro.el"

# Logging
log() {
    echo "[$(date -Iseconds)] [$1] $2" | tee -a "${LOG_DIR}/service.log"
}

# Pre-flight checks
preflight() {
    log "INFO" "Running pre-flight checks"
    
    [[ -f "${CONFIG_DIR}/kiro-spool.el" ]] || {
        log "ERROR" "Missing kiro-spool.el"
        exit 1
    }
    
    command -v emacs >/dev/null || {
        log "ERROR" "Emacs not found"
        exit 1
    }
    
    command -v tmux >/dev/null || {
        log "ERROR" "tmux not found"
        exit 1
    }
    
    mkdir -p "${SPOOL_DIR}" "${LOG_DIR}"
    log "INFO" "Pre-flight checks passed"
}

# Service start
start() {
    log "INFO" "Starting ${SERVICE_NAME}"
    
    if tmux has-session -t "${TMUX_SESSION}" 2>/dev/null; then
        log "WARN" "Service already running"
        return 0
    fi
    
    preflight
    
    tmux new-session -d -s "${TMUX_SESSION}" \
        "emacs --batch \
         -l ${CONFIG_DIR}/kiro-spool.el \
         --eval '(kiro-spool-start)' \
         --eval '(while t (sleep-for 1))' \
         2>&1 | tee -a ${LOG_DIR}/emacs.log"
    
    sleep 2
    
    if tmux has-session -t "${TMUX_SESSION}" 2>/dev/null; then
        echo $$ > "${PID_FILE}"
        log "INFO" "Service started successfully"
        return 0
    else
        log "ERROR" "Service failed to start"
        return 1
    fi
}

# Service stop
stop() {
    log "INFO" "Stopping ${SERVICE_NAME}"
    
    if ! tmux has-session -t "${TMUX_SESSION}" 2>/dev/null; then
        log "WARN" "Service not running"
        return 0
    fi
    
    tmux kill-session -t "${TMUX_SESSION}"
    rm -f "${PID_FILE}"
    log "INFO" "Service stopped"
}

# Service restart
restart() {
    log "INFO" "Restarting ${SERVICE_NAME}"
    stop
    sleep 1
    start
}

# Service status
status() {
    if tmux has-session -t "${TMUX_SESSION}" 2>/dev/null; then
        log "INFO" "Service is running"
        echo "Status: ACTIVE"
        echo "Session: ${TMUX_SESSION}"
        echo "Spool: ${SPOOL_DIR}"
        return 0
    else
        log "INFO" "Service is not running"
        echo "Status: INACTIVE"
        return 1
    fi
}

# Health check
health() {
    log "INFO" "Running health check"
    
    if ! tmux has-session -t "${TMUX_SESSION}" 2>/dev/null; then
        log "ERROR" "Health check failed: service not running"
        return 1
    fi
    
    if [[ ! -d "${SPOOL_DIR}" ]]; then
        log "ERROR" "Health check failed: spool directory missing"
        return 1
    fi
    
    # Test request
    local test_id="health-$(date +%s)"
    cat > "${SPOOL_DIR}/request-${test_id}.json" <<EOF
{"id":"${test_id}","action":"eval","data":{"expr":"(+ 1 1)"}}
EOF
    
    for i in {1..10}; do
        if [[ -f "${SPOOL_DIR}/response-${test_id}.json" ]]; then
            local result=$(cat "${SPOOL_DIR}/response-${test_id}.json")
            rm -f "${SPOOL_DIR}/response-${test_id}.json"
            log "INFO" "Health check passed: ${result}"
            echo "Health: OK"
            return 0
        fi
        sleep 1
    done
    
    log "ERROR" "Health check failed: timeout"
    return 1
}

# Clean spool
clean() {
    log "INFO" "Cleaning spool directory"
    rm -f "${SPOOL_DIR}"/request-*.json "${SPOOL_DIR}"/response-*.json
    log "INFO" "Spool cleaned"
}

# Show logs
logs() {
    tail -f "${LOG_DIR}/service.log"
}

# Main
case "${1:-}" in
    start)   start ;;
    stop)    stop ;;
    restart) restart ;;
    status)  status ;;
    health)  health ;;
    clean)   clean ;;
    logs)    logs ;;
    *)
        echo "Usage: $0 {start|stop|restart|status|health|clean|logs}"
        exit 1
        ;;
esac
