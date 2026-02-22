# Kiro Emacs Service - ITIL/ISO9001 Compliant

## Service Management

### Start Service
```bash
~/.emacs.d/kiro.el/kiro-service.sh start
```

### Stop Service
```bash
~/.emacs.d/kiro.el/kiro-service.sh stop
```

### Restart Service
```bash
~/.emacs.d/kiro.el/kiro-service.sh restart
```

### Check Status
```bash
~/.emacs.d/kiro.el/kiro-service.sh status
```

### Health Check
```bash
~/.emacs.d/kiro.el/kiro-service.sh health
```

### View Logs
```bash
~/.emacs.d/kiro.el/kiro-service.sh logs
```

### Clean Spool
```bash
~/.emacs.d/kiro.el/kiro-service.sh clean
```

## Client Usage

### Evaluate Elisp
```bash
~/.emacs.d/kiro.el/kiro-emacs.py eval "(+ 2 3)"
```

### Open File
```bash
~/.emacs.d/kiro.el/kiro-emacs.py find-file "/path/to/file.el"
```

### Get Buffer Content
```bash
~/.emacs.d/kiro.el/kiro-emacs.py get-content
```

## Architecture

```
Kiro CLI → kiro-emacs.py → ~/kiro-spool/request-*.json
                                    ↓
                            Emacs (kiro-spool.el)
                                    ↓
                            ~/kiro-spool/response-*.json
                                    ↓
                            kiro-emacs.py → Kiro CLI
```

## Service Locations

- Service: `~/.emacs.d/kiro.el/kiro-service.sh`
- Spool: `~/kiro-spool/`
- Logs: `~/.kiro/logs/`
- PID: `~/.kiro/kiro-emacs.pid`
