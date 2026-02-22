# Kiro Dashboard

Interactive Emacs dashboard for Kiro system monitoring and query management.

## Features

- **Service Status** - Spool and job monitoring
- **Query Jobs** - View and manage active queries
- **Cache Stats** - Query cache statistics
- **Buffer Management** - List all Kiro buffers
- **Interactive Controls** - Execute, view, and manage jobs

## Usage

### Open Dashboard

```bash
~/.emacs.d/kiro.el/kiro-emacs.py dashboard
```

Or in Emacs:
```elisp
M-x kiro-dashboard
```

### Keybindings

- `g` - Refresh dashboard
- `n` - New query
- `e` - Execute job at cursor
- `v` - View job buffer
- `c` - Clear cache
- `q` - Quit

## Demo

```bash
~/.emacs.d/kiro.el/demo-dashboard.sh
```

## Dashboard Layout

```
╔═══════════════════════════════════════════════════════════╗
║              🚀 KIRO SYSTEM DASHBOARD 🚀                 ║
╚═══════════════════════════════════════════════════════════╝

## Service Status

Spool:  🟢 ACTIVE
Jobs:   3 active
Time:   Sun Feb 22 11:55:00 2026

## Query Jobs

• [123456] Monster prime 71 (parallel)
• [789012] tau function (single)
• [345678] Ramanujan (single)

## Cache Stats

Entries: 5
Memory:  ~12 KB

## Kiro Buffers

• *kiro-query-123456* (1024 bytes)
• *kiro-query-789012* (512 bytes)
```

## Integration

Dashboard integrates with:
- kiro-query-planner.el (job management)
- kiro-spool.el (service status)
- Emacs buffer system (navigation)
