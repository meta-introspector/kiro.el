# Kiro Emacs System - Complete Integration

A comprehensive Emacs-based system for managing shells, queries, and distributed workflows with real-time monitoring and prompt queue management.

## 🚀 Features

### Dashboard (`M-x kiro-dashboard`)
- Real-time service status monitoring
- Tunnel peer discovery (multi-Emacs instances)
- Shell buffer tracking with status (RUNNING/STOPPED)
- Query job management
- Cache statistics
- Interactive navigation (press RET to jump to any buffer)

### Prompt Queue (`M-x kiro-prompt-queue`)
- **Central queue for ALL shell prompts across all Emacs instances**
- Shows last 20 lines of context per prompt
- Batch operations: answer 'y' or 'n' to all prompts at once
- Jump directly to any waiting shell
- Detects `[y/n]` prompts and questions automatically

### Query Planner
- Rust-based query planning with Emacs buffer execution
- Multi-tool support (plocate, ripgrep, omnisearch, knowledge bases)
- Parallel/sequential/single/fallback strategies
- Query result caching
- Each job runs in dedicated buffer

### Tunnel System
- Connect multiple Emacs instances via spool
- Peer discovery and message passing
- Remote shell monitoring
- Distributed prompt queue

### Task Management
- Kiro-shell-task-mode for all shells
- Automatic task tracking
- Integration with org-mode
- 10-fold classification system

## 📦 Installation

```bash
cd ~/.emacs.d/kiro.el
./setup-emacs.sh
```

Or manually add to `~/.emacs`:

```elisp
(add-to-list 'load-path "~/.emacs.d/kiro.el")
(load-file "~/.emacs.d/kiro.el/kiro.el")

;; Optional keybindings
(global-set-key (kbd "C-c k d") 'kiro-dashboard)
(global-set-key (kbd "C-c k p") 'kiro-prompt-queue)
(global-set-key (kbd "C-c k q") 'kiro-query)
```

## 🎯 Quick Start

### Start Service
```bash
~/.emacs.d/kiro.el/kiro-service.sh start
```

### Open Dashboard
```elisp
M-x kiro-dashboard
```

### Manage Prompts
```elisp
M-x kiro-prompt-queue
```

## 📋 Commands

### Dashboard
- `g` - Refresh
- `n` - New query
- `e` - Execute job at point
- `v` / `RET` - View/jump to buffer
- `s` - Convert shells to Kiro task mode
- `c` - Clear cache
- `?` - Help
- `q` - Quit

### Prompt Queue
- `RET` - Answer prompt interactively
- `j` - Jump to shell buffer
- `y` - Answer 'y' to ALL prompts
- `n` - Answer 'n' to ALL prompts
- `g` - Refresh queue
- `q` - Quit

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    Kiro Dashboard                       │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐             │
│  │ Service  │  │  Tunnel  │  │  Shells  │             │
│  │ Status   │  │  Peers   │  │  Status  │             │
│  └──────────┘  └──────────┘  └──────────┘             │
└─────────────────────────────────────────────────────────┘
                        │
        ┌───────────────┼───────────────┐
        │               │               │
┌───────▼──────┐ ┌──────▼──────┐ ┌─────▼──────┐
│ Prompt Queue │ │Query Planner│ │   Tunnel   │
│              │ │             │ │            │
│ • Local      │ │ • Planning  │ │ • Peers    │
│ • Remote     │ │ • Execution │ │ • Messages │
│ • Batch Ops  │ │ • Caching   │ │ • Sync     │
└──────────────┘ └─────────────┘ └────────────┘
```

## 📁 Files

- `kiro.el` - Main package with autoloads
- `kiro-dashboard.el` - Dashboard mode
- `kiro-prompt-queue.el` - Central prompt queue
- `kiro-query-planner.el` - Query planner with caching
- `kiro-tunnel.el` - Emacs-to-Emacs communication
- `kiro-spool.el` - Spool service for CLI integration
- `kiro-task-mode.el` - Task management
- `kiro-meta-mode.el` - Meta mode with RDF/emoji
- `kiro-tenfold.el` - 10-fold classification
- `kiro-service.sh` - ITIL-compliant service manager
- `kiro-emacs.py` - Python CLI client

## 🧪 Testing

```bash
# Run ERT tests
~/.emacs.d/kiro.el/kiro-service.sh start
cd ~/.emacs.d/kiro.el && nix build

# Test dashboard
~/.emacs.d/kiro.el/demo-dashboard.sh

# Test prompt queue
M-x kiro-prompt-queue
```

## 🔧 Service Management

```bash
# Start/stop/restart
~/.emacs.d/kiro.el/kiro-service.sh start
~/.emacs.d/kiro.el/kiro-service.sh stop
~/.emacs.d/kiro.el/kiro-service.sh restart

# Status and health
~/.emacs.d/kiro.el/kiro-service.sh status
~/.emacs.d/kiro.el/kiro-service.sh health

# Logs
~/.emacs.d/kiro.el/kiro-service.sh logs
```

## 🌟 Key Innovations

1. **Central Prompt Queue** - Never miss a shell prompt again. All prompts from all shells (local and remote) in one place.

2. **Multi-Emacs Tunnel** - Connect multiple Emacs instances seamlessly. Share state, sync dashboards, monitor remote shells.

3. **Smart Shell Detection** - Automatically detects shells waiting for input with `⚠WAIT` indicator.

4. **Buffer-Based Jobs** - Query jobs run in dedicated Emacs buffers with full editing capabilities.

5. **ITIL-Compliant Service** - Professional service management with health checks, logging, and monitoring.

## 📊 Example Dashboard

```
╔═══════════════════════════════════════════════════════════╗
║              🚀 KIRO SYSTEM DASHBOARD 🚀                 ║
╚═══════════════════════════════════════════════════════════╝

## Service Status
Spool:  🟢 ACTIVE
Jobs:   3 active

## Tunnel Peers
• emacs-4082384 (PID: 4082384)
• emacs-4169559 (PID: 4169559)

## Shell Buffers
• kiro-shell-emacs.kiro.org ✓KIRO - RUN | ~/projects/ | ...
• dasl-kiro.kiro.org ✓KIRO - RUN | ~/dasl/ ⚠WAIT | Allow?
```

## 🤝 Contributing

This is a research project exploring distributed Emacs workflows, prompt management, and multi-instance coordination.

## 📄 License

See LICENSE file.

## 🔗 Links

- Documentation: `~/.emacs.d/kiro.el/README-*.md`
- Service: `~/.emacs.d/kiro.el/kiro-service.sh`
- Tests: `~/.emacs.d/kiro.el/kiro-test.el`
