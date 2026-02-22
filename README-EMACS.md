# Kiro Emacs Package

## Installation

Add to your `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
(add-to-list 'load-path "~/.emacs.d/kiro.el")
(require 'kiro)
```

Or use the provided init file:

```elisp
(load-file "~/.emacs.d/kiro.el/kiro-init.el")
```

## Commands

### M-x kiro-dashboard
Open the Kiro system dashboard with real-time monitoring.

### M-x kiro-query
Plan and execute a query using the Kiro query planner.

### M-x kiro-spool-start
Start the Kiro spool service for CLI integration.

## Keybindings (Optional)

```elisp
(global-set-key (kbd "C-c k d") 'kiro-dashboard)
(global-set-key (kbd "C-c k q") 'kiro-query)
(global-set-key (kbd "C-c k s") 'kiro-spool-start)
```

## Dashboard Keybindings

- `g` - Refresh
- `n` - New query
- `e` - Execute job at point
- `v` / `RET` - View job buffer
- `c` - Clear cache
- `?` - Help
- `q` - Quit

## Usage

```elisp
;; Open dashboard
M-x kiro-dashboard

;; Create query
M-x kiro-query RET Monster prime 71 RET

;; Start spool service
M-x kiro-spool-start
```

## Files

- `kiro.el` - Main package
- `kiro-dashboard.el` - Dashboard mode
- `kiro-query-planner.el` - Query planner
- `kiro-spool.el` - Spool service
- `kiro-meta-mode.el` - Meta mode
- `kiro-task-mode.el` - Task mode
- `kiro-tenfold.el` - 10-fold classification
