<<<<<<< Updated upstream
# kiro.el

Emacs integration for Kiro CLI - save, compact, and manage chat sessions.

Scripts for using kiro-cli in emacs comint in tmux on guix and nix on ubuntu.

## Features

- **Save & Compact Workflow**: Save chat, compact it, save compacted version
- **Auto-increment Step Counter**: Tracks session versions
- **Dry-run Mode**: Test commands without executing
- **Task Name Extraction**: Auto-detects from buffer/file names
- **Timestamp Filenames**: `task-step-YYYYMMDD-HHMMSS.json`

## Installation

```elisp
;; In your init.el
(add-to-list 'load-path "~/.emacs.d/lisp/kiro")
(require 'kiro-osm-save)
```

## Keybindings

- `C-c k c` - Save, compact, and save compacted (full workflow)
- `C-c k s` - Quick save only
- `C-c k d` - Toggle dry-run mode

## Usage

Run from any Kiro chat buffer (comint-mode):

```elisp
M-x kiro-osm-save-and-compact
```

Files saved to `chats/` directory with format:
- `task-name-N-YYYYMMDD-HHMMSS.json`
- `task-name-N-YYYYMMDD-HHMMSS-compacted.json`

## License

AGPL-3.0 + MIT on purchase

**ZK hackers gotta eat license** 🍕

- Open source under AGPL-3.0
- Commercial/proprietary use available under MIT with purchase
- Contact for licensing inquiries
=======
# Kiro.el - Emacs Integration for Kiro CLI

Complete Emacs integration for Kiro CLI with unified session, buffer, and shell management.

## Features

- 🐚 **Shell Integration** - Rename and manage kiro-cli chat shells
- 📝 **Buffer Management** - Save, rename, and compact all buffers  
- 💾 **Session Management** - Track and save CLI sessions
- ⌨️ **Unified Keybindings** - `C-c k s` works everywhere
- 🎯 **Interactive Managers** - Visual buffer and session management

## Installation

Add to `~/.emacs.d/init.el`:

```elisp
(straight-use-package
 '(kiro :type git
        :local-repo "~/.emacs.d/kiro.el"
        :files ("*.el")))
(require 'kiro)

;; Enable C-c k s in all shells
(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c k s") 'kiro-task-save-chat)))
```

## Quick Start

**One command to save everything:**
```elisp
M-x kiro-save-all
```

**Bind C-c k s in current shell:**
```elisp
M-: (local-set-key (kbd "C-c k s") 'kiro-task-save-chat)
```

## Commands

### Main Commands

- `M-x kiro-save-all` - Do everything (interactive shell renaming + save + compact)
- `M-x kiro-dashboard` - Open dashboard
- `M-x kiro-buffers` - Interactive buffer manager
- `M-x kiro-sessions` - Interactive session manager

### Shell Management

- `M-x kiro-shell-rename-all` - Rename all shells immediately
  - Analyzes last 50 lines for directory/commands
  - Renames to `*Kiro-Shell: taskname*`
  - Enables `C-c k s` keybinding

**Manual binding for current shell:**
```elisp
M-: (local-set-key (kbd "C-c k s") 'kiro-task-save-chat)
```

**Bind in all shells:**
```elisp
(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c k s") 'kiro-task-save-chat)))
```

### Buffer Management

- `M-x kiro-buffer-rename-all` - Rename buffers as `*Kiro: dir/file*`
- `M-x kiro-buffer-save-all` - Save all modified buffers + kiro chats
- `M-x kiro-buffer-compact-all` - DASL compress to `~/.kiro/buffer-compact/`

### Session Management

- `M-x kiro-session-save-all` - Save to `~/.kiro/sessions/TIMESTAMP/`
- `M-x kiro-session-rename-all` - Rename terminals as "Kiro: DIR (PID xxx)"
- `M-x kiro-session-compact-all` - Compact to `~/.kiro/compact/TIMESTAMP/`

### Existing Keybinding (Preserved)

```elisp
C-c k s → kiro-task-save-chat
```

Saves current chat to `chats/TASKNAME-TIMESTAMP.json`

## Interactive Managers

### Buffer Manager (`M-x kiro-buffers`)

Shows all file buffers with:
- `*` = modified
- `K` = kiro chat active

Keys:
- `r` - Rename all
- `s` - Save all
- `c` - Compact all
- `a` - All operations
- `q` - Quit

### Session Manager (`M-x kiro-sessions`)

Shows all running kiro-cli-chat processes.

Keys:
- `s` - Save all
- `r` - Rename all
- `c` - Compact all
- `q` - Quit

## File Structure

```
~/.emacs.d/kiro.el/
├── kiro.el                    # Main entry point
├── kiro-autoload.el           # Autoload definitions
├── kiro-task-mode.el          # Task mode (C-c k s)
├── kiro-shell-renamer.el      # Interactive shell renaming
├── kiro-buffer-manager.el     # Buffer management
├── kiro-session-manager.el    # CLI session management
├── kiro-dashboard.el          # Dashboard
├── kiro-prompt-queue.el       # Prompt queue
├── kiro-homedir-web.el        # Project browser
├── kiro-spool.el              # Spool service
└── kiro-query-planner.el      # Query planner

~/.kiro/
├── sessions/TIMESTAMP/        # Session metadata
├── compact/TIMESTAMP/         # Compacted sessions
└── buffer-compact/TIMESTAMP/  # Compacted buffers (DASL)
```

## Output Locations

### Buffer Compacts
`~/.kiro/buffer-compact/YYYYMMDD_HHMMSS/`
- `buffer_N.md` - Individual buffer metadata
- `summary.md` - Overall statistics
- 10x compression ratio (DASL codec)

### Session Saves
`~/.kiro/sessions/YYYYMMDD_HHMMSS/`
- `session_PID.txt` - Session metadata per process

### Session Compacts
`~/.kiro/compact/YYYYMMDD_HHMMSS/`
- `compact_PID.md` - Compacted session summaries

### Chat Saves
`chats/TASKNAME-TIMESTAMP.json`
- Saved via `C-c k s` in kiro-task-mode

## Features

### Shell Renaming
- Analyzes last 50 lines of shell content
- Extracts directory from prompt
- Detects `cd` commands
- Identifies common commands (cargo, npm, make, git)
- Interactive org-mode editing before applying

### Buffer Management
- Renames all file buffers with directory context
- Saves modified buffers
- Calls `kiro-task-save-chat` for buffers with kiro-task-mode
- DASL compression with metadata (CID, shard, hash)

### Session Management
- Finds all running kiro-cli-chat processes
- Saves metadata (PID, TTY, working directory)
- Renames terminal windows
- Creates compact summaries

## Example Workflow

1. Work in multiple shells and buffers
2. Run `M-x kiro-save-all`
3. Edit shell names in org-mode buffer
4. Click "Apply"
5. Everything saved, renamed, and compacted

Result:
- 5 shells → `*Kiro-Shell: taskname*`
- 7 buffers → `*Kiro: dir/file*`
- All saved to `~/.kiro/` with timestamps
- 10x compression on buffer content

## Requirements

- Emacs 27+
- straight.el package manager
- kiro-cli installed
- org-mode (built-in)

## Troubleshooting

**C-c k s not working in shell?**

Bind it manually:
```elisp
M-: (local-set-key (kbd "C-c k s") 'kiro-task-save-chat)
```

Or add to init.el:
```elisp
(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c k s") 'kiro-task-save-chat)))
```

**Shell rename hanging?**

The `kiro-shell-task-mode` derived mode can cause recursion. Use direct keybinding instead.

## License

Part of the Kiro CLI project.
>>>>>>> Stashed changes
