# Kiro.el Quick Reference

## Installation

Add to `~/.emacs.d/init.el`:

```elisp
(load-file "~/.emacs.d/kiro.el/init-kiro.el")
```

## Global Keybindings

| Key       | Command                  | Description                    |
|-----------|--------------------------|--------------------------------|
| `C-c k r` | kiro-task-rename-buffer  | Rename current buffer as task  |
| `C-c k n` | kiro-task-new            | Create new task                |
| `C-c k l` | kiro-task-list           | List all tasks                 |
| `C-c k b` | kiro-switch-buffer       | Switch to kiro buffer          |

## Shell Mode Keybindings

(When in shell-mode with kiro-cli running)

| Key       | Command                      | Description                |
|-----------|------------------------------|----------------------------|
| `C-c k s` | kiro-task-save-chat          | Save current chat          |
| `C-c k c` | kiro-task-save-compact-save  | Save, compact, save again  |

## Main Commands

### Task Management
- `M-x kiro-task-new` - Create new task with org template
- `M-x kiro-task-list` - Show all tasks in `~/tasks/`
- `M-x kiro-task-rename-buffer` - Rename buffer as task
- `M-x kiro-task-save-chat` - Save kiro-cli chat session

### Buffer Management
- `M-x kiro-switch-buffer` - Switch to kiro buffer by prime/name
- `M-x kiro-buffers` - Interactive buffer manager
- `M-x kiro-buffer-rename-all` - Rename all buffers
- `M-x kiro-buffer-save-all` - Save all modified buffers
- `M-x kiro-buffer-compact-all` - DASL compress buffers

### Session Management
- `M-x kiro-sessions` - Interactive session manager
- `M-x kiro-session-save-all` - Save all CLI sessions
- `M-x kiro-session-rename-all` - Rename terminal windows
- `M-x kiro-session-compact-all` - Compact sessions

### Shell Management
- `M-x kiro-shell-rename-all` - Rename all shells interactively

### All-in-One
- `M-x kiro-save-all` - Do everything (rename, save, compact)

### Other
- `M-x kiro-dashboard` - Open dashboard
- `M-x kiro-prompt-queue` - Open prompt queue
- `M-x kiro-homedir-projects` - List projects
- `M-x kiro-spool-start` - Start spool service
- `M-x kiro-query` - Query planner

## Buffer Names (Monster Primes)

| Prime | Buffer Name           | Purpose          |
|-------|-----------------------|------------------|
| 2     | kiro-binary           | Energy           |
| 3     | kiro-ternary          | Time/Triples     |
| 5     | kiro-pentagonal       | Space            |
| 7     | kiro-heptagonal       | Disk             |
| 11    | kiro-hendeca          | Network          |
| 13    | kiro-trideca          | Scheduling       |
| 17    | kiro-wallpaper        | Maps             |
| 19    | kiro-lattice          | Data/SU(3)       |
| 23    | kiro-consciousness    | Awareness        |
| 29    | kiro-lunar            | Phase            |
| 31    | kiro-communication    | Chat             |
| 41    | kiro-cache            | Memory           |
| 47    | kiro-cambridge        | Registers        |
| 59    | kiro-memory           | Attention/Vishnu |
| 71    | kiro-omega            | Terminal/Brahma  |

## Team Buffers

- kiro-jocko (Training protocol)
- kiro-arnie (UUCP processing)
- kiro-metzger (Leech embedding)
- kiro-agent1 (Coordination)
- kiro-prime (Research lead)
- kiro-875541 (ML systems)
- kiro-704400 (Consensus)
- kiro-520977 (Testing)

## File Locations

- Tasks: `~/tasks/task-NAME.kiro.org`
- Chats: `chats/YYYY/MM/DD/TASKNAME-TIMESTAMP.json`
- Sessions: `~/.kiro/sessions/TIMESTAMP/`
- Compacted: `~/.kiro/compact/TIMESTAMP/`
- Buffer compacts: `~/.kiro/buffer-compact/TIMESTAMP/`

## Workflow Example

1. Start kiro-cli in shell: `kiro-cli chat`
2. Rename buffer: `C-c k r` → enter task name
3. Work on task...
4. Save chat: `C-c k s`
5. Save everything: `M-x kiro-save-all`

## Troubleshooting

**C-c k s not working?**
```elisp
M-: (local-set-key (kbd "C-c k s") 'kiro-task-save-chat)
```

**Check if loaded:**
```elisp
M-: (featurep 'kiro)
```

**Reload:**
```elisp
M-x load-file RET ~/.emacs.d/kiro.el/init-kiro.el RET
```

## Testing

Run automated tests:
```bash
~/.emacs.d/kiro.el/test-kiro.sh
```

Run interactive test:
```bash
emacs -l ~/.emacs.d/kiro.el/test-interactive.el
```
