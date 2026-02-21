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
