# Testing kiro.el

## Quick Test

Run this in Emacs:

```elisp
M-x load-file RET ~/.emacs.d/kiro.el/kiro.el RET
```

## Test Commands

### 1. Test Basic Commands

```elisp
M-x kiro-task-new RET test-task RET
```

Should create a new task buffer with org-mode template.

### 2. Test Buffer Switching

```elisp
M-x kiro-switch-buffer RET 2 RET
```

Should switch to `kiro-binary` buffer.

### 3. Test Task List

```elisp
M-x kiro-task-list RET
```

Should show all tasks in `~/tasks/`.

### 4. Test Shell Integration

1. Open a shell: `M-x shell RET`
2. Start kiro-cli: `kiro-cli chat`
3. Press `C-c k s` - should save chat

### 5. Test Save All

```elisp
M-x kiro-save-all RET
```

Should rename, save, and compact everything.

## Manual Keybinding Test

In a shell buffer with kiro-cli running:

```elisp
M-: (local-set-key (kbd "C-c k s") 'kiro-task-save-chat) RET
```

Then press `C-c k s` to test.

## Expected Results

✓ All commands should be available via `M-x`
✓ Global keybindings work: `C-c k r`, `C-c k n`, `C-c k l`, `C-c k b`
✓ Mode-specific keybindings work in shell-mode: `C-c k s`, `C-c k c`
✓ Files saved to `chats/YYYY/MM/DD/` directory
✓ Tasks saved to `~/tasks/` directory

## Troubleshooting

### C-c k s not working in shell?

Add to your `~/.emacs.d/init.el`:

```elisp
(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c k s") 'kiro-task-save-chat)))
```

### Commands not found?

Reload kiro.el:

```elisp
M-x load-file RET ~/.emacs.d/kiro.el/kiro.el RET
```

### Check if loaded:

```elisp
M-: (featurep 'kiro) RET
```

Should return `t`.
