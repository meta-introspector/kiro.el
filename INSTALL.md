# Kiro.el Installation Guide

## Prerequisites

- Emacs 27.1+ (tested with 30.0.50)
- kiro-cli installed and in PATH
- org-mode (built-in with Emacs)

## Quick Install

### 1. Add to init.el

Add this line to your `~/.emacs.d/init.el`:

```elisp
(load-file "~/.emacs.d/kiro.el/init-kiro.el")
```

### 2. Restart Emacs

Or evaluate the line: `C-x C-e`

### 3. Test It

```elisp
M-x kiro-task-new RET test RET
```

## Verify Installation

Run the automated test:

```bash
~/.emacs.d/kiro.el/test-kiro.sh
```

Expected output:
```
✅ All tests passed!
```

## Interactive Demo

```bash
~/.emacs.d/kiro.el/demo-kiro.sh
```

## Manual Installation

If you prefer manual setup:

```elisp
;; Add to ~/.emacs.d/init.el

;; 1. Add to load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/kiro.el"))

;; 2. Load kiro
(require 'kiro)

;; 3. Enable shell keybindings
(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c k s") 'kiro-task-save-chat)
            (local-set-key (kbd "C-c k c") 'kiro-task-save-compact-save)))

;; 4. Optional: Custom task directory
;; (setq kiro-task-directory "~/my-tasks/")
```

## Troubleshooting

### Commands not found?

Check if kiro is loaded:

```elisp
M-: (featurep 'kiro) RET
```

Should return `t`. If not, reload:

```elisp
M-x load-file RET ~/.emacs.d/kiro.el/init-kiro.el RET
```

### C-c k s not working in shell?

The keybinding is added via hook. Manually bind it:

```elisp
M-: (local-set-key (kbd "C-c k s") 'kiro-task-save-chat) RET
```

Or ensure the hook is in your init.el (see Manual Installation above).

### kiro-cli not found?

Check PATH:

```bash
which kiro-cli
```

If not found, install kiro-cli first.

### Byte-compilation warnings?

Recompile all files:

```bash
cd ~/.emacs.d/kiro.el
emacs --batch --eval "(byte-recompile-directory \"~/.emacs.d/kiro.el\" 0 t)"
```

## Configuration Options

### Custom Task Directory

```elisp
(setq kiro-task-directory "~/my-tasks/")
```

### Disable Shell Keybindings

Remove or comment out the shell-mode-hook in your init.el.

### Custom Keybindings

```elisp
;; Change global keybindings
(global-set-key (kbd "C-c t n") 'kiro-task-new)
(global-set-key (kbd "C-c t l") 'kiro-task-list)

;; Change shell keybindings
(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c s") 'kiro-task-save-chat)))
```

## Uninstall

1. Remove from init.el:
   ```elisp
   (load-file "~/.emacs.d/kiro.el/init-kiro.el")
   ```

2. Optionally delete directory:
   ```bash
   rm -rf ~/.emacs.d/kiro.el
   ```

3. Restart Emacs

## Next Steps

- Read [QUICKREF.md](QUICKREF.md) for command reference
- Read [TESTING.md](TESTING.md) for testing guide
- Read [README.md](README.md) for full documentation
- Run `demo-kiro.sh` for interactive demo

## Support

For issues or questions:
1. Check [TESTING.md](TESTING.md) troubleshooting section
2. Run `test-kiro.sh` to verify installation
3. Check Emacs messages buffer: `*Messages*`

## License

Part of the Kiro CLI project.
