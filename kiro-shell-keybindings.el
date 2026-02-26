;;; kiro-shell-keybindings.el --- Enable C-c k s in all shells

(require 'kiro-task-mode)

(defun kiro-enable-shell-keybindings ()
  "Enable kiro keybindings in shell-mode buffers."
  (local-set-key (kbd "C-c k s") 'kiro-task-save-chat))

(add-hook 'shell-mode-hook 'kiro-enable-shell-keybindings)

(provide 'kiro-shell-keybindings)
;;; kiro-shell-keybindings.el ends here
