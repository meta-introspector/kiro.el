;;; init-kiro.el --- Kiro.el initialization snippet

;; Add this to your ~/.emacs.d/init.el or evaluate it to test

;; Get the directory of this file
(defvar kiro-base-dir (file-name-directory (or load-file-name buffer-file-name)))

;; Load kiro.el
(add-to-list 'load-path kiro-base-dir)
(require 'kiro)
(load-file (expand-file-name "kiro-dashboard.el" kiro-base-dir))
(load-file (expand-file-name "kiro-task-mode.el" kiro-base-dir))

;; Enable shell keybindings automatically
(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c k s") 'kiro-task-save-chat)
            (local-set-key (kbd "C-c k c") 'kiro-task-save-compact-save)))

;; Optional: Set custom task directory
;; (setq kiro-task-directory "~/my-tasks/")

(message "✅ Kiro.el loaded successfully!")

(provide 'init-kiro)
;;; init-kiro.el ends here
