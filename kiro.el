;;; kiro.el --- Kiro system integration for Emacs

;; Author: Kiro Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience

;;; Commentary:
;; Kiro system integration providing dashboard, query planner, and spool service.

;;; Code:

(require 'kiro-task-mode)
(require 'kiro-shell-keybindings)

(defgroup kiro nil
  "Kiro system integration."
  :group 'tools
  :prefix "kiro-")

;;;###autoload
(defun kiro-dashboard ()
  "Open Kiro dashboard."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-dashboard.el"))
  (switch-to-buffer "*Kiro Dashboard*")
  (kiro-dashboard-refresh))

;;;###autoload
(defun kiro-prompt-queue ()
  "Open Kiro prompt queue."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-prompt-queue.el"))
  (kiro-prompt-queue))

;;;###autoload
(defun kiro-homedir-projects ()
  "List homedir web projects."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-homedir-web.el"))
  (kiro-homedir-list-projects))

;;;###autoload
(defun kiro-spool-start ()
  "Start Kiro spool service."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-spool.el"))
  (kiro-spool-start))

;;;###autoload
(defun kiro-query (pattern)
  "Query PATTERN using Kiro planner."
  (interactive "sQuery: ")
  (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-query-planner.el"))
  (kiro-query-plan pattern)
  (message "Query planned: %s" pattern))

;;;###autoload
(defun kiro-sessions ()
  "Manage kiro CLI sessions."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-session-manager.el"))
  (kiro-session-manage))

;;;###autoload
(defun kiro-buffers ()
  "Manage all buffers."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-buffer-manager.el"))
  (kiro-buffer-manage))

;;;###autoload
(defun kiro-save-all ()
  "Rename, save, and compact everything."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-shell-renamer.el"))
  (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-buffer-manager.el"))
  (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-session-manager.el"))
  (kiro-shell-rename-all)
  (kiro-buffer-rename-all)
  (kiro-buffer-save-all)
  (kiro-buffer-compact-all)
  (kiro-session-save-all)
  (kiro-session-rename-all)
  (kiro-session-compact-all)
  (message "✅ All done!"))

(provide 'kiro)
;;; kiro.el ends here
