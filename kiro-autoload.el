;;; kiro-autoload.el --- Autoload definitions for Kiro

;;;###autoload
(defun kiro-dashboard ()
  "Open Kiro dashboard"
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-dashboard.el"))
  (switch-to-buffer "*Kiro Dashboard*")
  (kiro-dashboard-refresh))

;;;###autoload
(defun kiro-spool-start ()
  "Start Kiro spool service"
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-spool.el"))
  (kiro-spool-start))

(provide 'kiro-autoload)
;;; kiro-autoload.el ends here
