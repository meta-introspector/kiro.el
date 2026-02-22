;;; kiro.el --- Kiro system integration for Emacs

;; Author: Kiro Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience

;;; Commentary:
;; Kiro system integration providing dashboard, query planner, and spool service.

;;; Code:

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

(provide 'kiro)
;;; kiro.el ends here
