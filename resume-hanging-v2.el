;;; resume-hanging-kiro.el --- Resume hanging.org work in new Kiro session

(defun resume-hanging-kiro ()
  "Resume hanging.org file documentation work in new Kiro shell."
  (interactive)
  (let ((default-directory "~/time/2025/01/18/solfunmeme/"))
    ;; Create shell in work directory
    (shell "*kiro-hanging*")
    ;; Insert command and newline
    (goto-char (point-max))
    (insert "kiro-cli chat --resume\n")
    (message "Kiro-cli resuming last session in *kiro-hanging* buffer")))

(provide 'resume-hanging-kiro)
;;; resume-hanging-kiro.el ends here
