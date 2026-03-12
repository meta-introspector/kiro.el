;;; resume-hanging-kiro.el --- Resume hanging.org work in new Kiro session

(defun resume-hanging-kiro ()
  "Resume hanging.org file documentation work in new Kiro shell."
  (interactive)
  (let ((default-directory "~/time/2025/01/18/solfunmeme/"))
    ;; Create shell in work directory
    (shell "*kiro-hanging*")
    ;; Insert kiro-cli command and send
    (insert "kiro-cli chat --resume")
    (comint-send-input)
    (message "Kiro-cli resuming last session in *kiro-hanging* buffer")))

(provide 'resume-hanging-kiro)
;;; resume-hanging-kiro.el ends here
