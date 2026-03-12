;;; resume-hanging-kiro.el --- Resume hanging.org work in new Kiro session

(defun resume-hanging-kiro ()
  "Resume hanging.org file documentation work in new Kiro shell."
  (interactive)
  (let ((work-dir "~/time/2025/01/18/solfunmeme")
        (prompt "continue documenting files from FILE_DOCUMENTATION.md. Complete documentation for all Python scripts, test files, and update README.md"))
    
    ;; Create new shell buffer
    (let ((shell-buffer (generate-new-buffer "*kiro-hanging*")))
      (with-current-buffer shell-buffer
        (shell shell-buffer)
        (goto-char (point-max))
        
        ;; Change to work directory
        (insert (format "cd %s" work-dir))
        (comint-send-input)
        (sleep-for 0.5)
        
        ;; Launch kiro-cli
        (insert "kiro-cli")
        (comint-send-input)
        (sleep-for 2)
        
        ;; Send the prompt
        (insert prompt)
        (message "Kiro hanging work resumed in buffer: %s" (buffer-name shell-buffer)))
      
      ;; Switch to the new shell buffer
      (switch-to-buffer shell-buffer))))

(provide 'resume-hanging-kiro)
;;; resume-hanging-kiro.el ends here
