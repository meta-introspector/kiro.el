;;; kiro-jump.el --- Jump to next active kiro prompt

(defun kiro-jump-to-next-prompt ()
  "Jump to the next buffer with an active kiro prompt."
  (interactive)
  (let* ((shells (seq-filter
                  (lambda (buf)
                    (with-current-buffer buf
                      (and (or (eq major-mode 'shell-mode)
                               (eq major-mode 'eshell-mode)
                               (eq major-mode 'kiro-shell-task-mode))
                           (get-buffer-process buf)
                           (process-live-p (get-buffer-process buf)))))
                  (buffer-list)))
         (current (current-buffer))
         (after (cdr (member current shells)))
         (next (or (car after) (car shells))))
    (if next
        (switch-to-buffer next)
      (message "No active kiro shells found"))))

(global-set-key (kbd "C-c k n") 'kiro-jump-to-next-prompt)

(provide 'kiro-jump)
;;; kiro-jump.el ends here
