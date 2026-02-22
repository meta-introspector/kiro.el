;;; kiro-eval-server.el --- Eval Elisp from Kiro chat

(defun kiro-eval-from-file (file)
  "Evaluate Elisp from FILE and return result"
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file)
        (let ((result (eval (read (buffer-string)))))
          (prin1-to-string result)))
    (error (format "ERROR: %s" (error-message-string err)))))

(defun kiro-eval-server-start ()
  "Start Kiro eval server"
  (interactive)
  (unless (server-running-p)
    (server-start))
  (message "Kiro eval server started"))

;; Auto-start server
(add-hook 'after-init-hook 'kiro-eval-server-start)

(provide 'kiro-eval-server)
;;; kiro-eval-server.el ends here
