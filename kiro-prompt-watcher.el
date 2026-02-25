;;; kiro-prompt-watcher.el --- Auto-surface buffers waiting for input
;;; DASL-T42-FRACTRAN-ZKP: [(523 . 524) (151 . 152) (499 . 500)]
;;; Version: 1.0.0

(defvar kiro-prompt-watcher-timer nil
  "Timer for watching prompts.")

(defvar kiro-prompt-watcher-last-check nil
  "Last buffer that had a prompt.")

(defun kiro-prompt-watcher-has-prompt-p (buf)
  "Check if buffer has waiting prompt."
  (with-current-buffer buf
    (and (or (derived-mode-p 'shell-mode 'eshell-mode 'term-mode 'comint-mode)
             (string-match-p "kiro\\|dasl\\|task" (buffer-name)))
         (save-excursion
           (goto-char (point-max))
           (or (re-search-backward "\\[y/n\\]\\s-*$" (max (point-min) (- (point-max) 200)) t)
               (re-search-backward "\\?\\s-*$" (max (point-min) (- (point-max) 200)) t)
               (re-search-backward ":\\s-*$" (max (point-min) (- (point-max) 200)) t))))))

(defun kiro-prompt-watcher-check ()
  "Check all Kiro buffers for prompts and surface if found."
  (let ((prompt-buffers '()))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (kiro-prompt-watcher-has-prompt-p buf))
        (push buf prompt-buffers)))
    
    (when prompt-buffers
      (let ((buf (car prompt-buffers)))
        (unless (eq buf kiro-prompt-watcher-last-check)
          (setq kiro-prompt-watcher-last-check buf)
          (switch-to-buffer buf)
          (goto-char (point-max))
          (message "⚠️  Prompt waiting in: %s" (buffer-name buf)))))))

;;;###autoload
(defun kiro-prompt-watcher-start ()
  "Start watching for prompts."
  (interactive)
  (when kiro-prompt-watcher-timer
    (cancel-timer kiro-prompt-watcher-timer))
  (setq kiro-prompt-watcher-timer
        (run-with-timer 2 2 'kiro-prompt-watcher-check))
  (message "✅ Prompt watcher started (checking every 2s)"))

;;;###autoload
(defun kiro-prompt-watcher-stop ()
  "Stop watching for prompts."
  (interactive)
  (when kiro-prompt-watcher-timer
    (cancel-timer kiro-prompt-watcher-timer)
    (setq kiro-prompt-watcher-timer nil)
    (message "✅ Prompt watcher stopped")))

(provide 'kiro-prompt-watcher)
;;; kiro-prompt-watcher.el ends here
