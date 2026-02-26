;;; kiro-session-manager.el --- Manage kiro CLI sessions -*- lexical-binding: t; -*-

;;; Code:

(defun kiro-session-list ()
  "List all running kiro-cli-chat processes."
  (interactive)
  (let ((sessions '()))
    (with-temp-buffer
      (call-process "ps" nil t nil "aux")
      (goto-char (point-min))
      (while (re-search-forward "kiro-cli-chat" nil t)
        (beginning-of-line)
        (when (looking-at "\\([^ ]+\\) +\\([0-9]+\\) +.* \\([^ ]+\\) +.* \\(/.*kiro-cli-chat.*\\)")
          (push (list :user (match-string 1)
                     :pid (match-string 2)
                     :tty (match-string 3)
                     :cmd (match-string 4))
                sessions))
        (forward-line 1)))
    sessions))

(defun kiro-session-save-all ()
  "Save all kiro session metadata."
  (interactive)
  (let* ((sessions (kiro-session-list))
         (save-dir (format-time-string "~/.kiro/sessions/%Y%m%d_%H%M%S"))
         (count 0))
    (make-directory save-dir t)
    (dolist (session sessions)
      (let ((pid (plist-get session :pid))
            (tty (plist-get session :tty)))
        (with-temp-file (format "%s/session_%s.txt" save-dir pid)
          (insert (format "PID: %s\n" pid))
          (insert (format "TTY: %s\n" tty))
          (insert (format "Saved: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S"))))
        (setq count (1+ count))))
    (message "✅ Saved %d sessions to: %s" count save-dir)))

(defun kiro-session-rename-all ()
  "Rename all kiro session terminals."
  (interactive)
  (let ((sessions (kiro-session-list))
        (count 0))
    (dolist (session sessions)
      (let ((pid (plist-get session :pid))
            (tty (plist-get session :tty)))
        (shell-command (format "echo -ne \"\\033]0;Kiro: solfunmeme (PID %s)\\007\" > /dev/%s" pid tty))
        (setq count (1+ count))))
    (message "✅ Renamed %d terminal windows" count)))

(defun kiro-session-compact-all ()
  "Create compact summaries of all sessions."
  (interactive)
  (let* ((sessions (kiro-session-list))
         (compact-dir (format-time-string "~/.kiro/compact/%Y%m%d_%H%M%S"))
         (count 0))
    (make-directory compact-dir t)
    (dolist (session sessions)
      (let ((pid (plist-get session :pid)))
        (with-temp-file (format "%s/compact_%s.md" compact-dir pid)
          (insert (format "# Kiro Session %s\n\n" pid))
          (insert (format "**PID**: %s\n" pid))
          (insert (format "**TTY**: %s\n" (plist-get session :tty)))
          (insert (format "**Compacted**: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S"))))
        (setq count (1+ count))))
    (message "✅ Compacted %d sessions to: %s" count compact-dir)))

(defun kiro-session-manage ()
  "Interactive session manager."
  (interactive)
  (with-current-buffer (get-buffer-create "*Kiro Sessions*")
    (erase-buffer)
    (insert "╔═══════════════════════════════════════════════════════╗\n")
    (insert "║         KIRO SESSION MANAGER                         ║\n")
    (insert "╚═══════════════════════════════════════════════════════╝\n\n")
    
    (let ((sessions (kiro-session-list)))
      (insert (format "Active sessions: %d\n\n" (length sessions)))
      (dolist (session sessions)
        (insert (format "PID %s (%s): %s\n"
                       (plist-get session :pid)
                       (plist-get session :tty)
                       (plist-get session :cmd)))))
    
    (insert "\n\nKeys: [s]ave [r]ename [c]ompact [q]uit\n")
    (goto-char (point-min))
    (display-buffer (current-buffer))
    
    (local-set-key (kbd "s") 'kiro-session-save-all)
    (local-set-key (kbd "r") 'kiro-session-rename-all)
    (local-set-key (kbd "c") 'kiro-session-compact-all)
    (local-set-key (kbd "q") 'quit-window)))

(provide 'kiro-session-manager)
;;; kiro-session-manager.el ends here
