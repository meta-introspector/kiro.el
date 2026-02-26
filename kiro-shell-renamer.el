;;; kiro-shell-renamer.el --- Rename shell buffers -*- lexical-binding: t; -*-

;;; Code:

(require 'org)

(defvar kiro-shell-rename-buffer "*Kiro Shell Names*")

(defun kiro-shell-extract-task-name (buffer)
  "Extract task name from shell BUFFER content."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (forward-line -50)
      (let ((content (buffer-substring-no-properties (point) (point-max))))
        (cond
         ((string-match "\\(/[^/]+/[^/]+\\)\\$" content)
          (file-name-nondirectory (directory-file-name (match-string 1 content))))
         ((string-match "cd \\([^ \n]+\\)" content)
          (file-name-nondirectory (match-string 1 content)))
         ((string-match "\\(cargo\\|npm\\|make\\|git\\) \\([a-z]+\\)" content)
          (format "%s-%s" (match-string 1 content) (match-string 2 content)))
         (t "shell"))))))

(defun kiro-shell-rename-all ()
  "Rename all shell buffers immediately as kiro tasks."
  (interactive)
  (let ((count 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'shell-mode)
          (let* ((task-name (kiro-shell-extract-task-name buf))
                 (new-name (format "*Kiro-Shell: %s*" task-name)))
            (rename-buffer new-name t)
            (local-set-key (kbd "C-c k s") 'kiro-task-save-chat)
            (setq count (1+ count))))))
    (message "✅ Renamed %d shells with C-c k s bound" count)))

(provide 'kiro-shell-renamer)
;;; kiro-shell-renamer.el ends here
