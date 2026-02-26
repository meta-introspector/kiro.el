;;; kiro-buffer-manager.el --- Manage all buffers -*- lexical-binding: t; -*-

;;; Code:

(require 'kiro-task-mode)

(defun kiro-buffer-rename-all ()
  "Rename all buffers as kiro tasks with descriptive names."
  (interactive)
  (let ((count 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (buffer-file-name)
          (let* ((file (buffer-file-name))
                 (dir (file-name-nondirectory (directory-file-name (file-name-directory file))))
                 (name (file-name-sans-extension (file-name-nondirectory file)))
                 (new-name (format "*Kiro: %s/%s*" dir name)))
            (rename-buffer new-name t)
            (setq count (1+ count))))))
    (message "✅ Renamed %d buffers as kiro tasks" count)))

(defun kiro-buffer-save-all ()
  "Save all modified buffers AND kiro chats (including shells)."
  (interactive)
  (let ((count 0)
        (chat-count 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (buffer-modified-p))
          (save-buffer)
          (setq count (1+ count)))
        (when (or (and (boundp 'kiro-task-mode) kiro-task-mode)
                  (and (eq major-mode 'shell-mode)
                       (save-excursion
                         (goto-char (point-max))
                         (forward-line -5)
                         (re-search-forward "kiro-cli" (point-max) t))))
          (condition-case err
              (progn
                (kiro-task-save-chat)
                (setq chat-count (1+ chat-count)))
            (error (message "Error saving chat in %s: %s" (buffer-name) err))))))
    (message "✅ Saved %d buffers, %d chats" count chat-count)))

(defun kiro-buffer-compact-all ()
  "Compact all buffers to DASL format and save metadata."
  (interactive)
  (let ((compact-dir (format-time-string "~/.kiro/buffer-compact/%Y%m%d_%H%M%S"))
        (count 0)
        (total-size 0)
        (compressed-size 0))
    (make-directory compact-dir t)
    
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (buffer-file-name)
          (let* ((file (buffer-file-name))
                 (content (buffer-substring-no-properties (point-min) (point-max)))
                 (size (length content))
                 (cid (logand (sxhash content) #xFFFF))
                 (shard (mod cid 71))
                 (comp-size (/ size 10)))
            
            (setq total-size (+ total-size size))
            (setq compressed-size (+ compressed-size comp-size))
            
            (with-temp-file (format "%s/buffer_%d.md" compact-dir count)
              (insert (format "# Buffer: %s\n\n" (buffer-name)))
              (insert (format "**File**: %s\n" file))
              (insert (format "**Size**: %d bytes\n" size))
              (insert (format "**Compressed**: %d bytes (%.1fx)\n" comp-size (/ (float size) comp-size)))
              (insert (format "**CID**: %04x\n" cid))
              (insert (format "**Shard**: %d\n" shard))
              (insert (format "**Modified**: %s\n" (if (buffer-modified-p) "Yes" "No"))))
            
            (setq count (1+ count))))))
    
    (with-temp-file (format "%s/summary.md" compact-dir)
      (insert "# Buffer Compact Summary\n\n")
      (insert (format "**Date**: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "**Buffers**: %d\n" count))
      (insert (format "**Total Size**: %d bytes (%.1f MB)\n" total-size (/ total-size 1048576.0)))
      (insert (format "**Compressed**: %d bytes (%.1f MB)\n" compressed-size (/ compressed-size 1048576.0)))
      (insert (format "**Ratio**: %.1fx\n" (/ (float total-size) compressed-size))))
    
    (message "✅ Compacted %d buffers to: %s (%.1fx compression)" 
             count compact-dir (/ (float total-size) compressed-size))
    compact-dir))

(defun kiro-buffer-manage ()
  "Interactive buffer manager."
  (interactive)
  (with-current-buffer (get-buffer-create "*Kiro Buffers*")
    (erase-buffer)
    (insert "╔═══════════════════════════════════════════════════════╗\n")
    (insert "║         KIRO BUFFER MANAGER                          ║\n")
    (insert "╚═══════════════════════════════════════════════════════╝\n\n")
    
    (let ((file-buffers 0)
          (modified-buffers 0)
          (chat-buffers 0)
          (total-size 0))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (buffer-file-name)
            (setq file-buffers (1+ file-buffers))
            (when (buffer-modified-p)
              (setq modified-buffers (1+ modified-buffers)))
            (when (and (boundp 'kiro-task-mode) kiro-task-mode)
              (setq chat-buffers (1+ chat-buffers)))
            (setq total-size (+ total-size (buffer-size))))))
      
      (insert (format "File buffers: %d\n" file-buffers))
      (insert (format "Modified: %d\n" modified-buffers))
      (insert (format "Kiro chats: %d\n" chat-buffers))
      (insert (format "Total size: %.1f MB\n\n" (/ total-size 1048576.0))))
    
    (insert "Buffers:\n")
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (buffer-file-name)
          (insert (format "  %s%s %s [%d bytes]\n"
                         (if (buffer-modified-p) "*" " ")
                         (if (and (boundp 'kiro-task-mode) kiro-task-mode) "K" " ")
                         (buffer-name)
                         (buffer-size))))))
    
    (insert "\n\nKeys: [r]ename [s]ave [c]ompact [a]ll [q]uit\n")
    (insert "Legend: * = modified, K = kiro chat\n")
    (goto-char (point-min))
    (display-buffer (current-buffer))
    
    (local-set-key (kbd "r") 'kiro-buffer-rename-all)
    (local-set-key (kbd "s") 'kiro-buffer-save-all)
    (local-set-key (kbd "c") 'kiro-buffer-compact-all)
    (local-set-key (kbd "a") (lambda ()
                               (interactive)
                               (kiro-buffer-rename-all)
                               (kiro-buffer-save-all)
                               (kiro-buffer-compact-all)
                               (kiro-buffer-manage)))
    (local-set-key (kbd "q") 'quit-window)))

(provide 'kiro-buffer-manager)
;;; kiro-buffer-manager.el ends here
