;;; kiro-task-monster.el --- Monster symmetry task naming
;;; DASL-T42-FRACTRAN-ZKP: [(523 . 524) (151 . 152) (499 . 500)]
;;; Version: 1.0.0

(require 'kiro-fractran-cid)
(require 'kiro-monster-tree)
(require 'kiro-task-mode)

(defun kiro-task-monster-name (task-name)
  "Generate Monster symmetry name for task."
  (let* ((cid (logand (sxhash task-name) #xFFFF))
         (class (kiro-monster-tree-classify cid))
         (shard (plist-get class :shard))
         (seph (cdr (plist-get class :sephirah)))
         (tarot (cdr (plist-get class :tarot)))
         (hecke (plist-get class :hecke))
         (sanitized (replace-regexp-in-string " " "-" task-name)))
    (format "task-%s-Sh%d-%s-T%d.kiro.org" sanitized shard seph hecke)))

(defun kiro-task-monster-classify (task-name)
  "Show full Monster classification for task."
  (let* ((cid (logand (sxhash task-name) #xFFFF))
         (class (kiro-monster-tree-classify cid)))
    class))

;;;###autoload
(defun kiro-task-new-monster (task-name)
  "Create new task with Monster symmetry naming."
  (interactive
   (list (read-string "New task name: " nil 'kiro-task-history)))
  (let* ((monster-name (kiro-task-monster-name task-name))
         (file-path (expand-file-name monster-name kiro-task-directory))
         (class (kiro-task-monster-classify task-name)))
    (find-file file-path)
    (kiro-task-mode)
    (insert (format "#+TITLE: %s\n" task-name))
    (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d")))
    (insert "#+AUTHOR: Kiro Task\n")
    (insert (format "#+MONSTER_CID: 0x%04X\n" (plist-get class :cid)))
    (insert (format "#+MONSTER_SHARD: %d/71\n" (plist-get class :shard)))
    (insert (format "#+SEPHIRAH: %s\n" (cdr (plist-get class :sephirah))))
    (insert (format "#+TAROT: %s\n" (cdr (plist-get class :tarot))))
    (insert (format "#+BOTT: %s\n" (cdr (plist-get class :bott))))
    (insert (format "#+HECKE: T_%d\n" (plist-get class :hecke)))
    (insert (format "#+MONSTER_CLASSES: %s\n\n" 
                    (mapconcat #'identity (plist-get class :monster-classes) ", ")))
    (insert "* Task\n\n")
    (message "✅ Created Monster task: %s" monster-name)))

;;;###autoload
(defun kiro-task-rename-monster (task-name)
  "Rename current buffer with Monster symmetry."
  (interactive
   (list (read-string "Task name: " nil 'kiro-task-history)))
  (let* ((monster-name (kiro-task-monster-name task-name))
         (file-path (expand-file-name monster-name kiro-task-directory))
         (class (kiro-task-monster-classify task-name)))
    (unless (file-exists-p kiro-task-directory)
      (make-directory kiro-task-directory t))
    (rename-buffer monster-name t)
    (set-visited-file-name file-path)
    (kiro-task-mode)
    (save-excursion
      (goto-char (point-min))
      (unless (looking-at "^#\\+MONSTER_CID:")
        (insert (format "#+MONSTER_CID: 0x%04X\n" (plist-get class :cid)))
        (insert (format "#+MONSTER_SHARD: %d/71\n" (plist-get class :shard)))
        (insert (format "#+SEPHIRAH: %s\n" (cdr (plist-get class :sephirah))))
        (insert (format "#+TAROT: %s\n" (cdr (plist-get class :tarot))))
        (insert (format "#+HECKE: T_%d\n\n" (plist-get class :hecke)))))
    (message "✅ Renamed to Monster task: %s (Shard %d, %s)" 
             monster-name 
             (plist-get class :shard)
             (cdr (plist-get class :sephirah)))))

;;;###autoload
(defun kiro-task-list-monster ()
  "List all tasks with Monster classifications."
  (interactive)
  (with-current-buffer (get-buffer-create "*Kiro-Tasks-Monster*")
    (erase-buffer)
    (insert "╔═══════════════════════════════════════════════════════╗\n")
    (insert "║         KIRO TASKS - MONSTER CLASSIFICATION           ║\n")
    (insert "╚═══════════════════════════════════════════════════════╝\n\n")
    
    (let ((tasks (directory-files kiro-task-directory nil "^task-.*\\.kiro\\.org$")))
      (insert (format "Total tasks: %d\n\n" (length tasks)))
      (dolist (task tasks)
        (let* ((name (replace-regexp-in-string "^task-\\|.kiro.org$" "" task))
               (cid (logand (sxhash name) #xFFFF))
               (class (kiro-monster-tree-classify cid)))
          (insert (format "%-40s │ Sh:%2d %10s T_%d\n"
                          task
                          (plist-get class :shard)
                          (cdr (plist-get class :sephirah))
                          (plist-get class :hecke))))))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(provide 'kiro-task-monster)
;;; kiro-task-monster.el ends here
