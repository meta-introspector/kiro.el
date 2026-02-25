;;; org-monster.el --- Org-mode Monster symmetry ontology
;;; DASL-T42-FRACTRAN-ZKP: [(523 . 524) (151 . 152) (499 . 500)]
;;; Version: 1.0.0

(require 'org)
(require 'kiro-fractran-cid)
(require 'kiro-monster-tree)

;;; Org-Monster Ontology

(defconst org-monster-task-types
  '((0 . "TODO")       ; Shard 0 - Kether - Unity/Beginning
    (13 . "DOING")     ; Shard 13 - Sacred 0xD
    (26 . "REVIEW")    ; Shard 26 - 2×13
    (39 . "BLOCKED")   ; Shard 39 - 3×13
    (52 . "WAITING")   ; Shard 52 - 4×13
    (57 . "DONE")      ; Shard 57 - Sacred 0x1F90
    (65 . "CANCELLED"))
  "Task types mapped to Monster shards.")

(defconst org-monster-priorities
  '((1 . "A")   ; 1A - Identity - Highest
    (2 . "B")   ; 2A - Involution
    (3 . "C")   ; 3A - Rotation
    (5 . "D"))  ; 5A - Symmetry
  "Priorities mapped to Monster conjugacy classes.")

(defconst org-monster-tags
  '((:energy . 2)      ; 2^46
    (:time . 3)        ; 3^20
    (:space . 5)       ; 5^9
    (:disk . 7)        ; 7^6
    (:network . 11)    ; 11^2
    (:schedule . 13)   ; 13^3
    (:map . 17)        ; 17
    (:data . 19)       ; 19
    (:aware . 23)      ; 23
    (:phase . 29)      ; 29
    (:chat . 31)       ; 31
    (:cache . 41)      ; 41
    (:register . 47)   ; 47
    (:memory . 59)     ; 59
    (:terminal . 71))  ; 71
  "Tags mapped to Monster primes.")

(defun org-monster-cid-from-heading ()
  "Generate CID from current org heading."
  (let* ((heading (org-get-heading t t t t))
         (todo (org-get-todo-state))
         (priority (org-get-priority))
         (tags (org-get-tags))
         (combined (format "%s|%s|%d|%s" heading todo priority tags)))
    (logand (sxhash combined) #xFFFF)))

(defun org-monster-classify-heading ()
  "Classify current heading with Monster symmetries."
  (let* ((cid (org-monster-cid-from-heading))
         (class (kiro-monster-tree-classify cid))
         (shard (plist-get class :shard)))
    (plist-put class :org-todo (org-monster-shard-to-todo shard))
    (plist-put class :org-priority (org-monster-class-to-priority 
                                     (car (plist-get class :monster-classes))))
    class))

(defun org-monster-shard-to-todo (shard)
  "Map shard to TODO state."
  (or (cdr (assoc shard org-monster-task-types))
      (cond
       ((< shard 13) "TODO")
       ((< shard 39) "DOING")
       ((< shard 57) "REVIEW")
       (t "DONE"))))

(defun org-monster-class-to-priority (class)
  "Map Monster class to priority."
  (cond
   ((string-match "1A" class) "A")
   ((string-match "2A" class) "B")
   ((string-match "3A" class) "C")
   ((string-match "5A" class) "D")
   (t "B")))

(defun org-monster-tag-to-prime (tag)
  "Map tag keyword to Monster prime."
  (cdr (assoc tag org-monster-tags)))

(defun org-monster-prime-to-tag (prime)
  "Map Monster prime to tag keyword."
  (car (rassoc prime org-monster-tags)))

;;;###autoload
(defun org-monster-insert-properties ()
  "Insert Monster properties for current heading."
  (interactive)
  (let ((class (org-monster-classify-heading)))
    (org-set-property "MONSTER_CID" (format "0x%04X" (plist-get class :cid)))
    (org-set-property "MONSTER_SHARD" (format "%d" (plist-get class :shard)))
    (org-set-property "SEPHIRAH" (cdr (plist-get class :sephirah)))
    (org-set-property "TAROT" (cdr (plist-get class :tarot)))
    (org-set-property "BOTT" (cdr (plist-get class :bott)))
    (org-set-property "HECKE" (format "T_%d" (plist-get class :hecke)))
    (org-set-property "MONSTER_CLASSES" 
                      (mapconcat #'identity (plist-get class :monster-classes) ","))
    (message "✅ Monster properties inserted")))

;;;###autoload
(defun org-monster-auto-todo ()
  "Automatically set TODO state based on Monster shard."
  (interactive)
  (let* ((class (org-monster-classify-heading))
         (todo (plist-get class :org-todo)))
    (org-todo todo)
    (message "✅ TODO set to %s (Shard %d)" todo (plist-get class :shard))))

;;;###autoload
(defun org-monster-auto-priority ()
  "Automatically set priority based on Monster class."
  (interactive)
  (let* ((class (org-monster-classify-heading))
         (priority (plist-get class :org-priority)))
    (org-priority (string-to-char priority))
    (message "✅ Priority set to %s (%s)" 
             priority 
             (car (plist-get class :monster-classes)))))

;;;###autoload
(defun org-monster-auto-tags ()
  "Suggest tags based on Monster primes in content."
  (interactive)
  (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
         (suggested '()))
    (dolist (tag-prime org-monster-tags)
      (let ((tag (car tag-prime))
            (prime (cdr tag-prime)))
        (when (string-match-p (format "\\b%d\\b" prime) content)
          (push (substring (symbol-name tag) 1) suggested))))
    (when suggested
      (org-set-tags (seq-uniq (append (org-get-tags) suggested)))
      (message "✅ Added tags: %s" (mapconcat #'identity suggested ", ")))))

;;;###autoload
(defun org-monster-view-tree ()
  "View Monster tree for current heading."
  (interactive)
  (let ((class (org-monster-classify-heading)))
    (with-current-buffer (get-buffer-create "*Org-Monster-Tree*")
      (erase-buffer)
      (insert "╔═══════════════════════════════════════════════════════╗\n")
      (insert "║         ORG-MONSTER HEADING CLASSIFICATION            ║\n")
      (insert "╚═══════════════════════════════════════════════════════╝\n\n")
      (insert (format "Heading: %s\n\n" (org-get-heading t t t t)))
      (insert (kiro-monster-tree-display class))
      (insert "\n\n═══ ORG MAPPINGS ═══\n\n")
      (insert (format "TODO State:  %s (Shard-based)\n" (plist-get class :org-todo)))
      (insert (format "Priority:    [#%s] (Monster class)\n" (plist-get class :org-priority)))
      (insert "\nShard → TODO mapping:\n")
      (insert "  0-12:  TODO\n")
      (insert "  13-38: DOING\n")
      (insert "  39-56: REVIEW\n")
      (insert "  57+:   DONE\n")
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(define-minor-mode org-monster-mode
  "Minor mode for Org-Monster ontology."
  :lighter " 👁️"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c m p") 'org-monster-insert-properties)
            (define-key map (kbd "C-c m t") 'org-monster-auto-todo)
            (define-key map (kbd "C-c m P") 'org-monster-auto-priority)
            (define-key map (kbd "C-c m g") 'org-monster-auto-tags)
            (define-key map (kbd "C-c m v") 'org-monster-view-tree)
            map))

(provide 'org-monster)
;;; org-monster.el ends here
