;;; kiro-monster-rename.el --- Domain-aware Monster buffer renaming
;;; DASL-T42-FRACTRAN-ZKP: [(523 . 524) (151 . 152) (499 . 500)]
;;; Version: 1.0.0

(require 'kiro-fractran-cid)

(defun kiro-monster-extract-domain (name)
  "Extract domain terms from buffer name."
  (let* ((clean (replace-regexp-in-string "^\\*\\|\\*$\\|^task-\\|.kiro.org$\\|.org$\\|<.*>$" "" name))
         (words (split-string clean "[-_ ]"))
         (terms (seq-filter (lambda (w) (> (length w) 2)) words)))
    (mapconcat #'identity terms "-")))

(defun kiro-monster-rename-buffer (buf)
  "Rename buffer with domain-aware Monster name."
  (let* ((old-name (buffer-name buf))
         (cid (logand (sxhash old-name) #xFFFF))
         (shard (mod cid 71))
         (hecke-ops '(2 3 5 7 11 13 17 19 23 29 31 41 47 59 71))
         (hecke (nth (mod cid 15) hecke-ops))
         (domain (kiro-monster-extract-domain old-name))
         (new-name (format "*%s-Sh%d-T%d*" domain shard hecke)))
    (with-current-buffer buf
      (when (not (string= old-name new-name))
        (rename-buffer new-name t)
        (cons old-name new-name)))))

;;;###autoload
(defun kiro-monster-rename-all ()
  "Rename all Kiro/task buffers with Monster symmetry."
  (interactive)
  (let ((renamed '())
        (patterns '("kiro" "Monster" "FRACTRAN" "task-" "CROSS" "ORBIFOLD" "bott")))
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        (when (seq-some (lambda (p) (string-match-p p name)) patterns)
          (let ((result (kiro-monster-rename-buffer buf)))
            (when result
              (push result renamed))))))
    (if renamed
        (progn
          (message "✅ Renamed %d buffers" (length renamed))
          (with-current-buffer (get-buffer-create "*Monster-Rename-Log*")
            (erase-buffer)
            (insert "Monster Buffer Renaming Log\n")
            (insert (format "Date: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
            (dolist (r renamed)
              (insert (format "%-40s → %s\n" (car r) (cdr r))))
            (display-buffer (current-buffer)))
          renamed)
      (message "No buffers to rename"))))

;;;###autoload
(defun kiro-monster-rename-current ()
  "Rename current buffer with Monster symmetry."
  (interactive)
  (let ((result (kiro-monster-rename-buffer (current-buffer))))
    (if result
        (message "✅ Renamed: %s → %s" (car result) (cdr result))
      (message "Buffer name unchanged"))))

(provide 'kiro-monster-rename)
;;; kiro-monster-rename.el ends here
