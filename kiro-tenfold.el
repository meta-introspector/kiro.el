;;; kiro-tenfold.el --- Map buffers to 10-fold way by content

(require 'kiro-meta-mode)

(defvar kiro-tenfold-patterns
  '((0 . ("vacuum" "empty" "void" "nil"))
    (1 . ("binary" "bool" "true" "false" "0" "1" "bit"))
    (2 . ("pair" "dual" "double" "two" "particle" "wave"))
    (3 . ("triple" "three" "rdf" "time" "past" "present" "future"))
    (4 . ("quad" "four" "force" "spacetime"))
    (5 . ("penta" "five" "golden" "phi" "space"))
    (6 . ("hex" "six" "honey" "benzene"))
    (7 . ("hept" "seven" "disk" "octonion" "fano"))
    (8 . ("oct" "eight" "byte" "gluon"))
    (9 . ("nine" "su3" "gell-mann" "matrix" "consciousness")))
  "Content patterns for 10-fold classification")

(defvar kiro-tenfold-emoji
  '((0 . "∅") (1 . "⚡") (2 . "⚖️") (3 . "⏰") (4 . "🌌")
    (5 . "✨") (6 . "🔷") (7 . "🎭") (8 . "🎯") (9 . "🌀"))
  "10-fold way emojis")

(defun kiro-tenfold-classify (buffer)
  "Classify BUFFER content into 10-fold way (0-9)"
  (with-current-buffer buffer
    (let ((content (downcase (buffer-substring-no-properties (point-min) (min (point-max) 10000))))
          (scores (make-vector 10 0)))
      ;; Score each level
      (dotimes (level 10)
        (dolist (pattern (cdr (assoc level kiro-tenfold-patterns)))
          (when (string-match-p pattern content)
            (aset scores level (1+ (aref scores level))))))
      ;; Return level with highest score, or hash-based if tie
      (let ((max-score (seq-max scores)))
        (if (> max-score 0)
            (seq-position scores max-score)
          (mod (sxhash (buffer-name buffer)) 10))))))

(defun kiro-tenfold-map-all ()
  "Map all buffers to 10-fold way"
  (interactive)
  (let ((mapping (make-hash-table :test 'equal)))
    (dolist (buf (buffer-list))
      (unless (string-match-p "^[ *]" (buffer-name buf))
        (let ((level (kiro-tenfold-classify buf)))
          (puthash (buffer-name buf) level mapping))))
    mapping))

(defun kiro-tenfold-rename-all ()
  "Rename all buffers with 10-fold prefix"
  (interactive)
  (let ((count 0)
        (mapping (kiro-tenfold-map-all)))
    (maphash
     (lambda (name level)
       (let* ((buf (get-buffer name))
              (emoji (cdr (assoc level kiro-tenfold-emoji)))
              (new-name (format "%s-kiro-%d-%s" emoji level name)))
         (when buf
           (with-current-buffer buf
             (rename-buffer new-name t)
             (setq count (1+ count))))))
     mapping)
    (message "✅ Renamed %d buffers with 10-fold classification" count)))

(defun kiro-tenfold-report ()
  "Generate 10-fold classification report"
  (interactive)
  (let ((mapping (kiro-tenfold-map-all))
        (counts (make-vector 10 0))
        (report (get-buffer-create "*Kiro 10-Fold Report*")))
    (maphash (lambda (name level)
               (aset counts level (1+ (aref counts level))))
             mapping)
    (with-current-buffer report
      (erase-buffer)
      (insert "# Kiro 10-Fold Way Buffer Classification\n\n")
      (dotimes (level 10)
        (let ((emoji (cdr (assoc level kiro-tenfold-emoji)))
              (count (aref counts level)))
          (insert (format "## Level %d %s: %d buffers\n\n" level emoji count))
          (maphash (lambda (name buf-level)
                     (when (= buf-level level)
                       (insert (format "- %s\n" name))))
                   mapping)
          (insert "\n")))
      (org-mode)
      (goto-char (point-min)))
    (switch-to-buffer report)))

(provide 'kiro-tenfold)
;;; kiro-tenfold.el ends here
