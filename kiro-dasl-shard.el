;;; kiro-dasl-shard.el --- Shard buffer contents into 71 Monster shards -*- lexical-binding: t; -*-

;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Shard large buffers into 71 pieces based on Monster group symmetry
;; Each shard gets its own FRACTRAN encoding and RDFa metadata

;;; Code:

(require 'kiro-fractran-cid)
(require 'kiro-monster-tree)
(require 'kiro-dasl-compressor)

(defun kiro-dasl-shard-buffer (buffer &optional shard-count)
  "Shard BUFFER into SHARD-COUNT Monster shards (default 71).
Can also use 59 or 47 (Monster primes)."
  (let* ((count (or shard-count 71))
         (shards '()))
    (with-current-buffer buffer
      (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
             (size (length content))
             (shard-size (max 1 (/ size count))))
        
        (dotimes (i count)
          (let* ((start (* i shard-size))
                 (end (min (+ start shard-size) size))
                 (chunk (substring content start end))
                 (cid (logand (sxhash chunk) #xFFFF))
                 (class (kiro-monster-tree-classify cid))
                 (structure (kiro-dasl-extract-structure chunk major-mode))
                 (fractran (kiro-dasl-to-fractran structure))
                 ;; Multi-level analysis
                 (sum (apply #'+ (mapcar (lambda (f) (car f)) fractran)))
                 (mod16 (mod sum 16))
                 (mod10 (mod sum 10))
                 (mod8 (mod sum 8))
                 (hecke-idx (mod sum 15))
                 (hecke-prime (nth hecke-idx '(2 3 5 7 11 13 17 19 23 29 31 41 47 59 71))))
            (push (list :shard i
                        :start start
                        :end end
                        :size (- end start)
                        :cid cid
                        :class class
                        :fractran fractran
                        :sum sum
                        :mod16 mod16
                        :mod10 mod10
                        :mod8 mod8
                        :hecke hecke-prime
                        :ratio (if (> (length fractran) 0)
                                   (/ (float (- end start)) (length fractran))
                                 0))
                  shards)))))
    (nreverse shards)))

(defun kiro-dasl-shard-report (buffer &optional shard-count)
  "Generate sharding report for BUFFER using SHARD-COUNT (71, 59, or 47)."
  (interactive "bBuffer: \nnShard count (71/59/47): ")
  (let* ((count (or shard-count 71))
         (buf (get-buffer buffer))
         (shards (kiro-dasl-shard-buffer buf count))
         (total-size (with-current-buffer buf (buffer-size)))
         (total-fractran (apply #'+ (mapcar (lambda (s) (length (plist-get s :fractran))) shards))))
    
    (with-current-buffer (get-buffer-create "*DASL-Sharding*")
      (erase-buffer)
      (insert "╔═══════════════════════════════════════════════════════╗\n")
      (insert (format "║     MONSTER SHARDING: %-30s ║\n" (truncate-string-to-width (buffer-name buf) 30)))
      (insert "╚═══════════════════════════════════════════════════════╝\n\n")
      
      (insert (format "Buffer: %s\n" (buffer-name buf)))
      (insert (format "Total size: %d bytes\n" total-size))
      (insert (format "Shards: %d (Monster prime)\n" count))
      (insert (format "Avg shard: %d bytes\n" (/ total-size count)))
      (insert (format "FRACTRAN: %d fractions\n" total-fractran))
      (insert (format "Compression: %.1fx\n\n" (/ (float total-size) total-fractran)))
      
      (insert "Shard breakdown:\n")
      (insert "─────────────────────────────────────────────────────\n")
      (insert "Sh# | Size  | Frac | Sum   | %16 %10 %8 | Hecke\n")
      (insert "────┼───────┼──────┼───────┼────────────┼──────\n")
      
      (dolist (shard shards)
        (let ((class (plist-get shard :class)))
          (insert (format "%3d | %5d | %4d | %5d | %2d  %2d  %d | T_%d\n"
                          (plist-get shard :shard)
                          (plist-get shard :size)
                          (length (plist-get shard :fractran))
                          (plist-get shard :sum)
                          (plist-get shard :mod16)
                          (plist-get shard :mod10)
                          (plist-get shard :mod8)
                          (plist-get shard :hecke)))))
      
      (insert "\n\nMod 16 distribution:\n")
      (insert "─────────────────────────────────────────────────────\n")
      (let ((mod16-counts (make-hash-table)))
        (dolist (shard shards)
          (let ((m (plist-get shard :mod16)))
            (puthash m (1+ (gethash m mod16-counts 0)) mod16-counts)))
        (dotimes (i 16)
          (let ((count (gethash i mod16-counts 0)))
            (when (> count 0)
              (insert (format "%2d: %s (%d)\n" i (make-string count ?█) count))))))
      
      (insert "\n\nMod 10 distribution:\n")
      (insert "─────────────────────────────────────────────────────\n")
      (let ((mod10-counts (make-hash-table)))
        (dolist (shard shards)
          (let ((m (plist-get shard :mod10)))
            (puthash m (1+ (gethash m mod10-counts 0)) mod10-counts)))
        (dotimes (i 10)
          (let ((count (gethash i mod10-counts 0)))
            (when (> count 0)
              (insert (format "%d: %s (%d)\n" i (make-string count ?█) count))))))
      
      (insert "\n\nMod 8 distribution:\n")
      (insert "─────────────────────────────────────────────────────\n")
      (let ((mod8-counts (make-hash-table)))
        (dolist (shard shards)
          (let ((m (plist-get shard :mod8)))
            (puthash m (1+ (gethash m mod8-counts 0)) mod8-counts)))
        (dotimes (i 8)
          (let ((count (gethash i mod8-counts 0)))
            (when (> count 0)
              (insert (format "%d: %s (%d)\n" i (make-string count ?█) count))))))
      
      (insert "\n\nHecke operator distribution:\n")
      (insert "─────────────────────────────────────────────────────\n")
      (let ((hecke-counts (make-hash-table)))
        (dolist (shard shards)
          (let ((h (plist-get shard :hecke)))
            (puthash h (1+ (gethash h hecke-counts 0)) hecke-counts)))
        (dolist (prime '(2 3 5 7 11 13 17 19 23 29 31 41 47 59 71))
          (let ((count (gethash prime hecke-counts 0)))
            (when (> count 0)
              (insert (format "T_%2d: %s (%d)\n" prime (make-string count ?█) count))))))
      
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun kiro-dasl-shard-multilevel (buffer)
  "Shard BUFFER at 3 levels: 71, 59, 47."
  (let ((level1 (kiro-dasl-shard-buffer buffer 71))
        (level2 (kiro-dasl-shard-buffer buffer 59))
        (level3 (kiro-dasl-shard-buffer buffer 47)))
    (list :level1 level1 :level2 level2 :level3 level3)))

(defun kiro-dasl-shard-multilevel-report (buffer)
  "Generate 3-level sharding report for BUFFER."
  (interactive "bBuffer: ")
  (let* ((buf (get-buffer buffer))
         (result (kiro-dasl-shard-multilevel buf))
         (level1 (plist-get result :level1))
         (level2 (plist-get result :level2))
         (level3 (plist-get result :level3))
         (total-size (with-current-buffer buf (buffer-size))))
    
    (with-current-buffer (get-buffer-create "*DASL-Multilevel*")
      (erase-buffer)
      (insert "╔═══════════════════════════════════════════════════════╗\n")
      (insert (format "║   3-LEVEL SHARDING: %-30s ║\n" (truncate-string-to-width (buffer-name buf) 30)))
      (insert "╚═══════════════════════════════════════════════════════╝\n\n")
      
      (insert (format "Buffer: %s\n" (buffer-name buf)))
      (insert (format "Total size: %d bytes\n\n" total-size))
      
      ;; Level 1: 71 shards
      (insert "═══ LEVEL 1: 71 SHARDS (Conjugacy Classes) ═══\n\n")
      (kiro-dasl-insert-level-summary level1 71)
      
      ;; Level 2: 59 shards
      (insert "\n═══ LEVEL 2: 59 SHARDS (Monster Prime) ═══\n\n")
      (kiro-dasl-insert-level-summary level2 59)
      
      ;; Level 3: 47 shards
      (insert "\n═══ LEVEL 3: 47 SHARDS (Monster Prime) ═══\n\n")
      (kiro-dasl-insert-level-summary level3 47)
      
      ;; Cross-level analysis
      (insert "\n═══ CROSS-LEVEL ANALYSIS ═══\n\n")
      (kiro-dasl-insert-cross-analysis level1 level2 level3)
      
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun kiro-dasl-insert-level-summary (shards count)
  "Insert summary for one level of shards."
  (let ((total-fractran (apply #'+ (mapcar (lambda (s) (length (plist-get s :fractran))) shards)))
        (avg-size (/ (apply #'+ (mapcar (lambda (s) (plist-get s :size)) shards)) count))
        (sum-total (apply #'+ (mapcar (lambda (s) (plist-get s :sum)) shards))))
    
    (insert (format "Shards: %d\n" count))
    (insert (format "Avg size: %d bytes\n" avg-size))
    (insert (format "FRACTRAN: %d fractions\n" total-fractran))
    (insert (format "Total sum: %d\n\n" sum-total))
    
    ;; Mod distributions
    (insert "Mod 16: ")
    (let ((counts (make-hash-table)))
      (dolist (s shards)
        (puthash (plist-get s :mod16) (1+ (gethash (plist-get s :mod16) counts 0)) counts))
      (maphash (lambda (k v) (insert (format "%d:%d " k v))) counts))
    (insert "\n")
    
    (insert "Mod 10: ")
    (let ((counts (make-hash-table)))
      (dolist (s shards)
        (puthash (plist-get s :mod10) (1+ (gethash (plist-get s :mod10) counts 0)) counts))
      (maphash (lambda (k v) (insert (format "%d:%d " k v))) counts))
    (insert "\n")
    
    (insert "Mod 8:  ")
    (let ((counts (make-hash-table)))
      (dolist (s shards)
        (puthash (plist-get s :mod8) (1+ (gethash (plist-get s :mod8) counts 0)) counts))
      (maphash (lambda (k v) (insert (format "%d:%d " k v))) counts))
    (insert "\n")
    
    (insert "Hecke:  ")
    (let ((counts (make-hash-table)))
      (dolist (s shards)
        (puthash (plist-get s :hecke) (1+ (gethash (plist-get s :hecke) counts 0)) counts))
      (maphash (lambda (k v) (insert (format "T_%d:%d " k v))) counts))
    (insert "\n")))

(defun kiro-dasl-insert-cross-analysis (l1 l2 l3)
  "Insert cross-level analysis."
  (let ((sum1 (apply #'+ (mapcar (lambda (s) (plist-get s :sum)) l1)))
        (sum2 (apply #'+ (mapcar (lambda (s) (plist-get s :sum)) l2)))
        (sum3 (apply #'+ (mapcar (lambda (s) (plist-get s :sum)) l3))))
    
    (insert (format "Level 1 sum: %d\n" sum1))
    (insert (format "Level 2 sum: %d\n" sum2))
    (insert (format "Level 3 sum: %d\n\n" sum3))
    
    (let ((total (+ sum1 sum2 sum3)))
      (insert (format "Total sum: %d\n" total))
      (insert (format "Mod 16: %d\n" (mod total 16)))
      (insert (format "Mod 10: %d\n" (mod total 10)))
      (insert (format "Mod 8:  %d\n" (mod total 8)))
      (insert (format "Hecke:  T_%d\n" (nth (mod total 15) '(2 3 5 7 11 13 17 19 23 29 31 41 47 59 71)))))))

(defun kiro-dasl-shard-current-buffer-multilevel ()
  "Shard current buffer at 3 levels."
  (interactive)
  (kiro-dasl-shard-multilevel-report (current-buffer)))

(provide 'kiro-dasl-shard)
;;; kiro-dasl-shard.el ends here
