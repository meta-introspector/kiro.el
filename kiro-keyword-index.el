;;; kiro-keyword-index.el --- Keyword indexing for all buffers
;;; DASL-T42-FRACTRAN-ZKP: [(523 . 524) (151 . 152) (499 . 500)]
;;; Version: 1.0.0

(require 'kiro-fractran-cid)

(defun kiro-keyword-extract (text)
  "Extract keywords from text."
  (let ((words (split-string (downcase text) "[^a-z0-9-]+" t))
        (keywords (make-hash-table :test 'equal)))
    (dolist (word words)
      (when (> (length word) 3)  ; Skip short words
        (puthash word (1+ (gethash word keywords 0)) keywords)))
    keywords))

(defun kiro-keyword-index-buffer (buf)
  "Index keywords in buffer."
  (with-current-buffer buf
    (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
           (keywords (kiro-keyword-extract text))
           (cid (logand (sxhash (buffer-name buf)) #xFFFF))
           (shard (mod cid 71)))
      (list :buffer (buffer-name buf)
            :cid cid
            :shard shard
            :size (buffer-size)
            :keywords keywords))))

(defun kiro-keyword-index-all ()
  "Index all buffers."
  (let ((indices '()))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (push (kiro-keyword-index-buffer buf) indices)))
    indices))

(defun kiro-keyword-search (keyword indices)
  "Search for keyword in indices."
  (let ((results '()))
    (dolist (idx indices)
      (let ((kw-hash (plist-get idx :keywords)))
        (when (gethash keyword kw-hash)
          (push (list :buffer (plist-get idx :buffer)
                      :count (gethash keyword kw-hash)
                      :shard (plist-get idx :shard))
                results))))
    (seq-sort (lambda (a b) (> (plist-get a :count) (plist-get b :count))) results)))

;;;###autoload
(defun kiro-keyword-index-report ()
  "Generate keyword index report."
  (interactive)
  (let* ((indices (kiro-keyword-index-all))
         (search-terms '("dasl" "cbor" "fractran" "emacs" "monster" "nix"))
         (all-keywords (make-hash-table :test 'equal)))
    
    ;; Collect all keywords
    (dolist (idx indices)
      (maphash (lambda (kw count)
                 (puthash kw (+ count (gethash kw all-keywords 0)) all-keywords))
               (plist-get idx :keywords)))
    
    (with-current-buffer (get-buffer-create "*Kiro-Keyword-Index*")
      (erase-buffer)
      (insert "╔═══════════════════════════════════════════════════════╗\n")
      (insert "║         KIRO KEYWORD INDEX REPORT                     ║\n")
      (insert "╚═══════════════════════════════════════════════════════╝\n\n")
      (insert (format "Indexed: %d buffers\n" (length indices)))
      (insert (format "Date: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      
      (insert "SEARCH RESULTS:\n\n")
      (dolist (term search-terms)
        (let ((results (kiro-keyword-search term indices)))
          (insert (format "%-10s: %d buffers\n" term (length results)))
          (dolist (r (seq-take results 5))
            (insert (format "  %3d × %-40s (Shard %2d)\n"
                            (plist-get r :count)
                            (plist-get r :buffer)
                            (plist-get r :shard))))))
      
      (insert "\nTOP KEYWORDS (all buffers):\n")
      (let ((sorted (seq-sort (lambda (a b) (> (cdr a) (cdr b)))
                              (let (pairs)
                                (maphash (lambda (k v) (push (cons k v) pairs)) all-keywords)
                                pairs))))
        (dolist (kw (seq-take sorted 20))
          (insert (format "  %4d × %s\n" (cdr kw) (car kw)))))
      
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'kiro-keyword-index)
;;; kiro-keyword-index.el ends here
