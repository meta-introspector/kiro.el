;;; kiro-fractran-memory-walk.el --- Monster walk entire Emacs memory
;;; DASL-T42-FRACTRAN-ZKP: [(523 . 524) (151 . 152) (499 . 500)]
;;; Version: 1.0.0
;;; Monster-Class: 13
;;; Shard-ID: 0

(require 'kiro-fractran-cid)

(defun kiro-fractran-memory-cid (obj)
  "Generate CID from any Emacs Lisp object."
  (logand (sxhash (format "%S" obj)) #xFFFF))

(defun kiro-fractran-memory-walk-buffer (buffer)
  "Monster walk a buffer."
  (with-current-buffer buffer
    (let* ((cid (kiro-fractran-memory-cid (buffer-name)))
           (walk (kiro-fractran-cid-hex-walk cid))
           (shard (kiro-fractran-cid-shard cid)))
      (list :type 'buffer
            :name (buffer-name)
            :cid cid
            :hex (format "0x%04X" cid)
            :shard shard
            :monster-classes (mapcar (lambda (s) (plist-get s :monster-class)) walk)))))

(defun kiro-fractran-memory-walk-function (func)
  "Monster walk a function."
  (let* ((cid (kiro-fractran-memory-cid func))
         (walk (kiro-fractran-cid-hex-walk cid))
         (shard (kiro-fractran-cid-shard cid)))
    (list :type 'function
          :name func
          :cid cid
          :hex (format "0x%04X" cid)
          :shard shard
          :monster-classes (mapcar (lambda (s) (plist-get s :monster-class)) walk))))

(defun kiro-fractran-memory-walk-all-buffers ()
  "Monster walk all buffers."
  (mapcar #'kiro-fractran-memory-walk-buffer (buffer-list)))

(defun kiro-fractran-memory-walk-all-functions ()
  "Monster walk all defined functions."
  (let ((funcs '()))
    (mapatoms (lambda (sym)
                (when (fboundp sym)
                  (push (kiro-fractran-memory-walk-function sym) funcs))))
    funcs))

(defun kiro-fractran-memory-shard-distribution (walks)
  "Compute shard distribution from walks."
  (let ((shards (make-vector 71 0)))
    (dolist (walk walks)
      (let ((shard (plist-get walk :shard)))
        (aset shards shard (1+ (aref shards shard)))))
    shards))

(defun kiro-fractran-memory-monster-distribution (walks)
  "Compute Monster class distribution."
  (let ((classes (make-hash-table :test 'equal)))
    (dolist (walk walks)
      (dolist (class (plist-get walk :monster-classes))
        (puthash class (1+ (gethash class classes 0)) classes)))
    classes))

;;;###autoload
(defun kiro-fractran-memory-walk-report ()
  "Generate full memory walk report."
  (interactive)
  (let* ((buffer-walks (kiro-fractran-memory-walk-all-buffers))
         (buffer-shards (kiro-fractran-memory-shard-distribution buffer-walks))
         (buffer-monsters (kiro-fractran-memory-monster-distribution buffer-walks)))
    (with-current-buffer (get-buffer-create "*FRACTRAN-Memory-Walk*")
      (erase-buffer)
      (insert "=== FRACTRAN Memory Walk Report ===\n\n")
      (insert (format "Total Buffers: %d\n" (length buffer-walks)))
      (insert (format "Date: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      
      (insert "--- Shard Distribution (mod 71) ---\n")
      (dotimes (i 71)
        (let ((count (aref buffer-shards i)))
          (when (> count 0)
            (insert (format "Shard %2d: %3d buffers %s\n" 
                            i count (make-string count ?█))))))
      
      (insert "\n--- Monster Class Distribution ---\n")
      (maphash (lambda (class count)
                 (insert (format "%10s: %3d %s\n" 
                                 class count (make-string count ?█))))
               buffer-monsters)
      
      (insert "\n--- Sample Buffers ---\n")
      (dolist (walk (seq-take buffer-walks 20))
        (insert (format "%-30s CID=0x%04X Shard=%2d Classes=%s\n"
                        (plist-get walk :name)
                        (plist-get walk :cid)
                        (plist-get walk :shard)
                        (mapconcat #'identity (plist-get walk :monster-classes) ","))))
      
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'kiro-fractran-memory-walk)
;;; kiro-fractran-memory-walk.el ends here
