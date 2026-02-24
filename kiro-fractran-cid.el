;;; kiro-fractran-cid.el --- FRACTRAN with CID hex-walk integration
;;; DASL-T42-FRACTRAN-ZKP: [(523 . 524) (151 . 152) (499 . 500)]
;;; Version: 2.0.0
;;; Monster-Class: 13
;;; Shard-ID: 0

(require 'kiro-fractran)

;;; Hex Walk CID System

(defun kiro-fractran-cid-extract-nibbles (value)
  "Extract 4 nibbles from VALUE (u16)."
  (list (logand (ash value -12) #xF)
        (logand (ash value -8) #xF)
        (logand (ash value -4) #xF)
        (logand value #xF)))

(defun kiro-fractran-cid-classify-nibble (nibble bit-count)
  "Classify NIBBLE to Monster conjugacy class."
  (pcase (cons nibble bit-count)
    ('(0 . 0) "Trivial")
    ('(1 . 1) "1A")
    ('(15 . 4) "2A-Max")
    (`(,_ . 2) "2A")
    ('(7 . 3) "3A")
    ('(9 . 2) "3A")
    ('(13 . 3) "5A-13")  ; 0xD special
    (`(,_ . 3) "5A")
    (_ (format "%dA" bit-count))))

(defun kiro-fractran-cid-hex-walk (value)
  "Perform hex walk on VALUE, return steps."
  (let* ((nibbles (kiro-fractran-cid-extract-nibbles value))
         (steps '())
         (accumulated 0))
    (dotimes (i 4)
      (let* ((nibble (nth i nibbles))
             (weight (expt 16 (- 3 i)))
             (contribution (* nibble weight))
             (binary (let ((s ""))
                       (dotimes (b 4)
                         (setq s (concat s (if (zerop (logand nibble (ash 1 (- 3 b)))) "0" "1"))))
                       s))
             (bit-count (logcount nibble)))
        (setq accumulated (+ accumulated contribution))
        (push (list :position i
                    :nibble nibble
                    :weight weight
                    :contribution contribution
                    :accumulated accumulated
                    :binary binary
                    :bit-count bit-count
                    :monster-class (kiro-fractran-cid-classify-nibble nibble bit-count))
              steps)))
    (nreverse steps)))

(defun kiro-fractran-cid-shard (value)
  "Compute shard assignment (mod 71)."
  (mod value 71))

(defun kiro-fractran-cid-from-program (program)
  "Generate CID from FRACTRAN program."
  (let ((hash (sxhash (format "%S" program))))
    (logand hash #xFFFF)))  ; u16

(defun kiro-fractran-cid-encode (program state)
  "Encode FRACTRAN PROGRAM and STATE as CID with hex walk."
  (let* ((cid (kiro-fractran-cid-from-program program))
         (walk (kiro-fractran-cid-hex-walk cid))
         (shard (kiro-fractran-cid-shard cid)))
    (list :cid cid
          :hex (format "0x%04X" cid)
          :walk walk
          :shard shard
          :program program
          :state state)))

(defun kiro-fractran-cid-step (encoded-state)
  "Execute one FRACTRAN step with CID tracking."
  (let* ((program (plist-get encoded-state :program))
         (state (plist-get encoded-state :state))
         (new-state (catch 'found
                      (dolist (frac program)
                        (let* ((num (car frac))
                               (den (cdr frac))
                               (result (* state num)))
                          (when (zerop (mod result den))
                            (throw 'found (/ result den)))))
                      nil)))
    (if new-state
        (kiro-fractran-cid-encode program new-state)
      nil)))

(defun kiro-fractran-cid-run (program initial-state max-steps)
  "Run FRACTRAN with CID tracking."
  (let ((encoded (kiro-fractran-cid-encode program initial-state))
        (trace '())
        (step 0))
    (while (and encoded (< step max-steps))
      (push (list :step step
                  :cid (plist-get encoded :cid)
                  :shard (plist-get encoded :shard)
                  :state (plist-get encoded :state))
            trace)
      (setq encoded (kiro-fractran-cid-step encoded)
            step (1+ step)))
    (nreverse trace)))

(defun kiro-fractran-cid-display-walk (cid)
  "Display hex walk for CID."
  (let ((walk (kiro-fractran-cid-hex-walk cid)))
    (insert (format "=== Hex Walk: 0x%04X = %d ===\n\n" cid cid))
    (dolist (step walk)
      (insert (format "Step %d: 0x%X\n" 
                      (1+ (plist-get step :position))
                      (plist-get step :nibble)))
      (insert (format "  Binary:       %s\n" (plist-get step :binary)))
      (insert (format "  Bit count:    %d\n" (plist-get step :bit-count)))
      (insert (format "  Weight:       %d (16^%d)\n" 
                      (plist-get step :weight)
                      (- 3 (plist-get step :position))))
      (insert (format "  Contribution: %d\n" (plist-get step :contribution)))
      (insert (format "  Accumulated:  %d\n" (plist-get step :accumulated)))
      (insert (format "  Monster:      %s\n\n" (plist-get step :monster-class))))
    (insert (format "Shard: %d (mod 71)\n" (kiro-fractran-cid-shard cid)))))

;;;###autoload
(defun kiro-fractran-cid-demo ()
  "Demo CID hex walk with 0x1F90."
  (interactive)
  (with-current-buffer (get-buffer-create "*FRACTRAN-CID*")
    (erase-buffer)
    (kiro-fractran-cid-display-walk #x1F90)
    (insert "\n--- 0xD Special ---\n\n")
    (kiro-fractran-cid-display-walk #x000D)
    (insert "\n--- FRACTRAN Execution ---\n\n")
    (let* ((program '((17 . 91) (78 . 85) (19 . 51)))
           (trace (kiro-fractran-cid-run program 2 10)))
      (dolist (entry trace)
        (insert (format "Step %d: CID=0x%04X Shard=%d State=%d\n"
                        (plist-get entry :step)
                        (plist-get entry :cid)
                        (plist-get entry :shard)
                        (plist-get entry :state)))))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(provide 'kiro-fractran-cid)
;;; kiro-fractran-cid.el ends here
