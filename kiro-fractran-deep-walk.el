;;; kiro-fractran-deep-walk.el --- Monster walk every Emacs object
;;; DASL-T42-FRACTRAN-ZKP: [(523 . 524) (151 . 152) (499 . 500)]
;;; Version: 1.0.0

(require 'kiro-fractran-cid)

(defun kiro-fractran-walk-line (line)
  "Walk a line of text."
  (let* ((cid (logand (sxhash line) #xFFFF))
         (shard (mod cid 71)))
    (list :type 'line :content line :cid cid :shard shard)))

(defun kiro-fractran-walk-paragraph ()
  "Walk current paragraph."
  (let* ((bounds (bounds-of-thing-at-point 'paragraph))
         (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (cid (logand (sxhash text) #xFFFF)))
    (list :type 'paragraph :cid cid :shard (mod cid 71))))

(defun kiro-fractran-walk-token (token)
  "Walk a token (word/symbol)."
  (let ((cid (logand (sxhash token) #xFFFF)))
    (list :type 'token :value token :cid cid :shard (mod cid 71))))

(defun kiro-fractran-walk-variable (var)
  "Walk a variable."
  (let ((cid (logand (sxhash (symbol-name var)) #xFFFF)))
    (list :type 'variable :name var :cid cid :shard (mod cid 71)
          :value (when (boundp var) (symbol-value var)))))

(defun kiro-fractran-walk-function (func)
  "Walk a function."
  (let ((cid (logand (sxhash (symbol-name func)) #xFFFF)))
    (list :type 'function :name func :cid cid :shard (mod cid 71))))

(defun kiro-fractran-walk-sexpr (sexpr)
  "Walk an s-expression."
  (let ((cid (logand (sxhash (format "%S" sexpr)) #xFFFF)))
    (list :type 'sexpr :expr sexpr :cid cid :shard (mod cid 71))))

(defun kiro-fractran-walk-buffer-deep ()
  "Deep walk current buffer: lines, tokens, sexprs."
  (let ((lines '())
        (tokens '())
        (sexprs '()))
    ;; Walk lines
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties 
                     (line-beginning-position) (line-end-position))))
          (push (kiro-fractran-walk-line line) lines))
        (forward-line 1)))
    ;; Walk tokens
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\w+" nil t)
        (push (kiro-fractran-walk-token (match-string 0)) tokens)))
    ;; Walk sexprs (if Lisp mode)
    (when (derived-mode-p 'emacs-lisp-mode)
      (save-excursion
        (goto-char (point-min))
        (condition-case nil
            (while t
              (let ((sexpr (read (current-buffer))))
                (push (kiro-fractran-walk-sexpr sexpr) sexprs)))
          (error nil))))
    (list :lines (nreverse lines)
          :tokens (nreverse tokens)
          :sexprs (nreverse sexprs))))

(defun kiro-fractran-walk-all-variables ()
  "Walk all bound variables."
  (let ((vars '()))
    (mapatoms (lambda (sym)
                (when (boundp sym)
                  (push (kiro-fractran-walk-variable sym) vars))))
    vars))

(defun kiro-fractran-walk-all-functions ()
  "Walk all functions."
  (let ((funcs '()))
    (mapatoms (lambda (sym)
                (when (fboundp sym)
                  (push (kiro-fractran-walk-function sym) funcs))))
    funcs))

;;;###autoload
(defun kiro-fractran-deep-walk-report ()
  "Generate deep walk report for current buffer."
  (interactive)
  (let* ((deep (kiro-fractran-walk-buffer-deep))
         (lines (plist-get deep :lines))
         (tokens (plist-get deep :tokens))
         (sexprs (plist-get deep :sexprs)))
    (with-current-buffer (get-buffer-create "*FRACTRAN-Deep-Walk*")
      (erase-buffer)
      (insert (format "=== Deep Walk: %s ===\n\n" (buffer-name)))
      (insert (format "Lines:   %d\n" (length lines)))
      (insert (format "Tokens:  %d\n" (length tokens)))
      (insert (format "Sexprs:  %d\n\n" (length sexprs)))
      
      (insert "--- Sample Lines ---\n")
      (dolist (line (seq-take lines 5))
        (insert (format "CID=0x%04X Shard=%2d: %s\n"
                        (plist-get line :cid)
                        (plist-get line :shard)
                        (truncate-string-to-width (plist-get line :content) 50))))
      
      (insert "\n--- Sample Tokens ---\n")
      (dolist (tok (seq-take tokens 10))
        (insert (format "CID=0x%04X Shard=%2d: %s\n"
                        (plist-get tok :cid)
                        (plist-get tok :shard)
                        (plist-get tok :value))))
      
      (when sexprs
        (insert "\n--- Sample Sexprs ---\n")
        (dolist (sx (seq-take sexprs 5))
          (insert (format "CID=0x%04X Shard=%2d: %S\n"
                          (plist-get sx :cid)
                          (plist-get sx :shard)
                          (plist-get sx :expr)))))
      
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'kiro-fractran-deep-walk)
;;; kiro-fractran-deep-walk.el ends here
