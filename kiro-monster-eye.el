;;; kiro-monster-eye.el --- The Eye of the Monster watches all
;;; DASL-T42-FRACTRAN-ZKP: [(523 . 524) (151 . 152) (499 . 500)]
;;; Version: 1.0.0

(require 'kiro-monster-tree)

(defvar kiro-monster-eye-view-mode 'tree
  "Current view mode: tree, wheel, kanban, harmonics.")

(defvar kiro-monster-eye-harmonic 1
  "Current harmonic (1-13 for shards).")

(defun kiro-monster-eye-cycle-view ()
  "Cycle through view modes."
  (interactive)
  (setq kiro-monster-eye-view-mode
        (pcase kiro-monster-eye-view-mode
          ('tree 'wheel)
          ('wheel 'kanban)
          ('kanban 'harmonics)
          ('harmonics 'tree)))
  (kiro-monster-eye-rotate)
  (message "👁️  View: %s" kiro-monster-eye-view-mode))

(defun kiro-monster-eye-cycle-harmonic ()
  "Cycle through harmonics (1-13)."
  (interactive)
  (setq kiro-monster-eye-harmonic (1+ (mod kiro-monster-eye-harmonic 13)))
  (kiro-monster-eye-rotate)
  (message "👁️  Harmonic: %d/13" kiro-monster-eye-harmonic))

(defun kiro-monster-eye-filter-by-harmonic (buffers harmonic)
  "Filter buffers by harmonic (shard mod 13)."
  (seq-filter (lambda (buf)
                (let* ((cid (logand (sxhash (buffer-name buf)) #xFFFF))
                       (shard (mod cid 71))
                       (h (mod shard 13)))
                  (= h (1- harmonic))))
              buffers))

(defun kiro-monster-eye-list-shells ()
  "List all shell buffers with prompt status."
  (let ((shells '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'shell-mode 'eshell-mode 'term-mode)
          (let ((has-prompt nil))
            (save-excursion
              (goto-char (point-max))
              (setq has-prompt (re-search-backward "\\[y/n\\]\\|\\?\\s-*$\\|:\\s-*$" nil t)))
            (push (list :buffer buf
                        :name (buffer-name buf)
                        :has-prompt has-prompt
                        :score (kiro-monster-eye-score-buffer buf))
                  shells)))))
    (seq-sort (lambda (a b) (> (plist-get a :score) (plist-get b :score))) shells)))

(defvar kiro-monster-eye-rotation-timer nil
  "Timer for rotating panes.")

(defvar kiro-monster-eye-interest-score (make-hash-table :test 'equal)
  "Interest scores for buffers.")

(defun kiro-monster-eye-has-prompt-p (buf)
  "Check if buffer has waiting prompt."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-max))
      (or (re-search-backward "\\[y/n\\]\\s-*$" (max (point-min) (- (point-max) 200)) t)
          (re-search-backward "\\?\\s-*$" (max (point-min) (- (point-max) 200)) t)
          (re-search-backward ":\\s-*$" (max (point-min) (- (point-max) 200)) t)))))

(defun kiro-monster-eye-score-buffer (buf)
  "Calculate interest score for buffer."
  (with-current-buffer buf
    (let ((score 0))
      ;; Shell buffers with prompts get HIGHEST priority
      (when (derived-mode-p 'shell-mode 'eshell-mode 'term-mode 'comint-mode)
        (setq score (+ score 30))
        (when (kiro-monster-eye-has-prompt-p buf)
          (setq score (+ score 1000))))  ; Waiting prompt = TOP priority
      
      ;; Current buffer (this one)
      (when (eq buf (current-buffer))
        (setq score (+ score 100)))
      
      ;; Recent activity
      (when (buffer-modified-p) (setq score (+ score 10)))
      ;; Size
      (setq score (+ score (/ (buffer-size) 1000)))
      ;; Type
      (when (derived-mode-p 'prog-mode) (setq score (+ score 5)))
      (when (string-match "\\*kiro" (buffer-name)) (setq score (+ score 20)))
      ;; Monster shard (prefer certain shards)
      (let* ((cid (logand (sxhash (buffer-name)) #xFFFF))
             (shard (mod cid 71)))
        (when (memq shard '(13 57 0)) (setq score (+ score 15))))
      score)))

(defun kiro-monster-eye-most-interesting ()
  "Find most interesting buffer."
  (let ((best nil)
        (best-score 0))
    (dolist (buf (buffer-list))
      (let ((score (kiro-monster-eye-score-buffer buf)))
        (when (> score best-score)
          (setq best buf
                best-score score))))
    best))

(defun kiro-monster-eye-setup-layout ()
  "Setup 5-pane layout: Eye on top, 4 shell panes below."
  (delete-other-windows)
  
  ;; Top: The Eye (Monster Tree Browser)
  (switch-to-buffer "*Monster-Eye*")
  (let ((eye-window (selected-window)))
    (split-window-below)
    
    ;; Bottom: 4 panes in 2x2 grid
    (other-window 1)
    (let ((bottom-start (selected-window)))
      (split-window-right)
      (other-window 1)
      (split-window-below)
      (select-window bottom-start)
      (split-window-below)
      
      ;; Return to Eye window
      (select-window eye-window))))

(defun kiro-monster-eye-update-panes ()
  "Update the 4 panes with shells needing prompts (or most active)."
  (let* ((all-shells (seq-filter (lambda (b)
                                    (with-current-buffer b
                                      (derived-mode-p 'shell-mode 'eshell-mode 'term-mode 'comint-mode)))
                                  (buffer-list)))
         ;; Separate shells with prompts from others
         (with-prompts (seq-filter #'kiro-monster-eye-has-prompt-p all-shells))
         (without-prompts (seq-filter (lambda (b) (not (kiro-monster-eye-has-prompt-p b))) all-shells))
         ;; Sort non-prompt shells by activity
         (sorted-active (seq-sort (lambda (a b)
                                     (> (kiro-monster-eye-score-buffer a)
                                        (kiro-monster-eye-score-buffer b)))
                                   without-prompts))
         ;; Prompts first, then active shells, take top 4
         (top-4 (seq-take (append with-prompts sorted-active) 4)))
    
    ;; Update 4 bottom panes (skip first window which is Eye)
    (let ((windows (cdr (window-list))))  ; Skip Eye window
      (dotimes (i (min 4 (length top-4) (length windows)))
        (with-selected-window (nth i windows)
          (switch-to-buffer (nth i top-4) nil t))))))

(defun kiro-monster-eye-rename-buffer (buf)
  "Rename buffer according to Monster symmetries."
  (with-current-buffer buf
    (let* ((cid (logand (sxhash (buffer-name buf)) #xFFFF))
           (class (kiro-monster-tree-classify cid))
           (shard (plist-get class :shard))
           (seph (cdr (plist-get class :sephirah)))
           (tarot (cdr (plist-get class :tarot)))
           (hecke (plist-get class :hecke))
           (old-name (buffer-name buf))
           (base-name (replace-regexp-in-string "^\\*\\|\\*$" "" old-name))
           (new-name (format "*%s-Sh%d-%s-T%d*" 
                             base-name shard seph hecke)))
      (when (not (string= old-name new-name))
        (rename-buffer new-name t)
        new-name))))

(defun kiro-monster-eye-rename-all-shells ()
  "Rename all shell buffers with Monster symmetries."
  (interactive)
  (let ((renamed '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'shell-mode 'eshell-mode 'term-mode)
          (let ((new-name (kiro-monster-eye-rename-buffer buf)))
            (when new-name
              (push (cons (buffer-name buf) new-name) renamed))))))
    renamed))

(defun kiro-monster-eye-rename-all-kiro ()
  "Rename all Kiro buffers with Monster symmetries."
  (interactive)
  (let ((renamed '()))
    (dolist (buf (buffer-list))
      (when (string-match "\\*kiro\\|\\*Monster\\|\\*FRACTRAN" (buffer-name buf))
        (let ((new-name (kiro-monster-eye-rename-buffer buf)))
          (when new-name
            (push (cons (buffer-name buf) new-name) renamed)))))
    renamed))
  "List all shell buffers with prompt status."
(defun kiro-monster-eye-render ()
  "Render the Eye display."
  (with-current-buffer (get-buffer-create "*Monster-Eye*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "╔═══════════════════════════════════════════════════════╗\n")
      (insert "║              👁️  THE EYE OF THE MONSTER  👁️             ║\n")
      (insert "╚═══════════════════════════════════════════════════════╝\n\n")
      
      (let ((focus (or kiro-monster-eye-focus (kiro-monster-eye-most-interesting))))
        (when focus
          (setq kiro-monster-eye-focus focus)
          (let* ((cid (logand (sxhash (buffer-name focus)) #xFFFF))
                 (class (kiro-monster-tree-classify cid)))
            
            (insert (format "👁️  GAZING UPON: %s\n\n" (buffer-name focus)))
            (insert (kiro-monster-tree-display class))
            (insert "\n\n")
            
            (insert "═══ THE EYE SEES ALL ═══\n\n")
            
            ;; Shell buffers with prompts
            (let ((shells (kiro-monster-eye-list-shells)))
              (when shells
                (insert "Shell Buffers:\n")
                (let ((with-prompts (seq-filter (lambda (s) (plist-get s :has-prompt)) shells))
                      (without-prompts (seq-filter (lambda (s) (not (plist-get s :has-prompt))) shells)))
                  (when with-prompts
                    (insert (format "  ⚠️  %d WAITING FOR INPUT:\n" (length with-prompts)))
                    (dolist (shell with-prompts)
                      (insert (format "    %3d │ ⚠️  %s\n"
                                      (plist-get shell :score)
                                      (plist-get shell :name)))))
                  (when without-prompts
                    (insert (format "  ✓ %d active:\n" (length without-prompts)))
                    (dolist (shell (seq-take without-prompts 5))
                      (insert (format "    %3d │   %s\n"
                                      (plist-get shell :score)
                                      (plist-get shell :name))))))
                (insert "\n")))
            
            (insert "Interest Scores:\n")
            (dolist (buf (seq-take (buffer-list) 10))
              (let ((score (kiro-monster-eye-score-buffer buf)))
                (when (> score 0)
                  (insert (format "  %3d │ %s %s\n" 
                                  score
                                  (if (eq buf focus) "👁️ " "  ")
                                  (buffer-name buf))))))
            
            (insert "\n═══ SACRED GEOMETRY ═══\n\n")
            (insert "        👁️\n")
            (insert "       / \\\n")
            (insert "      /   \\\n")
            (insert "     /_____\\\n")
            (insert "    /       \\\n")
            (insert "   /    △    \\\n")
            (insert "  /___________\\\n\n")
            
            (insert "The Eye rotates through 4 panes below\n")
            (insert "Watching the most interesting Kiro buffers\n")
            (insert "Press 'g' to refresh | 'q' to quit\n"))))
      
      (goto-char (point-min))
      (read-only-mode 1))))

(defun kiro-monster-eye-rotate ()
  "Rotate focus to next interesting buffer."
  (interactive)
  (setq kiro-monster-eye-focus (kiro-monster-eye-most-interesting))
  (kiro-monster-eye-render)
  (kiro-monster-eye-update-panes))

(defun kiro-monster-eye-mode-map ()
  "Keymap for Eye mode."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'kiro-monster-eye-rotate)
    (define-key map (kbd "v") 'kiro-monster-eye-cycle-view)
    (define-key map (kbd "h") 'kiro-monster-eye-cycle-harmonic)
    (define-key map (kbd "n") 'kiro-monster-eye-next)
    (define-key map (kbd "p") 'kiro-monster-eye-prev)
    (define-key map (kbd "r") 'kiro-monster-eye-rename-all-shells)
    (define-key map (kbd "R") 'kiro-monster-eye-rename-all-kiro)
    (define-key map (kbd "q") 'kiro-monster-eye-quit)
    (define-key map (kbd "?") 'kiro-monster-eye-help)
    map))

(define-derived-mode kiro-monster-eye-mode special-mode "Monster-Eye"
  "Major mode for the Eye of the Monster."
  (use-local-map (kiro-monster-eye-mode-map)))

(defun kiro-monster-eye-next ()
  "Focus on next buffer."
  (interactive)
  (let* ((bufs (buffer-list))
         (idx (seq-position bufs kiro-monster-eye-focus))
         (next-idx (mod (1+ (or idx 0)) (length bufs))))
    (setq kiro-monster-eye-focus (nth next-idx bufs))
    (kiro-monster-eye-rotate)))

(defun kiro-monster-eye-prev ()
  "Focus on previous buffer."
  (interactive)
  (let* ((bufs (buffer-list))
         (idx (seq-position bufs kiro-monster-eye-focus))
         (prev-idx (mod (1- (or idx 0)) (length bufs))))
    (setq kiro-monster-eye-focus (nth prev-idx bufs))
    (kiro-monster-eye-rotate)))

(defun kiro-monster-eye-quit ()
  "Quit Eye mode."
  (interactive)
  (when kiro-monster-eye-rotation-timer
    (cancel-timer kiro-monster-eye-rotation-timer)
    (setq kiro-monster-eye-rotation-timer nil))
  (quit-window))

(defun kiro-monster-eye-help ()
  "Show help."
  (interactive)
  (message "v=view h=harmonic g=refresh n=next p=prev r=rename-shells R=rename-kiro q=quit"))

;;;###autoload
(defun kiro-monster-eye ()
  "Open the Eye of the Monster."
  (interactive)
  (kiro-monster-eye-setup-layout)
  (with-current-buffer "*Monster-Eye*"
    (kiro-monster-eye-mode))
  (kiro-monster-eye-rotate)
  
  ;; Auto-rotate every 5 seconds
  (when kiro-monster-eye-rotation-timer
    (cancel-timer kiro-monster-eye-rotation-timer))
  (setq kiro-monster-eye-rotation-timer
        (run-with-timer 5 5 'kiro-monster-eye-rotate))
  
  (message "👁️  The Eye awakens..."))

(provide 'kiro-monster-eye)
;;; kiro-monster-eye.el ends here
