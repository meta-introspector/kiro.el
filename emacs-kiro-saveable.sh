#!/bin/bash
# emacs-kiro-saveable.sh - Check which sessions have kiro prompt (>) and can be saved

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

emacsclient --eval "
(progn
  (add-to-list (quote load-path) \"$SCRIPT_DIR\")
  (require (quote kiro-dashboard))
  (require (quote kiro-task-mode))
  (let ((output "")
        (shells (seq-filter (lambda (buf)
                              (with-current-buffer buf
                                (or (eq major-mode (quote shell-mode))
                                    (eq major-mode (quote eshell-mode))
                                    (eq major-mode (quote kiro-shell-task-mode)))))
                            (buffer-list))))
    (setq output (concat output (format "%-8s %-40s %-10s %-10s %-8s %s\n" "PID" "Buffer" "Size" "Saveable" "CID" "Prompt")))
    (setq output (concat output (make-string 110 ?-)))
    (setq output (concat output "\n"))
    (dolist (buf shells)
      (let* ((name (buffer-name buf))
             (proc (get-buffer-process buf))
             (pid (if proc (number-to-string (process-id proc)) "N/A"))
             (size (format "%dK" (/ (buffer-size buf) 1024)))
             (last-line (with-current-buffer buf
                          (save-excursion
                            (goto-char (point-max))
                            (forward-line -1)
                            (buffer-substring-no-properties (point) (point-max)))))
             (has-prompt (string-match-p "^[0-9]+% >" last-line))
             (content (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max))))
             (cid (format "%04x" (logand (sxhash content) #xFFFF)))
             (saveable (and proc (process-live-p proc) has-prompt)))
        (setq output (concat output (format "%-8s %-40s %-10s %-10s %-8s %s\n" 
                      pid name size 
                      (if saveable "YES" "NO")
                      (if saveable cid "N/A")
                      (if (> (length last-line) 40) 
                          (concat (substring last-line 0 37) "...")
                        last-line))))))
    output))
' 2>/dev/null | sed 's/^"//; s/"$//; s/\\n/\n/g'
