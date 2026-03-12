#!/bin/bash
# emacs-kiro-sessions.sh - List running kiro sessions with buffer info using existing dashboard code

emacsclient --eval '
(progn
  (add-to-list (quote load-path) "~/.emacs.d/kiro.el")
  (require (quote kiro-dashboard))
  (let ((output "")
        (shells (seq-filter (lambda (buf)
                              (with-current-buffer buf
                                (or (eq major-mode (quote shell-mode))
                                    (eq major-mode (quote eshell-mode))
                                    (eq major-mode (quote term-mode))
                                    (eq major-mode (quote vterm-mode))
                                    (eq major-mode (quote kiro-shell-task-mode)))))
                            (buffer-list))))
    (setq output (concat output (format "%-8s %-40s %-10s %-10s %s\n" "PID" "Buffer" "Size" "Mode" "Status")))
    (setq output (concat output (make-string 100 ?-)))
    (setq output (concat output "\n"))
    (dolist (buf shells)
      (let* ((name (buffer-name buf))
             (proc (get-buffer-process buf))
             (pid (if proc (number-to-string (process-id proc)) "N/A"))
             (size (format "%dK" (/ (buffer-size buf) 1024)))
             (mode (with-current-buffer buf (symbol-name major-mode)))
             (running (and proc (process-live-p proc)))
             (dir (with-current-buffer buf (abbreviate-file-name default-directory))))
        (setq output (concat output (format "%-8s %-40s %-10s %-10s %s | %s\n" 
                      pid name size mode 
                      (if running "RUN" "STOP")
                      dir)))))
    output))
' 2>/dev/null | sed 's/^"//; s/"$//; s/\\n/\n/g; s/\\t/  /g'
