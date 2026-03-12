#!/bin/bash
# emacs-kiro-save-sessions.sh - Save all kiro sessions with > prompt to canonical log files

emacsclient --eval '
(progn
  (add-to-list (quote load-path) "~/.emacs.d/kiro.el")
  (require (quote kiro-task-mode))
  (let ((save-dir (expand-file-name "~/03-march/10/chats/"))
        (saved-count 0)
        (shells (seq-filter (lambda (buf)
                              (with-current-buffer buf
                                (or (eq major-mode (quote shell-mode))
                                    (eq major-mode (quote eshell-mode))
                                    (eq major-mode (quote kiro-shell-task-mode)))))
                            (buffer-list))))
    (make-directory save-dir t)
    (dolist (buf shells)
      (let* ((proc (get-buffer-process buf))
             (last-line (with-current-buffer buf
                          (save-excursion
                            (goto-char (point-max))
                            (forward-line -1)
                            (buffer-substring-no-properties (point) (point-max)))))
             (has-prompt (string-match-p "^[0-9]+% >" last-line)))
        (when (and proc (process-live-p proc) has-prompt)
          (let* ((content (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max))))
                 (cid (format "%04x" (logand (sxhash content) #xFFFF)))
                 (pid (number-to-string (process-id proc)))
                 (timestamp (format-time-string "%Y%m%d-%H%M%S"))
                 (log-file (format "%s/kiro-session-cid-%s-pid-%s-%s.log" save-dir cid pid timestamp))
                 (json-file (format "%s/kiro-session-cid-%s-pid-%s-%s.json" save-dir cid pid timestamp)))
            ;; Save log
            (with-temp-file log-file
              (insert content))
            ;; Try to save JSON via kiro-cli
            (with-current-buffer buf
              (condition-case err
                  (let ((default-directory (file-name-directory json-file)))
                    (kiro-task-save-chat))
                (error nil)))
            (setq saved-count (1+ saved-count))))))
    (format "Saved %d sessions to %s" saved-count save-dir)))
' 2>/dev/null | sed 's/^"//; s/"$//'

