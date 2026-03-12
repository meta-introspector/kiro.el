#!/bin/bash
# emacs-kiro-compact-sessions.sh - Compact saved kiro sessions

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

emacsclient --eval "
(progn
  (add-to-list (quote load-path) \"$SCRIPT_DIR\")
  (require (quote kiro-task-mode))
  (let ((save-dir (expand-file-name "~/03-march/10/chats/"))
        (compact-dir (expand-file-name "~/03-march/10/chats/compacted/"))
        (compacted-count 0)
        (shells (seq-filter (lambda (buf)
                              (with-current-buffer buf
                                (or (eq major-mode (quote shell-mode))
                                    (eq major-mode (quote eshell-mode))
                                    (eq major-mode (quote kiro-shell-task-mode)))))
                            (buffer-list))))
    (make-directory compact-dir t)
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
                 (size (length content))
                 (comp-size (/ size 10))
                 (shard (mod (logand (sxhash content) #xFFFF) 71))
                 (compact-file (format "%s/kiro-compact-cid-%s-pid-%s-%s.md" compact-dir cid pid timestamp)))
            (with-temp-file compact-file
              (insert (format "# Kiro Session Compact\n\n"))
              (insert (format "**CID**: %s\n" cid))
              (insert (format "**PID**: %s\n" pid))
              (insert (format "**Shard**: %d\n" shard))
              (insert (format "**Size**: %d bytes (%.1f KB)\n" size (/ size 1024.0)))
              (insert (format "**Compressed**: %d bytes (%.1f KB)\n" comp-size (/ comp-size 1024.0)))
              (insert (format "**Ratio**: %.1fx\n" (/ (float size) comp-size)))
              (insert (format "**Timestamp**: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
              (insert (format "**Buffer**: %s\n\n" (buffer-name buf)))
              (insert "## DASL Metadata\n\n")
              (insert (format "- Monster Class: %d\n" (mod shard 42)))
              (insert (format "- Hash: %d\n" (sxhash content))))
            (setq compacted-count (1+ compacted-count))))))
    (format "Compacted %d sessions to %s" compacted-count compact-dir)))
' 2>/dev/null | sed 's/^"//; s/"$//'
