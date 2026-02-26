;;; kiro-prompt-queue.el --- Central prompt queue for all shells

(defvar kiro-prompt-queue '()
  "Queue of pending prompts from shells")

(defvar kiro-prompt-queue-buffer "*Kiro Prompt Queue*")

(defun kiro-prompt-scan-shells ()
  "Scan all shells for pending prompts"
  (interactive)
  (setq kiro-prompt-queue '())
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (or (eq major-mode 'shell-mode)
                (eq major-mode 'kiro-shell-task-mode)
                (eq major-mode 'eshell-mode))
        (let* ((proc (get-buffer-process buf))
               (running (and proc (process-live-p proc)))
               (last-output (when running
                             (save-excursion
                               (goto-char (point-max))
                               (forward-line -20)
                               (buffer-substring-no-properties (point) (point-max)))))
               (needs-input (and last-output
                                (or (string-match-p "\\[y/n\\]\\|\\[Y/n\\]\\|(y or n)" last-output)
                                    (string-match-p "\\?" last-output)))))
          (when needs-input
            (push (list :buffer buf
                       :name (buffer-name buf)
                       :prompt (string-trim last-output)
                       :dir default-directory
                       :local t)
                  kiro-prompt-queue)))))))

(defun kiro-prompt-scan-remote-shells ()
  "Scan remote Emacs instances for prompts via tunnel"
  (let ((tunnel-dir "~/kiro-spool/tunnel/"))
    (when (file-directory-p tunnel-dir)
      (dolist (file (directory-files tunnel-dir t "^peer-.*\\.json$"))
        (let* ((peer (ignore-errors
                      (with-temp-buffer
                        (insert-file-contents file)
                        (json-parse-buffer :object-type 'alist))))
               (peer-id (when peer (alist-get 'id peer))))
          (when (and peer-id (boundp 'kiro-tunnel-id) 
                    (not (string= peer-id kiro-tunnel-id)))
            ;; Request shell status from peer
            (let ((req-file (expand-file-name 
                            (format "msg-%s-to-%s-shells.json" 
                                   (or kiro-tunnel-id "local") peer-id)
                            tunnel-dir)))
              (with-temp-buffer
                (insert (json-encode `((from . ,(or kiro-tunnel-id "local"))
                                      (action . "get-shells")
                                      (data . ""))))
                (write-region (point-min) (point-max) req-file)))
            ;; Check for response
            (let ((resp-file (expand-file-name 
                             (format "msg-%s-to-%s-shells-resp.json" 
                                    peer-id (or kiro-tunnel-id "local"))
                             tunnel-dir)))
              (when (file-exists-p resp-file)
                (let ((shells (ignore-errors
                               (with-temp-buffer
                                 (insert-file-contents resp-file)
                                 (json-parse-buffer :object-type 'alist)))))
                  (when shells
                    (dolist (shell (alist-get 'shells shells))
                      (push (list :name (alist-get 'name shell)
                                 :prompt (alist-get 'prompt shell)
                                 :dir (alist-get 'dir shell)
                                 :peer peer-id
                                 :local nil)
                            kiro-prompt-queue)))
                  (delete-file resp-file))))))))))

(defun kiro-prompt-request-from-peer (peer-id)
  "Simplified peer request - just mark as remote"
  (push (list :name (format "Remote shells @%s" peer-id)
             :prompt "Checking remote Emacs..."
             :dir "~/"
             :peer peer-id
             :local nil)
        kiro-prompt-queue))

(defun kiro-prompt-queue-refresh ()
  "Refresh prompt queue display"
  (interactive)
  (unless (eq this-command 'kiro-prompt-queue-refresh)
    (kiro-prompt-scan-shells)
    (kiro-prompt-scan-remote-shells))
  (with-current-buffer (get-buffer-create kiro-prompt-queue-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "╔═══════════════════════════════════════════════════════════╗\n" 'face 'bold))
      (insert (propertize "║              ⚠️  KIRO PROMPT QUEUE ⚠️                    ║\n" 'face '(:foreground "yellow" :weight bold)))
      (insert (propertize "╚═══════════════════════════════════════════════════════════╝\n\n" 'face 'bold))
      
      (if (null kiro-prompt-queue)
          (insert (propertize "No pending prompts\n\n" 'face 'shadow))
        (insert (propertize (format "%d prompts waiting\n\n" (length kiro-prompt-queue)) 'face 'bold))
        (let ((idx 1))
          (dolist (prompt kiro-prompt-queue)
            (let ((buf-name (plist-get prompt :name))
                  (text (plist-get prompt :prompt))
                  (dir (plist-get prompt :dir))
                  (local (plist-get prompt :local))
                  (peer (plist-get prompt :peer)))
              (insert (propertize (format "[%d] " idx) 'face '(:foreground "cyan")))
              (insert (propertize buf-name 'face '(:foreground "yellow")))
              (unless local
                (insert (propertize (format " @%s" peer) 'face '(:foreground "magenta"))))
              (insert "\n")
              (insert (propertize (format "    %s\n" (abbreviate-file-name dir)) 'face 'shadow))
              (insert (format "    %s\n\n" 
                            (if (> (length text) 200)
                                (concat (substring text 0 197) "...")
                              text)))
              (setq idx (1+ idx))))))
      
      (insert (propertize "\nCommands: " 'face 'bold))
      (insert "RET:answer  j:jump  y:yes-all  n:no-all  g:refresh  q:quit\n"))
    (kiro-prompt-queue-mode)
    (goto-char (point-min))))

(defun kiro-prompt-queue-answer ()
  "Answer prompt at point"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\[\\([0-9]+\\)\\]")
      (let* ((idx (string-to-number (match-string 1)))
             (prompt (nth (1- idx) kiro-prompt-queue))
             (buf (plist-get prompt :buffer))
             (answer (read-string "Answer: ")))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (goto-char (point-max))
            (insert answer)
            (comint-send-input))
          (message "Sent '%s' to %s" answer (plist-get prompt :name))
          (sit-for 0.5)
          (kiro-prompt-queue-refresh))))))

(defun kiro-prompt-queue-jump ()
  "Jump to shell buffer at point"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\[\\([0-9]+\\)\\]")
      (let* ((idx (string-to-number (match-string 1)))
             (prompt (nth (1- idx) kiro-prompt-queue))
             (buf (plist-get prompt :buffer)))
        (when (buffer-live-p buf)
          (switch-to-buffer buf)
          (goto-char (point-max)))))))

(defun kiro-prompt-queue-yes-all ()
  "Answer 'y' to all prompts"
  (interactive)
  (when (yes-or-no-p (format "Answer 'y' to all %d prompts? " (length kiro-prompt-queue)))
    (dolist (prompt kiro-prompt-queue)
      (let ((buf (plist-get prompt :buffer)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "y")
            (comint-send-input))
          (sit-for 0.2))))
    (message "Answered 'y' to all prompts")
    (sit-for 1)
    (kiro-prompt-queue-refresh)))

(defun kiro-prompt-queue-no-all ()
  "Answer 'n' to all prompts"
  (interactive)
  (when (yes-or-no-p (format "Answer 'n' to all %d prompts? " (length kiro-prompt-queue)))
    (dolist (prompt kiro-prompt-queue)
      (let ((buf (plist-get prompt :buffer)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "n")
            (comint-send-input))
          (sit-for 0.2))))
    (message "Answered 'n' to all prompts")
    (sit-for 1)
    (kiro-prompt-queue-refresh)))

(defvar kiro-prompt-queue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'kiro-prompt-queue-answer)
    (define-key map (kbd "j") 'kiro-prompt-queue-jump)
    (define-key map (kbd "y") 'kiro-prompt-queue-yes-all)
    (define-key map (kbd "n") 'kiro-prompt-queue-no-all)
    (define-key map (kbd "g") 'kiro-prompt-queue-refresh)
    (define-key map (kbd "q") 'quit-window)
    map))

(define-derived-mode kiro-prompt-queue-mode special-mode "Kiro-Prompts"
  "Major mode for Kiro prompt queue.

\\{kiro-prompt-queue-mode-map}"
  (setq buffer-read-only t)
  (hl-line-mode 1))

(defun kiro-prompt-queue ()
  "Open Kiro prompt queue"
  (interactive)
  (switch-to-buffer kiro-prompt-queue-buffer)
  (kiro-prompt-queue-refresh))

(provide 'kiro-prompt-queue)
;;; kiro-prompt-queue.el ends here
