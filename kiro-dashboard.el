;;; kiro-dashboard.el --- Kiro system dashboard

;; Author: Kiro Team
;; Version: 1.0.0

;;; Code:

(defgroup kiro-dashboard nil
  "Kiro dashboard settings."
  :group 'kiro
  :prefix "kiro-dashboard-")

(defcustom kiro-spool-directory "~/kiro-spool/"
  "Directory for Kiro spool files."
  :type 'directory
  :group 'kiro-dashboard)

(defcustom kiro-elisp-directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing Kiro elisp files."
  :type 'directory
  :group 'kiro-dashboard)

(defvar kiro-query-jobs nil
  "Active query jobs")

(defvar kiro-query-cache nil
  "Query result cache")

(unless kiro-query-jobs
  (setq kiro-query-jobs (make-hash-table :test 'equal)))

(unless kiro-query-cache
  (setq kiro-query-cache (make-hash-table :test 'equal)))

(defun kiro-query-buffer-name (job-id)
  "Get buffer name for JOB-ID"
  (format "*kiro-query-%s*" job-id))

(defvar kiro-dashboard-buffer "*Kiro Dashboard*")

(defun kiro-dashboard-refresh ()
  "Refresh dashboard"
  (interactive)
  (with-current-buffer (get-buffer-create kiro-dashboard-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (kiro-dashboard-insert-header)
      (kiro-dashboard-insert-shells)
      (kiro-dashboard-insert-query-jobs)
      (goto-char (point-min)))
    (kiro-dashboard-mode)))

(defun kiro-dashboard-insert-header ()
  "Insert dashboard header"
  (insert (propertize "KIRO DASHBOARD" 'face '(:foreground "cyan" :weight bold)))
  (insert (propertize " | Press ? for help\n\n" 'face 'shadow)))

(defun kiro-dashboard-insert-service-status ()
  "Insert service status"
  (insert (propertize "## Service Status\n\n" 'face 'bold))
  (let ((spool-active (file-directory-p kiro-spool-directory))
        (jobs-count (hash-table-count kiro-query-jobs)))
    (insert (format "Spool:  %s\n" (if spool-active 
                                       (propertize "🟢 ACTIVE" 'face '(:foreground "green"))
                                     (propertize "🔴 INACTIVE" 'face '(:foreground "red")))))
    (insert (format "Jobs:   %s\n" (propertize (format "%d active" jobs-count) 'face 'bold)))
    (insert (format "Time:   %s\n\n" (current-time-string)))))

(defun kiro-dashboard-insert-tunnel-status ()
  "Insert tunnel peer status"
  (insert (propertize "## Tunnel Peers\n\n" 'face 'bold))
  (let ((tunnel-dir (expand-file-name "tunnel/" kiro-spool-directory))
        (peers '()))
    (when (file-directory-p tunnel-dir)
      (dolist (file (directory-files tunnel-dir t "^peer-.*\\.json$"))
        (let* ((peer (with-temp-buffer
                       (insert-file-contents file)
                       (ignore-errors (json-parse-buffer :object-type 'alist))))
               (id (when peer (alist-get 'id peer)))
               (pid (when peer (alist-get 'pid peer))))
          (when id
            (insert (propertize "• " 'face 'success))
            (insert (propertize id 'face '(:foreground "cyan")))
            (insert (format " (PID: %s)\n" pid))
            (push id peers)))))
    (unless peers
      (insert (propertize "No peers connected\n" 'face 'shadow)))
    (insert "\n")))

(defun kiro-dashboard--kiro-shell-p (buf)
  "Return non-nil if BUF is a kiro shell."
  (with-current-buffer buf
    (and (or (eq major-mode 'shell-mode)
             (eq major-mode 'eshell-mode)
             (eq major-mode 'comint-mode)
             (derived-mode-p 'comint-mode))
         (or ;; buffer name contains kiro
             (string-match-p "\\b[Kk]iro\\b" (buffer-name))
             ;; kiro-task-mode active
             (and (boundp 'kiro-task-mode) kiro-task-mode)
             ;; process cmdline contains kiro
             (let ((proc (get-buffer-process buf)))
               (and proc (process-live-p proc)
                    (string-match-p "kiro" (or (process-name proc) ""))))
             ;; last 20 lines contain kiro prompt or kiro-cli text
             (save-excursion
               (goto-char (point-max))
               (forward-line -20)
               (or (re-search-forward "^[0-9]+% >" (point-max) t)
                   (re-search-forward "kiro-cli\\|kiro>" (point-max) t)))))))

(defun kiro-dashboard-insert-shells ()
  "Insert running kiro shell buffers"
  (insert (propertize "Kiro Shells\n" 'face 'bold))
  (let* ((shells (seq-filter #'kiro-dashboard--kiro-shell-p (buffer-list)))
         (shells-with-status (mapcar (lambda (buf)
                                       (cons buf (kiro-dashboard-shell-waiting-p buf)))
                                     shells))
         (sorted-shells (sort shells-with-status (lambda (a b) (and (cdr a) (not (cdr b)))))))
    (if (null sorted-shells)
        (insert (propertize "  none\n\n" 'face 'shadow))
      (dolist (item sorted-shells)
        (let* ((buf (car item))
               (waiting (cdr item))
               (name (buffer-name buf))
               (proc (get-buffer-process buf))
               (running (and proc (process-live-p proc)))
               (dir (with-current-buffer buf 
                      (abbreviate-file-name default-directory))))
          (insert "  ")
          (insert (if running 
                      (propertize "●" 'face '(:foreground "green"))
                    (propertize "○" 'face '(:foreground "red"))))
          (insert " ")
          (when waiting
            (insert (propertize "⚠ " 'face '(:foreground "yellow"))))
          (insert (propertize name 'face '(:foreground "yellow")))
          (insert (format " %s\n" (propertize dir 'face 'shadow)))))
      (insert "\n"))))

(defun kiro-dashboard-shell-waiting-p (buf)
  "Check if shell BUF is waiting for input (has prompt)"
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-max))
      (forward-line -1)
      (re-search-forward "^[0-9]+% >" (line-end-position) t))))

(defun kiro-dashboard-get-shell-status (buf)
  "Get status of shell buffer BUF"
  (with-current-buffer buf
    (let* ((proc (get-buffer-process buf))
           (running (and proc (process-live-p proc)))
           (dir (or default-directory "unknown"))
           (last-output (save-excursion
                          (goto-char (point-max))
                          (forward-line -10)
                          (let ((text (buffer-substring-no-properties 
                                      (point) (point-max))))
                            (string-trim text))))
           (needs-input (or (string-match-p "\\[y/n\\]\\|\\[Y/n\\]\\|(y or n)" last-output)
                           (string-match-p "\\?" last-output)
                           (and (not (string-match-p "\\$\\|#\\|>" last-output))
                                (> (length last-output) 0)))))
      (format "%s | %s%s | %s" 
              (if running 
                  (propertize "RUN" 'face '(:foreground "green"))
                (propertize "STOP" 'face '(:foreground "red")))
              (abbreviate-file-name dir)
              (if needs-input 
                  (propertize " ⚠WAIT" 'face '(:foreground "yellow"))
                "")
              (if (> (length last-output) 60)
                  (concat (substring last-output 0 57) "...")
                last-output)))))

(defun kiro-dashboard-convert-shells-to-kiro ()
  "Convert all shell buffers to Kiro task mode"
  (interactive)
  (let ((count 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (or (eq major-mode 'shell-mode)
                       (eq major-mode 'eshell-mode))
                   (not (string-match-p "\\.kiro\\.org$" (buffer-name))))
          (let ((new-name (format "%s.kiro.org" (buffer-name))))
            (rename-buffer new-name t)
            (setq count (1+ count))))))
    (message "Converted %d shells to Kiro task mode" count)
    (kiro-dashboard-refresh)))

(defun kiro-dashboard-insert-query-jobs ()
  "Insert active query jobs"
  (let ((count (hash-table-count kiro-query-jobs)))
    (when (> count 0)
      (insert (propertize "Query Jobs\n" 'face 'bold))
      (maphash (lambda (id job)
                 (let ((pattern (plist-get job :pattern)))
                   (insert (format "  [%s] %s\n" id pattern))))
               kiro-query-jobs)
      (insert "\n"))))

(defun kiro-dashboard-insert-cache-stats ()
  "Insert cache statistics"
  (insert (propertize "## Cache Stats\n\n" 'face 'bold))
  (insert (format "Entries: %s\n" 
                  (propertize (format "%d" (hash-table-count kiro-query-cache)) 'face 'bold)))
  (insert (format "Memory:  ~%d KB\n\n" 
                  (/ (length (format "%S" kiro-query-cache)) 1024))))

(defun kiro-dashboard-insert-buffers ()
  "Insert Kiro buffers"
  (insert (propertize "## Kiro Buffers\n\n" 'face 'bold))
  (let ((kiro-bufs (seq-filter (lambda (buf) 
                                  (string-match-p "^\\*kiro-" (buffer-name buf)))
                                (buffer-list))))
    (if (null kiro-bufs)
        (insert (propertize "No Kiro buffers\n\n" 'face 'shadow))
      (dolist (buf kiro-bufs)
        (insert (propertize "• " 'face 'success))
        (insert (propertize (buffer-name buf) 'face '(:foreground "cyan")))
        (insert (format " (%d bytes)\n" (buffer-size buf))))
      (insert "\n"))))

(defun kiro-dashboard-new-query ()
  "Create new query from dashboard"
  (interactive)
  (let ((pattern (read-string "Query pattern: ")))
    (load-file (expand-file-name "kiro-query-planner.el" kiro-elisp-directory))
    (kiro-query-plan pattern)
    (kiro-dashboard-refresh)
    (message "Query planned: %s" pattern)))

(defun kiro-dashboard-execute-job ()
  "Execute job at point"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "  \\[\\([0-9]+\\)\\]")
      (let ((job-id (match-string 1)))
        (load-file (expand-file-name "kiro-query-planner.el" kiro-elisp-directory))
        (kiro-query-execute job-id)
        (message "Executing job %s" job-id)
        (sit-for 1)
        (kiro-dashboard-refresh)))))

(defun kiro-dashboard-view-job ()
  "View job buffer at point"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Query job
     ((looking-at "  \\[\\([0-9]+\\)\\]")
      (let* ((job-id (match-string 1))
             (buf (get-buffer (kiro-query-buffer-name job-id))))
        (if buf
            (switch-to-buffer buf)
          (message "Buffer not found for job %s" job-id))))
     ;; Shell buffer
     ((looking-at "  [●○] \\([^ ]+\\)")
      (let ((buf-name (match-string 1)))
        (if (get-buffer buf-name)
            (switch-to-buffer buf-name)
          (message "Buffer not found: %s" buf-name))))
     (t (message "No buffer at point")))))

(defun kiro-dashboard-clear-cache ()
  "Clear query cache"
  (interactive)
  (when (yes-or-no-p "Clear query cache? ")
    (clrhash kiro-query-cache)
    (kiro-dashboard-refresh)
    (message "Cache cleared")))

(defun kiro-dashboard-help ()
  "Show dashboard help"
  (interactive)
  (message "g:refresh n:new-query e:execute v:view s:convert-shells c:clear-cache h:history q:quit ?:help"))

(defun kiro-dashboard-open-history ()
  "Open chat history from dashboard."
  (interactive)
  (load-file (expand-file-name "kiro-chat-history.el" kiro-elisp-directory))
  (kiro-chat-history))

(defvar kiro-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'kiro-dashboard-refresh)
    (define-key map (kbd "n") 'kiro-dashboard-new-query)
    (define-key map (kbd "e") 'kiro-dashboard-execute-job)
    (define-key map (kbd "v") 'kiro-dashboard-view-job)
    (define-key map (kbd "c") 'kiro-dashboard-clear-cache)
    (define-key map (kbd "s") 'kiro-dashboard-convert-shells-to-kiro)
    (define-key map (kbd "h") 'kiro-dashboard-open-history)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "RET") 'kiro-dashboard-view-job)
    (define-key map (kbd "?") 'kiro-dashboard-help)
    map))

(define-derived-mode kiro-dashboard-mode special-mode "Kiro-Dashboard"
  "Major mode for Kiro dashboard.

\\{kiro-dashboard-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (hl-line-mode 1)
  (setq-local revert-buffer-function #'kiro-dashboard-revert)
  (run-mode-hooks 'kiro-dashboard-mode-hook))

(defun kiro-dashboard-revert (&rest _)
  "Revert function for dashboard"
  (kiro-dashboard-refresh))

(defun kiro-dashboard ()
  "Open Kiro dashboard"
  (interactive)
  (switch-to-buffer kiro-dashboard-buffer)
  (kiro-dashboard-refresh))

(provide 'kiro-dashboard)
;;; kiro-dashboard.el ends here
