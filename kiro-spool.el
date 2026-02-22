;;; kiro-spool.el --- Read Kiro CLI spool and respond via Emacs

(require 'json)

(defvar kiro-spool-dir "~/kiro-spool/"
  "Directory for Kiro CLI spool files")

(defvar kiro-spool-timer nil
  "Timer for polling spool")

(defun kiro-spool-read-request (file)
  "Read JSON request from FILE"
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil :false-object nil)))

(defun kiro-spool-write-response (file data)
  "Write JSON response to FILE"
  (with-temp-buffer
    (insert (json-encode data))
    (write-region (point-min) (point-max) file)))

(defun kiro-spool-handle-request (req)
  "Handle spool request REQ"
  (let ((action (alist-get 'action req))
        (data (alist-get 'data req)))
    (pcase (intern action)
      ('eval (format "%S" (eval (read (alist-get 'expr data)))))
      ('insert (insert (alist-get 'text data)) "inserted")
      ('goto-line (goto-line (alist-get 'line data)) "moved")
      ('find-file (find-file (alist-get 'path data)) "opened")
      ('save-buffer (save-buffer) "saved")
      ('get-buffer-content (buffer-substring-no-properties (point-min) (point-max)))
      ('query-plan (progn
                     (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-query-planner.el"))
                     (format "%S" (kiro-query-plan (alist-get 'pattern data)))))
      ('query-execute (progn
                        (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-query-planner.el"))
                        (kiro-query-execute (alist-get 'job_id data))
                        "executing"))
      ('query-results (progn
                        (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-query-planner.el"))
                        (kiro-query-get-results (alist-get 'job_id data))))
      ('dashboard (progn
                    (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-dashboard.el"))
                    (kiro-dashboard)
                    "dashboard opened"))
      (_ (error "Unknown action: %s" action)))))

(defun kiro-spool-process ()
  "Process pending spool requests"
  (when (file-directory-p kiro-spool-dir)
    (dolist (file (directory-files kiro-spool-dir t "^request-.*\\.json$"))
      (let* ((req (kiro-spool-read-request file))
             (id (alist-get 'id req))
             (result (condition-case err
                         (kiro-spool-handle-request req)
                       (error (format "Error: %s" err))))
             (response-file (expand-file-name (format "response-%s.json" id) kiro-spool-dir)))
        (kiro-spool-write-response response-file `((id . ,id) (result . ,result)))
        (delete-file file)))))

(defun kiro-spool-start ()
  "Start spool polling"
  (interactive)
  (unless (file-directory-p kiro-spool-dir)
    (make-directory kiro-spool-dir t))
  (when kiro-spool-timer
    (cancel-timer kiro-spool-timer))
  (setq kiro-spool-timer (run-with-timer 1 1 'kiro-spool-process))
  (message "🚀 Kiro spool started: %s" kiro-spool-dir))

(defun kiro-spool-stop ()
  "Stop spool polling"
  (interactive)
  (when kiro-spool-timer
    (cancel-timer kiro-spool-timer)
    (setq kiro-spool-timer nil)
    (message "⏹ Kiro spool stopped")))

(provide 'kiro-spool)
;;; kiro-spool.el ends here
