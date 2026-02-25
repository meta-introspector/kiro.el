;;; kiro-tunnel.el --- Connect multiple Emacs instances via spool tunnel

(require 'json)

(defvar kiro-tunnel-peers (make-hash-table :test 'equal)
  "Connected Emacs peers")

(defvar kiro-tunnel-id nil
  "This Emacs instance ID")

(defvar kiro-tunnel-dir "~/kiro-spool/tunnel/"
  "Tunnel directory for peer communication")

(defun kiro-tunnel-init ()
  "Initialize tunnel for this Emacs"
  (interactive)
  (unless kiro-tunnel-id
    (setq kiro-tunnel-id (format "emacs-%d" (emacs-pid))))
  (make-directory kiro-tunnel-dir t)
  (kiro-tunnel-announce)
  (run-with-timer 1 5 'kiro-tunnel-poll)
  (message "Kiro tunnel initialized: %s" kiro-tunnel-id))

(defun kiro-tunnel-announce ()
  "Announce this Emacs to peers"
  (let ((announce-file (expand-file-name 
                        (format "peer-%s.json" kiro-tunnel-id)
                        kiro-tunnel-dir)))
    (with-temp-buffer
      (insert (json-encode `((id . ,kiro-tunnel-id)
                            (pid . ,(emacs-pid))
                            (time . ,(current-time-string)))))
      (write-region (point-min) (point-max) announce-file))))

(defun kiro-tunnel-poll ()
  "Poll for peer messages"
  (when (file-directory-p kiro-tunnel-dir)
    (dolist (file (directory-files kiro-tunnel-dir t "^msg-.*\\.json$"))
      (when (string-match (format "msg-.*-to-%s\\.json" kiro-tunnel-id) file)
        (kiro-tunnel-handle-message file)))))

(defun kiro-tunnel-handle-message (file)
  "Handle incoming message from FILE"
  (let* ((msg (with-temp-buffer
                (insert-file-contents file)
                (json-parse-buffer :object-type 'alist)))
         (from (alist-get 'from msg))
         (action (alist-get 'action msg))
         (data (alist-get 'data msg)))
    (delete-file file)
    (pcase (intern action)
      ('eval (kiro-tunnel-eval-remote from data))
      ('sync-dashboard (kiro-tunnel-sync-dashboard from))
      ('ping (kiro-tunnel-pong from))
      (_ (message "Unknown tunnel action: %s" action)))))

(defun kiro-tunnel-eval-remote (from expr)
  "Evaluate EXPR from peer FROM"
  (let ((result (condition-case err
                    (eval (read expr))
                  (error (format "Error: %s" err)))))
    (kiro-tunnel-send from 'eval-result (format "%S" result))))

(defun kiro-tunnel-sync-dashboard (from)
  "Sync dashboard state to peer FROM"
  (load-file (expand-file-name "~/.emacs.d/kiro.el/kiro-dashboard.el"))
  (let ((jobs (hash-table-count kiro-query-jobs))
        (cache (hash-table-count kiro-query-cache)))
    (kiro-tunnel-send from 'dashboard-state 
                     (json-encode `((jobs . ,jobs) (cache . ,cache))))))

(defun kiro-tunnel-send (to action data)
  "Send message to peer TO"
  (let ((msg-file (expand-file-name 
                   (format "msg-%s-to-%s.json" kiro-tunnel-id to)
                   kiro-tunnel-dir)))
    (with-temp-buffer
      (insert (json-encode `((from . ,kiro-tunnel-id)
                            (action . ,action)
                            (data . ,data))))
      (write-region (point-min) (point-max) msg-file))))

(defun kiro-tunnel-list-peers ()
  "List connected peers"
  (interactive)
  (let ((peers '()))
    (dolist (file (directory-files kiro-tunnel-dir t "^peer-.*\\.json$"))
      (let* ((peer (with-temp-buffer
                     (insert-file-contents file)
                     (json-parse-buffer :object-type 'alist)))
             (id (alist-get 'id peer)))
        (unless (string= id kiro-tunnel-id)
          (push id peers))))
    (if peers
        (message "Peers: %s" (string-join peers ", "))
      (message "No peers connected"))))

(defun kiro-tunnel-ping (peer-id)
  "Ping PEER-ID"
  (interactive "sPeer ID: ")
  (kiro-tunnel-send peer-id 'ping "ping")
  (message "Pinged %s" peer-id))

(defun kiro-tunnel-pong (from)
  "Respond to ping from FROM"
  (message "Pong from %s" from))

(provide 'kiro-tunnel)
;;; kiro-tunnel.el ends here
