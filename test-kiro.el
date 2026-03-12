;;; test-kiro.el --- Test script for kiro.el

;; Load kiro.el
(add-to-list 'load-path (expand-file-name "~/.emacs.d/kiro.el"))
(require 'kiro)
(require 'kiro-task-mode)

(defun test-kiro-basic ()
  "Test basic kiro.el loading"
  (message "✓ kiro.el loaded successfully")
  (when (boundp 'kiro-task-mode-version)
    (message "✓ Version: %s" kiro-task-mode-version))
  (when (boundp 'kiro-task-mode-content-hash)
    (message "✓ Content hash: %s" kiro-task-mode-content-hash)))

(defun test-kiro-commands ()
  "Test that all commands are defined"
  (let ((commands '(kiro-dashboard
                   kiro-prompt-queue
                   kiro-homedir-projects
                   kiro-spool-start
                   kiro-query
                   kiro-sessions
                   kiro-buffers
                   kiro-save-all
                   kiro-task-save-chat
                   kiro-task-rename-buffer
                   kiro-task-new
                   kiro-task-list
                   kiro-switch-buffer)))
    (dolist (cmd commands)
      (if (fboundp cmd)
          (message "✓ %s defined" cmd)
        (message "✗ %s NOT defined" cmd)))))

(defun test-kiro-keybindings ()
  "Test that keybindings are set"
  (let ((bindings '(("C-c k s" . kiro-task-save-chat)
                   ("C-c k c" . kiro-task-save-compact-save)
                   ("C-c k r" . kiro-task-rename-buffer)
                   ("C-c k n" . kiro-task-new)
                   ("C-c k l" . kiro-task-list)
                   ("C-c k b" . kiro-switch-buffer))))
    (dolist (binding bindings)
      (let* ((key (car binding))
             (cmd (cdr binding))
             (bound (key-binding (kbd key))))
        (if (eq bound cmd)
            (message "✓ %s → %s" key cmd)
          (message "✗ %s → %s (got %s)" key cmd bound))))))

(defun test-kiro-directories ()
  "Test directory creation"
  (let ((dirs '("~/tasks/" "chats/")))
    (dolist (dir dirs)
      (let ((expanded (expand-file-name dir)))
        (if (file-directory-p expanded)
            (message "✓ Directory exists: %s" expanded)
          (message "○ Directory will be created: %s" expanded))))))

(defun run-all-tests ()
  "Run all kiro.el tests"
  (interactive)
  (message "\n=== Testing kiro.el ===\n")
  (test-kiro-basic)
  (message "")
  (test-kiro-commands)
  (message "")
  (test-kiro-keybindings)
  (message "")
  (test-kiro-directories)
  (message "\n=== Tests complete ==="))

;; Run tests
(run-all-tests)

;;; test-kiro.el ends here
