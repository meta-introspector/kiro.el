;;; test-interactive.el --- Interactive test for kiro.el

;; Load kiro
(add-to-list 'load-path (expand-file-name "~/.emacs.d/kiro.el"))
(require 'kiro)

(defun test-kiro-interactive ()
  "Run interactive tests for kiro.el"
  (interactive)
  (let ((results '()))
    
    ;; Test 1: Create a test task
    (condition-case err
        (progn
          (kiro-task-new "test-interactive-task")
          (push "✓ Created test task" results))
      (error (push (format "✗ Failed to create task: %s" err) results)))
    
    ;; Test 2: Check buffer name
    (if (string-match-p "task-test-interactive-task" (buffer-name))
        (push "✓ Buffer named correctly" results)
      (push (format "✗ Buffer name wrong: %s" (buffer-name)) results))
    
    ;; Test 3: Check major mode
    (if (eq major-mode 'kiro-task-mode)
        (push "✓ kiro-task-mode active" results)
      (push (format "✗ Wrong mode: %s" major-mode) results))
    
    ;; Test 4: Check keybindings in current buffer
    (let ((bindings '(("C-c k s" . kiro-task-save-chat)
                     ("C-c k c" . kiro-task-save-compact-save)
                     ("C-c k r" . kiro-task-rename-buffer))))
      (dolist (binding bindings)
        (let* ((key (car binding))
               (cmd (cdr binding))
               (bound (key-binding (kbd key))))
          (if (eq bound cmd)
              (push (format "✓ %s bound to %s" key cmd) results)
            (push (format "✗ %s not bound (got %s)" key bound) results)))))
    
    ;; Test 5: Check file path
    (if (and (buffer-file-name)
             (string-match-p "tasks/task-test-interactive-task\\.kiro\\.org" (buffer-file-name)))
        (push "✓ File path correct" results)
      (push (format "✗ File path wrong: %s" (buffer-file-name)) results))
    
    ;; Test 6: Check buffer content
    (if (save-excursion
          (goto-char (point-min))
          (and (search-forward "#+TITLE:" nil t)
               (search-forward "* Overview" nil t)
               (search-forward "* Tasks" nil t)))
        (push "✓ Buffer content has org template" results)
      (push "✗ Buffer content missing template" results))
    
    ;; Display results
    (switch-to-buffer "*Kiro Test Results*")
    (erase-buffer)
    (insert "# Kiro.el Interactive Test Results\n\n")
    (insert (format "Tested at: %s\n\n" (current-time-string)))
    (dolist (result (reverse results))
      (insert result "\n"))
    (insert "\n## Next Steps\n\n")
    (insert "1. Try M-x kiro-task-list to see all tasks\n")
    (insert "2. Try M-x kiro-switch-buffer to switch between buffers\n")
    (insert "3. Open a shell and run kiro-cli chat, then press C-c k s\n")
    (insert "4. Try M-x kiro-save-all to test the full workflow\n")
    (org-mode)
    (goto-char (point-min))
    (message "Test complete! See *Kiro Test Results* buffer")))

;; Run the test
(test-kiro-interactive)

;;; test-interactive.el ends here
