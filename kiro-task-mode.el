;;     kiro.el/kiro-task-mode.el [7012 bytes]
;;; kiro-task-mode.el --- Org-inspired task buffer management for Kiro
;;; DASL-T42-FRACTRAN-ZKP: [(523 . 524) (151 . 152) (499 . 500)]
;;; Content-Hash: 260680809
;;; Version: 1.0.1
;;; Monster-Class: 42
;;; Shard-ID: 3

(require 'org)

(defun kiro-send-command (cmd)
  "Send CMD to kiro CLI via comint if in shell-mode, else just create directory."
  (let ((chat-dir (expand-file-name "chats" default-directory)))
    (make-directory chat-dir t)
    (when (and (eq major-mode 'shell-mode)
               (get-buffer-process (current-buffer)))
      (comint-send-string (current-buffer) (concat cmd "\n")))))

(defconst kiro-task-mode-version "1.0.1"
  "Version of kiro-task-mode")

(defconst kiro-task-mode-fractran-zkp '((523 . 524) (151 . 152) (499 . 500))
  "FRACTRAN zero-knowledge proof of this version")

(defconst kiro-task-mode-content-hash 260680809
  "DASL T42 content hash")

(defvar kiro-task-directory "~/tasks/"
  "Directory for Kiro task files")

;; Kiro buffer naming scheme (Monster primes)
(defvar kiro-buffer-names
  '((2 . "kiro-binary")      ; 2^46 Energy
    (3 . "kiro-ternary")     ; 3^20 Time/Triples
    (5 . "kiro-pentagonal")  ; 5^9 Space
    (7 . "kiro-heptagonal")  ; 7^6 Disk
    (11 . "kiro-hendeca")    ; 11^2 Network
    (13 . "kiro-trideca")    ; 13^3 Scheduling
    (17 . "kiro-wallpaper")  ; 17^1 Maps
    (19 . "kiro-lattice")    ; 19^1 Data/SU(3)
    (23 . "kiro-consciousness") ; 23^1 Awareness
    (29 . "kiro-lunar")      ; 29^1 Phase
    (31 . "kiro-communication") ; 31^1 Chat
    (41 . "kiro-cache")      ; 41^1 Memory
    (47 . "kiro-cambridge")  ; 47^1 Registers
    (59 . "kiro-memory")     ; 59^1 Attention/Vishnu
    (71 . "kiro-omega"))     ; 71^1 Terminal/Brahma
  "Monster prime buffer names")

;; Team member buffers
(defvar kiro-team-buffers
  '("kiro-jocko"    ; Training protocol
    "kiro-arnie"    ; UUCP processing
    "kiro-metzger"  ; Leech embedding
    "kiro-agent1"   ; Coordination
    "kiro-prime"    ; Research lead
    "kiro-875541"   ; ML systems
    "kiro-704400"   ; Consensus
    "kiro-520977")  ; Testing
  "Team member buffers")

(defvar kiro-task-history nil
  "History of task names")

(defun kiro-get-buffer-name (prime-or-name)
  "Get Kiro buffer name for prime number or team member"
  (cond
   ((numberp prime-or-name)
    (or (cdr (assoc prime-or-name kiro-buffer-names))
        (format "kiro-%d" prime-or-name)))
   ((stringp prime-or-name)
    (if (member prime-or-name kiro-team-buffers)
        prime-or-name
      (format "kiro-%s" prime-or-name)))
   (t "kiro-scratch")))

(defun kiro-switch-buffer (target)
  "Switch to Kiro buffer by prime or name"
  (interactive
   (list (completing-read "Kiro buffer: "
                         (append (mapcar (lambda (x) (format "%d" (car x))) kiro-buffer-names)
                                kiro-team-buffers)
                         nil nil)))
  (let* ((prime (string-to-number target))
         (buffer-name (if (> prime 0)
                         (kiro-get-buffer-name prime)
                       target)))
    (switch-to-buffer (get-buffer-create buffer-name))
    (unless (eq major-mode 'org-mode)
      (org-mode))
    (message "Switched to %s" buffer-name)))

(defun kiro-task-rename-buffer (task-name)
  "Rename current buffer to task-<TASK-NAME>.kiro.org"
  (interactive
   (list (read-string "Task name: " nil 'kiro-task-history)))
  (let* ((sanitized-name (replace-regexp-in-string " " "-" task-name))
         (buffer-name (format "task-%s.kiro.org" sanitized-name))
         (file-path (expand-file-name buffer-name kiro-task-directory))
         (was-shell (eq major-mode 'shell-mode)))
    ;; Create directory if needed
    (unless (file-exists-p kiro-task-directory)
      (make-directory kiro-task-directory t))
    
    ;; Rename buffer
    (rename-buffer buffer-name t)
    
    ;; Use appropriate mode
    (if was-shell
        (progn
          (kiro-shell-task-mode)
          (message "✅ Renamed to %s (shell-mode)" buffer-name))
      (progn
        (set-visited-file-name file-path)
        (kiro-task-mode)
        (message "✅ Renamed to %s" buffer-name)))))

(defun kiro-task-new (task-name)
  "Create new Kiro task buffer"
  (interactive
   (list (read-string "New task name: " nil 'kiro-task-history)))
  (let* ((sanitized-name (replace-regexp-in-string " " "-" task-name))
         (buffer-name (format "task-%s.kiro.org" sanitized-name))
         (file-path (expand-file-name buffer-name kiro-task-directory)))
    (find-file file-path)
    (kiro-task-mode)
    (insert (format "#+TITLE: %s\n" task-name))
    (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d")))
    (insert "#+AUTHOR: Kiro Task\n\n")
    (insert "* Overview\n\n")
    (insert "* Tasks\n\n")
    (insert "** TODO \n\n")
    (insert "* Notes\n\n")
    (goto-char (point-max))
    (message "✅ Created task: %s" sanitized-name)))

(defun kiro-task-list ()
  "List all Kiro task files"
  (interactive)
  (let ((tasks (directory-files kiro-task-directory nil "^task-.*\\.kiro\\.org$")))
    (if tasks
        (progn
          (switch-to-buffer "*Kiro Tasks*")
          (erase-buffer)
          (insert "# Kiro Tasks\n\n")
          (dolist (task tasks)
            (insert (format "- [[file:%s][%s]]\n" 
                          (expand-file-name task kiro-task-directory)
                          task)))
          (org-mode)
          (goto-char (point-min)))
      (message "No tasks found in %s" kiro-task-directory))))

(defun kiro-task-save-chat ()
  "Save current Kiro chat to task file"
  (interactive)
  (let* ((base-name (if (buffer-file-name)
                        (file-name-nondirectory (buffer-file-name))
                      (buffer-name)))
         (task-name (replace-regexp-in-string "[*<>]" "" 
                      (replace-regexp-in-string "^task-\\(.*\\)\\.kiro\\.org$" "\\1" base-name)))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (save-file (format "chats/%s-%s.json" task-name timestamp)))
    (kiro-send-command (format "/chat save %s" save-file))
    (message "✅ Saved: %s" save-file)))

(defalias 'kiro-osm-save-chat 'kiro-task-save-chat)

(defvar kiro-task-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c k s") 'kiro-task-save-chat)
    (define-key map (kbd "C-c k r") 'kiro-task-rename-buffer)
    (define-key map (kbd "C-c k n") 'kiro-task-new)
    (define-key map (kbd "C-c k l") 'kiro-task-list)
    (define-key map (kbd "C-c k b") 'kiro-switch-buffer)
    map)
  "Keymap for kiro-task-mode")

(define-derived-mode kiro-task-mode comint-mode "Kiro-Task"
  "Major mode for managing Kiro tasks with comint features.

\\{kiro-task-mode-map}"
  (message "Kiro Task Mode enabled"))

(define-derived-mode kiro-shell-task-mode shell-mode "Kiro-Shell"
  "Major mode for Kiro shell tasks, derived from shell-mode.

\\{kiro-task-mode-map}"
  (use-local-map (make-composed-keymap kiro-task-mode-map (current-local-map)))
  (message "Kiro Shell Task Mode enabled"))

;; Auto-enable for .kiro.org files
(add-to-list 'auto-mode-alist '("\\.kiro\\.org\\'" . kiro-task-mode))

;; Global keybindings
(global-set-key (kbd "C-c k r") 'kiro-task-rename-buffer)
(global-set-key (kbd "C-c k n") 'kiro-task-new)
(global-set-key (kbd "C-c k l") 'kiro-task-list)
(global-set-key (kbd "C-c k b") 'kiro-switch-buffer)

(provide 'kiro-task-mode)
;;; kiro-task-mode.el ends here
