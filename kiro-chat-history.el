;;; kiro-chat-history.el --- Browsable pane for all kiro chats and logs -*- lexical-binding: t; -*-

;;; Code:

(defgroup kiro-chat-history nil
  "Kiro chat history browser."
  :group 'kiro
  :prefix "kiro-chat-history-")

(defcustom kiro-chat-directories
  (list (expand-file-name "~/chats/")
        (expand-file-name "~/.emacs.d.kiro/chats/"))
  "Directories to scan for saved chat JSON files."
  :type '(repeat directory)
  :group 'kiro-chat-history)

(defcustom kiro-data-directory (expand-file-name "~/.kiro/")
  "Base directory for kiro session/compact/log data."
  :type 'directory
  :group 'kiro-chat-history)

(defcustom kiro-cli-data-directory (expand-file-name "~/.local/share/kiro-cli/")
  "Directory for kiro-cli data (todo lists, knowledge bases)."
  :type 'directory
  :group 'kiro-chat-history)

(defvar kiro-chat-history-buffer "*Kiro Chat History*")

(defvar kiro-chat-history--collapsed-sections nil
  "Set of collapsed section names.")

;; -- file discovery --

(defun kiro-chat-history--find-files (dir pattern)
  "Recursively find files matching PATTERN under DIR."
  (when (file-directory-p dir)
    (directory-files-recursively dir pattern)))

(defun kiro-chat-history--file-entry (file type)
  "Create an entry plist for FILE with TYPE label."
  (let ((attrs (file-attributes file)))
    (list :file file
          :type type
          :size (file-attribute-size attrs)
          :time (file-attribute-modification-time attrs))))

(defun kiro-chat-history--gather ()
  "Gather all kiro files, return list of entry plists sorted newest first."
  (let ((entries '()))
    ;; Saved chats
    (dolist (dir kiro-chat-directories)
      (dolist (f (kiro-chat-history--find-files dir "\\.json$"))
        (push (kiro-chat-history--file-entry f "Chat") entries)))
    ;; Sessions
    (dolist (f (kiro-chat-history--find-files
                (expand-file-name "sessions/" kiro-data-directory) "\\.txt$"))
      (push (kiro-chat-history--file-entry f "Session") entries))
    ;; Compacts
    (dolist (f (kiro-chat-history--find-files
                (expand-file-name "compact/" kiro-data-directory) "\\.md$"))
      (push (kiro-chat-history--file-entry f "Compact") entries))
    ;; Buffer compacts
    (dolist (f (kiro-chat-history--find-files
                (expand-file-name "buffer-compact/" kiro-data-directory) "\\.md$"))
      (push (kiro-chat-history--file-entry f "BufCompact") entries))
    ;; Logs
    (dolist (f (kiro-chat-history--find-files
                (expand-file-name "logs/" kiro-data-directory) ""))
      (when (file-regular-p f)
        (push (kiro-chat-history--file-entry f "Log") entries)))
    ;; Todo lists
    (dolist (f (kiro-chat-history--find-files
                (expand-file-name "todo-lists/" kiro-cli-data-directory) "\\.json$"))
      (push (kiro-chat-history--file-entry f "Todo") entries))
    ;; Sort newest first
    (sort entries (lambda (a b) (time-less-p (plist-get b :time) (plist-get a :time))))))

;; -- rendering --

(defun kiro-chat-history--format-size (bytes)
  "Human-readable BYTES."
  (cond ((> bytes 1048576) (format "%.1fM" (/ bytes 1048576.0)))
        ((> bytes 1024) (format "%.0fK" (/ bytes 1024.0)))
        (t (format "%dB" bytes))))

(defun kiro-chat-history--format-time (time)
  "Format TIME as date string."
  (format-time-string "%Y-%m-%d %H:%M" time))

(defun kiro-chat-history--type-face (type)
  "Face for TYPE label."
  (pcase type
    ("Chat"       '(:foreground "cyan"))
    ("Session"    '(:foreground "green"))
    ("Compact"    '(:foreground "magenta"))
    ("BufCompact" '(:foreground "blue"))
    ("Log"        '(:foreground "yellow"))
    ("Todo"       '(:foreground "orange"))
    (_            'default)))

(defun kiro-chat-history--insert-section (type entries)
  "Insert a collapsible section for TYPE with ENTRIES."
  (let ((collapsed (member type kiro-chat-history--collapsed-sections))
        (count (length entries)))
    (insert (propertize (format "%s %s (%d)\n"
                                (if collapsed "▶" "▼")
                                type count)
                        'face 'bold
                        'kiro-section type))
    (unless collapsed
      (dolist (entry entries)
        (let ((file (plist-get entry :file))
              (size (plist-get entry :size))
              (time (plist-get entry :time)))
          (insert "  ")
          (insert (propertize (format "%-10s" (kiro-chat-history--format-time time))
                              'face 'shadow))
          (insert " ")
          (insert (propertize (format "%6s" (kiro-chat-history--format-size size))
                              'face 'shadow))
          (insert "  ")
          (insert (propertize (abbreviate-file-name file)
                              'face (kiro-chat-history--type-face type)
                              'kiro-file file))
          (insert "\n"))))
    (insert "\n")))

(defun kiro-chat-history-refresh ()
  "Refresh the chat history buffer."
  (interactive)
  (with-current-buffer (get-buffer-create kiro-chat-history-buffer)
    (let ((inhibit-read-only t)
          (pos (point)))
      (erase-buffer)
      (insert (propertize "KIRO CHAT HISTORY" 'face '(:foreground "cyan" :weight bold)))
      (insert (propertize "  g:refresh  RET:open  TAB:toggle  q:quit\n\n" 'face 'shadow))
      (let* ((all (kiro-chat-history--gather))
             (grouped (seq-group-by (lambda (e) (plist-get e :type)) all)))
        (insert (propertize (format "%d files found\n\n" (length all)) 'face 'shadow))
        (dolist (type '("Chat" "Compact" "Session" "BufCompact" "Log" "Todo"))
          (when-let ((entries (alist-get type grouped nil nil #'equal)))
            (kiro-chat-history--insert-section type entries))))
      (goto-char (min pos (point-max))))
    (kiro-chat-history-mode)))

;; -- commands --

(defun kiro-chat-history-open ()
  "Open file at point."
  (interactive)
  (when-let ((file (get-text-property (point) 'kiro-file)))
    (find-file file)))

(defun kiro-chat-history-toggle-section ()
  "Toggle collapse of section at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when-let ((section (get-text-property (point) 'kiro-section)))
      (if (member section kiro-chat-history--collapsed-sections)
          (setq kiro-chat-history--collapsed-sections
                (delete section kiro-chat-history--collapsed-sections))
        (push section kiro-chat-history--collapsed-sections))
      (kiro-chat-history-refresh))))

(defvar kiro-chat-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'kiro-chat-history-refresh)
    (define-key map (kbd "RET") 'kiro-chat-history-open)
    (define-key map (kbd "TAB") 'kiro-chat-history-toggle-section)
    (define-key map (kbd "q") 'quit-window)
    map))

(define-derived-mode kiro-chat-history-mode special-mode "Kiro-History"
  "Major mode for browsing kiro chat history."
  (setq buffer-read-only t
        truncate-lines t)
  (hl-line-mode 1))

;;;###autoload
(defun kiro-chat-history ()
  "Open the Kiro chat history browser."
  (interactive)
  (switch-to-buffer (get-buffer-create kiro-chat-history-buffer))
  (kiro-chat-history-refresh))

(provide 'kiro-chat-history)
;;; kiro-chat-history.el ends here
