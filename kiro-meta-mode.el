;;; kiro-meta-mode.el --- Quasi-meta mode combining org+comint+task+introspection+RDF+emoji

(require 'org)
(require 'comint)
(require 'kiro-task-mode)

;;; RDF(a) escaped format
(defvar kiro-rdf-prefix-map
  '(("rdf" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    ("rdfs" . "http://www.w3.org/2000/01/rdf-schema#")
    ("kiro" . "https://kiro.zone/")
    ("monster" . "https://kiro.zone/monster/")
    ("zone" . "https://kiro.zone/zone/"))
  "RDF prefix mappings")

(defun kiro-rdf-escape (text)
  "Escape TEXT for RDF literal"
  (replace-regexp-in-string "\"" "\\\\\"" text))

(defun kiro-rdf-triple (subject predicate object)
  "Create RDF triple as org property"
  (format ":RDF: <%s> <%s> \"%s\"" subject predicate (kiro-rdf-escape object)))

;;; Emoji mappings (Monster primes)
(defvar kiro-emoji-map
  '((2 . "⚡")    ; Energy
    (3 . "⏰")    ; Time
    (5 . "✨")    ; Space
    (7 . "🎭")    ; Disk
    (11 . "🌐")   ; Network
    (13 . "📅")   ; Scheduling
    (17 . "🗺️")   ; Maps
    (19 . "🔷")   ; Lattice
    (23 . "🧠")   ; Consciousness
    (29 . "🌙")   ; Lunar
    (31 . "💬")   ; Communication
    (41 . "💾")   ; Cache
    (47 . "🎓")   ; Cambridge
    (59 . "👁️")   ; Memory/Vishnu
    (71 . "👑"))  ; Omega/Brahma
  "Monster prime emoji mappings")

(defun kiro-emoji (prime)
  "Get emoji for PRIME"
  (or (cdr (assoc prime kiro-emoji-map)) "🔢"))

;;; Sharable invariant URLs
(defun kiro-url-encode (text)
  "URL encode TEXT"
  (url-hexify-string text))

(defun kiro-make-url (type id &optional params)
  "Create sharable Kiro URL"
  (let ((base "https://kiro.zone")
        (query (if params (concat "?" (mapconcat
                                       (lambda (p) (format "%s=%s" (car p) (kiro-url-encode (cdr p))))
                                       params "&"))
                 "")))
    (format "%s/%s/%s%s" base type id query)))

;;; Introspection
(defvar kiro-introspection-history nil)

(defun kiro-introspect ()
  "Record current state"
  (let ((state (list :time (current-time)
                     :buffer (buffer-name)
                     :point (point)
                     :mode major-mode)))
    (push state kiro-introspection-history)
    state))

(defun kiro-meta-insert-header ()
  "Insert Kiro meta header"
  (interactive)
  (let* ((prime (read-number "Monster prime (2-71): " 71))
         (emoji (kiro-emoji prime))
         (url (kiro-make-url "zone" (number-to-string prime)))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
    (insert (format "#+TITLE: %s Zone %d\n" emoji prime))
    (insert (format "#+DATE: %s\n" timestamp))
    (insert (format "#+URL: %s\n" url))
    (insert (format "#+PRIME: %d\n" prime))
    (insert (format "#+EMOJI: %s\n" emoji))
    (insert ":PROPERTIES:\n")
    (insert (kiro-rdf-triple (concat "zone:" (number-to-string prime))
                             "rdf:type" "kiro:Zone"))
    (insert "\n")
    (insert (kiro-rdf-triple (concat "zone:" (number-to-string prime))
                             "kiro:prime" (number-to-string prime)))
    (insert "\n:END:\n\n")))

(defun kiro-meta-insert-task (task-name)
  "Insert task with RDF metadata"
  (interactive "sTask: ")
  (let* ((id (format "task-%d" (random 1000000)))
         (url (kiro-make-url "task" id)))
    (insert (format "** TODO %s\n" task-name))
    (insert ":PROPERTIES:\n")
    (insert (format ":ID: %s\n" id))
    (insert (format ":URL: %s\n" url))
    (insert (kiro-rdf-triple (concat "kiro:" id) "rdfs:label" task-name))
    (insert "\n:END:\n\n")))

(defun kiro-meta-comint-send ()
  "Send current line to comint and record"
  (interactive)
  (kiro-introspect)
  (comint-send-input))

;;; Mode definition
(defvar kiro-meta-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m h") 'kiro-meta-insert-header)
    (define-key map (kbd "C-c m t") 'kiro-meta-insert-task)
    (define-key map (kbd "C-c m i") 'kiro-introspect)
    (define-key map (kbd "C-c m e") (lambda () (interactive)
                                      (insert (kiro-emoji (read-number "Prime: ")))))
    (define-key map (kbd "C-c m u") (lambda () (interactive)
                                      (insert (kiro-make-url
                                              (read-string "Type: ")
                                              (read-string "ID: ")))))
    (define-key map (kbd "C-c m r") (lambda () (interactive)
                                      (insert (kiro-rdf-triple
                                              (read-string "Subject: ")
                                              (read-string "Predicate: ")
                                              (read-string "Object: ")))))
    map)
  "Keymap for kiro-meta-mode")

(define-derived-mode kiro-meta-mode org-mode "Kiro-Meta"
  "Quasi-meta mode: org+comint+task+introspection+RDF+emoji+URLs

\\{kiro-meta-mode-map}"
  (setq-local org-todo-keywords
              '((sequence "TODO(t)" "DOING(d)" "BLOCKED(b)" "|" "DONE(✓)" "CANCELLED(✗)")))
  ;; Enable comint features if in shell buffer
  (when (derived-mode-p 'comint-mode)
    (setq-local comint-process-echoes t))
  (kiro-introspect)
  (message "🚀 Kiro Meta Mode enabled"))

;; Auto-enable for .kiro.meta files
(add-to-list 'auto-mode-alist '("\\.kiro\\.meta\\'" . kiro-meta-mode))

(define-minor-mode kiro-meta-minor-mode
  "Kiro meta features as minor mode (for shells)"
  :lighter " 🚀"
  :keymap kiro-meta-mode-map
  (when kiro-meta-minor-mode
    (kiro-introspect)
    (message "🚀 Kiro Meta Minor Mode enabled")))

(provide 'kiro-meta-mode)
;;; kiro-meta-mode.el ends here
