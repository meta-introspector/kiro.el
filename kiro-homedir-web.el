;;; kiro-homedir-web.el --- HOMEDIR WEB CONVENTION - Monster Symmetry Index

(require 'json)

(defvar kiro-homedir-base "~/projects/"
  "Base directory for homedir web projects")

(defvar kiro-homedir-nix-base "~/nix/"
  "Base directory for Nix projects (15k+ linked projects)")

(defvar kiro-homedir-data-base "/mnt/data1/"
  "Base directory for data1 projects")

(defvar kiro-homedir-port-base 10000
  "Base port for services")

(defvar kiro-monster-primes '(2 3 5 7 11 13 17 19 23 29 31 41 47 59 71)
  "Monster primes for door system")

(defun kiro-homedir-door-address (layer sector zone)
  "Create door address LAYER.SECTOR.ZONE"
  (format "%d.%d.%d" layer sector zone))

(defun kiro-homedir-port (layer sector zone)
  "Calculate service port for door address"
  (+ kiro-homedir-port-base (* layer 100) sector zone))

(defun kiro-homedir-project-path (project)
  "Get path for PROJECT"
  (expand-file-name project kiro-homedir-base))

(defun kiro-homedir-read-symmetries (project)
  "Read .monster/symmetries.json for PROJECT"
  (let ((sym-file (expand-file-name ".monster/symmetries.json" 
                                     (kiro-homedir-project-path project))))
    (when (file-exists-p sym-file)
      (with-temp-buffer
        (insert-file-contents sym-file)
        (json-parse-buffer :object-type 'alist)))))

(defun kiro-homedir-create-monster-dir (project)
  "Create .monster/ directory for PROJECT"
  (interactive "sProject: ")
  (let* ((project-path (kiro-homedir-project-path project))
         (monster-dir (expand-file-name ".monster" project-path))
         (sym-file (expand-file-name "symmetries.json" monster-dir)))
    (make-directory monster-dir t)
    (unless (file-exists-p sym-file)
      (with-temp-buffer
        (insert (json-encode `((project . ,project)
                              (symmetries . ((input . [71])
                                           (output . [71])
                                           (invariants . [])))
                              (complexity . ((lines . 0)
                                           (cyclomatic . 0))))))
        (write-region (point-min) (point-max) sym-file)))
    (message "Created .monster/ for %s" project)))

(defun kiro-homedir-list-projects ()
  "List all homedir web projects"
  (interactive)
  (let ((projects (append 
                   (mapcar (lambda (p) (cons p "projects"))
                          (directory-files kiro-homedir-base nil "^[^.]"))
                   (mapcar (lambda (p) (cons p "nix"))
                          (directory-files kiro-homedir-nix-base nil "^[^.]"))
                   (mapcar (lambda (p) (cons p "data1"))
                          (directory-files kiro-homedir-data-base nil "^[^.]")))))
    (switch-to-buffer "*Kiro Projects*")
    (erase-buffer)
    (insert (propertize "╔═══════════════════════════════════════════════════════════╗\n" 'face 'bold))
    (insert (propertize "║           🌐 HOMEDIR WEB PROJECTS 🌐                     ║\n" 'face '(:foreground "cyan" :weight bold)))
    (insert (propertize "╚═══════════════════════════════════════════════════════════╝\n\n" 'face 'bold))
    (insert (propertize (format "Total: %d projects (%d ~/projects + %d ~/nix + %d /mnt/data1)\n\n" 
                               (length projects)
                               (length (directory-files kiro-homedir-base nil "^[^.]"))
                               (length (directory-files kiro-homedir-nix-base nil "^[^.]"))
                               (length (directory-files kiro-homedir-data-base nil "^[^.]")))
                       'face 'bold))
    (dolist (proj-pair projects)
      (let* ((project (car proj-pair))
             (source (cdr proj-pair))
             (base (cond ((string= source "nix") kiro-homedir-nix-base)
                        ((string= source "data1") kiro-homedir-data-base)
                        (t kiro-homedir-base)))
             (path (expand-file-name project base))
             (has-monster (file-exists-p (expand-file-name ".monster" path)))
             (symmetries (when has-monster (kiro-homedir-read-symmetries-from path))))
        (insert (propertize "• " 'face 'success))
        (insert (propertize project 'face '(:foreground "yellow")))
        (insert (propertize (format " [%s]" source) 'face 'shadow))
        (when has-monster
          (insert (propertize " ✓MONSTER" 'face '(:foreground "green")))
          (when symmetries
            (let ((input (alist-get 'input (alist-get 'symmetries symmetries)))
                  (output (alist-get 'output (alist-get 'symmetries symmetries))))
              (insert (format " [%s→%s]" 
                            (mapconcat #'number-to-string input ",")
                            (mapconcat #'number-to-string output ","))))))
        (insert "\n")))
    (goto-char (point-min))))

(defun kiro-homedir-read-symmetries-from (path)
  "Read .monster/symmetries.json from PATH"
  (let ((sym-file (expand-file-name ".monster/symmetries.json" path)))
    (when (file-exists-p sym-file)
      (with-temp-buffer
        (insert-file-contents sym-file)
        (json-parse-buffer :object-type 'alist)))))

(defun kiro-homedir-find-by-symmetry (input-prime output-prime)
  "Find projects with INPUT-PRIME → OUTPUT-PRIME symmetry"
  (interactive "nInput prime: \nnOutput prime: ")
  (let ((matches '()))
    (dolist (project (directory-files kiro-homedir-base nil "^[^.]"))
      (let ((symmetries (kiro-homedir-read-symmetries project)))
        (when symmetries
          (let ((input (alist-get 'input (alist-get 'symmetries symmetries)))
                (output (alist-get 'output (alist-get 'symmetries symmetries))))
            (when (and (member input-prime input)
                      (member output-prime output))
              (push project matches))))))
    (if matches
        (message "Projects with %d→%d: %s" input-prime output-prime 
                (string-join matches ", "))
      (message "No projects found with %d→%d symmetry" input-prime output-prime))
    matches))

(defun kiro-homedir-open-project (project)
  "Open PROJECT in dired"
  (interactive 
   (list (completing-read "Project: " 
                         (directory-files kiro-homedir-base nil "^[^.]"))))
  (dired (kiro-homedir-project-path project)))

(defun kiro-homedir-serve-project (project)
  "Generate nginx config for PROJECT"
  (interactive 
   (list (completing-read "Project: " 
                         (directory-files kiro-homedir-base nil "^[^.]"))))
  (let* ((path (kiro-homedir-project-path project))
         (public-html (expand-file-name "public_html" path))
         (config (format "location /~%s/ {\n    alias %s/;\n    autoindex on;\n}\n" 
                        project public-html)))
    (with-current-buffer (get-buffer-create "*Nginx Config*")
      (erase-buffer)
      (insert config)
      (switch-to-buffer (current-buffer)))))

(provide 'kiro-homedir-web)
;;; kiro-homedir-web.el ends here
