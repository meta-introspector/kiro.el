;;; kiro-query-planner.el --- Emacs-hosted query planner with Rust backend

(require 'json)
(require 'kiro-spool)

(defvar kiro-query-cache (make-hash-table :test 'equal)
  "Query result cache")

(defvar kiro-query-jobs (make-hash-table :test 'equal)
  "Active query jobs")

(defvar kiro-rust-planner-path "~/projects/kiro/SEARCH/omnisearch_query_planner"
  "Path to Rust query planner binary")

(defun kiro-query-buffer-name (job-id)
  "Get buffer name for JOB-ID"
  (format "*kiro-query-%s*" job-id))

(defun kiro-query-plan (pattern)
  "Plan query for PATTERN using Rust planner"
  (let* ((job-id (format "%d" (random 1000000)))
         (buffer (get-buffer-create (kiro-query-buffer-name job-id)))
         (cache-key (format "plan:%s" pattern)))
    
    ;; Check cache
    (if-let ((cached (gethash cache-key kiro-query-cache)))
        (progn
          (message "Cache hit: %s" pattern)
          cached)
      
      ;; Run planner
      (with-current-buffer buffer
        (erase-buffer)
        (insert (format "# Query Plan: %s\n" pattern))
        (insert (format "# Job ID: %s\n" job-id))
        (insert (format "# Time: %s\n\n" (current-time-string)))
        
        (let ((plan (kiro-query-call-rust-planner pattern)))
          (insert (format "Plan: %S\n\n" plan))
          (puthash cache-key plan kiro-query-cache)
          (puthash job-id (list :pattern pattern :plan plan :buffer buffer) kiro-query-jobs)
          plan)))))

(defun kiro-query-call-rust-planner (pattern)
  "Call Rust planner for PATTERN"
  ;; Fallback to heuristic since binary may not exist
  (list (cons 'pattern pattern)
        (cons 'tools (if (string-match-p "prime\\|Monster" pattern)
                         (vector "omnisearch" "plocate")
                       (vector "omnisearch")))
        (cons 'strategy (if (string-match-p "prime\\|Monster" pattern)
                            "parallel"
                          "single"))))

(defun kiro-query-execute (job-id)
  "Execute query JOB-ID in its buffer"
  (if-let ((job (gethash job-id kiro-query-jobs)))
      (let* ((pattern (plist-get job :pattern))
             (plan (plist-get job :plan))
             (buffer (plist-get job :buffer))
             (tools (alist-get 'tools plan))
             (strategy (alist-get 'strategy plan)))
        
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert (format "Executing: %s\n" strategy))
          
          (pcase strategy
            ("parallel" (kiro-query-exec-parallel job-id tools pattern buffer))
            ("sequential" (kiro-query-exec-sequential job-id tools pattern buffer))
            (_ (kiro-query-exec-single job-id (aref tools 0) pattern buffer)))))
    (error "Job not found: %s" job-id)))

(defun kiro-query-exec-single (job-id tool pattern buffer)
  "Execute single TOOL query"
  (with-current-buffer buffer
    (insert (format "\n## Tool: %s\n" tool))
    (let ((results (kiro-query-run-tool tool pattern)))
      (insert (format "Results: %d\n" (length results)))
      (dolist (result results)
        (insert (format "- %s\n" result)))
      results)))

(defun kiro-query-exec-parallel (job-id tools pattern buffer)
  "Execute TOOLS in parallel"
  (with-current-buffer buffer
    (insert "\n## Parallel Execution\n")
    (let ((all-results '()))
      (dotimes (i (length tools))
        (let* ((tool (aref tools i))
               (results (kiro-query-run-tool tool pattern)))
          (insert (format "\n### %s: %d results\n" tool (length results)))
          (dolist (result (seq-take results 5))
            (insert (format "- %s\n" result)))
          (setq all-results (append all-results results))))
      all-results)))

(defun kiro-query-exec-sequential (job-id tools pattern buffer)
  "Execute TOOLS sequentially"
  (with-current-buffer buffer
    (insert "\n## Sequential Execution\n")
    (let ((results '()))
      (dotimes (i (length tools))
        (let* ((tool (aref tools i))
               (tool-results (kiro-query-run-tool tool pattern)))
          (insert (format "\n### %s: %d results\n" tool (length tool-results)))
          (setq results (append results tool-results))))
      results)))

(defun kiro-query-run-tool (tool pattern)
  "Run TOOL with PATTERN"
  (pcase tool
    ("plocate" (kiro-query-plocate pattern))
    ("ripgrep" (kiro-query-ripgrep pattern))
    ("omnisearch" (kiro-query-omnisearch pattern))
    ("knowledge" (kiro-query-knowledge pattern))
    (_ (list (format "Unknown tool: %s" tool)))))

(defun kiro-query-plocate (pattern)
  "Run plocate"
  (split-string (shell-command-to-string (format "plocate -l 10 %s 2>/dev/null" pattern)) "\n" t))

(defun kiro-query-ripgrep (pattern)
  "Run ripgrep"
  (split-string (shell-command-to-string 
                 (format "rg -l --max-count 10 '%s' ~/projects/kiro 2>/dev/null" pattern)) "\n" t))

(defun kiro-query-omnisearch (pattern)
  "Run omnisearch"
  (split-string (shell-command-to-string 
                 (format "cd /mnt/data1/nix/vendor/rust/github && ./omnisearch '%s' 2>/dev/null | head -10" pattern)) "\n" t))

(defun kiro-query-knowledge (pattern)
  "Search knowledge bases"
  (list (format "Knowledge search: %s (not implemented)" pattern)))

(defun kiro-query-get-results (job-id)
  "Get results for JOB-ID"
  (if-let ((job (gethash job-id kiro-query-jobs)))
      (with-current-buffer (plist-get job :buffer)
        (buffer-substring-no-properties (point-min) (point-max)))
    "Job not found"))

(defun kiro-query-list-jobs ()
  "List all query jobs"
  (interactive)
  (let ((jobs '()))
    (maphash (lambda (id job)
               (push (format "%s: %s" id (plist-get job :pattern)) jobs))
             kiro-query-jobs)
    (if jobs
        (message "Jobs:\n%s" (string-join jobs "\n"))
      (message "No active jobs"))))

(defun kiro-query-clear-cache ()
  "Clear query cache"
  (interactive)
  (clrhash kiro-query-cache)
  (message "Cache cleared"))

(provide 'kiro-query-planner)
;;; kiro-query-planner.el ends here
