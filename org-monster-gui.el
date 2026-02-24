;;; org-monster-gui.el --- Semantic GUI from Org-Monster ontology
;;; DASL-T42-FRACTRAN-ZKP: [(523 . 524) (151 . 152) (499 . 500)]
;;; Version: 1.0.0

(require 'org-monster)
(require 'svg)

;;; Semantic GUI Components

(defun org-monster-gui-color-for-shard (shard)
  "Get color for Monster shard."
  (let ((hue (/ (* shard 360.0) 71)))
    (format "hsl(%d, 70%%, 50%%)" (floor hue))))

(defun org-monster-gui-render-task (heading cid class)
  "Render task as SVG widget."
  (let* ((shard (plist-get class :shard))
         (seph (cdr (plist-get class :sephirah)))
         (todo (org-monster-shard-to-todo shard))
         (priority (org-monster-class-to-priority 
                    (car (plist-get class :monster-classes))))
         (color (org-monster-gui-color-for-shard shard))
         (svg (svg-create 400 100)))
    
    ;; Background
    (svg-rectangle svg 0 0 400 100 :fill color :opacity 0.2 :rx 10)
    
    ;; Shard indicator
    (svg-circle svg 30 50 20 :fill color)
    (svg-text svg (format "%d" shard) :x 22 :y 55 :fill "white" :font-size 16)
    
    ;; Heading
    (svg-text svg heading :x 60 :y 35 :font-size 18 :font-weight "bold")
    
    ;; TODO state
    (svg-rectangle svg 60 45 60 20 :fill color :rx 5)
    (svg-text svg todo :x 65 :y 60 :fill "white" :font-size 12)
    
    ;; Priority
    (svg-text svg (format "[#%s]" priority) :x 130 :y 60 :font-size 12)
    
    ;; Sephirah
    (svg-text svg seph :x 60 :y 80 :font-size 10 :fill "#666")
    
    svg))

(defun org-monster-gui-render-kanban ()
  "Render Kanban board from org headings."
  (let* ((columns '("TODO" "DOING" "REVIEW" "DONE"))
         (col-width 250)
         (svg (svg-create (* (length columns) col-width) 600)))
    
    ;; Column headers
    (dotimes (i (length columns))
      (let* ((col (nth i columns))
             (x (* i col-width))
             (shard (pcase col
                      ("TODO" 0)
                      ("DOING" 13)
                      ("REVIEW" 26)
                      ("DONE" 57)))
             (color (org-monster-gui-color-for-shard shard)))
        (svg-rectangle svg x 0 col-width 50 :fill color :opacity 0.3)
        (svg-text svg col :x (+ x 10) :y 30 :font-size 20 :font-weight "bold")))
    
    svg))

(defun org-monster-gui-render-tree ()
  "Render Tree of Life with tasks."
  (let ((svg (svg-create 600 800)))
    
    ;; Sephiroth positions (Tree of Life layout)
    (let ((sephiroth '((300 50 "Kether" 0)
                       (200 150 "Chokmah" 1)
                       (400 150 "Binah" 2)
                       (150 300 "Chesed" 3)
                       (450 300 "Geburah" 4)
                       (300 350 "Tiphareth" 5)
                       (150 500 "Netzach" 6)
                       (450 500 "Hod" 7)
                       (300 550 "Yesod" 8)
                       (300 700 "Malkuth" 9))))
      
      ;; Draw paths (22 Tarot)
      (svg-line svg 300 50 200 150 :stroke "#ccc" :stroke-width 2)
      (svg-line svg 300 50 400 150 :stroke "#ccc" :stroke-width 2)
      (svg-line svg 200 150 400 150 :stroke "#ccc" :stroke-width 2)
      (svg-line svg 200 150 150 300 :stroke "#ccc" :stroke-width 2)
      (svg-line svg 400 150 450 300 :stroke "#ccc" :stroke-width 2)
      (svg-line svg 150 300 300 350 :stroke "#ccc" :stroke-width 2)
      (svg-line svg 450 300 300 350 :stroke "#ccc" :stroke-width 2)
      (svg-line svg 300 350 150 500 :stroke "#ccc" :stroke-width 2)
      (svg-line svg 300 350 450 500 :stroke "#ccc" :stroke-width 2)
      (svg-line svg 150 500 300 550 :stroke "#ccc" :stroke-width 2)
      (svg-line svg 450 500 300 550 :stroke "#ccc" :stroke-width 2)
      (svg-line svg 300 550 300 700 :stroke "#ccc" :stroke-width 2)
      
      ;; Draw Sephiroth
      (dolist (seph sephiroth)
        (let* ((x (nth 0 seph))
               (y (nth 1 seph))
               (name (nth 2 seph))
               (idx (nth 3 seph))
               (color (org-monster-gui-color-for-shard (* idx 7))))
          (svg-circle svg x y 40 :fill color :opacity 0.7)
          (svg-text svg name :x (- x 30) :y (+ y 5) :fill "white" :font-size 12))))
    
    svg))

(defun org-monster-gui-render-wheel ()
  "Render Monster wheel (71 shards)."
  (let* ((svg (svg-create 600 600))
         (cx 300)
         (cy 300)
         (radius 250))
    
    ;; Draw 71 shard segments
    (dotimes (i 71)
      (let* ((angle1 (* i (/ (* 2 pi) 71)))
             (angle2 (* (1+ i) (/ (* 2 pi) 71)))
             (x1 (+ cx (* radius (cos angle1))))
             (y1 (+ cy (* radius (sin angle1))))
             (x2 (+ cx (* radius (cos angle2))))
             (y2 (+ cy (* radius (sin angle2))))
             (color (org-monster-gui-color-for-shard i)))
        (svg-line svg cx cy x1 y1 :stroke color :stroke-width 2)
        (svg-line svg x1 y1 x2 y2 :stroke color :stroke-width 2)))
    
    ;; Center circle
    (svg-circle svg cx cy 50 :fill "#333")
    (svg-text svg "71" :x 285 :y 310 :fill "white" :font-size 30)
    
    svg))

;;;###autoload
(defun org-monster-gui-show-task ()
  "Show GUI for current task."
  (interactive)
  (let* ((heading (org-get-heading t t t t))
         (cid (org-monster-cid-from-heading))
         (class (kiro-monster-tree-classify cid))
         (svg (org-monster-gui-render-task heading cid class)))
    (with-current-buffer (get-buffer-create "*Org-Monster-GUI*")
      (erase-buffer)
      (svg-insert-image svg)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun org-monster-gui-show-kanban ()
  "Show Kanban board GUI."
  (interactive)
  (let ((svg (org-monster-gui-render-kanban)))
    (with-current-buffer (get-buffer-create "*Org-Monster-Kanban*")
      (erase-buffer)
      (svg-insert-image svg)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun org-monster-gui-show-tree ()
  "Show Tree of Life GUI."
  (interactive)
  (let ((svg (org-monster-gui-render-tree)))
    (with-current-buffer (get-buffer-create "*Org-Monster-Tree-GUI*")
      (erase-buffer)
      (svg-insert-image svg)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun org-monster-gui-show-wheel ()
  "Show Monster wheel GUI."
  (interactive)
  (let ((svg (org-monster-gui-render-wheel)))
    (with-current-buffer (get-buffer-create "*Org-Monster-Wheel*")
      (erase-buffer)
      (svg-insert-image svg)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun org-monster-gui-dashboard ()
  "Show complete Monster dashboard."
  (interactive)
  (delete-other-windows)
  (org-monster-gui-show-wheel)
  (split-window-right)
  (other-window 1)
  (org-monster-gui-show-tree)
  (split-window-below)
  (other-window 1)
  (org-monster-gui-show-kanban))

(provide 'org-monster-gui)
;;; org-monster-gui.el ends here
