;; Kiro Emacs Setup
;; Add to your ~/.emacs or ~/.emacs.d/init.el

;; Add kiro.el to load path
(add-to-list 'load-path "~/.emacs.d/kiro.el")

;; Load Kiro
(require 'kiro)

;; Optional: Global keybindings
(global-set-key (kbd "C-c k d") 'kiro-dashboard)
(global-set-key (kbd "C-c k q") 'kiro-query)
(global-set-key (kbd "C-c k s") 'kiro-spool-start)

;; Optional: Start spool service on Emacs startup
;; (add-hook 'after-init-hook 'kiro-spool-start)
