;;
;; ffe-look
;;
;; * font config
;; * theme config
;; * modeline config

(require 'ffe-core)

;; Source Code Pro
(set-frame-font "Source Code Pro 13" nil t)

;; Nord clean arctic theme
;; Many useful options accessible only via develop branch =(
(use-package nord-theme
  :straight
  (nord-theme
   :type git
   :host github
   :repo "arcticicestudio/nord-emacs"
   :branch "develop")
  :config
  (setq nord-comment-brightness 13)
  (load-theme 'nord t))

;; Bigger initial frame
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 120))

;; Window numbers
(use-package winum
  :general
  (general-nmap
   :prefix ffe-leader-normal
   "0" 'winum-select-window-0
   "1" 'winum-select-window-1
   "2" 'winum-select-window-2
   "3" 'winum-select-window-3
   "4" 'winum-select-window-4
   "5" 'winum-select-window-5
   "6" 'winum-select-window-6)
  :config
  (winum-mode))

;; Powerline
(use-package spaceline
  :config
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))

;; Dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

;;
;; Look & Feel control functions
;;
(defun ffe/toggle-fullscreen ()
  (interactive)
  "Toggles frame fullscreen (native macOS one)."
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil
                         'fullscreen)))

;; Keybindings
(general-def ffe-toggle-map
  "F" 'ffe/toggle-fullscreen)

(provide 'ffe-look)
