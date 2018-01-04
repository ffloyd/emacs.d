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

;; Powerline
(use-package spaceline
  :config
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))

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
