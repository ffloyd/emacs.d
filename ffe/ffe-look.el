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
(use-package nord-theme
  :config
  (setq nord-comment-brightness 20)
  (load-theme 'nord t))

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
