;;
;; ffe-editing
;;
;; Common text edit stuff.
;;

(require 'ffe-core)

(use-package undo-tree
  :diminish
  :general
  (ffe-app-map
   "u" 'undo-tree-visualize)
  :config
  (global-undo-tree-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; linum
(general-def ffe-toggle-map
  "n" 'linum-mode)
(use-package linum-relative
  :general
  (ffe-toggle-map
   "N" 'linum-relative-toggle))

(provide 'ffe-editing)
