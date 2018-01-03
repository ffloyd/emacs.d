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

(provide 'ffe-editing)
