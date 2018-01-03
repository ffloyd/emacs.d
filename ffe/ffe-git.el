;;
;; ffe-git
;;
;; magit and stuff

(require 'ffe-core)

(use-package magit
  :general
  (ffe-git-map
   "s" 'magit-status))

(use-package evil-magit)

(provide 'ffe-git)
