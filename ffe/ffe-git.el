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

(use-package magithub
  :config
  (magithub-feature-autoinject t))

(use-package gitignore-mode
  :config
  (add-hook 'gitignore-mode-hook
            (lambda ()
              (setq-local company-backends '(company-files)))))

(use-package gitconfig-mode)

(use-package gitattributes-mode)

(provide 'ffe-git)
