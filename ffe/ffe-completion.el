;;
;; ffe-completion
;;
;; * ivy/counsel stuff
;;

(require 'ffe-core)

(use-package ivy
  :diminish
  :general

  ;; Ivy-based interface to standard commands
  ("C-s" 'swiper)
  ("M-x" 'counsel-M-x)
  ("C-x C-f" 'counsel-find-file)
  ("<f1> f" 'counsel-describe-function)
  ("<f1> v" 'counsel-describe-variable)
  ("<f1> l" 'counsel-find-library)
  ("<f2> i" 'counsel-info-lookup-symbol)
  ("<f2> u" 'counsel-unicode-char)

  ;; Ivy minibuffer tweaks
  (ivy-minibuffer-map
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line)

  ;; File-related utils
  (ffe-files-map
   "f" 'counsel-find-file
   "r" 'counsel-recentf
   "l" 'counsel-find-library)

  ;; Buffer-related utils
  (ffe-buffer-map
   "b" 'ivy-switch-buffer
   "d" 'kill-this-buffer
   "n" 'next-buffer
   "p" 'previous-buffer)

  ;; Search related utils
  (ffe-search-map
   "f" 'counsel-rg
   "s" 'counsel-grep-or-swiper)

  ;; Help related utils
  (ffe-help-map
   "f" 'counsel-describe-function
   "v" 'counsel-describe-variable)

  ;; M-x shortcut
  (general-nmap
   :prefix ffe-leader-normal
   "SPC" 'counsel-M-x)

  :config
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode))

(provide 'ffe-completion)
