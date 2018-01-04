;;
;; ffe-company
;;
;; Basic company.el installation and configuration
;;

(require 'ffe-core)

(use-package company
  :general
  (company-active-map
    "C-j" 'company-select-next-or-abort
    "C-k" 'company-select-previous-or-abort)
  (company-search-map
    "C-j" 'company-select-next-or-abort
    "C-k" 'company-select-previous-or-abort)
  :config
  (global-company-mode +1))

(provide 'ffe-company)
