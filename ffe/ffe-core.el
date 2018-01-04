;;
;; ffe-core
;;
;; 0xFFE core configuration file. It resposible for:
;;
;; * core package management (straight.el, use-package etc.)
;; * core emacs tweaks (better-defaults, no-littering, etc)
;; * basic evil setup
;; * general.el
;; * basic keymaps
;;

;; We aren't using package.el, but Emacs will initialize it for us if
;; we don't tell it not to.
(setq package-enable-at-startup nil)

;; We are using a package manager called straight.el.
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package for package requiring
;; we don't need any lazy loading
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-demand t)

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Better ENV-variables management
(use-package exec-path-from-shell
  :config
  ;; Disable emacs warning
  (setq exec-path-from-shell-check-startup-files nil)
  ;; Initialize PATH from shell
  (exec-path-from-shell-initialize))

;; Core emacs configuration
(use-package better-defaults)
(use-package no-littering)

;; Evil initialization
(use-package evil
  :config
  (evil-mode 1))

;; Improve bindings discoverability
(use-package which-key
  :config
  (which-key-mode))

;; Keybindings
(defvar ffe-leader-normal ;; TODO: make it custom
  "SPC"
  "0xFFE leader key in normal mode")

(use-package general
  :config
  (general-evil-setup)
  ;; Define base keymaps
  ;; TODO: create general definer with preset ffe-leader-normal and ffe-leader-non-normal
  (general-nmap
   :prefix ffe-leader-normal
   "a" '(:prefix-command ffe-app-map :wk "App")
   "b" '(:prefix-command ffe-buffer-map :wk "Buffer")
   "f" '(:prefix-command ffe-files-map :wk "File")
   "g" '(:prefix-command ffe-git-map :wk "Git")
   "h" '(:prefix-command ffe-help-map :wk "Help")
   "m" '(:prefix-command ffe-mode-map :wk "Mode")
   "t" '(:prefix-command ffe-toggle-map :wk "Toggle")
   "s" '(:prefix-command ffe-search-map :wk "Search")))

;; macos specific behaviour
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(general-def
  "s-q" 'save-buffers-kill-terminal
  "s-v" 'yank
  "s-c" 'evil-yank
  "s-a" 'mark-whole-buffer
  "s-x" 'kill-region
  "s-w" 'delete-window
  "s-W" 'delete-frame
  "s-n" 'make-frame
  "s-z" 'undo-tree-undo
  "s-X" 'undo-tree-redo)

;; disable ring bell
(setq ring-bell-function 'ignore)

;; benchmarking initialization
(use-package esup)

(provide 'ffe-core)
