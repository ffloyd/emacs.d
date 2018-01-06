;;; init.el --- 0xFFE emacs config

;;
;; Prevent package.el from running
;;

(setq package-enable-at-startup nil)

;;
;; Essential requires
;;

(require 'cl-lib)
(require 'subr-x)

;;
;; Emacs version workaround
;;

(defvar ffe-minimum-emacs-version "25.1"
  "0xFFE does not support any Emacs version below this.")

(if (version< emacs-version ffe-minimum-emacs-version)
    (error "0xFFE requires at least Emacs %s, but you are running Emacs %s"
           ffe-minimum-emacs-version emacs-version))

;;
;; Load ffe-config feature
;;

(defvar ffe-directory
  (file-name-directory
   (or load-file-name buffer-file-name))
  "Path to 0xFFE directory, ~/emacs.d/ in most cases.")

(load-file (expand-file-name "ffe-config.el" ffe-directory))

;;
;; 0xFFE configs declarations
;;

(ffe-config
 pack "Straight.el as package manager."
 :init (let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
               (bootstrap-version 3))
           (unless (file-exists-p bootstrap-file)
             (with-current-buffer
                 (url-retrieve-synchronously
                  "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                  'silent 'inhibit-cookies)
               (goto-char (point-max))
               (eval-print-last-sexp)))
           (load bootstrap-file nil 'nomessage)))

(ffe-config
 benchmark-init "Benchmarks for 0xFFE startup process."
 :deps (pack)
 :packages (benchmark-init esup)
 :config (add-hook 'after-init-hook 'benchmark-init/deactivate))

(ffe-config
 custom-file "Separate custom file custom.el"
 :init (progn
         (defvar ffe-custom-file
           (expand-file-name "custom.el" ffe-directory)
           "0xFFE file for customs.")

         (setq custom-file ffe-custom-file)
         (add-hook 'after-init-hook #'(lambda () (load-file ffe-custom-file)))))

(ffe-config
 common-tweaks "Common core Emacs tweaks."
 :deps (pack)
 :packages (better-defaults no-littering exec-path-from-shell)
 :config (progn
           (setq exec-path-from-shell-check-startup-files nil)
           (setq ring-bell-function 'ignore)
           ;; Bigger initial frame
           (add-to-list 'default-frame-alist '(height . 40))
           (add-to-list 'default-frame-alist '(width . 120))))

(ffe-config
 evil "Basic Evil setup."
 :deps (pack)
 :packages (evil)
 :config (evil-mode 1))

(ffe-config
 which-key "Keybindings suggestions via which-key."
 :deps (pack)
 :packages (which-key)
 :config (which-key-mode)) 

(ffe-config
 keys "Keybindings control via general.el and common keymap definitions."
 :deps (pack)
 :packages (general)
 :config (progn
           (general-evil-setup)

           (defvar ffe-leader-normal
             "SPC"
             "0xFFE leader key in normal mode")

           ;; define base keymaps
           (general-nmap
            :prefix ffe-leader-normal
            "a" '(:prefix-command ffe-app-map :wk "App")
            "b" '(:prefix-command ffe-buffer-map :wk "Buffer")
            "e" '(:prefix-command ffe-emacs-map :wk "Emacs")
            "f" '(:prefix-command ffe-files-map :wk "File")
            "g" '(:prefix-command ffe-git-map :wk "Git")
            "h" '(:prefix-command ffe-help-map :wk "Help")
            "m" '(:prefix-command ffe-mode-map :wk "Mode")
            "t" '(:prefix-command ffe-toggle-map :wk "Toggle")
            "s" '(:prefix-command ffe-search-map :wk "Search"))))

(ffe-config
 benchmark-init-keys "Keybindings for initialization benchmark tools."
 :deps (keys benchmark-init)
 :config (general-def ffe-emacs-map
           "i" 'benchmark-init/show-durations-tabulated
           "I" 'benchmark-init/show-durations-tree
           "B" 'esup))

(ffe-config
 osx "Basic osx tuning"
 :deps (keys)
 :config (progn
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
             "s-X" 'undo-tree-redo)))

(ffe-config
 look "Font, frame tweaks and nord theme."
 :deps (pack)
 :packages ((nord-theme
             :type git
             :host github
             :repo "arcticicestudio/nord-emacs"
             :branch "develop"))
 :config (progn
           (set-frame-font "Source Code Pro 13" nil t)
           
           (setq nord-comment-brightness 13)
           (load-theme 'nord t)))

(ffe-config winum "Jump to window by SPC-number."
            :deps (pack keys)
            :packages (winum)
            :config (progn
                      (winum-mode)
                      (general-nmap
                       :prefix ffe-leader-normal
                       "0" 'winum-select-window-0
                       "1" 'winum-select-window-1
                       "2" 'winum-select-window-2
                       "3" 'winum-select-window-3
                       "4" 'winum-select-window-4
                       "5" 'winum-select-window-5
                       "6" 'winum-select-window-6)))

(ffe-config modeline "Spaceline as modeline."
            :deps (pack keys winum)
            :packages (spaceline)
            :config (progn
                      (require 'spaceline-config)

                      (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
                      (spaceline-spacemacs-theme)
                      (spaceline-toggle-minor-modes-off)

                      (general-def ffe-toggle-map
                        "m" 'spaceline-toggle-minor-modes)))

(ffe-config dashboard "Pretty dashboard as start screen."
            :deps (pack)
            :packages (dashboard)
            :config (dashboard-setup-startup-hook))

(ffe-config toggle-fullscreen "Toggling macos fullscreen."
            :deps (pack keys)
            :config (progn
                      (defun ffe/toggle-fullscreen ()
                        (interactive)
                        "Toggles frame fullscreen (native macOS one)."
                        (set-frame-parameter nil 'fullscreen
                                             (if (frame-parameter nil 'fullscreen)
                                                 nil
                                               'fullscreen)))

                      (general-def ffe-toggle-map
                        "F" 'ffe/toggle-fullscreen)))

(ffe-config undo-tree "Undo tree."
            :deps (pack evil)
            :packages (undo-tree)
            :config (progn
                      (global-undo-tree-mode)
                      (general-def ffe-app-map
                        "u" 'undo-tree-visualize)))

(ffe-config rainbow-delimiters "Rainbow delimeters."
            :deps (pack)
            :packages (rainbow-delimiters)
            :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(ffe-config linum "Line numbers."
            :deps (pack keys)
            :packages (linum linum-relative)
            :config (general-def ffe-toggle-map
                      "n" 'linum-mode
                      "N" 'linum-relative-toggle))

(ffe-config ivy "Ivy & Counsel basic setup"
            :deps (pack keys)
            :packages (ivy)
            :config (progn
                      (setq ivy-count-format "(%d/%d) ")
                      (ivy-mode)

                      ;; Ivy-based interface to standard commands
                      (general-def
                        "C-s" 'swiper
                        "M-x" 'counsel-M-x
                        "C-x C-f" 'counsel-find-file
                        "<f1> f" 'counsel-describe-function
                        "<f1> v" 'counsel-describe-variable
                        "<f1> l" 'counsel-find-library
                        "<f2> i" 'counsel-info-lookup-symbol
                        "<f2> u" 'counsel-unicode-char)

                      ;; Ivy minibuffer tweaks
                      (general-def ivy-minibuffer-map
                       "C-j" 'ivy-next-line
                       "C-k" 'ivy-previous-line)

                      ;; File-related utils
                      (general-def ffe-files-map
                        "f" 'counsel-find-file
                        "r" 'counsel-recentf
                        "l" 'counsel-find-library)

                      ;; Buffer-related utils
                      (general-def ffe-buffer-map
                       "b" 'ivy-switch-buffer
                       "d" 'kill-this-buffer
                       "n" 'next-buffer
                       "p" 'previous-buffer)

                      ;; Search related utils
                      (general-def ffe-search-map
                       "f" 'counsel-rg
                       "s" 'counsel-grep-or-swiper)

                      ;; Help related utils
                      (general-def ffe-help-map
                       "f" 'counsel-describe-function
                       "v" 'counsel-describe-variable)

                      ;; M-x shortcut
                      (general-nmap
                       :prefix ffe-leader-normal
                       "SPC" 'counsel-M-x)))

(ffe-config company "Company autocompletion"
            :deps (pack keys)
            :packages (company)
            :config (progn
                      (global-company-mode +1)

                      (general-def company-active-map
                       "C-j" 'company-select-next-or-abort
                       "C-k" 'company-select-previous-or-abort)

                      (general-def company-search-map
                       "C-j" 'company-select-next-or-abort
                       "C-k" 'company-select-previous-or-abort)))


(ffe-config magit "Magit and extesions."
            :deps (pack keys)
            :packages (magit evil-magit magithub)
            :config (progn
                      (general-def ffe-git-map
                        "s" 'magit-status)

                      (magithub-feature-autoinject t)))

(ffe-config gitfiles ".git* files support."
            :deps (pack company)
            :packages (gitignore-mode gitconfig-mode gitattributes-mode)
            :config (add-hook 'gitignore-mode-hook
                              (lambda ()
                                (setq-local company-backends '(company-files)))))

;;
;; Load configurations
;;

(ffe-config-load-all)
