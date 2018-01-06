;;; ffe-config.el --- 0xFFE configuration framework

(require 'cl-lib)

(defvar ffe-config-alist
  nil
  "alist of ffe-config definitions.")

(defvar ffe-config-loaded-list
  nil
  "List of loaded ffe-config definitions.")

(defvar ffe-config-failed-list
  nil
  "List of failed to load ffe-config definitions.")

(cl-defmacro ffe-config (name
                         docstring
                         &rest argplist
                         &key deps init packages config)
  "Append new config to ffe-config-alist."

  (let ((built-plist `(:docstring ,docstring
                                  :deps ',deps
                                  :init (lambda () ,init)
                                  :packages ',packages
                                  :config (lambda () ,config))))
                     
    `(push (cons ',name ',built-plist) ffe-config-alist)))

(defun ffe-config-p (symbol)
  "Checks config existanse."

  (if (assoc symbol ffe-config-alist) t))

(defun ffe-config-loaded-p (symbol)
  "Checks config existanse."

  (if (member symbol ffe-config-loaded-list) t))

(defun ffe-config-load (name)
  "Load config with given name and its dependencies."

  (if-let ((config-plist (alist-get name ffe-config-alist)))
      (condition-case error-cons
          (ffe-config-load--process-plist config-plist)
        (error
         (warn "Error %S happened while loading %S[%s]: %s"
               (car error-cons)
               name
               (plist-get :docstring config-plist)
               (cdr error-cons))
         nil))
    (error "Configuration %S not found" name)))

(defun ffe-config-load-all ()
  "Loads all defined unloaded configurations."

  (dolist (config-cons ffe-config-alist)
    (let ((config-name (car config-cons)))
      (ffe-config-load config-name))))
          
  
(defun ffe-config-load--process-plist (plist)
  "Usafe execution of config PLIST"
  
  ;; load dependencies
  (when-let ((deps (plist-get :deps plist)))
    (dolist (config-name deps) (ffe-config-load config-name)))
        
  ;; starts init
  (when-let ((init-fun (plist-get :init plist)))
    (funcall init-fun))
        
  ;; process packages
  (when-let ((recepies (plist-get :packages plist)))
    (if (featurep 'straight)
        (dolist (recepie packages) #'straight-use-package)
      (error "Straight uninitialized")))
    
  ;; process config
  (when-let ((config-fun (plist-get :config plist)))
    (funcall config-fun))

  ;; mark as loaded
  (push name ffe-config-loaded-list)

  t)

(provide 'ffe-config)
