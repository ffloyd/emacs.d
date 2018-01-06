;;; ffe-config.el --- 0xFFE configuration framework

(require 'cl-lib)
(require 'subr-x)

(defvar ffe-config-alist
  nil
  "alist of ffe-config definitions.")

(defvar ffe-config-loaded-list
  nil
  "List of loaded ffe-config definitions.")

(defvar ffe-config-failed-list
  nil
  "List of failed to load ffe-config definitions.")

(defmacro ffe-config-message (scope message &rest args)
  `(let ((base-message (format-message ,message ,@args)))
     (message "ffe-config[%S]: %s" ,scope base-message)))

(cl-defmacro ffe-config (name
                         docstring
                         &rest argplist
                         &key deps init packages config)
  "Append new config to ffe-config-alist."

  (let ((built-plist `(:docstring ,docstring
                                  :deps ,deps
                                  :init (lambda () ,init)
                                  :packages ,packages
                                  :config (lambda () ,config))))
                     
    `(progn
       (ffe-config-message ',name ,docstring)
       (push (cons ',name ',built-plist) ffe-config-alist)
       (ffe-config-message ',name "Defined."))))

(defun ffe-config-p (symbol)
  "Checks config existanse."

  (if (assoc symbol ffe-config-alist) t))

(defun ffe-config-loaded-p (symbol)
  "Checks config existanse."

  (if (member symbol ffe-config-loaded-list) t))

(defun ffe-config-load (name)
  "Load config with given name and its dependencies."

  (if (ffe-config-loaded-p name)
      t

    (ffe-config-message name "Loading...")
    (if-let ((config-plist (alist-get name ffe-config-alist)))
        (condition-case error-cons
            (ffe-config-load--process-plist config-plist name)
          (error
           (ffe-config-message name "Config loading error %S: %s"
                               (car error-cons)
                               (cdr error-cons))
           (push name ffe-config-failed-list)
           nil))
      (error "Configuration %S not found" name))))

(defun ffe-config-load-all ()
  "Loads all defined unloaded configurations in order of definition."

  (dolist (config-cons (reverse ffe-config-alist))
    (let ((config-name (car config-cons)))
      (ffe-config-load config-name))))
          
  
(defun ffe-config-load--process-plist (plist name)
  "Usafe execution of config PLIST"
  
  ;; load dependencies
  (when-let ((deps (plist-get plist :deps)))
    (ffe-config-message name "Checking and loading deps: %S..." deps)
    (cl-mapc #'ffe-config-load deps))
        
  ;; starts init
  (when-let ((init-fun (plist-get plist :init)))
    (ffe-config-message name "Initialization...")
    (funcall init-fun))
        
  ;; process packages
  (when-let ((packages (plist-get plist :packages)))
    (ffe-config-message name "Checking and loading packages: %S..." packages)
    (if (functionp 'straight-use-package)
        (cl-mapc #'ffe-config--process-package packages)
      (error "Straight.el uninitialized")))

  ;; process config
  (when-let ((config-fun (plist-get plist :config)))
    (ffe-config-message name "Configuration...")
    (funcall config-fun))

  ;; mark as loaded
  (push name ffe-config-loaded-list)
    
  (ffe-config-message name "Loaded.")

  t)

(defun ffe-config--process-package (package)
  "Loads and requires package."

  (straight-use-package package)
  (require package))

(provide 'ffe-config)
