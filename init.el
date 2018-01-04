(setq package-enable-at-startup nil)

(defvar ffe-minimum-emacs-version "25.1"
  "0xFFE does not support any Emacs version below this.")

(if (version< emacs-version ffe-minimum-emacs-version)
    (warn (concat "Radian Emacs requires at least Emacs %s, "
		  "but you are running Emacs %s")
	  ffe-minimum-emacs-version emacs-version)
  
  ;; We have a modern Emacs, proceed with init.
  (unwind-protect
      (with-demoted-errors "%S"
	
	;; Require some libraries that everyone needs, just to be
	;; explicit about it.
	(require 'cl-lib)
	(require 'subr-x)

	(defvar ffe-directory
	  (file-name-directory
	   (or load-file-name buffer-file-name))
	  "Path to 0xFFE directory, ~/emacs.d/ in most cases.")

	(defvar ffe-lib-directory
	  (expand-file-name "ffe/" ffe-directory)
	  "Path to the 0xFFE libraries.")

	;; 0xFFE features list
	(defvar ffe-features
	  (mapcar
	   (lambda (file)
	     (intern (string-remove-suffix ".el" file)))
	   (directory-files
	    ffe-lib-directory nil
	    "^[a-z-]+\\.el$")))

        (defvar ffe-user-init-file
          (expand-file-name "init.user.el" ffe-directory)
          "0xFFE user configuration.")

        (defvar ffe-user-init-example-file
          (expand-file-name "init.user.example.el" ffe-directory)
          "0xFFE user configuration.")

        (unless (file-exists-p ffe-user-init-file)
          (copy-file ffe-user-init-example-file ffe-user-init-file))

	;; Make 0xFFE libraries available
	(add-to-list 'load-path ffe-lib-directory)

        ;; User pre-init
	(load-file ffe-user-init-file)
        (ffe/user-pre-init)
        
	;; Load features
	(dolist (feature ffe-features)
	  (condition-case-unless-debug error-data
	      (require feature)
	    (error (warn "Could not load `%S': %s" feature
			 (error-message-string error-data)))))

        ;; User post-init
        (ffe/user-post-init))))
