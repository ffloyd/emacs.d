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

	;; Make 0xFFE libraries available
	(add-to-list 'load-path ffe-lib-directory)

	;; 0xFFE features list
	(defvar ffe-features
	  (mapcar
	   (lambda (file)
	     (intern (string-remove-suffix ".el" file)))
	   (directory-files
	    ffe-lib-directory nil
	    "^[a-z-]+\\.el$")))

	;; Load features
	(dolist (feature ffe-features)
	  (condition-case-unless-debug error-data
	      (require feature)
	    (error (warn "Could not load `%S': %s" feature
			 (error-message-string error-data))))))))
