;;; **************
;;; CUSTOMIZATIONS
;;; **************

(defgroup emacs-setup nil
  "Easy emacs setup."
  :group 'environment)

(defcustom emacs-setup-elisp-base-dir "~/.emacs.d"
  "Base directory where you keep your .el files to be loaded."
  :group 'emacs-setup
  :type '(file :must-match t))

(defcustom emacs-setup-elisp-ignore-dirs nil
  "Sub-directories of emacs-setup-base-elisp-dir to ignore when loading 
(i.e. .svn, etc.)."
  :group 'emacs-setup
  :type '(repeat :tag "Sub-directory name: " (string)))

(defcustom emacs-setup-base-sexp nil
  "List of function names to run during base setup."
  :group 'emacs-setup
  :type '(repeat :tag "S-expression: " (sexp)))

(defcustom emacs-setup-pre-sexp nil
  "List of function names to call before setup is run."
  :group 'emacs-setup
  :type '(repeat :tag "S-expression: " (sexp)))

(defcustom emacs-setup-pre-layout-sexp nil
  "List of function names to call during setup before any layout stuff is run."
  :group 'emacs-setup
  :type '(repeat :tag "S-expression: " (sexp)))

(defcustom emacs-setup-post-sexp nil
  "List of function names to call after setup has loaded."
  :group 'emacs-setup
  :type '(repeat :tag "S-expression: " (sexp)))

;;; *********
;;; FUNCTIONS
;;; *********
(defun emacs-setup-base (&optional frame the-custom-file)
  "Performs initial setup. The frame argument is there for 
after-make-frame-hook."
  (interactive)
  (let ((dir (file-name-directory 
              (find-lisp-object-file-name 'emacs-setup-base 'function))))
    (add-to-list 'load-path dir)
    (require 'emacs-setup-require)
    (require 'emacs-setup-layout)
    (require 'emacs-setup-keys))
  (when the-custom-file
    (setq custom-file the-custom-file))
  (if custom-file
      (load custom-file)
    (error "No custom file set."))
  ;; This must come first for requires to work from here on out!
  (emacs-setup-load-recursive-el-directories
   emacs-setup-elisp-base-dir
   emacs-setup-elisp-ignore-dirs)
  (dolist (dir emacs-setup-load-path-list)
    (add-to-list 'load-path dir))
  (let ((env-path (getenv "PATH")))
    (dolist (dir emacs-setup-env-path-list)
      (setq env-path (concat dir ":" env-path)))
    (setenv "PATH" env-path))
  (emacs-setup-call-base-sexp))

(defun emacs-setup ()
  (interactive)
  (emacs-setup-call-pre-sexp)
  (emacs-setup-require-packages)
  (emacs-setup-call-pre-layout-sexp)
  (emacs-setup-layout)
  (emacs-setup-call-post-sexp)
  (emacs-setup-bind-keys)
  (message "Setup complete. Emacs is ready to go!"))

(defun emacs-setup-call-base-sexp ()
  (dolist (sexp emacs-setup-base-sexp)
    (eval sexp)))

(defun emacs-setup-call-pre-sexp ()
  (dolist (sexp emacs-setup-pre-sexp)
    (eval sexp)))

(defun emacs-setup-call-pre-layout-sexp ()
  (dolist (sexp emacs-setup-pre-layout-sexp)
    (eval sexp)))

(defun emacs-setup-call-post-sexp ()
  (dolist (sexp emacs-setup-post-sexp)
    (eval sexp)))

(provide 'emacs-setup)
