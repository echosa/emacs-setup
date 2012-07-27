;;; emacs-setup-require.el --- Functions for emacs-setup to manage requiring and loading pacakges.

;;; Commentary:
;; This file holds the functions for let emacs-setup handle loading and
;; requiring packages.

(require 'emacs-setup-util)

;;; **************
;;; CUSTOMIZATIONS
;;; **************

;;; Code:

(defgroup emacs-setup-require nil
  "Emacs setup layout customizations."
  :group 'emacs-setup)

(defcustom emacs-setup-require-base-dir "~/.emacs.d"
  "Base directory where you keep your .el files to be loaded."
  :group 'emacs-setup-require
  :type '(file :must-match t))

(defcustom emacs-setup-require-ignore-dirs '(".svn" ".git")
  "Sub-directories of emacs-setup-base-elisp-dir to ignore when loading 
(i.e. .svn, .git, etc.)."
  :group 'emacs-setup-require
  :type '(repeat :tag "Sub-directory name: " (string)))

(defcustom emacs-setup-load-path-list nil
  "This is a list of directory paths to add to the Emacs `load-path'."
  :group 'emacs-setup-require
  :type '(repeat :tag "Directory: " (directory)))

(defcustom emacs-setup-env-path-list nil
  "This is a list of directories to add to the Emacs env PATH."
  :group 'emacs-setup-require
  :type '(repeat :tag "Directory: " (directory)))

(defcustom emacs-setup-require-list nil
  "Holds the names of all packages to be required.
This is an alist where the key is the package name that should be passed to
\(require) and the value is an optional list of s-expressions after the
require statement is called."
  :group 'emacs-setup-require
  :type '(alist :key-type (string :tag "Package Name: ")
                :value-type
                (repeat :tag "Configuration Lines: " (sexp))))

(defcustom emacs-setup-load-elpa nil
  "If t, load elpa from `emacs-setup-elpa-package-file'."
  :group 'emacs-setup-require
  :type 'boolean)

(defcustom emacs-setup-elpa-package-file "~/.emacs.d/elpa/package.el"
  "This points to the ELPA package.el, if used."
  :group 'emacs-setup-require
  :type 'file)

;;; *********
;;; FUNCTIONS
;;; *********

(defun emacs-setup-require-set-paths ()
  "Set up the load-path and PATH."
  (add-to-list 'load-path emacs-setup-require-base-dir)
  (let ((default-directory emacs-setup-require-base-dir))
    (normal-top-level-add-subdirs-to-load-path))
  (add-to-list 'load-path emacs-setup-load-path-list)
  (setenv "PATH" (mapconcat 'concat
                            (append emacs-setup-env-path-list
                                    (list (getenv "PATH")))
                            ":")))

(defun emacs-setup-load-package-el ()
  "Return the appropriate package.el."
  (when (and emacs-setup-load-elpa
             (not (string= "" emacs-setup-elpa-package-file))
             (file-readable-p emacs-setup-elpa-package-file))
      (load (expand-file-name emacs-setup-elpa-package-file)))
  (fboundp 'package-initialize))

(defun emacs-setup-require-packages ()
  "Load the packages in `emacs-setup-require-list'.
`emacs-setup-require-list' is a list of cons cells with
the car being a string of the name of the packages and an optional cdr that is
any functions that need to run to accompany the package.  Also loads elpa if
user has that option set."
  (interactive)
  (let ((failed ""))
    (condition-case e
        (progn
          ;; elpa
          (when (emacs-setup-load-package-el)
            (package-initialize))
          ;; required packages
          (when (emacs-setup-thing-exists 'emacs-setup-require-list)
            (dolist (package emacs-setup-require-list)
              (let ((package-symbol (intern (car package))))
                (unless (featurep package-symbol) (require package-symbol))
                (if (featurep package-symbol)
                    (when (cdr package) (mapc 'eval (cdr package)))
                  (setq failed (concat failed (car package) " ")))))
            (unless (string= "" failed)
              (message (concat "Some packages were not loaded: " failed)))))
      (error
       (message "There was an error loading packages: %s" failed)
       (message "%s" (error-message-string e))))
    (not (string= "" failed))))

(defun emacs-setup-add-feature ()
  "Add an entry to `emacs-setup-require-list'."
  (interactive)
  (let ((feature (read-string "Require: "))
        config
        sexp)
    (condition-case nil
        (while (setq sexp (read-from-minibuffer "s-expression: " nil nil t))
          (add-to-list 'config sexp))
      ;; we catch error to signify no s-expression was entered
      (error
       (set-variable
        'emacs-setup-require-list
        (add-to-list 'emacs-setup-require-list (cons feature config) t))
       (customize-save-variable 'emacs-setup-require-list
                                emacs-setup-require-list)
       (message "Added feature %s with configuration: %s" feature config)))))

(defun emacs-setup-remove-feature ()
  "Remove an entry from emacs-seutp-require-list."
  (interactive)
  (let (features)
    (dolist (feature emacs-setup-require-list)
      (add-to-list 'features (car feature)))
    (let* ((feature (completing-read "Feature: " features nil t)))
      (setq features
            (delete (cons feature
                          (cdr (assoc feature emacs-setup-require-list)))
                    emacs-setup-require-list))
      (set-variable 'emacs-setup-require-list features)
      (customize-save-variable
       'emacs-setup-require-list
       emacs-setup-require-list)
      (message "Removed feature: %s" feature))))

(provide 'emacs-setup-require)

;;; emacs-setup-require.el ends here
