;;; emacs-setup.el --- Package for maintaining your emacs configuration. Clean up your .emacs!

;;; Commentary:

;; emacs-setup is an emacs package that is meant to help make maintaining your
;; emacs setup easier. Through the use of M-x customize, the following can be
;; setup through emacs-setup:

;; Directories to be added to the load path.
;; Packages to require, and any setup elisp code.
;; Keybindings.
;; Emacs window/frame session setups (through the use of revive.el)
;; and more!

;; Installation:

;; The package can be installed with M-x package-list-packages (requires
;; package.el, which is included for emacs 24 but availble for 23). The MELPA
;; repository must be added to your packages setup first. Instructions can be found
;; at http://melpa.milkbox.net/

;; Alternatively, the source can be pulled direclty from github:
;; https://github.com/echosa/emacs-setup

;; If you install via package-list-packages, revive.el will be installed for you.
;; If you do not, you will need to manually get, install, and load revive.el.
;; You can find it at http://www.gentei.org/~yuuji/software/revive.el

;; Usage:

;; In your .emacs, load emacs-setup:

;; (load-file "~/path/to/emacs-setup/emacs-setup.el")

;; then run (emacs-setup-base), which takes two optional arguments:
;;   FRAME - passed to after-make-frame-hook (typically nil)
;;   THE-CUSTOM-FILE - if you store your file which is used by customize in a
;;                     place other than default (e.g. I keep mine in Dropbox)
;;                     you must specifiy that location here.

;; (emacs-setup-base)
;;  - or - 
;; (emacs-setup-base nil "~/path/to/custom-file.el")

;; finally, make a call to

;; (emacs-setup)

;; Once loaded, you can use M-x customize-group emacs-setup to setup your
;; environment.

;; emacs-setup is broken down into several parts, which can each be customized
;; individually:

;; emacs-setup - This is the main part of emacs-setup. You can set your base
;;               directory (your .emacs.d or equivalent), directories to ignore
;;               when recursively adding to load path, and various list of
;;               s-expressions (base, pre, post, etc.) The s-expression lists
;;               can be used to setup things that would normally be in your
;;               .emacs, but are not customizable options. For instance,
;;               (set-frame-font), (set-background-color), (transient-mark-mode),
;;               etc. I'm not going to try an support every option of emacs.
;;               Instead, simply add these configuration lines (one sexp per line)
;;               to the appropriate sexp group, depending on when they need to run.
;;               When emacs-setup-base is run, the last thing it does is run all
;;               the s-expressions in emacs-setup-base-sexp. When emacs-setup is
;;               run, it runs in this order:
;;               - emacs-setup-pre-sexp
;;               - require pacakges via emacs-setup-require
;;               - emacs-setup-pre-layout-sexp
;;               - setup layout - via emacs-setup-layout
;;               - emacs-setup-post-sexp
;;               - bind keys in emacs-setup-keys

;; emacs-setup-keys - This part of emacs-setup allows you to have your keybindings
;;                    all in one place via customize. You can manually add and
;;                    remove keybindings, or you can use the functions
;;                    emacs-setup-bind-key,
;;                    emacs-seutp-unbind-key-by-key, or
;;                    emacs-setup-unbind-key-by-functions
;;                    to interactively bind or unbind keys, which are saved to
;;                    customize for you.
                   
;; emacs-setup-require - This is ths part of emacs-setup where you can tell it
;;                       which packages to load, and give setup s-expressions.
;;                       You can customize the load-path and env-path, whether or
;;                       not to loade elpa and where your package.el is (if not
;;                       using emacs 24). Customizing the variable
;;                       emacs-setup-require-list
;;                       is where you can add which packages should be load, in
;;                       the order you supply them, as well as any configuration
;;                       for each package after it is loaded.
;;                       When emacs-setup is run, if any pacakges fail to load, a
;;                       buffer called *invalid-packages* will be displayed telling
;;                       you which failed.

;; emacs-setup is written and maintained by Brian Zwahr <echosa@gmail.com>

;;; Code:

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

(defcustom emacs-setup-elisp-ignore-dirs '(".svn" ".git")
  "Sub-directories of emacs-setup-base-elisp-dir to ignore when loading 
(i.e. .svn, .git, etc.)."
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
    (message "Path: %s" 'load-path)
    (require 'emacs-setup-require)
    (require 'emacs-setup-keys))
  (when the-custom-file
    (setq custom-file the-custom-file)
    (load custom-file))
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
  (dolist (sexp emacs-setup-base-sexp)
    (eval sexp)))

(defun emacs-setup ()
  (interactive)
  (let (errorp)
    (dolist (sexp emacs-setup-pre-sexp)
      (eval sexp))
    (setq errorp (emacs-setup-require-packages))
    (dolist (sexp emacs-setup-post-sexp)
      (eval sexp))
    (emacs-setup-bind-keys)
    (if errorp
        (message "Setup complete, with errors. Check the *Messages* buffer.")
      (message "Setup complete. Emacs is ready to go!"))))

(defadvice custom-set-variables (after my-advice-custom-setup)
  (emacs-setup-base)
  (emacs-setup))

(ad-activate 'custom-set-variables)

(provide 'emacs-setup)

;;; emacs-setup.el ends here
