;;; emacs-setup.el --- Package for maintaining your emacs configuration. Clean up your .emacs!

;; Emacs-setup is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; Emacs-setup is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; emacs-setup is an emacs package that is meant to help make maintaining your
;; emacs setup easier. Through the use of M-x customize, the following can be
;; setup through emacs-setup:

;; Add/remove directories to the load path.
;; Add/remove directories to the environment PATH.
;; Add/remove packages to require, including any accompanying setup elisp code.
;; Set/unset and save keybindings.

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

(defcustom emacs-setup-base-sexp nil
  "List of function names to run during base setup."
  :group 'emacs-setup
  :type '(repeat :tag "S-expression: " (sexp)))

(defcustom emacs-setup-post-sexp nil
  "List of function names to call after setup has loaded."
  :group 'emacs-setup
  :type '(repeat :tag "S-expression: " (sexp)))

;;; *********
;;; FUNCTIONS
;;; *********
(defun emacs-setup ()
  (interactive)
  (add-to-list 'load-path (file-name-directory 
                           (find-lisp-object-file-name 'emacs-setup 'function)))
  (require 'emacs-setup-require)
  (require 'emacs-setup-keys)
  (emacs-setup-require-set-paths)
  (mapc 'eval emacs-setup-base-sexp)
  (let ((errorp (emacs-setup-require-packages)))
    (mapc 'eval emacs-setup-post-sexp)
    (emacs-setup-bind-keys)
    (if errorp
        (message "Setup complete, with errors. Check the *Messages* buffer.")
      (message "Setup complete. Emacs is ready to go!"))))

(defadvice custom-set-variables (after my-advice-custom-setup)
  (emacs-setup))
(ad-activate 'custom-set-variables)

(provide 'emacs-setup)

;;; emacs-setup.el ends here
