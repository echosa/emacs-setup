;;; emacs-setup-keys.el --- Functions for handling key bindings in the emacs-setup package.

;;; Commentary:
;; This file contains the functions that allow the emacs-setup package to handle
;; managing keybindings.

;;; Code:

;;; **************
;;; CUSTOMIZATIONS
;;; **************

(defgroup emacs-setup-keys nil
  "Emacs setup layout customizations."
  :group 'emacs-setup)

(defcustom emacs-setup-keybindings nil
  "Alist where car is the function and cdr is the keybinding."
  :group 'emacs-setup-keys
  :type '(alist :key-type (string :tag "Function: ")
                :value-type (string :tag "Keybinding: ")))

;;; *********
;;; FUNCTIONS
;;; *********

(defun emacs-setup-bind-key-if-fboundp (key-cmd func)
  "Bind KEY-CMD to FUNC if FUNC is a bound function."
  (when (fboundp func)
   (global-set-key key-cmd func)))

(defun emacs-setup-bind-keys ()
  "Bind all keys set in `emacs-setup-keybindings'."
  (dolist (binding emacs-setup-keybindings)
    (emacs-setup-bind-key-if-fboundp
     (read-kbd-macro (cdr binding))
     (intern (car binding)))))

(defun emacs-setup-bind-key ()
  "Interactively bind a key to a function.
The binding is saved in `emacs-setup-keybindings'."
  (interactive)
  (let ((function (read-string "Function: "))
        (binding (read-key-sequence "Key binding: ")))
    (while (key-binding binding)
      (when (equal binding "")
        (keyboard-quit))
      (setq binding (read-key-sequence (concat binding " is already bound to " (symbol-name (key-binding binding)) ". Choose another key binding: "))))
    (if (equal binding "")
        (message "Cannot rebind C-g.")
      (set-variable
       'emacs-setup-keybindings
       (add-to-list 'emacs-setup-keybindings
                    (cons function (key-description binding))
                    t))
      (customize-save-variable 'emacs-setup-keybindings
                               emacs-setup-keybindings)
      (emacs-setup-bind-key-if-fboundp binding (intern function))
      (message "%s bound to %s" function (key-description binding)))))

(defun emacs-setup-unbind-key-by-key ()
  "Interactively unbind a key from `emacs-setup-keybindings'."
  (interactive)
  (let* ((binding (key-description (read-key-sequence "Key binding: ")))
         (function (key-binding (read-kbd-macro binding))))
    (if (equal binding "C-g")
        (message "Cannot unbind C-g.")
      (emacs-setup-unbind-key binding function))))

(defun emacs-setup-unbind-key-by-function ()
  "Interactively unbind a function from `emacs-setup-keybindings'."
  (interactive)
  (let (functions)
    (dolist (key-binding emacs-setup-keybindings)
      (add-to-list 'functions (car key-binding)))
    (let* ((function (completing-read "Function: " functions nil t))
           (binding (cdr (assoc function emacs-setup-keybindings))))
      (unless (equal function "keyboard-escape-quit")
        (emacs-setup-unbind-key binding (intern function))))))

(defun emacs-setup-unbind-key (binding function)
  "Unbind a key and remove from `emacs-setup-keybindings'.
Argument BINDING Key binding to unbind.
Argument FUNCTION Funciton to unbind."
  (let ((bindings emacs-setup-keybindings))
    (if (or (not function)
            (not (member (cons (symbol-name function) binding) bindings)))
        (message "No emacs-setup binding set for %s" binding)
      (setq bindings (delete (cons (symbol-name function) binding) bindings))
      (set-variable 'emacs-setup-keybindings bindings)
      (customize-save-variable
       'emacs-setup-keybindings
       emacs-setup-keybindings)
      (global-unset-key (read-kbd-macro binding))
      (message "Unbound %s from %s" function binding))))

(provide 'emacs-setup-keys)

;;; emacs-setup-keys.el ends here
