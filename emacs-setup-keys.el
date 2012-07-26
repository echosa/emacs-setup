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

(defun emacs-setup-bind-keys ()
  "Bind all keys set in `emacs-setup-keybindings'."
  (dolist (binding emacs-setup-keybindings)
    (emacs-setup-bind-key
     (intern (car binding))
     (read-kbd-macro (cdr binding))
     t)))

(defun emacs-setup-bind-key (function binding &optional allow-override-p)
  "Interactively bind a key to a function.
The binding is saved in `emacs-setup-keybindings'."
  (interactive "aFunction: \nkKey binding: \nP")
  (when (or (equal binding "")
            (equal function "keyboard-escape-quit"))
    (keyboard-quit))
  (while (and (not allow-override-p) (key-binding binding))
    (setq binding (read-key-sequence (concat
                                      (key-description binding) 
                                      " is already bound to "
                                      (symbol-name (key-binding binding))
                                      ". Choose another key binding: "))))
  (when (fboundp function)
    (set-variable
     'emacs-setup-keybindings
     (remove (rassoc (key-description binding) emacs-setup-keybindings)
             emacs-setup-keybindings))
    (emacs-setup-custom-save
     'emacs-setup-keybindings
     (add-to-list 'emacs-setup-keybindings
                  (cons (symbol-name function) (key-description binding))
                  t))
    (global-set-key binding function)
    (message "%s bound to %s" function (key-description binding))))

(defun emacs-setup-unbind-key-by-key ()
  "Interactively unbind a key from `emacs-setup-keybindings'."
  (interactive)
  (let ((binding (read-key-sequence "Key binding: ")))
    (unless (equal binding "")
      (emacs-setup-unbind-key :binding binding))))

(defun emacs-setup-unbind-key-by-function ()
  "Interactively unbind a function from `emacs-setup-keybindings'."
  (interactive)
  (let ((function (completing-read "Function: "
                                   (mapcar 'car emacs-setup-keybindings)
                                   nil t)))
    (unless (equal function "keyboard-escape-quit")
      (emacs-setup-unbind-key :function function))))

(defun* emacs-setup-unbind-key (&key binding function)
  "Unbind a key and remove from `emacs-setup-keybindings'.
Argument BINDING Key binding to unbind.
Argument FUNCTION Funciton to unbind."
  (let ((bind-cons
         (if binding
             (rassoc (key-description binding) emacs-setup-keybindings)
           (when function
             (assoc function emacs-setup-keybindings)))))
    (when bind-cons
      (global-unset-key (read-kbd-macro (cdr bind-cons)))
      (emacs-setup-custom-save
       'emacs-setup-keybindings
       (remove bind-cons emacs-setup-keybindings))
      (message "Unbound %s from %s" (car bind-cons) (cdr bind-cons)))))
                       
(provide 'emacs-setup-keys)

;;; emacs-setup-keys.el ends here
