;;; emacs-setup-util.el --- Utility functions for the emacs-setup package.

;;; Commentary:
;; This file contains utility functions for emacs-setup.

;;; Code:

(defun emacs-setup-thing-exists (thing)
  "Return true if THING exists, nil otherwise."
  (and (boundp thing)
       (not (eq (eval thing) nil))))

(defun emacs-setup-custom-save (variable value)
  "Saves the VALUE into VARIABLE in customize and sets the value for the current
running emacs."
  (set-variable variable value)
  (customize-save-variable variable (eval variable)))

(provide 'emacs-setup-util)

;;; emacs-setup-util.el ends here
