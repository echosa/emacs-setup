;;; emacs-setup-util.el --- Utility functions for the emacs-setup package.

;;; Commentary:
;; This file contains utility functions for emacs-setup.

;;; Code:

(defun emacs-setup-thing-exists (thing)
  "Return true if THING exists, nil otherwise."
  (and (boundp thing)
       (not (eq (eval thing) nil))))

(provide 'emacs-setup-util)

;;; emacs-setup-util.el ends here
