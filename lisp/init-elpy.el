;;; init-elpy.el -- Displays a list of key completions.
;;
;;; Commentary:
;;; 
;;; Elpy is an Emacs package to bring powerful Python editing to
;;; Emacs.  It combines and configures a number of other packages, both
;;; written in Emacs Lisp as well as Python.  Elpy is fully documented
;;; at: https://elpy.readthedocs.io/en/latest/index.html
;;; ------------------------------------------------------------------------

;;; Code:

(use-package elpy
   :ensure t
   :config
   (elpy-enable)
   (highlight-indentation-mode 0))

(provide 'init-elpy)

;;; init-elpy.el ends here.
