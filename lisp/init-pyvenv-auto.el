;;; init-pyvenv-auto.el -- Displays a list of key completions.
;;
;;; Commentary:
;;; 
;;; We use Pytvenv-auto is a package that automatically changes to the
;;; Python virtual environment based upon the project's
;;; directory.  pyvenv-auto looks at the root director of the project
;;; for a *.venv* or *venv* (and a few others)

;;; ------------------------------------------------------------------------

;;; Code:

(use-package pyvenv-auto
   :ensure t
   :init (message "Starting pyvenv-auto")
   :hook ((python-mode . pyvenv-auto-run)))

(provide 'init-pyvenv-auto)

;;; init-pyvenv-auto.el ends here.
