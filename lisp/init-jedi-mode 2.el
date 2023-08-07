;;; init-jedi-mode.el -- Displays a list of key completions.
;;
;;; Commentary:
;;; 
;;; ------------------------------------------------------------------------

;;; Code:


(use-package company-jedi
   :hook (python-mode . (add-to-list 'company-backends 'company-jedi))

(provide 'init-jedi-mode)

;;; init-jedi-mode.el ends here.
