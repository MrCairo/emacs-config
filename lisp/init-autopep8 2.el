;;; init-autopep8.el -- Things that need to be done first.
;;
;;; Commentary:
;;; autopep8 automatically formats Python code to conform to the `PEP 8` style
;;; guide.  It uses the pycodestyle_ utility to determine what parts of the code
;;; needs to be formatted.  autopep8 is capable of fixing most of the formatting
;;; issues_ that can be reported by pycodestyle.
;;; ------------------------------------------------------------------------

;;; Code:

(use-package py-autopep8
   :ensure t
   :config
   (add-hook 'python-mode-hook 'py-autopep8-mode))

;; (use-package py-autopep8
;;   :ensure t
;;   :config
;;   (add-hook elpy-mode-hook 'py-autopep8-mode))


(provide 'init-autopep8)

;;; init-autopep8.el ends here.

