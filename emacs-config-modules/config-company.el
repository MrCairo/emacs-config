;;; --------------------------------------------------------------------------
(use-package company-box
    :after company
    :diminish cb
    :hook (company-mode . company-box-mode))

(use-package company-jedi
    :when  (equal custom-ide 'custom-ide-elpy)
    :after python company
    :config
    (jedi:setup)
    (defun my/company-jedi-python-mode-hook ()
        (add-to-list 'company-backends 'company-jedi))
    (add-hook 'python-mode-hook 'my/company-jedi-python-mode-hook))

(use-package company-anaconda
    :when (equal custom-ide 'custom-ide-anaconda)
    :after anaconda
    :hook (python-mode . anaconda-mode)
    :config
    (eval-after-load "company"
      '(add-to-list 'company-backends 'company-anaconda)))
