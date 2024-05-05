;;; --------------------------------------------------------------------------
(when (equal custom-ide 'custom-ide-eglot-lsp)
    (use-package company
	:after lsp-mode
	:hook (after-init . global-company-mode)
	:custom
	(company-minimum-prefix-length 1)
	(company-idle-delay 0.0)
	:bind (:map company-active-map
		  ("<tab>" . company-complete-selection))
	(:map lsp-mode-map
            ("<tab>" . company-indent-or-complete-common))))

(when (equal custom-ide 'custom-ide-elpy)
    (use-package company
	:after elpy
	:hook (after-init . global-company-mode)
	:custom
	(company-minimum-prefix-length 1)
	(company-idle-delay 0.0)
	:bind (:map company-active-map
		  ("<tab>" . company-complete-selection))
	(:map elpy-mode-map
            ("<tab>" . company-indent-or-complete-common))))

(when (equal custom-ide 'custom-ide-anaconda)
    (use-package company
	:after anaconda-mode
	:hook (after-init . global-company-mode)
	:custom
	(company-minimum-prefix-length 1)
	(company-idle-delay 0.0)
	:bind (:map company-active-map
		  ("<tab>" . company-complete-selection))
	(:map elpy-mode-map
            ("<tab>" . company-indent-or-complete-common))))

(elpaca-process-queues)
;; IMPORTANT:
;; Don't use company at all if lsp-bridge is active.
;; lsp-bridge already provides similar functionality.

;; :config
;; (add-to-list 'company-backends 'company-yasnippet))

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

(provide 'config-company)
;;; config-company.el ends here.
