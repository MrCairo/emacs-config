;;; --------------------------------------------------------------------------

(when (equal custom-ide 'custom-ide-eglot-lsp)
    (use-package company
        :after lsp-mode
        :hook (lsp-mode . company-mode)
        :bind (:map company-active-map
                  ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
            ("<tab>" . company-indent-or-complete-common))))

(when (equal custom-ide 'custom-ide-elpy)
    (use-package company
        :after elpy
        :hook (elpy-mode . company-mode)
        :bind (:map company-active-map
                  ("<tab>" . company-complete-selection))
        (:map elpy-mode-map
            ("<tab>" . company-indent-or-complete-common))))

(when (equal custom-ide 'custom-ide-anaconda)
    (use-package company
        :after anaconda-mode
        :hook (anaconda-mode . company-mode)
        :bind (:map company-active-map
                  ("<tab>" . company-complete-selection))
        (:map elpy-mode-map
            ("<tab>" . company-indent-or-complete-common))))

;; Don't use company at all if lsp-bridge is active.
;; lsp-bridge already provides similar functionality.
(unless (equal custom-ide 'custom-ide-lsp-bridge)
    (use-package company
        :custom
        (company-minimum-prefix-length 1)
        (company-idle-delay 0.0)
        :hook (after-init . global-company-mode)))
;; :config
;; (add-to-list 'company-backends 'company-yasnippet))

;;; --------------------------------------------------------------------------

(when (featurep 'company)
    (use-package company-box
        :diminish cb
        :hook (company-mode . company-box-mode))

    (when (equal custom-ide 'custom-ide-elpy)
        (use-package company-jedi
            :after python
            :config
            (jedi:setup)
            (defun my/company-jedi-python-mode-hook ()
                (add-to-list 'company-backends 'company-jedi))
            (add-hook 'python-mode-hook 'my/company-jedi-python-mode-hook)))

    (when (equal custom-ide 'custom-ide-anaconda)
        (use-package company-anaconda
            :after anaconda
            :hook (python-mode . anaconda-mode))
        (eval-after-load "company"
            '(add-to-list 'company-backends 'company-anaconda))))

(provide 'config-company)
;;; config-company.el ends here.
