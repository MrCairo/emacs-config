;;; --------------------------------------------------------------------------

(when (equal debug-adapter 'enable-dap-mode)
    (use-package typescript-ts-mode
        ;; :after (dap-mode)
        :mode "\\.ts\\'"
        :hook
        (typescript-ts-mode . lsp-deferred)
        (js2-mode . lsp-deferred)
        (rust-mode . lsp-deferred)
        :bind (:map typescript-mode-map
                  ("C-c ." . dap-hydra/body))
        :config
        (setq typescript-indent-level 4)
        (dap-node-setup)))

(when (equal debug-adapter 'enable-dape)
    (use-package typescript-ts-mode
        :after (dape-mode)
        :mode ("\\.ts\\'")
        :hook
        (typescript-ts-mode . lsp-deferred)
        (js2-mode . lsp-deferred)
        (rust-mode . lsp-deferred)
        :bind (:map typescript-mode-map
                  ("C-c ." . dape-hydra/body))
        :config
        (setq typescript-indent-level 4)))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

;;; --------------------------------------------------------------------------

(defun mrf/load-js-file-hook ()
    (message "Running JS file hook")
    (js2-mode)
    
    (when (equal debug-adapter 'enable-dap-mode)
        (dap-mode)
        (dap-firefox-setup))
    
    (when (equal debug-adapter 'enable-dape)
        (dape))

    (highlight-indentation-mode nil)
    (dap-firefox-setup))

(use-package nodejs-repl)

(defun mrf/nvm-which ()
    (let ((output (shell-command-to-string "source ~/.nvm/nvm.sh; nvm which")))
        (cadr (split-string output "[\n]+" t))))

(setq nodejs-repl-command #'mrf/nvm-which)

;;; --------------------------------------------------------------------------

(use-package js2-mode
    :hook (js-mode . js2-minor-mode)
    :bind (:map js2-mode-map
              ("{" . paredit-open-curly)
              ("}" . paredit-close-curly-and-newline))
    :mode ("\\.js\\'" "\\.mjs\\'")
    :custom (js2-highlight-level 3))

(use-package ac-js2
    :hook (js2-mode . ac-js2-mode))

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(provide config-lang-js)
;;; config-js-langs.el ends here.
