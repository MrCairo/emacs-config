;;; --------------------------------------------------------------------------

(use-package slime
    :mode ("\\.lisp\\'" . slime-mode)
    :config
    (setq inferior-lisp-program "/opt/homebrew/bin/sbcl"))

;;; --------------------------------------------------------------------------

(use-package swift-mode
    :defer t)

(use-package swift-helpful
    :straight (swift-helpful :type git
		  :flavor melpa
		  :files ("*.el" "swift-info/*.info"
			     ("images" "swift-info/images/*.png") "swift-helpful-pkg.el")
		  :host github
		  :repo "danielmartin/swift-helpful")
    :defer t)

(use-package swift-playground-mode :ensure t :defer t :init
    (autoload 'swift-playground-global-mode "swift-playground-mode" nil t)
    (add-hook 'swift-mode-hook #'swift-playground-global-mode))

;;; --------------------------------------------------------------------------

;; (use-package graphql-mode)
(use-package js2-mode :defer t)
(use-package rust-mode
    :defer t
    :init (setq rust-mode-treesitter-derive t)
    :hook (rust-mode . (lambda ()
			   (setq indent-tabs-mode nil)
			   (prettify-symbols-mode)))
    :config
    (setq rust-format-on-save t))

;;; --------------------------------------------------------------------------

(use-package go-mode
    :defer t
    :mode ("\\.go\\'" . go-mode)
    :hook (go-mode . lsp-deferred))

(use-package go-eldoc
    :after go-mode
    :hook (go-mode . go-eldoc-setup)
    :config
    (set-face-attribute 'eldoc-highlight-function-argument nil
        :underline t :foreground "green"
        :weight 'bold))

(provide 'config-lang-other)
;;; config-lang-other.el ends here.
