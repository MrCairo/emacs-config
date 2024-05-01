(use-package slime
    :mode ("\\.lisp\\'" . slime-mode)
    :config
    (setq inferior-lisp-program "/opt/homebrew/bin/sbcl"))

;;; --------------------------------------------------------------------------

;; (use-package graphql-mode)
(use-package js2-mode :defer t)
(use-package rust-mode :defer t)
(use-package swift-mode :defer t)

(provide 'config-lang-other)
;;; config-lang-other.el ends here.
