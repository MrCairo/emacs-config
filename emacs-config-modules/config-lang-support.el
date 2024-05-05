;;; --------------------------------------------------------------------------

(use-package flycheck
    :unless (equal custom-ide 'custom-ide-elpy)
    :diminish FlM
    :ensure (:host github :repo "flycheck/flycheck")
    :config
    (eval-after-load 'flycheck
	'(flycheck-package-setup))
    (global-flycheck-mode))

(use-package flycheck-package
    :after flycheck)

;;; --------------------------------------------------------------------------

(defun mrf/tree-sitter-setup ()
    (tree-sitter-hl-mode t)
    (ts-fold-mode t))

(use-package tree-sitter
    :init
    (message ">>> Loading tree-sitter")
    :after prog-mode
    :config
    ;; Activate tree-sitter globally (minor mode registered on every buffer)
    (global-tree-sitter-mode)
    :hook
    (tree-sitter-after-on . mrf/tree-sitter-setup)
    (typescript-mode . lsp-deferred)
    (c-mode . lsp-deferred)
    (c++-mode . lsp-deferred)
    (rust-mode . lsp-deferred)
    (js2-mode . lsp-deferred))

(use-package tree-sitter-langs
    :after tree-sitter)

(use-package ts-fold
    :after tree-sitter
    :ensure (:host github :repo "emacs-tree-sitter/ts-fold")
    :bind (("C-<tab>" . ts-fold-toggle)
              ("C-c f"   . ts-fold-open-all)))

;;; --------------------------------------------------------------------------

(use-package magit
    :after (:any python-mode c-mode c++-mode rust-mode)
    :commands (magit-status magit-get-current-branch)
    ;; :custom
    ;;  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    )

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started

(use-package forge :after magit)

;;; --------------------------------------------------------------------------

(use-package treemacs-magit
    :after treemacs magit)


(elpaca-process-queues)

(provide 'config-lang-support)
;;; config-lang-support.el ends here.
