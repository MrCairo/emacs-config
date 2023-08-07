;;; init-lsp-mode.el -- Language Server Protocol.
;;
;;; Commentary:
;;; LSP Mode enables an IDE-like experience and functionality for many
;;; different programming languages.  LSP (Language Server Protocol) is
;;; a standard type protocol that many other packages can interface
;;; with.
;;; ------------------------------------------------------------------------

;;; Code:

(defun mrf/lsp-mode-setup ()
  "Set up LSP header-line."
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . mrf/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :config (setq lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-doc-delay 5
                lsp-ui-sideline-ignore-duplicates t
                lsp-ui-doc-position 'bottom
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-header nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-childframe t)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'bottom)
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :after lsp)


(provide 'init-lsp-mode)

;;; init-lsp-mode.el ends here.
