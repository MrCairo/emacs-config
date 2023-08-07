;;; init-typescript-mode.el -- Completion package.
;;
;;; Commentary:
;;; This is a basic configuration for the TypeScript language so that
;;; =.ts= files activate =typescript-mode= when opened.  We're also
;;; adding a hook to =typescript-mode-hook= to call =lsp-deferred= so
;;; that we activate =lsp-mode= to get LSP features every time we edit
;;; TypeScript code.
;;;
;;; *Important note!* For =lsp-mode= to work with TypeScript (and
;;; *JavaScript) you will need to install a language server on your
;;; *machine.  If you have Node.js installed, the easiest way to do
;;; *that is by running the following command:
;;;
;;; npm install -g typescript-language-server typescript
;;;
;;; ------------------------------------------------------------------------

;;; Code:

  (use-package typescript-mode
    :mode "\\.ts\\'"
    :hook (typescript-mode . lsp-deferred)
    :config
    (setq typescript-indent-level 2))

(provide 'init-typescript-mode)

;;; init-typescript-mode.el ends here.

