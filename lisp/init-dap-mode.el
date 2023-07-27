;;; init-dap-mode.el -- Debug Adapter Protocol
;;
;;; Commentary:
;;; Provides a common protocol for debugging different systems.
;;; ------------------------------------------------------------------------

;;; Code:

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  :config
   (dap-ui-mode 1)
   (require 'dap-python)
  :commands dap-debug
  :custom (dap-auto-configure-features '(sessions locals controls tooltip))
  )

(setq dap-python-debugger 'debugpy)

(provide 'init-dap-mode)

;;; init-dap-mode.el ends here.
