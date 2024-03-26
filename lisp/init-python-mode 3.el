;;; init-python-mode.el -- Things that need to be done first.
;;
;;; Commentary:
;;; ------------------------------------------------------------------------

;;; Code:


;;; See init-python-keybind.el for keybindings for python-mode.
;;;
(message "Initializing Python mode...")
(message "Install the following Python packages for the best experience:")
(message "     python-lsp-server[all]")
(message "     debnugpy")
(message "     singleton-decorator") ;; Needed for several projects

(use-package python-mode
   :ensure nil
   :config
   (eglot-ensure)
   (highlight-indentation-current-column-mode)
   (elpy-enable))

(use-package blacken) ;Format Python file upon save.

(provide 'init-python-mode)

;;; init-python-mode.el ends here.

