;;; init-impatient-mode.el -- Show the effects of HTML as you type.

;;
;;; Commentary:
;;; Impatient mode is a mode that shows the effects of your HTML as
;;; you type.
;;; ------------------------------------------------------------------------

;;; Code:


(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-right)
  (setq which-key-idle-delay 0.2))

(provide 'init-impatient-mode)

;;; init-impatient-mode.el ends here.

