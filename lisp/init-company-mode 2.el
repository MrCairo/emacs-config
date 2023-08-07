;;; init-company-mode.el -- Things that need to be done first.
;;
;;; Commentary:
;;; Company Mode provides a nicer in-buffer completion interface than
;;; =completion-at-point= which is more reminiscent of what you would expect
;;; from an IDE.  We add a simple configuration to make the keybindings a
;;; little more useful (=TAB= now completes the selection and initiates
;;; completion at the current location if needed).
;;;
;;; We also use company-box to further enhance the look of the completions
;;; with icons and better overall presentation.
;;; ------------------------------------------------------------------------

;;; Code:

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-jedi
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook))

(provide 'init-company-mode)

;;; init-company-mode.el ends here.

