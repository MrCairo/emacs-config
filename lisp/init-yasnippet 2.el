;;; init-yasnippet.el -- Snippet handling system.
;;
;;; Commentary:
;;; ------------------------------------------------------------------------

;;; Code:

(use-package yasnippet
  :defer t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode t)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "C-'") #'yas-expand)
  (add-to-list #'yas-snippet-dirs "~/Documents/Emacs-Related/Snippets")
  (yas-reload-all)
  (setq yas-prompt-functions '(yas-maybe-ido-prompt))
  (defun help/yas-after-exit-snippet-hook-fn ()
    (prettify-symbols-mode)
    (prettify-symbols-mode))
  (add-hook 'yas-after-exit-snippet-hook #'help/yas-after-exit-snippet-hook-fn)
  :diminish yas-minor-mode)

(add-to-list 'load-path "~/Documents/Emacs-Related/Snippets")


(provide 'init-yasnippet)

;;; init-yasnippet.el ends here.

