;;; init-projectile.el -- Things that need to be done first.
;;
;;; Commentary:
;;; Projectile is a project management library for Emacs which makes it a lot
;;; easier to navigate around code projects for various languages.  Many
;;; packages integrate with Projectile so it's a good idea to have it
;;; installed even if you don't use its commands directly.
;;; ------------------------------------------------------------------------

;;; Code:

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Developer/src")
    (setq projectile-project-search-path '("~/Developer/src")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(provide 'init-projectile)

;;; init-projectile.el ends here.

