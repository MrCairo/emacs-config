;;; init-universal.el --- Universal Packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package system-packages :ensure t)

;;; ##########################################################################

(use-package jsonrpc
  :ensure t)
  ;; :config
  ;; For some odd reason, it is possible that jsonrpc will try to load a
  ;; theme. (jsonrpc/lisp/custom.el:1362). If our theme hasn't been loaded
  ;; yet, go ahead and try. This could prevent a startup without the theme
  ;; properly loaded.
  ;; (unless theme-did-load
  ;;   (mifi/load-theme-from-selector)))

;; All kept in local /lisp directory.
;; (use-package web-server-status-codes :ensure 1)
;; (use-package simple-httpd :ensure 1)
;; (use-package web-server :ensure 1)

;;; ##########################################################################

(use-package helpful
  :ensure t
  ;; :commands (helpful-callable helpful-variable helpful-command helpful-key helpful-function)
  :config
  (bind-keys
    ([remap describe-command] . helpful-command)
    ([remap describe-function] . helpful-function)
    ([remap describe-variable] . helpful-variable)
    ([remap describe-key] . helpful-key)))

;;; ##########################################################################

(defun mifi/setup-hooks-for-eldoc ()
  (interactive)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  ;; Eldoc will try to load/unload a theme which can cause issues with our
  ;; theme loading mechanism. Our theme could fail to load because of this.
  ;; So, to get our themes loading properly, load it here if not already
  ;; loaded.
  (unless theme-did-load
    (mifi/load-theme-from-selector)))

(use-package eldoc
  :ensure 1)

(use-package eldoc-box
  :ensure t
  :delight DocBox
  :hook (after-init . mifi/setup-hooks-for-eldoc))

;;; ##########################################################################

(use-package hydra
  :vc (:url "https://github.com/abo-abo/hydra" :ignored-files ("lv.el")))
  ;; :ensure (:repo "abo-abo/hydra" :fetcher github
  ;;           :files (:defaults (:exclude "lv.el"))))

;;; ##########################################################################

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
          ("C->" . mc/mark-next-like-this)
          ("C-<" . mc/mark-previous-like-this)
          ("C-c C-<" . mc/mark-all-like-this)))

;;; ##########################################################################

(use-package visual-fill-column
  :ensure 1
  :after org)

(use-package writeroom-mode
  :defer t
  :after visual-fill-column)

;;; ##########################################################################
;;; Default keys are C-M-= or C-M--

(use-package default-text-scale
  :ensure t
  :hook (after-init . default-text-scale-mode))

;;; ##########################################################################

(defun mifi/set-mac-modifier-keys ()
  (interactive)
  ;; Macintosh specific configurations.
  (when *is-a-mac*
    (setq mac-command-modifier   'meta
      mac-option-modifier        'super
      mac-control-modifier       'control
      mac-right-command-modifier 'meta
      mac-right-control-modifier 'hyper)))

(add-hook 'after-init-hook #'mifi/set-mac-modifier-keys)

;;; ##########################################################################

(defun mifi/setup-global-keybindings ()
  (interactive)
  (bind-key "C-c ]" 'indent-region prog-mode-map)
  (bind-key "C-c }" 'indent-region prog-mode-map) 
  (bind-key "C-x C-j" 'dired-jump)

  ;;
  ;; A little better than just the typical "C-x o"
  ;; windmove is a built-in Emacs package.
  ;;
  (global-set-key (kbd "C-c <left>")  'windmove-left)
  (global-set-key (kbd "C-c <right>") 'windmove-right)
  (global-set-key (kbd "C-c <up>")    'windmove-up)
  (global-set-key (kbd "C-c <down>")  'windmove-down)

  ;;
  ;; Ctl-mouse to adjust/scale fonts will be disabled.
  ;; I personally like this since it was all to easy to accidentally
  ;; change the size of the font.
  ;;
  (global-unset-key (kbd "C-<mouse-4>"))
  (global-unset-key (kbd "C-<mouse-5>"))
  (global-unset-key (kbd "C-<wheel-down>"))
  (global-unset-key (kbd "C-<wheel-up>")))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(add-hook 'emacs-startup-hook #'mifi/setup-global-keybindings)

;;; ##########################################################################
;;; Automatic Package Updates

(use-package auto-package-update
  ;; :ensure (:fetcher github :repo "rranelli/auto-package-update.el")
  :defer t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;;; ##########################################################################
;; YASnippets

(use-package yasnippet
  :bind (:map yas-minor-mode-map
          ("<C-'>" . yas-expand))
  :config
  (setq yas-global-mode t)
  (setq yas-minor-mode t)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (add-to-list #'yas-snippet-dirs (expand-file-name "Snippets" custom-docs-directory))
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)
  (setq yas-prompt-functions '(yas-ido-prompt))
  (defun help/yas-after-exit-snippet-hook-fn ()
    (prettify-symbols-mode))
  (add-hook 'yas-after-exit-snippet-hook #'help/yas-after-exit-snippet-hook-fn))

;;; ##########################################################################

(use-package yasnippet-snippets
  :after yasnippet)

;;; ##########################################################################

(use-package all-the-icons
  :ensure t)

;;; ##########################################################################

(use-package ace-window
  :vc (:url "https://github.com/abo-abo/ace-window") 
  ;;:ensure (:repo "abo-abo/ace-window" :fetcher github)
  :bind ("M-o" . ace-window))

;;; ##########################################################################
;;; Window Number

(use-package winum
  :ensure t
  :config (winum-mode))

;;; ##########################################################################

(use-package dashboard
  :custom
  (dashboard-items '( (recents   . 12)
                      (bookmarks . 5)
                      (projects  . 5)
                      (agenda    . 5)))
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)  
  (dashboard-footer-messages '("Greetings Program!"))
  (dashboard-banner-logo-title "Welcome to Emacs!")
  :commands dashboard-open
  :bind ("M-RET d" . dashboard-open)
  :config
  ;; (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (add-hook 'after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'after-init-hook #'dashboard-initialize)
  (when (equal custom-project-handler 'custom-project-projectile)
    (setq dashboard-projects-backend 'projectile))
  (setq dashboard-startup-banner (expand-file-name "Emacs-modern-is-sexy-v1.png" user-emacs-directory))
  (dashboard-setup-startup-hook))

;;; ##########################################################################

(use-package jinx
  :vc (:url "https://github.com/minad/jinx")
  ;; :ensure (:host github :repo "minad/jinx")
  ;;:hook (emacs-startup . global-jinx-mode)
  :bind (("C-c C-$" . jinx-correct)
          ("C-x C-$" . jinx-languages))
  :config
  (dolist (hook '(text-mode-hook prog-mode-hook org-mode-hook))
    (add-hook hook #'jinx-mode)))

;;; ##########################################################################
;; These are packages located in the site-lisp or lisp directories in the
;; 'emacs-config-directory'

(provide 'init-universal)
;;; init-universal.el ends here.
