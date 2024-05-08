;;; --------------------------------------------------------------------------

(setq-default
    window-resize-pixelwise t ;; enable smooth resizing
    window-resize-pixelwise t
    frame-resize-pixelwise t
    dired-dwim-target t       ;; try to guess target directory
    truncate-partial-width-windows 1 ;; truncate lines in partial-width windows
    backup-inhibited t        ;; disable backup (No ~ tilde files)
    auto-save-default nil     ;; disable auto save
    global-auto-revert-mode 1 ;; Refresh buffer if file has changed
    global-auto-revert-non-file-buffers t
    history-length 25         ;; Reasonable buffer length
    inhibit-startup-message t ;; Hide the startup message
    inhibit-startup-screent t
    lisp-indent-offset '4     ;; emacs lisp tab size
    visible-bell t            ;; Set up the visible bell
    truncate-lines 1          ;; long lines of text do not wrap
    fill-column 80            ;; Default line limit for fills
    ;; Triggers project for directories with any of the following files:
    project-vc-extra-root-markers '(".dir-locals.el"
                                       "requirements.txt"
                                       "Gemfile"
                                       "package.json")
    )

;; (global-display-line-numbers-mode 1) ;; Line numbers appear everywhere
(save-place-mode 1)                  ;; Remember where we were last editing a file.
(savehist-mode t)
(show-paren-mode 1)
(tool-bar-mode -1)                   ;; Hide the toolbar
(global-prettify-symbols-mode 1)     ;; Display pretty symbols (i.e. λ = lambda)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Allow access from emacsclient
(add-hook 'after-init-hook
    (lambda ()
        (require 'server)
        (unless (server-running-p)
            (server-start))))

(when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode))

(use-package default-text-scale
    :hook (after-init . default-text-scale-mode))

;;; --------------------------------------------------------------------------

  
(use-package diminish
    :wait t
    :preface
    (defun mrf/set-diminish ()
	(diminish 'projectile-mode "PrM")
	(diminish 'anaconda-mode)
	(diminish 'tree-sitter-mode "ts")
	(diminish 'ts-fold-mode)
	(diminish 'counsel-mode)
	(diminish 'golden-ratio-mode)
	(diminish 'company-box-mode)
	(diminish 'company-mode))
    :ensure (:host github :repo "myrjola/diminish.el")
    :hook (after-init . mrf/set-diminish))
;; (use-package pabbrev)

(elpaca-process-queues)

;;; --------------------------------------------------------------------------
;; Which Key Helper

(use-package which-key
    :diminish which-key-mode
    :custom (which-key-idle-delay 1)
    :config
    (which-key-mode)
    (which-key-setup-side-window-right))

;;; --------------------------------------------------------------------------

(use-package multiple-cursors
    :bind (("C-S-c C-S-c" . mc/edit-lines)
		("C->" . mc/mark-next-like-this)
		("C-<" . mc/mark-previous-like-this)
	      ("C-c C-<" . mc/mark-all-like-this)))

;;; --------------------------------------------------------------------------

(use-package anzu
    :custom
    (anzu-mode-lighter "")                    
    (anzu-deactivate-region t)                
    (anzu-search-threshold 1000)              
    (anzu-replace-threshold 50)               
    (anzu-replace-to-string-separator " => ")
    :config
    (global-anzu-mode +1)
    (set-face-attribute 'anzu-mode-line nil
        :foreground "yellow" :weight 'bold)
    (define-key isearch-mode-map
        [remap isearch-query-replace]  #'anzu-isearch-query-replace)
    (define-key isearch-mode-map
        [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))

;;; --------------------------------------------------------------------------

(column-number-mode)

(use-package page-break-lines
    :config
    (global-page-break-lines-mode))

(use-package rainbow-delimiters
    :config
    (rainbow-delimiters-mode))

(use-package dash
    :disabled
    :ensure (:files ("dash.el" "dash.texi" "dash-pkg.el")
             :host github
             :repo "magnars/dash.el"))

(defun mrf/set-fill-column-interactively (num)
    "Asks for the fill column."
    (interactive "nfill-column: ")
    (set-fill-column num))

(defun mrf/set-org-fill-column-interactively (num)
    "Asks for the fill column for Org mode."
    (interactive "norg-fill-column: ")
    (setq custom-org-fill-column num)
    (mrf/org-mode-visual-fill)
    (redraw-display))

;;; --------------------------------------------------------------------------

;; Macintosh specific configurations.

(defconst *is-a-mac* (eq system-type 'darwin))
(when (eq system-type 'darwin)
    (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'super))

;;; --------------------------------------------------------------------------

(bind-key "C-c ]" 'indent-region prog-mode-map)
(bind-key "C-c }" 'indent-region prog-mode-map)
(bind-key "C-x C-j" 'dired-jump)

(use-package evil-nerd-commenter
    :bind ("M-/" . evilnc-comment-or-uncomment-lines))

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
(global-unset-key (kbd "C-<wheel-up>"))

;;; --------------------------------------------------------------------------

(use-package hydra
  :wait t
  :ensure (:repo "abo-abo/hydra" :fetcher github
		:files (:defaults (:exclude "lv.el"))))

;;; --------------------------------------------------------------------------

(use-package eldoc
    :wait t
    :config
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
    (add-hook 'ielm-mode-hook 'eldoc-mode))

(use-package eldoc-box
    :after eldoc
    :diminish DocBox
    :config
    (global-eldoc-mode t))

;;; --------------------------------------------------------------------------
;;; Automatic Package Updates

(use-package auto-package-update
    :ensure (:fetcher github :repo "rranelli/auto-package-update.el")
    :custom
    (auto-package-update-interval 7)
    (auto-package-update-prompt-before-update t)
    (auto-package-update-hide-results t)
    :config
    (auto-package-update-maybe)
    (auto-package-update-at-time "09:00"))

;;; --------------------------------------------------------------------------
;; YASnippets

(use-package yasnippet
    :bind (:map yas-minor-mode-map
              ("<C-'>" . yas-expand))
    :config
    (setq yas-global-mode t)
    (setq yas-minor-mode t)
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (add-to-list #'yas-snippet-dirs (expand-file-name "Snippets" custom-docs-dir))
    (yas-reload-all)
    (setq yas-prompt-functions '(yas-ido-prompt))
    (defun help/yas-after-exit-snippet-hook-fn ()
        (prettify-symbols-mode))
    (add-hook 'yas-after-exit-snippet-hook #'help/yas-after-exit-snippet-hook-fn)
    (message ">>> YASnippet Configured"))

;;; --------------------------------------------------------------------------

(use-package yasnippet-snippets
    :after yasnippet
    :config
    (message ">>> YASnippet-Snippets Configured"))

(elpaca-process-queues)

(provide 'config-global)
;;; config-global.el ends here.
