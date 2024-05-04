;;; --------------------------------------------------------------------------
;; helpful package

(if (equal completion-handler 'comphand-ivy-counsel)
    (use-package helpful
        :commands (helpful-callable helpful-variable helpful-command helpful-key)
        :custom
        (counsel-describe-function-function #'helpful-callable)
        (counsel-describe-variable-function #'helpful-variable)
        :bind
        ([remap describe-function] . counsel-describe-function)
        ([remap describe-command] . helpful-command)
        ([remap describe-variable] . counsel-describe-variable)
        ([remap describe-key] . helpful-key))
    (use-package helpful
        :commands (helpful-callable helpful-variable helpful-command helpful-key)
        :bind
        ([remap describe-command] . helpful-command)
        ([remap describe-key] . helpful-key)))

;;; --------------------------------------------------------------------------

(use-package solaire-mode
    :straight (solaire-mode :type git :flavor melpa
                  :host github :repo "hlissner/emacs-solaire-mode")
    :hook (after-init . solaire-global-mode)
    :config
    (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
    (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist))

;;; --------------------------------------------------------------------------
;; Golen Ratio

(if enable-golden-ratio
    (use-package golden-ratio
        :custom
        (golden-ratio-auto-scale t)
        (golden-ratio-adjust-factor .4)
        (golden-ratio-wide-adjust-factor .4)
        (golden-ratio-max-width 100)
        (golden-ratio-exclude-modes '(treemacs-mode
                                         undo-tree-visdualizer-mode
                                         inferior-python-mode
                                         vundo-mode
                                         which-key-mode
                                         c-mode
                                         cc-mode
                                         dashboard-mode
                                         python-mode
                                         markdown-mode))
        (golden-ratio-exclude-buffer-regexp '("dap*"
                                                 "*dape*"
                                                 "*python*"))
        :config
        (golden-ratio-mode 1)))

;;; --------------------------------------------------------------------------

(use-package ace-window
    :bind ("M-o" . ace-window))

;;; --------------------------------------------------------------------------

(when enable-neotree
    (use-package neotree
        :config
        (global-set-key [f8] 'neotree-toggle)
        (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))

;;; --------------------------------------------------------------------------

(use-package all-the-icons)
(use-package nerd-icons)

;; (use-package doom-modeline
;;   :diabled
;;   :init (doom-modeline-mode 1)
;;   :custom ((doom-modeline-height 15)))

;;; --------------------------------------------------------------------------
;; Enable tabs for each buffer

(when enable-centaur-tabs
    (use-package centaur-tabs
        :custom
        ;; Set the style to rounded with icons (setq centaur-tabs-style "bar")
        (centaur-tabs-style "bar")
        (centaur-tabs-set-icons t)
        (centaur-tabs-set-modified-marker t)
        :bind (("C-c <" . centaur-tabs-backward)
                  ("C-c >" . centaur-tabs-forward))
        :config ;; Enable centaur-tabs
        (centaur-tabs-mode t)))

;;; --------------------------------------------------------------------------

(use-package diff-hl
    :config
    (global-diff-hl-mode))

;;; --------------------------------------------------------------------------

(use-package pulsar
    :config
    (pulsar-global-mode)
    :custom
    (pulsar-pulse t)
    (pulsar-delay 0.10)
    (pulsar-iterations 10)
    (pulsar-face 'pulsar-magenta)
    (pulsar-highlight-face 'pulsar-yellow))

;;; --------------------------------------------------------------------------

(use-package popper
    :defer t
    :bind (("C-`"   . popper-toggle)
              ("M-`"   . popper-cycle)
              ("C-M-`" . popper-toggle-type))
    :init
    (setq popper-reference-buffers
        '("\\*Messages\\*"
             "\\*scratch\\*"
             "\\*ielm\\*"
             "Output\\*$"
             "\\*Async Shell Command\\*"
             "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
             "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
             "^\\*term.*\\*$"   term-mode   ;term as a popup
             "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
             help-mode
             compilation-mode))
    (popper-mode +1)
    (popper-echo-mode +1))

(provide 'config-qol)
;;; config-qol.el ends here.
