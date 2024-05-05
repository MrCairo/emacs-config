;;; --------------------------------------------------------------------------

(use-package orderless
    :when (or (equal completion-handler 'comphand-vertico)
	  (equal completion-handler 'comphand-ivy-counsel))
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles basic partial-completion)))))

;;; --------------------------------------------------------------------------
;;; Swiper and IVY mode

(use-package ivy
    :when (equal completion-handler 'comphand-ivy-counsel)
    :bind (("C-s" . swiper)
              :map ivy-minibuffer-map
          ;;; ("TAB" . ivy-alt-done)
              ("C-l" . ivy-alt-done)
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)
              :map ivy-switch-buffer-map
              ("C-k" . ivy-previous-line)
              ("C-l" . ivy-done)
              ("C-d" . ivy-switch-buffer-kill)
              :map ivy-reverse-i-search-map
              ("C-k" . ivy-previous-line)
              ("C-d" . ivy-reverse-i-search-kill))
    :custom
    (enable-recursive-minibuffers t)
    (ivy-use-virtual-buffers t)
    :config
    (ivy-mode 1)
    (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
    (add-to-list 'ivy-highlight-functions-alist
	'(orderless-ivy-re-builder . orderless-ivy-highlight)))

;;; --------------------------------------------------------------------------

(use-package ivy-rich
    :when (equal completion-handler 'comphand-ivy-counsel)
    :after ivy
    :init
    (ivy-rich-mode 1)
    :config
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package ivy-yasnippet
    :when (equal completion-handler 'comphand-ivy-counsel)
    :after (:any yasnippet ivy)
    :defer t
    :ensure (:host github
                :repo "mkcms/ivy-yasnippet"))

;;; --------------------------------------------------------------------------

(use-package swiper
    :when (equal completion-handler 'comphand-ivy-counsel)
    :after ivy)

;;; --------------------------------------------------------------------------

(use-package counsel
    :when (equal completion-handler 'comphand-ivy-counsel)
    :after ivy
    :bind (   ("C-M-j" . 'counsel-switch-buffer)
              ("M-x" . 'counsel-M-x)
              ("C-x C-f" . 'counsel-find-file)
              ("C-c C-r" . 'ivy-resume)
              :map minibuffer-local-map
              ("C-r" . 'counsel-minibuffer-history))
    :custom
    (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
    :config
    (counsel-mode 1))

;;; --------------------------------------------------------------------------

(use-package ivy-prescient
    :when (equal completion-handler 'comphand-ivy-counsel)
    :after ivy
    :custom
    (prescient-persist-mode t)
    (ivy-prescient-mode t)
    (ivy-prescient-enable-filtering t))

;;; --------------------------------------------------------------------------

;;;; Code Completion
(use-package corfu
    :when enable-corfu
    ;; Optional customizations
    :custom
    (corfu-cycle t)                 ; Allows cycling through candidates
    (corfu-auto t)                  ; Enable auto completion
    (corfu-auto-prefix 2)
    (corfu-auto-delay 0.8)
    (corfu-popupinfo-delay '(0.5 . 0.2))
    (corfu-preview-current 'insert) ; insert previewed candidate
    (corfu-preselect 'prompt)
    (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
    ;; Optionally use TAB for cycling, default is `corfu-complete'.
    :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil))
    :init
    (global-corfu-mode)
    (corfu-history-mode)
    (corfu-popupinfo-mode) ; Popup completion info
    :config
	  (use-package corfu-prescient
        :after corfu))
    (add-hook 'eshell-mode-hook
        (lambda () (setq-local corfu-quit-at-boundary t
                       corfu-quit-no-match t
                       corfu-auto nil)
            (corfu-mode)))

;;; --------------------------------------------------------------------------

(use-package vertico
    :when (equal completion-handler 'comphand-vertico)
    :wait t
    :demand t   ; Otherwise won't get loaded immediately
    :ensure (:repo "minad/vertico" :files (:defaults "extensions/vertico-*.el") :fetcher github)
    :config
    (vertico-mode)
    (recentf-mode t)
    (vertico-multiform-mode)
    (vertico-count 13)
    (vertico-cycle nil)
    (use-package vertico-prescient
        :after vertico)
    (use-package vertico-posframe
	:after vertico
        :custom
        (vertico-posframe-parameters
            '((left-fringe . 8)
                 (right-fringe . 8))))
    ;; Clean up file path when typing
    :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)
	      ;; Make sure vertico state is saved
              (minibuffer-setup . vertico-repeat-save)))

;;; --------------------------------------------------------------------------

(use-package marginalia
    :when (equal completion-handler 'comphand-vertico)
    :custom
    (marginalia-max-relative-age 0)
    (marginalia-align 'right)
    :config
    (marginalia-mode t))

;;; --------------------------------------------------------------------------
;; Example configuration for Consult

(use-package consult
    :when (equal completion-handler 'comphand-vertico)
    ;; Replace bindings. Lazily loaded due by `use-package'.
    :bind (;; C-c bindings in `mode-specific-map'
              ("C-c M-x" . consult-mode-command)
              ("C-c h" . consult-history)
              ("C-c k" . consult-kmacro)
              ("C-c m" . consult-man)
              ("C-c i" . consult-info)
              ([remap Info-search] . consult-info)
              ;; C-x bindings in `ctl-x-map'
              ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
              ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
              ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
              ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
              ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
              ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
              ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
              ;; Custom M-# bindings for fast register access
              ("M-#" . consult-register-load)
              ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
              ("C-M-#" . consult-register)
              ;; Other custom bindings
              ("M-y" . consult-yank-pop)                ;; orig. yank-pop
              ;; M-g bindings in `goto-map'
              ("M-g e" . consult-compile-error)
              ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
              ("M-g g" . consult-goto-line)             ;; orig. goto-line
              ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
              ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
              ("M-g m" . consult-mark)
              ("M-g k" . consult-global-mark)
              ("M-g i" . consult-imenu)
              ("M-g I" . consult-imenu-multi)
              ;; M-s bindings in `search-map'
              ("M-s d" . consult-find)                  ;; Alternative: consult-fd
              ("M-s c" . consult-locate)
              ("M-s g" . consult-grep)
              ("M-s G" . consult-git-grep)
              ("M-s r" . consult-ripgrep)
              ("M-s l" . consult-line)
              ("M-s L" . consult-line-multi)
              ("M-s k" . consult-keep-lines)
              ("M-s u" . consult-focus-lines)
              ;; Isearch integration
              ("M-s e" . consult-isearch-history)
              :map isearch-mode-map
              ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
              ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
              ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
              ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
              ;; Minibuffer history
              :map minibuffer-local-map
              ("M-s" . consult-history)                 ;; orig. next-matching-history-element
              ("M-r" . consult-history))                ;; orig. previous-matching-history-element

    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key "M-.")
    ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
        consult-theme :preview-key '(:debounce 0.2 any)
        consult-ripgrep consult-git-grep consult-grep
        consult-bookmark consult-recent-file consult-xref
        consult--source-bookmark consult--source-file-register
        consult--source-recent-file consult--source-project-recent-file
        ;; :preview-key "M-."
        :preview-key '(:debounce 0.4 any))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<")) ;; "C-+"

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; By default `consult-project-function' uses `project-root' from project.el.
    ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
    ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
    ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
    ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
    ;; (autoload 'projectile-project-root "projectile")
    ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
    ;; (setq consult-project-function nil)

(elpaca-process-queues)
(provide 'config-completion)
;;; config-completion.el ends here.
