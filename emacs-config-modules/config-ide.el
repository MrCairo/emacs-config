;;; --------------------------------------------------------------------------
;;; Emacs Polyglot is the Emacs LSP client that stays out of your way:

(defvar mrf/clangd-path (executable-find "clangd")
    "Clangd executable path.")

(defun mrf/projectile-proj-find-function (dir)
    "Find the project `DIR' function for Projectile.
Thanks @wyuenho on GitHub"
    (let ((root (projectile-project-root dir)))
        (and root (cons 'transient root))))

(when (equal custom-ide 'custom-ide-eglot-lsp)
    ;; Open python files in tree-sitter mode.
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
    (use-package eglot
        :defer t
        :after company
        :init
        (setq company-backends
            (cons 'company-capf
                (remove 'company-capf company-backends)))
        :hook
        (lisp-mode . eglot-ensure)
        (c-mode . eglot-ensure)
        (c++-mode . eglot-ensure)
        (python-mode . eglot-ensure)
        ;; (prog-mode . eglot-ensure)
        (rust-mode-hook . eglot-ensure)
        :config
        (which-key-add-key-based-replacements "C-c g r" "find-symbol-reference")
        (which-key-add-key-based-replacements "C-c g o" "find-defitions-other-window")
        (which-key-add-key-based-replacements "C-c g g" "find-defitions")
        (which-key-add-key-based-replacements "C-c g ?" "eldoc-definition")
        ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)
	(add-to-list 'eglot-stay-out-of 'flymake)
        (add-to-list 'eglot-server-programs '((c-mode c++-mode) "clangd"))
        (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
        (add-to-list 'eglot-server-programs
            '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
	(setq-default eglot-workspace-configuration
            '((:pylsp . (:configurationSources ["flake8"]
			    :plugins (:pycodestyle (:enabled nil)
					 :mccabe (:enabled nil)
					 :flake8 (:enabled t))))))
        ))

;;; --------------------------------------------------------------------------
;;; Language Server Protocol

(when (equal custom-ide 'custom-ide-eglot-lsp)
    (eval-when-compile (defvar lsp-enable-which-key-integration)))

(defun mrf/lsp-mode-setup ()
    "Custom LSP setup function."
    (when (equal custom-ide 'custom-ide-eglot-lsp)
        (message "Set up LSP header-line and other vars")
        (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
        (setq lsp-clangd-binary-path "/usr/bin/clangd")'
        (lsp-headerline-breadcrumb-mode)))

(when (equal custom-ide 'custom-ide-eglot-lsp)
    (use-package lsp-mode
        :defer t
        :commands (lsp lsp-deferred)
        :hook (lsp-mode . mrf/lsp-mode-setup)
        :init
        (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
        :config
        (lsp-enable-which-key-integration t)))

(when (equal custom-ide 'custom-ide-eglot-lsp)
    (use-package lsp-ui
        :after lsp
        :config (setq lsp-ui-sideline-enable t
                    lsp-ui-sideline-show-hover t
                    lsp-ui-sideline-delay 0.5
                    lsp-ui-sideline-ignore-duplicates t
                    lsp-ui-doc-delay 3
                    lsp-ui-doc-position 'top
                    lsp-ui-doc-alignment 'frame
                    lsp-ui-doc-header nil
                    lsp-ui-doc-show-with-cursor t
                    lsp-ui-doc-include-signature t
                    lsp-ui-doc-use-childframe t)
        :commands lsp-ui-mode
        :bind (:map lsp-ui-mode-map
                  ("C-c l d" . lsp-ui-doc-focus-frame))
        :custom
        (lsp-ui-doc-position 'bottom)
        :hook (lsp-mode . lsp-ui-mode)))

(when (equal custom-ide 'custom-ide-eglot-lsp)
    (use-package lsp-treemacs
        :after lsp
        :bind (:map prog-mode-map
                  ("C-c t" . treemacs))
        :config
        (lsp-treemacs-sync-mode 1)))

(when (and (equal custom-ide 'custom-ide-eglot-lsp)
          (equal completion-handler 'comphand-ivy-counsel))
    (use-package lsp-ivy
        :after lsp ivy))

;;; --------------------------------------------------------------------------

(when (equal custom-ide 'custom-ide-lsp-bridge)
    (use-package markdown-mode)
    (use-package lsp-bridge
        :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                       :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                       :build (:not compile))
        :custom
        (lsp-bridge-python-lsp-server "pylsp")
        :config
        (global-lsp-bridge-mode)))

;;; --------------------------------------------------------------------------

(when (equal custom-ide 'custom-ide-anaconda)
    (use-package anaconda-mode
        :bind (:map python-mode-map
                  ("C-c g o" . anaconda-mode-find-definitions-other-frame)
                  ("C-c g g" . anaconda-mode-find-definitions)
                  ("C-c C-x" . next-error))        
        :config
        (which-key-add-key-based-replacements "C-c g o" "find-defitions-other-window")
        (which-key-add-key-based-replacements "C-c g g" "find-defitions")
        (require 'pyvenv)
        :hook
        (python-mode-hook . anaconda-eldoc-mode)))

;;; --------------------------------------------------------------------------

(when (equal custom-ide 'custom-ide-elpy)
    (use-package elpy
        :after python
        :custom
        (elpy-rpc-python-command "python3")
        (display-fill-column-indicator-mode 1)
        (highlight-indentation-mode nil)
        :bind (:map python-mode-map
                  ("C-c g a" . elpy-goto-assignment)
                  ("C-c g o" . elpy-goto-definition-other-window)
                  ("C-c g g" . elpy-goto-definition)
                  ("C-c g ?" . elpy-doc))
        :config
        (message "elpy loaded")
        (use-package jedi)
        (which-key-add-key-based-replacements "C-c g a" "goto-assignment")
        (which-key-add-key-based-replacements "C-c g o" "find-defitions-other-window")
        (which-key-add-key-based-replacements "C-c g g" "find-defitions")
        (which-key-add-key-based-replacements "C-c g ?" "eldoc-definition")
        (elpy-enable)))

;;; --------------------------------------------------------------------------

(when (equal custom-ide 'custom-ide-elpy)
    (use-package flycheck
	:after elpy
	:diminish FlM
	:straight (flycheck :type git :flavor melpa
                      :host github
                      :repo "flycheck/flycheck")
	:config
	(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
	:hook (elpy-mode . flycheck-mode)))

(provide 'config-ide)
;;; config-ide.el ends here.
