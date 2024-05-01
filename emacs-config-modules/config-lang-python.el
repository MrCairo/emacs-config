(defun mrf/set-custom-ide-python-keymaps ()
    (message "<<< Set python-mode keymaps based upon IDE.")
    (cond
        ((equal custom-ide 'custom-ide-eglot-lsp)
            ;; (unless (featurep 'lsp)
            ;;     (lsp-deferred))
            ;; (unless (featurep 'eglot)
            ;;     (eglot))
            (bind-keys :map python-mode-map
                ("C-c g r" . lsp-find-references)
                ("C-c g o" . xref-find-definitions-other-window)
                ("C-c g g" . xref-find-definitions)
                ("C-c g ?" . eldoc-doc-buffer))
            (message (format ">>> set python-mode-map for %s" custom-ide)))
        ;; Activate LSP and EGLOT *if* selected as custom-ide
        ((equal custom-ide 'custom-ide-elpy)
            (elpy-enable)
            (bind-keys :map python-mode-map
                ("C-c g a" . elpy-goto-assignment)
                ("C-c g o" . elpy-goto-definition-other-window)
                ("C-c g g" . elpy-goto-definition)
                ("C-c g ?" . elpy-doc))
            (message (format ">>> setting python-mode-map for %s" custom-ide)))
        ((equal custom-ide 'custom-ide-lsp-bridge)
            (bind-keys :map python-mode-map
                ("C-c g a" . lsp-bridge-find-reference)
                ("C-c g o" . lsp-bridge-find-def-other-window)
                ("C-c g g" . lsp-bridge-find-def)
                ("C-c g i" . lsp-bridge-find-impl)
                ("C-c g r" . lsp-bridge-rename)
                ("C-c g ?" . lsp-bridge-popup-documentation))
            (message (format ">>> set python-mode-map for %s" custom-ide)))
        ))

;;; --------------------------------------------------------------------------

(defun mrf/load-python-file-hook ()
    (python-mode)
    ;; (unless (featurep 'jedi)
    ;;  (use-package jedi
    ;;      :config
    ;;      (jedi:setup)))
    (setq highlight-indentation-mode -1)
    (setq display-fill-column-indicator-mode t))

(defun mrf/before-save ()
    "Force the check of the current python file being saved."
    (when (eq major-mode 'python-mode) ;; Python Only
        (flycheck-mode 0)
        (flycheck-mode t)
        (message "deleting trailing whitespace enabled")
        (delete-trailing-whitespace)))

(defun mrf/python-mode-triggered ()
    (message ">>> mrf/python-mode-triggered")
    ;; (eldoc-box-hover-at-point-mode t) ;; Using Mitch Key for this
    (if (equal debug-adapter 'enable-dap-mode)
        (unless (featurep 'dap-mode)
            (dap-mode))
        (if (not (featurep 'dape))
            (use-package dape :demand t)))
    (mrf/set-custom-ide-python-keymaps)
    (unless (featurep 'yasnippet)
	(yas-global-mode t))
    (add-hook 'before-save-hook 'mrf/before-save)
    (set-fill-column 80))

(use-package python-mode
    :defer t
    :diminish Py
    :config
    :hook (python-mode . mrf/python-mode-triggered) )

(add-to-list 'auto-mode-alist '("\\.py\\'" . mrf/load-python-file-hook))

(use-package blacken
    :after python) ;Format Python file upon save.

(if (boundp 'python-shell-completion-native-disabled-interpreters)
    (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
    (setq python-shell-completion-native-disabled-interpreters '("python3")))

;;; --------------------------------------------------------------------------

(use-package py-autopep8
    :after (python-mode python)
    :hook ((python-mode) . py-autopep8-mode))

;;; --------------------------------------------------------------------------

;; This is a helpful macro that is used to put double quotes around a word.
(defalias 'quote-word
    (kmacro "\" M-d \" <left> C-y"))

(defalias 'quote-region
    (kmacro "C-w \" \" <left> C-y <right>"))

(eval-after-load "python"
    #'(bind-keys :map python-mode-map
          ("C-c C-q" . quote-region)
          ("C-c q"   . quote-word)
          ("C-c |"   . display-fill-column-indicator-mode)))

;;; --------------------------------------------------------------------------

(when (equal debug-adapter 'enable-dap-mode)
    ;; (dolist (m (list python-mode-map typescript-ts-mode-map c-mode-map c++-mode-map))
    (use-package dap
        :defer t
        :bind (:map prog-mode-map
                  ("C-c ." . dap-hydra/body))))

(when (equal debug-adapter 'enable-dape)
    ;; (dolist (m (list python-mode-map typescript-ts-mode-map c-mode-map c++-mode-map))
    (use-package dape
        :defer t
        :bind (:map prog-mode-map
                  ("C-c ." . dape-hydra/body))))

;;; --------------------------------------------------------------------------

(use-package pyvenv-auto
    :after python
    :config (message ">>> Starting pyvenv-auto")
    :hook (python-mode . pyvenv-auto-run))

(use-package pydoc
    :straight (pydoc :type git :flavor melpa
                  :host github :repo "statmobile/pydoc")
    :after python
    :custom
    (pydoc-python-command "python3")
    (pydoc-pip-version-command "pip3 --version")
    )

(provide 'config-lang-python)
;;; config-lang-python.el ends here.
