;;; mifi-setup-python.el --- Configure Python dev environment -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; This file is used to configure the Python environment for Emacs which
;;; also includes checking for needed executables.

;;; Code:

;;; ##########################################################################
;;; These are variables that should have already been defined external
;;; to this module.

(defvar custom-ide)
(defvar debug-adapter)
(defvar enable-python)
(defvar python-executable (locate-file "python3" exec-path))
;;; ...........................................................................

;;; ##########################################################################

(defun mifi/set-custom-ide-python-keymaps ()
  (cond
    ((equal custom-ide 'custom-ide-lsp)
      (bind-keys :map python-mode-map
        ("C-c g r" . lsp-find-references)
        ("C-c g g" . xref-find-definitions)
        ("C-c g G" . xref-find-definitions-other-window)
        ("C-c g ?" . eldoc-doc-buffer)))
    ((equal custom-ide 'custom-ide-eglot)
      (bind-keys :map python-mode-map
        ("C-c g i" . eglot-find-implementation)
        ("C-c g r" . xref-find-references)
        ("C-c g D" . xref-find-definitions-other-window)
        ("C-c g g" . xref-find-definitions)
        ("C-c g n" . xref-find-references-and-replace)
        ("C-c g ." . python-eldoc-at-point)
        ("C-c g ?" . eldoc-doc-buffer)))
    ((equal custom-ide 'custom-ide-elpy)
      (elpy-enable)
      (bind-keys :map python-mode-map
        ("C-c g a" . elpy-goto-assignment)
        ("C-c g A" . elpy-goto-definition-other-window)
        ("C-c g g" . elpy-goto-definition)
        ("C-c g ?" . elpy-doc)))
    ((equal custom-ide 'custom-ide-lsp-bridge)
      (bind-keys :map python-mode-map
        ("C-c g a" . lsp-bridge-find-reference)
        ("C-c g A" . lsp-bridge-find-def-other-window)
        ("C-c g g" . lsp-bridge-find-def)
        ("C-c g i" . lsp-bridge-find-impl)
        ("C-c g n" . lsp-bridge-rename)
        ("C-c g ?" . lsp-bridge-popup-documentation)))
    ))

;;; ##########################################################################

(defun mifi/load-python-file-hook ()
  (python-mode)
  (flymake-mode 0)
  (message ">>> mifi/load-python-file-hook")
  (setq highlight-indentation-mode -1)
  (setq display-fill-column-indicator-mode t))

(defun mifi/before-save ()
  "Force the check of the current python file being saved."
  (when (eq major-mode 'python-mode) ;; Python Only
    (flycheck-mode 0)
    (flycheck-mode t)
    (delete-trailing-whitespace)))

(defun mifi/setup-python-debug-adapter () 
  (cond
    ((equal debug-adapter 'debug-adapter-dap-mode)
      (unless (featurep 'dap-mode) (dap-mode 1)) ;; Load if not loaded.
      (define-python-dap-hydra)
      (bind-keys :map python-mode-map
  ("C-c ." . dap-python-hydra/body)))))

(defun mifi/setup-python-custom-ide ()
  (cond
    ((equal custom-ide 'custom-ide-eglot)
      (message ">>> eglot-ensure")
      (when (boundp 'eglot-current-server)
  (let ((server (eglot-current-server)))
        	(when server
        	  (message "<<< Shutting down current EGLOT server before restart.")
        	  (eglot-shutdown server))))
      (eglot-ensure))
    ((equal custom-ide 'custom-ide-lsp)
      (message ">>> lsp-deferred")
      (lsp-deferred))
    (t
      (message ">>> failed to init custom-ide"))))

;; This function should only be called ONCE during python-mode startup.
(defun mifi/enable-python-features ()
  (message ">>> mifi/enable-python-features")
  ;; _____________________________
  (mifi/setup-python-debug-adapter)
  ;;___________________________
  ;; check for which custom-ide
  (mifi/setup-python-custom-ide)
  (when (featurep 'pydoc)
    (pydoc-python-command python-executable)
    (pydoc-pip-version-command (concat python-executable " -m pip --version")))
  (when (featurep 'yasnippet)
    (yas-global-mode t))
  (when (featurep 'pyvenv-auto)
    (pyvenv-auto-run))
  ;;
  ;; only do this after pyvenv-auto-run
  (setq python-executable (locate-file "python" exec-path))
  (unless (boundp 'python-executable)
    (setq python-executable (locate-file "python3" exec-path)))
  (message "Python executable is %S" python-executable))

(defun mifi/python-mode-triggered ()
  ;; (eldoc-box-hover-at-point-mode t) ;; Using Mitch Key for this
  (mifi/enable-python-features)
  (mifi/set-custom-ide-python-keymaps)
  (add-hook 'before-save-hook 'mifi/before-save)
  (setq dap-python-executable python-executable) ;; Otherwise it looks for 'python' else error.
  (set-fill-column 80))

;;; ##########################################################################

;; Use built-in python language mode.
(use-package python-mode
  :when enable-python
  :ensure t
  :defer t
  :mode ("\\.py\\'" . mifi/load-python-file-hook)
  :hook (python-mode . mifi/python-mode-triggered)
  :bind (:map python-mode-map
          ("C-c C-q" . quote-region)
          ("C-c q"   . quote-word)
          ("C-c |"   . display-fill-column-indicator-mode))
  :ensure-system-package
  ( (autoflake . "pip3 install autoflake") )
  :config
  (if (boundp 'python-shell-completion-native-disabled-interpreters)
    (add-to-list 'python-shell-completion-native-disabled-interpreters python-executable)
    (setq python-shell-completion-native-disabled-interpreters '(python-executable))))

(use-package blacken
  :ensure t
  :when enable-python
  :after python) ;Format Python file upon save.

;;; ##########################################################################

(use-package py-autopep8
  :when enable-python
  :defer t
  :ensure t
  ;; :vc (:url "https://github.com/emacsmirror/py-autopep8.git")
  :after python
  :hook ((python-mode . py-autopep8-mode)))

;;; ##########################################################################

;; This is a helpful macro that is used to put double quotes around a word.
(defalias 'quote-word
  (kmacro "\" M-d \" <left> C-y"))

(defalias 'quote-region
  (kmacro "C-w \" \" <left> C-y <right>"))

(defalias 'reformat-src-block
  (kmacro "C-s b e g i n _ s r c SPC e m a c s - l i s p <return> <down> C-c ' C-x h C-c ] C-x h M-x u n t a b <return> C-c ' C-s e n d _ s r c <return> <down>"))

(when enable-python
  (eval-after-load "python-mode"
    #'(bind-keys :map python-mode-map
      ("C-c C-q" . quote-region)
      ("C-c q"   . quote-word)
      ("C-c |"   . display-fill-column-indicator-mode))))

;;; ##########################################################################

(use-package pyvenv-auto
  :when enable-python
  :ensure t
  :after python
  :hook (python-mode . pyvenv-auto-run))

;;; ##########################################################################

(use-package pydoc
  ;;:ensure (:host github :repo "statmobile/pydoc")
  :ensure t
  :defer t
  :when enable-python
  :after python
  :custom
  (pydoc-python-command python-executable)
  (pydoc-pip-version-command (concat python-executable " -m pip --version")))

(use-package python-isort
  :ensure t)

;;; ##########################################################################

(defun mifi/register-dap-python-templates ()
  ;; (with-eval-after-load 'dap-lldb
  ;;   (dap-register-debug-template
  ;;     "Rust::LLDB Run Configuration"
  ;;     (list :type "lldb"
  ;;       :request "launch"
  ;;       :name "LLDB::Run"
  ;;       :gdbpath "rust-lldb"
  ;;       :target nil
  ;;       :cwd nil)))
  (dap-register-debug-template "Python :: Run file from project directory"
    (list :type "python"
      :args ""
      :cwd nil
      :module nil
      :program nil
      :request "launch"))

  (dap-register-debug-template "Python :: Run file (buffer)"
    (list :type "python"
      :args ""
      :cwd nil
      :module nil
      :program nil
      :request "launch"
      :name "Python :: Run file (buffer)")))

;;; ##########################################################################
;;; DAP for Python

(use-package dap-python
  :when (and (equal debug-adapter 'debug-adapter-dap-mode) enable-python)
  :ensure nil ;; It is already included if 'dap-mode has already been installed.
  :after dap-mode
  :config
  (mifi/register-dap-python-templates)
  (setq dap-python-executable python-executable) ;; Otherwise it looks for 'python' else error.
  (setq dap-python-debugger 'debugpy))

;;; ##########################################################################

(defun mifi/dap-end-python-debug-session ()
  "End the debug session and delete project Python buffers."
  (interactive)
  (kill-matching-buffers "\*Python :: Run file [from|\(buffer]*" nil :NO-ASK)
  (kill-matching-buffers "\*Python: Current File*" nil :NO-ASK)
  (kill-matching-buffers "\*dap-ui-*" nil :NO-ASK)
  (dap-disconnect (dap--cur-session)))

(defun mifi/dap-delete-all-python-debug-sessions ()
  "End the debug session and delete project Python buffers and all breakpoints."
  (interactive)
  (dap-breakpoint-delete-all)
  (mifi/dap-end-python-debug-session))

(defun mifi/dap-begin-python-debug-session ()
  "Begin a debug session with several dap windows enabled."
  (interactive)
  (dap-ui-show-many-windows)
  (dap-debug))

;;; ##########################################################################

(defhydra dap-python-hydra (:color pink :hint nil :foreign-keys run)
  "
  ^Stepping^            ^Switch^                 ^Breakpoints^          ^Debug^                     ^Eval
  ^^^^^^^^-----------------------------------------------------------------------------------------------------------------
  _._: Next            _ss_: Session            _bb_: Toggle           _dd_: Debug                 _ee_: Eval
  _/_: Step in         _st_: Thread             _bd_: Delete           _dr_: Debug recent          _er_: Eval region
  _,_: Step out        _sf_: Stack frame        _ba_: Add              _dl_: Debug last            _es_: Eval thing at point
  _c_: Continue        _su_: Up stack frame     _bc_: Set condition    _de_: Edit debug template   _ea_: Add expression.
  _r_: Restart frame   _sd_: Down stack frame   _bh_: Set hit count    _ds_: Debug restart
  _Q_: Disconnect      _sl_: List locals        _bl_: Set log message  _dx_: end session
                       _sb_: List breakpoints                          _dX_: end all sessions
                       _sS_: List sessions
                       _sR_: Session Repl
"
  ("n" dap-next)
  ("i" dap-step-in)
  ("o" dap-step-out)
  ("." dap-next)
  ("/" dap-step-in)
  ("," dap-step-out)
  ("c" dap-continue)
  ("r" dap-restart-frame)
  ("ss" dap-switch-session)
  ("st" dap-switch-thread)
  ("sf" dap-switch-stack-frame)
  ("su" dap-up-stack-frame)
  ("sd" dap-down-stack-frame)
  ("sl" dap-ui-locals)
  ("sb" dap-ui-breakpoints)
  ("sR" dap-ui-repl)
  ("sS" dap-ui-sessions)
  ("bb" dap-breakpoint-toggle)
  ("ba" dap-breakpoint-add)
  ("bd" dap-breakpoint-delete)
  ("bc" dap-breakpoint-condition)
  ("bh" dap-breakpoint-hit-condition)
  ("bl" dap-breakpoint-log-message)
  ("dd" dap-debug)
  ("dr" dap-debug-recent)
  ("ds" dap-debug-restart)
  ("dl" dap-debug-last)
  ("de" dap-debug-edit-template)
  ("ee" dap-eval)
  ("ea" dap-ui-expressions-add)
  ("er" dap-eval-region)
  ("es" dap-eval-thing-at-point)
  ("dx" mifi/dap-end-python-debug-session)
  ("dX" mifi/dap-delete-all-python-debug-sessions)
  ("x" nil "exit Hydra" :color yellow)
  ("q" mifi/dap-end-python-debug-session "quit" :color blue)
  ("Q" mifi/dap-delete-all-python-debug-sessions :color red))

(defun define-python-dap-hydra ()
  "Define the hydra keymap for Python."
  )

;;; ##########################################################################

(provide 'mifi-setup-python)
;;; mifi-setup-python.el ends here.
