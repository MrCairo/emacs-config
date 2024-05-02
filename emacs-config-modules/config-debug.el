;;; ------------------------------------------------------------------------
  ;;; Alternate fork to handle possible performance bug(s)
(use-package jsonrpc
    :straight (jsonrpc :type git :host github :repo "emacs-straight/jsonrpc" :files ("*" (:exclude ".git"))))

(when (equal debug-adapter 'enable-dape)
    (use-package dape
        :after jsonrpc
        :defer t
        ;; :defer t
        ;; To use window configuration like gud (gdb-mi)
        ;; :init
        ;; (setq dape-buffer-window-arrangement 'gud)
        :custom
        (dape-buffer-window-arrangement 'right)  ;; Info buffers to the right
        ;; To not display info and/or buffers on startup
        ;; (remove-hook 'dape-on-start-hooks 'dape-info)
        (remove-hook 'dape-on-start-hooks 'dape-repl)

        ;; To display info and/or repl buffers on stopped
        ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
        ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

        ;; By default dape uses gdb keybinding prefix
        ;; If you do not want to use any prefix, set it to nil.
        ;; (setq dape-key-prefix "\C-x\C-a")

        ;; Kill compile buffer on build success
        ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

        ;; Save buffers on startup, useful for interpreted languages
        ;; (add-hook 'dape-on-start-hooks
        ;;           (defun dape--save-on-start ()
        ;;             (save-some-buffers t t)))

        :config
        ;; Projectile users
        ;; (setq dape-cwd-fn 'projectile-project-root)
        ;; :straight (dape :type git
        ;;           :host github :repo "emacs-straight/dape"
        ;;           :files ("*" (:exclude ".git")))
        (message "DAPE Configured")))

;;; --------------------------------------------------------------------------
;;; Debug Adapter Protocol      
(when (equal debug-adapter 'enable-dap-mode)
    (use-package dap-mode
        ;; Uncomment the config below if you want all UI panes to be hidden by default!
        ;; :custom
        ;; (lsp-enable-dap-auto-configure nil)
        :commands dap-debug
        :custom
        (dap-auto-configure-features '(sessions locals breakpoints expressions repl controls tooltip))
        :config
        (dap-ui-mode 1)
        (message "DAP mode loaded and configured.")))

;;; --------------------------------------------------------------------------

(setq mrf/vscode-js-debug-dir (file-name-concat user-emacs-directory "dape/vscode-js-debug"))

(defun mrf/install-vscode-js-debug ()
    "Run installation procedure to install JS debugging support"
    (interactive)
    (mkdir mrf/vscode-js-debug-dir t)
    (let ((default-directory (expand-file-name mrf/vscode-js-debug-dir)))
        
        (vc-git-clone "https://github.com/microsoft/vscode-js-debug.git" "." nil)
        (message "git repository created")
        (call-process "npm" nil "*snam-install*" t "install")
        (message "npm dependencies installed")
        (call-process "npx" nil "*snam-install*" t "gulp" "dapDebugServer")
        (message "vscode-js-debug installed")))

;;; --------------------------------------------------------------------------

;; (mrf/install-vscode-js-debug)

;;; --------------------------------------------------------------------------

(defun mrf/dape-end-debug-session ()
    "End the debug session."
    (interactive)
    (dape-quit))

(defun mrf/dape-delete-all-debug-sessions ()
    "End the debug session and delete all breakpoints."
    (interactive)
    (dape-breakpoint-remove-all)
    (mrf/dape-end-debug-session))

(defhydra dape-hydra (:color pink :hint nil :foreign-keys run)
    "
  ^Stepping^          ^Switch^                 ^Breakpoints^          ^Debug^                     ^Eval
  ^^^^^^^^----------------------------------------------------------------------------------------------------------------
  _._: Next           _st_: Thread            _bb_: Toggle           _dd_: Debug                 _ee_: Eval Expression
  _/_: Step in        _si_: Info              _bd_: Delete           _dw_: Watch dwim
  _,_: Step out       _sf_: Stack Frame       _ba_: Add              _dx_: end session
  _c_: Continue       _su_: Up stack frame    _bc_: Set condition    _dX_: end all sessions
  _r_: Restart frame  _sd_: Down stack frame  _bl_: Set log message
  _Q_: Disconnect     _sR_: Session Repl
                      _sU_: Info Update

"
    ("n" dape-next)
    ("i" dape-step-in)
    ("o" dape-step-out)
    ("." dape-next)
    ("/" dape-step-in)
    ("," dape-step-out)
    ("c" dape-continue)
    ("r" dape-restart)
    ("si" dape-info)
    ("st" dape-select-thread)
    ("sf" dape-select-stack)
    ("su" dape-stack-select-up)
    ("sU" dape-info-update)
    ("sd" dape-stack-select-down)
    ("sR" dape-repl)
    ("bb" dape-breakpoint-toggle)
    ("ba" dape--breakpoint-place)
    ("bd" dape-breakpoint-remove-at-point)
    ("bc" dape-breakpoint-expression)
    ("bl" dape-breakpoint-log)
    ("dd" dape)
    ("dw" dape-watch-dwim)
    ("ee" dape-evaluate-expression)
    ("dx" mrf/dape-end-debug-session)
    ("dX" mrf/dape-delete-all-debug-sessions)
    ("x" nil "exit Hydra" :color yellow)
    ("q" mrf/dape-end-debug-session "quit" :color blue)
    ("Q" mrf/dape-delete-all-debug-sessions :color red))

;;; --------------------------------------------------------------------------

(setq dap-lldb-debug-program
    "/Users/strider/Developer/plain_unix/llvm-project/build/bin/lldb-dap")

(defun mrf/populate-lldb-start-file-args (conf)
    "Populate CONF with the required arguments."
    (-> conf
        (dap--put-if-absent :dap-server-path dap-lldb-debug-program)
        (dap--put-if-absent :type "lldb-dap")
        (dap--put-if-absent :cwd default-directory)
        (dap--put-if-absent :program (funcall dap-lldb-debugged-program-function))
        (dap--put-if-absent :name "LLDB Debug")))

(when (equal debug-adapter 'enable-dap-mode)
    (use-package dap-cpptools
	:disabled
        :after dap-mode
        :straight (dap-lldb :type git :host github :repo "emacs-lsp/dap-mode"))

    (use-package dap-lldb
	:disabled
        :straight (dap-lldb :type git :host github :repo "emacs-lsp/dap-mode")
        :after dap-mode
        :config
        (dap-register-debug-provider "lldb-dap" 'mrf/populate-lldb-start-file-args)
        (dap-register-debug-template "LLDB DAP :: Run from project directory"
            (list :type "lldb-dap"
                :name "LLDB using DAP"
                :program "a.out"
                :request "launch"))))

;;; --------------------------------------------------------------------------
;;; DAP for Python

(when (equal debug-adapter 'enable-dap-mode)
    (use-package dap-python
        :straight (dap-python :type git :host github :repo "emacs-lsp/dap-mode")
        :after dap-mode
        :config
        (setq dap-python-executable "python3") ;; Otherwise it looks for 'python' else error.
        (setq dap-python-debugger 'debugpy)
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
                :name "Python :: Run file (buffer)"))))

;;; --------------------------------------------------------------------------
;;; DAP for NodeJS


(defun my-setup-dap-node ()
    "Require dap-node feature and run dap-node-setup if VSCode module isn't already installed"
    (require 'dap-node)
    (unless (file-exists-p dap-node-debug-path) (dap-node-setup)))

(when (equal debug-adapter 'enable-dap-mode)
    (use-package dap-node
	:disabled
        :defer t
        :straight (dap-node :type git
                      :flavor melpa
                      :files (:defaults "icons" "dap-mode-pkg.el")
                      :host github
                      :repo "emacs-lsp/dap-mode")
        :after dap-mode
        :config
        (require 'dap-firefox)
        (dap-register-debug-template
            "Launch index.ts"
            (list :type "node"
                :request "launch"
                :program "${workspaceFolder}/index.ts"
                :dap-compilation "npx tsc index.ts --outdir dist --sourceMap true"
                :outFiles (list "${workspaceFolder}/dist/**/*.js")
                :name "Launch index.ts"))
        ;; (dap-register-debug-template
        ;;    "Launch index.ts"
        ;;    (list :type "node"
        ;;    :request "launch"
        ;;    :program "${workspaceFolder}/index.ts"
        ;;    :dap-compilation "npx tsc index.ts --outdir dist --sourceMap true"
        ;;    :outFiles (list "${workspaceFolder}/dist/**/*.js")
        ;;    :name "Launch index.ts"))
        )
    (add-hook 'typescript-mode-hook 'my-setup-dap-node)
    (add-hook 'js2-mode-hook 'my-setup-dap-node))

;;; --------------------------------------------------------------------------

(defun mrf/end-debug-session ()
    "End the debug session and delete project Python buffers."
    (interactive)
    (kill-matching-buffers "\*Python :: Run file [from|\(buffer]*" nil :NO-ASK)
    (kill-matching-buffers "\*Python: Current File*" nil :NO-ASK)
    (kill-matching-buffers "\*dap-ui-*" nil :NO-ASK)
    (dap-disconnect (dap--cur-session)))

(defun mrf/delete-all-debug-sessions ()
    "End the debug session and delete project Python buffers and all breakpoints."
    (interactive)
    (dap-breakpoint-delete-all)
    (mrf/end-debug-session))

(defun mrf/begin-debug-session ()
    "Begin a debug session with several dap windows enabled."
    (interactive)
    (dap-ui-show-many-windows)
    (dap-debug))

(defhydra dap-hydra (:color pink :hint nil :foreign-keys run)
    "
  ^Stepping^          ^Switch^                 ^Breakpoints^          ^Debug^                     ^Eval
  ^^^^^^^^----------------------------------------------------------------------------------------------------------------
  _._: Next           _ss_: Session            _bb_: Toggle           _dd_: Debug                 _ee_: Eval
  _/_: Step in        _st_: Thread             _bd_: Delete           _dr_: Debug recent          _er_: Eval region
  _,_: Step out       _sf_: Stack frame        _ba_: Add              _dl_: Debug last            _es_: Eval thing at point
  _c_: Continue       _su_: Up stack frame     _bc_: Set condition    _de_: Edit debug template   _ea_: Add expression.
  _r_: Restart frame  _sd_: Down stack frame   _bh_: Set hit count    _ds_: Debug restart
  _Q_: Disconnect     _sl_: List locals        _bl_: Set log message  _dx_: end session
                    _sb_: List breakpoints                          _dX_: end all sessions
                    _sS_: List sessions
                    _sR_: Session Repl
"
    ("n" dap-next)    ("i" dap-step-in)    ("o" dap-step-out)   ("." dap-next)
    ("/" dap-step-in) ("," dap-step-out)   ("c" dap-continue)   ("r" dap-restart-frame)
    
    ("ss" dap-switch-session) ("st" dap-switch-thread)    ("sf" dap-switch-stack-frame)
    ("su" dap-up-stack-frame) ("sd" dap-down-stack-frame) ("sl" dap-ui-locals)
    ("sb" dap-ui-breakpoints) ("sR" dap-ui-repl)          ("sS" dap-ui-sessions)
    
    ("bb" dap-breakpoint-toggle)    ("ba" dap-breakpoint-add)           ("bd" dap-breakpoint-delete)
    ("bc" dap-breakpoint-condition) ("bh" dap-breakpoint-hit-condition) ("bl" dap-breakpoint-log-message)
    
    ("dd" dap-debug)      ("dr" dap-debug-recent) ("ds" dap-debug-restart)
    ("dl" dap-debug-last) ("de" dap-debug-edit-template)
    
    ("ee" dap-eval) ("ea" dap-ui-expressions-add) ("er" dap-eval-region) ("es" dap-eval-thing-at-point)
    
    ("dx" mrf/end-debug-session) ("dX" mrf/delete-all-debug-sessions)
    
    ("x" nil "exit Hydra" :color yellow) ("q" mrf/end-debug-session "quit" :color blue)
    ("Q" mrf/delete-all-debug-sessions :color red))

;;; --------------------------------------------------------------------------

(use-package realgud
    :disabled
    :after c-mode
    :defer t)

(use-package realgud-lldb
    :disabled
    :after realgud
    :straight (realgud-lldb
                  :type git
                  :flavor melpa
                  :files (:defaults ("lldb" "lldb/*.el") "realgud-lldb-pkg.el")
                  :host github
                  :repo "realgud/realgud-lldb"))

;;; --------------------------------------------------------------------------

(when (package-installed-p 'realgud)
    (use-package cc-mode
        :bind (:map c-mode-map
                  ("C-c , j" . realgud:cmd-jump)
                  ("C-c , k" . realgud:cmd-kill)
                  ("C-c , s" . realgud:cmd-step)
                  ("C-c , n" . realgud:cmd-next)
                  ("C-c , q" . realgud:cmd-quit)
                  ("C-c , F" . realgud:window-bt)
                  ("C-c , U" . realgud:cmd-until)
                  ("C-c , X" . realgud:cmd-clear)
                  ("C-c , !" . realgud:cmd-shell)
                  ("C-c , b" . realgud:cmd-break)
                  ("C-c , f" . realgud:cmd-finish)
                  ("C-c , D" . realgud:cmd-delete)
                  ("C-c , +" . realgud:cmd-enable)
                  ("C-c , R" . realgud:cmd-restart)
                  ("C-c , -" . realgud:cmd-disable)
                  ("C-c , B" . realgud:window-brkpt)
                  ("C-c , c" . realgud:cmd-continue)
                  ("C-c , e" . realgud:cmd-eval-dwim)
                  ("C-c , Q" . realgud:cmd-terminate)
                  ("C-c , T" . realgud:cmd-backtrace)
                  ("C-c , h" . realgud:cmd-until-here)
                  ("C-c , u" . realgud:cmd-older-frame)
                  ("C-c , 4" . realgud:cmd-goto-loc-hist-4)
                  ("C-c , 5" . realgud:cmd-goto-loc-hist-5)
                  ("C-c , 6" . realgud:cmd-goto-loc-hist-6)
                  ("C-c , 7" . realgud:cmd-goto-loc-hist-7)
                  ("C-c , 8" . realgud:cmd-goto-loc-hist-8)
                  ("C-c , 9" . realgud:cmd-goto-loc-hist-9)
                  ("C-c , d" . realgud:cmd-newer-frame)
                  ("C-c , RET" . realgud:cmd-repeat-last)
                  ("C-c , E" . realgud:cmd-eval-at-point)
                  ("C-c , I" . realgud:cmdbuf-info-describe)
                  ("C-c , C-i" . realgud:cmd-info-breakpoints))))

(provide 'config-debug)
;; config-debug.el ends here.
