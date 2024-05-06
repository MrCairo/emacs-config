;;; --------------------------------------------------------------------------

(use-package term+
    :ensure (:repo "tarao/term-plus-el" :fetcher github)
    :commands term
    :config
    (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
    ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

    ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
    (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;;; --------------------------------------------------------------------------

(use-package eterm-256color
    :defer t
    :hook (term-mode . eterm-256color-mode))

;;; --------------------------------------------------------------------------

(use-package vterm
    :ensure (:fetcher github :repo "akermu/emacs-libvterm")
    :commands vterm
    :config
    (setq vterm-environment ("PS1=\\u@\\h:\\w \n$"))
    (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
    (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
    (setq vterm-max-scrollback 10000))

;;; --------------------------------------------------------------------------

(defun efs/configure-eshell ()
    ;; Save command history when commands are entered
    (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

    ;; Truncate buffer for performance
    (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

    ;; Bind some useful keys for evil-mode
    ;; (bind-keys :map eshell-mode-map
    ;;  ("C-r" . eshell-hist-mode)
    ;;  ("<home>" . eshell-bol))
    
    ;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
    ;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
    ;; (evil-normalize-keymaps)

    (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
    :after eshell)

(use-package eshell
    :hook (eshell-first-time-mode . efs/configure-eshell)
    :config
    (with-eval-after-load 'esh-opt
        (setq eshell-destroy-buffer-when-process-dies t)
        (setq eshell-visual-commands '("htop" "zsh" "vim")))

    (eshell-git-prompt-use-theme 'powerline))

(elpaca-process-queues)

(provide 'config-term)
;;; config-term.el ends here.
