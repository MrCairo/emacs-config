;;; --------------------------------------------------------------------------

(defun mrf/load-c-file-hook ()
    (message "Running C/C++ file hook")
    (c-mode)
    (unless (featurep 'realgud))
    (use-package realgud)
    (highlight-indentation-mode nil)
    (display-fill-column-indicator-mode t))

(defun code-compile ()
    "Look for a Makefile and compiles the code with gcc/cpp."
    (interactive)
    (unless (file-exists-p "Makefile")
        (set (make-local-variable 'compile-command)
            (let ((file (file-name-nondirectory buffer-file-name)))
                (format "%s -o %s %s"
                    (if  (equal (file-name-extension file) "cpp") "g++" "gcc" )
                    (file-name-sans-extension file)
                    file)))
        (compile compile-command)))

(global-set-key [f9] 'code-compile)
(add-to-list 'auto-mode-alist '("\\.c\\'" . mrf/load-c-file-hook))

;;; --------------------------------------------------------------------------

(when enable-gb-dev
    (use-package z80-mode
        :straight (z80-mode
                      :type git
                      :host github
                      :repo "SuperDisk/z80-mode"))

    (use-package mwim
        :straight (mwim
                      :type git
                      :flavor melpa
                      :host github
                      :repo "alezost/mwim.el"))

    (use-package rgbds-mode
        :after mwim
        :straight (rgbds-mode
                      :type git :host github
                      :repo "japanoise/rgbds-mode")))

(provide 'config-lang-c)
;;; config-lang-c.el ends here.
