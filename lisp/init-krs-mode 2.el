;;; init-krs-mode.el -- Kernighan, Ritchie, Stroustrup mode
;;; Commentary:
;;; Otherwise known as the C/C++ mode.

;;; Code:

(use-package modern-cpp-font-lock
  :ensure t)

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

(provide 'init-krs-mode)

;;; init-krs-mode.el ends here.

