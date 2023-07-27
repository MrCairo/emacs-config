;;; init-flycheck-mode.el -- Syntax Checker.
;;;
;;; Commentary:
;;; ------------------------------------------------------------------------
;;; Flycheck is a modern on-the-fly syntax checking extension for GNU Emacs,
;;; intended as replacement for the older Flymake extension which is part of
;;; GNU Emacs.  For a detailed comparison to Flymake see Flycheck versus
;;; Flymake.
;;;
;;; https://www.flycheck.org/en/latest/user/flycheck-versus-flymake.html#flycheck-versus-flymake
;;;

;; Package-Requires: use-package

;;; Code:

;;; Forces flycheck to check.
;;; Right now it's not working out of the box which is why this little
;;; hack is here.

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(require 'flycheck-package)

(eval-after-load 'flycheck-package
  '(flycheck-package-setup))

(defun mrf/check_fly ()
  "Force the check of the current python file being saved."
  (when (eq major-mode 'python-mode) ;; Python Only
    (flycheck-mode 0)
    (flycheck-mode t)))

(add-hook 'before-save-hook #'mrf/check_fly)

(provide 'init-flycheck-mode)

;;; init-flycheck-mode.el ends here.
