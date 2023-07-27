;;; init-dashboard.el -- A nice, well, startup Dashboard
;;
;;; Commentary:
;;; Dashboard is an extensible Emacs startup screen showing you what’s
;;; most important.
;;; ------------------------------------------------------------------------

;;; Code:

;; (create-file-buffer "*dashboard*")

(use-package all-the-icons
   :if (display-graphic-p))

(use-package dashboard
   :ensure t
   :preface
   (defun mrf/dashboard-banner ()
      (setq dashboard-footer-messages '("Greetings Program!"))
      (setq dashboard-banner-logo-title "Welcome to Emacs!")
      (setq dashboard-startup-banner "~/Pictures/Book-icon.png"))
   :custom
   (dashboard-items '((recents . 9)
                        (bookmarks . 5)))
   :config
   (dashboard-setup-startup-hook)
   (dashboard-open)
   (setq dashboard-center-content t)
   :hook ((after-init     . dashboard-refresh-buffer)
            (dashboard-mode . mrf/dashboard-banner)))

(provide 'init-dashboard)

;;; init-dashboard.el ends here.

