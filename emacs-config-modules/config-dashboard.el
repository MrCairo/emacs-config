;;; --------------------------------------------------------------------------

(use-package all-the-icons
    :if (display-graphic-p))

(defun mrf/setup-dashboard-buffer ()
    "Set up the dashboard buffer and optionally make it the first."
    (setq dashboard-items '((recents . 15)
                               (bookmarks . 10)
                               (projects . 10))
        dashboard-icon-type 'all-the-icons
        dashboard-display-icons-p t
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t)
    ;; dashboard-projects-backend 'projectile)

    (global-set-key (kbd "C-c d") 'dashboard-open)

    (if (equal display-dashboard-at-start t)
        (progn
            (setq initial-buffer-choice
                (lambda ()
                    (get-buffer-create "*dashboard*")))
            (dashboard-open))
        (get-buffer-create "*dashboard*")))

(defun mrf/dashboard-banner ()
    "Setup defaults for the dashboard banner buffer."
    (setq dashboard-footer-messages '("Greetings Program!"))
    (setq dashboard-banner-logo-title "Welcome to Emacs!")
    (setq dashboard-startup-banner 'logo))

(use-package dashboard
    :after dired
    :init
    (mrf/dashboard-banner)
    :hook ((after-init     . mrf/setup-dashboard-buffer)
              (dashboard-mode . mrf/dashboard-banner)))

(provide 'config-dashboard)
;;; config-dashboard.el ends here
