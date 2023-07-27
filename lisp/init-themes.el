;;; init-themes.el -- Keep packages up-to-date.
;;; Commentary:
;;; ------------------------------------------------------------------------
;;; The following are packages that make the default Emacs environment
;;; a little more comfortable.  Things like color and font themes, a
;;; dashboard when Emacs starts

;;; Code:

(add-to-list 'custom-theme-load-path "~/Documents/Emacs-Related/Additional-Themes")

(defvar mrf/list-theme-packages
   '(
       color-theme-sanityinc-tomorrow
       doom-themes
       exotica-theme
       immaterial-theme
       material-theme
       timu-caribbean-theme
       timu-macos-theme
    ))

(mapc #'(lambda (theme)
          (unless (package-installed-p theme)
            (package-install theme)))
      mrf/list-theme-packages)

;;; =========================================================================

;; (load-theme 'deeper-blue t) ;; My favorite theme for term or UI modes
;; (load-theme 'doom-palenight t)
;; (load-theme 'doom-monokai-pro t)
;; (load-theme 'afternoon t)
;; (load-theme 'tomorrow-night-blue t)
;; (load-theme 'tomorrow-night-bright t)
;; (load-theme 'borland-blue t)

(provide 'init-themes)

;;; init-themes.el ends here.

