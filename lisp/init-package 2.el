;;; init-package.el -- Sets up package archives and priority.
;;; Commentary:

;;; Code:

(require 'package)
(message "seting up archive...")

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("elpa" . "https://elpa.gnu.org/packages/")))

;; (setq package-archive-priorities '(  ("melpa" . 10)
;;                                      ("melpa-stable" . 5)
;;                                      ("org" . 1)))


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defvar mrf/must-install-packages
   '(
       general
       cl-lib
       auto-complete
       better-defaults
       bind-key
    ))

(mapc #'(lambda (theme)
          (unless (package-installed-p theme)
            (package-install theme)))
      mrf/must-install-packages)

;; (require 'init-site-lisp)

(provide 'init-package)

;;; init-package.el ends here.
