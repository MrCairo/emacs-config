;;; early-init.el -*- flycheck-disabled-checkers: (emacs-lisp); lexical-binding: nil -*-
;;;
;;; Commentary:

;; Settings/Packages that need to be used early in the initialization process
;; of the Emacs startup. This file is executed before init.el.
;;
;;
;; DO NOT MODIFY this file directly as changes will be overwritten.
;; The source this file is generated from is from "emacs-config-elpa.org"

;;; Code:

;;; ##########################################################################
;; (setq package-vc-register-as-project nil) ; Emacs 30
(add-hook 'package-menu-mode-hook #'hl-line-mode)

(setq package-enable-at-startup t)

;; This allows for a set of PROXY variables/settings to be loaded before
;; we actually begin the load.
(let
  ((file (expand-file-name "early-init-proxy.el" user-emacs-directory)))
  (when (file-exists-p file)
    (load file)))

(setq package-archives
  '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
     ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
  '(
     ( "org" . 5 )
     ( "gnu" . 50 )
     ( "melpa-stable" . 40 )
     ( "melpa" . 60 )
     ( "gnu-dev" . 20 )
     ( "nongnu" . 10)
     ))

;;
;; I pull the whole mirror locally - it's not huge, 2.1 GB. If it's
;; available, add them to the start of the package-archives list.
;;
(when (file-directory-p "/opt/local/elpa-mirror")
  ;; Make sure to refresh this local reppo often!!
  (add-to-list 'package-archives '("local-gnu" . "/opt/local/elpa-mirror/gnu") t)
  (add-to-list 'package-archives '("local-nongnu" . "/opt/local/elpa-mirror/nongnu") t)
  (add-to-list 'package-archives '("local-melpa" . "/opt/local/elpa-mirror/melpa") t)
  (add-to-list 'package-archives '("local-melpa-stable" . "/opt/local/elpa-mirror/stable-melpa") t)
  (add-to-list 'package-archive-priorities '( "local-gnu" . 99 ))
  (add-to-list 'package-archive-priorities '( "local-melpa" . 98 ))
  (add-to-list 'package-archive-priorities '( "local-nongnu" . 97))
  (add-to-list 'package-archive-priorities '( "local-melpa-stable" . 90 )))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ;; w/o this Emacs freezes when refreshing ELPA

;;; ##########################################################################

(let ( (lisp-dir (expand-file-name "lisp" user-emacs-directory))
       (lisp-lang-dir (expand-file-name "lisp/lang" user-emacs-directory)) )
  (when (file-directory-p lisp-dir)
    (add-to-list 'load-path lisp-dir))
  (when (file-directory-p lisp-lang-dir)
    (add-to-list 'load-path lisp-lang-dir)))

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; For some reason, the function recentf-expand-file-name has been showing up
;; as 'undefined' even though this is a byte-compiled internal function. So,
;; instead of trying to find the issue, I'm just including it here as a
;; local package so that it works. Maybe one day I can remove it.
;; (use-package recentf :ensure nil :demand t)
(use-package recentf :ensure nil :demand t)

;;
;; For use-package-always-ensure to t If the 'elpa' directory is missing.
;; It's a simple way to more intelligently determine if packages should
;; more aggressively be required. The good thing is that the user doesn't
;; have to manually change the value just for a clean install.
;;
(if (file-directory-p (expand-file-name "elpa" user-emacs-directory))
  (setq use-package-always-ensure nil)
  (setq use-package-always-ensure t))

(setq use-package-compute-statistics t
  use-package-verbose t
  use-package-always-demand nil
  use-package-always-defer nil)

;;; ##########################################################################

(setq gc-cons-threshold (* 1024 1024 80))
;;; Set high for initial startup
(setq gc-cons-percentage 0.5)

;; Process performance tuning
(setq read-process-output-max (* 4048 1024))
(setq process-adaptive-read-buffering nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-percentage 0.1)
    (setq startup-time-message
      (format "Emacs read in %.2f seconds with %d garbage collections."
        (float-time (time-subtract after-init-time before-init-time))
        gcs-done))
    (message startup-time-message)))

;;; ##########################################################################

(defconst *is-a-mac* (eq system-type 'darwin))

(defun mifi/setup-path-from-exec-path ()
  "Sets the environment PATH from the the `exec-path' list using the OS's
defined path-separator."
  (interactive)
  (let ((path-from-exec-path (string-join exec-path path-separator)))
    (setenv "PATH" path-from-exec-path)))

(defun mifi/setup-exec-path ()
  "A list of customized executable paths for standard Linux and macOS
(and possibly) other UN*X type environments."
  (interactive)
  (cond
    ((eq system-type 'darwin)
      (setq exec-path
        '( "~/.cargo/bin" "~/.local/bin"
           "/opt/homebrew/bin" "/opt/homebrew/sbin"
           "/Library/Frameworks/Python.framework/Versions/Current/bin"
           "/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin"
           "/sbin" "/bin" "/opt/local/bin")))
    ((eq system-type 'gnu/linux)
      (setq exec-path
        '( "/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin"
           "/sbin" "/bin" "/usr/local/games" "/usr/games")))
    (t ;; default to something
      (setq exec-path '( "/usr/local/sbin" "/usr/local/bin"
                         "/usr/sbin" "/usr/bin"))))
  (when (file-directory-p "/usr/local/go/bin")
    (add-to-list 'exec-path "/usr/local/go/bin"))    
  (mifi/setup-path-from-exec-path))

(when *is-a-mac*
  (setq browse-url-firefox-program
    "/Applications/Firefox.app/Contents/MacOS/firefox")
  (setq browse-url-chrome-program
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))

(add-hook 'before-init-hook #'mifi/setup-exec-path)

;;; early-init.el ends here.
