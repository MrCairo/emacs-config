;;; early-init.el -*- flycheck-disabled-checkers: (emacs-lisp); lexical-binding: nil -*-
;;;
;;; Commentary:

;; Settings/Packages that need to be used early in the initialization process
;; of the Emacs startup. This file is executed before init.el.
;;
;;
;; DO NOT MODIFY this file directly as changes will be overwritten.
;; The source this file is generated from is from "emacs-config.org"

;;; Code:

;;; ##########################################################################
(setq package-enable-at-startup nil)

;; Process performance tuning

(setq read-process-output-max (* 64 1024))
(setq process-adaptive-read-buffering nil)

(setq package-vc-register-as-project nil) ; Emacs 30
(add-hook 'package-menu-mode-hook #'hl-line-mode)

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

(when (file-directory-p "/opt/local/elpa-mirror")
  ;; Make sure to refresh this local reppo often!!
  (add-to-list 'package-archives '("local-gnu" . "/opt/local/elpa-mirror/gnu"))
  (add-to-list 'package-archives '("local-nongnu" . "/opt/local/elpa-mirror/nongnu"))
  (add-to-list 'package-archives '("local-melpa" . "/opt/local/elpa-mirror/melpa"))
  (add-to-list 'package-archives '("local-melpa-stable" . "/opt/local/elpa-mirror/stable-melpa")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
  '(
     ( "local-gnu" . 99 )
     ( "local-melpa" . 98 )
     ( "local-nongnu" . 97)
     ( "local-melpa-stable" . 90 )
     ( "org" . 5 )
     ( "gnu" . 50 )
     ( "melpa-stable" . 40 )
     ( "melpa" . 30 )
     ( "gnu-dev" . 20 )
     ( "nongnu" . 10)
     ))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ;; w/o this Emacs freezes when refreshing ELPA

;;; ##########################################################################

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(let ((ver-need-vc "30.0"))
  (when (version< emacs-version ver-need-vc)
    ;; (unless (package-installed-p 'package-vc)
    ;;   (package-vc-install "https://github.com/slotThe/vc-use-package"))
    (message ">>> Loading local-package-vc.el")
    (require 'local-package-vc)))

(setq use-package-compute-statistics t
  use-package-verbose t
  use-package-always-ensure nil
  use-package-always-demand nil
  use-package-always-defer nil)

;; (use-package gcmh
;;   :delight gcmh-mode
;;   :config
;;   (setq gcmh-idle-delay 5
;;     gcmh-high-cons-threshold (* 100 1024 1024))      ; 100mb
;;   (gcmh-mode 1))

;;; Set high for initial load.
;; (setq gc-cons-threshold (* 1024 1024 100))
(setq gc-cons-threshold 8000000) ;; Default
(setq gc-cons-percentage 0.3)

(add-hook 'emacs-startup-hook
  (lambda ()
    ;; (setq gc-cons-percentage 0.1) ;; Default value for `gc-cons-percentage'
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
  (mifi/setup-path-from-exec-path))

(when *is-a-mac*
  (setq browse-url-firefox-program
    "/Applications/Firefox.app/Contents/MacOS/firefox")
  (setq browse-url-chrome-program
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))

;;; Fixup native-comp-eln-load-path directory
(add-to-list 'native-comp-eln-load-path
  (expand-file-name "eln-cache" user-emacs-directory))

(add-hook 'before-init-hook #'mifi/setup-exec-path)

;;; early-init.el ends here.
