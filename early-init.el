;;; early-init.el -*- flycheck-disabled-checkers: (emacs-lisp); lexical-binding: nil -*-
;;;
;;; Commentary:

;; Settings/Packages that need to be used early in the initialization process
;; of the Emacs startup. This file is executed before init.el.
;;
;; DO NOT MODIFY this file directly as changes will be overwritten.

;;; Code:

;;; ##########################################################################

(setq gc-cons-threshold 80000000) ;; original value * 100
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
  '(( "gnu-elpa" . "https://elpa.gnu.org/packages/")
     ( "nongnu" . "https://elpa.nongnu.org/nongnu/")
     ( "gnu-dev" . "https://elpa.gnu.org/devel/")
     ( "melpa" . "https://melpa.org/packages/")
     ( "org" . "https://orgmode.org/elpa/")
     ( "melpa-stable" . "https://stable.melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
  '(
     ( "org" . 99 )
     ( "gnu-elpa" . 50 )
     ( "melpa-stable" . 40 )
     ( "melpa" . 30 )
     ( "gnu-dev" . 20 )
     ( "nongnu" . 10)
     ))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ;; w/o this Emacs freezes when refreshing ELPA

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
;; (setq gc-cons-threshold (* 128 1024 1024))
;; (setq gc-cons-percentage 0.3)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-percentage 0.1) ;; Default value for `gc-cons-percentage'
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

(add-hook 'before-init-hook #'mifi/setup-exec-path)

;;; early-init.el ends here.
