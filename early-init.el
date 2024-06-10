;;; early-init.el -*- flycheck-disabled-checkers: (emacs-lisp); lexical-binding: nil -*-
;;;
;;; Commentary:

;; Settings/Packages that need to be used early in the initialization process
;; of the Emacs startup. This file is executed before init.el.
;;
;; DO NOT MODIFY this file directly as changes will be overwritten.

;;; Code:

;;; ##########################################################################

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 100 1024 1024))

;; Process performance tuning

(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

(setq package-vc-register-as-project nil) ; Emacs 30
(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; This allows for a set of PROXY variables/settings to be loaded before
;; we actually begin the load.
(let
  ((file (expand-file-name "early-init-proxy.el" user-emacs-directory)))
  (if (file-exists-p file)
    (load "early-init-proxy")))

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
;;     gcmh-high-cons-threshold (* 16 1024 1024))      ; 16mb
;;   (gcmh-mode 1))

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

(defun mifi/setup-exec-path ()
  ;; A list of customized executable paths. This is just something I (mifi)
  ;; do personally rather than have another package do it for me. For the
  ;; most part, the paths are typical on a Mac and with homebrew installed.
  (interactive)
  (setq exec-path '( "/Users/strider/.cargo/bin"
                     "/Users/strider/.local/bin"
                     "/opt/homebrew/bin" "/opt/homebrew/sbin"
                     "/usr/bin" "/bin" "/usr/sbin" "/sbin"
                     "/usr/local/bin" "/opt/local/bin"
                     "/Library/Frameworks/Python.framework/Versions/Current/bin"))

  (let ((path-from-exec-path (string-join exec-path path-separator)))
    (setenv "PATH" path-from-exec-path)))

(setq browse-url-firefox-program
  "/Applications/Firefox.app/Contents/MacOS/firefox")
(setq browse-url-chrome-program
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")

(add-hook 'before-init-hook #'mifi/setup-exec-path)

;;; early-init.el ends here.
