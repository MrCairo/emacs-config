;; Early initialization file for emacs

;; Since we're using straight, we DON'T want to use the
;; normal package.el since straight replaces this.
(setq package-enable-at-startup nil)

;;; =========================================================================
;;; startup
;;; The default is 800 kilobytes.  Measured in bytes.
;;; Process performance tuning
;;
;; ** See early-init.el where some of the allocation variables are set **
;;

;; Force the eln-cache directory to be within the user-emacs-directory
;; like (emacs.d). This is important since the user-emacs-directory could
;; be changed (as it does in my Config.org). Doing this will prevent Emacs
;; from compiling all the .eln files upon startup.
(when (boundp 'native-comp-eln-load-path)
    (startup-redirect-eln-cache (expand-file-name "eln-cache/" user-emacs-directory)))

(add-hook 'before-init-hook
    (lambda ()
	;; warn when opening files bigger than 100MB
	(setq large-file-warning-threshold 100000000)
	;; reduce the frequency of garbage collection by making it happen on
	;; each 50MB of allocated data (the default is on every 0.76MB)
	(setq gc-cons-threshold 50000000)))

(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)
;; (add-to-list 'default-frame-alist '(undecorated . t))

;;; -------------------------------------------------------------------------
;;; Package setup

;;; --------------------------------------------------------------------------

  ;;;; Packages

(setq package-vc-register-as-project nil) ; Emacs 30

(add-hook 'package-menu-mode-hook #'hl-line-mode)

(setq package-archives
    '(( "gnu-elpa" . "https://elpa.gnu.org/packages/")
	 ( "nongnu" . "https://elpa.nongnu.org/nongnu/")
	 ( "melpa" . "https://melpa.org/packages/")
	 ( "org" . "https://orgmode.org/elpa/")
	 ( "melpa-stable" . "https://stable.melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
    '(( "gnu-elpa" . 4)
	 ( "melpa-stable" . 3)
	 ( "melpa" . 2)
	 ( "nongnu" . 1)))

(defun mrf/display-startup-time ()
    "Calculate and display startup time."
    (message "Emacs loaded in %s with %d garbage collections."
	(format "%.2f seconds"
	    (float-time
		(time-subtract after-init-time before-init-time)))
	gcs-done))

(add-hook 'emacs-startup-hook #'mrf/display-startup-time)
