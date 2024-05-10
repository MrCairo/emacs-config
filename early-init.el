;; -*- lexical-binding: t; -*-
;; Early initialization file for emacs
;; This file has been auto-generated from an org-mode file. DO NOT EDIT.

;; Since we're using straight, we DON'T want to use the
;; normal package.el since straight replaces this.
(setq package-enable-at-startup nil)

;; Compile warnings
;;  (setq warning-minimum-level :emergency)
(setq native-comp-async-report-warnings-errors 'silent) ;; native-comp warning
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

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
        (setq read-process-output-max (* 64 1024 1024)) ;; 1mb
        (setq process-adaptive-read-buffering nil)
        ;; warn when opening files bigger than 100MB
        (setq large-file-warning-threshold 100000000)
        ;; reduce the frequency of garbage collection by making it happen on
        ;; each 50MB of allocated data (the default is on every 0.76MB)
        ;; Garbage Collections
        (setq gc-cons-percentage 0.5)
        (setq gc-cons-threshold 50000000)))

(setq read-process-output-max (* 80 1024 1024))
(setq process-adaptive-read-buffering nil)
;; (add-to-list 'default-frame-alist '(undecorated . t))

;;; --------------------------------------------------------------------------

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
    '(
	 ( "org" . 99 )
	 ( "melpa" . 40 )
	 ( "gnu-elpa" . 30 )
	 ( "melpa-stable" . 20 )
	 ( "nongnu" . 10)
	 ))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ;; w/o this Emacs freezes when refreshing ELPA

(defun add-site-lisp-to-load-path (parent-dir)
    "Add every non-hidden subdir of PARENT-DIR to `load-path'."
    (use-package cl-lib)
    (let ((default-directory parent-dir))
  	(setq load-path
            (append
  		(cl-remove-if-not
  		    #'file-directory-p
  		    (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
  		load-path))))

;; Add both site-lisp and its immediate subdirs to `load-path'
(when (file-directory-p (expand-file-name "site-lisp/" user-emacs-directory))
    (let ((site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory)))
  	(push site-lisp-dir load-path)
  	(add-site-lisp-to-load-path site-lisp-dir)))

(setq use-package-compute-statistics t
    use-package-verbose t
    use-package-always-ensure nil
    use-package-always-demand nil
    use-package-always-defer nil)

(use-package gcmh
    :diminish gcmh-mode
    :config
    (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
    (gcmh-mode 1))

(add-hook 'emacs-startup-hook
    (lambda ()
        (setq gc-cons-percentage 0.1))) ;; Default value for `gc-cons-percentage'

(add-hook 'emacs-startup-hook
    (lambda ()
        (message "Emacs ready in %s with %d garbage collections."
            (format "%.2f seconds"
                (float-time
                    (time-subtract after-init-time before-init-time)))
            gcs-done)))

;;; early-init.el ends here.
