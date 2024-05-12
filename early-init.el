;;; --------------------------------------------------------------------------

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning

(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

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

;; (defun add-site-lisp-to-load-path (parent-dir)
;;    "Add every non-hidden subdir of PARENT-DIR to `load-path'."
;;    (use-package cl-lib)
;;    (let ((default-directory parent-dir))
;; 	(setq load-path
;; 	   (append
;; 	      (cl-remove-if-not
;; 		 #'file-directory-p
;; 		 (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
;; 	      load-path))))

;; ;; Add both site-lisp and its immediate subdirs to `load-path'
;; (when (file-directory-p (expand-file-name "site-lisp/" user-emacs-directory))
;;    (let ((site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory)))
;; 	(push site-lisp-dir load-path)
;; 	(add-site-lisp-to-load-path site-lisp-dir)))

(setq use-package-compute-statistics t
   use-package-verbose t
   use-package-always-ensure nil
   use-package-always-demand nil
   use-package-always-defer nil)

(add-hook 'emacs-startup-hook
   (lambda ()
      (setq startup-time-message
	 (format "Emacs read in %.2f seconds with %d garbage collections."
	    (float-time (time-subtract after-init-time before-init-time))
	    gcs-done))
      (message startup-time-message)))

(use-package gcmh
       :diminish gcmh-mode
       :config
       (setq gcmh-idle-delay 5
	   gcmh-high-cons-threshold (* 16 1024 1024))	 ; 16mb
       (gcmh-mode 1))

(add-hook 'emacs-startup-hook
       (lambda ()
	   (setq gc-cons-percentage 0.1))) ;; Default value for `gc-cons-percentage'

;;; early-init.el ends here.
