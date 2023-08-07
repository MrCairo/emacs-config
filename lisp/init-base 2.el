;;; init-base.el -- Things that need to be done first.
;;
;;; Commentary:
;;; ------------------------------------------------------------------------

;;; Code:

(require 'paren)
(show-paren-mode 1)

(defconst *is-a-mac* (eq system-type 'darwin))

(setq inhibit-startup-message t)  ;; Hide the startup message
(setq visible-bell t)             ;; Set up the visible bell

(save-place-mode 1)          ;; Remember where we were last editing a file.

(setq backup-inhibited t)    ;; disable backup
(setq auto-save-default nil) ;; disable auto save

(column-number-mode)
(global-display-line-numbers-mode t) ;; Line numbers appear everywhere

;; number of characters until the fill column
(setq-default fill-column 78)

;; emacs lisp tab size
(setq lisp-indent-offset '3)

;; each line of text gets one line on the screen (i.e., text will run
;; off the left instead of wrapping around onto a new line)
(setq-default truncate-lines 1)

;; truncate lines even in partial-width windows
(setq truncate-partial-width-windows 1)

(use-package evil-nerd-commenter
   :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package treemacs-all-the-icons)

(use-package rainbow-delimiters
  :config
  (rainbow-delimiters-mode))

(general-def  prog-mode-map
   "C-c ]"  'indent-region
   "C-c }"  'indent-region)

(provide 'init-base)

;;; init-base.el ends here.
