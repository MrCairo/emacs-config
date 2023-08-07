;;; init-auto-complete.el -- Provides code-completion features.
;;
;;; Commentary:
;;; Auto-Complete is an intelligent auto-completion extension for Emacs.
;;; It extends the standard Emacs completion interface and provides an
;;; environment that allows users to concentrate more on their own work.
;;; ------------------------------------------------------------------------

;;; Code:

(defvar ac-directory (unless (file-exists-p "auto-complete")
		     (make-directory "auto-complete")))
(add-to-list 'load-path ac-directory)

(require 'auto-complete)
(ac-config-default)

(global-auto-complete-mode 1)
(setq-default ac-sources '(ac-source-pycomplete
                           ac-source-yasnippet
                           ac-source-abbrev
                           ac-source-dictionary
                           ac-source-words-in-same-mode-buffers))

; hack to fix ac-sources after pycomplete.el breaks it
(add-hook 'python-mode-hook
          #'(lambda ()
             (setq ac-sources '(ac-source-pycomplete
                                ac-source-yasnippet
                                ac-source-abbrev
                                ac-source-dictionary
                                ac-source-words-in-same-mode-buffers))))

;; from http://truongtx.me/2013/01/06/config-yasnippet-and-autocomplete-on-emacs/
; set the trigger key so that it can work together with yasnippet on
; tab key, if the word exists in yasnippet, pressing tab will cause
; yasnippet to activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")


;; from http://blog.deadpansincerity.com/2011/05/setting-up-emacs-as-a-javascript-editing-environment-for-fun-and-profit/
; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

(provide 'init-auto-complete)

;;; init-auto-complete.el ends here.
