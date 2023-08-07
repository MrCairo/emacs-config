;;; init-which-key-mode.el -- Displays a list of key completions.
;;
;;; Commentary:
;;; 
;;; which-key is a useful UI panel that appears when you start
;;; pressing any key binding in Emacs to offer you all possible
;;; completions for the prefix.  For example, if you press =C-c= (hold
;;; control and press the letter =c=), a panel will appear at the
;;; bottom of the frame displaying all of the bindings under that
;;; prefix and which command they run.  This is very useful for
;;; learning the possible key bindings in the mode of your current
;;; buffer.

;;; ------------------------------------------------------------------------

;;; Code:


(use-package which-key
   :defer 0
   :diminish which-key-mode
   :custom (which-key-idle-delay 1.5)
   :config
   (which-key-mode)
   (which-key-setup-side-window-right))

(provide 'init-which-key-mode)

;;; init-which-key-mode.el ends here.
