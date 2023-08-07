;;; init-helpful.el -- Provide more helpful describes.
;;
;;; Commentary:
;;; Helpful adds a lot of very helpful (get it?) information to
;;; Emacs' =describe-= command buffers.  For example, if you use
;;; =describe-function=, you will not only get the documentation about
;;; the function, you will also see the source code of the function
;;; and where it gets used in other places in the Emacs configuration.
;;; It is very useful for figuring out how things work in Emacs.
;;; 
;;; ------------------------------------------------------------------------

;;; Code:

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(provide 'init-helpful)

;;; init-helpful.el ends here.

