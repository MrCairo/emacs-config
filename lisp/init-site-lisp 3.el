;;; init-site-list.el -- Setup a local site-lisp
;;
;;; Commentary:
;;;

;;; ------------------------------------------------------------------------

;;; Code:

(require 'cl-lib)

(let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
   (setq load-path
      (append
         (let ((load-path  (copy-sequence load-path))) ;; Shadow
            (normal-top-level-add-subdirs-to-load-path))
         load-path)))


(defun mrf/add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((default-directory parent-dir))
    (setq load-path
          (append
           (cl-remove-if-not
            #'file-directory-p
            (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
           load-path))))

;; Add both site-lisp and its immediate subdirs to `load-path'
;; (let ((site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)))
;;   (push site-lisp-dir load-path)
;;   (mrf/add-subdirs-to-load-path site-lisp-dir))


(provide 'init-site-lisp)

;;; init-site-lisp.el ends here.
