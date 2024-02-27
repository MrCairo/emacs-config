;;; init-neotree.el -- Displays a list of key completions.
;;
;;; Commentary:
;;; A tree plugin like NerdTree for Vim
;;; ------------------------------------------------------------------------

;;; Code:

(use-package neotree
   :ensure t
   :config
   (global-set-key [f8] 'neotree-toggle))

(provide 'init-neotree)

;;; init-neo-tree.el ends here.
