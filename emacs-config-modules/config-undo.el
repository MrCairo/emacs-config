;;; --------------------------------------------------------------------------
;; A cleaner and simpler undo package.

(if enable-vundo
    (use-package vundo
        :bind (("C-x u" . vundo)
                  ("C-x r u" . vundo))
        :config
        (setq vundo-glyph-alist vundo-unicode-symbols)
        (set-face-attribute 'vundo-default nil :family "Wingdings2")))

;;; --------------------------------------------------------------------------
;; Full-featured undo-tree handling. Look to Vundo for something a little
;; simpler.

(defun mrf/undo-tree-hook ()
    (set-frame-width (selected-frame) 20))

(if (not enable-vundo)
    (use-package undo-tree
        ;; :hook (undo-tree-visualizer-mode-hook . mrf/undo-tree-hook)
        :init
        (setq undo-tree-visualizer-timestamps t
            ;; undo-tree-visualizer-diff t
            undo-tree-enable-undo-in-region t
            ;; 10X bump of the undo limits to avoid issues with premature
            ;; Emacs GC which truncages the undo history very aggresively
            undo-limit 800000
            undo-strong-limit 12000000
            undo-outer-limit 120000000)
        (global-undo-tree-mode))
    ;; This prevents the *.~undo-tree~ files from being persisted.
    (with-eval-after-load 'undo-tree
        (setq undo-tree-auto-save-history nil)))

(provide 'config-undo)
;;; config-undo.el ends here.
