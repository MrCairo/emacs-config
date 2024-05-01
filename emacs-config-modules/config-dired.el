;;; --------------------------------------------------------------------------

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls)))

(use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
    :commands (dired dired-jump)
    :config
    ;; Doesn't work as expected!
    ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
    (setq dired-open-extensions '(("png" . "feh")
                                     ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
    :hook (dired-mode . dired-hide-dotfiles-mode))

;;; --------------------------------------------------------------------------
;; Single Window dired - don't continually open new buffers

(defun mrf/dired-single-keymap-init ()
    "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
    (define-key dired-mode-map
        [remap dired-find-file] 'dired-single-buffer)
    (define-key dired-mode-map
        [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse)
    (define-key dired-mode-map
        [remap dired-up-directory] 'dired-single-up-directory))

(use-package dired-single
    :config
    (mrf/dired-single-keymap-init))

(provide 'config-dired)
;;; config-dired.el ends here.
