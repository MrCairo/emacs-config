;;; --------------------------------------------------------------------------

(use-package popup)

(defun mmm-menu ()
    (interactive)
    (let ((mmm-menu-choice (popup-cascade-menu '
                               ("open-dashboard" "open-ielm"
                                   ("Themes" "next-theme" "previous-theme" "which-theme")
                                   ("Shells" "vterm" "vterm-other-window" "eshell")
                                   "set-fill-column" "set-org-fill-column"
                                   "eldoc-help" "pydoc-help"))))
        (cond
            ((equal mmm-menu-choice "open-dashboard")
                (dashboard-open))
            ((equal mmm-menu-choice "open-ielm")
                (ielm))
            ((equal mmm-menu-choice "next-theme")
                (next-theme))
            ((equal mmm-menu-choice "previous-theme")
                (previous-theme))
            ((equal mmm-menu-choice "which-theme")
                (which-theme))
            ((equal mmm-menu-choice "vterm")
                (vterm))
            ((equal mmm-menu-choice "vterm-other-window")
                (vterm-other-window))
            ((equal mmm-menu-choice "eshell")
                (eshell))
            ((equal mmm-menu-choice "set-fill-column")
                (call-interactively 'mrf/set-fill-column-interactively))
            ((equal mmm-menu-choice "set-org-fill-column")
                (call-interactively 'mrf/set-org-fill-column-interactively))
            ((equal mmm-menu-choice "eldoc-help")
                (eldoc-box-help-at-point))
            ((equal mmm-menu-choice "pydoc-help")
                (pydoc-at-point)))
        ))

;;; --------------------------------------------------------------------------

(defvar mmm-keys-minor-mode-map
    (let ((map (make-sparse-keymap)))
        (bind-keys :map map
            ("M-RET p" . pulsar-pulse-line)
            ("M-RET P" . pulsar-highlight-line)
            ("M-RET RET" . mmm-menu)
            ("M-RET d" . dashboard-open)
            ("M-RET f" . mrf/set-fill-column-interactively)
            ("M-RET F" . mrf/set-org-fill-column-interactively)
            ("M-RET i" . ielm)
            ("M-RET v" . vterm-other-window)
            ("M-RET S" . smartparens-strict-mode)
            ("M-RET |" . global-display-fill-column-indicator-mode)
            ("M-RET C-=" . next-theme)
            ("M-RET C--" . previous-theme)
            ("M-RET C-?" . which-theme)
            ("M-RET ?" . eldoc-box-help-at-point))
        (if (featurep 'python)
            (define-key map (kbd "M-RET C-.") 'pydoc-at-point))
        map)
    "mmm-keys-minor-mode keymap.")

(define-minor-mode mmm-keys-minor-mode
    "A minor mode so that my key settings override annoying major modes."
    :init-value t
    :lighter " mmm-keys")

(mmm-keys-minor-mode 1)

(which-key-add-key-based-replacements "M-RET f" "set-fill-column")
(which-key-add-key-based-replacements "M-RET" "Mitch's Menu")
(diminish 'mmm-keys-minor-mode "m3k")

(provide 'config-mmm)
;;; config-mmm.el ends here
