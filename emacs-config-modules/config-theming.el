;;; --------------------------------------------------------------------------

;;
;; 1. The function `mrf/load-theme-from-selector' is called from the
;;    "C-= =" Keybinding (just search for it).
;;
;; 2. Once the new theme is loaded via the `theme-selector', the previous
;;    theme is unloaded (or disabled) the function(s) defined in the
;;    `disable-theme-functions' hook are called (defined in the load-theme.el
;;    package).
;;
;; 3. The function `mrf/cycle-theme-selector' is called by the hook. This
;;    function increments the theme-selector by 1, cycling the value to 0
;;    if beyond the `theme-list' bounds.
;;
(setq-default loaded-theme (nth theme-selector theme-list))
(add-to-list 'savehist-additional-variables 'loaded-theme)
(add-to-list 'savehist-additional-variables 'custom-default-font-size)
(add-to-list 'savehist-additional-variables 'theme-selector)

;;; --------------------------------------------------------------------------

(defun mrf/cycle-theme-selector (&rest theme)
    "Cycle the `theme-selector' by 1, resetting to 0 if beyond array bounds."
    (interactive)
    (let ((step theme-cycle-step) (result 0))

        (if (not step) (setq step 1)) ;; If nil, default to step of 1
        
        (when step
            (setq result (+ step theme-selector))
            (when (< result 0)
                (setq result (- (length theme-list) 1)))
            (when (> result (- (length theme-list) 1))
                (setq result 0)))
        
        (message (format ">>> Current theme %S" theme))
        (setq-default theme-selector result)))

;; This is used to trigger the cycling of the theme-selector
;; It is called when a theme is disabled. The theme is disabled from the
;; `mrf/load-theme-from-selector' function.
(add-hook 'disable-theme-functions #'mrf/cycle-theme-selector)

;;; --------------------------------------------------------------------------

(defun mrf/load-theme-from-selector (&optional step)
    "Load the theme in `theme-list' indexed by `theme-selector'."
    (interactive)
    (if step
        (setq theme-cycle-step step)
      (setq theme-cycle-step 1))
    (when loaded-theme
        (disable-theme (intern loaded-theme)))
    (setq loaded-theme (nth theme-selector theme-list))
    (message (concat ">>> Loading theme "
                 (format "%d: %S" theme-selector loaded-theme)))
    (load-theme (intern loaded-theme) t)
    (when (equal (fboundp 'mrf/org-font-setup) t)
        (mrf/org-font-setup))
    (set-face-foreground 'line-number "SkyBlue4"))

(defun mrf/print-custom-theme-name ()
    "Print the current loaded theme from the `theme-list' on the modeline."
    (interactive)
    (message (format "Custom theme is %S" loaded-theme)))

;; Quick Helper Functions
(defun next-theme ()
    "Go to the next theme in the list."
    (interactive)
    (mrf/load-theme-from-selector 1))

(defun previous-theme ()
    "Go to the next theme in the list."
    (interactive)
    (mrf/load-theme-from-selector -1))

(defun which-theme ()
    "Go to the next theme in the list."
    (interactive)
    (mrf/print-custom-theme-name))


;; Go to NEXT theme
(global-set-key (kbd "C-c C-=") 'next-theme)
;; Go to PREVIOUS theme
(global-set-key (kbd "C-c C--") 'previous-theme)
;; Print current theme
(global-set-key (kbd "C-c C-?") 'which-theme)

;;; --------------------------------------------------------------------------

;; Normally not used but it's here so it's easy to change the block colors.
(defun mrf/customize-org-block-colors ()
    (defface org-block-begin-line
        '((t (:underline "#1D2C39" :foreground "#676E95" :background "#1D2C39")))
        "Face used for the line delimiting the begin of source blocks.")

    (defface org-block-end-line
        '((t (:overline "#1D2C39" :foreground "#676E95" :background "#1D2C39")))
        "Face used for the line delimiting the end of source blocks."))

;;; --------------------------------------------------------------------------

(add-to-list 'custom-theme-load-path (expand-file-name "Themes" custom-docs-dir))

(use-package ef-themes :ensure t)
(use-package modus-themes :ensure t)
(use-package material-theme :ensure t)
(use-package color-theme-modern :ensure t)
(use-package color-theme-sanityinc-tomorrow :ensure t)
(use-package darktooth-theme :ensure t)
(use-package zenburn-theme :ensure t)

;;; --------------------------------------------------------------------------

(defun mrf/customize-modus-theme ()
    (message "Applying modus customization")
    (setq modus-themes-common-palette-overrides
        '((bg-mode-line-active bg-blue-intense)
             (fg-mode-line-active fg-main)
             (border-mode-line-active blue-intense))))

(add-hook 'after-init-hook 'mrf/customize-modus-theme)

(defun mrf/customize-ef-theme ()
    (setq ef-themes-common-palette-overrides
        '(  (bg-mode-line bg-blue-intense)
             (fg-mode-line fg-main)
             (border-mode-line-active blue-intense))))

(add-hook 'after-init-hook 'mrf/customize-ef-theme)

;;; --------------------------------------------------------------------------

(defvar loaded-theme nil
    "The text representation of the loaded custom theme.")

(defun mrf/print-custom-theme-name ()
    (message (format "Custom theme is %S" loaded-theme)))

(bind-keys
    ("C-= =" . mrf/load-theme-from-selector)
    ("C-= ?" . mrf/print-custom-theme-name))

;;; --------------------------------------------------------------------------
;; (add-hook 'emacs-startup-hook #'(mrf/load-theme-from-selector))
;; (mrf/load-theme-from-selector)
;; For terminal mode we choose Material theme
(if (not (display-graphic-p))
    (progn
        (defun load-terminal-theme ()
            (load-theme (intern default-terminal-theme) t))
        (add-hook 'after-init-hook 'load-terminal-theme))
    (mrf/load-theme-from-selector))

(provide 'config-theming)
;;; config-theming.el ends here.
