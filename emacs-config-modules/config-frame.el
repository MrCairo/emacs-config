;;; --------------------------------------------------------------------------

;; Frame (view) setup including fonts.
;; You will most likely need to adjust this font size for your system!

(setq-default mrf/small-font-size 150)
(setq-default mrf/small-variable-font-size 170)

(setq-default mrf/medium-font-size 170)
(setq-default mrf/medium-variable-font-size 190)

(setq-default mrf/large-font-size 190)
(setq-default mrf/large-variable-font-size 210)

(setq-default mrf/x-large-font-size 220)
(setq-default mrf/x-large-variable-font-size 240)

;; (setq-default custom-default-font-size mrf/medium-font-size)
(setq-default mrf/default-variable-font-size (+ custom-default-font-size 20))
;; (setq-default mrf/set-frame-maximized t)  ;; or f

;; Make frame transparency overridable
;; (setq-default mrf/frame-transparency '(90 . 90))

(setq frame-resize-pixelwise t)

;;; --------------------------------------------------------------------------

;; Functions to set the frame size

(defun mrf/frame-recenter (&optional frame)
    "Center FRAME on the screen.  FRAME can be a frame name, a terminal name,
  or a frame.  If FRAME is omitted or nil, use currently selected frame."
    (interactive)
    ;; (set-frame-size (selected-frame) 250 120)
    (unless (eq 'maximised (frame-parameter nil 'fullscreen))
        (progn
            (let ((width (nth 3 (assq 'geometry (car (display-monitor-attributes-list)))))
                     (height (nth 4 (assq 'geometry (car (display-monitor-attributes-list))))))
                (cond (( > width 3000) (mrf/update-large-display))
                    (( > width 2000) (mrf/update-built-in-display))
                    (t (mrf/set-frame-alpha-maximized)))
                )
            )
        )
    )

(defun mrf/update-large-display ()
    (modify-frame-parameters
        frame '((user-position . t)
                   (top . 0.0)
                   (left . 0.70)
                   (width . (text-pixels . 2800))
                   (height . (text-pixels . 1650))) ;; 1800
        )
    )

(defun mrf/update-built-in-display ()
    (modify-frame-parameters
        frame '((user-position . t)
                   (top . 0.0)
                   (left . 0.90)
                   (width . (text-pixels . 1800))
                   (height . (text-pixels . 1170)));; 1329
        )
    )


;; Set frame transparency
(defun mrf/set-frame-alpha-maximized ()
    "Function to set the alpha and also maximize the frame."
    ;; (set-frame-parameter (selected-frame) 'alpha mrf/frame-transparency)
    (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
    (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; default window width and height
(defun mrf/custom-set-frame-size ()
    "Simple function to set the default frame width/height."
    ;; (set-frame-parameter (selected-frame) 'alpha mrf/frame-transparency)
    (setq swidth (nth 3 (assq 'geometry (car (display-monitor-attributes-list)))))
    (setq sheight (nth 4 (assq 'geometry (car (display-monitor-attributes-list)))))

    (add-to-list 'default-frame-alist '(fullscreen . maximized))
    (mrf/frame-recenter)
    )

;;; --------------------------------------------------------------------------

;; Default fonts

(defun mrf/update-face-attribute ()
    "Set the font faces."
    ;; ====================================
    (set-face-attribute 'default nil
        ;; :font "Hack"
        ;; :font "Fira Code Retina"
        ;; :font "Menlo"
        :family default-font-family
        :height custom-default-font-size
        :weight 'medium)

    ;; Set the fixed pitch face
    (set-face-attribute 'fixed-pitch nil
        ;; :font "Lantinghei TC Demibold"
        :family mono-spaced-font-family
        ;; :font "Fira Code Retina"
        :height custom-default-font-size
        :weight 'medium)

    ;; Set the variable pitch face
    (set-face-attribute 'variable-pitch nil
        :family variable-pitch-font-family
        :height (+ custom-default-font-size 20)
        :weight 'medium))

;; (mrf/update-face-attribute)
;; (add-hook 'window-setup-hook #'mrf/frame-recenter)
;; (add-hook 'after-init-hook #'mrf/frame-recenter)

;; This is done so that the Emacs window is sized early in the init phase along with the default font size.
;; Startup works without this but it's nice to see the window expand early...
(when (display-graphic-p)
    (mrf/update-face-attribute)
    (unless (daemonp)
        (mrf/frame-recenter)))

;;; --------------------------------------------------------------------------

(defun mrf/default-font-height-change ()
    (setq-default custom-default-font-size (face-attribute 'default :height))
    (mrf/update-face-attribute)
    (mrf/frame-recenter))

(add-hook 'after-setting-font-hook 'mrf/default-font-height-change)

;;; --------------------------------------------------------------------------

(defun mrf/default-font-height-change ()
    (setq-default custom-default-font-size (face-attribute 'default :height))
    (mrf/update-face-attribute)
    (mrf/frame-recenter))

(add-hook 'after-setting-font-hook 'mrf/default-font-height-change)

;;; --------------------------------------------------------------------------
;; Frame font selection

(defvar mrf/font-size-slot 1)

(defun mrf/update-font-size ()
    (message "adjusting font size")
    (cond
        ((equal mrf/font-size-slot 3)
            (message "X-Large Font")
            (setq custom-default-font-size mrf/x-large-font-size
                mrf/default-variable-font-size (+ custom-default-font-size 20)
                mrf/font-size-slot 2)
            (mrf/update-face-attribute))
        ((equal mrf/font-size-slot 2)
            (message "Large Font")
            (setq custom-default-font-size mrf/large-font-size
                mrf/default-variable-font-size (+ custom-default-font-size 20)
                mrf/font-size-slot 1)
            (mrf/update-face-attribute))
        ((equal mrf/font-size-slot 1)
            (message "Medium Font")
            (setq custom-default-font-size mrf/medium-font-size
                mrf/default-variable-font-size (+ custom-default-font-size 20)
                mrf/font-size-slot 0)
            (mrf/update-face-attribute))
        ((equal mrf/font-size-slot 0)
            (message "Small Font")
            (setq custom-default-font-size mrf/small-font-size
                mrf/default-variable-font-size (+ custom-default-font-size 20)
                mrf/font-size-slot 3)
            (mrf/update-face-attribute))
        )
    )

;;; --------------------------------------------------------------------------
;; Some alternate keys below....

(bind-keys ("C-c 1". use-small-display-font)
    ("C-c 2". use-medium-display-font)
    ("C-c 3". use-large-display-font)
    ("C-c 4". use-x-large-display-font))

;;; --------------------------------------------------------------------------
;; Frame support functions

(defun mrf/set-frame-font (slot)
    (setq mrf/font-size-slot slot)
    (mrf/update-font-size)
    (mrf/frame-recenter)
    )

(defun use-small-display-font ()
    (interactive)
    (mrf/set-frame-font 0)
    (mrf/frame-recenter)
    )

(defun use-medium-display-font ()
    (interactive)
    (mrf/set-frame-font 1)
    (mrf/frame-recenter)
    )

(defun use-large-display-font ()
    (interactive)
    (mrf/set-frame-font 2)
    (mrf/frame-recenter)
    )

(defun use-x-large-display-font ()
    (interactive)
    (mrf/set-frame-font 3)
    (mrf/frame-recenter)
    )

(when (display-graphic-p)
    (add-hook 'after-init-hook
        (lambda ()
            (progn
                (mrf/update-face-attribute)
                (mrf/frame-recenter)))
        ))

;;; --------------------------------------------------------------------------

(use-package spacious-padding
    :hook (after-init . spacious-padding-mode)
    :custom
    (spacious-padding-widths
	'( :internal-border-width 15
	     :header-line-width 4
	     :mode-line-width 6
	     :tab-width 4
	     :right-divider-width 30
	     :scroll-bar-width 8
	     :fringe-width 8)))

;; Read the doc string of `spacious-padding-subtle-mode-line' as it
;; is very flexible and provides several examples.
;; (setq spacious-padding-subtle-mode-line
;;       `( :mode-line-active 'default
;;          :mode-line-inactive vertical-border))

(provide 'config-frame)
;;; config-frame.el ends here.
