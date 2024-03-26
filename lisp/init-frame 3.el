;;; init-frame.el -- Things that need to be done first.
;;
;;; Commentary:
;;; Initializes the frame and fonts.
;;; ------------------------------------------------------------------------

;;; Code:

;; You will most likely need to adjust this font size for your system!
(defvar mrf/default-font-size 150)
(defvar mrf/default-variable-font-size 150)

;; Make frame transparency overridable
(defvar mrf/frame-transparency '(90 . 90))

;; Set frame transparency
(defun mrf/set-frame-alpha-maximized ()
   "Function to set the alpha and also maximize the frame."
   (set-frame-parameter (selected-frame) 'alpha mrf/frame-transparency)
   (set-frame-parameter (selected-frame) 'fullscreen 'maximized))

;; default window width and height
(defun mrf/custom-set-frame-size ()
   "Simple function to set the default frame width/height."
   (add-to-list 'default-frame-alist '(height . 60))
   (add-to-list 'default-frame-alist '(width . 140)))

;;; (mrf/set-frame-alpha-maximized)
(mrf/custom-set-frame-size)


;; (mrf/custom-set-frame-size)
;; (add-hook 'before-make-frame-hook 'mrf/custom-set-frame-size)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; ====================================
;; Set the font faces
;; ====================================
(set-face-attribute 'default nil
                    :font "SF Mono"
                    :height mrf/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "SF Mono"
                    :height mrf/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font "Optima"
                    :height mrf/default-variable-font-size
                    :weight 'regular)

(provide 'init-frame)

;;; init-frame.el ends here.

