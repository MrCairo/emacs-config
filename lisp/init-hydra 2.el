;;; init-hydra.el -- Things that need to be done first.
;;
;;; Commentary:
;;; This is an example of using Hydra to design a transient key
;;; binding for quickly adjusting the scale of the text on screen.  We
;;; define a hydra that is bound to =C-s t s= and, once activated, =j=
;;; and =k= increase and decrease the text scale.  You can press any
;;; other key (or =f= specifically) to exit the transient key map.
;;;
;;; https://github.com/abo-abo/hydra
;;; ------------------------------------------------------------------------

;;; Code:
(use-package hydra
   :disabled
   :defer t
   :config
   (defhydra hydra-text-scale (:timeout 4)
      "scale text"
      ("j" text-scale-increase "in")
      ("k" text-scale-decrease "out")
      ("f" nil "finished" :exit t)))

;;  (mrf/leader-keys
;;    "ts" '(hydra-text-scale/body :which-key "scale text"))

(provide 'init-hydra)

;;; init-hydra.el ends here.
