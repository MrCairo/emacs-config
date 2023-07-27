;;; init-ivy.el -- Completion package.
;;
;;; Commentary:
;;; Ivy is a completion framework, similar to *Helm*.  When downloading
;;; Ivy, it comes with Counsel and Swipee.  Ivy doesn't try to do too
;;; many things, instead it provides an interface to list, search,
;;; filter and perform actions on a collection of "things".  These
;;; "things" can range from strings to buffers, Ivy doesn't really
;;; care.  It just provides a way for the user to interact with this
;;; collection.
;;; ------------------------------------------------------------------------

;;; Code:

(require 'swiper)
;; (ido-mode) ;; Activate Interactive DO Things package for completion

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
;;         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :custom     (ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;; (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package ivy-yasnippet)

(provide 'init-ivy)

;;; init-ivy.el ends here.

