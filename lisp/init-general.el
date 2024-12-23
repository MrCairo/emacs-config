;;; init-general.el --- General Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; ##########################################################################

(defun mifi/after-which-key ()
  (interactive)
  (which-key-mode 1)
  (add-to-list 'savehist-additional-variables 'which-key-side-window-location)
  (which-key-add-key-based-replacements
    "M-RET |" "display-fill-column"
    "M-RET ?" "help-at-point")
  (mmm-keys-minor-mode 1)
  (when (featurep 'prog-mode)
    (which-key-add-key-based-replacements
      "C-c g r" "find-symbol-reference"
      "C-c g o" "find-defitions-other-window"
      "C-c g g" "find-defitions"
      "C-c g ?" "eldoc-definition"))
  (mifi/set-recenter-keys))

(use-package which-key
  ;; :ensure (:wait t)
  :demand t
  :commands which-key-mode
  :delight which-key-mode
  :custom
  (which-key-popup-type 'side-window)
  (which-key-preserve-window-configuration t)
  (which-key-idle-delay 1,0)
  (which-key-prefix-prefix "✪ ")
  ;; (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-min-display-lines 3)
  :config
  (add-hook 'after-init-hook #'mifi/after-which-key))

;;; ##########################################################################

(use-package f :ensure t :demand t
  :config
  (let ((epath (f-dirname
               (expand-file-name invocation-name invocation-directory))))
    (add-to-list 'exec-path (format "%s:%s/bin" epath epath))
    (mifi/setup-path-from-exec-path)))

;; mostly for OCaml
(add-to-list 'load-path (expand-file-name "." emacs-config-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "Themes" custom-docs-directory))

;;; ##########################################################################

;; Add both site-lisp and its immediate subdirs to `load-path'
(let ((site-lisp-dir (expand-file-name "site-lisp/" emacs-config-directory)))
  (when (file-directory-p site-lisp-dir)
    (push site-lisp-dir load-path)
    ;; Add every non-hidden subdir of PARENT-DIR to `load-path'.
    (let ((default-directory site-lisp-dir))
      (setq load-path
        (append
          (cl-remove-if-not
            #'file-directory-p
            (directory-files (expand-file-name site-lisp-dir) t "^[^\\.]"))
          load-path)))))

;;; ##########################################################################

(setq-default
  window-resize-pixelwise t ;; enable smooth resizing
  window-resize-pixelwise t
  frame-resize-pixelwise t
  dired-dwim-target t       ;; try to guess target directory
  use-short-answers t
  truncate-partial-width-windows 1 ;; truncate lines in partial-width windows
  backup-inhibited t        ;; disable backup (No ~ tilde files)
  auto-save-default nil     ;; disable auto save
  global-auto-revert-mode 1 ;; Refresh buffer if file has changed
  global-eldoc-mode t       ;; Enabled in all buffers
  history-length 25         ;; Reasonable buffer length
  inhibit-startup-message t ;; Hide the startup message
  inhibit-startup-screent t
  lisp-indent-offset '2     ;; emacs lisp tab size
  visible-bell t            ;; Set up the visible bell
  truncate-lines 1          ;; long lines of text do not wrap
  sentence-end-double-space nil
  fill-column 79            ;; Default line limit for fills
  ;; Triggers project for directories with any of the following files:
  global-auto-revert-non-file-buffers t
  project-vc-extra-root-markers '(".dir-locals.el"
                                   "requirements.txt"
                                   "Gemfile"
                                   "package.json"))

;; Rebind C-z/C-. to act like vim's repeat previous command ( . )
(unbind-key "C-z")
(bind-key "C-." 'repeat)
(bind-key "C-z" 'repeat-complex-command)
;; Since there used to be a supported dape mode, we force the
;; existing configuration to the only option, dap-mode since
;; dape used to be supported. This resets any previous value.
(setq-default debug-adapter 'debug-adapter-dap-mode)

;;; ##########################################################################

(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
(savehist-mode t)
(setq history-length 150)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
  '(kill-ring
     search-ring
     regexp-search-ring))

;;; ##########################################################################
;; (global-display-line-numbers-mode 1) ;; Line numbers appear everywhere
;; A cool mode to revert a window configuration
(winner-mode 1)
(save-place-mode 1)                  ;; Remember where we were last editing a file.
(column-number-mode 1)
(tool-bar-mode -1)                   ;; Hide the toolbar
(global-prettify-symbols-mode 1)     ;; Display pretty symbols (i.e. λ = lambda)
(repeat-mode 0)                      ;; Also in MmM
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "C-c C-/") 'hippie-expand)
;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
  '( try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol))

;;; ##########################################################################

(defun mifi/delight-config ()
  (interactive)
  (delight '( (abbrev-mode " Abv" abbrev)
              (anaconda-mode)
              (buffer-face-mode "Buff")
              (company-box-mode "CBox")
              (counsel-mode)
              (golden-ratio-mode " 𝜑")
              (lisp-interaction-mode " 𝝺")
              (mmm-keys-minor-mode " m3")
              (projectile-mode " ->")
              (tree-sitter-mode " ts")
            (eldoc-mode " 📖")
              (overwrite-mode " Ov" t)
              (python-mode " Py" :major)
              (rainbow-mode " 🌈")
              (emacs-lisp-mode "Elisp" :major))))

(use-package delight
  :ensure t
  :demand t ;; Force early startup for all use-package calls after this
  :config (mifi/delight-config))

;;; ##########################################################################

;; Used to highlight matching delimiters '( { [ ] } )
(use-package paren
  :ensure 1    ;; built-in
  :custom
  show-paren-delay 0.1
  show-paren-highlight-openparen t
  show-paren-when-point-inside-paren t
  show-paren-when-point-in-periphery t
  show-paren-context-when-offscreen t
  :config
  (show-paren-mode 1))

;;; ##########################################################################

(defun mifi/save-desktop-frameset ()
  (unless (or (daemonp)
            (not enable-frameset-restore)
            (not (display-graphic-p)))
    (desktop-save-mode 0)
    (desktop-save-frameset)
    (with-temp-file (expand-file-name "saved-frameset.el" user-emacs-directory)
      (insert (format
                "(setq desktop-saved-frameset %S)"
                desktop-saved-frameset)))))

(add-hook 'kill-emacs-hook 'mifi/save-desktop-frameset)

;;; ##########################################################################

(defun mifi/restore-desktop-frameset ()
  (unless (or (daemonp)
            (not enable-frameset-restore)
            (not (display-graphic-p)))
    (let
      ((file (expand-file-name "saved-frameset.el" user-emacs-directory)))
      (desktop-save-mode 0)
      (if (file-exists-p file)
        (progn
          (load file)
          (desktop-restore-frameset)
          (when (featurep 'spacious-padding)
            (when spacious-padding-mode
              (spacious-padding-mode 0)
              (spacious-padding-mode 1))))
        (use-medium-display-font t)))))

;;; ##########################################################################

(setq register-preview-delay 0) ;; Show registers ASAP
(set-register ?O (cons 'file (concat emacs-config-directory "emacs-config.org")))
(set-register ?G '(file . "~/Developer/game-dev/GB_asm"))
(set-register ?S (cons 'file (concat emacs-config-directory "org-files/important-scripts.org")))

;;; ##########################################################################
;;
;; This list is processed as a LIFO queue. This entry _should_ be made to be
;; the first so it executes last.
(add-hook 'after-init-hook
  (lambda ()
    (mifi/config-landing)
    (mifi/set-recenter-keys)))

;;; ##########################################################################
;; Allow access from emacsclient
(add-hook 'after-init-hook
  (lambda ()
    (unless (server-running-p)
      (server-start))))

(use-package server :ensure t)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

(provide 'init-general)
;;; init-general.el ends here.
