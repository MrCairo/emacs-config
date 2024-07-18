;;; init.el -*- flycheck-disabled-checkers: (emacs-lisp); lexical-binding: nil -*-
;;;
;;; Commentary:

;; This file bootstraps the configuration which is generated from tangling an org-mode file.
;; So, DO NOT MODIFY this file directly as changes will be overwritten.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)
;;

;;; ##########################################################################

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                        :ref nil :depth 1
                        :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                        :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
        (build (expand-file-name "elpaca/" elpaca-builds-directory))
        (order (cdr elpaca-order))
        (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
      (if-let ((buffer
                 (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                ((zerop (apply #'call-process
                          `("git" nil ,buffer t "clone"
                             ,@(when-let ((depth (plist-get order :depth)))
                                 (list (format "--depth=%d" depth)
                                   "--no-single-branch"))
                             ,(plist-get order :repo) ,repo))))
                ((zerop (call-process "git" nil buffer t "checkout"
                          (or (plist-get order :ref) "--"))))
                (emacs (concat invocation-directory invocation-name))
                ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                          "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                ((require 'elpaca))
                ((elpaca-generate-autoloads "elpaca" repo)))
        (progn (message "%s" (buffer-string)) (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca elpaca-use-package
  (elpaca-use-package-mode 1)
  (setq use-package-always-ensure t))

;;; ##########################################################################
;;; Define my customization groups

(defgroup mifi-custom nil
  "M.R. Fisher's configuration section."
  :group 'Local)

(defgroup mifi-custom-toggles nil
  "A set of toggles that enable or disable specific packages or behaviors."
  :group 'mifi-custom)

(defgroup mifi-custom-features nil
  "Customization from a selection of a curated list of features and handlers."
  :group 'mifi-custom)

(defgroup mifi-custom-fonts nil
  "Customization of fonts and font sizes."
  :group 'mifi-custom)

(defgroup mifi-custom-theming nil
  "Custom theming list and list index values."
  :group 'mifi-custom)

;;; ##########################################################################

(defcustom custom-docs-dir "~/Documents/Emacs-Related"
  "A directory used to store documents and customized data."
  :type 'string
  :group 'mifi-custom)

(defcustom working-files-directory
  (expand-file-name "emacs-working-files" custom-docs-dir)
  "The directory where to store Emacs working files."
  :type 'string
  :group 'mifi-custom)

(defcustom custom-org-fill-column 120
  "The fill column width for Org mode text.
    Note that the text is also centered on the screen so that should
    be taken into consideration when providing a width."
  :type 'natnum
  :group 'mifi-custom)

;;; ##########################################################################
;;; Feature Toggles

(defcustom enable-gb-dev nil
  "If set to t, the z80-mode and other GameBoy related packages
    will be enabled."
  :type 'boolean
  :group 'mifi-custom-toggles)

(defcustom enable-ts nil
  "Set to t to enable TypeScript handling."
  :type 'boolean
  :group 'mifi-custom-toggles)

(defcustom enable-centaur-tabs nil
  "Set to t to enable `centaur-tabs' which uses tabs to represent open buffer."
  :type 'boolean
  :group 'mifi-custom-toggles)

(defcustom enable-neotree nil
  "Set to t to enable the `neotree' package."
  :type 'boolean
  :group 'mifi-custom-toggles)

(defcustom enable-golden-ratio nil
  "Set to t to enable `golden-ratio-mode' which resizes the active buffer
   window to the dimensions of a golden-rectangle"
  :type 'boolean
  :group 'mifi-custom-toggles)

(defcustom enable-org-fill-column-centering nil
  "Set to t to center the visual-fill column of the Org display."
  :type 'boolean
  :group 'mifi-custom-toggles)

(defcustom enable-embark nil
  "Set to t to enable the Embark package."
  :type 'boolean
  :group 'mifi-custom-toggles)

(defcustom enable-thesaurus t
  "When set to t, enables the Merriam-Webster Thesaurus."
  :type 'boolean
  :group 'mifi-custom-toggles)

;; Keep as defvar until the frameset save/restore process works better.
(defcustom enable-frameset-restore t
  "Set to t to enable restoring the last Emacs window size and position
   upon startup."
  :type 'boolean
  :group 'mifi-custom-toggles)

;;; ##########################################################################

(defcustom default-landing-mode 'landing-mode-scratch
  "Select which landing screen to end up on once Emacs has finished
launching.

Dashboard provides an overview of items and tasks such as recent files,
agendas, projects, and bookmarks. The Dashboard appears in the *dashboard*
buffer and can also be opened using \"C-c d\" or \"M-RET d\" from anywhere
with the MmM mode enabled.

Scratch is the standard *scratch* buffer that Emacs provides but has a slightly
different startup message. It continues to be a place to write things or test
out Lisp expressions.

IELM (Inferior Emacs Lisp Mode) is a more interactive Lisp environment over the
*scratch* buffer.

eshell is the Emacs shell environment that is part terminal and part Lisp
interpreter.
"
  :type '(radio
           (const :tag "Dashboard" landing-mode-dashboard)
           (const :tag "*scratch*" landing-mode-scratch)
           (const :tag "IELM" landing-mode-ielm)
	   (const :tag "eshell" landing-mode-eshell))
  :group 'mifi-custom-features)

(defcustom undo-handler 'undo-handler-vundo
  "Select the undo handler to use.

Vundo is a minimalistic undo handler that provides a simple, graphical undo
horizontal tree.

Undo-tree is a very mature and full featured undo handler. It also has the
capability to persist undo history across Emacs sessions.

Finally, the standard undo handler can also be chosen."
  :type '(radio
           (const :tag "Vundo (default)" undo-handler-vundo)
           (const :tag "Undo-tree" undo-handler-undo-tree)
           (const :tag "Built-in" undo-handler-built-in))
  :group 'mifi-custom-features)

(defcustom completion-handler 'comphand-vertico
  "Select the default minibuffer completion handler.

Vertico provides a performant and minimalistic minibuffer vertical completion
UI based on the default completion system. Corfu provides a
completion-at-point feature in main buffers. Cape provides Corfu with
additional completion-at-point backends to use.

Ivy is a generic completion mechanism for Emacs. While it operates similarly to
other completion schemes such as icomplete-mode, Ivy aims to be more efficient,
smaller, simpler, and smoother to use yet highly customizable.  The Ivy package
also includes Counsel. Counsel provides completion versions of common Emacs
commands that are customised to make the best use of Ivy.  Swiper is an
alternative to isearch that uses Ivy to show an overview of all matches."
  :type '(radio
           (const :tag "Vertico, Corfu, Cape, Consult completion system." comphand-vertico)
           (const :tag "Ivy, Counsel, Swiper completion systems" comphand-ivy-counsel)
           (const :tag "Built-in Ido" comphand-built-in))
  :group 'mifi-custom-features)

(defcustom debug-adapter 'debug-adapter-dape
  "Select the debug adapter to use for debugging applications.  dap-mode is an
Emacs client/library for Debug Adapter Protocol is a wire protocol for
communication between client and Debug Server. It’s similar to the LSP but
provides integration with debug server.

dape (Debug Adapter Protocol for Emacs) is similar to dap-mode but is
implemented entirely in Emacs Lisp. There are no other external dependencies
with DAPE. DAPE supports most popular languages, however, not as many as
dap-mode."
  :type '(radio
           (const :tag "Debug Adapter Protocol (DAP)" debug-adapter-dap-mode)
           (const :tag "Debug Adapter Protocol for Emacs (DAPE)" debug-adapter-dape))
  :group 'mifi-custom-features)

(defcustom custom-ide 'custom-ide-eglot
  "Select which IDE will be used for Python development.

Elpy is an Emacs package to bring powerful Python editing to Emacs. It
combines and configures a number of other packages, both written in Emacs
Lisp as well as Python. Elpy is fully documented at
https://elpy.readthedocs.io/en/latest/index.html.

Eglot/LSP Eglot is the Emacs client for the Language Server Protocol
(LSP). Eglot provides infrastructure and a set of commands for enriching the
source code editing capabilities of Emacs via LSP. Eglot itself is
completely language-agnostic, but it can support any programming language
for which there is a language server and an Emacs major mode.

Anaconda-mode is another IDE for Python very much like Elpy. It is not as
configurable but has a host of great feaures that just work."
  :type '(radio
           (const :tag "Elpy: Emacs Lisp Python Environment" custom-ide-elpy)
           (const :tag "Emacs Polyglot (Eglot)" custom-ide-eglot)
           (const :tag "Language Server Protocol (LSP)" custom-ide-lsp)
           (const :tag "LSP Bridge (standalone)" custom-ide-lsp-bridge)
           (const :tag "Python Anaconda-mode for Emacs" custom-ide-anaconda))
  :group 'mifi-custom-features)

(defcustom custom-project-handler 'custom-project-project
  "Select which project handler to use."
  :type '(radio (const :tag "Projectile" custom-project-projectile)
           (const :tag "Built-in project" custom-project-project))
  :group 'mifi-custom-features)

;;; ##########################################################################
;;; Theming related

(defcustom theme-list '( "palenight-deeper-blue"
                         "ef-symbiosis"
                         "ef-maris-light"
                         "ef-maris-dark"
                         "ef-kassio"
                         "ef-bio"
                         "ef-dream"
                         "ef-deuteranopia-dark"
                         "sanityinc-tomorrow-bright"
                         "ef-melissa-dark"
                         "darktooth-dark"
                         "material"
                         "tron-legacy")

  "My personal list of themes to cycle through indexed by `theme-selector'.
If additional themes are added, they must be previously installed."
  :group 'mifi-custom-theming
  :type '(repeat string))

(defcustom default-terminal-theme "sanityinc-tomorrow-bright"
  "The default theme used for a terminal invocation of Emacs."
  :group 'mifi-custom-theming
  :type 'string)

(defcustom theme-selector 0
  "The index into the list of custom themes."
  :group 'mifi-custom-theming
  :type 'natnum)

;;; Font related
(defcustom default-font-family "Andale Mono"
  "The font family used as the default font."
  :type 'string
  :group 'mifi-custom-fonts)

(defcustom mono-spaced-font-family "Andale Mono"
  "The font family used as the mono-spaced font."
  :type 'string
  :group 'mifi-custom-fonts)

(defcustom variable-pitch-font-family "Helvetica"
  "The font family used as the default proportional font."
  :type 'string
  :group 'mifi-custom-fonts)

(defcustom small-mono-font-size 150
  "The small font size in pixels."
  :type 'natnum
  :group 'mifi-custom-fonts)

(defcustom medium-mono-font-size 170
  "The medium font size in pixels."
  :type 'natnum
  :group 'mifi-custom-fonts)

(defcustom large-mono-font-size 190
  "The large font size in pixels."
  :type 'natnum
  :group 'mifi-custom-fonts)

(defcustom x-large-mono-font-size 220
  "The extra-large font size in pixels."
  :type 'natnum
  :group 'mifi-custom-fonts)

(defcustom small-variable-font-size 170
  "The small font size in pixels."
  :type 'natnum
  :group 'mifi-custom-fonts)

(defcustom medium-variable-font-size 190
  "The small font size in pixels."
  :type 'natnum
  :group 'mifi-custom-fonts)

(defcustom large-variable-font-size 210
  "The small font size in pixels."
  :type 'natnum
  :group 'mifi-custom-fonts)

(defcustom x-large-variable-font-size 240
  "The small font size in pixels."
  :type 'natnum
  :group 'mifi-custom-fonts)

(defcustom custom-default-font-size 170
  "A place to store the most current (face-attribute 'default :height).  This
is specifically for the mono-spaced and default font. The variable type-face
font size is computed + 20 of this value."
  :type 'natnum
  :group 'mifi-custom-fonts)

(defvar custom-default-mono-font-size 170
  "Storage for the current mono-spaced font height.")

(defvar theme-did-load nil
  "Set to true if the last Theme was loaded.")

;;; ##########################################################################

(defun mifi/validate-variable-pitch-font ()
  (let* ((variable-pitch-font
           (cond
             ((x-list-fonts variable-pitch-font-family) variable-pitch-font-family)
             ((x-list-fonts "SF Pro")           "SF Pro")
             ((x-list-fonts "DejaVu Sans")      "DejaVu Sans")
             ((x-list-fonts "Ubuntu")           "Ubuntu")
             ((x-list-fonts "Helvetica")        "Helvetica")
             ((x-list-fonts "Source Sans Pro")  "Source Sans Pro")
             ((x-list-fonts "Lucida Grande")    "Lucida Grande")
             ((x-list-fonts "Verdana")          "Verdana")
             ((x-family-fonts "Sans Serif")     "Sans Serif")
             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro.")))))
    (if variable-pitch-font
      (when (not (equal variable-pitch-font variable-pitch-font-family))
        (setq variable-pitch-font-family variable-pitch-font))
      (message "---- Can't find a variable-pitch font to use.")))

  (message (format ">>> variable-pitch font is %s" variable-pitch-font-family)))

;;; ##########################################################################

(defun mifi/validate-monospace-font ()
  (let* ((monospace-font
           (cond
             ((x-list-fonts mono-spaced-font-family) mono-spaced-font-family)
             ((x-list-fonts "Fira Code Retina")  "Fira Code Retina")
             ((x-list-fonts "Fira Code")         "Fira Code")
             ((x-list-fonts "Source Code Pro")   "Source Code Pro")
             ((x-list-fonts "Ubuntu Monospaced") "Ubuntu Monospaced")
             ((x-family-fonts "Monospaced")      "Monospaced")
             (nil (warn "Cannot find a monospaced Font.  Install Source Code Pro.")))))
    (if monospace-font
      (when (not (equal monospace-font variable-pitch-font-family))
        (setq mono-spaced-font-family monospace-font)
        (setq default-font-family monospace-font))
      (message "---- Can't find a monospace font to use.")))

  (message (format ">>> monospace font is %s" mono-spaced-font-family)))

;;; ##########################################################################
;;; Set a variable that represents the actual emacs configuration directory.
;;; This is being done so that the user-emacs-directory which normally points
;;; to the .emacs.d directory can be re-assigned so that customized files don't
;;; pollute the configuration directory. This is where things like YASnippet
;;; snippets are saved and also additional color themese are stored.

(defvar emacs-config-directory user-emacs-directory)

;;; Different emacs configuration installs with have their own configuration
;;; directory.
(make-directory working-files-directory t)

;;; user-emacs-directory always ends in a "/" so we need to make the
;;; working-files-directory act the same since it becomes the new
;;; user-emacs-directory. So, add a "/" if there isn't one already.
(unless (string-suffix-p "/" working-files-directory)
  (setq working-files-directory (concat working-files-directory "/")))

;;; Point the user-emacs-directory to the new working directory
(setq user-emacs-directory working-files-directory)

;;; Put any emacs cusomized variables in a special file
(setq custom-file (expand-file-name "customized-vars.el" user-emacs-directory))

(unless (file-exists-p custom-file) ;; create custom file if it doesn't exists
  (write-region "" nil custom-file))
(load custom-file 'noerror 'nomessage)

;; ensure that the loaded font values are supported by this OS. If not, try
;; to correct them.
(mifi/validate-variable-pitch-font)
(mifi/validate-monospace-font)

;;; ##########################################################################

(add-to-list 'load-path (expand-file-name "lisp" emacs-config-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "Themes" custom-docs-dir))

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

;;; ##########################################################################
(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
(savehist-mode t)
(setq history-length t)
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

;;; ##########################################################################

;; Used to highlight matching delimiters '( { [ ] } )
(use-package paren
  :ensure nil    ;; built-in
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
  (unless (daemonp)
    (desktop-save-mode 0)
    (desktop-save-frameset)
    (with-temp-file (expand-file-name "saved-frameset.el" user-emacs-directory)
      (insert (format
                "(setq desktop-saved-frameset %S)"
                desktop-saved-frameset)))))

(add-hook 'kill-emacs-hook 'mifi/save-desktop-frameset)

;;; ##########################################################################

(defun mifi/restore-desktop-frameset ()
  (unless (and (daemonp) (not enable-frameset-<restore))
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

(setq register-preview-delay 0) ;; Show registers ASAP
(set-register ?O (cons 'file (concat emacs-config-directory "emacs-config.org")))
(set-register ?G '(file . "~/Developer/game-dev/GB_asm"))
(set-register ?S (cons 'file (concat emacs-config-directory "org-files/important-scripts.org")))

;;; ##########################################################################
;; Allow access from emacsclient
(add-hook 'elpaca-after-init-hook
  (lambda ()
    (use-package server :ensure nil)
    (unless (server-running-p)
      (server-start))))

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

;;; ##########################################################################

(use-package helpful
  :ensure t
  ;; :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :config
  (bind-keys
    ([remap describe-command] . helpful-command)
    ([remap describe-key] . helpful-key)))

(defun mifi/set-delight ()
  (interactive)
  (delight '( (abbrev-mode " Abv" abbrev)
	      (anaconda-mode)
	      (buffer-face-mode "Buff")
	      (company-box-mode)
	      (company-mode)
	      (counsel-mode)
	      (golden-ratio-mode " 𝜑")
	      (lisp-interaction-mode " iLisp")
	      (mmm-keys-minor-mode " m3")
	      (projectile-mode " ->")
	      (tree-sitter-mode " ts")
              ;;(eldoc-mode nil " eldoc")
              (overwrite-mode " Ov" t)
              (python-mode " Py" :major)
              (rainbow-mode " 🌈")
              (emacs-lisp-mode "Elisp" :major))))

(use-package delight
  :ensure t
  :config
  :hook (elpaca-after-init . mifi/set-delight))

(use-package xref :ensure nil)
(use-package dumb-jump
  :after xref :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;; ##########################################################################

(defun mifi/setup-hooks-for-eldoc ()
  (interactive)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  ;; Eldoc will try to load/unload a theme which can cause issues with our
  ;; theme loading mechanism. Our theme could fail to load because of this.
  ;; So, to get our themes loading properly, load it here if not already
  ;; loaded.
  (unless theme-did-load
    (mifi/load-theme-from-selector)))

(use-package eldoc-box
  :ensure t
  :delight DocBox
  :hook (elpaca-after-init . mifi/setup-hooks-for-eldoc))

;;; ##########################################################################

(defun mifi/after-which-key ()
  (interactive)
  (which-key-mode 1)
  (which-key-setup-minibuffer)
  (mifi/define-mmm-minor-mode-map)
  (mmm-keys-minor-mode 1)
  (when (featurep 'prog-mode)
    (which-key-add-key-based-replacements "C-c g r" "find-symbol-reference")
    (which-key-add-key-based-replacements "C-c g o" "find-defitions-other-window")
    (which-key-add-key-based-replacements "C-c g g" "find-defitions")
    (which-key-add-key-based-replacements "C-c g ?" "eldoc-definition"))
  (mifi/set-recenter-keys))

(use-package which-key
  :init
  (add-hook 'emacs-startup-hook #'mifi/after-which-key)
  :ensure (:wait t) :demand t
  :commands which-key-mode
  :delight which-key-mode
  :custom
  (which-key-idle-delay 1,0)
  (which-key-prefix-prefix "✪ ")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-min-display-lines 3)
  (which-key-max-display-columns nil))

;;; ##########################################################################

(use-package hydra
  :ensure (:repo "abo-abo/hydra" :fetcher github
            :files (:defaults (:exclude "lv.el"))))

;;; ##########################################################################

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
          ("C->" . mc/mark-next-like-this)
          ("C-<" . mc/mark-previous-like-this)
          ("C-c C-<" . mc/mark-all-like-this)))

;;; ##########################################################################

(use-package anzu
  :ensure t
  :custom
  (anzu-mode-lighter "")
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000)
  (anzu-replace-threshold 50)
  (anzu-replace-to-string-separator " => ")
  :config
  (global-anzu-mode +1)
  (set-face-attribute 'anzu-mode-line nil
    :foreground "yellow" :weight 'bold)
  (define-key isearch-mode-map
    [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map
    [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))

;;; ##########################################################################

(use-package visual-fill-column
  :ensure nil
  :after org)

(use-package writeroom-mode
  :defer t
  :after visual-fill-column)

;;; ##########################################################################
;;; Default keys are C-M-= or C-M--

(use-package default-text-scale
  :ensure t
  :hook (elpaca-after-init . default-text-scale-mode))

;;; ##########################################################################

(defun mifi/set-mac-modifier-keys ()
  (interactive)
  ;; Macintosh specific configurations.
  (when *is-a-mac*
    (setq mac-command-modifier   'meta
      mac-option-modifier        'super
      mac-control-modifier       'control
      mac-right-command-modifier 'super
      mac-right-control-modifier 'hyper)))

(add-hook 'elpaca-after-init-hook #'mifi/set-mac-modifier-keys)

;;; ##########################################################################

(defun mifi/setup-global-keybindings ()
  (interactive)
  (bind-key "C-c ]" 'indent-region prog-mode-map)
  (bind-key "C-c }" 'indent-region prog-mode-map)
  (bind-key "C-x C-j" 'dired-jump)

  (use-package evil-nerd-commenter
    :bind ("M-/" . evilnc-comment-or-uncomment-lines))

  ;;
  ;; A little better than just the typical "C-x o"
  ;; windmove is a built-in Emacs package.
  ;;
  (global-set-key (kbd "C-c <left>")  'windmove-left)
  (global-set-key (kbd "C-c <right>") 'windmove-right)
  (global-set-key (kbd "C-c <up>")    'windmove-up)
  (global-set-key (kbd "C-c <down>")  'windmove-down)

  ;;
  ;; Ctl-mouse to adjust/scale fonts will be disabled.
  ;; I personally like this since it was all to easy to accidentally
  ;; change the size of the font.
  ;;
  (global-unset-key (kbd "C-<mouse-4>"))
  (global-unset-key (kbd "C-<mouse-5>"))
  (global-unset-key (kbd "C-<wheel-down>"))
  (global-unset-key (kbd "C-<wheel-up>")))

(add-hook 'elpaca-after-init-hook #'mifi/setup-global-keybindings)

;;; ##########################################################################
;;; Automatic Package Updates

(use-package auto-package-update
  ;; :ensure (:fetcher github :repo "rranelli/auto-package-update.el")
  :defer t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;;; ##########################################################################
;; YASnippets

(use-package yasnippet
  :bind (:map yas-minor-mode-map
          ("<C-'>" . yas-expand))
  :config
  (setq yas-global-mode t)
  (setq yas-minor-mode t)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (add-to-list #'yas-snippet-dirs (expand-file-name "Snippets" custom-docs-dir))
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)
  (setq yas-prompt-functions '(yas-ido-prompt))
  (defun help/yas-after-exit-snippet-hook-fn ()
    (prettify-symbols-mode))
  (add-hook 'yas-after-exit-snippet-hook #'help/yas-after-exit-snippet-hook-fn))

;;; ##########################################################################

(use-package yasnippet-snippets
  :after yasnippet)

;;; ##########################################################################

(use-package all-the-icons
  :ensure t
  :when (display-graphic-p))

;;; ##########################################################################

(use-package ace-window
  ;;:ensure (:repo "abo-abo/ace-window" :fetcher github)
  :bind ("M-o" . ace-window))

;;; ##########################################################################
;;; Window Number

(use-package winum
  :ensure t
  :config (winum-mode))

;;; ##########################################################################

(use-package dashboard
  :custom
  (dashboard-items '( (recents   . 12)
                      (bookmarks . 5)
                      (projects  . 5)
                      (agenda    . 5)))
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-footer-messages '("Greetings Program!"))
  (dashboard-banner-logo-title "Welcome to Emacs!")
  :commands dashboard-open
  :bind ("C-c d" . dashboard-open)
  :config
  ;; (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  ;;-or- (expand-file-name "Emacs-modern-is-sexy-v1.png" user-emacs-directory)
  (setq dashboard-startup-banner (expand-file-name "Emacs-modern-is-sexy-v1.png" user-emacs-directory))
  (dashboard-setup-startup-hook))

;;; ##########################################################################
(use-package jinx
  :ensure (:host github :repo "minad/jinx")
  ;;:hook (emacs-startup . global-jinx-mode)
  :bind (("C-c C-$" . jinx-correct)
          ("C-x C-$" . jinx-languages))
  :config
  (dolist (hook '(text-mode-hook prog-mode-hook org-mode-hook))
    (add-hook hook #'jinx-mode)))

;;; ##########################################################################
;; These are packages located in the site-lisp or lisp directories in the
;; 'emacs-config-directory'

;;; ##########################################################################

(use-package vundo
  ;;:ensure ( :host github :repo "casouri/vundo")
  :when (equal undo-handler 'undo-handler-vundo)
  :ensure t
  :commands vundo
  :bind
  ("C-x u" . vundo)
  ("C-x r u" . vundo)
  :config
  (set-face-attribute 'vundo-default nil :family "Symbola")
  (setq vundo-glyph-alist vundo-unicode-symbols))

;;; ##########################################################################

(defun mifi/undo-tree-hook ()
  (set-frame-width (selected-frame) 20))

(defun undo-tree-split-side-by-side (original-function &rest args)
  "Split undo-tree side-by-side"
  (let ((split-height-threshold nil)
         (split-width-threshold 0))
    (apply original-function args)))

;;; ##########################################################################

;;
;; Sometimes, when behind a firewall, the undo-tree package triggers elpaca
;; to queue up the Queue package which then hangs and fails. This happens
;; even if the :unless/:when option is specified in the use-package (only :disabled
;; seems to work which isn't what I want). So, we prevent the loading of the
;; page altogether unless the undo-handler is set to undo tree.
;;
(when (equal undo-handler 'undo-handler-undo-tree)
  (use-package undo-tree
    :ensure t
    :init
    (setq undo-tree-visualizer-timestamps nil
      undo-tree-visualizer-diff t
      undo-tree-enable-undo-in-region t
      ;; 10X bump of the undo limits to avoid issues with premature
      ;; Emacs GC which truncages the undo history very aggresively
      undo-limit 800000
      undo-strong-limit 12000000
      undo-outer-limit 120000000)
    :delight untree
    :config
    (global-undo-tree-mode)
    (advice-add 'undo-tree-visualize :around #'undo-tree-split-side-by-side)
    (bind-keys :map undo-tree-visualizer-mode-map
      ("RET" . undo-tree-visualizer-quit)
      ("C-g" . undo-tree-visualizer-abort))
    (setq undo-tree-auto-save-history nil)))

;;; ##########################################################################

(use-package prescient
  :ensure t)

;;; ##########################################################################

(use-package orderless
  :when (or (equal completion-handler 'comphand-vertico)
          (equal completion-handler 'comphand-ivy-counsel))
  :after (:any ivy swiper vertico counsel)
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; ##########################################################################
;;; Swiper and IVY mode

(use-package ivy
  :when (equal completion-handler 'comphand-ivy-counsel)
  :ensure t
  :bind (("C-s" . swiper)
          :map ivy-minibuffer-map
            ;;; ("TAB" . ivy-alt-done)
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
  :custom
  (enable-recursive-minibuffers t)
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  (add-to-list 'ivy-highlight-functions-alist
    '(orderless-ivy-re-builder . orderless-ivy-highlight)))

;;; ##########################################################################

(use-package ivy-rich
  :when (equal completion-handler 'comphand-ivy-counsel)
  :after ivy
  :init
  (ivy-rich-mode 1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package ivy-yasnippet
  :when (equal completion-handler 'comphand-ivy-counsel)
  :after (:any yasnippet ivy))
;; :ensure (:host github :repo "mkcms/ivy-yasnippet"))

;;; ##########################################################################

(use-package swiper
  :when (equal completion-handler 'comphand-ivy-counsel)
  :after ivy
  :ensure t)

;;; ##########################################################################

(use-package counsel
  :when (equal completion-handler 'comphand-ivy-counsel)
  :after ivy
  :defer t
  :bind ( ("C-M-j" . 'counsel-switch-buffer)
          ("M-x" . 'counsel-M-x)
          ("M-g o" . 'counsel-outline)
          ("C-x C-f" . 'counsel-find-file)
          ("C-c C-r" . 'ivy-resume)
          :map minibuffer-local-map
          ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (bind-keys
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-variable] . counsel-describe-variable))
  (when (featurep 'helpful)
    (setq counsel-describe-function-function #'helpful-callable)
    (setq counsel-describe-variable-function #'helpful-variable))
  (counsel-mode 1))

;;; ##########################################################################

(use-package ivy-prescient
  :when (equal completion-handler 'comphand-ivy-counsel)
  :after (ivy prescient)
  :custom
  (prescient-persist-mode t)
  (ivy-prescient-mode t)
  (ivy-prescient-enable-filtering t))

;;; ##########################################################################

;; Don't use lsp-bridge with company as lsp-bridge already provides the same
;; features. They actually collide.

(use-package company
  :unless (equal custom-ide 'custom-ide-lsp-bridge)
  :ensure (:wait t)
  :bind (:map company-active-map
          ("C-n". company-select-next)
          ("C-p". company-select-previous)
          ("M-<". company-select-first)
          ("M->". company-select-last)
          ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.5)
  :config
  (when (featurep 'prescient)
    (company-prescient-mode 1))
  (global-company-mode +1))

;; IMPORTANT:
;; Don't use company at all if lsp-bridge is active.
;; lsp-bridge already provides similar functionality.

;; :config
;; (add-to-list 'company-backends 'company-yasnippet))

;;; ##########################################################################

(use-package company-box
  :after company
  :delight cb
  :hook (company-mode . company-box-mode))

(use-package company-jedi
  :when  (equal custom-ide 'custom-ide-elpy)
  :after (:all python company)
  :config
  (jedi:setup)
  (defun my/company-jedi-python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/company-jedi-python-mode-hook))

(use-package company-anaconda
  :when (equal custom-ide 'custom-ide-anaconda)
  :after (:all anaconda company)
  :hook (python-mode . anaconda-mode)
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))

;;; ##########################################################################

;;;; Code Completion
(use-package corfu
  :disabled
  :when (equal completion-handler 'comphand-vertico)
  :after vertico
  ;; Optional customizations
  :custom
  (corfu-cycle t)                  ; Allows cycling through candidates
  (corfu-auto t)                   ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.8)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)       ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
          ("M-SPC"      . corfu-insert-separator)
          ("TAB"        . corfu-next)
          ([tab]        . corfu-next)
          ("S-TAB"      . corfu-previous)
          ([backtab]    . corfu-previous)
          ("S-<return>" . corfu-insert)
          ("RET"        . nil))
  :hook (prog-mode . corfu-mode)
  :config
  (corfu-prescient-mode t)
  (corfu-history-mode t)
  (corfu-popupinfo-mode) ; Popup completion info
  (add-hook 'eshell-mode-hook
    (lambda () (setq-local corfu-quit-at-boundary t
                 corfu-quit-no-match t
                 corfu-auto nil)
      (corfu-mode))))

;;; ##########################################################################
;; Add extensions
(use-package cape
  :when (equal completion-handler 'comphand-vertico)
  :after corfu
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind ( ("C-c C-p p" . completion-at-point) ;; capf
          ("C-c C-p t" . complete-tag)        ;; etags
          ("C-c C-p d" . cape-dabbrev)        ;; or dabbrev-completion
          ("C-c C-p h" . cape-history)
          ("C-c C-p f" . cape-file)
          ("C-c C-p k" . cape-keyword)
          ("C-c C-p s" . cape-elisp-symbol)
          ("C-c C-p e" . cape-elisp-block)
          ("C-c C-p a" . cape-abbrev)
          ("C-c C-p l" . cape-line)
          ("C-c C-p w" . cape-dict)
          ("C-c C-p :" . cape-emoji)
          ("C-c C-p \\" . cape-tex)
          ("C-c C-p _" . cape-tex)
          ("C-c C-p ^" . cape-tex)
          ("C-c C-p &" . cape-sgml)
          ("C-c C-p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-hook 'completion-at-point-functions #'cape-history)
  ;;(add-hook 'completion-at-point-functions #'cape-keyword)
  ;;(add-hook 'completion-at-point-functions #'cape-tex)
  ;;(add-hook 'completion-at-point-functions #'cape-sgml)
  ;;(add-hook 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-hook 'completion-at-point-functions #'cape-abbrev)
  ;;(add-hook 'completion-at-point-functions #'cape-dict)
  ;;(add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  (add-hook 'completion-at-point-functions #'cape-line)
  )

;;; ##########################################################################

(use-package corfu-prescient
  :ensure t
  :when (equal completion-handler 'comphand-vertico)
  :after corfu prescient)

;;; ##########################################################################

(use-package vertico
  :when (equal completion-handler 'comphand-vertico)
  :ensure t
  :custom
  (recentf-mode t)
  (vertico-count 12)
  (vertico-cycle nil)
  (vertico-multiform-mode 1)
  :config
  (vertico-mode)
  (when (featurep 'prescient)
    (vertico-prescient-mode 0))
  ;; :bind ("C-x C-f" . ido-find-file)
  ;; Clean up file path when typing
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)
          ;; Make sure vertico state is saved
          (minibuffer-setup . vertico-repeat-save)))

;;; ##########################################################################

(use-package marginalia
  :ensure t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left)
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode t))

;;; ##########################################################################

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

;;; ##########################################################################

(use-package consult
  :when (equal completion-handler 'comphand-vertico)
  :after vertico
  :ensure t
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap project-switch-to-buffer] . consult-project-buffer)
  ([remap bookmark-jump] . consult-bookmark)
  ([remap recentf-open] . consult-recent-file)
  ([remap yank] . nil)
  ([remap yank-pop] . consult-yank-pop)
  ([remap goto-line] . consult-goto-line)
  ("M-g m" . consult-mark)
  ("M-g M" . consult-global-mark)
  ("M-g o" . consult-outline)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ("M-s l" . consult-line)
  ("M-s p" . consult-preview)  
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ("M-s r" . consult-ripgrep)
  ("M-s f" . consult-find)
  ("M-s F" . consult-locate)
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flymake)
  ([remap repeat-complex-command] . consult-complex-command)
  ("M-s e" . consult-isearch-history)
  ([remap isearch-edit-string] . consult-isearch-history)
  ([remap next-matching-history-element] . consult-history)
  ([remap previous-matching-history-element] . consult-history)
  ([remap Info-search] . consult-info)
  :custom
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref)
  :config
  (setq consult-buffer-sources
    '(consult--source-hidden-buffer 
       consult--source-buffer
       (:name "Ephemeral" :state consult--buffer-state
         :narrow 109 :category buffer
         :items ("*Messages*"  "*scratch*" "*vterm*"
                  "*Async-native-compile-log*" "*dashboard*"))
       consult--source-modified-buffer
       consult--source-recent-file)))

;;; ##########################################################################

(use-package vertico-prescient
  :when (equal completion-handler 'comphand-vertico)
  :ensure t
  :after vertico prescient)

;;; ##########################################################################

(use-package vertico-posframe
  :when (equal completion-handler 'comphand-vertico)
  :ensure t
  :after vertico
  :custom
  (setq vertico-multiform-commands
    '((consult-line
        posframe
        (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
        (vertico-posframe-border-width . 10)
        ;; NOTE: This is useful when emacs is used in both in X and
        ;; terminal, for posframe do not work well in terminal, so
        ;; vertico-buffer-mode will be used as fallback at the
        ;; moment.
        (vertico-posframe-fallback-mode . vertico-buffer-mode))
       (t posframe)))
  (vertico-multiform-mode 1)
  (setq vertico-posframe-parameters
    '((left-fringe . 8)
       (right-fringe . 8))))

;;; ##########################################################################

;; This has to be evaluated at the end of the init since it's possible that the
;; completion-handler variable will not yet be defined at this point in the
;; init phase usi\ng elpaca.

(add-hook 'elpaca-after-init-hook
  (lambda ()
    (use-package ido
      :when (equal completion-handler 'comphand-built-in)
      :ensure nil
      :config
      (ido-everywhere t))))

;;; ##########################################################################

(use-package embark
  :when enable-embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
    ("C-;" . embark-dwim)        ;; good alternative: M-.
    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
    '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
       nil
       (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :when (equal completion-handler 'comphand-vertico)
  :defer t
  ;;:ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; ##########################################################################

(use-package term+
  ;;:ensure (:repo "tarao/term-plus-el" :fetcher github)
  :commands term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())          ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;;; ##########################################################################

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

;;; ##########################################################################

(use-package vterm
  ;;:ensure (:fetcher github :repo "akermu/emacs-libvterm")
  :commands vterm
  :config
  (setq vterm-environment ("PS1=\\u@\\h:\\w \n$"))
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh")                        ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

;;; ##########################################################################

(defun mifi/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-history-size   10000
    eshell-buffer-maximum-lines 10000
    eshell-hist-ignoredups t
    eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :ensure nil
  :defer t
  :hook (eshell-first-time-mode . mifi/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))
  (eshell-git-prompt-use-theme 'powerline))

;;; ##########################################################################

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq-default insert-directory-program gls
              dired-use-ls-dired t
              ;; Needed to fix an issue on Mac which causes dired to fail
              dired-listing-switches "-al --group-directories-first")))

(use-package all-the-icons-dired
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                 ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :after dired-mode
  :hook (dired-mode . dired-hide-dotfiles-mode))

;;; ##########################################################################
;; Single Window dired - don't continually open new buffers

(defun mifi/dired-single-keymap-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  (define-key dired-mode-map
    [remap dired-find-file] 'dired-single-buffer)
  (define-key dired-mode-map
    [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse)
  (define-key dired-mode-map
    [remap dired-up-directory] 'dired-single-up-directory))

(use-package dired-single
  :after dired
  :ensure t
  :config
  (mifi/dired-single-keymap-init))

;;; ##########################################################################
;;; Treemacs

(use-package treemacs
  :after (:all winum ace-window)
  :bind (:map global-map
          ("M-0"         . treemacs-select-window)
          ("C-x t 1"   . treemacs-delete-other-windows)
          ("C-x t t"   . treemacs)
          ("C-x t d"   . treemacs-select-directory)
          ("C-x t B"   . treemacs-bookmark)
          ("C-x t C-t" . treemacs-find-file)
          ("C-x t M-t" . treemacs-find-tag))
  :config
  (setq treemacs-collapse-dirs                  (if treemacs-python-executable 3 0)
    treemacs-deferred-git-apply-delay  0.5
    treemacs-directory-name-transformer        #'identity
    treemacs-display-in-side-window            t
    treemacs-eldoc-display                     'simple
    treemacs-file-event-delay          2000
    treemacs-file-extension-regex              treemacs-last-period-regex-value
    treemacs-file-follow-delay                 0.2
    treemacs-file-name-transformer             #'identity
    treemacs-follow-after-init                 t
    treemacs-expand-after-init                 t
    treemacs-find-workspace-method             'find-for-file-or-pick-first
    treemacs-git-command-pipe          ""
    treemacs-goto-tag-strategy                 'refetch-index
    treemacs-header-scroll-indicators  '(nil . "^^^^^^")
    treemacs-hide-dot-git-directory            t
    treemacs-indentation                       2
    treemacs-indentation-string                " "
    treemacs-is-never-other-window             nil
    treemacs-max-git-entries           5000
    treemacs-missing-project-action            'ask
    treemacs-move-forward-on-expand            nil
    treemacs-no-png-images                     nil
    treemacs-no-delete-other-windows   t
    treemacs-project-follow-cleanup            nil
    treemacs-persist-file                      (expand-file-name
                                                 ".cache/treemacs-persist"
                                                 user-emacs-directory)
    treemacs-position                  'left
    treemacs-read-string-input                 'from-child-frame
    treemacs-recenter-distance                 0.1
    treemacs-recenter-after-file-follow        nil
    treemacs-recenter-after-tag-follow         nil
    treemacs-recenter-after-project-jump       'always
    treemacs-recenter-after-project-expand     'on-distance
    treemacs-litter-directories                '("/node_modules"
                                                  "/.venv"
                                                  "/.cask"
                                                  "/__pycache__")
    treemacs-project-follow-into-home  nil
    treemacs-show-cursor                       nil
    treemacs-show-hidden-files                 t
    treemacs-silent-filewatch          nil
    treemacs-silent-refresh                    nil
    treemacs-sorting                   'alphabetic-asc
    treemacs-select-when-already-in-treemacs 'move-back
    treemacs-space-between-root-nodes  t
    treemacs-tag-follow-cleanup                t
    treemacs-tag-follow-delay          1.5
    treemacs-text-scale                        nil
    treemacs-user-mode-line-format             nil
    treemacs-user-header-line-format   nil
    treemacs-wide-toggle-width                 70
    treemacs-width                             38
    treemacs-width-increment           1
    treemacs-width-is-initially-locked         t
    treemacs-workspace-switch-cleanup  nil)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))
  (pcase (cons (not (null (executable-find "git")))
           (not (null treemacs-python-executable)))
    (`(t . t)
      (treemacs-git-mode 'deferred))
    (`(t . _)
      (treemacs-git-mode 'simple)))
  (treemacs-hide-gitignored-files-mode nil))

;;; ##########################################################################

(use-package treemacs-projectile
  :when (equal custom-project-handler 'custom-project-projectile)
  :after treemacs projectile)

;;; ##########################################################################

(use-package treemacs-icons-dired
  :after treemacs
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;;; ##########################################################################

;; (use-package treemacs-perspective
;;    :disabled
;;    :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;    :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  ;;:ensure (:files ("src/extra/treemacs-persp.el" "treemacs-persp-pkg.el"):host github :repo "Alexander-Miller/treemacs")
  :after (:any treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

;;; ##########################################################################

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after treemacs
  :config (treemacs-set-scope-type 'Tabs))

;;; ##########################################################################

(use-package treemacs-all-the-icons
  :after treemacs
  :if (display-graphic-p))

;;; ##########################################################################

;;
;; 1. The function `mifi/load-theme-from-selector' is called from the
;;    "C-= =" Keybinding (just search for it).
;;
;; 2. Once the new theme is loaded via the `theme-selector', the previous
;;    theme is unloaded (or disabled) the function(s) defined in the
;;    `disable-theme-functions' hook are called (defined in the load-theme.el
;;    package).
;;
;; 3. The function `mifi/cycle-theme-selector' is called by the hook. This
;;    function increments the theme-selector by 1, cycling the value to 0
;;    if beyond the `theme-list' bounds.
;;
(setq-default loaded-theme (nth theme-selector theme-list))
(add-to-list 'savehist-additional-variables 'loaded-theme)
(add-to-list 'savehist-additional-variables 'custom-default-font-size)
(add-to-list 'savehist-additional-variables 'theme-selector)
(add-to-list 'savehist-additional-variables 'custom-default-mono-font-size)

;;; ##########################################################################

(defun mifi/cycle-theme-selector (&rest theme)
  "Cycle the `theme-selector' by 1, resetting to 0 if beyond array bounds."
  (interactive)
  (when (not (eq theme-cycle-step nil))
    (let ((step theme-cycle-step) (result 0))
      (when step
        (setq result (+ step theme-selector))
        (when (< result 0)
          (setq result (- (length theme-list) 1)))
        (when (> result (- (length theme-list) 1))
          (setq result 0)))
      (setq-default theme-selector result))))

;; This is used to trigger the cycling of the theme-selector
;; It is called when a theme is disabled. The theme is disabled from the
;; `mifi/load-theme-from-selector' function.
(add-hook 'disable-theme-functions #'mifi/cycle-theme-selector)

;;; ##########################################################################

(defun mifi/reset-if-spacious-padding-mode ()
  (interactive)
  (when-let ((spm? (featurep 'spacious-padding))
            (spm-on-off (default-value 'spacious-padding-mode)))
    (spacious-padding-mode 0)
    (run-with-timer 0.2 nil
      (lambda (on-off) (spacious-padding-mode on-off)) spm-on-off)))

;;; ##########################################################################

(defun mifi/load-theme-from-selector (&optional step)
  "Load the theme in `theme-list' indexed by `theme-selector'."
  (interactive)
  ;; Save value of spacious-padding-mode
  (setq theme-cycle-step nil)
  (cond
    ((or (eq step nil) (eq step 0)) (setq theme-cycle-step 0))
    ((> step 0) (setq theme-cycle-step 1))
    ((< step 0) (setq theme-cycle-step -1)))
  (when loaded-theme
    (disable-theme (intern loaded-theme)))
  (setq loaded-theme (nth theme-selector theme-list))
  (setq theme-did-load (load-theme (intern loaded-theme) t))
  (when (featurep 'org)
    (mifi/org-font-setup))
  (mifi/reset-if-spacious-padding-mode)
  (set-face-foreground 'line-number "SkyBlue4"))

;;; ##########################################################################

(defun mifi/print-custom-theme-name ()
  "Print the current loaded theme from the `theme-list' on the modeline."
  (interactive)
  (message (format "Custom theme is %S" loaded-theme)))

;; Quick Helper Functions
(defun next-theme ()
  "Go to the next theme in the list."
  (interactive)
  (mifi/load-theme-from-selector 1))

(defun previous-theme ()
  "Go to the next theme in the list."
  (interactive)
  (mifi/load-theme-from-selector -1))

(defun which-theme ()
  "Go to the next theme in the list."
  (interactive)
  (mifi/print-custom-theme-name))

;; Go to NEXT theme
(global-set-key (kbd "C-c C-=") 'next-theme)
;; Go to PREVIOUS theme
(global-set-key (kbd "C-c C--") 'previous-theme)
;; Print current theme
(global-set-key (kbd "C-c C-?") 'which-theme)

;;; ##########################################################################

(defun mifi/org-theme-override-values ()
  (defface org-block-begin-line
    '((t (:underline "#1D2C39" :foreground "SlateGray" :background "#1D2C39")))
    "Face used for the line delimiting the begin of source blocks.")

  (defface org-block
    '((t (:background "#242635" :extend t :font "Avenir Next")))
    "Face used for the source block background.")

  (defface org-block-end-line
    '((t (:overline "#1D2C39" :foreground "SlateGray" :background "#1D2C39")))
    "Face used for the line delimiting the end of source blocks.")

  (defface org-modern-horizontal-rule
    '((t (:strike-through "green" :weight bold)))
    "Face used for the Horizontal like (-----)"))

;;; ##########################################################################

(defun mifi/customize-modus-theme ()
  (when (featurep 'org)
    (mifi/org-font-setup))
  (setq modus-themes-common-palette-overrides
    '((bg-mode-line-active bg-blue-intense)
       (fg-mode-line-active fg-main)
       (border-mode-line-active blue-intense))))

(add-hook 'elpaca-after-init-hook 'mifi/customize-modus-theme)

(defun mifi/customize-ef-theme ()
  (defface ef-themes-fixed-pitch
    '((t (:background "#242635" :extend t :font "Courier New")))
    "Face used for the source block background.")
  (when (featurep 'org)
    (mifi/org-font-setup))
  (setq ef-themes-common-palette-override
    '( (bg-mode-line bg-blue-intense)
       (fg-mode-line fg-main)
       (border-mode-line-active blue-intense))))
;;(add-hook 'org-load-hook 'mifi/customize-ef-theme)
(add-hook 'elpaca-after-init-hook 'mifi/customize-ef-theme)

;;; ##########################################################################

(add-to-list 'custom-theme-load-path (expand-file-name "Themes" custom-docs-dir))
(add-to-list 'custom-theme-load-path (expand-file-name "lisp" emacs-config-directory))

(mifi/org-theme-override-values)
(use-package tron-legacy-theme :defer t)
(use-package ef-themes :init (mifi/customize-ef-theme) :defer t)
(use-package modus-themes :init (mifi/customize-modus-theme) :defer t)
(use-package material-theme :defer t)
(use-package color-theme-modern :defer t)
(use-package color-theme-sanityinc-tomorrow :defer t)
;; Can't defer darktooth since we need the base theme to always load
(use-package darktooth-theme :ensure t)
(use-package zenburn-theme :defer t)

;;; ##########################################################################
;; (add-hook 'emacs-startup-hook #'(mifi/load-theme-from-selector))
;; (mifi/load-theme-from-selector)
;; For terminal mode we choose Material theme

(defun mifi/load-terminal-theme ()
  (load-theme (intern default-terminal-theme) t))

(if (not (display-graphic-p))
  (add-hook 'elpaca-after-init-hook 'mifi/load-terminal-theme)
  ;;else
  (progn
    (if (not elpaca-after-init-time)
      (add-hook 'elpaca-after-init-hook
        (lambda ()
          (unless theme-did-load
            (mifi/load-theme-from-selector))))
      ;; else
      (add-hook 'window-setup-hook
        (lambda ()
          (unless theme-did-load
            (mifi/load-theme-from-selector))))
      )))

;;; ##########################################################################

;; Frame (view) setup including fonts.
;; You will most likely need to adjust this font size for your system!

(setq-default mifi/small-font-size 150)
(setq-default mifi/small-mono-font-size 150)
(setq-default mifi/small-variable-font-size 170)

(setq-default mifi/medium-font-size 170)
(setq-default mifi/medium-mono-font-size 170)
(setq-default mifi/medium-variable-font-size 190)

(setq-default mifi/large-font-size 190)
(setq-default mifi/large-mono-font-size 190)
(setq-default mifi/large-variable-font-size 210)

(setq-default mifi/x-large-font-size 220)
(setq-default mifi/x-large-mono-font-size 220)
(setq-default mifi/x-large-variable-font-size 240)

;; (setq-default custom-default-font-size mifi/medium-font-size)
(setq-default mifi/default-variable-font-size (+ custom-default-font-size 20))
;; (setq-default mifi/set-frame-maximized t)  ;; or f

;; Make frame transparency overridable
;; (setq-default mifi/frame-transparency '(90 . 90))

(setq frame-resize-pixelwise t)

;;; ##########################################################################

(use-package mixed-pitch
  :defer t
  :custom
  (mixed-pitch-set-height t)
  :config
  (dolist (face '(org-date org-priority org-special-keyword org-tag))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

;;; ##########################################################################
;; Functions to set the frame size

(defun mifi/frame-recenter (&optional frame)
  "Center FRAME on the screen.  FRAME can be a frame name, a terminal name,
  or a frame.  If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  ;; (set-frame-size (selected-frame) 250 120)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (progn
      (let ((width (nth 3 (assq 'geometry (car (display-monitor-attributes-list)))))
             (height (nth 4 (assq 'geometry (car (display-monitor-attributes-list))))))
        (cond (( > width 3000) (mifi/update-large-display))
          (( > width 2000) (mifi/update-built-in-display))
          (t (mifi/set-frame-alpha-maximized)))))))

(defun mifi/update-large-display ()
  (modify-frame-parameters
    frame '((user-position . t)
             (top . 0.0)
             (left . 0.70)
             (width . (text-pixels . 2800))
             (height . (text-pixels . 1650))))) ;; 1800

(defun mifi/update-built-in-display ()
  (modify-frame-parameters
    frame '((user-position . t)
             (top . 0.0)
             (left . 0.90)
             (width . (text-pixels . 1800))
             (height . (text-pixels . 1170))))) ;; 1329

;; Set frame transparency
(defun mifi/set-frame-alpha-maximized ()
  "Function to set the alpha and also maximize the frame."
  ;; (set-frame-parameter (selected-frame) 'alpha mifi/frame-transparency)
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; default window width and height
(defun mifi/custom-set-frame-size ()
  "Simple function to set the default frame width/height."
  ;; (set-frame-parameter (selected-frame) 'alpha mifi/frame-transparency)
  (setq swidth (nth 3 (assq 'geometry (car (display-monitor-attributes-list)))))
  (setq sheight (nth 4 (assq 'geometry (car (display-monitor-attributes-list)))))

  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (unless enable-frameset-restore (mifi/frame-recenter)))

;;; ##########################################################################

;; Default fonts

(defun mifi/update-face-attribute ()
  "Set the font faces."
  (interactive)
  ;; ====================================
  (set-face-attribute 'default nil
    :family default-font-family
    :height custom-default-font-size
    :weight 'medium)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil
    :family mono-spaced-font-family
    :height custom-default-mono-font-size
    :weight 'medium)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil
    :family variable-pitch-font-family
    :height (+ custom-default-font-size 20)
    :weight 'medium))

;; This is done so that the Emacs window is sized early in the init phase along
;; with the default font size. Startup works without this but it's nice to see
;; the window expand early...

(add-hook 'elpaca-after-init-hook
  (lambda ()
    (when (display-graphic-p)
      (mifi/update-face-attribute)
      (unless (daemonp)
        (if enable-frameset-restore
          (mifi/restore-desktop-frameset)
          (mifi/frame-recenter)))
      )))

;;; ##########################################################################

(defun mifi/default-font-height-change ()
  (setq-default custom-default-font-size (face-attribute 'default :height))
  (mifi/update-face-attribute)
  (unless enable-frameset-restore (mifi/frame-recenter)))

(add-hook 'after-setting-font-hook 'mifi/default-font-height-change)

;;; ##########################################################################
;; Frame font selection

(defvar mifi/font-size-slot 1)

(defun mifi/update-font-size ()
  (cond
    ((equal mifi/font-size-slot 3)
      (setq custom-default-font-size mifi/x-large-font-size
        custom-default-mono-font-size mifi/x-large-mono-font-size
        mifi/default-variable-font-size (+ custom-default-font-size 20)
        mifi/font-size-slot 2)
      (mifi/update-face-attribute))
    ((equal mifi/font-size-slot 2)
      (setq custom-default-font-size mifi/large-font-size
        custom-default-mono-font-size mifi/large-mono-font-size
        mifi/default-variable-font-size (+ custom-default-font-size 20)
        mifi/font-size-slot 1)
      (mifi/update-face-attribute))
    ((equal mifi/font-size-slot 1)
      (setq custom-default-font-size mifi/medium-font-size
        custom-default-mono-font-size mifi/medium-mono-font-size
        mifi/default-variable-font-size (+ custom-default-font-size 20)
        mifi/font-size-slot 0)
      (mifi/update-face-attribute))
    ((equal mifi/font-size-slot 0)
      (setq custom-default-font-size mifi/small-font-size
        custom-default-mono-font-size mifi/small-mono-font-size
        mifi/default-variable-font-size (+ custom-default-font-size 20)
        mifi/font-size-slot 3)
      (mifi/update-face-attribute))))

;;; ##########################################################################
;; Some alternate keys below....

(bind-keys ("C-c 1". use-small-display-font)
  ("C-c 2". use-medium-display-font)
  ("C-c 3". use-large-display-font)
  ("C-c 4". use-x-large-display-font))

(defun mifi/set-recenter-keys ()
  (let ((map global-map))
    (define-key map (kbd "C-S-c 1")
      (lambda () (interactive) (use-small-display-font t)))
    (define-key map (kbd "C-S-c 2")
      (lambda () (interactive) (use-medium-display-font t)))
    (define-key map (kbd "C-S-c 3")
      (lambda () (interactive) (use-large-display-font t)))
    (define-key map (kbd "C-S-c 4")
      (lambda () (interactive) (use-x-large-display-font t)))
    (which-key-add-key-based-replacements "C-S-c 1" "recenter-with-small-font")
    (which-key-add-key-based-replacements "C-S-c 2" "recenter-with-medium-font")
    (which-key-add-key-based-replacements "C-S-c 3" "recenter-with-large-font")
    (which-key-add-key-based-replacements "C-S-c 4" "recenter-with-x-large-font")))

;;; ##########################################################################
;; Frame support functions

(defun mifi/set-frame-font (slot)
  (setq mifi/font-size-slot slot)
  (mifi/update-font-size)
  (unless enable-frameset-restore (mifi/frame-recenter)))

(defun mifi/should-recenter (&optional force-recenter)
  (if force-recenter
    (mifi/frame-recenter)
    ;;else
    (unless enable-frameset-restore (mifi/frame-recenter))))

;;; ##########################################################################

(defun use-small-display-font (&optional force-recenter)
  (interactive)
  (mifi/set-frame-font 0)
  (mifi/reset-if-spacious-padding-mode)
  (mifi/should-recenter force-recenter))


(defun use-medium-display-font (&optional force-recenter)
  (interactive)
  (mifi/set-frame-font 1)
  (mifi/reset-if-spacious-padding-mode)
  (mifi/should-recenter force-recenter))


(defun use-large-display-font (&optional force-recenter)
  (interactive)
  (mifi/set-frame-font 2)
  (mifi/reset-if-spacious-padding-mode)
  (mifi/should-recenter force-recenter))


(defun use-x-large-display-font (&optional force-recenter)
  (interactive)
  (mifi/set-frame-font 3)
  (mifi/reset-if-spacious-padding-mode)
  (mifi/should-recenter force-recenter))

;;; ##########################################################################
;; This is done so that the Emacs window is sized early in the init phase along with the default font size.
;; Startup works without this but it's nice to see the window expand early...
(when (display-graphic-p)
  (add-hook 'elpaca-after-init-hook
    (lambda ()
      (progn
        (mifi/update-face-attribute)
        (unless (daemonp)
          (unless enable-frameset-restore (mifi/frame-recenter))))
      )))

;;; ##########################################################################

(use-package spacious-padding
  :custom
  (spacious-padding-widths
    '( :internal-border-width 10
       :header-line-width 4
       :mode-line-width 6
       :tab-width 4
       :right-divider-width 10
       :scroll-bar-width 8
       :fringe-width 8))
  :ensure t
  :config
  (spacious-padding-mode t))

;; Read the doc string of `spacious-padding-subtle-mode-line' as it
;; is very flexible and provides several examples.
;; (setq spacious-padding-subtle-mode-line
;;       `( :mode-line-active 'default
;;          :mode-line-inactive vertical-border))

;;; ##########################################################################

(use-package faces :ensure nil)
(defun mifi/org-font-setup ()
  "Setup org mode fonts."

  (font-lock-add-keywords
    'org-mode
    '(("^ *\\([-]\\) "
        (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  
  (set-face-attribute 'org-block nil
    :foreground 'unspecified
    :inherit 'fixed-pitch
    :font mono-spaced-font-family
    :height custom-default-mono-font-size)
  
  (set-face-attribute 'org-formula nil
    :inherit 'fixed-pitch)
  
  (set-face-attribute 'org-code nil
    :foreground 'unspecified
    :font mono-spaced-font-family
    :height custom-default-mono-font-size
    :inherit '(shadow fixed-pitch))

  (set-face-attribute 'org-table nil
    :foreground 'unspecified
    :font mono-spaced-font-family
    :height custom-default-mono-font-size
    :inherit '(shadow fixed-pitch))
  
  (set-face-attribute 'org-verbatim nil
    :foreground 'unspecified
    :font mono-spaced-font-family
    :height custom-default-mono-font-size
    :inherit '(shadow fixed-pitch))
  
  (set-face-attribute 'org-special-keyword nil
    :inherit '(font-lock-comment-face fixed-pitch))
  
  (set-face-attribute 'org-meta-line nil
    :inherit '(font-lock-comment-face fixed-pitch))
  
  (set-face-attribute 'org-checkbox nil
    :foreground 'unspecified
    :font mono-spaced-font-family
    :height custom-default-mono-font-size
    :inherit 'fixed-pitch)
  
  (set-face-attribute 'line-number nil
    :foreground 'unspecified
    :font mono-spaced-font-family
    :height custom-default-mono-font-size
    :inherit 'fixed-pitch)
  
  (set-face-attribute 'line-number-current-line nil
    :foreground 'unspecified
    :font mono-spaced-font-family
    :height custom-default-mono-font-size
    :inherit 'fixed-pitch)

  (dolist (face '((org-level-1 . 1.50)
                   (org-level-2 . 1.25)
                   (org-level-3 . 1.15)
                   (org-level-4 . 1.05)
                   (org-level-5 . 0.95)
                   (org-level-6 . 0.90)
                   (org-level-7 . 0.90)
                   (org-level-8 . 0.90)))
    (set-face-attribute (car face) nil :font "Helvetica Neue" :weight 'regular
      :height (cdr face))))

;;; ##########################################################################

(defun mifi/org-mode-visual-fill ()
  (interactive)
  (setq visual-fill-column-width custom-org-fill-column
    visual-fill-column-center-text enable-org-fill-column-centering)
  (visual-fill-column-mode 1))

(defun mifi/org-mode-setup ()
  (interactive)
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (mifi/org-mode-visual-fill)
  (font-lock-add-keywords nil
    '(("^_\\{5,\\}"    0 '(:foreground "green" :weight bold))))
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  ;; (use-package org-habit)
  ;; (add-to-list 'org-modules 'org-habit)
  ;; (setq org-habit-graph-column 60)
  (setq org-refile-targets
    '(("Archive.org" :maxlevel . 1)
       ("Tasks.org" :maxlevel . 1))))

;;; ##########################################################################

(defun mifi/set-org-agenda-directory ()
  "Sets the org-agenda directory based upon the customized variable and then
sets the org-agenda-files list to all the files in that directory. The
directory is relative to the working-files-directory
(a.k.a user-emacs-directory)."
  (interactive)
  (let ((agenda-dir (format "%s/%s"
                      working-files-directory
                    org-agenda-dirname)))
    (make-directory agenda-dir t)
    (custom-set-variables
      '(org-directory agenda-dir)
      '(org-agenda-files (list org-directory)))))

;;; ##########################################################################

(defun mifi/org-setup-agenda ()
  "Function to setup basic org-agenda settings."
  (bind-key "C-c a" 'org-agenda org-mode-map)
  ;; (mifi/set-org-agenda-directory) ;; Where all the org-agenda files live    
  (setq org-agenda-custom-commands
    '(("d" "Dashboard"
        ((agenda "" ((org-deadline-warning-days 7)))
          (todo "NEXT"
            ((org-agenda-overriding-header "Next Tasks")))
          (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

       ("n" "Next Tasks"
         ((todo "NEXT"
            ((org-agenda-overriding-header "Next Tasks")))))

       ("W" "Work Tasks" tags-todo "+work-email")

       ;; Low-effort next actions
       ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Tasks")
           (org-agenda-max-todos 20)
           (org-agenda-files org-agenda-files)))

       ("w" "Workflow Status"
         ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
              (org-agenda-files org-agenda-files)))
           (todo "REVIEW"
             ((org-agenda-overriding-header "In Review")
               (org-agenda-files org-agenda-files)))
           (todo "PLAN"
             ((org-agenda-overriding-header "In Planning")
               (org-agenda-todo-list-sublevels nil)
               (org-agenda-files org-agenda-files)))
           (todo "BACKLOG"
             ((org-agenda-overriding-header "Project Backlog")
               (org-agenda-todo-list-sublevels nil)
               (org-agenda-files org-agenda-files)))
           (todo "READY"
             ((org-agenda-overriding-header "Ready for Work")
               (org-agenda-files org-agenda-files)))
           (todo "ACTIVE"
             ((org-agenda-overriding-header "Active Projects")
               (org-agenda-files org-agenda-files)))
           (todo "COMPLETED"
             ((org-agenda-overriding-header "Completed Projects")
               (org-agenda-files org-agenda-files)))
           (todo "CANC"
             ((org-agenda-overriding-header "Cancelled Projects")
               (org-agenda-files org-agenda-files)))))))
  ) ;; mifi/org-setup-agenda

;;; ##########################################################################

(defun mifi/org-setup-capture-templates ()
  (setq org-capture-templates
    `(("t" "Tasks / Projects")
       
       ("tt" "Task" entry (file+olp (expand-file-name "OrgFiles/Tasks.org" user-emacs-directory) "Inbox")
         "* TODO %?\n  %U\n  %a\n        %i" :empty-lines 1)

       ("j" "Journal Entries")
       ("jj" "Journal" entry
         (file+olp+datetree (expand-file-name "OrgFiles/Journal.org" user-emacs-directory))
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
         :clock-in :clock-resume
         :empty-lines 1)
       ("jm" "Meeting" entry
         (file+olp+datetree (expand-file-name "OrgFiles/Journal.org" user-emacs-directory))
         "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)

       ("w" "Workflows")
       ("we" "Checking Email" entry (file+olp+datetree
                                      (expand-file-name "OrgFiles/Joural.org" user-emacs-directory))
         "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

       ("m" "Metrics Capture")
       ("mw" "Weight" table-line (file+headline
                                   (expand-file-name "OrgFiles/Metrics.org" user-emacs-directory)
                                   "Weight")
         "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t))))

(defun mifi/org-setup-todos ()
  "Setup the org TODO keywords and colors."
  (setq org-todo-keywords
    '((type
        "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" 
        "RESEARCH(r)" "SOMEDAY(-)" "READING(e)"
        "CONTACT(c)" "|" "DONE(d)" "CANCELLED(C@)")))

  (setq org-todo-keyword-faces
    '(("TODO" :inherit (region org-todo) :foreground "gray70" :weight bold)
       ("WAITING" :inherit (org-todo region) :foreground "red1" :weight bold)
       ("IN-PROGRESS" :inherit (org-todo region) :foreground "gold1" :weight bold)
       ("RESEARCH" :inherit (org-todo region) :foreground "OliveDrab3" :weight bold)
       ("SOMEDAY" :inherit (org-todo region) :foreground "MediumPurple2" :weight bold)
       ("READING" :inherit (org-todo region) :foreground "DeepSkyBlue1" :weight bold)
       ("CONTACT" :inherit (org-todo region) :foreground "orange1" :weight bold)
       ("DONE" :inherit (region org-todo) :foreground "green1"   :weight bold)
       ("CANCELLED" :inherit (region org-todo) :foreground "green4"   :weight bold))))

(custom-theme-set-faces
  'user
  '(org-block ((t (:inherit fixed-pitch))))
  '(org-code ((t (:inherit (shadow fixed-pitch)))))
  '(org-document-info ((t (:foreground "dark orange"))))
  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
  '(org-link ((t (:foreground "royal blue" :underline t))))
  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  '(org-property-value ((t (:inherit fixed-pitch))) t)
  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;;; ##########################################################################

(use-package org
  :preface
  (mifi/org-theme-override-values)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  :commands (org-capture org-agenda)
  :defer t
  :hook (org-mode . mifi/org-mode-setup)
  :custom
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-use-sub-superscripts "{}")
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(300))
  :bind (:map org-mode-map
          ("C-c e" . org-edit-src-code))
  :config
  (setq org-hide-emphasis-markers nil)
  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq org-tag-alist
    '((:startgroup)
       ;; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))
  ;; Configure custom agenda views
  (mifi/org-setup-agenda)
  (mifi/org-setup-capture-templates)
  (mifi/org-font-setup)
  (mifi/org-setup-todos)
  (yas-global-mode t)
  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj"))))

;;; ##########################################################################

(with-eval-after-load 'org
  (org-babel-do-load-languages
    'org-babel-load-languages
    (seq-filter
      (lambda (pair)
        (locate-library (concat "ob-" (symbol-name (car pair)))))
      '((emacs-lisp . t)
         (ditaa . t)
         (dot . t)
         (emacs-lisp . t)
         (gnuplot . t)
         (haskell . nil)
         (latex . t)
         (ledger . t)
         (ocaml . nil)
         (octave . t)
         (plantuml . t)
         (python . t)
         (ruby . t)
         (screen . nil)
         (sh . t) ;; obsolete
         (shell . t)
         (sql . t)
         (sqlite . t))))
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

;;; ##########################################################################

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(use-package org-modern
  :when (display-graphic-p)
  :after org
  :ensure t
  :hook (org-mode . org-modern-mode)
  :config
  ;; Add frame borders and window dividers
  (modify-all-frames-parameters
    '((right-divider-width . 40)
       (internal-border-width . 40)))
  (dolist (face '(window-divider
                   window-divider-first-pixel
                   window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background nil)))
  (set-face-background 'fringe (face-attribute 'default :background nil))
  (setq
    ;;   ;; Edit settings
    ;;   org-auto-align-tags nil
    ;;   org-tags-column 0
    org-catch-invisible-edits 'show-and-error
    org-special-ctrl-a/e t
    org-insert-heading-respect-content t
    org-modern-star 'replace

    ;;   ;; Org styling, hide markup etc.
    org-hide-emphasis-markers nil
    org-pretty-entities t
    org-ellipsis "…"

    ;; Agenda styling
    org-agenda-tags-column 0
    org-agenda-block-separator ?─
    org-agenda-time-grid
    '((daily today require-timed)
       (800 1000 1200 1400 1600 1800 2000)
       " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
    org-agenda-current-time-string
    "◀── now ─────────────────────────────────────────────────"
    )
  (mifi/reset-if-spacious-padding-mode)
  (global-org-modern-mode))

;;; ##########################################################################

(defun mifi/define-denote-keymap ()
  (interactive)
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.
  ;; Just in case, unbind some org-roam keys so it doesn't get loaded
  ;; unintentionally. These are some that show up in the which-key menu:
  (unbind-key "C-c n f")
  (unbind-key "C-c n l")
  (unbind-key "C-c n p")
  (let ((map global-map))
    (define-key map (kbd "C-c n n") #'denote)
    (define-key map (kbd "C-c n c") #'denote-region) ; "contents" mnemonic
    (define-key map (kbd "C-c n N") #'denote-type)
    (define-key map (kbd "C-c n o") #'denote-open-or-create)
    (define-key map (kbd "C-c n d") #'denote-date)
    (define-key map (kbd "C-c n z") #'denote-signature) ; "zettelkasten" mnemonic
    (define-key map (kbd "C-c n s") #'denote-subdirectory)
    (define-key map (kbd "C-c n t") #'denote-template)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
    (define-key map (kbd "C-c n I") #'denote-add-links)
    (define-key map (kbd "C-c n b") #'denote-backlinks)
    (define-key map (kbd "C-c n f f") #'denote-find-link)
    (define-key map (kbd "C-c n f b") #'denote-find-backlink)
    ;; Note that `denote-rename-file' can work from any context, not just
    ;; Dired bufffers.  That is why we bind it here to the `global-map'.
    (define-key map (kbd "C-c n r") #'denote-rename-file)
    (define-key map (kbd "C-c n R") #'denote-rename-file-using-front-matter))

  ;; Key bindings specifically for Dired.
  (let ((map dired-mode-map))
    (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
    (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-files)
    (define-key map (kbd "C-c C-d C-k") #'denote-dired-rename-marked-files-with-keywords)
    (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter))

  (when (bound-and-true-p which-key-mode)
    (which-key-add-key-based-replacements "C-c n f" "denote-find")))

;;; ##########################################################################

(use-package denote
  :defer t
  :after which-key
  :custom
  (denote-directory (expand-file-name "notes" user-emacs-directory))
  (denote-save-buffers nil)
  ;; (denote-known-keywords '("emacs" "philosophy" "politics" "economics"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil) ; Org is the default, set others here
  (denote-prompts '(title keywords))
  (denote-excluded-directories-regexp nil)
  (denote-excluded-keywords-regexp nil)
  (denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  (denote-date-prompt-use-org-read-date t)
  (denote-date-format nil) ; read doc string
  (denote-backlinks-show-context t)
  (denote-dired-directories
    (list denote-directory
      (thread-last denote-directory (expand-file-name "attachments"))
      (expand-file-name "books" user-emacs-directory)))
  :config
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
  (denote-rename-buffer-mode 1)

  (mifi/define-denote-keymap) ;; Define the keymap for Denote.

  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
      '("n" "New note (with denote.el)" plain
         (file denote-last-path)
         #'denote-org-capture
         :no-save t
         :immediate-finish nil
         :kill-buffer t
         :jump-to-captured t)))

  (add-hook 'context-menu-functions #'denote-context-menu))

;;; ##########################################################################

(use-package projectile
  :when (equal custom-project-handler 'custom-project-projectile)
  :delight Proj
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Developer")
    (setq projectile-project-search-path '("~/Developer")))
  (setq projectile-switch-project-action #'projectile-dired))

(when (equal completion-handler 'comphand-ivy-counsel)
  (use-package counsel-projectile
    :when (equal custom-project-handler 'custom-project-projectile)
    :after projectile
    :config
    (setq projectile-completion-system 'ivy)
    (counsel-projectile-mode)))

;;; ##########################################################################

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(use-package project
  :when (equal custom-project-handler 'custom-project-project)
  :ensure nil
  :defer t
  :config
  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))
  (add-hook 'project-find-functions #'project-find-go-module))

;;; ##########################################################################
;;; Emacs Polyglot is the Emacs LSP client that stays out of your way:

(defvar mifi/clangd-path (executable-find "clangd")
  "Clangd executable path.")

(defun mifi/projectile-proj-find-function (dir)
  "Find the project `DIR' function for Projectile.
  Thanks @wyuenho on GitHub"
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package track-changes
  :defer t
  :config
  (unless theme-did-load
    (mifi/load-theme-from-selector)))

;;; ##########################################################################

;; Consider doing an "M-x eglot-upgrade-eglot" to ensure that you have the most
;; current eglot (yes, even though it's a built in package).

(use-package eglot
  :when (equal custom-ide 'custom-ide-eglot)
  :ensure nil
  :defer t
  :hook
  (lisp-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  (rustic-mode . eglot-ensure)
  ;; (c-mode . eglot-ensure)
  ;; (c++-mode . eglot-ensure)
  ;; (prog-mode . eglot-ensure)
  :config
  (flymake-mode 0)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  ;; Eldoc/Eglot will try to load/unload a theme which can cause issues with our
  ;; theme loading mechanism. Our theme could fail to load because of this.  So,
  ;; to get our themes loading properly, load it here if not already loaded.
  (unless theme-did-load
    (mifi/load-theme-from-selector))
  (add-to-list 'eglot-stay-out-of 'flymake)
  (if (featurep 'company) ;; Company should be loaded.
    (bind-keys :map eglot-mode-map
      ("<tab>" . company-indent-or-complete-common))
    (message "Eglot: Company was expected to be loaded but wasn't.")))

;;; ##########################################################################
;;; Language Server Protocol

;; (when (equal custom-ide 'custom-ide-lsp)
;;   (eval-when-compile (defvar lsp-enable-which-key-integration)))

(use-package lsp-mode
  :when (equal custom-ide 'custom-ide-lsp)
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . mifi/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (if (featurep 'company)
    (bind-keys :map lsp-mode-map
      ("<tab>" . company-indent-or-complete-common)))
  (mifi/define-rust-lsp-values)
  (lsp-enable-which-key-integration t))

;;; ##########################################################################

(use-package lsp-ui
  :when (equal custom-ide 'custom-ide-lsp)
  :after lsp
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-sideline-ignore-duplicates t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-doc-delay 3)
  (lsp-ui-doc-position 'bottom)
  ;;(lsp-ui-doc-position 'top)
  (lsp-ui-doc-alignment 'frame)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-use-childframe t)
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
          ("C-c l d" . lsp-ui-doc-focus-frame))
  :hook (lsp-mode . lsp-ui-mode))

;;; ##########################################################################
;;; To enable bidirectional synchronization of lsp workspace folders and
;;; treemacs projects set lsp-treemacs-sync-mode to 1.

(use-package lsp-treemacs
  :when (equal custom-ide 'custom-ide-lsp)
  :after lsp treemacs
  :bind (:map prog-mode-map
          ("C-c t" . treemacs))
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-ivy
  :when (and (equal custom-ide 'custom-ide-lsp)
          (equal completion-handler 'comphand-ivy-counsel))
  :after lsp ivy)

;;; ##########################################################################
;;; LSP mode setup hook

(defun mifi/lsp-mode-setup ()
  "Custom LSP setup function."
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  (when (equal custom-ide 'custom-ide-lsp)
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (setq lsp-clangd-binary-path "/usr/bin/clangd")'
    (lsp-headerline-breadcrumb-mode)))

;;; ##########################################################################

(defun mifi/define-rust-lsp-values ()
  (setq-default lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq-default lsp-eldoc-render-all t)
  (setq-default lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (setq-default lsp-inlay-hint-enable t)
  ;; These are optional configurations. See
  ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints
  ;; for a full list
  (setq-default lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (setq-default lsp-rust-analyzer-display-chaining-hints t)
  (setq-default lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (setq-default lsp-rust-analyzer-display-closure-return-type-hints t)
  (setq-default lsp-rust-analyzer-display-parameter-hints nil)
  (setq-default lsp-rust-analyzer-display-reborrow-hints nil))

;;; ##########################################################################

(use-package lsp-bridge
  :when (equal custom-ide 'custom-ide-lsp-bridge)
  :ensure ( :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver"
                     "multiserver" "resources") :build (:not compile))
  :custom
  (lsp-bridge-python-lsp-server "pylsp")
  :config
  (global-lsp-bridge-mode))

;;; ##########################################################################

(use-package anaconda-mode
  :after (:any which-key company)
  :when (equal custom-ide 'custom-ide-anaconda)
  :bind (:map python-mode-map
          ("C-c g o" . anaconda-mode-find-definitions-other-frame)
          ("C-c g g" . anaconda-mode-find-definitions)
          ("C-c C-x" . next-error))
  :config
  (when (featurep 'company)
    (bind-keys :map anaconda-mode-map
      ("<tab>" . company-indent-or-complete-common)))
  (use-package pyvenv-auto)
  :hook (python-mode-hook . anaconda-eldoc-mode))

;;; ##########################################################################

(use-package elpy
  :when (equal custom-ide 'custom-ide-elpy)
  :after python which-key
  :custom
  (elpy-rpc-python-command "python3")
  (display-fill-column-indicator-mode 1)
  (highlight-indentation-mode nil)
  :bind (:map python-mode-map
          ("C-c g a" . elpy-goto-assignment)
          ("C-c g o" . elpy-goto-definition-other-window)
          ("C-c g g" . elpy-goto-definition)
          ("C-c g ?" . elpy-doc))
  :config
  (use-package jedi)
  ;; (use-package flycheck
  ;;   :when (equal custom-ide 'custom-ide-elpy)
  ;;   :after elpy
  ;;   :defer t
  ;;   :delight "fc"
  ;;   ;;:ensure (:host github :repo "flycheck/flycheck")
  ;;   :hook (elpy-mode . flycheck-mode))
  (which-key-add-key-based-replacements "C-c g a" "goto-assignment")
  (which-key-add-key-based-replacements "C-c g o" "find-defitions-other-window")
  (which-key-add-key-based-replacements "C-c g g" "find-defitions")
  (which-key-add-key-based-replacements "C-c g ?" "eldoc-definition")
  (if (featurep 'company)
    (bind-keys :map elpy-mode-map
      ("<tab>" . company-indent-or-complete-common)))
  (elpy-enable))

;;; ##########################################################################

(use-package flycheck
  ;;:unless (equal custom-ide 'custom-ide-elpy)
  :delight fc
  :defer t
  ;;:ensure (:host github :repo "flycheck/flycheck")
  :config
  (eval-after-load 'flycheck
    '(flycheck-package-setup))
  (global-flycheck-mode))

(use-package flycheck-package
  :after flycheck)

;;; ##########################################################################

(defun mifi/tree-sitter-setup ()
  (tree-sitter-hl-mode t))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;;; ##########################################################################

(use-package tree-sitter
  :defer t
  :after (:any python python-mode lisp-mode)
  :config
  ;; Activate tree-sitter globally (minor mode registered on every buffer)
  (global-tree-sitter-mode)
  (cond
    ((equal custom-ide 'custom-ide-eglot)
      (add-hook 'go-mode-hook 'eglot-ensure))
    ((equal custom-ide 'custom-ide-lsp)
      (add-hook 'go-mode-hook 'lsp-deferred)))
  :hook
  (tree-sitter-after-on . mifi/tree-sitter-setup)
  (typescript-mode . lsp-deferred)
  ;; (c-mode . lsp-deferred)
  ;; (c++-mode . lsp-deferred)
  (before-save . lsp-go-install-save-hooks)
  (js2-mode . lsp-deferred))

(use-package tree-sitter-langs
  :after tree-sitter)

;;; ##########################################################################

(use-package treesit-auto
  :demand t :ensure t
  :config
  (global-treesit-auto-mode))

;;; ##########################################################################

(use-package transient :defer t)
(use-package git-commit :after transient :defer t)
(use-package magit :after git-commit :defer t)

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started

(use-package forge :after magit :defer t)
(use-package treemacs-magit :defer t :after treemacs magit)

;;; ##########################################################################

(defun mifi/set-custom-ide-python-keymaps ()
  (cond
    ((equal custom-ide 'custom-ide-lsp)
      (bind-keys :map python-mode-map
        ("C-c g r" . lsp-find-references)
        ("C-c g g" . xref-find-definitions)
        ("C-c g G" . xref-find-definitions-other-window)
        ("C-c g ?" . eldoc-doc-buffer)))
    ((equal custom-ide 'custom-ide-eglot)
      (bind-keys :map python-mode-map
        ("C-c g i" . eglot-find-implementation)
        ("C-c g r" . xref-find-references)
        ("C-c g D" . xref-find-definitions-other-window)
        ("C-c g g" . xref-find-definitions)
        ("C-c g n" . xref-find-references-and-replace)
        ("C-c g ?" . eldoc-doc-buffer)))
    ((equal custom-ide 'custom-ide-elpy)
      (elpy-enable)
      (bind-keys :map python-mode-map
        ("C-c g a" . elpy-goto-assignment)
        ("C-c g A" . elpy-goto-definition-other-window)
        ("C-c g g" . elpy-goto-definition)
        ("C-c g ?" . elpy-doc)))
    ((equal custom-ide 'custom-ide-lsp-bridge)
      (bind-keys :map python-mode-map
        ("C-c g a" . lsp-bridge-find-reference)
        ("C-c g A" . lsp-bridge-find-def-other-window)
        ("C-c g g" . lsp-bridge-find-def)
        ("C-c g i" . lsp-bridge-find-impl)
        ("C-c g n" . lsp-bridge-rename)
        ("C-c g ?" . lsp-bridge-popup-documentation)))
    ((equal custom-ide 'custom-ide-anaconda)
      (bind-keys :map python-mode-map
        ("C-c g a" . anaconda-mode-find-assignments)
        ("C-c g A" . anaconda-mode-find-assignments-other-window)
        ("C-c g r" . anaconda-mode-find-references)
        ("C-c g R" . anaconda-mode-find-references-other-window)
        ("C-c g g" . anaconda-mode-find-definitions)
        ("C-c g G" . anaconda-mode-find-definitions-other-window)
        ("C-c g ?" . anaconda-mode-show-doc)))
    ))

;;; ##########################################################################

(defun mifi/load-python-file-hook ()
  (python-mode)
  (flymake-mode 0)
  (when (equal custom-ide 'custom-ide-anaconda)
    (anaconda-mode 1))
  (message ">>> mifi/load-python-file-hook")
  (setq highlight-indentation-mode -1)
  (setq display-fill-column-indicator-mode t))

(defun mifi/before-save ()
  "Force the check of the current python file being saved."
  (when (eq major-mode 'python-mode) ;; Python Only
    (flycheck-mode 0)
    (flycheck-mode t)
    (delete-trailing-whitespace)))

;; Enable DAP or DAPE, Eglot or LSP modes
;; This function should only be called ONCE during python-mode startup.
(defun mifi/enable-python-features ()
  (message ">>> mifi/enable-python-features")
  ;; _____________________________
  ;; check for which debug adapter
  (cond
    ((equal debug-adapter 'debug-adapter-dap-mode)
      (unless (featurep 'dap-mode) (dap-mode)) ;; Load if not loaded.
      (define-dap-hydra))
    ((equal debug-adapter 'debug-adapter-dape)
      ;; dape should load as part of (use-package .... :after python)
      (message "dape should be auto-loading for Python.")))
  ;;___________________________
  ;; check for which custom-ide
  (cond
    ((equal custom-ide 'custom-ide-eglot)
      (eglot-ensure))
    ((equal custom-ide 'custom-ide-lsp)
      (lsp-deferred))))

(defun mifi/python-mode-triggered ()
  ;; (eldoc-box-hover-at-point-mode t) ;; Using Mitch Key for this
  (mifi/enable-python-features)
  (mifi/set-custom-ide-python-keymaps)
  (unless (featurep 'yasnippet)
    (yas-global-mode t))
  (add-hook 'before-save-hook 'mifi/before-save)
  (set-fill-column 80))

;;; ##########################################################################

(add-to-list 'auto-mode-alist '("\\.py\\'" . mifi/load-python-file-hook))

(use-package python-mode
  :hook (python-mode . mifi/python-mode-triggered))

(use-package blacken :after python) ;Format Python file upon save.

(if (boundp 'python-shell-completion-native-disabled-interpreters)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
  (setq python-shell-completion-native-disabled-interpreters '("python3")))

;;; ##########################################################################

(use-package py-autopep8
  :after python
  :hook (python-mode . py-autopep8-mode))

;;; ##########################################################################

;; This is a helpful macro that is used to put double quotes around a word.
(defalias 'quote-word
  (kmacro "\" M-d \" <left> C-y"))

(defalias 'quote-region
  (kmacro "C-w \" \" <left> C-y <right>"))

(defalias 'reformat-src-block
  (kmacro "C-s b e g i n _ s r c SPC e m a c s - l i s p <return> <down> C-c ' C-x h C-c ] C-x h M-x u n t a b <return> C-c ' C-s e n d _ s r c <return> <down>"))

(eval-after-load "python"
  #'(bind-keys :map python-mode-map
      ("C-c C-q" . quote-region)
      ("C-c q"   . quote-word)
      ("C-c |"   . display-fill-column-indicator-mode)))

;;; ##########################################################################

(use-package pyvenv-auto
  :after python
  :hook (python-mode . pyvenv-auto-run))

;;; ##########################################################################

(use-package pydoc
  ;;:ensure (:host github :repo "statmobile/pydoc")
  :after python
  :custom
  (pydoc-python-command "python3")
  (pydoc-pip-version-command "pip3 --version"))

;;; ##########################################################################

(use-package typescript-mode
  :defer t
  :mode "\\.ts\\'"
  :hook
  (typescript-mode . lsp-deferred)
  (js2-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 4)
  (cond
    ((equal debug-adapter 'debug-adapter-dap-mode)
      (bind-keys :map typescript-mode-map
        ("C-c ." . dap-hydra/body))
      (dap-node-setup))
    ((equal debug-adapter 'debug-adapter-dape)
      (bind-keys :map typescript-mode-map
        ("C-c ." . dape-hydra/body)))))

;;; ##########################################################################

(defun mifi/load-js-file-hook ()
  (js2-mode)

  (when (equal debug-adapter 'debug-adapter-dap-mode)
    (dap-mode)
    (dap-firefox-setup))

  (when (equal debug-adapter 'debug-adapter-dape)
    (dape))

  (highlight-indentation-mode nil)
  (dap-firefox-setup))

(use-package nodejs-repl :defer t)

(defun mifi/nvm-which ()
  (let ((output (shell-command-to-string "source ~/.nvm/nvm.sh; nvm which")))
    (cadr (split-string output "[\n]+" t))))

(setq nodejs-repl-command #'mifi/nvm-which)

;;; ##########################################################################

(use-package js2-mode
  :hook (js-mode . js2-minor-mode)
  :bind (:map js2-mode-map
          ("{" . paredit-open-curly)
          ("}" . paredit-close-curly-and-newline))
  :mode ("\\.js\\'" "\\.mjs\\'" "\\.json$")
  :custom (js2-highlight-level 3))

(use-package ac-js2
  :after js2-mode
  :hook (js2-mode . ac-js2-mode))

;;; ##########################################################################

(defun mifi/load-c-file-hook ()
  (c-mode)
  (unless (featurep 'realgud))
  (use-package realgud)
  (highlight-indentation-mode nil)
  (display-fill-column-indicator-mode t))

(defun code-compile ()
  "Look for a Makefile and compiles the code with gcc/cpp."
  (interactive)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
      (let ((file (file-name-nondirectory buffer-file-name)))
        (format "%s -o %s %s"
          (if  (equal (file-name-extension file) "cpp") "g++" "gcc" )
          (file-name-sans-extension file)
          file)))
    (compile compile-command)))

(global-set-key [f9] 'code-compile)
(add-to-list 'auto-mode-alist '("\\.c\\'" . mifi/load-c-file-hook))

;;; ##########################################################################

(use-package z80-mode
  :when enable-gb-dev
  :ensure (:host github :repo "SuperDisk/z80-mode"))

(use-package mwim
  :when enable-gb-dev
  :ensure (:host github :repo "alezost/mwim.el"))

(use-package rgbds-mode
  :when enable-gb-dev
  :after mwim
  :ensure (:host github :repo "japanoise/rgbds-mode"))

;;; ##########################################################################

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
          ("M-j" . lsp-ui-imenu)
          ("M-?" . lsp-find-references)
          ("C-c C-c l" . flycheck-list-errors)
          ("C-c C-c a" . lsp-execute-code-action)
          ("C-c C-c r" . lsp-rename)
          ("C-c C-c q" . lsp-workspace-restart)
          ("C-c C-c Q" . lsp-workspace-shutdown)
          ("C-c C-c s" . lsp-rust-analyzer-status))
  :hook (rustic-mode . rk/rustic-mode-hook)
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))


(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;;; ##########################################################################

;; (use-package graphql-mode)
(use-package rust-mode
  :disabled ;;; Older than rustic so don't use but 
  :defer t
  :init (setq rust-mode-treesitter-derive t)
  :hook
  (rust-mode . lsp-deferred)
  (rust-mode . (lambda () (setq indent-tabs-mode nil)
                 (prettify-symbols-mode)))
  :config
  (setq rust-format-on-save t))

(use-package rust-playground :ensure t :after rust-mode)

;;; ##########################################################################
;; for Cargo.toml and other config files

(use-package toml-mode :ensure t)

;;; ##########################################################################

(use-package cargo-mode
  :defer t
  :after rust-mode
  :ensure (:fetcher github :repo "ayrat555/cargo-mode"
            :files ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                       "*-test.el" "*-tests.el" "LICENSE" "README*"
                       "*-pkg.el"))))

;;; ##########################################################################

(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

(use-package go-mode
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :custom
  (compile-command "go build -v && go test -v && go vet")
  :bind (:map go-mode-map
          ("C-c C-c" . 'compile))
  :config
  (eglot-format-buffer-on-save)
  (define-key (current-local-map) "\C-c\C-c" 'compile)
  (cond
    ((equal custom-ide 'custom-ide-eglot)
      (add-hook 'go-mode-hook 'eglot-ensure)
      (add-hook 'go-mode-hook #'elot-format-buffer-on-save))
    ((equal custom-ide 'custom-ide-lsp)
      (add-hook 'go-mode-hook 'lsp-deferred))))

;;; ##########################################################################

(use-package go-eldoc
  :after go-mode
  :hook (go-mode . go-eldoc-setup)
  :config
  (eglot-format-buffer-on-save)
  (set-face-attribute 'eldoc-highlight-function-argument nil
    :underline t :foreground "green"
    :weight 'bold))

;;; ##########################################################################

(use-package go-guru
  :after go-mode
  :hook (go-mode . go-guru-hl-identifier-mode))

;;; ##########################################################################

(use-package slime
  :defer t
  :mode ("\\.lisp\\'" . slime-mode)
  :config
  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl"))

;;; ##########################################################################

(use-package swift-mode
  :defer t
  :mode ("\\.swift\\'" . swift-mode))

(use-package swift-helpful
  :defer t
  :after swift-mode
  :ensure (:files ("*.el" "swift-info/*.info"
                    ("images" "swift-info/images/*.png") "swift-helpful-pkg.el")
            :host github
            :repo "danielmartin/swift-helpful"))

(use-package swift-playground-mode
  :defer t
  :after swift-mode
  :ensure ( :package "swift-playground-mode"
          :repo "https://gitlab.com/michael.sanders/swift-playground-mode.git")
  :init
  (autoload 'swift-playground-global-mode "swift-playground-mode" nil t)
  (add-hook 'swift-mode-hook #'swift-playground-global-mode))

;;; ------------------------------------------------------------------------

(use-package dape
  :when (equal debug-adapter 'debug-adapter-dape)
  :after (:any python go)
  :custom
  (dape-buffer-window-arrangement 'right)  ;; Info buffers to the right
  :config
  (define-dape-hydra)
  (message "prepare-dape end")
  (bind-keys :map prog-mode-map
    ("C-c ." . dape-hydra/body))
  (mifi/additional-dape-configs))

;;; ##########################################################################

(setq mifi/vscode-js-debug-dir (file-name-concat user-emacs-directory "dape/vscode-js-debug"))

(defun mifi/install-vscode-js-debug ()
  "Run installation procedure to install JS debugging support"
  (interactive)
  (mkdir mifi/vscode-js-debug-dir t)
  (let ((default-directory (expand-file-name mifi/vscode-js-debug-dir)))

    (vc-git-clone "https://github.com/microsoft/vscode-js-debug.git" "." nil)
    (call-process "npm" nil "*snam-install*" t "install")
    (call-process "npx" nil "*snam-install*" t "gulp" "dapDebugServer")))

;;; ##########################################################################

;; (mifi/install-vscode-js-debug)

;;; ------------------------------------------------------------------------
(defun mifi/additional-dape-configs ()
  "Additional DAPE configruations for various languages."

  (with-eval-after-load
    (add-to-list 'dape-configs
      `(delve
         modes (go-mode go-ts-mode)
         command "dlv"
         command-args ("dap" "--listen" "127.0.0.1:55878")
         command-cwd dape-cwd-fn
         host "127.0.0.1"
         port 55878
         :type "debug"  ;; needed to set the adapterID correctly as a string type
         :request "launch"
         :cwd dape-cwd-fn
         :program dape-cwd-fn))))

;;; ##########################################################################

(defun mifi/dape-end-debug-session ()
  "End the debug session."
  (interactive)
  (dape-quit))

(defun mifi/dape-delete-all-debug-sessions ()
  "End the debug session and delete all breakpoints."
  (interactive)
  (dape-breakpoint-remove-all)
  (mifi/dape-end-debug-session))

;;; ##########################################################################

(defun define-dape-hydra ()
  (defhydra dape-hydra (:color pink :hint nil :foreign-keys run)
    "
  ^Stepping^          ^Switch^                 ^Breakpoints^          ^Debug^                     ^Eval
  ^^^^^^^^----------------------------------------------------------------------------------------------------------------
  _._: Next           _st_: Thread             _bb_: Toggle           _dd_: Debug                 _ee_: Eval Expression
  _/_: Step in        _si_: Info               _bd_: Delete           _dw_: Watch dwim
  _,_: Step out       _sf_: Stack Frame        _ba_: Add              _dx_: end session
  _c_: Continue       _su_: Up stack frame     _bc_: Set condition    _dX_: end all sessions
  _r_: Restart frame  _sd_: Down stack frame   _bl_: Set log message  _dp_: Initialize DAPE
  _Q_: Disconnect     _sR_: Session Repl
                    _sU_: Info Update"
    ("n" dape-next)
    ("i" dape-step-in)
    ("o" dape-step-out)
    ("." dape-next)
    ("/" dape-step-in)
    ("," dape-step-out)
    ("c" dape-continue)
    ("r" dape-restart)
    ("si" dape-info)
    ("st" dape-select-thread)
    ("sf" dape-select-stack)
    ("su" dape-stack-select-up)
    ("sU" dape-info-update)
    ("sd" dape-stack-select-down)
    ("sR" dape-repl)
    ("bb" dape-breakpoint-toggle)
    ("ba" dape--breakpoint-place)
    ("bd" dape-breakpoint-remove-at-point)
    ("bc" dape-breakpoint-expression)
    ("bl" dape-breakpoint-log)
    ("dd" dape)
    ("dw" dape-watch-dwim)
    ("ee" dape-evaluate-expression)
    ("dx" mifi/dape-end-debug-session)
    ("dX" mifi/dape-delete-all-debug-sessions)
    ("dp" dape-prepare)
    ("x" nil "exit Hydra" :color yellow)
    ("q" mifi/dape-end-debug-session "quit" :color blue)
    ("Q" mifi/dape-delete-all-debug-sessions :color red)))

;;; ##########################################################################
;;; Debug Adapter Protocol
(use-package dap-mode
  :when (equal debug-adapter 'debug-adapter-dap-mode)
  :after hydra
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  :commands dap-debug
  :custom
  (dap-auto-configure-features '(sessions locals breakpoints expressions repl controls tooltip))
  :config
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (define-dap-hydra)
  (bind-keys :map prog-mode-map
    ("C-c ." . dap-hydra/body))
  (dap-ui-controls-mode)
  (dap-ui-mode 1))

;;; ##########################################################################
;;; DAP for Python

(use-package dap-python
  :ensure (:package "dap-python" :type git :host github :repo "emacs-lsp/dap-mode")
  :when (equal debug-adapter 'debug-adapter-dap-mode)
  :after dap-mode
  :config
  (setq dap-python-executable "python3") ;; Otherwise it looks for 'python' else error.
  (setq dap-python-debugger 'debugpy))

;;; ##########################################################################

(use-package dap-lldb
  :when (equal debug-adapter 'debug-adapter-dap-mode)
  :defer t
  :after dap-mode
  :ensure ( :package "dap-lldb" :source nil :protocol https
	    :inherit t :depth 1 :type git
	    :host github :repo "emacs-lsp/dap-mode")
  :custom
  (dap-lldb-debug-program "~/Developer/command-line-unix/llvm/lldb-build/bin/lldb-dap")
  :config
  (dap-register-debug-template
    "Rust::LLDB Run Configuration"
    (list :type "lldb"
      :request "launch"
      :name "LLDB::Run"
      :gdbpath "rust-lldb"
      :target nil
      :cwd nil)))

(use-package dap-gdb-lldb
  :when (equal debug-adapter 'debug-adapter-dap-mode)
  :ensure ( :package "dap-gdb-lldb" :source nil :protocol https
	    :inherit t :depth 1 :type git :host github
	    :repo "emacs-lsp/dap-mode")
  :defer t
  :after dap-lldb
  :config
  (dap-gdb-lldb-setup))

(use-package dap-cpptools
  :when (equal debug-adapter 'debug-adapter-dap-mode)
  :defer t
  :after dap-mode
  :ensure ( :package "dap-cpptools" :source nil :protocol https
	    :inherit t :depth 1 :type git :host github
	    :repo "emacs-lsp/dap-mode"))
;; :config
;; (dap-cpptools-setup))

;;; ##########################################################################

(with-eval-after-load 'dap-lldb
  (dap-register-debug-template
    "Rust::LLDB Run Configuration"
    (list :type "lldb"
      :request "launch"
      :name "LLDB::Run"
      :gdbpath "rust-lldb"
      :target nil
      :cwd nil)))

(with-eval-after-load 'dap-python
  (dap-register-debug-template "Python :: Run file from project directory"
    (list :type "python"
      :args ""
      :cwd nil
      :module nil
      :program nil
      :request "launch"))
  (dap-register-debug-template "Python :: Run file (buffer)"
    (list :type "python"
      :args ""
      :cwd nil
      :module nil
      :program nil
      :request "launch"
      :name "Python :: Run file (buffer)")))

;;; ##########################################################################

(defun mifi/dap-end-debug-session ()
  "End the debug session and delete project Python buffers."
  (interactive)
  (kill-matching-buffers "\*Python :: Run file [from|\(buffer]*" nil :NO-ASK)
  (kill-matching-buffers "\*Python: Current File*" nil :NO-ASK)
  (kill-matching-buffers "\*dap-ui-*" nil :NO-ASK)
  (dap-disconnect (dap--cur-session)))

(defun mifi/dap-delete-all-debug-sessions ()
  "End the debug session and delete project Python buffers and all breakpoints."
  (interactive)
  (dap-breakpoint-delete-all)
  (mifi/dap-end-debug-session))

(defun mifi/dap-begin-debug-session ()
  "Begin a debug session with several dap windows enabled."
  (interactive)
  (dap-ui-show-many-windows)
  (dap-debug))

;;; ##########################################################################

(defun define-dap-hydra ()
  (defhydra dap-hydra (:color pink :hint nil :foreign-keys run)
    "
  ^Stepping^            ^Switch^                 ^Breakpoints^          ^Debug^                     ^Eval
  ^^^^^^^^-----------------------------------------------------------------------------------------------------------------
  _._: Next            _ss_: Session            _bb_: Toggle           _dd_: Debug                 _ee_: Eval
  _/_: Step in         _st_: Thread             _bd_: Delete           _dr_: Debug recent          _er_: Eval region
  _,_: Step out        _sf_: Stack frame        _ba_: Add              _dl_: Debug last            _es_: Eval thing at point
  _c_: Continue        _su_: Up stack frame     _bc_: Set condition    _de_: Edit debug template   _ea_: Add expression.
  _r_: Restart frame   _sd_: Down stack frame   _bh_: Set hit count    _ds_: Debug restart
  _Q_: Disconnect      _sl_: List locals        _bl_: Set log message  _dx_: end session
                       _sb_: List breakpoints                          _dX_: end all sessions
                       _sS_: List sessions
                       _sR_: Session Repl
"
    ("n" dap-next)
    ("i" dap-step-in)
    ("o" dap-step-out)
    ("." dap-next)
    ("/" dap-step-in)
    ("," dap-step-out)
    ("c" dap-continue)
    ("r" dap-restart-frame)
    ("ss" dap-switch-session)
    ("st" dap-switch-thread)
    ("sf" dap-switch-stack-frame)
    ("su" dap-up-stack-frame)
    ("sd" dap-down-stack-frame)
    ("sl" dap-ui-locals)
    ("sb" dap-ui-breakpoints)
    ("sR" dap-ui-repl)
    ("sS" dap-ui-sessions)
    ("bb" dap-breakpoint-toggle)
    ("ba" dap-breakpoint-add)
    ("bd" dap-breakpoint-delete)
    ("bc" dap-breakpoint-condition)
    ("bh" dap-breakpoint-hit-condition)
    ("bl" dap-breakpoint-log-message)
    ("dd" dap-debug)
    ("dr" dap-debug-recent)
    ("ds" dap-debug-restart)
    ("dl" dap-debug-last)
    ("de" dap-debug-edit-template)
    ("ee" dap-eval)
    ("ea" dap-ui-expressions-add)
    ("er" dap-eval-region)
    ("es" dap-eval-thing-at-point)
    ("dx" mifi/dap-end-debug-session)
    ("dX" mifi/dap-delete-all-debug-sessions)
    ("x" nil "exit Hydra" :color yellow)
    ("q" mifi/dap-end-debug-session "quit" :color blue)
    ("Q" mifi/dap-delete-all-debug-sessions :color red)))

;;; ------------------------------------------------------------------------
(use-package jsonrpc
  :ensure t
  :config
  ;; For some odd reason, it is possible that jsonrpc will try to load a
  ;; theme. (jsonrpc/lisp/custom.el:1362). If our theme hasn't been loaded
  ;; yet, go ahead and try. This could prevent a startup without the theme
  ;; properly loaded.
  (unless theme-did-load
    (mifi/load-theme-from-selector)))

(use-package mw-thesaurus
  :when enable-thesaurus
  :ensure t
  :custom
  (mw-thesaurus-api-key "429331e9-b40e-4f17-9988-0632ef3ddd2d")
  :defer t
  :commands mw-thesaurus-lookup-dwim
  :hook (mw-thesaurus-mode . variable-pitch-mode)
  :config
  ;; window on the right side
  (add-to-list 'display-buffer-alist '(,mw-thesaurus-buffer-name
                                        (display-buffer-reuse-window
                                          display-buffer-in-direction)
                                        (direction . right)
                                        (window . root)
                                        (window-width . 0.3))))

;;; ##########################################################################

(use-package solaire-mode
  :after treemacs
  :ensure (:package "solaire-mode" :source "MELPA"
            :repo "hlissner/emacs-solaire-mode" :fetcher github)
  :hook (elpaca-after-init . solaire-global-mode)
  :config
  (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
  (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist))

;;; ##########################################################################
;; Golen Ratio

(use-package golden-ratio
  :when enable-golden-ratio
  :custom
  (golden-ratio-auto-scale t)
  (golden-ratio-adjust-factor .4)
  (golden-ratio-wide-adjust-factor .4)
  (golden-ratio-max-width 100)
  (golden-ratio-exclude-modes '(
                                 prog-mode
                                 dashboard-mode
                                 ;;inferior-emacs-lisp-mode
                                 ;;inferior-python-mode
                                 comint-mode
                                 ;;lisp-interaction-mode
                                 treemacs-mode
                                 undo-tree-visualizer-mode
                                 vundo-mode
                                 ))
  (golden-ratio-exclude-buffer-regexp '("dap*"
                                         "*dape*"
                                         "*python*"))
  :config
  (golden-ratio-mode 1))

;;; ##########################################################################

(use-package buffer-move
  :bind (("C-S-<up>"     . buf-move-up)
          ("C-S-<down>"  . buf-move-down)
          ("C-S-<left>"  . buf-move-left)
          ("C-S-<right>" . buf-move-right)))

;;; ##########################################################################

(use-package neotree
  :when enable-neotree
  :config
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;;; ##########################################################################
;; Enable tabs for each buffer

(use-package centaur-tabs
  :when enable-centaur-tabs
  :custom
  ;; Set the style to rounded with icons (setq centaur-tabs-style "bar")
  (centaur-tabs-style "bar")
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  :bind (("C-c <" . centaur-tabs-backward)
          ("C-c >" . centaur-tabs-forward))
  :config ;; Enable centaur-tabs
  (centaur-tabs-mode t))

;;; ##########################################################################

(use-package diff-hl
  :config
  (global-diff-hl-mode))

;;; ##########################################################################

(use-package pulsar
  :ensure t
  :config
  (pulsar-global-mode)
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.10)
  (pulsar-iterations 10)
  (pulsar-face 'pulsar-magenta)
  (pulsar-highlight-face 'pulsar-yellow))

;;; ##########################################################################

(use-package rainbow-mode
  :ensure nil
  :hook (prog-mode . (lambda () (rainbow-mode t))))

;;; ##########################################################################

(use-package highlight-defined
  :defer t
  :ensure t
  :after prog-mode
  :hook (emacs-lisp-mode . highlight-defined-mode))

;;; ##########################################################################

(use-package popper
  :bind (("C-`"   . popper-toggle)
          ("M-`"   . popper-cycle)
          ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
    '("\\*Messages\\*"
       "\\*scratch\\*"
       "\\*ielm\\*"
       "Output\\*$"
       "\\*Async Shell Command\\*"
       "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
       "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
       "^\\*term.*\\*$"   term-mode   ;term as a popup
       "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
       help-mode
       compilation-mode))
  :config
  (popper-mode +1)
  (popper-echo-mode +1))

;;; ##########################################################################

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc"))

;;; ##########################################################################

;; Use keybindings
(use-package grip-mode
  :bind (:map markdown-mode-command-map
          ("g" . grip-mode)))

;; ;; Or using hooks
;; (use-package grip-mode
;;   :hook ((markdown-mode org-mode) . grip-mode))

;;; ##########################################################################

(defun mifi/set-fill-column-interactively (num)
  "Asks for the fill column."
  (interactive "nfill-column: ")
  (set-fill-column num))

(defun mifi/set-org-fill-column-interactively (num)
  "Asks for the fill column for Org mode."
  (interactive "norg-fill-column: ")
  (setq custom-org-fill-column num)
  (mifi/org-mode-visual-fill)
  (redraw-display))

(defun mifi/jump-to-register ()
  "Asks for a register to jump to."
  (interactive)
  (call-interactively 'jump-to-register))

;;; ##########################################################################

(defun mifi/define-mmm-minor-mode-map ()
  (defvar mmm-keys-minor-mode-map
    (let ((map (make-sparse-keymap)))
      (bind-keys :map map
        ("M-RET $" . jinx-correct)
        ("M-RET ?" . eldoc-box-help-at-point)
        ("M-RET S e" . eshell)
        ("M-RET S i" . ielm)
        ("M-RET S v" . vterm-other-window)
        ("M-RET T +" . next-theme)
        ("M-RET T -" . previous-theme)
        ("M-RET T ?" . which-theme)
        ("M-RET d" . dashboard-open)
        ("M-RET e" . treemacs) ;; e for Explore
        ("M-RET f" . mifi/set-fill-column-interactively)
        ("M-RET p" . pulsar-pulse-line)
        ("M-RET r" . repeat-mode)
        ("M-RET j" . mifi/jump-to-register)
        ("M-RET |" . global-display-fill-column-indicator-mode)
        ("M-RET C-g" . keyboard-quit))
      map)
    "mmm-keys-minor-mode keymap.")

  (which-key-add-key-based-replacements "M-RET |" "display-fill-column")
  (which-key-add-key-based-replacements "M-RET ?" "help-at-point")

  (define-minor-mode mmm-keys-minor-mode
    "A minor mode so that my key settings override annoying major modes."
    :init-value t
    :lighter " m3"))

;; (mifi/define-mmm-minor-mode-map)

;;; ##########################################################################

(defun mifi/mmm-handle-context-keys (&optional winframe)
  "Enable or Disable keys based upon featurep context."
  (when winframe
    (let ((map mmm-keys-minor-mode-map))
      (when enable-thesaurus
        (bind-keys :map map
          ("M-RET m t" . mw-thesaurus-lookup-dwim)))
      (cond
        ((equal major-mode 'org-mode)
          (bind-keys :map map
            ("M-RET M-RET" . org-insert-heading)
            ("M-RET o f" . mifi/set-org-fill-column-interactively)
            ("M-RET o l" . org-toggle-link-display)))
        ((equal major-mode 'python-mode)
          (bind-keys :map map
            ("M-RET P" . 'pydoc-at-point)))
        (t   ;; Default 
          (unbind-key "M-RET o f" map)
          (unbind-key "M-RET o l" map)
          (unbind-key "M-RET P ?" map)
          (unbind-key "M-RET M-RET" map)))))

  ;; Override default menu text with better things
  (which-key-add-key-based-replacements "M-RET m t" "thesaurus-at-point")
  (which-key-add-key-based-replacements "M-RET o" "org-menu")
  (which-key-add-key-based-replacements "M-RET o f" "set-org-fill-column"))

;;; ##########################################################################

(defun mifi/mmm-update-menu (&optional winframe)
  (interactive)
  (mifi/mmm-handle-context-keys nil)
  (which-key-add-key-based-replacements "M-RET S" "shells")
  (which-key-add-key-based-replacements "M-RET T" "theme-keys")
  (which-key-add-key-based-replacements "M-RET P" "python-menu")
  (which-key-add-key-based-replacements "M-RET e" "treemacs-toggle")
  (which-key-add-key-based-replacements "M-RET m" "Thesaurus")
  (which-key-add-key-based-replacements "M-RET f" "set-fill-column")
  (which-key-add-key-based-replacements "M-RET j" "jump-to-register")
  (which-key-add-key-based-replacements "M-RET C-g" "Exit menu")
  (which-key-add-key-based-replacements "M-RET" "Mitch's Menu"))

;;; ##########################################################################

;; Check the keys when:
;; - the whick-key menu is displayed
(add-hook 'which-key-inhibit-display-hook 'mifi/mmm-update-menu)
;; - the user updates/changes the buffer - like loading a file
;;   (but not switching to a new buffer)
(add-hook 'window-buffer-change-functions 'mifi/mmm-handle-context-keys)
;; - the user switches windows
(add-hook 'window-selection-change-functions 'mifi/mmm-handle-context-keys)

;; (add-hook 'which-key-mode-hook #'mifi/after-which-key)

;; (add-hook 'elpaca-after-init-hook #'mifi/after-which-key)
  ;; (lambda ()
  ;;   (mifi/after-which-key)
  ;;   (mifi/define-mmm-minor-mode-map)
  ;;   (mifi/set-recenter-keys)))

;;; ##########################################################################

(defun mifi/config-landing ()
  (setq-default initial-scratch-message
    (format
      ";; Hello, World and Happy hacking %s!\n%s\n\n" user-login-name
      ";; Press M-RET (Meta-RET) to open Mitch's Context Aware Menu"))
  (cond
    ((equal default-landing-mode 'landing-mode-dashboard)
      (dashboard-open))
    ((equal default-landing-mode 'landing-mode-scratch)
      (switch-to-buffer "*scratch*")
      (end-of-buffer))
    ((equal default-landing-mode 'landing-mode-ielm)
      (ielm))
    ((equal default-landing-mode 'landing-mode-eshell)
      (eshell)))  )
  ;; (add-hook 'lisp-interaction-mode-hook
  ;;   (lambda ())))

(add-hook 'elpaca-after-init-hook
  (lambda ()
    (mifi/config-landing)
    (mifi/set-recenter-keys)))

;;; ##########################################################################

(defun mifi/cleanup-when-exiting ()
  (let ((backdir (format "%s/config-backup" working-files-directory)))
    (make-directory backdir t)
    ;; Backup init.el
    (copy-file
      (expand-file-name "init.el" emacs-config-directory)
      (expand-file-name "init.el" backdir) t)))
;; (copy-file
;;   (expand-file-name "emacs-config.org" emacs-config-directory)
;;   (expand-file-name "emacs-config.org" backdir) t)))

(add-hook 'kill-emacs-hook #'mifi/cleanup-when-exiting)

;;; ##########################################################################
;; Ignore Line Numbers for the following modes:

;; Line #'s appear everywhere if global-display-line-numbers-mode is set
;; ... except for when in these modes
(when (equal global-display-line-numbers-mode t)
  (dolist (mode '( dashboard-mode-hook
                   helpful-mode-hook
                   eshell-mode-hook
                   eww-mode-hook
                   help-mode-hook
                   org-mode-hook
                   shell-mode-hook
                   term-mode-hook
                   treemacs-mode-hook
                   vterm-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

;;; ##########################################################################
;; Supress common annoying warning.
;; These can still be found in the  *Warnings* buffer
(setq warning-suppress-types '((package reinitialization)
                                (package-initialize)
                                (package)
                                (use-package)
                                (python-mode)))

;;; ##########################################################################

;;; init.el ends here.
