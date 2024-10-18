;;; init.el -*- flycheck-disabled-checkers: (emacs-lisp); lexical-binding: nil -*-
;;;
;;; Commentary:

;; This file bootstraps the configuration which is generated from tangling an org-mode file.
;; DO NOT MODIFY this file directly as changes will be overwritten.
;; The source this file is generated from is from "emacs-config-elpa.org"

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)
;;

;;; ##########################################################################
;;; Define my customization groups

(defgroup mifi-config nil
  "M.R. Fisher's configuration section."
  :group 'Local)

(defgroup mifi-config-toggles nil
  "A set of toggles that enable or disable specific packages or behaviors."
  :group 'mifi-config)

(defgroup mifi-config-features nil
  "Customization from a selection of a curated list of features and handlers."
  :group 'mifi-config)

(defgroup mifi-config-fonts nil
  "Customization of fonts and font sizes."
  :group 'mifi-config)

(defgroup mifi-config-theming nil
  "Custom theming list and list index values."
  :group 'mifi-config)

;;; ##########################################################################

(defcustom custom-emacs-home
  (expand-file-name
    (format "emacs%d-home" emacs-major-version emacs-minor-version) "~/")
  "The base directory to where emacs user-operation files are stored. This is
in contrast to the `emacs-config-directory' where all the initialization and
configuration of Emacs are stored."
  :type 'string
  :group 'mifi-config)

(defcustom custom-docs-directory
  (expand-file-name "emacs-docs" custom-emacs-home)
  "A directory used to store documents and customized data."
  :type 'string
  :group 'mifi-config)

(defcustom custom-developer-root
  (expand-file-name "Developer/src" "~/")
  "The root of all development projects. Used when initializing project.el or
     projectile."
  :type 'string
  :group 'mifi-config)

(defcustom working-files-directory
  (expand-file-name "emacs-working-files" custom-emacs-home)
  "The directory where to store Emacs working files. `user-emacs-directory'
will also be set to this directory. The starting user-emacs-directory will
become `emacs-config-directory'."
  :type 'string
  :group 'mifi-config)

(defcustom custom-org-fill-column 120
  "The fill column width for Org mode text.
     Note that the text is also centered on the screen so that should
     be taken into consideration when providing a width."
  :type 'natnum
  :group 'mifi-config)

;;; ##########################################################################
;;; Feature Toggles

(defcustom enable-centaur-tabs nil
  "Set to t to enable `centaur-tabs' which uses tabs to represent open buffer."
  :type 'boolean
  :group 'mifi-config-toggles)

(defcustom enable-embark nil
  "Set to t to enable the Embark package."
  :type 'boolean
  :group 'mifi-config-toggles)

(defcustom enable-frameset-restore t
  "Set to t to enable restoring the last Emacs window size and position
   upon startup."
  :type 'boolean
  :group 'mifi-config-toggles)

(defcustom enable-gb-dev nil
  "If set to t, the z80-mode and other GameBoy related packages
    will be enabled."
  :type 'boolean
  :group 'mifi-config-toggles)

(defcustom enable-golden-ratio nil
  "Set to t to enable `golden-ratio-mode' which resizes the active buffer
   window to the dimensions of a golden-rectangle"
  :type 'boolean
  :group 'mifi-config-toggles)

(defcustom enable-ocaml nil
  "Set to t to enable inclusion of OCaml support: Merlin, Tuareg."
  :type 'boolean
  :group 'mifi-config-toggles)

(defcustom enable-org-fill-column-centering nil
  "Set to t to center the visual-fill column of the Org display."
  :type 'boolean
  :group 'mifi-config-toggles)

(defcustom enable-python t
  "Set to t to enable Python language support."
  :type 'boolean
  :group 'mifi-config-toggles)

(defcustom enable-neotree nil
  "Set to t to enable the `neotree' package."
  :type 'boolean
  :group 'mifi-config-toggles)

(defcustom enable-thesaurus t
  "When set to t, enables the Merriam-Webster Thesaurus."
  :type 'boolean
  :group 'mifi-config-toggles)

(defcustom enable-ts nil
  "Set to t to enable TypeScript handling."
  :type 'boolean
  :group 'mifi-config-toggles)

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
interpreter."
  :type '(radio
           (const :tag "Dashboard" landing-mode-dashboard)
           (const :tag "*scratch*" landing-mode-scratch)
           (const :tag "IELM" landing-mode-ielm)
           (const :tag "eshell" landing-mode-eshell))
  :group 'mifi-config-features)

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
  :group 'mifi-config-features)

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
           (const :tag "Vertico, Orderless, Consult, Embark completion system." comphand-vertico)
           (const :tag "Ivy, Counsel, Swiper completion systems" comphand-ivy)
         (const :tag "Corfu, Orderless, Cape" comphand-corfu)
           (const :tag "Built-in Ido" comp-hand-ido))
  :group 'mifi-config-features)

;; The debug-adapter used to also support DAPE. For now, that option has been
;; removed since DAP has more language coverage - especially for OCaml.
;; However, the variable will remain as an option so that not all code has to
;; be changed plus it allows for future debug-adapter support if a new one
;; becomes supported in this configuration.
(defcustom debug-adapter 'debug-adapter-dap-mode
  "Select the debug adapter to use for debugging applications.  dap-mode is an
Emacs client/library for Debug Adapter Protocol is a wire protocol for
communication between client and Debug Server. It’s similar to the LSP but
provides integration with debug server."
  :type '(radio
           (const :tag "Debug Adapter Protocol (DAP)" debug-adapter-dap-mode))
  :group 'mifi-config-features)

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
  :group 'mifi-config-features)

(defcustom custom-project-handler 'custom-project-project-el
  "Select which project handler to use."
  :type '(radio (const :tag "Projectile" custom-project-projectile)
           (const :tag "Built-in project.el" custom-project-project-el))
  :group 'mifi-config-features)

(defcustom custom-note-system 'custom-note-system-denote
  "Select which note-taking/knowledge system will be used.

The simpler, more efficient and lightweight for just simple note is `denote'.
`Denote' is a simple note-taking tool for Emacs. It is based on the idea that
notes should follow a predictable and descriptive file-naming scheme. The file
name must offer a clear indication of what the note is about, without reference
to any other metadata. Denote basically streamlines the creation of such files
while providing facilities to link between them.

A more full-featured note and other productivity tools like agenda, and todo is
`org-roam'. Org-roam allows for effortless non-hierarchical note-taking: with
Org-roam, notes flow naturally, making note-taking fun and easy. Org-roam
augments the Org-mode syntax, and will work for anyone already using Org-mode
for their personal wiki."
  :type '(radio
           (const :tag "Denote" custom-note-system-denote)
           (const :tag "Org-roam" custom-note-system-org-roam)
           (const :tag "None" custom-note-system-none))
  :group 'mifi-config-features)

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
  :group 'mifi-config-theming
  :type '(repeat string))

(defcustom default-terminal-theme "sanityinc-tomorrow-bright"
  "The default theme used for a terminal invocation of Emacs."
  :group 'mifi-config-theming
  :type 'string)

(defcustom theme-selector 0
  "The index into the list of custom themes."
  :group 'mifi-config-theming
  :type 'natnum)

;;; Font related
(defcustom default-font-family "Courier New"
  "The font family used as the default font."
  :type 'string
  :group 'mifi-config-fonts)

(defcustom mono-spaced-font-family "Monaco"
  "The font family used as the mono-spaced font."
  :type 'string
  :group 'mifi-config-fonts)

(defcustom variable-pitch-font-family "Helvetica"
  "The font family used as the default proportional font."
  :type 'string
  :group 'mifi-config-fonts)

(defcustom small-mono-font-size 150
  "The small font size in pixels."
  :type 'natnum
  :group 'mifi-config-fonts)

(defcustom medium-mono-font-size 170
  "The medium font size in pixels."
  :type 'natnum
  :group 'mifi-config-fonts)

(defcustom large-mono-font-size 190
  "The large font size in pixels."
  :type 'natnum
  :group 'mifi-config-fonts)

(defcustom x-large-mono-font-size 220
  "The extra-large font size in pixels."
  :type 'natnum
  :group 'mifi-config-fonts)

(defcustom small-variable-font-size 170
  "The small font size in pixels."
  :type 'natnum
  :group 'mifi-config-fonts)

(defcustom medium-variable-font-size 190
  "The small font size in pixels."
  :type 'natnum
  :group 'mifi-config-fonts)

(defcustom large-variable-font-size 210
  "The small font size in pixels."
  :type 'natnum
  :group 'mifi-config-fonts)

(defcustom x-large-variable-font-size 240
  "The small font size in pixels."
  :type 'natnum
  :group 'mifi-config-fonts)

(defcustom custom-default-font-size 170
  "A place to store the most current (face-attribute 'default :height).  This
is specifically for the mono-spaced and default font. The variable type-face
font size is computed + 20 of this value."
  :type 'natnum
  :group 'mifi-config-fonts)

(defvar custom-default-mono-font-size 170
  "Storage for the current mono-spaced font height.")

(defvar theme-did-load nil
  "Set to true if the last Theme was loaded.")

;;; ##########################################################################

(defun mifi/validate-variable-pitch-font ()
  (when (display-graphic-p)
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
    (message (format ">>> variable-pitch font is %s" variable-pitch-font-family))))

;;; ##########################################################################

(defun mifi/validate-monospace-font ()
  (when (display-graphic-p)
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
    (message (format ">>> monospace font is %s" mono-spaced-font-family))))

(use-package use-package-ensure-system-package :ensure t :demand t)

;;; ##########################################################################

;;; Set a variable that represents the actual emacs configuration directory.
;;; This is being done so that the user-emacs-directory which normally points
;;; to the .emacs.d directory can be re-assigned so that customized files don't
;;; pollute the configuration directory. This is where things like YASnippet
;;; snippets are saved and also additional color themese are stored.

(defvar emacs-config-directory user-emacs-directory)

;;; Put any emacs cusomized variables in a special file. Load this file early
;;; since things like the working-files-directory or custom-docs-directory
;;; customized values could be in this file.
(setq custom-file (expand-file-name "customized-vars.el" emacs-config-directory))

(unless (file-exists-p custom-file) ;; create custom file if it doesn't exists
  (write-region "" nil custom-file))
(load custom-file 'noerror 'nomessage)

;;;
;;; This directory stores any files that are used by the user to store
;;; additional Emacs files, like themes or specialized moduls. This is
;;; where emacs-config files are backed up to. Of course, any document that the
;;; user wants to associate with an Emacs installation can be stored here.
(message ">>> working-files-dir = %s" working-files-directory)
(make-directory working-files-directory t)

;;; user-emacs-directory always ends in a "/" so we need to make the
;;; working-files-directory act the same since it becomes the new
;;; user-emacs-directory. So, add a "/" if there isn't one already.
(unless (string-suffix-p "/" working-files-directory)
  (setq working-files-directory (concat working-files-directory "/")))

;;; Point the user-emacs-directory to the new working directory
(setq user-emacs-directory working-files-directory)

;;; Add an additional INFO dir for custom info docs
(let ((infodir (expand-file-name "share/info" custom-docs-directory)))
  (unless (file-exists-p infodir)
    (make-directory infodir t)))

;; ensure that the loaded font values are supported by this OS. If not, try
;; to correct them.
(mifi/validate-variable-pitch-font)
(mifi/validate-monospace-font)

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
  :ensure t
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

(use-package f
  :ensure t
  :demand t
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
  :ensure nil
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

(defun mifi/setup-common-registers ()
  "Define some common registers."
  (setq register-preview-delay 0) ;; Show registers ASAP
  (set-register ?o (cons 'file (concat emacs-config-directory "emacs-config-elpa.org")))
  (set-register ?O (cons 'file (concat emacs-config-directory "emacs-config.org")))
  (set-register ?G '(file . "~/Developer/game-dev/GB_asm"))
  (set-register ?S (cons 'file (concat emacs-config-directory "org-files/important-scripts.org"))))

(add-hook 'after-init-hook
  (lambda () (mifi/setup-common-registers)))

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
    (use-package server :ensure nil)
    (unless (server-running-p)
      (server-start))))

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

(use-package system-packages :ensure t)

;;; ##########################################################################

(use-package jsonrpc
  :ensure t)
  ;; :config
  ;; For some odd reason, it is possible that jsonrpc will try to load a
  ;; theme. (jsonrpc/lisp/custom.el:1362). If our theme hasn't been loaded
  ;; yet, go ahead and try. This could prevent a startup without the theme
  ;; properly loaded.
  ;; (unless theme-did-load
  ;;   (mifi/load-theme-from-selector)))

;; All kept in local /lisp directory.
;; (use-package web-server-status-codes )
;; (use-package simple-httpd )
;; (use-package web-server )

;;; ##########################################################################

(use-package helpful
  :ensure t
  ;; :commands (helpful-callable helpful-variable helpful-command helpful-key helpful-function)
  :config
  (bind-keys
    ([remap describe-command] . helpful-command)
    ([remap describe-function] . helpful-function)
    ([remap describe-variable] . helpful-variable)
    ([remap describe-key] . helpful-key)))

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

(use-package eldoc)

(use-package eldoc-box
  :delight DocBox
  :hook (after-init . mifi/setup-hooks-for-eldoc))

;;; ##########################################################################

(use-package hydra
  :ensure t)
  ;; :vc (:url "https://github.com/abo-abo/hydra" :ignored-files ("lv.el")))
  ;; :ensure (:repo "abo-abo/hydra" :fetcher github
  ;;           :files (:defaults (:exclude "lv.el"))))

;;; ##########################################################################

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
          ("C->" . mc/mark-next-like-this)
          ("C-<" . mc/mark-previous-like-this)
          ("C-c C-<" . mc/mark-all-like-this)))

;;; ##########################################################################

(use-package visual-fill-column
  :ensure nil ;; Should be installed in the local lisp dir.
  :after org)

(use-package writeroom-mode
  :ensure t
  :defer t
  :after visual-fill-column)

;;; ##########################################################################
;;; Default keys are C-M-= or C-M--

(use-package default-text-scale
  :ensure t
  :hook (after-init . default-text-scale-mode))

;;; ##########################################################################

(defun mifi/set-mac-modifier-keys ()
  (interactive)
  ;; Macintosh specific configurations.
  (when *is-a-mac*
    (setq mac-command-modifier   'meta
      mac-option-modifier        'super
      mac-control-modifier       'control
      mac-right-command-modifier 'meta
      mac-right-control-modifier 'hyper)))

(add-hook 'after-init-hook #'mifi/set-mac-modifier-keys)

;;; ##########################################################################

(defun mifi/setup-global-keybindings ()
  (interactive)
  (bind-key "C-c ]" 'indent-region prog-mode-map)
  (bind-key "C-c }" 'indent-region prog-mode-map) 
  (bind-key "C-x C-j" 'dired-jump)

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

(use-package evil-nerd-commenter
  :ensure t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(add-hook 'emacs-startup-hook #'mifi/setup-global-keybindings)

;;; ##########################################################################
;;; Automatic Package Updates

(use-package auto-package-update
  ;; :ensure (:fetcher github :repo "rranelli/auto-package-update.el")
  :ensure t
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
  :ensure t
  :defer t
  :bind (:map yas-minor-mode-map
          ("<C-'>" . yas-expand))
  :config
  (setq yas-global-mode t)
  (setq yas-minor-mode t)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (add-to-list #'yas-snippet-dirs (expand-file-name "Snippets" custom-docs-directory))
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)
  (setq yas-prompt-functions '(yas-ido-prompt))
  (defun help/yas-after-exit-snippet-hook-fn ()
    (prettify-symbols-mode))
  (add-hook 'yas-after-exit-snippet-hook #'help/yas-after-exit-snippet-hook-fn))

;;; ##########################################################################

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;; ##########################################################################

(use-package all-the-icons :ensure t)

;;; ##########################################################################

(use-package ace-window
  :ensure t
  ;; :vc (:url "https://github.com/abo-abo/ace-window")
  ;;:ensure (:repo "abo-abo/ace-window" :fetcher github)
  :bind ("M-o" . ace-window))

;;; ##########################################################################
;;; Window Number

(use-package winum
  :config (winum-mode))

(use-package init-windows ;; From purcell
  :ensure nil
  :demand t
  :hook (after-init . winner-mode))

;;; ##########################################################################

(use-package dashboard
  :ensure t
  :defer t
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
  :bind ("M-RET d" . dashboard-open)
  :config
  ;; (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (add-hook 'after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'after-init-hook #'dashboard-initialize)
  (when (equal custom-project-handler 'custom-project-projectile)
    (setq dashboard-projects-backend 'projectile))
  (setq dashboard-startup-banner (expand-file-name "Emacs-modern-is-sexy-v1.png" user-emacs-directory))
  (dashboard-setup-startup-hook))

;;; ##########################################################################

(use-package jinx
  ;; :vc (:url "https://github.com/minad/jinx")
  ;; :ensure (:host github :repo "minad/jinx")
  ;;:hook (emacs-startup . global-jinx-mode)
  :ensure t
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
  :after (:any ivy vertico corfu)
  :defer t)

(use-package company-prescient
  :after prescient)

;;; ##########################################################################

(use-package orderless
  :when (or (or (equal completion-handler 'comphand-vertico)
                (equal completion-handler 'comphand-ivy))
            (equal completion-handler 'comphand-corfu))
  :after (:any ivy swiper vertico counsel corfu)
  :ensure t
  :custom
  (when (equal completion-handler 'comphand-ivy)
    (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
    (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight)))
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; ##########################################################################
;;; Swiper and IVY mode

(use-package ivy
  :when (equal completion-handler 'comphand-ivy)
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
  :when (equal completion-handler 'comphand-ivy)
  :after ivy
  :init
  (ivy-rich-mode 1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package ivy-yasnippet
  :when (equal completion-handler 'comphand-ivy)
  :after (:any yasnippet ivy))
;; :ensure (:host github :repo "mkcms/ivy-yasnippet"))

;;; ##########################################################################

(use-package swiper
  :when (equal completion-handler 'comphand-ivy)
  :after ivy)

;;; ##########################################################################

(use-package counsel
  :when (equal completion-handler 'comphand-ivy)
  :ensure t
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
  :when (equal completion-handler 'comphand-ivy)
  :after (ivy prescient)
  :ensure t
  :custom
  (prescient-persist-mode t)
  (ivy-prescient-mode t)
  (ivy-prescient-enable-filtering t))

;;; ##########################################################################

;; Don't use lsp-bridge with company as lsp-bridge already provides the same
;; features. They actually collide.

(defun mifi/company-config ()
  (interactive)
  (bind-keys :map company-active-map
    ("C-n". company-select-next)
    ("C-p". company-select-previous)
    ("M-<". company-select-first)
    ("M->". company-select-last)
    ("<tab>" . company-complete-selection))
  (global-company-mode 1)
  (when (featurep 'prescient)
    (company-prescient-mode 1)))

(use-package company
  :unless (equal custom-ide 'custom-ide-lsp-bridge)
  ;; :after tree-sitter
  :ensure t
  :delight company-mode
  :config (mifi/company-config)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.5))

;; IMPORTANT:
;; Don't use company at all if lsp-bridge is active.
;; lsp-bridge already provides similar functionality.

;; :config
;; (add-to-list 'company-backends 'company-yasnippet))

;;; ##########################################################################

;; (require 'company-box)
;; (add-hook 'company-mode-hook 'company-box-mode)

(use-package company-box
  :ensure t
  :after company
  :delight 'cb
  ;; :vc (:url "https://github.com/sebastiencs/company-box.git")
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
  :ensure t
  :when (equal completion-handler 'comphand-corfu)
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
  :when (equal completion-handler 'comphand-corfu)
  :after corfu
  :ensure t
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
  :config
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
  :when (equal completion-handler 'comphand-corfu)
  :ensure t
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
  ;; (when (featurep 'prescient)
  ;;   (vertico-prescient-mode 0))
  ;; :bind ("C-x C-f" . ido-find-file)
  ;; Clean up file path when typing
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)
          ;; Make sure vertico state is saved
          (minibuffer-setup . vertico-repeat-save)))

;;; ##########################################################################

(use-package marginalia
  :when (or (equal completion-handler 'comphand-vertico)
          (equal completion-handler 'comphand-corfu))
  :ensure t
  ;; :commands marginalia-mode
  :custom
  (marginalia-max-relative-age 60)
  (marginalia-align 'left)
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode t))

;;; ##########################################################################

(use-package all-the-icons-completion
  :ensure t
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
  :after (vertico prescient)
  :config (vertico-prescient-mode t))

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

(add-hook 'after-init-hook
  (lambda ()
    (use-package ido
      :when (equal completion-handler 'comp-hand-ido)
      :ensure nil
      :config
      (ido-everywhere t))))

;;; ##########################################################################

(use-package embark
  :when (equal completion-handler 'comphand-vertico)
  :ensure t
  :defer t
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
  :ensure t
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
  :ensure t
  :defer t
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
;;  Extensions to Dired.
;;
;;  This file extends functionalities provided by standard GNU Emacs
;;  files `dired.el', `dired-aux.el', and `dired-x.el'.
;;
;;  Key bindings changed.  Menus redefined.  `diredp-mouse-3-menu'
;;  popup menu added.  New commands.  Some commands enhanced.
;;
;;  All of the new functions, variables, and faces defined here have
;;  the prefix `diredp-' (for Dired Plus) in their names.  
(use-package dired+
  :ensure nil
  :config
  (diredp-toggle-find-file-reuse-dir 1))

;;; ##########################################################################

(defun mifi/dired-ediff-marked-files ()
  "Compare two marked files in Dired with ediff."
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (unless (= (length marked-files) 2)
      (error "You need to mark exactly two files to compare."))
    (ediff-files (car marked-files) (cadr marked-files))))

(defun mifi/ediff-bsh ()
  "Function to be called before any buffers or window setup for
      ediff."
  (setq mifi/ediff-bwin-config (current-window-configuration))
  (when (characterp mifi/ediff-bwin-reg)
    (set-register mifi/ediff-bwin-reg
      (list mifi/ediff-bwin-config (point-marker)))))

(defun mifi/ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq mifi/ediff-awin-config (current-window-configuration))
  (when (characterp mifi/ediff-awin-reg)
    (set-register mifi/ediff-awin-reg
      (list mifi/ediff-awin-config (point-marker)))))

(defun mifi/ediff-qh ()
  "Function to be called when ediff quits."
  (when mifi/ediff-bwin-config
    (set-window-configuration mifi/ediff-bwin-config)))

;;; ##########################################################################

;; Restore window configuration after ediff exits
;;   URL: https://www.emacswiki.org/emacs/EdiffMode

(defvar mifi/ediff-bwin-config nil "Window configuration before ediff.")
(defcustom mifi/ediff-bwin-reg ?b
  "*Register to be set up to hold `mifi/ediff-bwin-config' configuration.")

(defvar mifi/ediff-awin-config nil "Window configuration after ediff.")
(defcustom mifi/ediff-awin-reg ?e
  "*Register to be used to hold `mifi/ediff-awin-config' window configuration.")

;;; ##########################################################################

(use-package dired
  ;;   ;; local package hint for elpaca
  :ensure nil
  :no-require t
  :bind (:map dired-mode-map
        ("=" . mifi/dired-ediff-marked-files)))

(use-package ediff
  ;;  ;; local package hint for elpaca
  :no-require t
  :custom
  (ediff-diff-options "-w")
  ;; Split windows so that they are compared horizontally
  (ediff-split-window-function 'split-window-horizontally)
  :hook
  (ediff-before-setup . mifi/ediff-bsh)
  (ediff-after-setup-window . (lambda () (mifi/ediff-ash 'append)))
  (ediff-quit . mifi/ediff-qh))

;;; ##########################################################################
;;; Treemacs

(use-package treemacs
  :after (:all winum ace-window)
  :ensure t
  :bind (:map global-map
          ("M-0"       . treemacs-select-window)
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
  :when (display-graphic-p))

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

(bind-keys
  ;; Go to NEXT theme
  ("M-RET =" . next-theme)
  ;; Go to PREVIOUS theme
  ("M-RET -" . previous-theme)
  ;; Message current theme
  ("M-RET _" . which-theme))

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

(add-hook 'after-init-hook 'mifi/customize-modus-theme)

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
(add-hook 'after-init-hook 'mifi/customize-ef-theme)

;;; ##########################################################################

(add-to-list 'custom-theme-load-path (expand-file-name "Themes" custom-docs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "lisp" emacs-config-directory))

(mifi/org-theme-override-values)
(use-package tron-legacy-theme :defer t :ensure t)
(use-package ef-themes :init (mifi/customize-ef-theme) :defer t :ensure t)
(use-package modus-themes :init (mifi/customize-modus-theme) :defer t :ensure t)
(use-package material-theme :defer t :ensure t)
(use-package color-theme-modern :defer t :ensure t)
(use-package color-theme-sanityinc-tomorrow :defer t :ensure t)
;; Can't defer darktooth since we need the base theme to always load
(use-package darktooth-theme :ensure t)
(use-package zenburn-theme :defer t :ensure t)

;;; ##########################################################################
;; (add-hook 'emacs-startup-hook #'(mifi/load-theme-from-selector))
;; (mifi/load-theme-from-selector)
;; For terminal mode we choose Material theme

(defun mifi/load-terminal-theme ()
  (load-theme (intern default-terminal-theme) t))

(unless (display-graphic-p)
  (add-hook 'after-init-hook 'mifi/load-terminal-theme)
  ;;else
  (progn
    (if (not after-init-time)
      (add-hook 'after-init-hook
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
  :ensure t
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

(add-hook 'after-init-hook
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
        mifi/default-variable-font-size (+ custom-default-font-size 20))
      (mifi/update-face-attribute))
    ((equal mifi/font-size-slot 2)
      (setq custom-default-font-size mifi/large-font-size
        custom-default-mono-font-size mifi/large-mono-font-size
        mifi/default-variable-font-size (+ custom-default-font-size 20))
      (mifi/update-face-attribute))
    ((equal mifi/font-size-slot 1)
      (setq custom-default-font-size mifi/medium-font-size
        custom-default-mono-font-size mifi/medium-mono-font-size
        mifi/default-variable-font-size (+ custom-default-font-size 20))
      (mifi/update-face-attribute))
    ((equal mifi/font-size-slot 0)
      (setq custom-default-font-size mifi/small-font-size
        custom-default-mono-font-size mifi/small-mono-font-size
        mifi/default-variable-font-size (+ custom-default-font-size 20))
      (mifi/update-face-attribute))))

;;; ##########################################################################
;; Some alternate keys below....

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
    (which-key-add-key-based-replacements
      "C-S-c 1" "recenter-with-small-font"
      "C-S-c 2" "recenter-with-medium-font"
      "C-S-c 3" "recenter-with-large-font"
      "C-S-c 4" "recenter-with-x-large-font")))

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

(defun mifi/update-other-modes-font ()
  "This updates/calls functions to update mode font sizes."
  (when (featurep 'org)
    (mifi/org-font-setup)))

;;; ##########################################################################

(defun use-small-display-font (&optional force-recenter)
  (interactive)
  (mifi/set-frame-font 0)
  (mifi/reset-if-spacious-padding-mode)
  (mifi/update-other-modes-font)
  (mifi/should-recenter force-recenter))
(defun use-small-display-font-t () (interactive) (use-small-display-font t))

(defun use-medium-display-font (&optional force-recenter)
  (interactive)
  (mifi/set-frame-font 1)
  (mifi/reset-if-spacious-padding-mode)
  (mifi/update-other-modes-font)
  (mifi/should-recenter force-recenter))
(defun use-medium-display-font-t () (interactive) (use-medium-display-font t))

(defun use-large-display-font (&optional force-recenter)
  (interactive)
  (mifi/set-frame-font 2)
  (mifi/reset-if-spacious-padding-mode)
  (mifi/update-other-modes-font)
  (mifi/should-recenter force-recenter))
(defun use-large-display-font-t () (interactive) (use-large-display-font t))

(defun use-x-large-display-font (&optional force-recenter)
  (interactive)
  (mifi/set-frame-font 3)
  (mifi/reset-if-spacious-padding-mode)
  (mifi/update-other-modes-font)
  (mifi/should-recenter force-recenter))
(defun use-x-large-display-font-t () (interactive) (use-x-large-display-font t))

;;; ##########################################################################
;; This is done so that the Emacs window is sized early in the init phase along with the default font size.
;; Startup works without this but it's nice to see the window expand early...
(when (display-graphic-p)
  (add-hook 'after-init-hook
    (lambda ()
      (progn
        (mifi/update-face-attribute)
        (unless (daemonp)
          (unless enable-frameset-restore (mifi/frame-recenter))))
      )))

;;; ##########################################################################

(use-package spacious-padding
  :ensure t
  :custom
  (spacious-padding-widths
    '( :internal-border-width 15
       :header-line-width 4
       :mode-line-width 6
       :tab-width 4
       :right-divider-width 30
       :scroll-bar-width 8
       :fringe-width 8))
  :config
  (spacious-padding-mode t))

;; Read the doc string of `spacious-padding-subtle-mode-line' as it
;; is very flexible and provides several examples.
;; (setq spacious-padding-subtle-mode-line
;;       `( :mode-line-active 'default
;;          :mode-line-inactive vertical-border))

;;; ##########################################################################

(use-package faces)
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

  (set-face-attribute 'org-verbatim nil
    :foreground 'unspecified
    :font mono-spaced-font-family
    :height custom-default-mono-font-size
    :inherit 'fixed-pitch)

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

(defun mifi/toggle-org-centering ()
  (interactive)
  (setq-default enable-org-fill-column-centering
    (not enable-org-fill-column-centering))
  (org-mode-restart))

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

;;; ##########################################################################

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

;;; ##########################################################################

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
  :ensure t
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
         (haskell . t)
         (tuareg . t)
         (latex . t)
         (ledger . t)
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

;;; ##########################################################################

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

(use-package emacsql :ensure t)

(use-package org-roam
  :after emacsql
  :when (equal custom-note-system 'custom-note-system-org-roam)
  :ensure t
  :demand t
  ;; :ensure ( :package "org-roam" :source "MELPA" :protocol https :inherit t :depth 1
  ;;           :fetcher github :repo "org-roam/org-roam" :files (:defaults "extensions/*"))
  :init
  (setq org-roam-v2-ack t)
  (which-key-add-key-based-replacements "C-c n" "org-roam")
  :custom
  (org-roam-directory (expand-file-name "RoamNotes" custom-docs-directory))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
          ("C-c n f" . org-roam-node-find)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n I" . org-roam-node-insert-immediate)
          ("C-c n p" . mifi/org-roam-find-project)
          ("C-c n t" . mifi/org-roam-capture-task)
          ("C-c n b" . mifi/org-roam-capture-inbox)
          :map org-mode-map
          ("C-M-i" . completion-at-point))
  :config
  (mifi/org-roam-set-which-key-replacements)
  (mifi/org-roam-refresh-agenda-list)
  (add-to-list 'org-after-todo-state-change-hook
    (lambda ()
      (when (equal org-state "DONE")
        (mifi/org-roam-copy-todo-to-today))))
  (org-roam-db-autosync-mode))

(defun mifi/org-roam-set-which-key-replacements ()
  (interactive)
  (which-key-add-key-based-replacements
    "C-c n l" "toggle-buffer"
    "C-c n f" "find-node"
    "C-c n i" "insert-node"
    "C-c n I" "insert-node-immediate"
    "C-c n p" "find-project"
    "C-c n t" "capture-task"
    "C-c n b" "capture-inbox"))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
         (org-roam-capture-templates
           (list (append (car org-roam-capture-templates)
                   '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(use-package org-roam-dailies
  :when (equal custom-note-system 'custom-note-system-org-roam)
  ;; :vc  ( :url "https://github.com/org-roam/org-roam"
  ;; 	 :main-file "extensions/org-roam-dailies.el")
  :ensure t
  :after org-roam
  :init
  (which-key-add-key-based-replacements "C-c n d" "org-roam-dailies")
  ;; :ensure ( :package "org-roam-dailies" :source "MELPA" :protocol https :inherit t :depth 1
  ;;           :fetcher github :repo "org-roam/org-roam" :files ("extensions/*"))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :bind (:map org-roam-dailies-map
	  ("." . org-roam-dailies-goto-date)
          ("Y" . org-roam-dailies-capture-yesterday)
          ("T" . org-roam-dailies-capture-tomorrow))
  :config
  (which-key-add-key-based-replacements
    "C-c n d" "org-roam-dailies"
    "C-c n d ." "goto-date"
    "C-c n d Y" "capture-yesterday"
    "C-c n d T" "capture-tomorrow"
    "C-c n d n" "capture-today"
    "C-c n d d" "goto-today"
    "C-c n d t" "goto-tomorrow"
    "C-c n d y" "goto-yesterday"))

;;; ##########################################################################
;; The buffer you put this code in must have lexical-binding set to t!
;; See the final configuration at the end for more details.

(defun mifi/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun mifi/org-roam-list-notes-by-tag (tag-name)
    (mapcar #'org-roam-node-file
        (seq-filter
            (mifi/org-roam-filter-by-tag tag-name)
            (org-roam-node-list))))

(defun mifi/org-roam-refresh-agenda-list ()
    (interactive)
    (setq org-agenda-files (mifi/org-roam-list-notes-by-tag "Project")))

;; Build the agenda list the first time for the session

;;; ##########################################################################

(defun mifi/org-roam-project-finalize-hook ()
    "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
    ;; Remove the hook since it was added temporarily
    (remove-hook 'org-capture-after-finalize-hook #'mifi/org-roam-project-finalize-hook)

    ;; Add project file to the agenda list if the capture was confirmed
    (unless org-note-abort
        (with-current-buffer (org-capture-get :buffer)
            (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun mifi/org-roam-find-project ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'mifi/org-roam-project-finalize-hook)

    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
        nil
        nil
        (mifi/org-roam-filter-by-tag "Project")
        :templates
        '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
              :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
              :unnarrowed t))))

(global-set-key (kbd "C-c n p") #'mifi/org-roam-find-project)

;;; ##########################################################################

(defun mifi/org-roam-capture-inbox ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create)
        :templates '(("i" "inbox" plain "* %?"
                         :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

;;; ##########################################################################

(defun mifi/org-roam-capture-task ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'mifi/org-roam-project-finalize-hook)

    ;; Capture the new task, creating the project file if necessary
    (org-roam-capture- :node (org-roam-node-read nil
                                 (mifi/org-roam-filter-by-tag "Project"))
        :templates '(("p" "project" plain "** TODO %?"
                         :if-new
                         (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                             ("Tasks"))))))

;;; ##########################################################################

(defun mifi/org-roam-copy-todo-to-today ()
    (interactive)
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
             (org-roam-dailies-capture-templates
                 '(("t" "tasks" entry "%?"
                       :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
             (org-after-refile-insert-hook #'save-buffer)
             today-file pos)
        (save-window-excursion
            (org-roam-dailies--capture (current-time) t)
            (setq today-file (buffer-file-name))
            (setq pos (point)))

        ;; Only refile if the target file is different than the current file
        (unless (equal (file-truename today-file)
                    (file-truename (buffer-file-name)))
            (org-refile nil nil (list "Tasks" today-file nil pos)))))

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
  :when (equal custom-note-system 'custom-note-system-denote)
  ;; :after which-key dired
  :ensure t
  :defer t
  :custom
  (denote-directory (expand-file-name "notes" user-emacs-directory))
  (denote-save-buffers nil)
  (denote-known-keywords '("Python" "OCaml" "Journal" "Wildlife" "Photography"))
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
  ;; (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
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
  :ensure t
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p custom-developer-root)
    (setq projectile-project-search-path '(custom-developer-root)))
  (setq projectile-switch-project-action #'projectile-dired))

(when (equal completion-handler 'comphand-ivy)
  (use-package counsel-projectile
    :when (equal custom-project-handler 'custom-project-projectile)
    :ensure t
    :after projectile
    :config
    (setq projectile-completion-system 'ivy)
    (counsel-projectile-mode)))

;;; ##########################################################################

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(use-package project
  :when (equal custom-project-handler 'custom-project-project-el)
  :config
  (setq project-vc-extra-root-markers '(".project.el" ".projectile" )))
  ;; (when (featurep 'go-mode
  ;; (cl-defmethod project-root ((project (head go-module)))
  ;;   (cdr project))
  ;; (add-hook 'project-find-functions #'project-find-go-module))

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
  :ensure t
  :defer t
  :config
  (unless theme-did-load
    (mifi/load-theme-from-selector)))

;;; ##########################################################################

;; Consider doing an "M-x eglot-upgrade-eglot" to ensure that you have the most
;; current eglot (yes, even though it's a built in package).

(use-package eglot
  :when (equal custom-ide 'custom-ide-eglot)
  :after lsp-mode
  :ensure nil
  :defer t
  :hook
  (lisp-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  (rustic-mode . eglot-ensure)
  (tuareg-mode . eglot-ensure)
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

;; (when (or (equal custom-ide 'custom-ide-lsp) (equal custom-ide 'custom-ide-eglot))
;;   (eval-when-compile (defvar lsp-enable-which-key-integration)))

(use-package lsp-mode
  :when (or (equal custom-ide 'custom-ide-lsp) (equal custom-ide 'custom-ide-eglot))
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
  :when (or (equal custom-ide 'custom-ide-lsp) (equal custom-ide 'custom-ide-eglot))
  :after lsp
  :ensure t
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
  :when (or (equal custom-ide 'custom-ide-lsp) (equal custom-ide 'custom-ide-eglot))
  :after lsp treemacs
  :bind (:map prog-mode-map
          ("C-c t" . treemacs))
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-ivy
  :when (and (or (equal custom-ide 'custom-ide-lsp) (equal custom-ide 'custom-ide-eglot))
          (equal completion-handler 'comphand-ivy))
  :after lsp ivy)

;;; ##########################################################################
;;; LSP mode setup hook

(defun mifi/lsp-mode-setup ()
  "Custom LSP setup function."
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  (when (or (equal custom-ide 'custom-ide-lsp) (equal custom-ide 'custom-ide-eglot))
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
  :disabled
  :when (equal custom-ide 'custom-ide-lsp-bridge)
  ;; :ensure ( :host github :repo "manateelazycat/lsp-bridge"
  ;;           :files (:defaults "*.el" "*.py" "acm" "core" "langserver"
  ;;                    "multiserver" "resources") :build (:not compile))
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
  (use-package pyvenv-auto :after python :hook (python-mode . pyvenv-auto-run))
  :hook (python-mode . anaconda-eldoc-mode))

;;; ##########################################################################

(use-package elpy
  :when (equal custom-ide 'custom-ide-elpy)
  :after python which-key
  :ensure t
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
  ;; (use-package flycheck
  ;;   :when (equal custom-ide 'custom-ide-elpy)
  ;;   :after elpy
  ;;   :defer t
  ;;   :delight "fc"
  ;;   ;;:ensure (:host github :repo "flycheck/flycheck")
  ;;   :hook (elpy-mode . flycheck-mode))
  (which-key-add-key-based-replacements
    "C-c g a" "goto-assignment"
    "C-c g o" "find-defitions-other-window"
    "C-c g g" "find-defitions"
    "C-c g ?" "eldoc-definition")
  (if (featurep 'company)
    (bind-keys :map elpy-mode-map
      ("<tab>" . company-indent-or-complete-common)))
  (elpy-enable))

(use-package jedi
  :ensure t
  :after python elpy)

;;; ##########################################################################

(use-package flycheck
  ;;:unless (equal custom-ide 'custom-ide-elpy)
  :ensure t
  :delight 'fc
  :defer t
  ;;:ensure (:host github :repo "flycheck/flycheck")
  :config
  (eval-after-load 'flycheck
    '(flycheck-package-setup))
  (global-flycheck-mode))

(use-package flycheck-package
  :ensure t
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
  :after (:any python python-mode lisp-mode merlin-mode tuareg)
  :ensure t
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
  :ensure t
  :after tree-sitter)

;;; ##########################################################################

(use-package treesit-auto
  :demand t
  :config
  (global-treesit-auto-mode))

;;; ##########################################################################

(use-package magit :after git-commit :ensure t)

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started

(use-package forge :after magit :defer t :ensure t)
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

;; This function should only be called ONCE during python-mode startup.
(defun mifi/enable-python-features ()
  (message ">>> mifi/enable-python-features")
  ;; _____________________________
  (cond
    ((equal debug-adapter 'debug-adapter-dap-mode)
      (unless (featurep 'dap-mode) (dap-mode)) ;; Load if not loaded.
      (define-dap-hydra)))
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

;; Use built-in python language mode.
(use-package python-mode
  :when enable-python
  :ensure t
  :mode ("\\.py\\'" . mifi/load-python-file-hook)
  :hook (python-mode . mifi/python-mode-triggered)
  :config
  (if (boundp 'python-shell-completion-native-disabled-interpreters)
    (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
    (setq python-shell-completion-native-disabled-interpreters '("python3"))))

(when enable-python
  (use-package blacken
    :ensure t
    :when enable-python
    :after python)) ;Format Python file upon save.

;;; ##########################################################################

;; Using when instead of :when so that the package doesn't get loaded.
(when enable-python
  (use-package py-autopep8
    :ensure t
    ;; :vc (:url "https://github.com/emacsmirror/py-autopep8.git")
    :after python
    :hook ((python-mode . py-autopep8-mode))))

;;; ##########################################################################

;; This is a helpful macro that is used to put double quotes around a word.
(defalias 'quote-word
  (kmacro "\" M-d \" <left> C-y"))

(defalias 'quote-region
  (kmacro "C-w \" \" <left> C-y <right>"))

(defalias 'reformat-src-block
  (kmacro "C-s b e g i n _ s r c SPC e m a c s - l i s p <return> <down> C-c ' C-x h C-c ] C-x h M-x u n t a b <return> C-c ' C-s e n d _ s r c <return> <down>"))

(when enable-python
  (eval-after-load "python"
    #'(bind-keys :map python-mode-map
      ("C-c C-q" . quote-region)
      ("C-c q"   . quote-word)
      ("C-c |"   . display-fill-column-indicator-mode))))

;;; ##########################################################################

;; Using when instead of :when so that the package doesn't get loaded.
(when enable-python
  (use-package pyvenv-auto
    :ensure t
    :after python
    :hook (python-mode . pyvenv-auto-run)))

;;; ##########################################################################
;; Using when instead of :when so that the package doesn't get loaded.
(when enable-python
  (use-package pydoc
    ;;:ensure (:host github :repo "statmobile/pydoc")
    :after python
    :custom
    (pydoc-python-command "python3")
    (pydoc-pip-version-command "pip3 --version")))

(let
  ((installer "bash -c \"sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh) --version 2.2.0\""))
  (use-package opam
    :when enable-ocaml
    :demand t
    :ensure-system-package (opam . installer)))

(defun opam-switch-prefix+relative-path (relative-path)
  (let ((base (getenv "OPAM_SWITCH_PREFIX")))
    (concat (concat base "/") relative-path)))

;;; ##########################################################################

(use-package opam-std-libs
  :when enable-ocaml
  :ensure-system-package
  ( ("~/.opam"                  . "opam init && opam install core utop base stdio --yes")
    ("~/.opam/default/lib/core" . "opam install core --yes")
    ("~/.opam/default/lib/utop" . "opam install utop --yes")))

;;; ##########################################################################

(use-package opam-emacs-setup
  :when enable-ocaml
  :after opam-std-libs
  :config
  (add-to-list 'exec-path "~/.opam/default/bin"))
  ;;
  ;; :ensure-system-package
  ;; ;; we're using the new lps-server preview for 5.2.0 compatibility.
  ;; ( ((opam-switch-prefix+relative-path "share/emacs/site-lisp/tuareg.el") .
  ;;     "opam install merlin tuareg ocaml-lsp-server.1.18.0~5.2preview --yes")))

;;; ##########################################################################

(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and (and opam-share (file-directory-p opam-share)) enable-ocaml)
    (message "Updating load-path for OPAM to %s" (expand-file-name "emacs/site-lisp" opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)))

;;; ##########################################################################

(use-package merlin
  :when enable-ocaml :defer t 
  :delight " 🪄"
  :after opam-emacs-setup)

(use-package merlin-eldoc
  :when enable-ocaml  :defer t :after merlin)

(use-package merlin-company
  :when (and enable-ocaml (not (equal custom-ide 'custom-ide-lsp-bridge)))
   :defer t :after merlin company
  :config
  (add-hook 'merlin-mode-hook 'company-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'merlin-company-backend)))

(use-package dune
  :when enable-ocaml
  :hook (dune-mode . opam-init)
  :ensure-system-package
  (dune . "opam install dune --yes"))

;;; ##########################################################################

(defun mifi/tuareg-mode-hook ()
  (interactive)
  (merlin-mode t)
  (opam-init)
  (eglot-ensure)
  (dap-mode)
  (setq tuareg-mode-name "🐫")
  (when (functionp 'prettify-symbols-mode)
    (prettify-symbols-mode)))

(use-package tuareg
  :when enable-ocaml  :defer t
  ;; :after opam-emacs-setup merlin jsonrpc
  :hook (tuareg-mode . mifi/tuareg-mode-hook)
  ;; ("\\.ml\\'" . mifi/tuareg-mode-hook)
  ;; ("\\.mli\\'" . tuareg-mode)
  :custom
  (tuareg-indent-align-with-first-arg t)
  (compile-command "dune build ")
  :config
  (bind-keys :map tuareg-mode-map
    ("C-c ," . dap-hydra/body)))

;; Does many things but also updates the exec-path to the local
;; opam environment.
(use-package tuareg-opam
  :when enable-ocaml
  
  :after tuareg)

;;; ##########################################################################

(let
  ((file (expand-file-name "opam-user-setup.el" emacs-config-directory)))
  (when (file-exists-p file)
    (use-package opam-user-setup
      :unless (featurep 'opam-user-setup) ;; Don't allow to be run twice!
      :when enable-ocaml
      
      :after tuareg
      :config
      (setq-default tuareg-indent-align-with-first-arg t)
      (setq-default compile-command "dune build ")
      (add-hook 'tuareg-mode-hook #'mifi/tuareg-mode-hook))))

;;; ##########################################################################

(use-package ocp-indent
  :when enable-ocaml
  :after opam-emacs-setup
  :ensure-system-package
  ("~/.opam/default/bin/ocp-indent" . "opam install ocp-indent --yes"))
  ;; :vc (:url "https://github.com/OCamlPro/ocp-indent" :main-file tools/ocp-indent))
  ;; :ensure (:inherit t :depth 1
  ;;          :fetcher github :repo "OCamlPro/ocp-indent"
  ;;          :files ("tools/ocp-indent.el")))

(use-package ocamlformat
  :when enable-ocaml
  :after ocp-indent
  :bind ("<f6>" . ocamlformat)
  :ensure-system-package
  ("~/.opam/default/lib/ocamlformat-lib" . "opam install ocamlformat --yes")
  :custom (ocamlformat-enable 'enable-outside-detected-project))

(use-package utop
  :when enable-ocaml
  :after opam-user-setup
  :defer t
  :custom
  (utop-command "opam config exec utop -- -emacs"))

(use-package opam-switch-mode
  :when enable-ocaml
  :after opam-user-setup
  :hook
  (tuareg-mode . opam-switch-mode))

;;; ##########################################################################

(when enable-ts
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
      (dap-node-setup)))))

;;; ##########################################################################

(defun mifi/load-js-file-hook ()
  (js2-mode)

  (when (equal debug-adapter 'debug-adapter-dap-mode)
    (dap-mode)
    (dap-firefox-setup))

  (highlight-indentation-mode nil)
  (dap-firefox-setup))

(defun mifi/nvm-which ()
  (let ((output (shell-command-to-string "source ~/.nvm/nvm.sh; nvm which")))
    (cadr (split-string output "[\n]+" t))))

(use-package nodejs-repl
  :defer t
  :config
  (setq nodejs-repl-command #'mifi/nvm-which))

;;; ##########################################################################

(use-package js2-mode
  ;;:after simple-httpd
  :hook (js-mode . js2-minor-mode)
  ;; :bind (:map js2-mode-map
  ;;         ("{" . paredit-open-curly)
  ;;         ("}" . paredit-close-curly-and-newline))
  :mode ("\\.js\\'" "\\.mjs\\'" "\\.json$")
  :custom (js2-highlight-level 3))

(use-package skewer-mode
  :after js2-mode)

(use-package ac-js2
  :after js2-mode skewer-mode
  :hook (js2-mode . ac-js2-mode))

;;; ##########################################################################

(when enable-gb-dev
  (use-package z80-mode
    :when enable-gb-dev
    :vc (:url "https://github.com/SuperDisk/z80-mode"))
    ;; :ensure (:host github :repo "SuperDisk/z80-mode"))

  (use-package mwim
    :when enable-gb-dev
    :vc (:url "https://github.com/alezost/mwim"))
    ;; :ensure (:host github :repo "alezost/mwim.el"))

  (use-package rgbds-mode
    :when enable-gb-dev
    :after mwim
    :vc (:url "https://github.com/japanoise/rgbds-mode")))
    ;; :ensure (:host github :repo "japanoise/rgbds-mode")))

;;; ##########################################################################

(use-package rustic
  :bind (:map rustic-mode-map
          ("M-j" . lsp-ui-imenu)
          ("M-?" . lsp-find-references)
          ("C-c C-c l" . flycheck-list-errors)
          ("C-c C-c a" . lsp-execute-code-action)
          ("C-c C-c r" . lsp-rename)
          ("C-c C-c q" . lsp-workspace-restart)
          ("C-c C-c Q" . lsp-workspace-shutdown)
          ("C-c C-c s" . lsp-rust-analyzer-status))
  :hook
  (rustic-mode . rk/rustic-mode-hook)
  :ensure-system-package
  ( (rustc . "curl https://sh/rustup.rs -sSf | sh")
    (cargo . "curl https://sh/rustup.rs -sSf | sh")
    )
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
  (lsp-deferred)
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

(use-package rust-playground :after rust-mode)

;;; ##########################################################################
;; for Cargo.toml and other config files

(use-package toml-mode :defer t :after rust-mode)

;;; ##########################################################################

(use-package cargo-mode
  :defer t
  :after rust-mode
  :ensure t)

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

(use-package elisp-mode
  
  :defer t
  :mode ("\\.el\\'" . emacs-lisp-mode))

;;; ##########################################################################

(use-package slime
  :defer t
  :mode
  ("\\.lisp\\'" . slime-mode)
  :config
  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl"))

;;; ##########################################################################
;;; Debug Adapter Protocol
(use-package dap-mode
  :when (equal debug-adapter 'debug-adapter-dap-mode)
  :defer t
  :after hydra
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  :commands dap-debug
  :custom
  (dap-auto-configure-features '(sessions locals breakpoints expressions repl controls tooltip))
  :config
  ;; (require 'dap-lldb)
  ;; (require 'dap-gdb-lldb)
  (define-dap-hydra)
  (bind-keys :map prog-mode-map
    ("C-c ." . dap-hydra/body))
  (dap-ui-controls-mode)
  (dap-ui-mode 1))

;;; ##########################################################################

;; For Emacs >= 30.0 it is possible to use the VC command like this:
;; :vc (:url "https://github.com/emacs-lsp/dap-mode"
;;      :main-file "dap-ocaml.el")
;;
(use-package dap-ocaml
  :when enable-ocaml
  :after (:all dap-mode opam-emacs-setup)
  :ensure t
  :vc (:url "https://github.com/emacs-lsp/dap-mode" :main-file dap-ocaml)
  ;; :ensure (:package "dap-ocaml" :type git :host github :repo "emacs-lsp/dap-mode")
  :ensure-system-package
  ((ocamllsp . "opam install earlybird --yes")))

(use-package dap-codelldb
  :when enable-ocaml
  :after dap-mode
  :defer t
  :vc (:url "https://github.com/emacs-lsp/dap-mode" :main-file dap-codelldb) :ensure t)
  ;; :ensure (:package "dap-codelldb" :type git :host github :repo "emacs-lsp/dap-mode"))

;;; ##########################################################################
;;; DAP for Python

(when enable-python
  (use-package dap-python
    :vc (:url "https://github.com/emacs-lsp/dap-mode" :main-file dap-python)
    ;; :ensure (:package "dap-python" :type git :host github :repo "emacs-lsp/dap-mode")
    :defer t
    :when (equal debug-adapter 'debug-adapter-dap-mode)
    :after dap-mode
    :config
    (setq dap-python-executable "python3") ;; Otherwise it looks for 'python' else error.
    (setq dap-python-debugger 'debugpy)))

;;; ##########################################################################

(use-package dap-lldb
  :when (equal debug-adapter 'debug-adapter-dap-mode)
  :defer t
  :after dap-mode
  :vc (:url "https://github.com/emacs-lsp/dap-mode" :main-file dap-lldb)
  ;; :ensure ( :package "dap-lldb" :source nil :protocol https
  ;;           :inherit t :depth 1 :type git
  ;;           :host github :repo "emacs-lsp/dap-mode")
  :custom
  (dap-lldb-debug-program "~/Developer/command-line-unix/llvm/lldb-build/bin/lldb-dap"))
  ;; :config
  ;; (dap-register-debug-template
  ;;   "Rust::LLDB Run Configuration"
  ;;   (list :type "lldb"
  ;;     :request "launch"
  ;;     :name "LLDB::Run"
  ;;     :gdbpath "rust-lldb"
  ;;     :target nil
  ;;     :cwd nil)))

(use-package dap-gdb-lldb
  :when (equal debug-adapter 'debug-adapter-dap-mode)
  :vc (:url "https://github.com/emacs-lsp/dap-mode" :main-file dap-gdb-lldb) :ensure t
  ;; :ensure ( :package "dap-gdb-lldb" :source nil :protocol https
  ;;           :inherit t :depth 1 :type git :host github
  ;;           :repo "emacs-lsp/dap-mode")
  :defer t
  :after dap-lldb
  :config
  (dap-gdb-lldb-setup))

(use-package dap-cpptools
  :when (equal debug-adapter 'debug-adapter-dap-mode)
  :defer t
  :after dap-mode
  :vc (:url "https://github.com/emacs-lsp/dap-mode" :main-file dap-cpptools) :ensure t)
  ;; :ensure ( :package "dap-cpptools" :source nil :protocol https
  ;;           :inherit t :depth 1 :type git :host github
  ;;           :repo "emacs-lsp/dap-mode"))
;; :config
;; (dap-cpptools-setup))

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

;;; ##########################################################################

(use-package mw-thesaurus
  :when enable-thesaurus
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
  :ensure t
  :after treemacs
  :hook (after-init . solaire-global-mode)
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
  :after track-changes
  :config
  (global-diff-hl-mode))

;;; ##########################################################################

(use-package pulsar
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
  
  :hook (prog-mode . (lambda () (rainbow-mode t))))

;;; ##########################################################################

(use-package highlight-defined
  :defer t
  :after emacs-lisp-mode
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

;; (defun mifi/load-web-support ()
;;   (use-package web-server-status-codes )
;;   (use-package simple-httpd
;;     :preface (setq warning-minimum-level :emergency)
;;     
;;     :config (setq warning-minimum-level :warning))
;;   (use-package websocket )
;;   (use-package web-server ))

(use-package markdown-mode
  ;;:defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
  ;;:hook (after-init . mifi/load-web-support))

;;; ##########################################################################

(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
	   (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
    (current-buffer)))

(use-package simple-httpd
  :ensure t)

(use-package impatient-mode
  :ensure t
  :config
  (imp-set-user-filter #'markdown-html)
  :hook (elpaca-after-init . httpd-start))

;;; ##########################################################################

;; Use keybindings
(use-package grip-mode
  :bind (:map markdown-mode-command-map
          ("g" . grip-mode)))

;; ;; Or using hooks
;; (use-package grip-mode
;;   :hook ((markdown-mode org-mode) . grip-mode))

;;; ##########################################################################

(defadvice custom-buffer-create (before my-advice-custom-buffer-create)
  "Exit the current Customize buffer before creating a new one, unless there are modified widgets."
  (if (eq major-mode 'Custom-mode)
      (let ((custom-buffer-done-kill t)
            (custom-buffer-modified nil))
        (mapc (lambda (widget)
                (and (not custom-buffer-modified)
                     (eq (widget-get widget :custom-state) 'modified)
                     (setq custom-buffer-modified t)))
              custom-options)
        (if (not custom-buffer-modified)
            (Custom-buffer-done)))))

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

(defun mifi/customize-mifi ()
  "Opens up the customize section for all of the MiFi options."
  (interactive)
  (ad-activate 'custom-buffer-create)
  (customize-apropos "mifi-config"))

;;; ##########################################################################

(defun mifi/define-mmm-minor-mode-map ()
  (defvar mmm-keys-minor-mode-map
    (let ((map (make-sparse-keymap)))
      (bind-keys :map map
        ("M-RET $" . jinx-correct)
        ("M-RET ?" . eldoc-box-help-at-point)
        ("M-RET M s" . markdown-preview-mode)
        ("M-RET M e" . markdown-preview-cleanup)
        ("M-RET S e" . eshell)
        ("M-RET S i" . ielm)
        ("M-RET S v" . vterm-other-window)
        ("M-RET v s" . use-small-display-font)
        ("M-RET v m" . use-medium-display-font)
        ("M-RET v l" . use-large-display-font)
        ("M-RET v x" . use-x-large-display-font)
        ("M-RET v 1" . use-small-display-font-t)
        ("M-RET v 2" . use-medium-display-font-t)
        ("M-RET v 3" . use-large-display-font-t)
        ("M-RET v 4" . use-x-large-display-font-t)
	("M-RET w <right>" . which-key-setup-side-window-right-bottom)
	("M-RET w <down>" . which-key-setup-side-window-bottom)
        ("M-RET =" . next-theme)
        ("M-RET -" . previous-theme)
        ("M-RET _" . which-theme)
        ("M-RET M-c" . mifi/customize-mifi)
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

  (define-minor-mode mmm-keys-minor-mode
    "A minor mode so that my key settings override annoying major modes."
    :init-value t
    :lighter " MmM"))

(mifi/define-mmm-minor-mode-map)

;;; ##########################################################################

(defun mifi/mmm-handle-context-keys (&optional winframe)
  "Enable or Disable keys based upon featurep context."
  (when winframe
    (let ((map mmm-keys-minor-mode-map))
      (when enable-thesaurus
        (bind-keys :map map
          ("M-RET t t" . mw-thesaurus-lookup-dwim)))
      (cond
        ((equal major-mode 'org-mode)
          (bind-keys :map map
            ("M-RET M-RET" . org-insert-heading)
            ("M-RET o f" . mifi/set-org-fill-column-interactively)
            ("M-RET o c" . mifi/toggle-org-centering)
            ("M-RET o l" . org-toggle-link-display)))
        ((equal major-mode 'python-mode)
          (bind-keys :map map
            ("M-RET P" . 'pydoc-at-point)))
	((equal major-mode 'tuareg-mode)
	  (bind-keys :map map
	    ("M-RET c m" . tuarg-browse-manual)))
        (t   ;; Default 
          (unbind-key "M-RET o f" map)
          (unbind-key "M-RET o c" map)
          (unbind-key "M-RET o l" map)
          (unbind-key "M-RET P ?" map)
          (unbind-key "M-RET c m" map)
          (unbind-key "M-RET M-RET" map)))))

  ;; Override default menu text with better things
  (which-key-add-key-based-replacements
    "M-RET t t" "thesaurus-at-point"
    "M-RET o" "org-menu"
    "M-RET c" "ocaml-menu"
    "M-RET o c" "toggle-org-centering"
    "M-RET o f" "set-org-fill-column"))

;;; ##########################################################################

(defun mifi/mmm-update-menu (&optional winframe)
  (interactive)
  (mifi/mmm-handle-context-keys nil)
  (which-key-add-key-based-replacements
    "M-RET w" "which-key-position"
    "M-RET w <right>" "which-key-on-right"
    "M-RET w <down>" "which-key-on-bottom"
    "M-RET M" "markdown-preview"
    "M-RET S" "shells"
    "M-RET P" "python-menu"
    "M-RET e" "treemacs-toggle"
    "M-RET t" "Thesaurus"
    "M-RET f" "set-fill-column"
    "M-RET j" "jump-to-register"
    "M-RET v" "font-size"
    "M-RET C-g" "Exit menu"
    "M-RET" "Mitch's Menu"))

;;; ##########################################################################

;; Check the keys when:
;; - the whick-key menu is displayed
(add-hook 'after-init-hook
  (lambda ()
    (add-hook 'which-key-inhibit-display-hook 'mifi/mmm-update-menu)
    ;; - the user updates/changes the buffer - like loading a file
    ;;   (but not switching to a new buffer)
    (add-hook 'window-buffer-change-functions 'mifi/mmm-handle-context-keys)
    ;; - the user switches windows
    (add-hook 'window-selection-change-functions 'mifi/mmm-handle-context-keys)
    ;; (add-hook 'which-key-mode-hook #'mifi/after-which-key)
    (mifi/after-which-key)
    (mifi/define-mmm-minor-mode-map)
    (mifi/set-recenter-keys)))

;;; ##########################################################################

(defun mifi/config-landing ()
  (cond
    ((equal default-landing-mode 'landing-mode-dashboard)
      (dashboard-open))
    ((equal default-landing-mode 'landing-mode-scratch)
      (switch-to-buffer "*scratch*")
      (erase-buffer)
      (beginning-of-buffer)
      (insert (concat 
              ";; 'Tis but a scratch! A scratch? Your arm's off! - No, it isn't!\n"
              (format ";; Happy hacking, %s! %s" user-login-name
                "Press M-RET (Meta-RET) to open the MiFi Menu\n")))
      (end-of-buffer))
    ((equal default-landing-mode 'landing-mode-ielm)
      (ielm))
    ((equal default-landing-mode 'landing-mode-eshell)
      (eshell))))

;;; ##########################################################################

(defun mifi/backup-file (file)
  "Backup the file from the configuration directory into the
backup directory. If a file already exists in the backup directory, the old
file is renamed with a ~ at the end before the new file is copied. If Emacs
is running in server mode, then don't backup the files when the emacsclient
exits."
  (unless (server-running-p)
    (let ((backdir (format "%s/config-backup" working-files-directory)))
      (make-directory backdir t)
      ;; --------------------------------------------------
      (when (file-exists-p (format "%s/%s" backdir file))
      (copy-file
        (expand-file-name file backdir)
        (expand-file-name (format "%s~" file) backdir) t))
      (when (file-exists-p (format "%s/%s" emacs-config-directory file))
      (copy-file
        (expand-file-name file emacs-config-directory)
        (expand-file-name file backdir) t)))))

(defun mifi/when-exiting-emacs ()
  "Backup Emacs initialization files for recovery. If old files exist, they are
backed up as tilde (~) files. Also, if ocaml is enabled, byte (re)compile the
opam-user-setup.el so that upon next startup, it can be loaded quickly."
  (progn
    (when enable-ocaml
      (let ((src (expand-file-name "opam-user-setup.el" emacs-config-directory)))
      (when (file-exists-p src)
        (byte-compile-file src))))
    (mifi/backup-file "early-init.el")
    (mifi/backup-file "init.el")
    (mifi/backup-file "emacs-config.org")))

(add-hook 'kill-emacs-hook #'mifi/when-exiting-emacs)

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

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;;-- This is already handled in the OCaml language config above...
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
