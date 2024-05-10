;;; init.el -*- flycheck-disabled-checkers: (emacs-lisp); lexical-binding: nil -*-
;;;
;;; Commentary:

;; This file bootstraps the configuration which is generated from tangling an org-mode file.
;; So, DO NOT MODIFY this file directly as changes will be overwritten.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)
;;

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
    (setq elpaca-use-package-by-default t))

;;  (use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))

;;; --------------------------------------------------------------------------
;;; Define my customization groups

(defgroup mrf-custom nil
    "Customization toggles for my personal Emacs installation."
    :group 'Local)

(defgroup mrf-custom-toggles nil
    "A set of toggles that enable or disable  specific packages."
    :group 'mrf-custom)

(defgroup mrf-custom-choices nil
    "Customization from a selection of specific features."
    :group 'mrf-custom)

(defgroup mrf-custom-fonts nil
    "Customization of fonts and sizes."
    :group 'mrf-custom)

(defgroup mrf-custom-theming nil
    "Custom theming values."
    :group 'mrf-custom)

;;; --------------------------------------------------------------------------

(defcustom display-dashboard-at-start t
    "If set to t, the `dashboard' package will be displayed first.
  Otherwise, the `dashboard' will be available but in the buffer
   *dashboard*."
    :type 'boolean
    :group 'mrf-custom)

(defcustom custom-docs-dir "~/Documents/Emacs-Related"
    "A directory used to store documents and customized data."
    :type 'string
    :group 'mrf-custom)

(defcustom working-files-directory
    (expand-file-name "emacs-working-files" custom-docs-dir)
    "The directory where to store Emacs working files."
    :type 'string
    :group 'mrf-custom)

(defcustom custom-org-fill-column 120
    "The fill column width for Org mode text.
Note that the text is also centered on the screen so that should
be taken into consideration when providing a width."
    :type 'natnum
    :group 'mrf-custom)

;;; --------------------------------------------------------------------------
;;; Feature Toggles

(defcustom enable-gb-dev nil
    "If set to t, the z80-mode and other GameBoy related packages
    will be enabled."
    :type 'boolean
    :group 'mrf-custom-toggles)

(defcustom enable-ts nil
    "Set to t to enable TypeScript handling."
    :type 'boolean
    :group 'mrf-custom-toggles)

(defcustom enable-corfu nil
    "Setting to t enables Corfu instead of Ivy.
    Corfu is an alternative to the command completion package, IVY which also will
    include Swiper and Company.  If this value is set to nil then Ivy is used."
    :type 'boolean
    :group 'mrf-custom-toggles)

(defcustom enable-vundo t
    "Set to t to enable `vundo' which is an alternative to Emacs undo.
    Setting this value to nil will activate the alternate `undo-tree' package."
    :type 'boolean
    :group 'mrf-custom-toggles)

(defcustom enable-centaur-tabs nil
    "Set to t to enable `centaur-tabs' which uses tabs to represent open buffer."
    :type 'boolean
    :group 'mrf-custom)

(defcustom enable-neotree nil
    "Set to t to enable the `neotree' package."
    :type 'boolean
    :group 'mrf-custom-toggles)

(defcustom enable-golden-ratio nil
    "Set to t to enable `golden-ratio-mode' which resizes the active buffer
    window to the dimensions of a golden-rectangle "
    :type 'boolean
    :group 'mrf-custom)

(defcustom enable-org-fill-column-centering nil
    "Set to t to center the visual-fill column of the Org display."
    :type 'boolean
    :group 'mrf-custom)

;;; --------------------------------------------------------------------------

(defcustom completion-handler 'comphand-vertico
    "Select the default minibuffer completion handler.

Vertico provides a performant and minimalistic vertical completion UI based on
the default completion system.

Ivy is a generic completion mechanism for Emacs. While it operates similarly to
other completion schemes such as icomplete-mode, Ivy aims to be more efficient,
smaller, simpler, and smoother to use yet highly customizable.  The Ivy package
also includes Counsel. Counsel provides completion versions of common Emacs
commands that are customised to make the best use of Ivy.  Swiper is an
alternative to isearch that uses Ivy to show an overview of all matches."
    :type '(choice
	       (const :tag "Use the Vertico completion system." comphand-vertico)
		 (const :tag "Use Ivy, Counsel, Swiper completion systems" comphand-ivy-counsel)
	       (const :tag "Built-in Ido" comphand-built-in))
    :group 'mrf-custom-choices)

(defcustom debug-adapter 'enable-dape
    "Select the debug adapter to use for debugging applications.  dap-mode is an
Emacs client/library for Debug Adapter Protocol is a wire protocol for
communication between client and Debug Server. It’s similar to the LSP but
provides integration with debug server.

dape (Debug Adapter Protocol for Emacs) is similar to dap-mode but is
implemented entirely in Emacs Lisp. There are no other external dependencies
with DAPE. DAPE supports most popular languages, however, not as many as
dap-mode."
    :type '(choice (const :tag "Debug Adapter Protocol (DAP)" enable-dap-mode)
		 (const :tag "Debug Adapter Protocol for Emacs (DAPE)" enable-dape))
    :group 'mrf-custom-choices)

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
    :type '(choice (const :tag "Elpy: Emacs Lisp Python Environment" custom-ide-elpy)
		 (const :tag "Emacs Polyglot (Eglot)" custom-ide-eglot)
		 (const :tag "Language Server Protocol (LSP)" custom-ide-lsp)
		 (const :tag "LSP Bridge (standalone)" custom-ide-lsp-bridge)
		 (const :tag "Python Anaconda-mode for Emacs" custom-ide-anaconda))
    :group 'mrf-custom-choices)

;;; --------------------------------------------------------------------------
;;; Theming related

(defcustom theme-list '("palenight-deeper-blue"
                           "ef-symbiosis"
                           "ef-maris-light"
                           "ef-maris-dark"
                           "ef-kassio"
                           "ef-bio"
                           "sanityinc-tomorrow-bright"
                           "ef-melissa-dark"
                           "darktooth-dark"
                           "material"
                           "deeper-blue")
    "My personal list of themes to cycle through indexed by `theme-selector'.
If additional themes are added, they must be previously installed."
    :group 'mrf-custom-theming
    :type '(repeat string))

(defcustom default-terminal-theme "sanityinc-tomorrow-bright"
    "The default theme used for a terminal invocation of Emacs."
    :group 'mrf-custom-theming
    :type 'string)

(defcustom theme-selector 0
    "The index into the list of custom themes."
    :group 'mrf-custom-theming
    :type 'natnum)

;;; Font related
(defcustom default-font-family "Hack"
    "The font family used as the default font."
    :type 'string
    :group 'mrf-custom-fonts)

(defcustom mono-spaced-font-family "Hack"
    "The font family used as the mono-spaced font."
    :type 'string
    :group 'mrf-custom-fonts)

(defcustom variable-pitch-font-family "SF Pro"
    "The font family used as the default proportional font."
    :type 'string
    :group 'mrf-custom-fonts)

(defcustom small-mono-font-size 150
    "The small font size in pixels."
    :type 'natnum
    :group 'mrf-custom-fonts)

(defcustom medium-mono-font-size 170
    "The medium font size in pixels."
    :type 'natnum
    :group 'mrf-custom-fonts)

(defcustom large-mono-font-size 190
    "The large font size in pixels."
    :type 'natnum
    :group 'mrf-custom-fonts)

(defcustom x-large-mono-font-size 220
    "The extra-large font size in pixels."
    :type 'natnum
    :group 'mrf-custom-fonts)

(defcustom small-variable-font-size 170
    "The small font size in pixels."
    :type 'natnum
    :group 'mrf-custom-fonts)

(defcustom medium-variable-font-size 190
    "The small font size in pixels."
    :type 'natnum
    :group 'mrf-custom-fonts)

(defcustom large-variable-font-size 210
    "The small font size in pixels."
    :type 'natnum
    :group 'mrf-custom-fonts)

(defcustom x-large-variable-font-size 240
    "The small font size in pixels."
    :type 'natnum
    :group 'mrf-custom-fonts)

(defcustom custom-default-font-size 170
    "A place to store the most current (face-attribute 'default :height).  This
is specifically for the mono-spaced and default font. The variable type-face
font size is computed + 20 of this value."
    :type 'natnum
    :group 'mrf-custom-fonts)

;;; --------------------------------------------------------------------------

;; Use shell path

(defun set-exec-path-from-shell-PATH ()
   ;;; Set up Emacs' `exec-path' and PATH environment variable to match"
   ;;; that used by the user's shell.
   ;;; This is particularly useful under Mac OS X and macOS, where GUI
   ;;; apps are not started from a shell."
    (interactive)
    (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" ""
                               (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))))
        (setenv "PATH" path-from-shell)
        (setq exec-path (split-string path-from-shell path-separator))
        (add-to-list 'exec-path "/opt/homebrew/bin")
        (add-to-list 'exec-path "/usr/local/bin")
        (add-to-list 'exec-path "/opt/homebrew/opt/openjdk/bin")
        (add-to-list 'exec-path "/opt/homebrew/opt/node@20/bin/node")
        (setq-default insert-directory-program "gls"
            dired-use-ls-dired t
            ;; Needed to fix an issue on Mac which causes dired to fail
            dired-listing-switches "-al --group-directories-first")))

;;; --------------------------------------------------------------------------
;;; Set a variable that represents the actual emacs configuration directory.
;;; This is being done so that the user-emacs-directory which normally points
;;; to the .emacs.d directory can be re-assigned so that customized files don't
;;; pollute the configuration directory. This is where things like YASnippet
;;; snippets are saved and also additional color themese are stored.

(defvar emacs-config-directory user-emacs-directory)

;;; Different emacs configuration installs with have their own configuration
;;; directory.
(make-directory working-files-directory t)

;;; Point the user-emacs-directory to the new working directory
(setq user-emacs-directory working-files-directory)
(message (concat ">>> Setting emacs-working-files directory to: " user-emacs-directory))

;;; Put any emacs cusomized variables in a special file
(setq custom-file (expand-file-name "customized-vars.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;;; --------------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name "lisp" emacs-config-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "Themes" custom-docs-dir))

;;; --------------------------------------------------------------------------

(setq-default
    window-resize-pixelwise t ;; enable smooth resizing
    window-resize-pixelwise t
    frame-resize-pixelwise t
    dired-dwim-target t       ;; try to guess target directory
    truncate-partial-width-windows 1 ;; truncate lines in partial-width windows
    backup-inhibited t        ;; disable backup (No ~ tilde files)
    auto-save-default nil     ;; disable auto save
    global-auto-revert-mode 1 ;; Refresh buffer if file has changed
    global-auto-revert-non-file-buffers t
    history-length 25         ;; Reasonable buffer length
    inhibit-startup-message t ;; Hide the startup message
    inhibit-startup-screent t
    lisp-indent-offset '4     ;; emacs lisp tab size
    visible-bell t            ;; Set up the visible bell
    truncate-lines 1          ;; long lines of text do not wrap
    fill-column 80            ;; Default line limit for fills
    ;; Triggers project for directories with any of the following files:
    project-vc-extra-root-markers '(".dir-locals.el"
				       "requirements.txt"
				       "Gemfile"
				       "package.json")
    )

;; (global-display-line-numbers-mode 1) ;; Line numbers appear everywhere
(save-place-mode 1)                  ;; Remember where we were last editing a file.
(savehist-mode t)
(show-paren-mode 1)
(tool-bar-mode -1)                   ;; Hide the toolbar
(global-prettify-symbols-mode 1)     ;; Display pretty symbols (i.e. λ = lambda)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Allow access from emacsclient
(add-hook 'after-init-hook
    (lambda ()
        (require 'server)
        (unless (server-running-p)
            (server-start))))

(when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode))

(use-package default-text-scale
    :hook (elpaca-after-init . default-text-scale-mode))

;;; --------------------------------------------------------------------------


(use-package diminish
    :preface
    (defun mrf/set-diminish ()
	(diminish 'projectile-mode "PrM")
	(diminish 'anaconda-mode)
	(diminish 'tree-sitter-mode "ts")
	(diminish 'ts-fold-mode)
	(diminish 'lisp-interaction-mode "Lim")
	(diminish 'counsel-mode)
	(diminish 'golden-ratio-mode)
	(diminish 'company-box-mode)
	(diminish 'company-mode))
    ;; :ensure (:host github :repo "myrjola/diminish.el")
    :hook (elpaca-after-init . mrf/set-diminish))

;;; --------------------------------------------------------------------------
;; Which Key Helper

(use-package which-key
    :diminish which-key-mode
    :custom (which-key-idle-delay 1)
    :config
    (which-key-mode)
    (which-key-setup-side-window-right))

;;; --------------------------------------------------------------------------

(use-package multiple-cursors
    :bind (("C-S-c C-S-c" . mc/edit-lines)
              ("C->" . mc/mark-next-like-this)
              ("C-<" . mc/mark-previous-like-this)
            ("C-c C-<" . mc/mark-all-like-this)))

;;; --------------------------------------------------------------------------

(use-package anzu
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

;;; --------------------------------------------------------------------------

(column-number-mode)

(use-package page-break-lines
    :config
    (global-page-break-lines-mode))

(use-package rainbow-delimiters
    :config
    (rainbow-delimiters-mode))

(use-package dash
    :disabled)
    ;; :ensure (:files ("dash.el" "dash.texi" "dash-pkg.el")
    ;;          :host github
    ;;          :repo "magnars/dash.el"))


(defun mrf/set-fill-column-interactively (num)
    "Asks for the fill column."
    (interactive "nfill-column: ")
    (set-fill-column num))

(defun mrf/set-org-fill-column-interactively (num)
    "Asks for the fill column for Org mode."
    (interactive "norg-fill-column: ")
    (setq custom-org-fill-column num)
    (mrf/org-mode-visual-fill)
    (redraw-display))

;;; --------------------------------------------------------------------------

(use-package visual-fill-column
    :after org)

;;; --------------------------------------------------------------------------

;; Macintosh specific configurations.

(defconst *is-a-mac* (eq system-type 'darwin))
(when (eq system-type 'darwin)
    (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'super))

;;; --------------------------------------------------------------------------

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
(global-unset-key (kbd "C-<wheel-up>"))

;;; --------------------------------------------------------------------------

(use-package hydra
    :ensure (:repo "abo-abo/hydra" :fetcher github
             :files (:defaults (:exclude "lv.el"))))

;;; --------------------------------------------------------------------------

;; prevent (emacs) eldoc loaded before Elpaca activation warning.
;; (Warning only displayed during first Elpaca installation)

(elpaca-process-queues)
(use-package eldoc
    :config
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
    (add-hook 'ielm-mode-hook 'eldoc-mode))

(use-package eldoc-box
    :after eldoc
    :diminish DocBox
    :config
    (global-eldoc-mode t))

;;; --------------------------------------------------------------------------
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

;;; --------------------------------------------------------------------------
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
    (setq yas-prompt-functions '(yas-ido-prompt))
    (defun help/yas-after-exit-snippet-hook-fn ()
        (prettify-symbols-mode))
    (add-hook 'yas-after-exit-snippet-hook #'help/yas-after-exit-snippet-hook-fn)
    (message ">>> YASnippet Configured"))

;;; --------------------------------------------------------------------------

(use-package yasnippet-snippets
    :after yasnippet
    :config
    (message ">>> YASnippet-Snippets Configured"))

;;; --------------------------------------------------------------------------

(use-package all-the-icons
    :config (message ">> all-the-icons configured")
    :when (display-graphic-p))

;;; --------------------------------------------------------------------------
;; A cleaner and simpler undo package.

(use-package vundo
    ;;:ensure ( :host github :repo "casouri/vundo")
    :bind
     ("C-x u" . vundo)
     ("C-x r u" . vundo)
    :config
    (message "<<< Vundo configured")
    (set-face-attribute 'vundo-default nil :family "Symbola")
    (setq vundo-glyph-alist vundo-unicode-symbols))

;;; --------------------------------------------------------------------------
;; Full-featured undo-tree handling. Look to Vundo for something a little
;; simpler.

(defun mrf/undo-tree-hook ()
    (set-frame-width (selected-frame) 20))

;;
;; Sometimes, when behind a firewall, the undo-tree package triggers elpaca
;; to queue up the Queue package which then hangs and fails. This happens
;; even if the :unless option is specified in the use-package (only :disabled
;; seems to work which isn't what I want). So, we prevent the loading of the
;; page altogether.
;;
(unless enable-vundo
    (use-package undo-tree
	;; :hook (undo-tree-visualizer-mode-hook . mrf/undo-tree-hook)
	:init
	(setq undo-tree-visualizer-timestamps t
            ;; undo-tree-visualizer-diff t
            undo-tree-enable-undo-in-region t
            ;; 10X bump of the undo limits to avoid issues with premature
            ;; Emacs GC which truncages the undo history very aggresively
            undo-limit 800000
            undo-strong-limit 12000000
            undo-outer-limit 120000000)
	:config
	(global-undo-tree-mode)
	;; This prevents the *.~undo-tree~ files from being persisted.
	(with-eval-after-load 'undo-tree
            (setq undo-tree-auto-save-history nil))))

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
    (when (not (eq theme-cycle-step nil))
	(let ((step theme-cycle-step) (result 0))
            (when step
		(setq result (+ step theme-selector))
		(when (< result 0)
                    (setq result (- (length theme-list) 1)))
		(when (> result (- (length theme-list) 1))
                    (setq result 0)))
            (message (format ">>> Current theme %S" theme))
            (setq-default theme-selector result))))

;; This is used to trigger the cycling of the theme-selector
;; It is called when a theme is disabled. The theme is disabled from the
;; `mrf/load-theme-from-selector' function.
(add-hook 'disable-theme-functions #'mrf/cycle-theme-selector)

;;; --------------------------------------------------------------------------

(defun mrf/load-theme-from-selector (&optional step)
    "Load the theme in `theme-list' indexed by `theme-selector'."
    (interactive)
    (setq theme-cycle-step nil)
    (cond
	((or (eq step nil) (eq step 0)) (setq theme-cycle-step 0))
	((> step 0) (setq theme-cycle-step 1))
	((< step 0) (setq theme-cycle-step -1)))
    (when loaded-theme
        (disable-theme (intern loaded-theme)))
    (setq loaded-theme (nth theme-selector theme-list))
    (message (concat ">>> Loading theme "
                 (format "%d: %S" theme-selector loaded-theme)))
    (load-theme (intern loaded-theme) t)
    (when (featurep 'org)
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

(defun reload-theme--from-startup ()
    (setq loaded-theme (nth theme-selector theme-list))
    (message (concat ">>> Startup Loading theme "
                 (format "%d: %S" theme-selector loaded-theme)))
    (mrf/load-theme-from-selector)
    (load-theme (intern loaded-theme) t))

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

(defun mrf/org-theme-override-values ()
    (defface org-block-begin-line
        '((t (:underline "#1D2C39" :foreground "SlateGray" :background "#1D2C39")))
        "Face used for the line delimiting the begin of source blocks.")

    (defface org-block
        '((t (:background "#242635" :extend t :font "JetBrains Mono")))
        "Face used for the source block background.")

    (defface org-block-end-line
        '((t (:overline "#1D2C39" :foreground "SlateGray" :background "#1D2C39")))
        "Face used for the line delimiting the end of source blocks.")

    (defface org-modern-horizontal-rule
        '((t (:strike-through "green" :weight bold)))
        "Face used for the Horizontal like (-----)"))

;;; --------------------------------------------------------------------------

(defun mrf/customize-modus-theme ()
    (message ">> Applying modus customization")
    (setq modus-themes-common-palette-overrides
        '((bg-mode-line-active bg-blue-intense)
             (fg-mode-line-active fg-main)
             (border-mode-line-active blue-intense))))

(add-hook 'elpaca-after-init-hook 'mrf/customize-modus-theme)

(defun mrf/customize-ef-theme ()
    (message ">> Applying ef-themes customization")      
    (defface ef-themes-fixed-pitch
        '((t (:background "#242635" :extend t :font "Courier New")))
        "Face used for the source block background.")
    (when (featurep 'org)
	(mrf/org-font-setup))
    (setq ef-themes-common-palette-override
        '(  (bg-mode-line bg-blue-intense)
             (fg-mode-line fg-main)
             (border-mode-line-active blue-intense))))
(add-hook 'org-load-hook 'mrf/customize-ef-theme)
(add-hook 'elpaca-after-init-hook 'mrf/customize-ef-theme)

;;; --------------------------------------------------------------------------

(add-to-list 'custom-theme-load-path (expand-file-name "Themes" custom-docs-dir))

(mrf/org-theme-override-values)
(use-package ef-themes :init (mrf/customize-ef-theme))
(use-package modus-themes :init (mrf/customize-modus-theme))
(use-package material-theme)
(use-package color-theme-modern)
(use-package color-theme-sanityinc-tomorrow)
(use-package darktooth-theme)
(use-package zenburn-theme :defer t)

;;; --------------------------------------------------------------------------
;; (add-hook 'emacs-startup-hook #'(mrf/load-theme-from-selector))
;; (mrf/load-theme-from-selector)
;; For terminal mode we choose Material theme

(if (not (display-graphic-p))
    (progn
	  (defun load-terminal-theme ()
	      (load-theme (intern default-terminal-theme) t))
	  (add-hook 'elpaca-after-init-hook 'load-terminal-theme))
  (add-hook 'emacs-startup-hook 'reload-theme--from-startup))

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
    (message (format ">> mrf/update-lage-display %S" frame))    
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
;; (add-hook 'elpaca-after-init-hook #'mrf/frame-recenter)

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
    (add-hook 'elpaca-after-init-hook
        (lambda ()
            (progn
                (mrf/update-face-attribute)
                (mrf/frame-recenter)))
        ))

;;; --------------------------------------------------------------------------

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
    :config
    (spacious-padding-mode t))

;; Read the doc string of `spacious-padding-subtle-mode-line' as it
;; is very flexible and provides several examples.
;; (setq spacious-padding-subtle-mode-line
;;       `( :mode-line-active 'default
;;          :mode-line-inactive vertical-border))

;;; --------------------------------------------------------------------------

(defun mrf/org-font-setup ()
    "Setup org mode fonts."
    (use-package faces :ensure nil)

    (font-lock-add-keywords
        'org-mode
        '(("^ *\\([-]\\) "
              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
    
    (set-face-attribute 'org-block nil    :foreground 'unspecified
        :inherit 'fixed-pitch :font "JetBrains Mono" )
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

    (dolist (face '((org-level-1 . 1.75)
                       (org-level-2 . 1.5)
                       (org-level-3 . 1.25)
                       (org-level-4 . 1.15)
                       (org-level-5 . 1.1)
                       (org-level-6 . 1.1)
                       (org-level-7 . 1.1)
                       (org-level-8 . 1.1)))
        (set-face-attribute (car face) nil :font "SF Pro" :weight 'regular
            :height (cdr face))))

;; -----------------------------------------------------------------

(defun mrf/org-mode-visual-fill ()
    (interactive)
    (setq visual-fill-column-width custom-org-fill-column
        visual-fill-column-center-text enable-org-fill-column-centering)
    (visual-fill-column-mode 1))

(defun mrf/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1)
    (mrf/org-mode-visual-fill)
    (setq org-ellipsis " ▾")
    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    ;; (use-package org-habit)
    ;; (add-to-list 'org-modules 'org-habit)
    ;; (setq org-habit-graph-column 60)
    (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
             (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)"
                 "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
    (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
             ("Tasks.org" :maxlevel . 1))))

;;; --------------------------------------------------------------------------

(defun mrf/org-setup-agenda ()
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
    ) ;; mrf/org-setup-agenda

;;; --------------------------------------------------------------------------

(defun mrf/org-setup-capture-templates ()
    (setq org-capture-templates
        `(("t" "Tasks / Projects")
             ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
                 "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

             ("j" "Journal Entries")
             ("jj" "Journal" entry
                 (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
                 "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
                 ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
                 :clock-in :clock-resume
                 :empty-lines 1)
             ("jm" "Meeting" entry
                 (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
                 "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
                 :clock-in :clock-resume
                 :empty-lines 1)

             ("w" "Workflows")
             ("we" "Checking Email" entry (file+olp+datetree
                                              "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
                 "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

             ("m" "Metrics Capture")
             ("mw" "Weight" table-line (file+headline
                                           "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org"
                                           "Weight")
                 "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t))))

;;; --------------------------------------------------------------------------

(use-package org
    :preface
    (mrf/org-theme-override-values)
    :commands (org-capture org-agenda)
    :defer t
    :hook (org-mode . mrf/org-mode-setup)
    :bind (:map org-mode-map
              ("C-c e" . org-edit-src-code))
    :config
    (message "ORG-MODE Configured")
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
    (mrf/org-setup-agenda)
    (mrf/org-setup-capture-templates)
    (mrf/org-font-setup)
    (yas-global-mode t)
    (define-key global-map (kbd "C-c j")
        (lambda () (interactive) (org-capture nil "jj"))))

;;; --------------------------------------------------------------------------

(use-package org-modern
    :disabled
    :after org
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
      ;; Edit settings
      org-auto-align-tags nil
      org-tags-column 0
      org-catch-invisible-edits 'show-and-error
      org-special-ctrl-a/e t
      org-insert-heading-respect-content t

      ;; Org styling, hide markup etc.
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
      "◀── now ─────────────────────────────────────────────────")
    (global-org-modern-mode))

;;; --------------------------------------------------------------------------

(use-package org-superstar
    :after org
    :hook (org-mode . org-superstar-mode))

;;; --------------------------------------------------------------------------

(with-eval-after-load 'org
    (org-babel-do-load-languages
        'org-babel-load-languages
        '((emacs-lisp . t)
             (js . t)
             (shell . t)
             (python . t)))

    (push '("conf-unix" . conf-unix) org-src-lang-modes))

;;; --------------------------------------------------------------------------

(with-eval-after-load 'org
    ;; This is needed as of Org 9.2
    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python")))

;;; --------------------------------------------------------------------------
;; Automatically tangle our Configure.org config file when we save it
;; Org files that should use this need to add a '#+auto_tangle: t'
;; in the org file.
(use-package org-auto-tangle
    :disabled
    :after org
    :hook (org-mode . org-auto-tangle-mode))

;;; --------------------------------------------------------------------------

(with-eval-after-load 'org
    (require 'ox-gfm nil t))

;;; --------------------------------------------------------------------------
;; (use-package emacsql)
;; (use-package emacsql-sqlite)

(use-package org-roam
    ;; :demand t  ;; Ensure org-roam is loaded by default
    :init
    (setq org-roam-v2-ack t)
    :after org
    :custom
    (org-roam-directory (expand-file-name "RoamNotes" custom-docs-dir))
    (org-roam-completion-everywhere t)
    (org-roam-db-location (expand-file-name "RoamNotes" custom-docs-dir))
    :bind (("C-c n l" . org-roam-buffer-toggle)
              ("C-c n f" . org-roam-node-find)
              ("C-c n i" . org-roam-node-insert)
              ("C-c n I" . org-roam-node-insert-immediate)
              ("C-c n p" . my/org-roam-find-project)
              ("C-c n t" . my/org-roam-capture-task)
              ("C-c n b" . my/org-roam-capture-inbox)
              :map org-mode-map
              ("C-M-i" . completion-at-point)
              :map org-roam-dailies-map
              ("Y" . org-roam-dailies-capture-yesterday)
              ("T" . org-roam-dailies-capture-tomorrow))
    :bind-keymap
    ("C-c n d" . org-roam-dailies-map)
    :config
    (require 'org-roam-dailies) ;; Ensure the keymap is available
    (my/org-roam-refresh-agenda-list)
    (add-to-list 'org-after-todo-state-change-hook
        (lambda ()
            (when (equal org-state "DONE")
                (my/org-roam-copy-todo-to-today))))
    (org-roam-db-autosync-mode))

(defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (push arg args))
             (org-roam-capture-templates
                 (list (append (car org-roam-capture-templates)
                           '(:immediate-finish t)))))
        (apply #'org-roam-node-insert args)))

;;; --------------------------------------------------------------------------
;; The buffer you put this code in must have lexical-binding set to t!
;; See the final configuration at the end for more details.

(defun my/org-roam-filter-by-tag (tag-name)
    (lambda (node)
        (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
    (mapcar #'org-roam-node-file
        (seq-filter
            (my/org-roam-filter-by-tag tag-name)
            (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
    (interactive)
    (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

;; Build the agenda list the first time for the session

;;; --------------------------------------------------------------------------

(defun my/org-roam-project-finalize-hook ()
    "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
    ;; Remove the hook since it was added temporarily
    (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Add project file to the agenda list if the capture was confirmed
    (unless org-note-abort
        (with-current-buffer (org-capture-get :buffer)
            (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-find-project ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
        nil
        nil
        (my/org-roam-filter-by-tag "Project")
        :templates
        '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
              :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
              :unnarrowed t))))

(global-set-key (kbd "C-c n p") #'my/org-roam-find-project)

;;; --------------------------------------------------------------------------

(defun my/org-roam-capture-inbox ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create)
        :templates '(("i" "inbox" plain "* %?"
                         :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

;;; --------------------------------------------------------------------------

(defun my/org-roam-capture-task ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Capture the new task, creating the project file if necessary
    (org-roam-capture- :node (org-roam-node-read nil
                                 (my/org-roam-filter-by-tag "Project"))
        :templates '(("p" "project" plain "** TODO %?"
                         :if-new
                         (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                             ("Tasks"))))))

;;; --------------------------------------------------------------------------

(defun my/org-roam-copy-todo-to-today ()
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

(use-package toc-org
    :after org markdown-mode
    :hook
    (org-mode . toc-org-mode)
    (markdown-mode-hook . toc-org-mode)
    :bind (:map markdown-mode-map
	      ("C-c C-o" . toc-org-markdown-follow-thing-at-point)))

;;; --------------------------------------------------------------------------

(use-package ace-window
    ;;:ensure (:repo "abo-abo/ace-window" :fetcher github)
    :bind ("M-o" . ace-window))

;;; --------------------------------------------------------------------------
;;; Window Number

(use-package winum
    ;;:ensure (:host github :repo "deb0ch/emacs-winum")
    :config (winum-mode))

;;; --------------------------------------------------------------------------
;;; Treemacs

(use-package treemacs
    :after (:all winum ace-window)
    :bind (:map global-map
              ("M-0"       . treemacs-select-window)
              ("C-x t 1"   . treemacs-delete-other-windows)
              ("C-x t t"   . treemacs)
              ("C-x t d"   . treemacs-select-directory)
              ("C-x t B"   . treemacs-bookmark)
              ("C-x t C-t" . treemacs-find-file)
              ("C-x t M-t" . treemacs-find-tag))
    :config
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay        0.5
        treemacs-directory-name-transformer      #'identity
        treemacs-display-in-side-window          t
        treemacs-eldoc-display                   'simple
        treemacs-file-event-delay                2000
        treemacs-file-extension-regex            treemacs-last-period-regex-value
        treemacs-file-follow-delay               0.2
        treemacs-file-name-transformer           #'identity
        treemacs-follow-after-init               t
        treemacs-expand-after-init               t
        treemacs-find-workspace-method           'find-for-file-or-pick-first
        treemacs-git-command-pipe                ""
        treemacs-goto-tag-strategy               'refetch-index
        treemacs-header-scroll-indicators        '(nil . "^^^^^^")
        treemacs-hide-dot-git-directory          t
        treemacs-indentation                     2
        treemacs-indentation-string              " "
        treemacs-is-never-other-window           nil
        treemacs-max-git-entries                 5000
        treemacs-missing-project-action          'ask
        treemacs-move-forward-on-expand          nil
        treemacs-no-png-images                   nil
        treemacs-no-delete-other-windows         t
        treemacs-project-follow-cleanup          nil
        treemacs-persist-file                    (expand-file-name
                                                     ".cache/treemacs-persist"
                                                     user-emacs-directory)
        treemacs-position                        'left
        treemacs-read-string-input               'from-child-frame
        treemacs-recenter-distance               0.1
        treemacs-recenter-after-file-follow      nil
        treemacs-recenter-after-tag-follow       nil
        treemacs-recenter-after-project-jump     'always
        treemacs-recenter-after-project-expand   'on-distance
        treemacs-litter-directories              '("/node_modules"
                                                      "/.venv"
                                                      "/.cask"
                                                      "/__pycache__")
        treemacs-project-follow-into-home        nil
        treemacs-show-cursor                     nil
        treemacs-show-hidden-files               t
        treemacs-silent-filewatch                nil
        treemacs-silent-refresh                  nil
        treemacs-sorting                         'alphabetic-asc
        treemacs-select-when-already-in-treemacs 'move-back
        treemacs-space-between-root-nodes        t
        treemacs-tag-follow-cleanup              t
        treemacs-tag-follow-delay                1.5
        treemacs-text-scale                      nil
        treemacs-user-mode-line-format           nil
        treemacs-user-header-line-format         nil
        treemacs-wide-toggle-width               70
        treemacs-width                           38
        treemacs-width-increment                 1
        treemacs-width-is-initially-locked       t
        treemacs-workspace-switch-cleanup        nil)

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

;;; --------------------------------------------------------------------------

(use-package treemacs-icons-dired
    :after treemacs
    :hook (dired-mode . treemacs-icons-dired-enable-once))

;;; --------------------------------------------------------------------------

;; (use-package treemacs-perspective
;;    :disabled
;;    :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;    :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
    ;;:ensure (:files ("src/extra/treemacs-persp.el" "treemacs-persp-pkg.el"):host github :repo "Alexander-Miller/treemacs")
    :after (:any treemacs persp-mode) ;;or perspective vs. persp-mode
    :config (treemacs-set-scope-type 'Perspectives))

;;; --------------------------------------------------------------------------

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
    :after treemacs
    :config (treemacs-set-scope-type 'Tabs))

;;; --------------------------------------------------------------------------

(use-package treemacs-all-the-icons
    :after treemacs
    :if (display-graphic-p))

;;; --------------------------------------------------------------------------

(use-package dashboard
    :custom
    (dashboard-items '(   (recents . 15)
                          (bookmarks . 10)
                          (projects . 10)))
    (dashboard-center-content t)
    (dashboard-set-heading-icons t)
    (dashboard-set-file-icons t)
    (dashboard-footer-messages '("Greetings Program!"))
    (dashboard-banner-logo-title "Welcome to Emacs!")
    (dashboard-startup-banner 'logo)
    :bind ("C-c d" . dashboard-open)
    :config
    (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
    (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
    (dashboard-setup-startup-hook))

;;; --------------------------------------------------------------------------
;;; Emacs Polyglot is the Emacs LSP client that stays out of your way:

(defvar mrf/clangd-path (executable-find "clangd")
    "Clangd executable path.")

(defun mrf/projectile-proj-find-function (dir)
    "Find the project `DIR' function for Projectile.
Thanks @wyuenho on GitHub"
    (let ((root (projectile-project-root dir)))
        (and root (cons 'transient root))))

(use-package track-changes
    :ensure (:package "track-changes" :source nil :protocol https :inherit t :depth 1 :repo "https://github.com/emacs-mirror/emacs" :local-repo "track-changes" :branch "master" :files ("lisp/emacs-lisp/track-changes.el" (:exclude ".git"))))

(use-package eglot
    :when (equal custom-ide 'custom-ide-eglot)
    :ensure (:repo "https://github.com/emacs-mirror/emacs" :local-repo "eglot" :branch "master"
		:files ("lisp/progmodes/eglot.el" "doc/emacs/doclicense.texi" "doc/emacs/docstyle.texi"
			   "doc/misc/eglot.texi" "etc/EGLOT-NEWS" (:exclude ".git")))
    :after eldoc track-changes company
    ;; :after (:all company which-key eldoc jsonrpc)
    :init
    (setq company-backends
        (cons 'company-capf
            (remove 'company-capf company-backends)))
    :hook
    (lisp-mode . eglot-ensure)
    (python-mode . eglot-ensure)
    (go-mode . eglot-ensure)
    ;; (c-mode . eglot-ensure)
    ;; (c++-mode . eglot-ensure)
    ;; (prog-mode . eglot-ensure)
    :config
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
    (which-key-add-key-based-replacements "C-c g r" "find-symbol-reference")
    (which-key-add-key-based-replacements "C-c g o" "find-defitions-other-window")
    (which-key-add-key-based-replacements "C-c g g" "find-defitions")
    (which-key-add-key-based-replacements "C-c g ?" "eldoc-definition")
    ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)
    (add-to-list 'eglot-stay-out-of 'flymake)
    ;; (add-to-list 'eglot-server-programs '((c-mode c++-mode) "clangd"))
    (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
    (setq-default eglot-workspace-configuration
	'((:gopls .
	      ((staticcheck . t)
		  (matcher . "CaseSensitive")))))
    (setq-default eglot-workspace-configuration
        '((:pylsp . (:configurationSources ["flake8"]
			:plugins (:pycodestyle (:enabled nil)
                                     :mccabe (:enabled nil)
                                     :flake8 (:enabled t)))))))

;;; --------------------------------------------------------------------------
;;; Language Server Protocol

(when (equal custom-ide 'custom-ide-lsp)
    (eval-when-compile (defvar lsp-enable-which-key-integration)))

(defun mrf/lsp-mode-setup ()
    "Custom LSP setup function."
    (when (equal custom-ide 'custom-ide-lsp)
        (message "Set up LSP header-line and other vars")
        (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
        (setq lsp-clangd-binary-path "/usr/bin/clangd")'
        (lsp-headerline-breadcrumb-mode)))

(use-package lsp-mode
    :when (equal custom-ide 'custom-ide-lsp)
    :commands (lsp lsp-deferred)
    :hook (lsp-mode . mrf/lsp-mode-setup)
    :init
    (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
    :config
    (lsp-enable-which-key-integration t))

(use-package lsp-ui
    :when (equal custom-ide 'custom-ide-lsp)
    :after lsp
    :config (setq lsp-ui-sideline-enable t
                lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-sideline-ignore-duplicates t
                lsp-ui-doc-delay 3
                lsp-ui-doc-position 'top
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-header nil
                lsp-ui-doc-show-with-cursor t
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-childframe t)
    :commands lsp-ui-mode
    :bind (:map lsp-ui-mode-map
              ("C-c l d" . lsp-ui-doc-focus-frame))
    :custom
    (lsp-ui-doc-position 'bottom)
    :hook (lsp-mode . lsp-ui-mode))

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

;;; --------------------------------------------------------------------------

(use-package markdown-mode
    :when (equal custom-ide 'custom-ide-lsp-bridge))
    ;;:ensure (:fetcher github :repo "jrblevin/markdown-mode"))

(use-package lsp-bridge
    :when (equal custom-ide 'custom-ide-lsp-bridge)
    :ensure (:host github :repo "manateelazycat/lsp-bridge" :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources") :build (:not compile))
    :custom
    (lsp-bridge-python-lsp-server "pylsp")
    :config
    (global-lsp-bridge-mode))

;;; --------------------------------------------------------------------------

(use-package anaconda-mode
    :when (equal custom-ide 'custom-ide-anaconda)
    :bind (:map python-mode-map
              ("C-c g o" . anaconda-mode-find-definitions-other-frame)
              ("C-c g g" . anaconda-mode-find-definitions)
              ("C-c C-x" . next-error))
    :config
    (which-key-add-key-based-replacements "C-c g o" "find-defitions-other-window")
    (which-key-add-key-based-replacements "C-c g g" "find-defitions")
    (require 'pyvenv)
    :hook
    (python-mode-hook . anaconda-eldoc-mode))

;;; --------------------------------------------------------------------------

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
    (message "elpy loaded")
    (use-package jedi)
    (use-package flycheck
        :when (equal custom-ide 'custom-ide-elpy)
        :after elpy
	:defer t
        :diminish FlM
        ;;:ensure (:host github :repo "flycheck/flycheck")
        :hook (elpy-mode . flycheck-mode))      (which-key-add-key-based-replacements "C-c g a" "goto-assignment")
    (which-key-add-key-based-replacements "C-c g o" "find-defitions-other-window")
    (which-key-add-key-based-replacements "C-c g g" "find-defitions")
    (which-key-add-key-based-replacements "C-c g ?" "eldoc-definition")
    (elpy-enable))

;;; ------------------------------------------------------------------------
(use-package jsonrpc)
(use-package dape
    :when (equal debug-adapter 'enable-dape)
    :init
    (define-dape-hydra)
    :defer t
    :after (:any jsonrpc hydra python)
    ;; To use window configuration like gud (gdb-mi)
    ;; :init
    ;; (setq dape-buffer-window-arrangement 'gud)
    :custom
    (dape-buffer-window-arrangement 'right)  ;; Info buffers to the right
    ;; To not display info and/or buffers on startup
    ;; (remove-hook 'dape-on-start-hooks 'dape-info)
    (remove-hook 'dape-on-start-hooks 'dape-repl)

    ;; To display info and/or repl buffers on stopped
    ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
    ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

    ;; By default dape uses gdb keybinding prefix
    ;; If you do not want to use any prefix, set it to nil.
    ;; (setq dape-key-prefix "\C-x\C-a")

    ;; Kill compile buffer on build success
    ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

    ;; Save buffers on startup, useful for interpreted languages
    ;; (add-hook 'dape-on-start-hooks
    ;;           (defun dape--save-on-start ()
    ;;             (save-some-buffers t t)))
    ;; :bind (:map prog-mode-map ("C-c ." . dape-hydra/body))
    :config
    (define-dape-hydra)
    (bind-keys :map prog-mode-map
      ("C-c ." . dape-hydra/body))
    (message "DAPE Configured"))

;;; --------------------------------------------------------------------------
;;; Debug Adapter Protocol
(use-package dap-mode
    :when (equal debug-adapter 'enable-dap-mode)
    :after hydra
    ;; Uncomment the config below if you want all UI panes to be hidden by default!
    ;; :custom
    ;; (lsp-enable-dap-auto-configure nil)
    :commands dap-debug
    :custom
    (dap-auto-configure-features '(sessions locals breakpoints expressions repl controls tooltip))
    :config
    (define-dap-hydra)
    (bind-keys :map prog-mode-map
      ("C-c ." . dap-hydra/body))
    (dap-ui-mode 1)
    (message "DAP mode loaded and configured."))

;;; --------------------------------------------------------------------------

(setq mrf/vscode-js-debug-dir (file-name-concat user-emacs-directory "dape/vscode-js-debug"))

(defun mrf/install-vscode-js-debug ()
    "Run installation procedure to install JS debugging support"
    (interactive)
    (mkdir mrf/vscode-js-debug-dir t)
    (let ((default-directory (expand-file-name mrf/vscode-js-debug-dir)))

        (vc-git-clone "https://github.com/microsoft/vscode-js-debug.git" "." nil)
        (message "git repository created")
        (call-process "npm" nil "*snam-install*" t "install")
        (message "npm dependencies installed")
        (call-process "npx" nil "*snam-install*" t "gulp" "dapDebugServer")
        (message "vscode-js-debug installed")))

;;; --------------------------------------------------------------------------

;; (mrf/install-vscode-js-debug)

;;; --------------------------------------------------------------------------

(defun mrf/dape-end-debug-session ()
    "End the debug session."
    (interactive)
    (dape-quit))

(defun mrf/dape-delete-all-debug-sessions ()
    "End the debug session and delete all breakpoints."
    (interactive)
    (dape-breakpoint-remove-all)
    (mrf/dape-end-debug-session))

(defun define-dape-hydra ()
    (defhydra dape-hydra (:color pink :hint nil :foreign-keys run)
      "
  ^Stepping^          ^Switch^                 ^Breakpoints^          ^Debug^                     ^Eval
  ^^^^^^^^----------------------------------------------------------------------------------------------------------------
  _._: Next           _st_: Thread            _bb_: Toggle           _dd_: Debug                 _ee_: Eval Expression
  _/_: Step in        _si_: Info              _bd_: Delete           _dw_: Watch dwim
  _,_: Step out       _sf_: Stack Frame       _ba_: Add              _dx_: end session
  _c_: Continue       _su_: Up stack frame    _bc_: Set condition    _dX_: end all sessions
  _r_: Restart frame  _sd_: Down stack frame  _bl_: Set log message
  _Q_: Disconnect     _sR_: Session Repl
                      _sU_: Info Update

"
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
      ("dx" mrf/dape-end-debug-session)
      ("dX" mrf/dape-delete-all-debug-sessions)
      ("x" nil "exit Hydra" :color yellow)
      ("q" mrf/dape-end-debug-session "quit" :color blue)
      ("Q" mrf/dape-delete-all-debug-sessions :color red)))

;;; --------------------------------------------------------------------------

(setq dap-lldb-debug-program
    "/Users/strider/Developer/plain_unix/llvm-project/build/bin/lldb-dap")

(defun mrf/populate-lldb-start-file-args (conf)
    "Populate CONF with the required arguments."
    (-> conf
        (dap--put-if-absent :dap-server-path dap-lldb-debug-program)
        (dap--put-if-absent :type "lldb-dap")
        (dap--put-if-absent :cwd default-directory)
        (dap--put-if-absent :program (funcall dap-lldb-debugged-program-function))
        (dap--put-if-absent :name "LLDB Debug")))

    (use-package dap-cpptools
      :when (equal debug-adapter 'enable-dap-mode)
      :disabled
        :after dap-mode
        ;;:ensure (:host github :repo "emacs-lsp/dap-mode")
      :config
      (use-package dap-lldb
          :disabled
            ;;:ensure (:host github :repo "emacs-lsp/dap-mode")
            :after dap-mode
            :config
            (dap-register-debug-provider "lldb-dap" 'mrf/populate-lldb-start-file-args)
            (dap-register-debug-template "LLDB DAP :: Run from project directory"
              (list :type "lldb-dap"
                    :name "LLDB using DAP"
                    :program "a.out"
                    :request "launch"))))

;;; --------------------------------------------------------------------------
;;; DAP for Python


(use-package dap-python
    :ensure (:package "dap-python" :type git :host github :repo "emacs-lsp/dap-mode")
    :when (equal debug-adapter 'enable-dap-mode)
    ;;:ensure (:host github :repo "emacs-lsp/dap-mode")
    :after dap-mode
    :config
    (setq dap-python-executable "python3") ;; Otherwise it looks for 'python' else error.
    (setq dap-python-debugger 'debugpy)
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

;;; --------------------------------------------------------------------------
;;; DAP for NodeJS


(defun my-setup-dap-node ()
    "Require dap-node feature and run dap-node-setup if VSCode module isn't already installed"
    (require 'dap-node)
    (unless (file-exists-p dap-node-debug-path) (dap-node-setup)))

(use-package dap-node
    :when (equal debug-adapter 'enable-dap-mode)
    :disabled
    :hook ((typescript-mode . my-setup-dap-node)
              (js2-mode . my-setup-dap-node))
    ;;:ensure (:host github :repo "emacs-lsp/dap-mode" :files (:defaults "icons" "dap-mode-pkg.el"))
    :after dap-mode
    :config
    (require 'dap-firefox)
    (dap-register-debug-template
        "Launch index.ts"
        (list :type "node"
            :request "launch"
            :program "${workspaceFolder}/index.ts"
            :dap-compilation "npx tsc index.ts --outdir dist --sourceMap true"
            :outFiles (list "${workspaceFolder}/dist/**/*.js")
            :name "Launch index.ts")))
;; (dap-register-debug-template
;;    "Launch index.ts"
;;    (list :type "node"
;;    :request "launch"
;;    :program "${workspaceFolder}/index.ts"
;;    :dap-compilation "npx tsc index.ts --outdir dist --sourceMap true"
;;    :outFiles (list "${workspaceFolder}/dist/**/*.js")
;;    :name "Launch index.ts"))

;;; --------------------------------------------------------------------------

(defun mrf/end-debug-session ()
    "End the debug session and delete project Python buffers."
    (interactive)
    (kill-matching-buffers "\*Python :: Run file [from|\(buffer]*" nil :NO-ASK)
    (kill-matching-buffers "\*Python: Current File*" nil :NO-ASK)
    (kill-matching-buffers "\*dap-ui-*" nil :NO-ASK)
    (dap-disconnect (dap--cur-session)))

(defun mrf/delete-all-debug-sessions ()
    "End the debug session and delete project Python buffers and all breakpoints."
    (interactive)
    (dap-breakpoint-delete-all)
    (mrf/end-debug-session))

(defun mrf/begin-debug-session ()
    "Begin a debug session with several dap windows enabled."
    (interactive)
    (dap-ui-show-many-windows)
    (dap-debug))

(defun define-dap-hydra ()
    (defhydra dap-hydra (:color pink :hint nil :foreign-keys run)
      "
  ^Stepping^          ^Switch^                 ^Breakpoints^          ^Debug^                     ^Eval
  ^^^^^^^^----------------------------------------------------------------------------------------------------------------
  _._: Next           _ss_: Session            _bb_: Toggle           _dd_: Debug                 _ee_: Eval
  _/_: Step in        _st_: Thread             _bd_: Delete           _dr_: Debug recent          _er_: Eval region
  _,_: Step out       _sf_: Stack frame        _ba_: Add              _dl_: Debug last            _es_: Eval thing at point
  _c_: Continue       _su_: Up stack frame     _bc_: Set condition    _de_: Edit debug template   _ea_: Add expression.
  _r_: Restart frame  _sd_: Down stack frame   _bh_: Set hit count    _ds_: Debug restart
  _Q_: Disconnect     _sl_: List locals        _bl_: Set log message  _dx_: end session
                    _sb_: List breakpoints                          _dX_: end all sessions
                    _sS_: List sessions
                    _sR_: Session Repl
"
      ("n" dap-next)    ("i" dap-step-in)    ("o" dap-step-out)   ("." dap-next)
      ("/" dap-step-in) ("," dap-step-out)   ("c" dap-continue)   ("r" dap-restart-frame)

      ("ss" dap-switch-session) ("st" dap-switch-thread)    ("sf" dap-switch-stack-frame)
      ("su" dap-up-stack-frame) ("sd" dap-down-stack-frame) ("sl" dap-ui-locals)
      ("sb" dap-ui-breakpoints) ("sR" dap-ui-repl)          ("sS" dap-ui-sessions)

      ("bb" dap-breakpoint-toggle)    ("ba" dap-breakpoint-add)           ("bd" dap-breakpoint-delete)
      ("bc" dap-breakpoint-condition) ("bh" dap-breakpoint-hit-condition) ("bl" dap-breakpoint-log-message)

      ("dd" dap-debug)      ("dr" dap-debug-recent) ("ds" dap-debug-restart)
      ("dl" dap-debug-last) ("de" dap-debug-edit-template)

      ("ee" dap-eval) ("ea" dap-ui-expressions-add) ("er" dap-eval-region) ("es" dap-eval-thing-at-point)

      ("dx" mrf/end-debug-session) ("dX" mrf/delete-all-debug-sessions)

      ("x" nil "exit Hydra" :color yellow) ("q" mrf/end-debug-session "quit" :color blue)
      ("Q" mrf/delete-all-debug-sessions :color red)))

;;; --------------------------------------------------------------------------

(use-package realgud
    :disabled
    :after c-mode)

(use-package realgud-lldb
    :disabled
    :after realgud)
    ;;:ensure (:files (:defaults ("lldb" "lldb/*.el") "realgud-lldb-pkg.el") :host github :repo "realgud/realgud-lldb"))

;;; --------------------------------------------------------------------------

(use-package orderless
    :when (or (equal completion-handler 'comphand-vertico)
              (equal completion-handler 'comphand-ivy-counsel))
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles basic partial-completion)))))

;;; --------------------------------------------------------------------------
;;; Swiper and IVY mode

(use-package ivy
    :when (equal completion-handler 'comphand-ivy-counsel)
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

;;; --------------------------------------------------------------------------

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

;;; --------------------------------------------------------------------------

(use-package swiper
    :when (equal completion-handler 'comphand-ivy-counsel)
    :after ivy)

;;; --------------------------------------------------------------------------

(use-package counsel
    :when (equal completion-handler 'comphand-ivy-counsel)
    :bind (   ("C-M-j" . 'counsel-switch-buffer)
              ("M-x" . 'counsel-M-x)
              ("C-x C-f" . 'counsel-find-file)
              ("C-c C-r" . 'ivy-resume)
              :map minibuffer-local-map
              ("C-r" . 'counsel-minibuffer-history))
    :custom
    (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
    :config
    (counsel-mode 1))

;;; --------------------------------------------------------------------------

(use-package ivy-prescient
    :when (equal completion-handler 'comphand-ivy-counsel)
    :after ivy
    :custom
    (prescient-persist-mode t)
    (ivy-prescient-mode t)
    (ivy-prescient-enable-filtering t))

;;; --------------------------------------------------------------------------

;;;; Code Completion
(use-package corfu
    :when enable-corfu
    ;; Optional customizations
    :custom
    (corfu-cycle t)                 ; Allows cycling through candidates
    (corfu-auto t)                  ; Enable auto completion
    (corfu-auto-prefix 2)
    (corfu-auto-delay 0.8)
    (corfu-popupinfo-delay '(0.5 . 0.2))
    (corfu-preview-current 'insert) ; insert previewed candidate
    (corfu-preselect 'prompt)
    (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
    ;; Optionally use TAB for cycling, default is `corfu-complete'.
    :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil))
    :init
    (global-corfu-mode)
    (corfu-history-mode)
    (corfu-popupinfo-mode) ; Popup completion info
    :config
        (use-package corfu-prescient
        :after corfu))
    (add-hook 'eshell-mode-hook
        (lambda () (setq-local corfu-quit-at-boundary t
                       corfu-quit-no-match t
                       corfu-auto nil)
            (corfu-mode)))

;;; --------------------------------------------------------------------------

(defun mrf/vertico-other ()
    (use-package vertico-prescient
	:ensure (:package "vertico-prescient" :fetcher github
		    :repo "radian-software/prescient.el" :files ("vertico-prescient.el")))

    (use-package vertico-posframe
	:ensure (:package "vertico-posframe" :repo "https://github.com/tumashu/vertico-posframe"
		    :local-repo "vertico-posframe" :files ("*" (:exclude ".git")))
        :custom (vertico-posframe-parameters
		    '((left-fringe . 8)
			 (right-fringe . 8)))))

(use-package vertico
    :when (equal completion-handler 'comphand-vertico)
    :demand t
    ;;:wait t
    ;;:ensure (:repo "minad/vertico" :files (:defaults "extensions/vertico-*.el") :fetcher github)
    :custom
    (recentf-mode t)
    (vertico-count 12)
    (vertico-cycle nil)
    (vertico-multiform-mode 1)
    :config
    (vertico-mode)
    (mrf/vertico-other)
    ;; :bind ("C-x C-f" . ido-find-file)
    ;; Clean up file path when typing
    :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)
              ;; Make sure vertico state is saved
              (minibuffer-setup . vertico-repeat-save)))

;;; --------------------------------------------------------------------------

(use-package marginalia
    :when (equal completion-handler 'comphand-vertico)
    :after vertico
    :custom
    (marginalia-max-relative-age 0)
    (marginalia-align 'left)
    (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :config
    (marginalia-mode t))

;;; --------------------------------------------------------------------------
;; Example configuration for Consult

(use-package consult
    :when (equal completion-handler 'comphand-vertico)
    ;; Replace bindings. Lazily loaded due by `use-package'.
    :after vertico
    :bind (;; C-c bindings in `mode-specific-map'
	      ([remap isearch-forward] . consult-line)
	      ([remap isearch-backward] . consult-line)
              ("C-c M-x" . consult-mode-command)
              ("C-c h" . consult-history)
              ("C-c k" . consult-kmacro)
              ("C-c m" . consult-man)
              ("C-c i" . consult-info)
              ([remap Info-search] . consult-info)
              ;; C-x bindings in `ctl-x-map'
              ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
              ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
              ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
              ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
              ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
              ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
              ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
              ;; Custom M-# bindings for fast register access
              ("M-#" . consult-register-load)
              ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
              ("C-M-#" . consult-register)
              ;; Other custom bindings
              ("M-y" . consult-yank-pop)                ;; orig. yank-pop
              ;; M-g bindings in `goto-map'
              ("M-g e" . consult-compile-error)
              ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
              ("M-g g" . consult-goto-line)             ;; orig. goto-line
              ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
              ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
              ("M-g m" . consult-mark)
              ("M-g k" . consult-global-mark)
              ("M-g i" . consult-imenu)
              ("M-g I" . consult-imenu-multi)
              ;; M-s bindings in `search-map'
              ("M-s d" . consult-find)                  ;; Alternative: consult-fd
              ("M-s c" . consult-locate)
              ("M-s g" . consult-grep)
              ("M-s G" . consult-git-grep)
              ("M-s r" . consult-ripgrep)
              ("M-s l" . consult-line)
              ("M-s L" . consult-line-multi)
              ("M-s k" . consult-keep-lines)
              ("M-s u" . consult-focus-lines)
              ;; Isearch integration
              ("M-s e" . consult-isearch-history)
              :map isearch-mode-map
              ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
              ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
              ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
              ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
              ;; Minibuffer history
              :map minibuffer-local-map
              ("M-s" . consult-history)                 ;; orig. next-matching-history-element
              ("M-r" . consult-history))                ;; orig. previous-matching-history-element

    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)
    ;; The :init configuration is always executed (Not lazy)
    :init
    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config
    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key "M-.")
    ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
        consult-theme :preview-key '(:debounce 0.2 any)
        consult-ripgrep consult-git-grep consult-grep
        consult-bookmark consult-recent-file consult-xref
        consult--source-bookmark consult--source-file-register
        consult--source-recent-file consult--source-project-recent-file
        ;; :preview-key "M-."
        :preview-key '(:debounce 0.4 any))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<")) ;; "C-+"

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; By default `consult-project-function' uses `project-root' from project.el.
    ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
    ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
    ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
    ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
    ;; (autoload 'projectile-project-root "projectile")
    ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
    ;; (setq consult-project-function nil)

;;; --------------------------------------------------------------------------

;; This has to be evaluated at the end of the init since it's possible that the
;; completion-handler variable will not yet be defined at this point in the
;; init phase using elpaca.
(add-hook 'elpaca-after-init-hook
    (lambda ()
	(when (equal completion-handler 'comphand-built-in)
	    (ido-everywhere t))))

;;; --------------------------------------------------------------------------

(use-package flycheck
    :unless (equal custom-ide 'custom-ide-elpy)
    :diminish FlM
    :defer t
    ;;:ensure (:host github :repo "flycheck/flycheck")
    :config
    (eval-after-load 'flycheck
      '(flycheck-package-setup))
    (global-flycheck-mode))

(use-package flycheck-package
    :after flycheck)

;;; --------------------------------------------------------------------------

(defun mrf/tree-sitter-setup ()
    (tree-sitter-hl-mode t)
    (ts-fold-mode t))

(defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package tree-sitter
    :defer t
    :after (:any python python-mode lisp-mode)
    :config
    ;; Activate tree-sitter globally (minor mode registered on every buffer)
    (message "TREE-SITTER Configured")
    (global-tree-sitter-mode)
    :hook
    (tree-sitter-after-on . mrf/tree-sitter-setup)
    (typescript-mode . lsp-deferred)
    ;; (c-mode . lsp-deferred)
    ;; (c++-mode . lsp-deferred)
    (go-mode . lsp-deferred)
    (before-save . lsp-go-install-save-hooks)
    (js2-mode . lsp-deferred))

(use-package tree-sitter-langs
    :after tree-sitter)

(use-package ts-fold
    :disabled
    :after tree-sitter
    ;;:ensure (:host github :repo "emacs-tree-sitter/ts-fold")
    :bind (("C-<tab>" . ts-fold-toggle)
              ("C-c f"   . ts-fold-open-all)))

;;; --------------------------------------------------------------------------

;; (use-package transient
;;     :wait t
;;     :ensure (:repo "https://github.com/magit/transient" :fetcher github
;; 		:local-repo "transient"
;; 		:files ("*" (:exclude ".git"))))

;; (use-package git-commit
;;     :ensure (:fetcher github :repo "magit/magit"
;; 		:files ("lisp/git-commit.el" "lisp/git-commit-pkg.el")
;; 		:old-names (git-commit-mode))
;;     :after transient)

(use-package transient :defer t)
(use-package git-commit :after transient :defer t)
(use-package magit :after git-commit :defer t)

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started

(use-package forge :after magit :defer t)
(use-package treemacs-magit :defer t :after treemacs magit)

;;; --------------------------------------------------------------------------

(defun mrf/load-js-file-hook ()
    (message "Running JS file hook")
    (js2-mode)

    (when (equal debug-adapter 'enable-dap-mode)
        (dap-mode)
        (dap-firefox-setup))

    (when (equal debug-adapter 'enable-dape)
        (dape))

    (highlight-indentation-mode nil)
    (dap-firefox-setup))

(use-package nodejs-repl :defer t)

(defun mrf/nvm-which ()
    (let ((output (shell-command-to-string "source ~/.nvm/nvm.sh; nvm which")))
        (cadr (split-string output "[\n]+" t))))

(setq nodejs-repl-command #'mrf/nvm-which)

;;; --------------------------------------------------------------------------

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

;;; --------------------------------------------------------------------------

(defun mrf/load-c-file-hook ()
    (message "Running C/C++ file hook")
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
(add-to-list 'auto-mode-alist '("\\.c\\'" . mrf/load-c-file-hook))

;;; --------------------------------------------------------------------------

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

;;; --------------------------------------------------------------------------

(defun mrf/set-custom-ide-python-keymaps ()
    (cond
        ((equal custom-ide 'custom-ide-lsp)
            (bind-keys :map python-mode-map
                ("C-c g r" . lsp-find-references)
                ("C-c g o" . xref-find-definitions-other-window)
                ("C-c g g" . xref-find-definitions)
                ("C-c g ?" . eldoc-doc-buffer))
            (message (format ">>> set python-mode-map for %s" custom-ide)))
        ((equal custom-ide 'custom-ide-eglot)
            (bind-keys :map python-mode-map
                ("C-c g r" . eglot-find-implementation)
                ("C-c g o" . xref-find-definitions-other-window)
                ("C-c g g" . xref-find-definitions)
                ("C-c g ?" . eldoc-doc-buffer))
            (message (format ">>> set python-mode-map for %s" custom-ide)))
        ;; Activate LSP and EGLOT *if* selected as custom-ide
        ((equal custom-ide 'custom-ide-elpy)
            (elpy-enable)
            (bind-keys :map python-mode-map
                ("C-c g a" . elpy-goto-assignment)
                ("C-c g o" . elpy-goto-definition-other-window)
                ("C-c g g" . elpy-goto-definition)
                ("C-c g ?" . elpy-doc))
            (message (format ">>> setting python-mode-map for %s" custom-ide)))
        ((equal custom-ide 'custom-ide-lsp-bridge)
            (bind-keys :map python-mode-map
                ("C-c g a" . lsp-bridge-find-reference)
                ("C-c g o" . lsp-bridge-find-def-other-window)
                ("C-c g g" . lsp-bridge-find-def)
                ("C-c g i" . lsp-bridge-find-impl)
                ("C-c g r" . lsp-bridge-rename)
                ("C-c g ?" . lsp-bridge-popup-documentation))
            (message (format ">>> set python-mode-map for %s" custom-ide)))
        ))

;;; --------------------------------------------------------------------------

(defun mrf/load-python-file-hook ()
    (python-mode)
    (setq highlight-indentation-mode -1)
    (setq display-fill-column-indicator-mode t))

(defun mrf/before-save ()
    "Force the check of the current python file being saved."
    (when (eq major-mode 'python-mode) ;; Python Only
        (flycheck-mode 0)
        (flycheck-mode t)
        (message "deleting trailing whitespace enabled")
        (delete-trailing-whitespace)))

(defun mrf/python-mode-triggered ()
    (message ">>> mrf/python-mode-triggered")
    ;; (eldoc-box-hover-at-point-mode t) ;; Using Mitch Key for this
    (if (equal debug-adapter 'enable-dap-mode)
        (unless (featurep 'dap-mode)
            (dap-mode))
        (if (not (featurep 'dape))
            (use-package dape :demand t)))
    (mrf/set-custom-ide-python-keymaps)
    (unless (featurep 'yasnippet)
      (yas-global-mode t))
    (add-hook 'before-save-hook 'mrf/before-save)
    (set-fill-column 80))

(add-to-list 'auto-mode-alist '("\\.py\\'" . mrf/load-python-file-hook))

(use-package python-mode
    :hook (python-mode . mrf/python-mode-triggered))

(use-package blacken :after python) ;Format Python file upon save.

(if (boundp 'python-shell-completion-native-disabled-interpreters)
    (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
    (setq python-shell-completion-native-disabled-interpreters '("python3")))

;;; --------------------------------------------------------------------------

(use-package py-autopep8
    :after python
    :hook (python-mode . py-autopep8-mode))

;;; --------------------------------------------------------------------------

;; This is a helpful macro that is used to put double quotes around a word.
(defalias 'quote-word
    (kmacro "\" M-d \" <left> C-y"))

(defalias 'quote-region
    (kmacro "C-w \" \" <left> C-y <right>"))

(eval-after-load "python"
    #'(bind-keys :map python-mode-map
          ("C-c C-q" . quote-region)
          ("C-c q"   . quote-word)
          ("C-c |"   . display-fill-column-indicator-mode)))

;;; --------------------------------------------------------------------------

(use-package pyvenv-auto
    :after python
    :config (message ">>> Starting pyvenv-auto")
    :hook (python-mode . pyvenv-auto-run))

;;; --------------------------------------------------------------------------

(use-package pydoc
    ;;:ensure (:host github :repo "statmobile/pydoc")
    :after python
    :custom
    (pydoc-python-command "python3")
    (pydoc-pip-version-command "pip3 --version"))

;;; --------------------------------------------------------------------------

(use-package slime
    :mode ("\\.lisp\\'" . slime-mode)
    :config
    (setq inferior-lisp-program "/opt/homebrew/bin/sbcl"))

;;; --------------------------------------------------------------------------

;; (use-package graphql-mode)
(use-package rust-mode
    :disabled
    :init (setq rust-mode-treesitter-derive t)
    :hook (rust-mode . (lambda ()
                         (setq indent-tabs-mode nil)
                         (prettify-symbols-mode)))
    :config
    (setq rust-format-on-save t))

;;; --------------------------------------------------------------------------

(use-package go-mode
    :mode ("\\.go\\'" . go-mode)
    :config
    (cond
	((equal custom-ide 'custom-ide-eglot)
	    (add-hook 'go-mode-hook 'eglot-ensure))
	((equal custom-ide 'custom-ide-lsp)
  	    (add-hook 'go-mode-hook 'lsp-deferred))))

(use-package go-eldoc
    :after go-mode
    :hook (go-mode . go-eldoc-setup)
    :config
    (set-face-attribute 'eldoc-highlight-function-argument nil
        :underline t :foreground "green"
        :weight 'bold))

;;; --------------------------------------------------------------------------

(use-package company
    :after (:any lsp-mode elpy anaconda-mode)
    ;; :ensure (:wait t)
    :hook (elpaca-after-init . global-company-mode)
    :bind (:map company-active-map
		("<tab>" . company-complete-selection))
    :custom
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.0)
    :config
    (cond
	  ((equal custom-ide 'custom-ide-eglot)
	      (bind-keys :map eglot-mode-map
		  ("<tab>" . company-indent-or-complete-common)))
	  ((equal custom-ide 'custom-ide-lsp)
	      (bind-keys :map lsp-mode-map
		  ("<tab>" . company-indent-or-complete-common)))
	((equal custom-ide 'custom-ide-elpy)
	      (bind-keys :map elpy-mode-map
		  ("<tab>" . company-indent-or-complete-common)))
	  ((equal custom-ide 'custom-ide-anaconda)
	      (bind-keys :map anaconda-mode-map
		  ("<tab>" . company-indent-or-complete-common)))))

  ;; IMPORTANT:
  ;; Don't use company at all if lsp-bridge is active.
  ;; lsp-bridge already provides similar functionality.

  ;; :config
  ;; (add-to-list 'company-backends 'company-yasnippet))

;;; --------------------------------------------------------------------------
(use-package company-box
    :after company
    :diminish cb
    :hook (company-mode . company-box-mode))

(use-package company-jedi
    :when  (equal custom-ide 'custom-ide-elpy)
    :after python company
    :config
    (jedi:setup)
    (defun my/company-jedi-python-mode-hook ()
        (add-to-list 'company-backends 'company-jedi))
    (add-hook 'python-mode-hook 'my/company-jedi-python-mode-hook))

(use-package company-anaconda
    :when (equal custom-ide 'custom-ide-anaconda)
    :after anaconda
    :hook (python-mode . anaconda-mode)
    :config
    (eval-after-load "company"
      '(add-to-list 'company-backends 'company-anaconda)))

(defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
	(cons 'go-module root)))

(use-package project
    :ensure nil
    :config
    (cl-defmethod project-root ((project (head go-module)))
	(cdr project))
    (add-hook 'project-find-functions #'project-find-go-module))

;;; --------------------------------------------------------------------------
;; helpful package

(use-package helpful
    :commands (helpful-callable helpful-variable helpful-command helpful-key)
    :custom
    (when (equal completion-handler 'comphand-ivy-counsel)
        (counsel-describe-function-function #'helpful-callable)
        (counsel-describe-variable-function #'helpful-variable))
    :config
    (when (equal completion-handler 'comphand-ivy-counsel)
	(bind-keys
          ([remap describe-function] . counsel-describe-function)
            ([remap describe-variable] . counsel-describe-variable)))
    (bind-keys
	([remap describe-command] . helpful-command)
	([remap describe-key] . helpful-key)))

;;; --------------------------------------------------------------------------

(use-package solaire-mode
    ;;:ensure (:host github :repo "hlissner/emacs-solaire-mode")
    :hook (elpaca-after-init . solaire-global-mode)
    :config
    (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
    (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist))

;;; --------------------------------------------------------------------------
;; Golen Ratio

(use-package golden-ratio
    :when enable-golden-ratio
    :custom
    (golden-ratio-auto-scale t)
    (golden-ratio-adjust-factor .4)
    (golden-ratio-wide-adjust-factor .4)
    (golden-ratio-max-width 100)
    (golden-ratio-exclude-modes '(treemacs-mode
                                     undo-tree-visdualizer-mode
                                     inferior-python-mode
				     use-package-statistics-mode
                                     vundo-mode
                                     which-key-mode
                                     c-mode
                                     cc-mode
                                     dashboard-mode
                                     python-mode
                                     markdown-mode))
    (golden-ratio-exclude-buffer-regexp '("dap*"
                                             "*dape*"
                                             "*python*"))
    :config
    (golden-ratio-mode 1))

;;; --------------------------------------------------------------------------

(use-package neotree
    :when enable-neotree
    :config
    (global-set-key [f8] 'neotree-toggle)
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;;; --------------------------------------------------------------------------

;; (use-package nerd-icons )

;; (use-package doom-modeline
;;   :diabled
;;   :init (doom-modeline-mode 1)
;;   :custom ((doom-modeline-height 15)))

;;; --------------------------------------------------------------------------
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

;;; --------------------------------------------------------------------------

(use-package diff-hl
    :config
    (global-diff-hl-mode))

;;; --------------------------------------------------------------------------

(use-package pulsar
    :config
    (pulsar-global-mode)
    :custom
    (pulsar-pulse t)
    (pulsar-delay 0.10)
    (pulsar-iterations 10)
    (pulsar-face 'pulsar-magenta)
    (pulsar-highlight-face 'pulsar-yellow))

;;; --------------------------------------------------------------------------

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

;;; --------------------------------------------------------------------------

(use-package term+
    ;;:ensure (:repo "tarao/term-plus-el" :fetcher github)
    :commands term
    :config
    (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
    ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

    ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
    (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;;; --------------------------------------------------------------------------

(use-package eterm-256color
    :hook (term-mode . eterm-256color-mode))

;;; --------------------------------------------------------------------------

(use-package vterm
    ;;:ensure (:fetcher github :repo "akermu/emacs-libvterm")
    :commands vterm
    :config
    (setq vterm-environment ("PS1=\\u@\\h:\\w \n$"))
    (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
    (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
    (setq vterm-max-scrollback 10000))

;;; --------------------------------------------------------------------------

(defun efs/configure-eshell ()
    ;; Save command history when commands are entered
    (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

    ;; Truncate buffer for performance
    (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

    ;; Bind some useful keys for evil-mode
    ;; (bind-keys :map eshell-mode-map
    ;;  ("C-r" . eshell-hist-mode)
    ;;  ("<home>" . eshell-bol))

    ;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
    ;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
    ;; (evil-normalize-keymaps)

    (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
    :after eshell)

(use-package eshell
    :ensure nil
    :hook (eshell-first-time-mode . efs/configure-eshell)
    :config
    (with-eval-after-load 'esh-opt
        (setq eshell-destroy-buffer-when-process-dies t)
        (setq eshell-visual-commands '("htop" "zsh" "vim")))
    (eshell-git-prompt-use-theme 'powerline))

;;; --------------------------------------------------------------------------

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls)))

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

;;; --------------------------------------------------------------------------

(defvar mmm-keys-minor-mode-map
    (let ((map (make-sparse-keymap)))
        (bind-keys :map map
            ("M-RET p" . pulsar-pulse-line)
            ("M-RET P" . pulsar-highlight-line)
            ;;("M-RET RET" . mmm-menu)
            ("M-RET d" . dashboard-open)
            ("M-RET f" . mrf/set-fill-column-interactively)
            ("M-RET F" . mrf/set-org-fill-column-interactively)
            ("M-RET i" . ielm)
            ("M-RET v" . vterm-other-window)
            ("M-RET S" . smartparens-strict-mode)
	    ("M-RET t" . treemacs)
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
(add-hook 'which-key-mode-hook
    (lambda ()
	(which-key-add-key-based-replacements "M-RET t" "treemacs-toggle")
	(which-key-add-key-based-replacements "M-RET f" "set-fill-column")
	(which-key-add-key-based-replacements "M-RET F" "set-org-fill-column")
	(which-key-add-key-based-replacements "M-RET" "Mitch's Menu")
	(diminish 'mmm-keys-minor-mode "m3k")))

;;; --------------------------------------------------------------------------

(setq-default initial-scratch-message
      (format ";; Hello, World and Happy hacking %s!\n%s\n\n"
          user-login-name
          ";; Press M-RET (Meta-RET) to open the Mitch's Menu"))

(if display-dashboard-at-start
    (add-hook 'lisp-interaction-mode-hook 'dashboard-open))

;; (add-hook 'lisp-interaction-mode-hook 'use-package-report)

;;; --------------------------------------------------------------------------

;;; init.el ends here.
