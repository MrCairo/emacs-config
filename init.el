;;; init.el --- My customized emacs init file -- lexical-binding: t --
;;;
;;; Commentary:

;; This file bootstraps the configuration which is created from tangling
;; Configuration.org into this init.el file.
;;
;; So, DO NOT MODIFY.  THIS FILE IS GENERATED
;; Edit the Configure.org file, save (auto-tangle) and this file will be
;; generated.  Plus, there are a lot of comments that are in the Configure.org
;; file that are not exported as part of this source.  The comments provide more
;; detail for certain modes as well as other important details.  If there is a
;; question as to why something is a certain way or how a package may work,
;; the Configure.org file may contain those answers.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)
;;

;;; --------------------------------------------------------------------------

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
       (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t
      use-package-verbose t)

(straight-use-package 'use-package)

(setq use-package-compute-statistics t
    use-package-verbose t
    use-package-always-ensure nil
    use-package-always-demand nil
    use-package-always-defer nil)

(use-package el-patch)

;; Load org early on in the init process
;; (use-package org :straight t)
;; Make sure that we set the read buffer above the default 4k
(setq read-process-output-max (* 1024 1024))

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
    (expand-file-name
  	(concat "emacs-working-files_" emacs-version) custom-docs-dir)
    "The directory where to store Emacs working files."
    :type 'string
    :group 'mrf-custom)

(defcustom custom-org-fill-column 150
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
    :type '(choice (const :tag "Use the Vertico completion system." comphand-vertico)
               (const :tag "Use Ivy, Counsel, Swiper completion systems" comphand-ivy-counsel))
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

(defcustom custom-ide 'custom-ide-eglot-lsp
    "Select which IDE will be used for Python development.

Elpy is an Emacs package to bring powerful Python editing to Emacs. It
combines and configures a number of other packages, both written in Emacs
Lisp as well as Python. Elpy is fully documented at
https://elpy.readthedocs.io/en/latest/index.html.

Elgot/LSP Eglot is the Emacs client for the Language Server Protocol
(LSP). Eglot provides infrastructure and a set of commands for enriching the
source code editing capabilities of Emacs via LSP. Eglot itself is
completely language-agnostic, but it can support any programming language
for which there is a language server and an Emacs major mode.

Anaconda-mode is another IDE for Python very much like Elpy. It is not as
configurable but has a host of great feaures that just work."
    :type '(choice (const :tag "Elpy: Emacs Lisp Python Environment" custom-ide-elpy)
               (const :tag "Eglot/Language Server Protocol" custom-ide-eglot-lsp)
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
;;; Set a variable that represents the actual emacs configuration directory.
;;; This is being done so that the user-emacs-directory which normally points
;;; to the .emacs.d directory can be re-assigned so that customized files don't
;;; pollute the configuration directory. This is where things like YASnippet
;;; snippets are saved and also additional color themese are stored.

(defvar emacs-config-directory user-emacs-directory)

;;; Different emacs configuration installs with have their own configuration
;;; directory.
(make-directory working-files-directory t)  ;; Continues to work even if dir exists

;;; Point the user-emacs-directory to the new working directory
(setq user-emacs-directory working-files-directory)
(message (concat ">>> Setting emacs-working-files directory to: " user-emacs-directory))

;;; Put any emacs cusomized variables in a special file
(setq custom-file (expand-file-name "customized-vars.el" working-files-directory))
(load custom-file 'noerror 'nomessage)

;;; --------------------------------------------------------------------------

(setq-default
    window-resize-pixelwise t ;; enable smooth resizing
    window-resize-pixelwise t
    frame-resize-pixelwise t
    dired-dwim-target t       ;; try to guess target directory
    truncate-partial-width-windows 1 ;; truncate lines in partial-width windows
    auto-save-default nil     ;; disable auto save
    backup-inhibited t        ;; disable backup (No ~ tilde files)
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

(use-package page-break-lines
    :config
    (global-page-break-lines-mode))

(use-package default-text-scale
    :hook (after-init . default-text-scale-mode))

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

(add-to-list 'load-path (expand-file-name "lisp" emacs-config-directory))

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

(use-package diminish
    :straight (diminish :type git :flavor melpa :host github :repo "myrjola/diminish.el"))

(defun mrf/set-diminish ()
   (diminish 'projectile-mode "PrM")
   (diminish 'anaconda-mode)
   (diminish 'tree-sitter-mode "ts")
   (diminish 'ts-fold-mode)
   (diminish 'counsel-mode)
   (diminish 'golden-ratio-mode)
   (diminish 'company-box-mode)
   (diminish 'company-mode))

;; Need to run late in the startup process
(add-hook 'after-init-hook 'mrf/set-diminish)

;; (use-package pabbrev)

;;; --------------------------------------------------------------------------

(use-package spacious-padding
    :hook (after-init . spacious-padding-mode)
    :custom
    (spacious-padding-widths
      '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
  	 :fringe-width 10
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8)))

;; Read the doc string of `spacious-padding-subtle-mode-line' as it
;; is very flexible and provides several examples.
;; (setq spacious-padding-subtle-mode-line
;;       `( :mode-line-active 'default
;;          :mode-line-inactive vertical-border))

;;; --------------------------------------------------------------------------

(column-number-mode)

(use-package page-break-lines
    :config
    (global-page-break-lines-mode))

(use-package rainbow-delimiters
    :config
    (rainbow-delimiters-mode))

(use-package dash
    :disabled
    :straight (dash
  		:type git
  		:flavor melpa
  		:files ("dash.el" "dash.texi" "dash-pkg.el")
  		:host github
  		:repo "magnars/dash.el"))

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

;; Macintosh specific configurations.

(defconst *is-a-mac* (eq system-type 'darwin))
(when (eq system-type 'darwin)
   (setq mac-option-key-is-meta nil
         mac-command-key-is-meta t
         mac-command-modifier 'meta
         mac-option-modifier 'super))

;;; --------------------------------------------------------------------------

;; Prompt indicator/Minibuffer

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

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

(use-package hydra)

;;; --------------------------------------------------------------------------
;; Which Key Helper

(use-package which-key
   :diminish which-key-mode
   :custom (which-key-idle-delay 1)
   :config
   (which-key-mode)
   (which-key-setup-side-window-right))

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
    :defer t
    :ensure t
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
    :defer t
    :bind (:map yas-minor-mode-map
              ("<C-'>" . yas-expand))
    :config
    (message ">>> YASnippet Configured")
    (setq yas-global-mode t)
    (setq yas-minor-mode t)
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (add-to-list #'yas-snippet-dirs (expand-file-name "Snippets" custom-docs-dir))
    (yas-reload-all)
    (setq yas-prompt-functions '(yas-ido-prompt))
    (defun help/yas-after-exit-snippet-hook-fn ()
      (prettify-symbols-mode))
    (add-hook 'yas-after-exit-snippet-hook #'help/yas-after-exit-snippet-hook-fn))

;;; --------------------------------------------------------------------------

(use-package yasnippet-snippets
  :after yasnippet
  :config
  (message ">>> YASnippet-Snippets Configured"))

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

;;; --------------------------------------------------------------------------
;;; Window Number

(use-package winum
   :straight (winum :type git :flavor melpa :host github :repo "deb0ch/emacs-winum"))
(winum-mode)

;;; --------------------------------------------------------------------------
;;; Treemacs

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
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
          treemacs-workspace-switch-cleanup        nil
       )

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
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;;; --------------------------------------------------------------------------

(use-package treemacs-magit
  :after (treemacs magit)
   )

;;; --------------------------------------------------------------------------

(use-package treemacs-icons-dired
   :hook (dired-mode . treemacs-icons-dired-enable-once)
   )

;;; --------------------------------------------------------------------------

;; (use-package treemacs-perspective
;;    :disabled
;;    :straight (treemacs-perspective :type git :flavor melpa
;;            :files ("src/extra/treemacs-perspective.el" "treemacs-perspective-pkg.el")
;;            :host github :repo "Alexander-Miller/treemacs")
;;    :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;    :config (treemacs-set-scope-type 'Perspectives))


(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
   :straight (treemacs-persp :type git :flavor melpa
              :files ("src/extra/treemacs-persp.el" "treemacs-persp-pkg.el")
              :host github :repo "Alexander-Miller/treemacs")
   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
   :config (treemacs-set-scope-type 'Perspectives))

;;; --------------------------------------------------------------------------

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

;;; --------------------------------------------------------------------------

(use-package treemacs-all-the-icons
    :defer t
 :if (display-graphic-p))

;;; --------------------------------------------------------------------------
;;; Emacs Polyglot is the Emacs LSP client that stays out of your way:

(defvar mrf/clangd-path (executable-find "clangd")
    "Clangd executable path.")

(defun mrf/projectile-proj-find-function (dir)
    "Find the project `DIR' function for Projectile.
Thanks @wyuenho on GitHub"
    (let ((root (projectile-project-root dir)))
        (and root (cons 'transient root))))

(when (equal custom-ide 'custom-ide-eglot-lsp)
    (use-package eglot
        :defer t
        :after company
        :init
        (setq company-backends
            (cons 'company-capf
                (remove 'company-capf company-backends)))
        :hook
        (lisp-mode . eglot-ensure)
        (c-mode . eglot-ensure)
        (c++-mode . eglot-ensure)
        (python-mode . eglot-ensure)
        (prog-mode . eglot-ensure)
        (rust-mode-hook . eglot-ensure)
        :config
        (which-key-add-key-based-replacements "C-c g r" "find-symbol-reference")
        (which-key-add-key-based-replacements "C-c g o" "find-defitions-other-window")
        (which-key-add-key-based-replacements "C-c g g" "find-defitions")
        (which-key-add-key-based-replacements "C-c g ?" "eldoc-definition")
        ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)
        (add-to-list 'eglot-server-programs '((c-mode c++-mode) "clangd"))
        (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
        (add-to-list 'eglot-server-programs
            '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
        (setq-default eglot-workspace-configuration
            '((:pylsp . (:configurationSources ["flake8"]
                            :plugins (:pycodestyle (:enabled :json-false)
                                         :mccabe (:enabled :json-false)
                                         :pyflakes (:enabled :json-false)
                                         :flake8 (:enabled :json-false
                                                     :maxLineLength 88)
                                         :pydocstyle (:enabled t
                                                         :convention "numpy")
                                         :yapf (:enabled :json-false)
                                         :autopep8 (:enabled :json-false)
                                         :black (:enabled t
                                                    :line_length 88
                                                    :cache_config t))))))
        ))

;;; --------------------------------------------------------------------------
;;; Language Server Protocol

(when (equal custom-ide 'custom-ide-eglot-lsp)
    (eval-when-compile (defvar lsp-enable-which-key-integration)))

(defun mrf/lsp-mode-setup ()
    "Custom LSP setup function."
    (when (equal custom-ide 'custom-ide-eglot-lsp)
        (message "Set up LSP header-line and other vars")
        (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
        (setq lsp-clangd-binary-path "/usr/bin/clangd")'
        (lsp-headerline-breadcrumb-mode)))

(when (equal custom-ide 'custom-ide-eglot-lsp)
    (use-package lsp-mode
        :defer t
        :commands (lsp lsp-deferred)
        :hook (lsp-mode . mrf/lsp-mode-setup)
        :init
        (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
        :config
        (lsp-enable-which-key-integration t)))

(when (equal custom-ide 'custom-ide-eglot-lsp)
    (use-package lsp-ui
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
        :hook (lsp-mode . lsp-ui-mode)))

(when (equal custom-ide 'custom-ide-eglot-lsp)
    (use-package lsp-treemacs
        :after lsp
        :bind (:map prog-mode-map
                  ("C-c t" . treemacs))
        :config
        (lsp-treemacs-sync-mode 1)))

(when (and (equal custom-ide 'custom-ide-eglot-lsp)
         (equal completion-handler 'comphand-ivy-counsel))
    (use-package lsp-ivy
        :after lsp ivy))

;;; --------------------------------------------------------------------------

(when (equal custom-ide 'custom-ide-lsp-bridge)
    (use-package markdown-mode)
    (use-package lsp-bridge
      :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
  		     :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
  		     :build (:not compile))
      :custom
      (lsp-bridge-python-lsp-server "pylsp")
      :config
      (global-lsp-bridge-mode)))

;;; --------------------------------------------------------------------------

(when (equal custom-ide 'custom-ide-anaconda)
   (use-package anaconda-mode
       :bind (:map python-mode-map
    	       ("C-c g o" . anaconda-mode-find-definitions-other-frame)
    	       ("C-c g g" . anaconda-mode-find-definitions)
  	       ("C-c C-x" . next-error))        
       :config
       (which-key-add-key-based-replacements "C-c g o" "find-defitions-other-window")
       (which-key-add-key-based-replacements "C-c g g" "find-defitions")
       (require 'pyvenv)
       :hook
       (python-mode-hook . anaconda-eldoc-mode)))

;;; --------------------------------------------------------------------------

(when (equal custom-ide 'custom-ide-elpy)
    (use-package elpy
        :after python
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
      (which-key-add-key-based-replacements "C-c g a" "goto-assignment")
      (which-key-add-key-based-replacements "C-c g o" "find-defitions-other-window")
      (which-key-add-key-based-replacements "C-c g g" "find-defitions")
      (which-key-add-key-based-replacements "C-c g ?" "eldoc-definition")
      (elpy-enable))
    ;; Enable Flycheck
    (use-package flycheck
      :after elpy
      :straight (flycheck :type git :flavor melpa
                      :host github
                      :repo "flycheck/flycheck")
      :config
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      :hook (elpy-mode . flycheck-mode)))

;;; ------------------------------------------------------------------------
  ;;; Alternate fork to handle possible performance bug(s)
(use-package jsonrpc
    :straight (jsonrpc :type git :host github :repo "emacs-straight/jsonrpc" :files ("*" (:exclude ".git"))))

(when (equal debug-adapter 'enable-dape)
    (use-package dape
        :after jsonrpc
        :defer t
        ;; :defer t
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

        :config
        ;; Projectile users
        ;; (setq dape-cwd-fn 'projectile-project-root)
        ;; :straight (dape :type git
        ;;           :host github :repo "emacs-straight/dape"
        ;;           :files ("*" (:exclude ".git")))
        (message "DAPE Configured")))

;;; --------------------------------------------------------------------------
;;; Debug Adapter Protocol      
(when (equal debug-adapter 'enable-dap-mode)
    (use-package dap-mode
        ;; Uncomment the config below if you want all UI panes to be hidden by default!
        ;; :custom
        ;; (lsp-enable-dap-auto-configure nil)
        :commands dap-debug
        :custom
        (dap-auto-configure-features '(sessions locals breakpoints expressions repl controls tooltip))
        :config
        (dap-ui-mode 1)
        (message "DAP mode loaded and configured.")))

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

(defun mrf/dape-end-debug-session ()
   "End the debug session."
   (interactive)
   (dape-quit))

(defun mrf/dape-delete-all-debug-sessions ()
   "End the debug session and delete all breakpoints."
   (interactive)
   (dape-breakpoint-remove-all)
   (mrf/dape-end-debug-session))

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
         ("Q" mrf/dape-delete-all-debug-sessions :color red))

;;; --------------------------------------------------------------------------
;;; DAP for Python

(when (equal debug-adapter 'enable-dap-mode)
    (use-package dap-python
      :straight (dap-python :type git :host github :repo "emacs-lsp/dap-mode")
      :after (dap-mode)
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
  	      :name "Python :: Run file (buffer)"))))

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
    ("Q" mrf/delete-all-debug-sessions :color red))

;;; --------------------------------------------------------------------------
;;; Swiper and IVY mode

(when (equal completion-handler 'comphand-ivy-counsel)
    (use-package ivy
      :diminish I
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
      :custom (ivy-use-virtual-buffers t)
      :config
      (ivy-mode 1))

    (use-package ivy-rich
      :after ivy
      :init
      (ivy-rich-mode 1)
      :config
      (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

    (use-package ivy-yasnippet
      :straight (ivy-yasnippet :type git
  		    :flavor melpa :host github
  		    :repo "mkcms/ivy-yasnippet")))

;;; --------------------------------------------------------------------------

(when (equal completion-handler 'comphand-ivy-counsel)
    (use-package swiper))

;;; --------------------------------------------------------------------------

(when (equal completion-handler 'comphand-ivy-counsel)
    (use-package counsel
      :straight t
      :bind (("C-M-j" . 'counsel-switch-buffer)
  		:map minibuffer-local-map
  		("C-r" . 'counsel-minibuffer-history))
      :custom
      (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
      :config
      (counsel-mode 1)))

;;; --------------------------------------------------------------------------

(when (equal completion-handler 'comphand-ivy-counsel)
    (use-package ivy-prescient
      :after counsel
      :custom
      (ivy-prescient-enable-filtering nil)
      :config
      (prescient-persist-mode 1)
      (ivy-prescient-mode 1)))

;;; --------------------------------------------------------------------------

;;;; Code Completion
(when enable-corfu
    (use-package corfu
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
      (add-hook 'eshell-mode-hook
  	  (lambda () (setq-local corfu-quit-at-boundary t
                      corfu-quit-no-match t
                      corfu-auto nil)
  	      (corfu-mode))))
    (use-package corfu-prescient
      :after corfu)
    )

;;; --------------------------------------------------------------------------

(defun mrf/tree-sitter-setup ()
   (tree-sitter-hl-mode t)
   (ts-fold-mode t))

(use-package tree-sitter
   :init
   (message ">>> Loading tree-sitter")
   ;; :after (lsp-mode)
   :config
   ;; Activate tree-sitter globally (minor mode registered on every buffer)
   (global-tree-sitter-mode)
   :hook
   (tree-sitter-after-on . mrf/tree-sitter-setup)
   (typescript-mode . lsp-deferred)
   (c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (js2-mode . lsp-deferred))

(use-package tree-sitter-langs)

(use-package ts-fold
   :straight (ts-fold :type git
              :host github
              :repo "emacs-tree-sitter/ts-fold")
    :bind (("C-<tab>" . ts-fold-toggle)
  	 ("C-c f"   . ts-fold-open-all)))

;;; --------------------------------------------------------------------------

(when (equal debug-adapter 'enable-dap-mode)
    (use-package typescript-ts-mode
      ;; :after (dap-mode)
      :mode "\\.ts\\'"
      :hook
      (typescript-ts-mode . lsp-deferred)
      (js2-mode . lsp-deferred)
      (rust-mode . lsp-deferred)
      :bind (:map typescript-mode-map
  		("C-c ." . dap-hydra/body))
      :config
      (setq typescript-indent-level 4)
      (dap-node-setup)))

(when (equal debug-adapter 'enable-dape)
    (use-package typescript-ts-mode
      :after (dape-mode)
      :mode ("\\.ts\\'")
      :hook
      (typescript-ts-mode . lsp-deferred)
      (js2-mode . lsp-deferred)
      (rust-mode . lsp-deferred)
      :bind (:map typescript-mode-map
  		("C-c ." . dape-hydra/body))
      :config
      (setq typescript-indent-level 4)))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

;;; --------------------------------------------------------------------------

(use-package js2-mode
   :hook (js-mode . js2-minor-mode)
    :bind (:map js2-mode-map
              ("{" . paredit-open-curly)
  	    ("}" . paredit-close-curly-and-newline))
   :mode ("\\.js\\'" "\\.mjs\\'")
   :custom (js2-highlight-level 3))

(use-package ac-js2
   :hook (js2-mode . ac-js2-mode))

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

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

;; (use-package graphql-mode)
(use-package js2-mode :defer t)
(use-package rust-mode :defer t)
(use-package swift-mode :defer t)

;;; --------------------------------------------------------------------------

(use-package flycheck
    :defer t
    :config
    (global-flycheck-mode))

(use-package flycheck-package
    :after flycheck)

(eval-after-load 'flycheck
  '(flycheck-package-setup))

(defun mrf/before-save ()
  "Force the check of the current python file being saved."
  (when (eq major-mode 'python-mode) ;; Python Only
     (flycheck-mode 0)
     (flycheck-mode t)
     (message "deleting trailing whitespace enabled")
     (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'mrf/before-save)

(defun mrf/set-custom-ide-python-keymaps ()
    (message "<<< Set python-mode keymaps based upon IDE.")
    (cond
        ((equal custom-ide 'custom-ide-eglot-lsp)
            (unless (featurep 'lsp)
                (lsp-deferred))
            (unless (featurep 'eglot)
                (eglot))
            (bind-keys :map python-mode-map
                ("C-c g r" . lsp-find-references)
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
    ;; (unless (featurep 'jedi)
    ;;  (use-package jedi
    ;;      :config
    ;;      (jedi:setup)))
    (setq highlight-indentation-mode -1)
    (setq display-fill-column-indicator-mode t))

(defun mrf/python-mode-triggered ()
    (message ">>> mrf/python-mode-triggered")
    ;; (eldoc-box-hover-at-point-mode t) ;; Using Mitch Key for this
    (if (equal debug-adapter 'enable-dap-mode)
        (unless (featurep 'dap-mode)
            (dap-mode))
        (if (not (featurep 'dape))
            (use-package dape :demand t)))
    (mrf/set-custom-ide-python-keymaps)
    (set-fill-column 80))

(use-package python-mode
    :defer t
    :hook (python-mode . mrf/python-mode-triggered) )

(add-to-list 'auto-mode-alist '("\\.py\\'" . mrf/load-python-file-hook))

(use-package blacken
    :after python) ;Format Python file upon save.

(if (boundp 'python-shell-completion-native-disabled-interpreters)
    (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
    (setq python-shell-completion-native-disabled-interpreters '("python3")))

;;; --------------------------------------------------------------------------

(use-package py-autopep8
   :after (python-mode python)
   :hook ((python-mode) . py-autopep8-mode))

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

(when (equal debug-adapter 'enable-dap-mode)
    ;; (dolist (m (list python-mode-map typescript-ts-mode-map c-mode-map c++-mode-map))
    (use-package dap
      :defer t
      :bind (:map prog-mode-map
  		("C-c ." . dap-hydra/body))))

(when (equal debug-adapter 'enable-dape)
    ;; (dolist (m (list python-mode-map typescript-ts-mode-map c-mode-map c++-mode-map))
    (use-package dape
      :defer t
      :bind (:map prog-mode-map
  		("C-c ." . dape-hydra/body))))

;;; --------------------------------------------------------------------------

(use-package pyvenv-auto
   :after python
   :config (message ">>> Starting pyvenv-auto")
   :hook (python-mode . pyvenv-auto-run))

(use-package pydoc
    :straight (pydoc :type git :flavor melpa
  		  :host github :repo "statmobile/pydoc")
    :after python
    :custom
    (pydoc-python-command "python3")
    (pydoc-pip-version-command "pip3 --version")
    )

;;; --------------------------------------------------------------------------

(when (package-installed-p 'realgud)
    (use-package cc-mode
      :bind (:map c-mode-map
  		("C-c , j" . realgud:cmd-jump)
  		("C-c , k" . realgud:cmd-kill)
  		("C-c , s" . realgud:cmd-step)
  		("C-c , n" . realgud:cmd-next)
  		("C-c , q" . realgud:cmd-quit)
  		("C-c , F" . realgud:window-bt)
  		("C-c , U" . realgud:cmd-until)
  		("C-c , X" . realgud:cmd-clear)
  		("C-c , !" . realgud:cmd-shell)
  		("C-c , b" . realgud:cmd-break)
  		("C-c , f" . realgud:cmd-finish)
  		("C-c , D" . realgud:cmd-delete)
  		("C-c , +" . realgud:cmd-enable)
  		("C-c , R" . realgud:cmd-restart)
  		("C-c , -" . realgud:cmd-disable)
  		("C-c , B" . realgud:window-brkpt)
  		("C-c , c" . realgud:cmd-continue)
  		("C-c , e" . realgud:cmd-eval-dwim)
  		("C-c , Q" . realgud:cmd-terminate)
  		("C-c , T" . realgud:cmd-backtrace)
  		("C-c , h" . realgud:cmd-until-here)
  		("C-c , u" . realgud:cmd-older-frame)
  		("C-c , 4" . realgud:cmd-goto-loc-hist-4)
  		("C-c , 5" . realgud:cmd-goto-loc-hist-5)
  		("C-c , 6" . realgud:cmd-goto-loc-hist-6)
  		("C-c , 7" . realgud:cmd-goto-loc-hist-7)
  		("C-c , 8" . realgud:cmd-goto-loc-hist-8)
  		("C-c , 9" . realgud:cmd-goto-loc-hist-9)
  		("C-c , d" . realgud:cmd-newer-frame)
  		("C-c , RET" . realgud:cmd-repeat-last)
  		("C-c , E" . realgud:cmd-eval-at-point)
  		("C-c , I" . realgud:cmdbuf-info-describe)
  		("C-c , C-i" . realgud:cmd-info-breakpoints))))

;;; --------------------------------------------------------------------------

(when enable-gb-dev
    (use-package z80-mode
      :straight (z80-mode
                      :type git
                      :host github
                      :repo "SuperDisk/z80-mode"))

    (use-package mwim
      :straight (mwim
                      :type git
                      :flavor melpa
                      :host github
                      :repo "alezost/mwim.el"))

    (use-package rgbds-mode
      :after mwim
      :straight (rgbds-mode
                      :type git :host github
                      :repo "japanoise/rgbds-mode")))

;;; --------------------------------------------------------------------------

(when (equal custom-ide 'custom-ide-eglot-lsp)
    (use-package company
      :after lsp-mode
      :hook (lsp-mode . company-mode)
      :bind (:map company-active-map
  		("<tab>" . company-complete-selection))
  	    (:map lsp-mode-map
  		("<tab>" . company-indent-or-complete-common))))

(when (equal custom-ide 'custom-ide-elpy)
    (use-package company
      :after elpy
      :hook (elpy-mode . company-mode)
      :bind (:map company-active-map
  		("<tab>" . company-complete-selection))
              (:map elpy-mode-map
  		("<tab>" . company-indent-or-complete-common))))

(when (equal custom-ide 'custom-ide-anaconda)
    (use-package company
      :after anaconda-mode
      :hook (anaconda-mode . company-mode)
      :bind (:map company-active-map
  		("<tab>" . company-complete-selection))
  	    (:map elpy-mode-map
  		("<tab>" . company-indent-or-complete-common))))

;; Don't use company at all if lsp-bridge is active.
;; lsp-bridge already provides similar functionality.
(unless (equal custom-ide 'custom-ide-lsp-bridge)
    (use-package company
      :custom
      (company-minimum-prefix-length 1)
      (company-idle-delay 0.0)
      :hook (after-init . global-company-mode)))
        ;; :config
        ;; (add-to-list 'company-backends 'company-yasnippet))

;;; --------------------------------------------------------------------------

(when (featurep 'company)
    (use-package company-box
      :diminish cb
      :hook (company-mode . company-box-mode))

    (when (equal custom-ide 'custom-ide-elpy)
      (use-package company-jedi
  	  :after python
  	  :config
  	  (jedi:setup)
  	  (defun my/company-jedi-python-mode-hook ()
  	      (add-to-list 'company-backends 'company-jedi))
  	  (add-hook 'python-mode-hook 'my/company-jedi-python-mode-hook)))

    (when (equal custom-ide 'custom-ide-anaconda)
      (use-package company-anaconda
  	  :after anaconda
  	  :hook (python-mode . anaconda-mode))
      (eval-after-load "company"
  	  '(add-to-list 'company-backends 'company-anaconda))))

;;; --------------------------------------------------------------------------

(use-package magit
   :defer t
   :commands (magit-status magit-get-current-branch)
;; :custom
;;  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
   )

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started

(use-package forge
  :after magit)

;;; --------------------------------------------------------------------------

(defun mrf/org-theme-override-values ()
    (defface org-block-begin-line
      '((t (:underline "#1D2C39" :foreground "SlateGray" :background "#1D2C39")))
      "Face used for the line delimiting the begin of source blocks.")

    (defface org-block
      '((t (:background "#242635" :extend t)))
      "Face used for the source block background.")

    (defface org-block-end-line
      '((t (:overline "#1D2C39" :foreground "SlateGray" :background "#1D2C39")))
      "Face used for the line delimiting the end of source blocks.")

    (defface org-modern-horizontal-rule
      '((t (:strike-through "green" :weight bold)))
      "Face used for the Horizontal like (-----)"))

;;; --------------------------------------------------------------------------

(defun mrf/org-font-setup ()
  "Setup org mode fonts."
    (require 'org-faces)

    (font-lock-add-keywords
      'org-mode
      '(("^ *\\([-]\\) "
              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
    ;; (setq org-src-fontify-natively t)

          ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 1.5)
                       (org-level-2 . 1.25)
                       (org-level-3 . 1.1)
                       (org-level-4 . 1.1)
                       (org-level-5 . 1.1)
                       (org-level-6 . 1.1)
                       (org-level-7 . 1.1)
                       (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font "SF Pro" :weight 'regular :height (cdr face)))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil    :foreground 'unspecified :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

;; -----------------------------------------------------------------

(defun mrf/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1)
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

(mrf/org-theme-override-values)

(use-package org
    :defer t
    :commands (org-capture org-agenda)
    :hook (org-mode . mrf/org-mode-setup)
    :bind (:map org-mode-map
  	    ("C-c e" . org-edit-src-code))
    :config
    (message ">>> Loading orgmode")
    (setq org-hide-emphasis-markers t)
    ;; Save Org buffers after refiling!
    (advice-add 'org-refile :after 'org-save-all-org-buffers)
    (setq org-tag-alist
      '((:startgroup)
  				      ; Put mutually exclusive tags here
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
    ;; If not already, enable yasnippet
    (yas-global-mode t)
    (define-key global-map (kbd "C-c j")
      (lambda () (interactive) (org-capture nil "jj")))
    (mrf/org-font-setup))

;;; --------------------------------------------------------------------------

(use-package org-modern
    :after org
    :hook (org-mode . org-modern-mode))

;; Add frame borders and window dividers
(modify-all-frames-parameters
    '((right-divider-width . 40)
       (internal-border-width . 40)))
(dolist (face '(window-divider
                   window-divider-first-pixel
                   window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(setq
    ;; Edit settings
    org-auto-align-tags nil
    org-tags-column 0
    org-catch-invisible-edits 'show-and-error
    org-special-ctrl-a/e t
    org-insert-heading-respect-content t

    ;; Org styling, hide markup etc.
    org-hide-emphasis-markers t
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

(global-org-modern-mode)

;;; --------------------------------------------------------------------------

(use-package org-bullets
   :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;;; --------------------------------------------------------------------------

(defun mrf/org-mode-visual-fill ()
  (setq visual-fill-column-width custom-org-fill-column
        visual-fill-column-center-text nil)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . mrf/org-mode-visual-fill))

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
;; (use-package emacsql)
;; (use-package emacsql-sqlite)

(use-package org-roam
   ;; :demand t  ;; Ensure org-roam is loaded by default
   :init
   (setq org-roam-v2-ack t)
   :custom
   (org-roam-directory (expand-file-name "RoamNotes" custom-docs-dir))
   (org-roam-completion-everywhere t)
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

;;; --------------------------------------------------------------------------
;; Automatically tangle our Configure.org config file when we save it
;; Org files that should use this need to add a '#+auto_tangle: t'
;; in the org file.
(use-package org-auto-tangle
   :defer t
   :hook (org-mode . org-auto-tangle-mode))

;;; --------------------------------------------------------------------------

(with-eval-after-load 'org
    (require 'ox-gfm nil t))

;;; --------------------------------------------------------------------------

(when (equal completion-handler 'comphand-vertico)
    (use-package vertico
      :demand t   ; Otherwise won't get loaded immediately
      :straight (vertico :files (:defaults "extensions/*") ; Special recipe to load extensions conveniently
  		    :includes (vertico-indexed
  				  vertico-flat
  				  vertico-grid
  				  vertico-mouse
  				  vertico-quick
  				  vertico-buffer
  				  vertico-repeat
  				  vertico-reverse
  				  vertico-directory
  				  vertico-multiform
  				  vertico-unobtrusive
  				  ))
      :config
      (vertico-mode)
      (recentf-mode t)
      (vertico-multiform-mode)
      (vertico-count 13)
      (vertico-cycle nil)
  				      ; Clean up file path when typing
      :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)
  				      ; Make sure vertico state is saved
  		(minibuffer-setup . vertico-repeat-save)))

    (use-package vertico-prescient
      :after vertico)

    (use-package vertico-posframe
      :straight t
      :custom
      (vertico-posframe-parameters
  	  '((left-fringe . 8)
  	       (right-fringe . 8)))))

;;; --------------------------------------------------------------------------

(when (equal completion-handler 'comphand-vertico)
    (use-package marginalia
        :custom
        (marginalia-max-relative-age 0)
        (marginalia-align 'right)
        :config
        (marginalia-mode t)))

;;; --------------------------------------------------------------------------

(when (equal completion-handler 'comphand-vertico)
    (use-package orderless
      :custom
      (completion-styles '(orderless basic))
      (completion-category-overrides '((file (styles basic partial-completion))))))

;;; --------------------------------------------------------------------------
;; Example configuration for Consult

(when (equal completion-handler 'comphand-vertico)
    (use-package consult
      :straight t
      ;; Replace bindings. Lazily loaded due by `use-package'.
      :bind (;; C-c bindings in `mode-specific-map'
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
  		("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
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
      (setq consult-narrow-key "<") ;; "C-+"

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
      ))

;;; --------------------------------------------------------------------------

(use-package solaire-mode
   :hook (after-init . solaire-global-mode)
   :config
   (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
   (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist))

;;; --------------------------------------------------------------------------
;; Golen Ratio

(if enable-golden-ratio
    (use-package golden-ratio
	:custom
	(golden-ratio-auto-scale t)
	(golden-ratio-adjust-factor .4)
	(golden-ratio-wide-adjust-factor .4)
	(golden-ratio-max-width 100)
	(golden-ratio-exclude-modes '(treemacs-mode
				      undo-tree-visdualizer-mode
				      inferior-python-mode
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
	(golden-ratio-mode 1)))

;;; --------------------------------------------------------------------------

(use-package ace-window
    :bind ("M-o" . ace-window))

;;; --------------------------------------------------------------------------

(use-package all-the-icons
    :if (display-graphic-p))

(defun mrf/setup-dashboard-buffer ()
    "Set up the dashboard buffer and optionally make it the first."
    (setq dashboard-items '((recents . 15)
                               (bookmarks . 10)
                               (projects . 10))
      dashboard-icon-type 'all-the-icons
      dashboard-display-icons-p t
      dashboard-center-content t
      dashboard-set-heading-icons t
      dashboard-set-file-icons t)
    ;; dashboard-projects-backend 'projectile)

    (global-set-key (kbd "C-c d") 'dashboard-open)

    (if (equal display-dashboard-at-start t)
      (progn
            (setq initial-buffer-choice
  	      (lambda ()
                    (get-buffer-create "*dashboard*")))
            (dashboard-open))
      (get-buffer-create "*dashboard*")))

(defun mrf/dashboard-banner ()
    "Setup defaults for the dashboard banner buffer."
    (setq dashboard-footer-messages '("Greetings Program!"))
    (setq dashboard-banner-logo-title "Welcome to Emacs!")
    (setq dashboard-startup-banner 'logo))

(use-package dashboard
    :after (dired)
    :init
    (mrf/dashboard-banner)
    :hook ((after-init     . mrf/setup-dashboard-buffer)
              (dashboard-mode . mrf/dashboard-banner)))

;;; --------------------------------------------------------------------------
;; A cleaner and simpler undo package.

(if enable-vundo
    (use-package vundo
      :bind (("C-x u" . vundo)
  		("C-x r u" . vundo))
      :config
      (setq vundo-glyph-alist vundo-unicode-symbols)
      (set-face-attribute 'vundo-default nil :family "Wingdings2")))

;;; --------------------------------------------------------------------------
;; Full-featured undo-tree handling. Look to Vundo for something a little
;; simpler.

(defun mrf/undo-tree-hook ()
    (set-frame-width (selected-frame) 20))

(if (not enable-vundo)
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
        (global-undo-tree-mode))
    ;; This prevents the *.~undo-tree~ files from being persisted.
    (with-eval-after-load 'undo-tree
        (setq undo-tree-auto-save-history nil)))

;;; --------------------------------------------------------------------------
;; helpful package

(if (equal completion-handler 'comphand-ivy-counsel)
    (use-package helpful
      :commands (helpful-callable helpful-variable helpful-command helpful-key)
      :custom
      (counsel-describe-function-function #'helpful-callable)
      (counsel-describe-variable-function #'helpful-variable)
      :bind
      ([remap describe-function] . counsel-describe-function)
      ([remap describe-command] . helpful-command)
      ([remap describe-variable] . counsel-describe-variable)
      ([remap describe-key] . helpful-key))
  (use-package helpful
      :commands (helpful-callable helpful-variable helpful-command helpful-key)
      :bind
      ([remap describe-command] . helpful-command)
      ([remap describe-key] . helpful-key)))

;;; --------------------------------------------------------------------------

(use-package term
  :defer t
  :commands term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;;; --------------------------------------------------------------------------

(use-package eterm-256color
  :defer t
  :hook (term-mode . eterm-256color-mode))

;;; --------------------------------------------------------------------------

(use-package vterm
  :defer t
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
    ;; 	("C-r" . eshell-hist-mode)
    ;; 	("<home>" . eshell-bol))

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
    :defer t
    :hook (eshell-first-time-mode . efs/configure-eshell)
    :config
    (with-eval-after-load 'esh-opt
      (setq eshell-destroy-buffer-when-process-dies t)
      (setq eshell-visual-commands '("htop" "zsh" "vim")))

    (eshell-git-prompt-use-theme 'powerline))

;;; --------------------------------------------------------------------------

(when enable-neotree
    (use-package neotree
      :config
      (global-set-key [f8] 'neotree-toggle)
      (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))

;;; --------------------------------------------------------------------------

(use-package all-the-icons)
(use-package nerd-icons)

;; (use-package doom-modeline
;;   :diabled
;;   :init (doom-modeline-mode 1)
;;   :custom ((doom-modeline-height 15)))

;;; --------------------------------------------------------------------------
;; Enable tabs for each buffer

(when enable-centaur-tabs
    (use-package centaur-tabs
      :custom
      ;; Set the style to rounded with icons (setq centaur-tabs-style "bar")
      (centaur-tabs-style "bar")
      (centaur-tabs-set-icons t)
      (centaur-tabs-set-modified-marker t)
      :bind (("C-c <" . centaur-tabs-backward)
  		("C-c >" . centaur-tabs-forward))
      :config ;; Enable centaur-tabs
      (centaur-tabs-mode t)))

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
    :defer t
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
    (popper-mode +1)
    (popper-echo-mode +1))

;;; --------------------------------------------------------------------------

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package all-the-icons-dired
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

;;; --------------------------------------------------------------------------

(setq-default initial-scratch-message
    (format ";; Hello, World and Happy hacking %s!\n%s\n\n"
      user-login-name
      ";; Press M-RET (Meta-RET) to open the Mitch's Menu"))

  ;; (concat ";; Hello, World and Happy hacking "
  ;;     user-login-name "!\n;; Press M-RET (C-c C-m) to open the Mitch Menu\n\n"))

;;; --------------------------------------------------------------------------
;; Ignore Line Numbers for the following modes:

;; Line #'s appear everywhere
;; ... except for when in these modes
(dolist (mode '(dashboard-mode-hook
                helpful-mode-hook
                eshell-mode-hook
                eww-mode-hook
                help-mode-hook
                org-mode-hook
                shell-mode-hook
                term-mode-hook
                treemacs-mode-hook
                vterm-mode-hook))
   (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq warning-suppress-types '((package reinitialization)
                                 (package-initialize)
                                 (package)
                                 (use-package)
                                 (python-mode)))

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

;;; init.el ends here.
