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

(provide 'init-customizable)
;;; init-customizable.el ends here.
