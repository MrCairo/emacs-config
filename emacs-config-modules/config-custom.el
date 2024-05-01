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

(provide 'config-custom)
;;; config-custom.el ends here.
