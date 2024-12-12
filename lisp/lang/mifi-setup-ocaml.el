;;; mifi-setup-ocaml.el --- Configure OCaml dev environment -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; This file is used to configure the OCaml environment for Emacs which
;;; also includes checking for needed executables.

;;; Code:

;;; ##########################################################################

(defvar enable-ocaml)

;;; ##########################################################################

(defun mifi/dap-end-ocaml-debug-session ()
  "End the debug session and delete project buffers."
  (interactive)
  (dap-disconnect (dap--cur-session)))

(defun mifi/dap-delete-all-ocaml-debug-sessions ()
  "End the debug session and delete project buffers and all breakpoints."
  (interactive)
  (dap-breakpoint-delete-all)
  (mifi/dap-end-ocaml-debug-session))

(defun mifi/dap-begin-ocaml-debug-session ()
  "Begin a debug session with several dap windows enabled."
  (interactive)
  (dap-ui-show-many-windows)
  (dap-debug))

;;; ##########################################################################

(defun mifi/define-ocaml-dap-hydra ()
  (defhydra dap-ocaml-hydra (:color pink :hint nil :foreign-keys run)
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
    ("dx" mifi/dap-end-ocaml-debug-session)
    ("dX" mifi/dap-delete-all-ocaml-debug-sessions)
    ("x" nil "exit Hydra" :color yellow)
    ("q" mifi/dap-end-ocaml-debug-session "quit" :color blue)
    ("Q" mifi/dap-delete-all-ocaml-debug-sessions :color red)))

(when enable-ocaml
  (mifi/define-ocaml-dap-hydra))

(let
  ((installer "bash -c \"sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh) --version 2.2.0\""))
  (use-package opam
    :ensure t
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

(defun mrf/set-opam-load-path ()
  (let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
    (when (and (and opam-share (file-directory-p opam-share)) enable-ocaml)
      (message "Updating load-path for OPAM to %s" (expand-file-name "emacs/site-lisp" opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (autoload 'merlin-mode "merlin" nil t nil)
      ;; Automatically start it in OCaml buffers
      ;; (add-hook 'tuareg-mode-hook 'merlin-mode t)
      (add-hook 'caml-mode-hook 'merlin-mode t)
      ;; Use opam switch to lookup ocamlmerlin binary
      (setq merlin-command 'opam)
      ;; To easily change opam switches within a given Emacs session, you can
      ;; install the minor mode https://github.com/ProofGeneral/opam-switch-mode
      ;; and use one of its "OPSW" menus.
      )))

;;; ##########################################################################

(use-package opam-emacs-setup
  :init
  (mrf/set-opam-load-path)
  :ensure nil
  :when enable-ocaml
  :after opam-std-libs
  :config
  (add-to-list 'exec-path "~/.opam/default/bin"))

;;; ##########################################################################

(use-package merlin
  :when enable-ocaml
  :demand t
  :ensure nil
  :delight " 🪄"
  :after opam-emacs-setup)

(use-package merlin-eldoc
  :when enable-ocaml :defer t :after merlin :ensure t)

(use-package merlin-company
  :when (and enable-ocaml (not (equal custom-ide 'custom-ide-lsp-bridge)))
  :ensure nil
  :defer t :after merlin company
  :config
  (add-hook 'merlin-mode-hook 'company-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'merlin-company-backend)))

;;; ##########################################################################

(use-package dune
  :ensure nil
  :when enable-ocaml
  :hook (dune-mode . opam-init)
  :demand t
  :ensure-system-package
  (dune . "opam install dune --yes"))

(use-package dune-flymake
  :when enable-ocaml
  :after dune
  :ensure nil
  :defer t)

(use-package dune-watch
  :when enable-ocaml
  :after dune
  :ensure nil
  :defer t)

;;; ##########################################################################

(defun mifi/tuareg-mode-hook ()
  (interactive)
  (merlin-mode t)
  (opam-init)
  (lsp-deferred)
  (dap-mode)
  (setq tuareg-mode-name "🐫")
  (when (functionp 'prettify-symbols-mode)
    (prettify-symbols-mode)))

(use-package tuareg
  :when enable-ocaml
  :init
  :ensure nil
  :demand t
  ;; :after opam-emacs-setup merlin jsonrpc
  :hook (tuareg-mode . mifi/tuareg-mode-hook)
  :mode
  ("\\.ml\\'" . mifi/tuareg-mode-hook)
  ("\\.mli\\'" . tuareg-mode)
  :custom
  (tuareg-indent-align-with-first-arg t)
  (compile-command "dune build ")
  :config
  (bind-keys :map tuareg-mode-map
    ("C-c ," . dap-ocaml-hydra/body)))

;; Does many things but also updates the exec-path to the local
;; opam environment.
(use-package tuareg-opam
  :when enable-ocaml
  :ensure nil
  :after tuareg)

;;; ##########################################################################

(let
  ((file (expand-file-name "opam-user-setup.el" emacs-config-directory)))
  (when (and (file-exists-p file) (not (featurep 'opam-user-setup)))
    (use-package opam-user-setup
      :when enable-ocaml
      :ensure nil
      :after tuareg
      :config
      (setq-default tuareg-indent-align-with-first-arg t)
      (setq-default compile-command "dune build ")
      (add-hook 'tuareg-mode-hook #'mifi/tuareg-mode-hook))))

;;; ##########################################################################

(use-package ocp-indent
  :when enable-ocaml
  :after opam-emacs-setup
  :ensure nil
  :ensure-system-package
  ("~/.opam/default/bin/ocp-indent" . "opam install ocp-indent --yes"))

(use-package ocamlformat
  :when enable-ocaml
  :after ocp-indent
  :ensure nil
  :bind ("<f6>" . ocamlformat)
  :ensure-system-package
  ("~/.opam/default/lib/ocamlformat-lib" . "opam install ocamlformat --yes")
  :custom (ocamlformat-enable 'enable-outside-detected-project))

(use-package utop
  :when enable-ocaml
  :after opam-user-setup
  :ensure nil
  :defer t
  :custom
  (utop-command "opam config exec utop -- -emacs"))

(use-package opam-switch-mode
  :ensure t
  :when enable-ocaml
  :after opam-user-setup
  :hook
  (tuareg-mode . opam-switch-mode))

;;; ##########################################################################

;; For Emacs >= 30.0 it is possible to use the VC command like this:
;; :vc (:url "https://github.com/emacs-lsp/dap-mode"
;;      :main-file "dap-ocaml.el")
;;
(use-package dap-ocaml
  :when enable-ocaml
  :after (:all dap-mode opam-emacs-setup)
  :ensure nil  ;; Should be part of dap-mode
  :config
  (mifi/define-ocaml-dap-hydra)
  :ensure-system-package
  ((ocamllsp . "opam install earlybird --yes")))

(use-package dap-codelldb
  :when enable-ocaml
  :after dap-mode
  :defer t
  :ensure nil)

;;; ##########################################################################

(provide 'mifi-setup-ocaml)
;;; mifi-setup-ocaml.el ends here.
