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
            (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
  			((zerop (apply #'call-process `("git" nil ,buffer t "clone"
  							   ,@(when-let ((depth (plist-get order :depth)))
  								 (list (format "--depth=%d" depth) "--no-single-branch"))
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
    (elpaca-use-package-mode)
    (setq elpaca-use-package-by-default t))
(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))

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

(defcustom custom-org-fill-column 120
    "The fill column width for Org mode text.
Note that the text is also centered on the screen so that should
be taken into consideration when providing a width."
    :type 'natnum
    :group 'mrf-custom)

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
(make-directory working-files-directory t)  ;; Continues to work even if dir exists

;;; Point the user-emacs-directory to the new working directory
(setq user-emacs-directory working-files-directory)
(message (concat ">>> Setting emacs-working-files directory to: " user-emacs-directory))

;;; Put any emacs cusomized variables in a special file
(setq custom-file (expand-file-name "customized-vars.el" working-files-directory))
(load custom-file 'noerror 'nomessage)

;;; --------------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name "lisp" emacs-config-directory))
(add-to-list 'load-path (expand-file-name "emacs-config-modules" emacs-config-directory))

;; (use-package config-custom
;;     :straight (config-custom :local-repo "/Users/strider/.emacs.d/emacs-config-modules"))

(require 'config-custom)
(require 'config-global)
(require 'config-frame)
(require 'config-theming)
(require 'config-completion)
(require 'config-org)
(require 'config-org-roam)
(require 'config-treemacs)
(require 'config-debug)
(require 'config-ide)
(require 'config-lang-support)
;; (require 'config-lang-js)
(require 'config-lang-python)
(require 'config-lang-other)
(require 'config-company)
(require 'config-dashboard)
(require 'config-undo)
(require 'config-term)
(require 'config-dired)
(require 'config-qol)
(use-package config-mmm
    :ensure (:repo "~/.emacs.d/emacs-config-modules")
    :after which-key)

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

;;; init.el ends here.
