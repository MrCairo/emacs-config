;;; init-terminals.el -- Things that need to be done first.
;;
;;; Commentary:
;;; ------------------------------------------------------------------------

;;; Code:
;;; =term-mode= is a built-in terminal emulator in Emacs.  Because it is
;;; written in Emacs Lisp, you can start using it immediately with very little
;;; configuration.  If you are on Linux or macOS, =term-mode= is a great
;;; choice to get started because it supports fairly complex terminal
;;; applications (=htop=, =vim=, etc) and works pretty reliably.  However,
;;; because it is written in Emacs Lisp, it can be slower than other options
;;; like =vterm=.  The speed will only be an issue if you regularly run
;;; console apps with a lot of output.
;;;
;;; One important thing to understand is =line-mode= versus =char-mode=.
;;; =line-mode= enables you to use normal Emacs keybindings while moving
;;; around in the terminal buffer while =char-mode= sends most of your
;;; keypresses to the underlying terminal.  While using =term-mode=, you will
;;; want to be in =char-mode= for any terminal applications that have their
;;; own keybindings.  If you're just in your usual shell, =line-mode= is
;;; sufficient and feels more integrated with Emacs.
;;; Run a terminal with =M-x term!=

;;; *Useful key bindings:*
;;; =C-c C-p= / =C-c C-n= - go back and forward in the buffer's prompts (also
;;;     =[[= and =]]= with evil-mode)

;;; =C-c C-k= - Enter char-mode

;;; =C-c C-j= - Return to line-mode

;;; If you have =evil-collection= installed, =term-mode= will enter char mode
;;; when you use Evil's Insert mode

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

;;; The =eterm-256color= package enhances the output of =term-mode= to enable
;;; handling of a wider range of color codes so that many popular terminal
;;; applications look as you would expect them to.  Keep in mind that this
;;; package requires =ncurses= to be installed on your machine so that it has
;;; access to the =tic= program.  Most Linux distributions come with this
;;; program installed already so you may not have to do anything extra to use
;;; it.


(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))


;;; vterm is an improved terminal emulator package which uses a compiled
;;; native module to interact with the underlying terminal applications.  This
;;; enables it to be much faster than =term-mode= and to also provide a more
;;; complete terminal emulation experience.
;;; https://github.com/akermu/emacs-libvterm
;;;
;;; Make sure that you have the necessary dependencies installed before trying
;;; to use =vterm= because there is a module that will need to be compiled
;;; before you can use it successfully.

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

;;;
;;; Eshell is Emacs' own shell implementation written in Emacs Lisp.  It
;;; provides you with a cross-platform implementation (even on Windows!) of
;;; the common GNU utilities you would find on Linux and macOS (=ls=, =rm=,
;;; =mv=, =grep=, etc).  It also allows you to call Emacs Lisp functions
;;; directly from the shell and you can even set up aliases (like aliasing
;;; =vim= to =find-file=).  Eshell is also an Emacs Lisp REPL which allows you
;;; to evaluate full expressions at the shell.
;;;
;;; https://www.gnu.org/software/emacs/manual/html_mono/eshell.html#Contributors-to-Eshell
;;;
;;; The downsides to Eshell are that it can be harder to configure than other
;;; packages due to the particularity of where you need to set some options
;;; for them to go into effect, the lack of shell completions (by default) for
;;; some useful things like Git commands, and that REPL programs sometimes
;;; don't work as well.  However, many of these limitations can be dealt with
;;; by good configuration and installing external packages, so don't let that
;;; discourage you from trying it!

;;; *Useful key bindings:*

;;; - =C-c C-p= / =C-c C-n= - go back and forward in the buffer's prompts
;;;    (also =[[= and =]]= with evil-mode)
;;;
;;; - =M-p= / =M-n= - go back and forward in the input history
;;; - =C-c C-u= - delete the current input string backwards up to the cursor
;;; - =counsel-esh-history= - A searchable history of commands typed into Eshell

(defun mrf/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . mrf/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(provide 'init-terminals)

;;; init-terminals.el ends here.
