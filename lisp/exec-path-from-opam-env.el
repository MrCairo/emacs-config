;;; exec-path-from-opam-env.el --- Get environment variables such as $PATH from the opam env and shell  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Mitchell Fisher, Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>, Mitchell Fisher <
;; Keywords: unix, environment
;; URL: https://github.com/purcell/exec-path-from-opam-env
;; Package-Version: 2.2
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; On macOS (and perhaps elsewhere) the $PATH environment variable and
;; `exec-path' used by a windowed Emacs instance will usually be the
;; system-wide default path, rather than that seen in a terminal
;; window.

;; This library allows the user to set Emacs' `exec-path' and $PATH
;; from the shell path, so that `shell-command', `compile' and the
;; like work as expected.

;; This library will also execute an `eval $(opam env)` anytime the
;; exec-path-from-opam-env-initialize() or
;; exec-path-from-opam-env() is called. This is necessary since the
;; opam environment can act like a python virtual environment
;; depending on how a project is initialized. This module does NOT
;; automatically detect a opam virtual environment however, the
;; exec-path-from-opam-env() must be explicitly called.

;; It also allows other environment variables to be retrieved from the
;; shell, so that Emacs will see the same values you get in a terminal.

;; If you use a non-POSIX-standard shell like "tcsh" or "fish", your
;; shell will be asked to execute "sh" as a subshell in order to print
;; out the variables in a format which can be reliably parsed.  "sh"
;; must be a POSIX-compliant shell in this case.

;; Note that shell variables which have not been exported as
;; environment variables (e.g. using the "export" keyword) may not be
;; visible to `exec-path-from-opam-env'.

;; Installation:

;; ELPA packages are available on Marmalade and MELPA.  Alternatively,
;; place this file on a directory in your `load-path', and explicitly
;; require it.

;; Usage:
;;
;;     (require 'exec-path-from-opam-env) ;; if not using the ELPA package
;;     (exec-path-from-opam-env-initialize)
;;
;; Customize `exec-path-from-opam-env-variables' to modify the list of
;; variables imported.
;;
;; If you use your Emacs config on other platforms, you can instead
;; make initialization conditional as follows:
;;
;;     (when (memq window-system '(mac ns))
;;       (exec-path-from-opam-env-initialize))
;;
;; Alternatively, you can use `exec-path-from-opam-env-copy-envs' or
;; `exec-path-from-opam-env-copy-env' directly, e.g.
;;
;;     (exec-path-from-opam-env-copy-env "PYTHONPATH")

;;; Code:

;; Satisfy the byte compiler
(eval-when-compile (require 'eshell))
(require 'cl-lib)
(require 'json)

(defgroup exec-path-from-opam-env nil
  "Make Emacs use shell-defined values for $PATH etc."
  :prefix "exec-path-from-opam-env-"
  :group 'environment)

(defcustom exec-path-from-opam-env-variables
  '("PATH" "MANPATH")
  "List of environment variables which are copied from the shell."
  :type '(repeat (string :tag "Environment variable"))
  :group 'exec-path-from-opam-env)

(defcustom exec-path-from-opam-env-warn-duration-millis 500
  "Print a warning if shell execution takes longer than this many milliseconds."
  :type 'integer)

(defcustom exec-path-from-opam-env-shell-name nil
  "If non-nil, use this shell executable.
Otherwise, use either `shell-file-name' (if set), or the value of
the SHELL environment variable."
  :type '(choice
          (file :tag "Shell executable")
          (const :tag "Use `shell-file-name' or $SHELL" nil))
  :group 'exec-path-from-opam-env)

(defvar exec-path-from-opam-env-debug t
  "Display debug info when non-nil.")

(defvar exec-path-eval-opam-env t
  "Execute an 'opam env' on the next initialize call.")

(defvar exec-path-opam-env-cmd "eval $(opam env)"
  "Command use to get the environment variables generated from the
opam environment.")

(defun exec-path-from-opam-env--double-quote (s)
  "Double-quote S, escaping any double-quotes already contained in it."
  (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\""))

(defun exec-path-from-opam-env--shell ()
  "Return the shell to use.
See documentation for `exec-path-from-opam-env-shell-name'."
  (or
   exec-path-from-opam-env-shell-name
   shell-file-name
   (getenv "SHELL")
   (error "SHELL environment variable is unset")))

(defcustom exec-path-from-opam-env-arguments
  (let ((shell (exec-path-from-opam-env--shell)))
    (if (string-match-p "t?csh$" shell)
        (list "-d")
      (if (string-match-p "fish" shell)
          (list "-l")
        (list "-l" "-i"))))
  "Additional arguments to pass to the shell.

The default value denotes an interactive login shell."
  :type '(repeat (string :tag "Shell argument"))
  :group 'exec-path-from-opam-env)

(defun exec-path-from-opam-env--debug (msg &rest args)
  "Print MSG and ARGS like `message', but only if debug output is enabled."
  (when exec-path-from-opam-env-debug
    (apply 'message msg args)))

(defun exec-path-from-opam-env--nushell-p (shell)
  "Return non-nil if SHELL is nu."
  (string-match-p "nu$" shell))

(defun exec-path-from-opam-env--standard-shell-p (shell)
  "Return non-nil iff SHELL supports the standard ${VAR-default} syntax."
  (not (string-match-p "\\(fish\\|nu\\|t?csh\\)$" shell)))

(defmacro exec-path-from-opam-env--warn-duration (&rest body)
  "Evaluate BODY and warn if execution duration exceeds a time limit.
The limit is given by `exec-path-from-opam-env-warn-duration-millis'."
  (let ((start-time (cl-gensym))
        (duration-millis (cl-gensym)))
    `(let ((,start-time (current-time)))
       (prog1
           (progn ,@body)
         (let ((,duration-millis (* 1000.0 (float-time (time-subtract (current-time) ,start-time)))))
           (if (> ,duration-millis exec-path-from-opam-env-warn-duration-millis)
               (message "Warning: exec-path-from-opam-env execution took %dms. See the README for tips on reducing this." ,duration-millis)
             (exec-path-from-opam-env--debug "Shell execution took %dms" ,duration-millis)))))))

(defun exec-path-from-opam-env-printf (str &optional args)
  "Return the result of printing STR in the user's shell.

Executes the shell as interactive login shell.

STR is inserted literally in a single-quoted argument to printf,
and may therefore contain backslashed escape sequences understood
by printf.

ARGS is an optional list of args which will be inserted by printf
in place of any % placeholders in STR.  ARGS are not automatically
shell-escaped, so they may contain $ etc."
  (let* ((printf-bin (or (executable-find "printf") "printf"))
         (printf-command
          (concat (shell-quote-argument printf-bin)
                  " '__RESULT\\000" str "\\000__RESULT' "
                  (mapconcat #'exec-path-from-opam-env--double-quote args " ")))
         (shell (exec-path-from-opam-env--shell))
         (shell-args (append exec-path-from-opam-env-arguments
                             (list "-c"
                                   (if (exec-path-from-opam-env--standard-shell-p shell)
                                     (concat exec-path-opam-env-cmd
                                       " && " printf-command)
                                     ;; else
                                     (concat "sh -c " (shell-quote-argument printf-command)))))))
    (with-temp-buffer
      (exec-path-from-opam-env--debug "Invoking shell %s with args %S" shell shell-args)
      (let ((exit-code (exec-path-from-opam-env--warn-duration
                        (apply #'call-process shell nil t nil shell-args))))
        (exec-path-from-opam-env--debug "Shell printed: %S" (buffer-string))
        (unless (zerop exit-code)
          (error "Non-zero exit code from shell %s invoked with args %S.  Output was:\n%S"
                 shell shell-args (buffer-string))))
      (goto-char (point-min))
      (if (re-search-forward "__RESULT\0\\(.*\\)\0__RESULT" nil t)
          (match-string 1)
        (error "Expected printf output from shell, but got: %S" (buffer-string))))))

(defun exec-path-from-opam-env-getenvs--nushell (names)
  "Use nushell to get the value of env vars with the given NAMES.

Execute the shell according to `exec-path-from-opam-env-arguments'.
The result is a list of (NAME . VALUE) pairs."
  (let* ((shell (exec-path-from-opam-env--shell))
         (expr (format "[ %s ] | to json"
                       (string-join
                        (mapcar (lambda (name)
                                  (format "$env.%s?" (exec-path-from-opam-env--double-quote name)))
                                names)
                        ", ")))
          (shell-args (append (concat exec-path-opam-env-cmd " && ")
                        exec-path-from-opam-env-arguments (list "-c" expr))))
    (with-temp-buffer
      (exec-path-from-opam-env--debug "Invoking the shell %s with args %S" shell shell-args)
      (let ((exit-code (exec-path-from-opam-env--warn-duration
                        (apply #'call-process shell nil t nil shell-args))))
        (exec-path-from-opam-env--debug "Shell printed: %S" (buffer-string))
        (unless (zerop exit-code)
          (error "Non-zero exit code from shell %s invoked with args %S.  Output was:\n%S"
                 shell shell-args (buffer-string))))
      (goto-char (point-min))
      (let ((json-array-type 'list)
            (json-null :null))
        (let ((values (json-read-array))
              result)
          (while values
            (let ((value (car values)))
              (push (cons (car names)
                          (unless (eq :null value)
                            (if (listp value)
                                (string-join value path-separator)
                              value)))
                    result))
            (setq values (cdr values)
                  names (cdr names)))
          result)))))

(defun exec-path-from-opam-env-getenvs (names)
  "Get the environment variables with NAMES from the user's shell.

Execute the shell according to `exec-path-from-opam-env-arguments'.
The result is a list of (NAME . VALUE) pairs."
  (when (file-remote-p default-directory)
    (error "You cannot run exec-path-from-opam-env from a remote buffer (Tramp, etc.)"))
  (if (exec-path-from-opam-env--nushell-p (exec-path-from-opam-env--shell))
      (exec-path-from-opam-env-getenvs--nushell names)
    (let* ((random-default (md5 (format "%s%s%s" (emacs-pid) (random) (current-time))))
           (dollar-names (mapcar (lambda (n) (format "${%s-%s}" n random-default)) names))
           (values (split-string (exec-path-from-opam-env-printf
                                  (mapconcat #'identity (make-list (length names) "%s") "\\000")
                                  dollar-names) "\0")))
      (let (result)
        (while names
          (prog1
              (let ((value (car values)))
                (push (cons (car names)
                            (unless (string-equal random-default value)
                              value))
                      result))
            (setq values (cdr values)
                  names (cdr names))))
        result))))

(defun exec-path-from-opam-env-getenv (name)
  "Get the environment variable NAME from the user's shell.

Execute the shell as interactive login shell, have it output the
variable of NAME and return this output as string."
  (cdr (assoc name (exec-path-from-opam-env-getenvs (list name)))))

(defun exec-path-from-opam-env-setenv (name value)
  "Set the value of environment var NAME to VALUE.
Additionally, if NAME is \"PATH\" then also update the
variables `exec-path' and `eshell-path-env'."
  (setenv name value)
  (when (string-equal "PATH" name)
    (setq exec-path (append (parse-colon-path value) (list exec-directory)))
    ;; `eshell-path-env' is a buffer local variable, so change its default
    ;; value.
    (setq-default eshell-path-env value)))

;;;###autoload
(defun exec-path-from-opam-env-copy-envs (names)
  "Set the environment variables with NAMES from the user's shell.

As a special case, if the variable is $PATH, then the variables
`exec-path' and `eshell-path-env' are also set appropriately.
The result is an alist, as described by
`exec-path-from-opam-env-getenvs'."
  (let ((pairs (exec-path-from-opam-env-getenvs names)))
    (mapc (lambda (pair)
            (exec-path-from-opam-env-setenv (car pair) (cdr pair)))
          pairs)))

;;;###autoload
(defun exec-path-from-opam-env-copy-env (name)
  "Set the environment variable $NAME from the user's shell.

As a special case, if the variable is $PATH, then the variables
`exec-path' and `eshell-path-env' are also set appropriately.
Return the value of the environment variable."
  (interactive "sCopy value of which environment variable from shell? ")
  (cdar (exec-path-from-opam-env-copy-envs (list name))))

;;;###autoload
(defun exec-path-from-opam-env-initialize ()
  "Initialize environment from the user's shell.

The values of all the environment variables named in
`exec-path-from-opam-env-variables' are set from the corresponding
values used in the user's shell."
  (interactive)
  (exec-path-from-opam-env-copy-envs exec-path-from-opam-env-variables))

;;;###autoload
(defun exec=path-from-opam-env ()
  "Using opam, fetch it's environment and include it in the exec-path."
  (interactive)
  (exec-path-from-opam-env-initialize))

(provide 'exec-path-from-opam-env)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; require-final-newline: t
;; checkdoc-minor-mode: t
;; End:

;;; exec-path-from-opam-env.el ends here
