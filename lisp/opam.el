;;; opam.el --- OPAM tools                 -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2015  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/lunaryorn/opam.el
;; Keywords: convenience
;; Version: 0.1-cvs
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; OPAM tools for Emacs.
;;
;; See URL `http://opam.ocamlpro.com/'.

;;; Code:

(eval-when-compile
  (require 'pcase)
  (require 'cl-lib))

(eval-and-compile
  (defconst opam-rx-constituents
    `((entry  . ,(rx alnum (+ (in alnum "." "+" "-"))))
      (status . ,(rx (in ?I ?C)))))

  (defmacro opam-rx (&rest regexps)
    "Opan mode specialized rx macro.

This variant of `rx' supports common opam named REGEXPS."
    (let ((rx-constituents (append opam-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t))))))

(defconst opam-switch-entry-regexp
  (opam-rx line-start
           (group (or entry "--"))
           (+ space)
           (group (or status "--"))
           (+ space)
           (group (or entry "--"))
           (+ space)
           (group (one-or-more not-newline))
           line-end)
  "Regexp for opam switch list entries.")

(cl-defstruct (opam-switch (:constructor opam-switch-new)
                           (:type vector))
  "A structure holding all the information of an Opam switch compiler."
  name                                  ; Switch name
  state                                 ; Switch state
  compiler                              ; Compiler name
  description                           ; Compiler Description
  )

;; Shamelessly stolen from: https://github.com/magnars/s.el/blob/fc395c8/s.el#L445-L462
(defun opam-regexp-match (regexp string &optional start)
  "Retrieve the match of REGEXP against a matching STRING.

Behaves like JavaScript's String.prototype.match.  When the given
expression matches the string, this function returns a list of
the whole matching string and a string for each matched
sub-expressions.  If it did not match the returned value is an
empty list (nil).

When START is non-nil the search will start at that index."
  (save-match-data
    (if (string-match regexp string start)
        (let ((match-data-list (match-data))
              result)
          (while match-data-list
            (let* ((beg (car match-data-list))
                   (end (cadr match-data-list))
                   (subs (if (and beg end) (substring string beg end) nil)))
              (setq result (cons subs result))
              (setq match-data-list
                    (cddr match-data-list))))
          (nreverse result)))))

(defun opam-switch-parse-output (output)
  "Parse OUTPUT of opam switch list."
  (cl-loop for line in (split-string output "\n" 'omit-nulls)
           when (opam-regexp-match opam-switch-entry-regexp line)
           collect (cl-multiple-value-bind (_match name state compiler description) it
                     (opam-switch-new :name name :state state :compiler compiler :description description))))

(defun opam-quote-command (command &rest args)
  "Quote COMMAND and ARGS."
  (mapconcat #'shell-quote-argument (cons command args) " "))

(defun opam-exec-insert (program &rest args)
  "Execute PROGRAM with ARGS, inserting its output at point."
  (apply #'process-file program nil (list t nil) nil args))

(defun opam-list-switch (&optional all)
  "Return a list of opam switch.  If ALL is non-nil return all switch available."
  (with-temp-buffer
    (apply #'opam-exec-insert "opam" "switch" "list" (and all '("--all")))
    (opam-switch-parse-output (buffer-string))))

(defun opam-switch-list-entries ()
  "Return a tabulated list entries."
  (cl-loop for switch in (opam-list-switch 'all)
           collect (list (opam-switch-compiler switch) switch)))

(defun opam-switch-read-compiler (&optional arg)
  "Read ocaml switch compiler, if ARG is non-nil will read all compilers."
  (or (and (not arg) (derived-mode-p 'opam-switch-list-mode) (tabulated-list-get-id))
      (completing-read "Switch compiler: " (mapcar #'opam-switch-compiler (opam-list-switch arg)) nil t)))

(defun opam-switch-list-reload ()
  "Revert opam switch list view."
  (let ((buffer (get-buffer "*opam switch*")))
    (and buffer (with-current-buffer buffer (revert-buffer)))))

;;;###autoload
(defun opam-switch (name)
  "Call opam switch NAME."
  (interactive (list (opam-switch-read-compiler current-prefix-arg)))
  (cl-labels ((opam-reload-env (&rest _) (opam-init) (opam-switch-list-reload))
              (update-start-hook (&rest _)
                                 (setq-local compilation-finish-functions (cons #'opam-reload-env compilation-finish-functions))))
    (let ((compilation-start-hook (cons #'update-start-hook compilation-start-hook)))
      (compilation-start (opam-quote-command "opam" "switch" name)))))

(defvar opam-switch-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "s" 'opam-switch)
    map)
  "Local keymap for `opam-switch-list-mode' buffers.")

(define-derived-mode opam-switch-list-mode tabulated-list-mode "opam-switch"
  "List available nix packages.

\\{opam-switch-list-mode-map}"
  (setq tabulated-list-format [("name" 30 t :read-only t)
                               ("state" 7 t :read-only t)
                               ("compiler" 30 t :read-only t)
                               ("description" 60 t :read-only t)]
        tabulated-list-padding 2
        tabulated-list-entries 'opam-switch-list-entries
        tabulated-list-sort-key '("state" . t))
  (tabulated-list-init-header))

;;;###autoload
(defun opam-switch-list ()
  "List of available opam switch."
  (interactive)
  (with-current-buffer (get-buffer-create "*opam switch*")
    (opam-switch-list-mode)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

(defun opam-env ()
  "Get the OPAM environment.

Return an alist mapping environment variables to their value."
  (with-temp-buffer
    (let ((opam (executable-find "opam")))
      (when opam
        (let ((exit-code (call-process "opam" nil t nil
                                       "config" "env" "--sexp")))
          (if (not (equal exit-code 0))
              (error "opam config env failed with exit code %S and output:
%s" exit-code (buffer-substring-no-properties (point-min) (point-max)))
            (goto-char (point-min))
            (let ((sexps (read (current-buffer))))
              (skip-chars-forward "[:space:]")
              (unless (eobp)
                (lwarn 'opam :warning "Trailing text in opam config env:\n%S"
                       (buffer-substring-no-properties (point) (point-max))))
              (mapcar (lambda (exp) (cons (car exp) (cadr exp))) sexps))))))))

;;;###autoload
(defun opam-init ()
  "Initialize OPAM in this Emacs.

See URL `http://opam.ocamlpro.com/' for more information about
OPAM."
  (pcase-dolist (`(,var . ,value) (opam-env))
    (setenv var value))
  ;; Update exec path
  (setq exec-path (append (parse-colon-path (getenv "PATH"))
                          (list exec-directory))))

(provide 'opam)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; opam.el ends here
