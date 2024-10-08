;;; ob-ocaml.el --- Babel Functions for OCaml      -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2024 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	 Dan Davison
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating OCaml source code.

;;; Code:
(require 'ob)

(declare-function org-remove-indentation "org" )
(declare-function org-trim "org" (s &optional keep-lead))
(declare-function run-ocaml "ext:tuareg" (&optional cmd dedicated show))
(declare-function dune-build "ext:dune" (&optional cmd dedicated show))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("ocaml" . "ml"))

(defvar org-babel-default-header-args:tuareg '())

(defcustom org-babel-ocaml-command "ocaml"
  "Name of the command for executing OCaml code."
  :version "29.4"
  :package-version '(Org . "9.8")
  :group 'org-babel
  :type 'string)

;;; The primary OCaml mode is handled by tuareg-mode
(defcustom org-babel-ocaml-mode
  (if (featurep 'tuareg-mode) 'tuareg-mode 'ocaml)
  "Preferred mode for use in running ocaml interactively.
This will typically be either `ocaml' or `tuareg-mode'."
  :group 'org-babel
  :version "29.4"
  :package-version '(Org . "9.8")
  :type 'symbol)

(defcustom org-babel-ocaml-hline-to "nil"
  "Replace hlines in incoming tables with this when translating to OCaml."
  :group 'org-babel
  :version "29.4"
  :package-version '(Org . "9.8")
  :type 'string)

(defcustom org-babel-ocaml-nil-to 'hline
  "Replace `nil' in ocaml tables with this before returning."
  :group 'org-babel
  :version "29.4"
  :package-version '(Org . "9.8")
  :type 'symbol)

(defun org-babel-execute:tuareg (body params)
  "Execute a block of OCaml code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((org-babel-ocaml-command
	  (or (cdr (assq :tuareg params))
	      org-babel-ocaml-command))
	 (session (org-babel-ocaml-initiate-session
		   (cdr (assq :session params))))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
	 (return-val (when (and (eq result-type 'value) (not session))
		       (cdr (assq :return params))))
	 (preamble (cdr (assq :preamble params)))
         (full-body
	  (org-babel-expand-body:generic
	   (concat body (if return-val (format "\n%s" return-val) ""))
	   params (org-babel-variable-assignments:tuareg params)))
         (result (org-babel-ocaml-evaluate
		  session full-body result-type result-params preamble)))
    (org-babel-reassemble-table
     result
     (org-babel-pick-name (cdr (assq :colname-names params))
			  (cdr (assq :colnames params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
			  (cdr (assq :rownames params))))))

(defun org-babel-prep-session:tuareg (session params)
  "Prepare SESSION according to the header arguments in PARAMS.
VARS contains resolved variable references"
  (let* ((session (org-babel-ocaml-initiate-session session))
	 (var-lines
	  (org-babel-variable-assignments:tuareg params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input)
              (org-babel-comint-wait-for-output session)) var-lines))
    session))

(defun org-babel-load-session:tuareg (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:tuareg session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions

(defun org-babel-variable-assignments:tuareg (params)
  "Return a list of OCaml statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (format "let %s = %s"
	     (car pair)
	     (org-babel-ocaml-var-to-ocaml (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-ocaml-var-to-ocaml (var)
  "Convert an elisp value to an ocaml variable.
Convert an elisp value, VAR, into a string of ocaml source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-ocaml-var-to-ocaml var ", ") "]")
    (if (eq var 'hline)
	org-babel-ocaml-hline-to
      (format
       (if (and (stringp var) (string-match "[\n\r]" var)) "\"\"%S\"\"" "%S")
       (if (stringp var) (substring-no-properties var) var)))))

(defun org-babel-ocaml-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If the results look like a list or tuple, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (let ((res (org-babel-script-escape results)))
    (if (listp res)
        (mapcar (lambda (el) (if (eq el 'None)
                            org-babel-ocaml-None-to el))
                res)
      res)))

(defvar org-babel-ocaml-buffers '((:default . "*OCaml*")))

(defun org-babel-ocaml-session-buffer (session)
  "Return the buffer associated with SESSION."
  (cdr (assoc session org-babel-ocaml-buffers)))

(defun org-babel-ocaml-with-earmuffs (session)
  (let ((name (if (stringp session) session (format "%s" session))))
    (if (and (string= "*" (substring name 0 1))
	     (string= "*" (substring name (- (length name) 1))))
	name
      (format "*%s*" name))))

(defun org-babel-ocaml-without-earmuffs (session)
  (let ((name (if (stringp session) session (format "%s" session))))
    (if (and (string= "*" (substring name 0 1))
	     (string= "*" (substring name (- (length name) 1))))
	(substring name 1 (- (length name) 1))
      name)))

(defvar ocaml-default-interpreter)
(defvar ocaml-which-bufname)
(defvar ocaml-shell-buffer-name)

(defconst org-babel-ocaml-mode 'tuareg-mode
  "OCaml mode for use in running python interactively.")

(defun org-babel-ocaml-initiate-session-by-key (&optional session)
  "Initiate an ocaml session.
If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (require org-babel-ocaml-mode)
  (save-window-excursion
    (let* ((session (if session (intern session) :default))
           (ocaml-buffer (org-babel-ocaml-session-buffer session))
	   (cmd (if (member system-type '(cygwin windows-nt ms-dos))
		    (concat org-babel-ocaml-command " -stdin")
		  (concat org-babel-ocaml-command " -stdin"))))
      (cond
       ((and (eq 'ocaml org-babel-ocaml-mode)
	     (fboundp 'run-ocaml)) ; ocaml.el
	(if (not (version< "27.1" emacs-version))
	    (run-ocaml cmd)
	  (unless ocaml-buffer
	    (setq ocaml-buffer (org-babel-ocaml-with-earmuffs session)))
	  (let ((ocaml-shell-buffer-name
		 (org-babel-ocaml-without-earmuffs ocaml-buffer)))
	    (run-ocaml cmd))))
       ((and (eq 'ocaml-mode org-babel-ocaml-mode)
	     (fboundp 'py-shell)) ; ocaml-mode.el
	;; Make sure that ocaml-which-bufname is initialized, as otherwise
	;; it will be overwritten the first time an OCaml buffer is
	;; created.
	(py-toggle-shells ocaml-default-interpreter)
	;; `py-shell' creates a buffer whose name is the value of
	;; `ocaml-which-bufname' with '*'s at the beginning and end
	(let* ((bufname (if (and ocaml-buffer (buffer-live-p ocaml-buffer))
			    (replace-regexp-in-string ;; zap surrounding *
			     "^\\*\\([^*]+\\)\\*$" "\\1" ocaml-buffer)
			  (concat "OCaml-" (symbol-name session))))
	       (ocaml-which-bufname bufname))
	  (py-shell)
	  (setq ocaml-buffer (org-babel-ocaml-with-earmuffs bufname))))
       (t
	(error "No function available for running an inferior OCaml")))
      (setq org-babel-ocaml-buffers
	    (cons (cons session ocaml-buffer)
		  (assq-delete-all session org-babel-ocaml-buffers)))
      session)))

(defun org-babel-ocaml-initiate-session (&optional session _params)
  "Create a session named SESSION according to PARAMS."
  (unless (string= session "none")
    (org-babel-ocaml-session-buffer
     (org-babel-ocaml-initiate-session-by-key session))))

(defvar org-babel-ocaml-eoe-indicator "'org_babel_ocaml_eoe'"
  "A string to indicate that evaluation has completed.")
(defconst org-babel-ocaml-wrapper-method "")
(defconst org-babel-ocaml-pp-wrapper-method "")
(defconst org-babel-ocaml--exec-tmpfile
  (concat
   "__org_babel_ocaml_fname = '%s'; "))

(defun org-babel-ocaml-evaluate
  (session body &optional result-type result-params preamble)
  "Evaluate BODY as OCaml code."
  (message "++++ org-babel-ocaml-evaluate")
  (if session
      (org-babel-ocaml-evaluate-session
       session body result-type result-params)
    (org-babel-ocaml-evaluate-external-process
     body result-type result-params preamble)))

(defun org-babel-ocaml-evaluate-external-process
    (body &optional result-type result-params preamble)
  "Evaluate BODY in external ocaml process.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (message "+++ org-babel-ocaml-evaluate-external-process \nresult-type: %S\nresult-params: %S\npreamble: %S\nbody: %S\n\n"
    result-type result-params preamble body)
  (let ((raw
         (pcase result-type
           (`output (org-babel-eval org-babel-ocaml-command
				    (concat (if preamble (concat preamble "\n"))
					    body)))
           (`value (let ((tmp-file (org-babel-temp-file "ocaml-")))
		     (org-babel-eval
		      (concat org-babel-ocaml-command " -stdin ")
		      (concat
		       (if preamble (concat preamble "\n") "")
		       (format
			(if (member "pp" result-params)
			    org-babel-ocaml-pp-wrapper-method
			  org-babel-ocaml-wrapper-method)
			(org-babel-process-file-name tmp-file 'noquote))))
		     (org-babel-eval-read-file tmp-file))))))
    (org-babel-result-cond result-params
      raw
      (org-babel-ocaml-table-or-string (org-trim raw)))))

(defun org-babel-ocaml-evaluate-session
    (session body &optional result-type result-params)
  "Pass BODY to the OCaml process in SESSION.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (message "+++ org-babel-ocaml-evaluate-session")
  (let* ((send-wait (lambda () (comint-send-input nil t) (sleep-for 0 5)))
	 (dump-last-value
	  (lambda
	    (tmp-file pp)
	    (mapc
	     (lambda (statement) (insert statement) (funcall send-wait)))))
	 (last-indent 0)
	 (input-body (lambda (body)
		       (dolist (line (split-string body "[\r\n]"))
			 ;; Insert a blank line to end an indent
			 ;; block.
			 (let ((curr-indent (string-match "\\S-" line)))
			   (if curr-indent
			       (progn
				 (when (< curr-indent last-indent)
				   (insert "")
				   (funcall send-wait))
				 (setq last-indent curr-indent))
			     (setq last-indent 0)))
			 (insert line)
			 (funcall send-wait))
		       (funcall send-wait)))
         (results
          (pcase result-type
            (`output
	     (let ((body (if (string-match-p ".\n+." body) ; Multiline
			     (let ((tmp-src-file (org-babel-temp-file
						  "ocaml-")))
			       (with-temp-file tmp-src-file (insert body))
			       (format org-babel-ocaml--exec-tmpfile
				       tmp-src-file))
			   body)))
	       (mapconcat
		#'org-trim
		(butlast
		 (org-babel-comint-with-output
		     (session org-babel-ocaml-eoe-indicator t body)
		   (funcall input-body body)
		   (funcall send-wait) (funcall send-wait)
		   (insert org-babel-ocaml-eoe-indicator)
		   (funcall send-wait))
		 2) "\n")))
            (`value
             (let ((tmp-file (org-babel-temp-file "ocaml-")))
               (org-babel-comint-with-output
                   (session org-babel-ocaml-eoe-indicator nil body)
                 (let ((comint-process-echoes nil))
                   (funcall input-body body)
                   (funcall dump-last-value tmp-file
                            (member "pp" result-params))
                   (funcall send-wait) (funcall send-wait)
                   (insert org-babel-ocaml-eoe-indicator)
                   (funcall send-wait)))
               (org-babel-eval-read-file tmp-file))))))
    (unless (string= (substring org-babel-ocaml-eoe-indicator 1 -1) results)
      (org-babel-result-cond result-params
	results
        (org-babel-ocaml-table-or-string results)))))

(defun org-babel-ocaml-read-string (string)
  "Strip \\='s from around OCaml string."
  (if (and (string-prefix-p "'" string)
	   (string-suffix-p "'" string))
      (substring string 1 -1)
    string))

(provide 'ob-tuareg)
;;; ob-tuareg.el ends here
