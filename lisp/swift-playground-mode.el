;;; swift-playground-mode.el --- Run Apple's playgrounds in Swift buffers -*- lexical-binding: t -*-
;;
;; Copyright 2019 Michael Sanders
;;
;; Authors: Michael Sanders <emacs@michaelsande.rs>
;; URL: https://gitlab.com/michael.sanders/swift-playground-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (seq "2.2.0"))
;; Keywords: languages swift
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; Run Apple's playgrounds in Swift buffers.
;;
;; License:
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;

;;; Code:

(require 'comint)
(require 'seq)

(defgroup swift-playground nil
  "A Swift playground Emacs client."
  :link '(url-link :tag "GitLab"
                   "https://gitlab.com/michael.sanders/swift-playground-mode")
  :group 'tools
  :group 'programming)

(defvar swift-playground-buffer nil
  "Stores the name of the current swift playground buffer, or nil.")

(defvar swift-playground--build-directory nil
  "Stores the name of the current swift playground build directory, or nil.")

;;; Keymap

(defvar swift-playground-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define swift-playground-menu map "Swift Mode menu"
      `("Swift Playground"
        :help "Swift playground specific features"
        ["Run playground" swift-playground-run
         :help "Run Swift Playground"]))
    map)
  "Swift playground mode key map.")

(defun swift-playground--populate-playground-buffer (transform)
  "Instantiate a new or existing playground buffer.
TRANSFORM is a function to invoke that modifies or populates the buffer."
  (let* ((buffer-name (or swift-playground-buffer "*Playground*"))
         (buffer (get-buffer-create buffer-name)))
    (display-buffer buffer '((display-buffer-in-side-window) (side . right)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall transform)))
      (read-only-mode t)
    (setq swift-playground-buffer buffer-name)))

;;;###autoload
(defun swift-playground-close-buffer ()
    "Close the current playground buffer if it is being displayed."
  (when swift-playground-buffer
    (delete-windows-on swift-playground-buffer)
    (kill-buffer swift-playground-buffer)
    (setq swift-playground-buffer nil)))

;; The next two function are taken from swift-mode-repl.el
;; https://github.com/swift-emacs/swift-mode/blob/be8d770/swift-mode-repl.el
(defun swift-playground--call-process (executable &rest args)
  "Call EXECUTABLE synchronously in separate process.
EXECUTABLE may be a string or a list. The string is splitted by
spaces, then unquoted. ARGS are rest arguments, appended to the
argument list. Return the exit status."
  (swift-playground--do-call-process executable nil t nil args))

(defun swift-playground--do-call-process (executable infile destination display args)
  "Wrapper for `call-process'.
EXECUTABLE may be a string or a list. The string is splitted by
spaces, then unquoted. For INFILE, DESTINATION, DISPLAY, see
`call-process'. ARGS are rest arguments, appended to the argument
list. Return the exit status."
  (let ((command-list
         (append (swift-playground--command-string-to-list executable) args)))
    (apply #'call-process
           (car command-list)
           infile
           destination
           display
           (cdr command-list))))

(defun swift-playground--call-process-with-output (executable &rest args)
  "Call EXECUTABLE synchronously in separate process.
EXECUTABLE may be a string or a list. The string is splitted by
spaces, then unquoted. ARGS are rest arguments, appended to the
argument list. Return the output of the process."
  (with-temp-buffer
    (unless (zerop
             (apply #'swift-playground--call-process executable args))
      (error "%s: %s" "Cannot invoke executable" (buffer-string)))
    (buffer-string)))

(defun swift-playground--command-string-to-list (cmd)
  "Split the CMD unless it is a list.
This function respects quotes."
  (if (listp cmd) cmd (split-string-and-unquote cmd)))

(defconst swift-playground--script-directory
  (when load-file-name (file-name-directory load-file-name))
  "Directory which contains swift-playground-mode.el.")

(defun swift-playground--parse-log-prefix (log-message separator)
  "Parse range in form of [_start_:_end_] from the prefix of LOG-MESSAGE.
SEPARATOR is the delimiter by which each record is joined, e.g.
`[_range_]_$builtin_LogMessage' has a separator of `_'. Return
list of two pairs in the form of
`((start_line start_col) (end_line end_col))'."
  (let* ((prefix (replace-regexp-in-string (concat "\\[\\([0-9\-:]*\\)\\]"
                                                   separator
                                                   ".*") "\\1" log-message))
         (ranges (split-string prefix "-")))
    (when (= (length ranges) 2)
      (mapcar (lambda (range) (mapcar
                               #'string-to-number
                               (split-string range ":"))) ranges))))

;;;###autoload
(defun swift-playground-current-buffer-playground-p ()
  "Return non-nil if the current swift buffer is a playground."
  (and (buffer-file-name)
       (string-suffix-p ".playground/Contents.swift" (buffer-file-name) t)))

;;;###autoload
(defun swift-playground-run ()
  "Run the current swift buffer as a playground."
  (interactive)
  (let* ((result (swift-playground--call-process-with-output
                  "bash"
                  (expand-file-name "runner.sh"
                                    swift-playground--script-directory)
                  (buffer-file-name)))
         (lines (split-string result "[\n\r]+"))
         ;; Track the line values, for they may be recorded multiple times by
         ;; the runtime.
         (set-lines (make-hash-table :test 'equal))
         (line-num 0)
         (doc ""))
    (setq swift-playground--build-directory (car lines))
    (dolist (line lines)
      (unless (gethash line set-lines)
        ;; Logs come in looking like [_range_] $builtin LogMessage.
        (let ((split-value (split-string line " \\$builtin_log ")))
          (unless (< (length split-value) 2)
            (let ((target (caar (swift-playground--parse-log-prefix line " "))))
              (while (< line-num (- target 1))
                (setq doc (concat doc "\n"))
                (setq line-num (+ 1 line-num)))

              (let ((line-value (nth 1 split-value)))
                (puthash line 1 set-lines)
                (setq doc (concat doc line-value "\n"))
                (setq line-num (+ 1 line-num))))))))
    (swift-playground--populate-playground-buffer (lambda () (insert doc)))))

;;;###autoload
(defun swift-playground-preview-image ()
  "Preview an image rendered from the current cursor position."
  (interactive)
  (when (seq-empty-p swift-playground--build-directory)
    (swift-playground-run))
  (let* ((current-line (line-number-at-pos))
         (current-col (current-column))
         (asset-dir (expand-file-name "Assets"
                                      swift-playground--build-directory))
         (matched-name "")
         (matched-col -1))
    (cl-dolist (name (directory-files asset-dir t ".*\.png"))
      (unless (< (length (split-string name "_\\$builtin_log_")) 2)
        (let* ((basename (file-name-base name))
               (range (swift-playground--parse-log-prefix basename "_"))
               (start (car range))
               (end (cadr range))
               (start-line (car start))
               (start-col (cadr end))
               (end-line (car end))
               (end-col (cadr end)))
          (when (and (<= start-line current-line) (>= end-line current-line)
                     (<= start-col current-col) (>= end-col current-col))
            (setq matched-name name)
            (cl-return))
          (when (and (= end-line current-line) (< matched-col end-col))
            (setq matched-name name)
            (setq matched-col end-col)))))

    (if (seq-empty-p matched-name)
        (error "No image can be rendered at the current position")
      (swift-playground--populate-playground-buffer
       (lambda () (insert-image (create-image matched-name)))))))

;;;###autoload
(define-minor-mode swift-playground-mode
  "Minor mode for editing/running Swift playgrounds.

  \\{swift-playground-mode-map}

When called interactively, toggle `swift-playground-mode'. With
prefix ARG, enable `swift-playground-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `swift-playground-mode' if ARG is
omitted, nil or positive. If ARG is `toggle', toggle
`swift-playground-mode'. Otherwise behave as if called
interactively."
  :init-value nil
  :group 'swift-playground
  :lighter " Playground"
  (if swift-playground-mode
      (progn
        (swift-playground-run)
        (add-hook 'after-save-hook #'swift-playground-run nil 'local))
    (swift-playground-close-buffer)
    (remove-hook 'after-save-hook #'swift-playground-run 'local)))

;;;###autoload
(define-globalized-minor-mode swift-playground-global-mode
  swift-playground-mode
  (lambda ()
    (when (swift-playground-current-buffer-playground-p)
      (swift-playground-mode t))))

(defun swift-playground-setup ()
  "Initialize Swift playground mode hooks."
  (add-hook 'swift-mode-hook #'swift-playground-global-mode))

(provide 'swift-playground-mode)

;;;; Closing remarks

;;; swift-playground-mode.el ends here

;; Local Variables:
;; sentence-end-double-space: nil
;; End:
