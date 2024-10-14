;;; init-registers.el --- Define common registers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; ##########################################################################

(defvar emacs-config-directory nil)

(defun setup-common-registers ()
  "Define some common registers."
  (setq register-preview-delay 0) ;; Show registers ASAP
  (set-register ?o (cons 'file (concat emacs-config-directory "emacs-config-elpa.org")))
  (set-register ?O (cons 'file (concat emacs-config-directory "emacs-config.org")))
  (set-register ?G '(file . "~/Developer/game-dev/GB_asm"))
  (set-register ?S (cons 'file (concat emacs-config-directory "org-files/important-scripts.org"))))

;;;###autoload
(defun init-registers ()
  "Autoload function to initialize some common registers."
  (setup-common-registers))
