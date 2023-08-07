;;; init-org-roam.el -- Notes Taking Package
;;
;;; Commentary:
;;;
;;; Org-roam is a tool for networked thought.  It reproduces some of Roam
;;; Research’s key features within Org-mode.  Org-roam allows for effortless
;;; non-hierarchical note-taking: with Org-roam, notes flow naturally, making
;;; note-taking fun and easy.  Org-roam augments the Org-mode syntax, and will
;;; work for anyone already using Org-mode for their personal wiki.  Org-roam
;;; leverages the mature ecosystem around Org-mode.  For example, it has
;;; first-class support for org-ref for citation management, and is able to
;;; piggyback off Org’s excellent LaTeX and source-block evaluation
;;; capabilities.
;;; ------------------------------------------------------------------------

;;; Code:
  
(use-package org-roam
   :ensure t
   :init
   (setq org-roam-v2-ack t)
   :custom
   (org-roam-directory (concat mrf/docs-dir "/RoamNotes"))
   (org-roam-completion-everywhere t)
   (org-roam-capture-templates
      '(("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)))
   :bind (("C-c n l" . org-roam-buffer-toggle)
            ("C-c n f" . org-roam-node-find)
            ("C-c n i" . org-roam-node-insert)
            :map org-mode-map
            ("C-M-i" . completion-at-point))
   :config
   (org-roam-setup))

(provide 'init-org-roam)

;;; init-org-roam.el ends here.

