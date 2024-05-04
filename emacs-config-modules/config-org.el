;;; --------------------------------------------------------------------------

(defun mrf/org-theme-override-values ()
    (defface org-block-begin-line
        '((t (:underline "#1D2C39" :foreground "SlateGray" :background "#1D2C39")))
        "Face used for the line delimiting the begin of source blocks.")

    (defface org-block
        '((t (:background "#242635" :extend t)))
        "Face used for the source block background.")

    (defface org-block-end-line
        '((t (:overline "#1D2C39" :foreground "SlateGray" :background "#1D2C39")))
        "Face used for the line delimiting the end of source blocks.")
    
    (defface org-modern-horizontal-rule
        '((t (:strike-through "green" :weight bold)))
        "Face used for the Horizontal like (-----)"))

;;; --------------------------------------------------------------------------

(defun mrf/org-font-setup ()
    "Setup org mode fonts."
    (use-package org-faces
	:when (featurep 'org)
	:config
        (font-lock-add-keywords
            'org-mode
            '(("^ *\\([-]\\) "
                  (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
        (dolist (face '((org-level-1 . 1.75)
                           (org-level-2 . 1.5)
                           (org-level-3 . 1.25)
                           (org-level-4 . 1.1)
                           (org-level-5 . 1.1)
                           (org-level-6 . 1.1)
                           (org-level-7 . 1.1)
                           (org-level-8 . 1.1)))
            (set-face-attribute (car face) nil :font "ETBembo" :weight 'regular :height (cdr face)))
	
        ;; Ensure that anything that should be fixed-pitch in Org files appears that way
        (set-face-attribute 'org-block nil    :foreground 'unspecified :inherit 'fixed-pitch)
        (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
        (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
        (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
        (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
        (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
        (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
        (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
        (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
        (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
        (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)))

;; -----------------------------------------------------------------

(defun mrf/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (visual-line-mode 1)
    (setq org-ellipsis " ▾")
    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    ;; (use-package org-habit)
    ;; (add-to-list 'org-modules 'org-habit)
    ;; (setq org-habit-graph-column 60)
    (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
             (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)"
                 "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
    (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
             ("Tasks.org" :maxlevel . 1))))

;;; --------------------------------------------------------------------------

(defun mrf/org-setup-agenda ()
    (setq org-agenda-custom-commands
        '(("d" "Dashboard"
              ((agenda "" ((org-deadline-warning-days 7)))
                  (todo "NEXT"
                      ((org-agenda-overriding-header "Next Tasks")))
                  (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

             ("n" "Next Tasks"
                 ((todo "NEXT"
                      ((org-agenda-overriding-header "Next Tasks")))))

             ("W" "Work Tasks" tags-todo "+work-email")

             ;; Low-effort next actions
             ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
                 ((org-agenda-overriding-header "Low Effort Tasks")
                     (org-agenda-max-todos 20)
                     (org-agenda-files org-agenda-files)))

             ("w" "Workflow Status"
                 ((todo "WAIT"
                      ((org-agenda-overriding-header "Waiting on External")
                          (org-agenda-files org-agenda-files)))
                     (todo "REVIEW"
                         ((org-agenda-overriding-header "In Review")
                             (org-agenda-files org-agenda-files)))
                     (todo "PLAN"
                         ((org-agenda-overriding-header "In Planning")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-files org-agenda-files)))
                     (todo "BACKLOG"
                         ((org-agenda-overriding-header "Project Backlog")
                             (org-agenda-todo-list-sublevels nil)
                             (org-agenda-files org-agenda-files)))
                     (todo "READY"
                         ((org-agenda-overriding-header "Ready for Work")
                             (org-agenda-files org-agenda-files)))
                     (todo "ACTIVE"
                         ((org-agenda-overriding-header "Active Projects")
                             (org-agenda-files org-agenda-files)))
                     (todo "COMPLETED"
                         ((org-agenda-overriding-header "Completed Projects")
                             (org-agenda-files org-agenda-files)))
                     (todo "CANC"
                         ((org-agenda-overriding-header "Cancelled Projects")
                             (org-agenda-files org-agenda-files)))))))
    ) ;; mrf/org-setup-agenda

;;; --------------------------------------------------------------------------

(defun mrf/org-setup-capture-templates ()
    (setq org-capture-templates
        `(("t" "Tasks / Projects")
             ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
                 "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

             ("j" "Journal Entries")
             ("jj" "Journal" entry
                 (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
                 "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
                 ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
                 :clock-in :clock-resume
                 :empty-lines 1)
             ("jm" "Meeting" entry
                 (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
                 "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
                 :clock-in :clock-resume
                 :empty-lines 1)

             ("w" "Workflows")
             ("we" "Checking Email" entry (file+olp+datetree
                                              "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
                 "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

             ("m" "Metrics Capture")
             ("mw" "Weight" table-line (file+headline
                                           "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org"
                                           "Weight")
                 "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t))))

;;; --------------------------------------------------------------------------

(mrf/org-theme-override-values)

(use-package org
    :defer t
    :commands (org-capture org-agenda)
    :hook (org-mode . mrf/org-mode-setup)
    :bind (:map org-mode-map
              ("C-c e" . org-edit-src-code))
    :config
    (message ">>> Loading orgmode")
    (setq org-hide-emphasis-markers nil)
    ;; Save Org buffers after refiling!
    (advice-add 'org-refile :after 'org-save-all-org-buffers)
    (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
             (:endgroup)
             ("@errand" . ?E)
             ("@home" . ?H)
             ("@work" . ?W)
             ("agenda" . ?a)
             ("planning" . ?p)
             ("publish" . ?P)
             ("batch" . ?b)
             ("note" . ?n)
             ("idea" . ?i)))
    ;; Configure custom agenda views
    (mrf/org-setup-agenda)
    (mrf/org-setup-capture-templates)
    (yas-global-mode t)
    (define-key global-map (kbd "C-c j")
        (lambda () (interactive) (org-capture nil "jj")))
    (mrf/org-font-setup))

;;; --------------------------------------------------------------------------

(use-package org-modern
    :after org
    :hook (org-mode . org-modern-mode)
    :config
    ;; Add frame borders and window dividers
    (modify-all-frames-parameters
      '((right-divider-width . 40)
             (internal-border-width . 40)))
    (dolist (face '(window-divider
                       window-divider-first-pixel
                       window-divider-last-pixel))
      (face-spec-reset-face face)
      (set-face-foreground face (face-attribute 'default :background)))
    (set-face-background 'fringe (face-attribute 'default :background))
    (setq
      ;; Edit settings
      org-auto-align-tags nil
      org-tags-column 0
      org-catch-invisible-edits 'show-and-error
      org-special-ctrl-a/e t
      org-insert-heading-respect-content t

      ;; Org styling, hide markup etc.
      org-hide-emphasis-markers nil
      org-pretty-entities t
      org-ellipsis "…"

      ;; Agenda styling
      org-agenda-tags-column 0
      org-agenda-block-separator ?─
      org-agenda-time-grid
      '((daily today require-timed)
             (800 1000 1200 1400 1600 1800 2000)
             " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
      org-agenda-current-time-string
      "◀── now ─────────────────────────────────────────────────")
    (global-org-modern-mode))

;;; --------------------------------------------------------------------------

(use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;;; --------------------------------------------------------------------------

(defun mrf/org-mode-visual-fill ()
    (setq visual-fill-column-width custom-org-fill-column
        visual-fill-column-center-text enable-org-fill-column-centering)
    (visual-fill-column-mode 1))

(use-package visual-fill-column
    :hook (org-mode . mrf/org-mode-visual-fill))

;;; --------------------------------------------------------------------------

(with-eval-after-load 'org    
    (org-babel-do-load-languages
        'org-babel-load-languages
        '((emacs-lisp . t)
             (js . t)
             (shell . t)
             (python . t)))

    (push '("conf-unix" . conf-unix) org-src-lang-modes))

;;; --------------------------------------------------------------------------

(with-eval-after-load 'org
    ;; This is needed as of Org 9.2

    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python")))

;;; --------------------------------------------------------------------------
;; Automatically tangle our Configure.org config file when we save it
;; Org files that should use this need to add a '#+auto_tangle: t'
;; in the org file.
(use-package org-auto-tangle
    :disabled
    :defer t
    :after org
    :hook (org-mode . org-auto-tangle-mode))

;;; --------------------------------------------------------------------------

(with-eval-after-load 'org
    (require 'ox-gfm nil t))

(provide 'config-org)
;;; config-org.el ends here.
