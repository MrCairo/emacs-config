;;; --------------------------------------------------------------------------
;; (use-package emacsql)
;; (use-package emacsql-sqlite)

(use-package org-roam
    ;; :demand t  ;; Ensure org-roam is loaded by default
    :init
    (setq org-roam-v2-ack t)
    :after org
    :custom
    (org-roam-directory (expand-file-name "RoamNotes" custom-docs-dir))
    (org-roam-completion-everywhere t)
    :bind (("C-c n l" . org-roam-buffer-toggle)
              ("C-c n f" . org-roam-node-find)
              ("C-c n i" . org-roam-node-insert)
              ("C-c n I" . org-roam-node-insert-immediate)
              ("C-c n p" . my/org-roam-find-project)
              ("C-c n t" . my/org-roam-capture-task)
              ("C-c n b" . my/org-roam-capture-inbox)
              :map org-mode-map
              ("C-M-i" . completion-at-point)
              :map org-roam-dailies-map
              ("Y" . org-roam-dailies-capture-yesterday)
              ("T" . org-roam-dailies-capture-tomorrow))
    :bind-keymap
    ("C-c n d" . org-roam-dailies-map)
    :config
    (require 'org-roam-dailies) ;; Ensure the keymap is available
    (my/org-roam-refresh-agenda-list)
    (add-to-list 'org-after-todo-state-change-hook
        (lambda ()
            (when (equal org-state "DONE")
                (my/org-roam-copy-todo-to-today))))
    (org-roam-db-autosync-mode))

(defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (push arg args))
             (org-roam-capture-templates
                 (list (append (car org-roam-capture-templates)
                           '(:immediate-finish t)))))
        (apply #'org-roam-node-insert args)))

;;; --------------------------------------------------------------------------
;; The buffer you put this code in must have lexical-binding set to t!
;; See the final configuration at the end for more details.

(defun my/org-roam-filter-by-tag (tag-name)
    (lambda (node)
        (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
    (mapcar #'org-roam-node-file
        (seq-filter
            (my/org-roam-filter-by-tag tag-name)
            (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
    (interactive)
    (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

;; Build the agenda list the first time for the session

;;; --------------------------------------------------------------------------

(defun my/org-roam-project-finalize-hook ()
    "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
    ;; Remove the hook since it was added temporarily
    (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Add project file to the agenda list if the capture was confirmed
    (unless org-note-abort
        (with-current-buffer (org-capture-get :buffer)
            (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-find-project ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
        nil
        nil
        (my/org-roam-filter-by-tag "Project")
        :templates
        '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
              :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
              :unnarrowed t))))

(global-set-key (kbd "C-c n p") #'my/org-roam-find-project)

;;; --------------------------------------------------------------------------

(defun my/org-roam-capture-inbox ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create)
        :templates '(("i" "inbox" plain "* %?"
                         :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

;;; --------------------------------------------------------------------------

(defun my/org-roam-capture-task ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Capture the new task, creating the project file if necessary
    (org-roam-capture- :node (org-roam-node-read nil
                                 (my/org-roam-filter-by-tag "Project"))
        :templates '(("p" "project" plain "** TODO %?"
                         :if-new
                         (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                             ("Tasks"))))))

;;; --------------------------------------------------------------------------

(defun my/org-roam-copy-todo-to-today ()
    (interactive)
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
             (org-roam-dailies-capture-templates
                 '(("t" "tasks" entry "%?"
                       :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
             (org-after-refile-insert-hook #'save-buffer)
             today-file pos)
        (save-window-excursion
            (org-roam-dailies--capture (current-time) t)
            (setq today-file (buffer-file-name))
            (setq pos (point)))

        ;; Only refile if the target file is different than the current file
        (unless (equal (file-truename today-file)
                    (file-truename (buffer-file-name)))
            (org-refile nil nil (list "Tasks" today-file nil pos)))))

(provide 'config-org-roam)
;;; config-org-roam.el ends here.
