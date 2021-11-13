;; org-mode
(defvar my-org-directory "~/org/")

(setq org-directory my-org-directory)

;; Add all files in this folder to agenda
(setq org-agenda-files (list my-org-directory))

;;; initial setup stuff
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;;;; start org-mode in indent mode
;; Currently done per file as a header argument
;(setq org-startup-indented t)

;;;; automatically timestamp when TODO items set to DONE
(setq org-log-done 'time)

;; Fontofy src blocks in org-mode
(setq org-src-fontify-natively t)

;;;; Set TODO States
(setq org-todo-keywords
      '((sequence "TODO(t)"
                  "NEXT(n)"
                  "IN-WORK(i)"
                  "CRITICAL(!)"
                  "WAITING(w)"
                  "|"
                  "DONE(d)"
                  "CANCELLED(c)")))

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5)
                                 (org-agenda-files :maxlevel . 5))))


;; don't show tasks that are scheduled in the future
;; The thinking here is that the schedule is the next time we want the task
;; to appear on the TODO list, so if we want to suppress an item to the future
;; we simply schedule it for a later date
(setq org-agenda-todo-ignore-scheduled 'future)

;; Use "second" instead of "day" for time comparison.
;; It hides tasks with a scheduled time like "<2020-11-15 Sun 11:30>"
(setq org-agenda-todo-ignore-time-comparison-use-seconds t)

;; Hide deadline warning prior to scheduled date
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

;; Custom agenda view "C-c a n"
(setq org-agenda-custom-commands
   '(("n" "Agenda / CRITICAL / IN-WORK / NEXT / TODO"
      ((agenda "" ((org-agenda-span 'day)))
       (todo "CRITICAL" nil)
       (todo "IN-WORK" nil)
       (todo "NEXT" nil)
       (todo "TODO" nil))
      nil)))

;;use the emacs calendar for agenda
(setq org-agenda-include-diary t)

;; Capture templates
(setq org-capture-templates
      '(("t"
         "Place a TODO entry in the inbox"
         entry
         (file+olp "home-tasks.org" "Inbox")
         "*** TODO %?")))
