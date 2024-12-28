(require 'org)

(with-eval-after-load 'org
  ;; General Org settings for better indentation and alignment
  (setq org-use-sub-superscripts '{}'                 ;; Disable subscripts globally
        org-log-done t                               ;; Log completion of tasks
        org-startup-indented t                        ;; Start Org with indented content (should help with item alignment)
        org-hide-leading-stars t                      ;; Hide leading stars in headings
        org-pretty-entities t                         ;; Display pretty entities (e.g., Greek letters)
        org-directory "~/Dropbox/aimacs/aimorg"       ;; Org directory
        org-mobile-directory "~/Dropbox/aimacs/aimorg"
        org-src-fontify-natively t                    ;; Syntax highlighting in source blocks
        org-src-tab-acts-natively t                   ;; TAB acts natively in source blocks
        org-src-window-setup 'current-window          ;; Use current window for editing source blocks
        org-agenda-start-on-weekday 5                 ;; Start agenda on Friday
        org-default-notes-file (concat org-directory "/0.Inbox.org") ;; Default notes file
        org-special-ctrl-a/e t                        ;; Enable special C-a and C-e behavior
        org-agenda-files (append
                          (directory-files-recursively "~/Dropbox/aimacs/aimorg/" "\\.org$")
                          (directory-files-recursively "~/Workspace/" "\\.org$"))
        org-agenda-files (remove "~/Dropbox/aimacs/aimorg/4.Archives.org" org-agenda-files)
        org-todo-keywords '((sequence "TODO(t)" "IN_PROGRESS(i)" "IN_REVIEW(r)" "|" "DONE(d)")
                           (sequence "NEXT(n)" "WAITING(w@/)" "DELEGATED(D)" "HOLD(h@/)" "|" "CANCELLED(c@/)"))
        org-global-properties '(("Effort_ALL" . "0:10 0:15 0:20 0:30 1:00 2:00 3:00 4:00 6:00 8:00"))
        org-columns-default-format "%50ITEM(Task) %TODO %TAGS %SCHEDULED %DEADLINE %Effort(Estimated Effort){:} %CLOCKSUM"
        create-lockfiles nil
        org-archive-location "~/Dropbox/aimacs/aimorg/4.Archives.org::* From %s"
        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-capture-templates
        '(("i" "Inbox" entry (file+headline "~/Dropbox/aimacs/aimorg/0.Inbox.org" "Inbox")
           "* %? \n"))
        org-agenda-window-setup 'current-window)        ;; Open agenda in current window

;; Org-appear for better visibility handling
(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)) ;; Ensure autolinks appear properly

;; Org-superstar for improved aesthetics and indentation
(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :custom
  ;; Headline bullets
  (org-superstar-headline-bullets-list '("◉" "○" "✸" "✿"))
  ;; Item bullets (ensure proper alignment)
  (org-superstar-item-bullet-alist '((?- . "•") (?- . "➤") (?- . "‣")))
  ;; Aligning the items and headings
  (org-superstar-heading-align t)
  (org-superstar-item-align t)         ;; Ensure items align under the heading
  (org-superstar-leading-bullet ?\s)   ;; Use space for leading bullet
  (org-hide-leading-stars t)           ;; Hide leading stars
  (org-superstar-pretty-lists t)       ;; Pretty lists (with custom bullets)
  ;; Aligning bullets with text for consistency
  (org-superstar-align (quote left))
  ;; Indentation for proper alignment of subheadings and items
  (org-startup-indented t))            ;; Make sure everything aligns properly

;; Keybindings for Org mode
(my-leader-def
  "o"  '(:ignore t :which-key "Org")  ;; Group Org mode commands under 'o'
  "o a" 'org-agenda                   ;; Open Org agenda
  "o c" 'org-capture                  ;; Capture a new entry
  "o l" 'org-store-link               ;; Store a link for later use
  "o t" 'org-todo                     ;; Change the todo state
  "o s" 'org-schedule                 ;; Schedule a task
  "o d" 'org-deadline                 ;; Set a deadline for a task
)

(provide 'org-config)
