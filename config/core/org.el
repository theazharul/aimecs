;;; Org config
(require 'org)

;; Disable Subscripts Globally
(setq org-use-sub-superscripts '{})

;; Enable logging when tasks are marked as done
(setq org-log-done t)

;; Start Org with indented content (better readability)
(setq org-startup-indented t)

;; Hide leading stars in headings for a cleaner view
(setq org-hide-leading-stars t)

;; Display pretty entities, e.g., Greek letters
(setq org-pretty-entities t)

;; Optional: Enable org-appear for better visibility handling
(straight-use-package 'org-appear)  ;; Install org-appear if not already done

(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'org-appear-mode))  ;; Activate org-appear mode for better visibility cycling

(use-package org-superstar
  :straight t
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("◉" "○" "✸" "✿")) ;; Customize the bullets
  (org-superstar-item-bullet-alist '((?- . "•") (?- . "➤") (?- . "‣"))) ;; Customize item bullets
  (org-hide-leading-stars t)        ;; Hide leading stars
  (org-superstar-leading-bullet ?\s)) ;; Use a space for leading stars

(my-leader-def
  "o"  '(:ignore t :which-key "org") ;; Group Org mode commands under 'o'
  "o a" 'org-agenda                ;; Open the Org agenda
  "o c" 'org-capture               ;; Capture a new entry
  "o l" 'org-store-link            ;; Store a link for later use
  "o t" 'org-todo                  ;; Change the todo state
  "o s" 'org-schedule              ;; Schedule a task
  "o d" 'org-deadline              ;; Set a deadline for a task
)
