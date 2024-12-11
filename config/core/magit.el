(straight-use-package 'magit)
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))
(my-leader-def
  "g g" 'magit-status)  ;; Use SPC g for Magit status

(provide 'magit-config)
