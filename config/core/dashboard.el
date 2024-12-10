(straight-use-package 'dashboard)
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-startup-banner 'official
        dashboard-center-content t
        dashboard-items '((recents . 5)
                          (projects . 5)))
  (dashboard-setup-startup-hook))
(provide 'dashboard-config)
