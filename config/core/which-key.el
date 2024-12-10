(straight-use-package 'which-key)
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))
(provide 'which-key-config)
