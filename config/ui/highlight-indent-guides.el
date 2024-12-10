(straight-use-package 'highlight-indent-guides)
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))
(provide 'highlight-indent-guides-config)
