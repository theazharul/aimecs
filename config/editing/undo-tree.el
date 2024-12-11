(straight-use-package 'undo-tree)

(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/tmp/undo-tree")))
  (setq undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode))

(provide 'undo-tree-config)
