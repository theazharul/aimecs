;; Non-keybindings general settings
(use-package emacs
  :config
  (setq display-line-numbers-type 't) ;; or 'relative
  (global-display-line-numbers-mode 1)
  (global-visual-line-mode 1)

  ;; Disable in specific modes
  (dolist (mode '(org-mode-hook
                  eshell-mode-hook
                  term-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))
