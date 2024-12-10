;; Python Language Configuration

(when (featurep 'lsp-config)
  (straight-use-package 'python-mode)
  (require 'python-mode)

  ;; Setup LSP for Python
  (add-hook 'python-mode-hook #'lsp)

  ;; Format on save
  (add-hook 'python-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'lsp-format-buffer nil t))))

(provide 'python-config)
