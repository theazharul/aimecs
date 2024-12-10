;; Web (HTML, CSS, JS) Language Configuration

(when (featurep 'lsp-config)
  (straight-use-package 'web-mode)
  (require 'web-mode)
  (straight-use-package 'lsp-mode)

  ;; Setup LSP for HTML, CSS, and JavaScript
  (add-hook 'web-mode-hook #'lsp)

  ;; Enable formatting for web files
  (add-hook 'web-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'lsp-format-buffer nil t))))

(provide 'web-config)
