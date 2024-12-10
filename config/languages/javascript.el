;; JavaScript Language Configuration

(when (featurep 'lsp-config)
  (straight-use-package 'js2-mode)
  (require 'js2-mode)
  (straight-use-package 'lsp-mode)

  ;; Setup LSP for JavaScript (and TypeScript)
  (add-hook 'js2-mode-hook #'lsp)

  ;; Format on save
  (add-hook 'js2-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'lsp-format-buffer nil t))))

(provide 'javascript-config)
