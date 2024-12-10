;; Dart Language Configuration

(when (featurep 'lsp-config)
  (straight-use-package 'dart-mode)
  (require 'dart-mode)

  ;; Setup LSP for Dart
  (add-hook 'dart-mode-hook #'lsp)

  ;; Format on save
  (add-hook 'dart-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'lsp-format-buffer nil t))))

(provide 'dart-config)
