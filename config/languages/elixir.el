;; Elixir Configuration

;; Ensure LSP is loaded
(when (featurep 'lsp-config)
  (straight-use-package 'elixir-mode)
  (require 'elixir-mode)

  ;; Setup LSP for Elixir
  (add-hook 'elixir-mode-hook #'lsp)

  ;; Format on save
  (add-hook 'elixir-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'lsp-format-buffer nil t))))
