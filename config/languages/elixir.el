;; Elixir Configuration

;; Ensure LSP is loaded
(when (featurep 'lsp-config)
  (straight-use-package 'elixir-mode)
  (require 'elixir-mode)

  ;; Setup LSP for Elixir
  (add-hook 'elixir-mode-hook #'lsp)

  ;; Format on save
  (add-hook 'before-save-hook 'elixir-lsp-format-buffer))

(defun elixir-lsp-format-buffer ()
  "Run `lsp-format-buffer` if LSP is available."
  (when (eq major-mode 'elixir-mode)
    (lsp-format-buffer)))
