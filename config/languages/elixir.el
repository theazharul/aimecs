;; Elixir Mode Configuration for LSP and Formatting
(use-package elixir-mode
  :straight t
  :mode ("\\.ex\\'" "\\.exs\\'")
  :hook (elixir-mode . lsp-deferred)
  :config
  ;; Ensure elixir-mode is mapped to the correct languageId
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '(elixir-mode . "elixir"))))

;; Optional: Elixir Language Server (elixir-ls) setup
(use-package lsp-elixir
  :straight t
  :after lsp-mode
  :config
  (setq lsp-elixir-dialyzer-enabled nil   ;; Disable Dialyzer for performance
        lsp-elixir-fetch-deps nil        ;; Do not fetch project dependencies
        lsp-elixir-suggest-specs t))     ;; Enable suggestions for specs

(provide 'elixir-heex-config)
