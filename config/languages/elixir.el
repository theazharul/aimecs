;; Elixir Mode Configuration for LSP, Phoenix, and LiveView
(use-package elixir-mode
  :straight t
  :mode ("\\.ex\\'" "\\.exs\\'")
  :hook (elixir-mode . lsp-deferred)         ;; Enable LSP for Elixir mode
  :config
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '(elixir-mode . "elixir")))) ;; Ensure LSP knows it's Elixir

;; Elixir Language Server (elixir-ls) setup
(use-package lsp-elixir
  :straight t
  :after lsp-mode
  :config
  (setq lsp-elixir-dialyzer-enabled nil   ;; Disable Dialyzer for performance optimization
        lsp-elixir-fetch-deps nil        ;; Do not fetch project dependencies automatically
        lsp-elixir-suggest-specs t))     ;; Enable suggestions for Elixir specs

;; Project-specific ElixirLS Configuration
(defun my-elixir-setup ()
  "Set up ElixirLS for the current project."
  (let ((project-root (lsp-workspace-root)))
    (setq-local lsp-elixir-server-command
                (list (concat project-root "/.elixir-ls/release/language_server.sh")))))

(add-hook 'elixir-mode-hook #'my-elixir-setup)

;; Phoenix LiveView Support for .leex and .heex files
(use-package phoenix-liveview
  :straight t
  :mode ("\\.leex\\'" "\\.heex\\'")          ;; Recognize Phoenix LiveView templates
  :hook ((elixir-mode . lsp-deferred)         ;; Enable LSP for LiveView templates
         (phoenix-liveview-mode . lsp-deferred))
  :config
  (setq phoenix-liveview-enable-lsp t)       ;; Enable LSP for Phoenix LiveView templates
  (setq phoenix-liveview-flycheck-mode t))    ;; Enable Flycheck for LiveView templates

;; Format Elixir code and Phoenix templates on save
(defun my-elixir-format-buffer ()
  "Format the current Elixir buffer using LSP or `mix format`."
  (when (and (bound-and-true-p lsp-mode)
             (lsp-feature? "textDocument/formatting"))
    (lsp-format-buffer)))  ;; Format the buffer in-place

(add-hook 'elixir-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my-elixir-format-buffer nil t))) ;; Format on save

;; Syntax Checking with Flycheck for Elixir and Phoenix
(use-package flycheck
  :straight t
  :hook (elixir-mode . flycheck-mode)
  :config
  (setq flycheck-checker 'elixir-credo)           ;; Use Elixir Credo for static analysis
  (setq flycheck-indication-mode 'right-fringe    ;; Show error indicators in the right fringe
        flycheck-highlighting-mode 'symbols))      ;; Highlight syntax errors
