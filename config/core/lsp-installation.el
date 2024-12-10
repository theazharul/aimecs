;; Function to install LSP servers on demand
(defun install-lsp-server-if-needed (server-name install-command)
  "Install the LSP SERVER-NAME if it's not already installed, using INSTALL-COMMAND."
  (unless (executable-find server-name)
    (message "Installing LSP server: %s" server-name)
    (shell-command install-command)))

;; Install servers only when needed for specific languages

(defun install-pyright-if-needed ()
  "Install pyright LSP server if needed."
  (when (eq major-mode 'python-mode)
    (install-lsp-server-if-needed "pyright" "npm install -g pyright")))

(defun install-elixir-ls-if-needed ()
  "Install elixir-ls LSP server if needed."
  (when (eq major-mode 'elixir-mode)
    (install-lsp-server-if-needed "elixir-ls" "mix escript.install hex elixir_ls")))

(defun install-typescript-language-server-if-needed ()
  "Install typescript-language-server if needed."
  (when (memq major-mode '(js-mode js2-mode typescript-mode))
    (install-lsp-server-if-needed "typescript-language-server" "npm install -g typescript-language-server")))

(defun install-dart-language-server-if-needed ()
  "Install dart-language-server if needed."
  (when (eq major-mode 'dart-mode)
    (install-lsp-server-if-needed "dart-language-server" "pub global activate dart_language_server")))

;; Hook these functions to run when the corresponding file type is opened
(add-hook 'python-mode-hook 'install-pyright-if-needed)
(add-hook 'elixir-mode-hook 'install-elixir-ls-if-needed)
(add-hook 'js-mode-hook 'install-typescript-language-server-if-needed)
(add-hook 'js2-mode-hook 'install-typescript-language-server-if-needed)
(add-hook 'typescript-mode-hook 'install-typescript-language-server-if-needed)
(add-hook 'dart-mode-hook 'install-dart-language-server-if-needed)

