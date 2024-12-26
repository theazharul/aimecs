;; Base LSP Mode Configuration
(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")        ;; Keymap prefix for LSP commands
  (setq lsp-completion-provider :capf)    ;; Use native completion-at-point (capf) for completions
  :hook
  ((php-mode dart-mode python-mode elixir-mode js-mode) . lsp-deferred) ;; Enable LSP for specific modes
  :commands lsp lsp-deferred
  :config
  (setq lsp-enable-snippet nil)           ;; Disable snippet support
  (setq lsp-enable-file-watchers nil)     ;; Disable file watchers for performance
  (setq lsp-headerline-breadcrumb-enable t) ;; Enable breadcrumb in headerline
  (setq lsp-format-on-save t))            ;; Enable format on save

;; Function to format buffer and reload it
(defun my-lsp-format-and-reload-buffer ()
  "Format the current buffer using LSP and reload it."
  (when (and (bound-and-true-p lsp-mode)
             (lsp-feature? "textDocument/formatting"))
    (lsp-format-buffer)  ;; Format the buffer
    (revert-buffer t t t)))  ;; Reload buffer without confirmation

;; Add to before-save-hook
(add-hook 'before-save-hook 'my-lsp-format-and-reload-buffer)

;; Optional UI Enhancements for LSP
(use-package lsp-ui
  :straight t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t))

;; Optional Completion Framework
(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.2
        company-backends '(company-capf)))

;; Optional Syntax Checking
(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe
        flycheck-highlighting-mode 'symbols))

;; Format buffer on save
(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
