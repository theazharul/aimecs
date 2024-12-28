;; LSP Mode Configuration 
(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")          ;; Keymap prefix for LSP commands
  (setq lsp-completion-provider :capf)      ;; Use native completion-at-point (capf) for completions
  :hook
  ((php-mode dart-mode python-mode js-mode elixir-mode) . lsp-deferred) ;; Enable LSP for specific modes
  :commands lsp lsp-deferred
  :config
  (setq lsp-enable-snippet t)               ;; Enable snippet support
  (setq lsp-enable-file-watchers t)         ;; Enable file watchers for LSP features
  (setq lsp-headerline-breadcrumb-enable t) ;; Enable breadcrumb in headerline
  (setq lsp-format-on-save t)               ;; Enable format on save
  (setq lsp-log-io nil)                     ;; Disable logging by default for better performance
  (setq lsp-idle-delay 0.500)               ;; Set idle delay for completion to 500ms
  (setq lsp-completion-use-capf t)          ;; Use native LSP completions (better with `company-mode`)
  (setq lsp-diagnostics-provider :flycheck) ;; Use Flycheck for diagnostics, improving accuracy
  (setq lsp-diagnostics-max-number 100)     ;; Limit the number of diagnostics shown
  (setq lsp-file-watch-threshold 500))      ;; Limit the number of watched files

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
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-peek-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-sideline-show-hover t))

;; Optional Completion Framework
(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.2
        company-backends '(company-capf))
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t))

;; Optional Syntax Checking with Flycheck
(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe
        flycheck-highlighting-mode 'symbols
        flycheck-check-syntax-automatically '(mode-enabled save)
        flycheck-display-errors-delay 0.3))

;; LSP Formatting on Save
(defun my-lsp-format-buffer ()
  "Format the current buffer using LSP."
  (when (and (bound-and-true-p lsp-mode)
             (lsp-feature? "textDocument/formatting"))
    (lsp-format-buffer)))

(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my-lsp-format-buffer nil t)))

;; Enable LSP logging (optional for debugging)
(setq lsp-log-io nil)
