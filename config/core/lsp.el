;; Base LSP Mode Configuration
(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")        ;; Keymap prefix for LSP commands
  :hook
  ((prog-mode . lsp-deferred))            ;; Enable LSP for programming modes
  :commands lsp lsp-deferred
  :config
  (setq lsp-enable-snippet nil)           ;; Disable snippet support
  (setq lsp-enable-file-watchers nil)     ;; Disable file watchers for performance
  (setq lsp-headerline-breadcrumb-enable t)) ;; Enable breadcrumb in headerline

;; Optional UI Enhancements for LSP
(use-package lsp-ui
  :straight t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)             ;; Enable inline documentation
  (setq lsp-ui-doc-delay 0.5)            ;; Delay before showing documentation
  (setq lsp-ui-doc-position 'at-point)   ;; Show documentation near the cursor
  (setq lsp-ui-sideline-enable t)        ;; Enable sideline diagnostics
  (setq lsp-ui-sideline-show-diagnostics t))

;; Optional Completion Framework
(use-package company
  :straight t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1) ;; Show suggestions quickly
  (setq company-idle-delay 0.0))         ;; No delay for completions

;; Optional Syntax Checking
(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-highlighting-mode 'symbols))

(provide 'lsp-config)
