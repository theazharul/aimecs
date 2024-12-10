;; Core LSP Configuration using use-package

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")         ;; Keymap prefix for LSP commands
  (setq lsp-enable-snippet nil)            ;; Disable snippet support
  (setq lsp-enable-file-watchers nil)      ;; Disable file watchers for performance
  (setq lsp-eldoc-render-all t)
  (setq lsp-ui-doc-enable nil) ;; Turn off LSP UI doc popups

  :hook
  ((prog-mode . lsp-deferred)              ;; Enable LSP for programming modes
   (lsp-mode . lsp-ui-mode)                ;; Enable LSP UI features
   (lsp-mode . company-mode)               ;; Enable company for auto-completion
   (lsp-mode . flycheck-mode))             ;; Enable flycheck for on-the-fly linting
  :commands lsp lsp-deferred
  :config
  (defun my/lsp-disable-in-elisp ()
    "Disable LSP in Emacs Lisp mode."
    (when (derived-mode-p 'emacs-lisp-mode)
      (lsp-mode -1)))

  ;; Add the hook to disable LSP for Emacs Lisp mode
  (add-hook 'lsp-mode-hook #'my/lsp-disable-in-elisp))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :init
  (setq lsp-ui-sideline-show-diagnostics t) ;; Show diagnostics in the sideline
  (setq lsp-ui-doc-show-with-cursor t)      ;; Show documentation popup on cursor hover
  (setq lsp-ui-doc-delay 0.5)               ;; Delay before showing documentation
  (setq lsp-ui-doc-position 'at-point)      ;; Display documentation near the cursor
  :bind
  (("C-c d" . lsp-ui-doc-show)              ;; Keybinding to manually show doc popup
   ("C-c h" . lsp-ui-doc-hide)))            ;; Keybinding to hide doc popup

(use-package company
  :straight t
  :hook (prog-mode . company-mode)          ;; Enable company globally for programming modes
  :init
  (setq company-minimum-prefix-length 1)    ;; Show completions after 1 character
  (setq company-idle-delay 0.0))            ;; No delay for completions

(use-package flycheck
  :straight t
  :hook (prog-mode . flycheck-mode)         ;; Enable flycheck globally for programming modes
  :init
  (setq flycheck-indication-mode 'right-fringe) ;; Display errors on the right fringe
  (setq flycheck-highlighting-mode 'symbols))   ;; Highlight errors with symbols

(provide 'lsp-config)
