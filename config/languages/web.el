;; Ensure web-mode is installed
(use-package web-mode
  :straight t
  :mode ("\\.html\\'" "\\.css\\'" "\\.js\\'" "\\.heex\\'")
  :hook
  ((web-mode . lsp-deferred) ;; Enable LSP for web-mode
   (web-mode . emmet-mode)  ;; Enable Emmet mode
   (web-mode . (lambda ()
                 ;; Format on save
                 (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
  :config
  ;; Configure web-mode indentation and settings
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-enable-auto-quoting nil ;; Disable automatic insertion of quotes
        web-mode-enable-auto-pairing t  ;; Enable auto pairing of tags
        web-mode-enable-current-column-highlight t
        web-mode-enable-current-element-highlight t)

  ;; Add prettify-symbols for web-mode
  (add-hook 'web-mode-hook
            (lambda ()
              (push '(">=" . ?\u2265) prettify-symbols-alist)
              (push '("<=" . ?\u2264) prettify-symbols-alist)
              (push '("!=" . ?\u2260) prettify-symbols-alist)
              (push '("==" . ?\u2A75) prettify-symbols-alist)
              (push '("->" . ?\u2192) prettify-symbols-alist)
              (prettify-symbols-mode 1))))

;; Install and configure emmet-mode
(use-package emmet-mode
  :straight t
  :hook ((web-mode css-mode sgml-mode) . emmet-mode) ;; Enable Emmet in web-mode, css-mode, and sgml-mode
  :config
  ;; Optional: Keybindings for Emmet
  (define-key emmet-mode-keymap (kbd "TAB") 'emmet-expand-line) ;; Bind TAB key to expand Emmet abbreviation
  (setq emmet-expand-jsx-className? t) ;; Use `className` instead of `class` for JSX
  (setq emmet-indent-after-expansion nil)) ;; Optional: Disable extra indentation after expansion

;; Provide the web-config module
(provide 'web-config)
