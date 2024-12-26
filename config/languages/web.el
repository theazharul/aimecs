;; Ensure web-mode is installed
(use-package web-mode
  :straight t
  :mode ("\\.html\\'" "\\.css\\'" "\\.js\\'"  "\\.html\\.heex\\'")  ;; Include .html.heex extension
  :hook ((web-mode . lsp-deferred)          ;; Enable LSP for web-mode
         (web-mode . (lambda () 
                        (add-hook 'before-save-hook 'lsp-format-buffer nil t))))  ;; Enable auto-format on save
  :config
  ;; Configure LSP language ID for HEEx files
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '(web-mode . "heex")))

;; Format HEEx and HTML files on save and reload the buffer
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "heex" (or (lsp-buffer-language) ""))
                (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                (add-hook 'after-save-hook #'revert-buffer nil t)))))

;; Install and configure emmet-mode
(use-package emmet-mode
  :straight t
  :hook ((web-mode css-mode sgml-mode) . emmet-mode)  ;; Enable Emmet in web-mode, css-mode, and sgml-mode
  :config
  ;; Optional: Keybindings for Emmet
  (define-key emmet-mode-keymap (kbd "TAB") 'emmet-expand-line)  ;; Bind TAB key to expand Emmet abbreviation
  (setq emmet-indent-after-expansion nil)) ;; Optional: Disable extra indentation after expansion


(provide 'web-config)
