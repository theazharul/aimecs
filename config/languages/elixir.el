;; Elixir Mode Configuration for LSP and Phoenix LiveView
(use-package elixir-mode
  :straight t
  :mode ("\\.ex\\'" "\\.exs\\'" "\\.html\\.heex\\'" "\\.heex\\'")
  :hook
  ((elixir-mode . lsp-deferred)  ;; Enable LSP for Elixir
   (elixir-mode . emmet-mode)   ;; Enable Emmet mode
   (elixir-mode . (lambda ()    ;; Prettify symbols
                    (setq prettify-symbols-alist
                          '((">=" . ?\u2265) ("<=" . ?\u2264)
                            ("!=" . ?\u2260) ("==" . ?\u2A75)
                            ("=~" . ?\u2245) ("<-" . ?\u2190)
                            ("->" . ?\u2192) ("|>" . ?\u25B7)))
                    (prettify-symbols-mode 1))))
  :config
  ;; Register .heex files as Elixir for LSP
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '(elixir-mode . "elixir"))
    (add-to-list 'lsp-language-id-configuration '(web-mode . "html"))))

;; LSP Configuration for Elixir Language Server
(use-package lsp-elixir
  :straight t
  :after lsp-mode
  :config
  (setq lsp-elixir-dialyzer-enabled nil
        lsp-elixir-fetch-deps nil
        lsp-elixir-suggest-specs t))

;; Phoenix LiveView Configuration
(use-package phoenix-liveview
  :straight t
  :mode ("\\.leex\\'" "\\.heex\\'")
  :hook (phoenix-liveview-mode . lsp-deferred)
  :config
  (setq phoenix-liveview-enable-lsp t
        phoenix-liveview-flycheck-mode t))

;; Formatting Elixir Code on Save
(defun my-elixir-format-buffer ()
  "Format the current Elixir buffer."
  (when (and (bound-and-true-p lsp-mode)
             (lsp-feature? "textDocument/formatting"))
    (lsp-format-buffer)))
(add-hook 'elixir-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my-elixir-format-buffer nil t)))

;; Polymode for Elixir Templates with ~H
(use-package polymode
  :straight t
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
  (define-innermode poly-liveview-elixir-innermode
    :mode 'web-mode
    :head-matcher (rx line-start (* space) "~H" (= 3 (char "\"'")) line-end)
    :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
    :head-mode 'host
    :tail-mode 'host
    :allow-nested nil
    :keep-in-mode 'host
    :fallback-mode 'host)
  (define-polymode poly-elixir-web-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-liveview-elixir-innermode)))

;; Flycheck for Elixir
(use-package flycheck
  :straight t
  :hook (elixir-mode . flycheck-mode)
  :config
  (setq flycheck-checker 'elixir-credo
        flycheck-indication-mode 'right-fringe
        flycheck-highlighting-mode 'symbols))
