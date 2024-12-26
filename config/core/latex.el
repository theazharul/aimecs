(use-package tex
  :ensure auctex
  :defer t
  :config
  ;; Enable PDF mode by default
  (setq TeX-PDF-mode t)
  ;; Enable syntax highlighting in math environments
  (setq font-latex-fontify-script t)
  ;; Automatically save before compiling
  (setq TeX-save-query nil)
  ;; Use synctex for forward and reverse search
  (setq TeX-source-correlate-mode t
        TeX-source-correlate-start-server t)
  ;; Enable RefTeX for citations and labels
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; Auto-fill mode for line wrapping
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  ;; Spell checking for comments and text
  (add-hook 'LaTeX-mode-hook 'flyspell-mode))
