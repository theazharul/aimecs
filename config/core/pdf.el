(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))
(setq TeX-source-correlate-mode t
      TeX-source-correlate-start-server t)
