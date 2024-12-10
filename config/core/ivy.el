(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'swiper)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "))
(use-package counsel
  :after ivy
  :config
  (counsel-mode 1))
(use-package swiper
  :after ivy
  :bind ("C-s" . swiper))
(provide 'ivy-config)
