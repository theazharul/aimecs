;; Ensure Projectile is installed
(straight-use-package 'projectile)

;; Projectile Configuration
(use-package projectile
  :ensure t
  :init
  ;; Enable caching for faster project navigation
  (setq projectile-enable-caching t)

  ;; Set the default search path for projects
  (setq projectile-project-search-path '("~/projects/" "~/Workspace/"))

  ;; Automatically switch to project directory view
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  ;; Enable Projectile globally
  (projectile-mode +1)

  ;; Keybindings
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  ;; Optional: Integrate with Ivy for better completion
  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-mode 1)))

(provide 'projectile-config)
