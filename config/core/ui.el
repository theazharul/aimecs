;; Core UI Configuration

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;; Set font and theme
(set-face-attribute 'default nil :font "Source Code Pro" :height 110)
;;(load-theme 'modus-operandi t)

(provide 'ui-config)
