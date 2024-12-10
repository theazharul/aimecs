;; config/autocompletion/company.el

(straight-use-package 'company)
(require 'company)

;; Enable company mode globally
(add-hook 'after-init-hook 'global-company-mode)

;; Set some custom company options
(setq company-idle-delay 0.2)  ;; Time before suggestions pop up
(setq company-minimum-prefix-length 2)  ;; Start suggesting after typing 2 characters

;; Enable company-mode in specific major modes, such as programming languages
(add-hook 'prog-mode-hook 'company-mode)  ;; Enable in programming modes
