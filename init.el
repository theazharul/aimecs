;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for all packages
(setq straight-use-package-by-default t)

;; Make sure org is installed from straight.el
(straight-use-package 'org)

;; Load org after installing the latest version from straight.el
(with-eval-after-load 'org
  (require 'org))

;; Initialize Evil mode with use-package
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)   ;; Optional, defaults to t
  (setq evil-want-keybinding nil)  ;; Set to nil before loading evil
  :config
  (evil-mode 1))

;; Install and configure evil-collection
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))


;; Define configuration directories

(defvar core-config-dir (expand-file-name "config/core" user-emacs-directory))
(defvar editing-config-dir (expand-file-name "config/editing" user-emacs-directory))
(defvar languages-config-dir (expand-file-name "config/languages" user-emacs-directory))
(defvar frameworks-config-dir (expand-file-name "config/frameworks" user-emacs-directory))
(defvar ui-config-dir (expand-file-name "config/ui" user-emacs-directory))
(defvar keybindings-config-dir (expand-file-name "config/keybindings" user-emacs-directory))
(defvar autocompletion-config-dir (expand-file-name "config/autocompletion" user-emacs-directory))


;; Helper function to load a specific configuration file
(defun load-config (directory file)
  "Load a configuration FILE from DIRECTORY if it exists."
  (let ((file-path (expand-file-name file directory)))
    (when (file-exists-p file-path)
      (load file-path))))

;; Load core configuration files
(load-config core-config-dir "lsp-installation.el")  ;; Install LSP servers only when needed

;; Explicitly load core configuration files
(load-config core-config-dir "lsp.el")
(load-config core-config-dir "ui.el")
(load-config core-config-dir "keybindings.el")
(load-config core-config-dir "org.el")
(load-config core-config-dir "projectile.el")
(load-config core-config-dir "which-key.el")
(load-config core-config-dir "hydra.el")
(load-config core-config-dir "ivy.el")
(load-config core-config-dir "magit.el")
(load-config core-config-dir "yasnippet.el")
(load-config core-config-dir "flycheck.el")
(load-config core-config-dir "editorconfig.el")
(load-config core-config-dir "treemacs.el")
(load-config core-config-dir "dashboard.el")

;; Load Editing Configs
(load-config editing-config-dir "multiple-cursors.el")
(load-config editing-config-dir "expand-region.el")
(load-config editing-config-dir "smartparens.el")
(load-config editing-config-dir "undo-tree.el")

;; Load UI and keybinding configurations
(load-config keybindings-config-dir "leader.el")  ;; for leader key setup

;; Load autocompletion configuration
(load-config autocompletion-config-dir "company.el")  ;; for company-mode


;; Explicitly load language-specific configuration files
(load-config languages-config-dir "elixir.el")
(load-config languages-config-dir "python.el")
(load-config languages-config-dir "javascript.el")
(load-config languages-config-dir "dart.el")
(load-config languages-config-dir "web.el")  ;; HTML, CSS, JS

;; Explicitly load framework-specific configuration files
(load-config frameworks-config-dir "flutter.el")
(load-config frameworks-config-dir "phoenix.el")
(load-config frameworks-config-dir "liveview.el")

;; Load UI Configs
(load-config ui-config-dir "doom-themes.el")
(load-config ui-config-dir "fonts.el")
(load-config ui-config-dir "all-the-icons.el")
(load-config ui-config-dir "rainbow-delimiters.el")
(load-config ui-config-dir "highlight-indent-guides.el")


;; Set custom file for auto-generated settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
