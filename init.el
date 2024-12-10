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
(setq config-directories
      `((core . ,(expand-file-name "config/core" user-emacs-directory))
        (editing . ,(expand-file-name "config/editing" user-emacs-directory))
        (languages . ,(expand-file-name "config/languages" user-emacs-directory))
        (frameworks . ,(expand-file-name "config/frameworks" user-emacs-directory))
        (ui . ,(expand-file-name "config/ui" user-emacs-directory))
        (keybindings . ,(expand-file-name "config/keybindings" user-emacs-directory))
        (autocompletion . ,(expand-file-name "config/autocompletion" user-emacs-directory))))

;; Helper function for logging errors
(defun log-error (message &rest args)
  "Log an error MESSAGE with ARGS to *Messages* buffer."
  (message "[Error] %s" (apply 'format message args)))

;; Helper function to load files lazily
(defun lazy-load-config (file-path)
  "Lazy load a FILE-PATH if it exists."
  (if (file-exists-p file-path)
      (condition-case err
          (load file-path nil 'nomessage)
        (error (log-error "Failed to load %s: %s" file-path (error-message-string err))))
    (log-error "File not found: %s" file-path)))

;; Load multiple configurations from a category
(defun load-configs (category files)
  "Load a list of FILES lazily from a CATEGORY in `config-directories`."
  (let ((directory (cdr (assoc category config-directories))))
    (if directory
        (dolist (file files)
          (let ((file-path (expand-file-name file directory)))
            (lazy-load-config file-path)))
      (log-error "Directory not found for category: %s" category))))

;; Load configurations
(load-configs 'core
              '("lsp-installation.el"
		"lsp.el"
		"ui.el"
		"keybindings.el"
		"org.el"
                "projectile.el"
		"which-key.el"
		"hydra.el"
		"ivy.el"
		"magit.el"
                "yasnippet.el"
		"flycheck.el"
		"editorconfig.el"
		"treemacs.el"
                "dashboard.el"))

(load-configs 'editing
              '("multiple-cursors.el"
		"expand-region.el"
		"smartparens.el"
		"undo-tree.el"))

(load-configs 'keybindings
              '("leader.el"))

(load-configs 'autocompletion
              '("company.el"))

(load-configs 'languages
              '("elixir.el"
		"python.el"
		"javascript.el"
		"dart.el"
		"web.el"))

(load-configs 'frameworks
              '("flutter.el"
		"phoenix.el"
		"liveview.el"))

(load-configs 'ui
              '("doom-themes.el"
		"fonts.el"
		"all-the-icons.el"
                "rainbow-delimiters.el"
		"highlight-indent-guides.el"))


;; Set custom file for auto-generated settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
