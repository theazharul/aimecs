;; Bootstrap straight.el
(setq bootstrap-version 6)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-url "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously bootstrap-url 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el by default for all packages
(setq straight-use-package-by-default t)

;; Ensure essential packages
(straight-use-package 'use-package) ;; Ensure `use-package` is installed
(straight-use-package 'org)        ;; Ensure the latest version of Org mode

;; Load keybindings before evil
(load (expand-file-name "config/core/keybindings.el" user-emacs-directory))


;; Initialize Evil mode
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil) ;; Set before loading Evil
  :config
  (evil-mode 1))

;; Configure Evil Collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Define configuration directories
(setq config-directories
      `((core . ,(expand-file-name "config/core" user-emacs-directory))
        (editing . ,(expand-file-name "config/editing" user-emacs-directory))
        (languages . ,(expand-file-name "config/languages" user-emacs-directory))
        (frameworks . ,(expand-file-name "config/frameworks" user-emacs-directory))
        (ui . ,(expand-file-name "config/ui" user-emacs-directory))
        (autocompletion . ,(expand-file-name "config/autocompletion" user-emacs-directory))))

;; Helper: Log errors
(defun log-error (message &rest args)
  "Log an error MESSAGE with ARGS to *Messages* buffer."
  (message "[Error] %s" (apply 'format message args)))

;; Helper: Load a file lazily
(defun lazy-load-config (file-path)
  "Lazy load a FILE-PATH if it exists."
  (if (file-exists-p file-path)
      (condition-case err
          (load file-path nil 'nomessage)
        (error (log-error "Failed to load %s: %s" file-path (error-message-string err))))
    (log-error "File not found: %s" file-path)))

;; Helper: Load multiple configurations
(defun load-configs (category files)
  "Load a list of FILES from CATEGORY in `config-directories`."
  (let ((directory (cdr (assoc category config-directories))))
    (if directory
        (dolist (file files)
          (lazy-load-config (expand-file-name file directory)))
      (log-error "Directory not found for category: %s" category))))

;; Load configurations by category
(mapc (lambda (entry)
        (apply #'load-configs entry))
      '((core
	 ("lsp-installation.el"
	  "lsp.el"
	  "files.el"
	  "ui.el"
          "org.el"
	  "pdf.el"
	  "projectile.el"
	  "which-key.el"
	  "hydra.el"
          "ivy.el"
	  "latex.el"
	  "magit.el"
	  "yasnippet.el"
	  "flycheck.el"
          "editorconfig.el"
	  "settings.el"
	  "treemacs.el"
	  "dashboard.el"))

        (editing
	 ("multiple-cursors.el"
	  "expand-region.el"
	  "smartparens.el"
	  "undo-tree.el"))


        (autocompletion
	 ("company.el"))

        (languages
	 ("web.el"
	  "elixir.el"
	  "python.el"
	  "javascript.el"
	  "dart.el"
	  ))

        (frameworks
	 ("flutter.el"
	  "phoenix.el"
	  "liveview.el"))

        (ui
	 ("doom-themes.el"
	  "fonts.el"
	  "all-the-icons.el"
          "rainbow-delimiters.el"
	  "highlight-indent-guides.el"))))

;; Load custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
