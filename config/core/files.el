(use-package files
  :ensure nil ;; Since 'files' is a built-in package, we don't install it
  :init
  ;; Set backup directory
  (setq backup-directory-alist `(("." . "~/.config/emacs/tmp/backups")))
  ;; Set auto-save directory
  (setq auto-save-file-name-transforms `((".*" "~/.config/emacs/tmp/auto-save/" t)))
  ;; Disable lockfiles (#filename#)
  (setq create-lockfiles nil)
  :config
  ;; Ensure directories exist
  (dolist (dir '("~/.config/emacs/tmp/backups"
                 "~/.config/emacs/tmp/auto-save"
                 "~/.config/emacs/tmp/undo-tree"))
    (unless (file-exists-p dir)
      (make-directory dir t))))
