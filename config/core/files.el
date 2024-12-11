(use-package files
  :ensure nil
  :init
  (setq backup-directory-alist `(("." . "~/.config/emacs/tmp/backups")))
  (setq auto-save-file-name-transforms `((".*" "~/.config/emacs/tmp/auto-save/" t)))
  (setq create-lockfiles nil) ;; Disable lockfiles (#filename#)
  :config
  ;; Ensure directories exist
  (dolist (dir '("~/.config/emacs/tmp/backups"
                 "~/.config/emacs/tmp/auto-save"
                 "~/.config/emacs/tmp/undo-tree"))
    (unless (file-exists-p dir)
      (make-directory dir t))))
