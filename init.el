;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Ensure lexical binding for Org Babel loading
(let ((lexical-binding t))
  (org-babel-load-file (expand-file-name "./config.org" user-emacs-directory)))

(provide 'init)
;;; init.el ends here
