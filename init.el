;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
  ;; Use SSH and shallow clones for faster cloning
  (setq straight-vc-git-default-protocol 'ssh)
  (setq straight-vc-git-default-clone-depth 1)
  (setq straight-repository-branch "develop")

  ;; Bootstrap straight.el
  (setq bootstrap-version 6)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-url
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el")) ;; still HTTPS, for install
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously bootstrap-url 'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; Ensure use-package is installed and integrated with straight.el
  (straight-use-package 'use-package)

  ;; (Optional) Always use straight.el with use-package
  (setq straight-use-package-by-default t)
  (setq use-package-always-ensure t)
  (setq native-comp-async-report-warnings-errors 'silent)

;; Install and load Org before anything else
(straight-use-package 'org)

;; Ensure lexical binding for Org Babel loading
(let ((lexical-binding t))
  (org-babel-load-file (expand-file-name "./config.org" user-emacs-directory)))

(provide 'init)
;;; init.el ends here
