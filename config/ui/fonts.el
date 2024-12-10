(defun my/create-directory-if-needed (dir)
  "Create directory DIR if it doesn't exist."
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defun my/download-font (url font-name install-dir)
  "Download and install a font from URL, save it to INSTALL-DIR."
  (my/create-directory-if-needed install-dir)  ;; Ensure the directory exists
  (let ((font-file (expand-file-name (concat font-name ".ttf") install-dir)))
    (unless (file-exists-p font-file)
      (url-copy-file url font-file t)
      (message "Downloaded font: %s" font-file))
    font-file))

(defun my/install-font (font-file font-name)
  "Install a font from FONT-FILE and apply it to Emacs."
  (when (file-exists-p font-file)
    (message "Installing font %s..." font-name)
    (set-fontset-font t 'unicode font-file nil 'prepend)
    (set-face-attribute 'default nil :font font-name :height 150)
    (message "Font %s installed and applied." font-name)))

(defun my/load-source-code-pro-font ()
  "Download and apply Source Code Pro font."
  (let* ((font-name "Source Code Pro")
         (font-url "https://github.com/adobe-fonts/source-code-pro/releases/download/variable-fonts/SourceCodePro-VariableFont_wght.ttf")
         (font-install-dir (expand-file-name "assets/fonts" user-emacs-directory))  ;; Use the assets folder
         (font-file (my/download-font font-url font-name font-install-dir)))
    (my/install-font font-file font-name)))

;; Call this function to download and install the font
(my/load-source-code-pro-font)
