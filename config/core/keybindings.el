;; Core Keybindings Configuration

;; General keybindings
(use-package general
  :ensure t
  :config
  (general-create-definer my-leader-def
    :keymaps 'normal  ;; Normal mode for Evil
    :prefix "SPC"    ;; Leader key is SPC
    :global-prefix "M-m"))  ;; Global prefix (Alt+m) if you want to use it outside Evil


;; File-related operations under 'f'
(my-leader-def
  "f"  '(:ignore t :which-key "file") ;; Group file operations under 'f'
  "f s" 'save-buffer          ;; Save the current buffer
  "f S" 'save-some-buffers    ;; Save all buffers
  "f f" 'find-file            ;; Find a file
  "f r" 'recentf-open-files   ;; Open recent files
  "f d" 'dired                ;; Open dired (directory manager)
  "f n" 'make-frame-command   ;; Create a new frame (optional)
  "f o" 'find-file-other-window) ;; Open file in another window

;; Buffer-related operations under 'b'
(my-leader-def
  "b"  '(:ignore t :which-key "buffer") ;; Group buffer operations under 'b'
  "b b" 'switch-to-buffer        ;; Switch to an existing buffer
  "b k" 'kill-buffer             ;; Kill the current buffer
  "b K" 'kill-buffer-and-window  ;; Kill the buffer and its window
  "b n" 'next-buffer             ;; Switch to the next buffer
  "b p" 'previous-buffer        ;; Switch to the previous buffer
  "b d" 'display-buffer         ;; Display buffer in another window
  "b r" 'rename-buffer          ;; Rename the current buffer
  "b l" 'list-buffers           ;; List all open buffers
  "b m" 'switch-to-buffer      ;; Interactive buffer switch

  ;; More buffer-related operations can be added here
  )

;; Window management (SPC w)
(my-leader-def
  "w"  '(:ignore t :which-key "window") ;; Group window management under 'w'
  "w k" 'windmove-up        ;; Navigate to the window above
  "w j" 'windmove-down      ;; Navigate to the window below
  "w h" 'windmove-left      ;; Navigate to the window on the left
  "w l" 'windmove-right     ;; Navigate to the window on the right
  "w s" 'split-window-below    ;; Split the window horizontally
  "w v" 'split-window-right    ;; Split the window vertically
  "w w" 'other-window          ;; Switch to the other window
  "w d" 'delete-window         ;; Delete the current window
  "w =" 'balance-windows       ;; Balance window sizes
  "w m" 'delete-other-windows  ;; Maximize the current window
  "w x" 'winner-undo           ;; Undo window configuration changes
  "w X" 'winner-redo           ;; Redo window configuration changes
  )

;; Search commands (SPC s)
(my-leader-def
  "s"  '(:ignore t :which-key "search") ;; Group search commands under 's'
  "s f" 'swiper                ;; Search with swiper (interactive search)
  "s r" 'replace-string        ;; Replace a string
  "s p" 'projectile-ag         ;; Search within the project with ag (requires Projectile)
  "s b" 'consult-buffer        ;; Search buffers with consult (if installed)
  "s t" 'consult-theme         ;; Search and switch themes with consult (if installed)

;; More custom commands can be added here, depending on your needs
)

;; Enable recentf for recent file operations
(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  (setq recentf-max-saved-items 25))

;; Global keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)

(provide 'keybindings-config)
