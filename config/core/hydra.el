(straight-use-package 'hydra)
(use-package hydra
  :ensure t
  :config
  ;; Example hydra for window management
  (defhydra hydra-window (:color pink :hint nil)
    "
Movement: [_h_] left  [_j_] down  [_k_] up  [_l_] right   Actions: [_v_] split [_x_] delete [_o_] maximize [_b_] balance [_q_] quit
"
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("v" split-window-right)
    ("x" delete-window)
    ("o" delete-other-windows)
    ("b" balance-windows)
    ("q" nil)))
(provide 'hydra-config)
