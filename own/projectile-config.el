(use-package projectile
  :straight t
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map))

  :config
  (setq projectile-completion-system 'default)  ; use selectrum
  (projectile-mode +1))

(use-package projectile-ripgrep
  :straight t
  :bind (("C-c C-r" . projectile-ripgrep)))

(provide 'projectile-config)
