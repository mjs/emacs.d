(use-package projectile
  :straight t
  :bind (("C-c C-r" . projectile-ripgrep)
         :map projectile-mode-map
         ("C-c p" . projectile-command-map)))

(use-package projectile-ripgrep
  :straight t)

(projectile-mode +1)

(provide 'projectile-config)
