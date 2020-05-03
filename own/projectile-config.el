(use-package projectile
  :straight t
  :config
  (setq projectile-completion-system 'default)  ; use selectrum
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map)))

(projectile-mode +1)

(provide 'projectile-config)
