(use-package yaml-mode
  :straight t
  :mode "\\.yml$"
  :mode "\\.yaml$"
  :bind (:map yaml-mode-map
         ("\C-m" . newline-and-indent)))

(provide 'yaml-config)
