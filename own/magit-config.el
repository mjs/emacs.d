(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(use-package evil-magit
  :straight t)

(provide 'magit-config)
