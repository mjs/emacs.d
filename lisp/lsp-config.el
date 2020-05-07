(use-package lsp-mode
  :straight t
  :config
  :bind (:map evil-normal-state-map
         ("M-." . lsp-find-definition)
         ("M-?" . lsp-find-references)
         ("M-," . pop-tag-mark)
         ("M-l r" . lsp-rename)))

(use-package company-lsp
  :straight t)

(provide 'lsp-config)
