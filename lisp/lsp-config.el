(use-package lsp-mode
  :straight t
  :config
  :bind (:map evil-normal-state-map
         ("M-." . lsp-find-definition)
         ("M-," . pop-tag-mark)
         ("M-l ." . lsp-find-references)
         ("M-l r" . lsp-rename)))

(provide 'lsp-config)
