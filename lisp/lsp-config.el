(use-package lsp-mode
  :straight t
  :config
  (defun init-lsp-for-buffer()
    (lsp-deferred)
    (define-key evil-normal-state-local-map (kbd "M-.") 'lsp-find-definition)
    (define-key evil-normal-state-local-map (kbd "M-,") 'pop-tag-mark)
    (define-key evil-normal-state-local-map (kbd "M-l .") 'lsp-find-references)
    (define-key evil-normal-state-local-map (kbd "M-l r") 'lsp-rename)))

(provide 'lsp-config)
