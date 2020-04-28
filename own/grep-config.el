;; Navigation of grep results/errors
(global-set-key "\M-n" 'next-error)
(global-set-key "\M-p" 'previous-error)

;; Unique grep buffer per search
(use-package grep-a-lot
  :straight t)
(grep-a-lot-setup-keys)

(defalias 'rg 'ripgrep-regexp)

(provide 'grep-config)
