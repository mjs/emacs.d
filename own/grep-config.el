;; Navigation of grep results/errors
(global-set-key "\M-n" 'next-error)
(global-set-key "\M-p" 'previous-error)

(use-package rg
  :straight t)
(rg-enable-default-bindings)

(provide 'grep-config)
