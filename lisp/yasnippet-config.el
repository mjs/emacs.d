(use-package yasnippet
  :straight t
  :config
  (setq yas-indent-line nil)  ; prevent annoying auto-indent behaviour
  :bind (:map evil-insert-state-map
         ("C-c y" . yas-expand)))

(yas-global-mode 1)

(provide 'yasnippet-config)
