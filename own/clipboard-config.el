;; Use the primary selection by default
(setq x-select-enable-clipboard nil)
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region t)

;; Bindings for clipboard interaction
(global-set-key "\C-cc" 'clipboard-kill-ring-save)
(global-set-key "\C-cx" 'clipboard-kill-region)
(global-set-key "\C-cv" 'clipboard-yank)


(provide 'clipboard-config)
