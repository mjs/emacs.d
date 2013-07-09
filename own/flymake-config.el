;; General setup for flymake

(require 'flymake-cursor)

(setq flymake-run-in-place nil)

(global-set-key (kbd "C-c k") 'flymake-goto-prev-error)
(global-set-key (kbd "C-c j") 'flymake-goto-next-error)
(global-set-key (kbd "C-c C-f") 'flymake-start-syntax-check)

(provide 'flymake-config)
