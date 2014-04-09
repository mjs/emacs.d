(require 'go-mode)

(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 4))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(require 'go-flymake)
;(require 'go-flycheck)

(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

(provide 'go-config)
