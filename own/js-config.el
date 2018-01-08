; (require js2-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

(add-hook 'js-mode-hook 'flycheck-mode)

;
;(require 'js2-refactor)
;(add-hook 'js2-mode-hook #'js2-refactor-mode)
;(js2r-add-keybindings-with-prefix "C-c C-r")
;(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

; TODO xref-js2 - needs Emacs 25
; TODO company-tern




(provide 'js-config)
