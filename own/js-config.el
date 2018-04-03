(require 'js)
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(setq js-indent-level 2)

(require 'web-beautify)
(eval-after-load 'js
  '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))

; TODO xref-js2 - needs Emacs 25
; TODO company-tern

(provide 'js-config)
