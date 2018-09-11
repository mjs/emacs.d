;;; yasnippet-config.el --- Configuration for yas-snippet
;;
;;; Commentary:
;;
;; Basic setup for yas-snippet.
;;
;;; Code:

(require 'yasnippet)
(require 'evil)

;; yasnippet
(yas-global-mode 1)
(setf yas-indent-line nil)  ; prevent annoying auto-indent behaviour

(define-key evil-insert-state-map (kbd "C-c y") 'yas-expand)

(provide 'yasnippet-config)
;;; yasnippet-config.el ends here
