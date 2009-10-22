;; Personal modifications to enhance modal-mode

(require 'modal-mode)

(setq default-major-mode 'modal-fundamental-mode)
(modal-mode 1)
(when window-system
  (modal-mode-line-background-mode 1))

(define-key modal-cmd-mode-map "p"   'yank)
(define-key modal-cmd-mode-map "u"   'undo)
(define-key modal-cmd-mode-map ":w" 'save-buffer)

(provide 'modal-mode-extras)
