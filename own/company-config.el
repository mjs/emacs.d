(require 'company)
(require 'company-lsp)

(setq company-minimum-prefix-length 1)               ; start completing after 1 char
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-tooltip-align-annotations t)
;(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

; Enable company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

;(define-key evil-normal-state-map (kbd "C-c /") 'company-complete)
(define-key evil-insert-state-map (kbd "TAB") #'company-indent-or-complete-common)
(define-key evil-insert-state-map (kbd "M-/") 'company-complete)
(evil-declare-change-repeat 'company-complete-common)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

;; Abort company-mode when exiting insert mode
(defun abort-company-on-insert-state-exit ()
  (company-abort))
(add-hook 'evil-insert-state-exit-hook 'abort-company-on-insert-state-exit)

(provide 'company-config)
