(use-package company
  :straight t
  :config
  (setq company-minimum-prefix-length 1)      ; start completing after 1 char
  (setq company-idle-delay .3)                ; decrease delay before autocompletion popup shows
  (setq company-tooltip-limit 20)             ; bigger popup window
  (setq company-echo-delay 0)                 ; remove annoying blinking
  (setq company-tooltip-align-annotations t)

  ; Prevent occasional wrong case completions
  (setq company-dabbrev-downcase nil)

  (evil-declare-change-repeat 'company-complete-common)

  ;; Abort company-mode when exiting insert mode
  (add-hook 'evil-insert-state-exit-hook 'company-abort)

  :bind (:map evil-insert-state-map
         ("TAB" . company-indent-or-complete-common)
         ("M-/" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)))

(global-company-mode)

(provide 'company-config)
