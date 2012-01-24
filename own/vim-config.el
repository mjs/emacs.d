(setq viper-mode t)                ; enable Viper at load time
;; These 2 are special and need to be set before viper is loaded. They don't
;; work if set in the customize block.
(setq viper-ex-style-editing nil)  ; can backspace past start of insert / line
(setq viper-ex-style-motion nil)   ; can move past end of line
(require 'viper)                   ; load Viper
(require 'vimpulse)                ; vim emulation
(defun viper-translate-all-ESC-keysequences () nil)

(defadvice viper-maybe-checkout (around viper-checkin-fix activate)
  "Stop viper from trying to do anything VC related"
  nil)

(require 'text-misc)
(require 'transpose-funcargs)

(define-key viper-vi-global-user-map "S" 'replace-symbol-at-point)
(define-key viper-vi-global-user-map "=" 'increment-number-at-point)
(define-key viper-vi-global-user-map "-" 'decrement-number-at-point)

(define-key viper-vi-global-user-map ")" 'forward-one-funcarg)
(define-key viper-vi-global-user-map "(" 'backward-one-funcarg)
(define-key viper-vi-global-user-map "gs" 'transpose-funcarg)
(define-key viper-vi-global-user-map "gS" 'transpose-previous-funcarg)

(setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))     ; make C-i and TAB different keys

(defun configure-TAB-key (filepath)
  (unless (or (boundp 'tab-configured-here) (string= major-mode "org-mode"))
    (vimpulse-map-local (kbd "<tab>") 'indent-for-tab-command)
    (vimpulse-map-local (kbd "C-i") 'vimpulse-jump-forward))
    (make-local-variable 'tab-configured-here)
    (setq tab-configured-here 't))

(add-to-list 'after-load-functions 'configure-TAB-key)

;; Bind this elsewhere since Viper uses C-u
;; p == 'prefix'
(global-set-key (kbd "C-p") 'universal-argument)

(provide 'vim-config)
