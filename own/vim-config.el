(setq viper-mode t)                ; enable Viper at load time
;; These 2 are special and need to be set before viper is loaded. They don't
;; work if set in the customize block.
(setq viper-ex-style-editing nil)  ; can backspace past start of insert / line
(setq viper-ex-style-motion nil)   ; can move past end of line
(require 'viper)                   ; load Viper
(defun viper-translate-all-ESC-keysequences () nil)

(defadvice viper-maybe-checkout (around viper-checkin-fix activate)
  "Stop viper from trying to do anything VC related"
  nil)

(keyboard-translate ?\C-i ?\H-i)       ; map C-i to Hyper-i to avoid conflict with TAB
(setq vimpulse-want-C-i-like-Vim nil)  ; don't let Vimpulse set the binding, we'll do this ourselves 
(setq vimpulse-want-C-u-like-Vim t)    ; 
(require 'vimpulse)                    ; vim emulation
(define-key viper-vi-global-user-map (kbd "H-i") 'vimpulse-jump-forward)   ; Now set up C-i / H-i

;; Bind this elsewhere since Viper uses C-u (think p == 'prefix')
(global-set-key (kbd "C-p") 'universal-argument)


;; Useful bindings

(require 'text-misc)
(require 'elemental)

(define-key viper-vi-global-user-map "S" 'replace-symbol-at-point)
(define-key viper-vi-global-user-map "=" 'increment-number-at-point)
(define-key viper-vi-global-user-map "-" 'decrement-number-at-point)

(define-key viper-vi-global-user-map ")" 'elem-forward-one)
(define-key viper-vi-global-user-map "(" 'elem-backward-one)
(define-key viper-vi-global-user-map "gs" 'elem-transpose)
(define-key viper-vi-global-user-map "gS" 'elem-transpose-backward)

(provide 'vim-config)
