(add-to-list 'load-path "~/.emacs.d/external/evil")

(keyboard-translate ?\C-i ?\H-i)       ; map C-i to Hyper-i to avoid conflict with TAB

(setq evil-want-C-i-jump nil)         ; don't let Evil set the binding, we'll do this ourselves
(setq evil-want-C-u-scroll t)
(setq evil-shift-width 4)
(setq evil-search-module 'evil-search)

(require 'evil)
(evil-mode 1)

(define-key evil-normal-state-map (kbd "H-i") 'evil-jump-forward)   ; Now set up C-i / H-i

;; Bind this elsewhere since Evil uses C-u (think p == 'prefix')
(global-set-key (kbd "C-p") 'universal-argument)

;; Useful bindings

(require 'text-misc)
(require 'elemental)

(define-key evil-normal-state-map "S" 'replace-symbol-at-point)
;; (define-key evil-normal-state-map "=" 'increment-number-at-point)
;; (define-key evil-normal-state-map "-" 'decrement-number-at-point)

(define-key evil-normal-state-map ")" 'elem/forward-one)
(define-key evil-normal-state-map "(" 'elem/backward-one)
(define-key evil-normal-state-map "gs" 'elem/transpose)
(define-key evil-normal-state-map "gS" '(lambda ()
                                          (interactive)
                                          (elem/transpose -1)))

(provide 'evil-config)
