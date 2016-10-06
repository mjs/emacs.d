(setq evil-esc-delay 0.001)           ; avoid ESC/meta mixups

(keyboard-translate ?\C-i ?\H-i)      ; map C-i to Hyper-i to avoid conflict with TAB

(setq evil-want-C-i-jump nil)         ; don't let Evil set the binding, we'll do this ourselves
(setq evil-want-C-u-scroll t)
(setq evil-shift-width 4)
(setq evil-search-module 'evil-search)

(require 'evil)
(evil-mode 1)

;; Use hippie-expand instead of evil's own completion for C-n
(setq evil-complete-next-func 'hippie-expand)

(define-key evil-normal-state-map (kbd "H-i") 'evil-jump-forward)   ; Now set up C-i / H-i

;; Bind this elsewhere since Evil uses C-u (think p == 'prefix')
(define-key evil-normal-state-map (kbd "C-p") 'universal-argument)
(define-key evil-insert-state-map (kbd "C-p") 'universal-argument)
(define-key evil-visual-state-map (kbd "C-p") 'universal-argument)

;; Provide a menu of tags when there's multiple matches
(require 'etags-select)
(defun etags-select-ultimate-find-tag ()
  "Try to find the tag at point but if there isn't one then prompt"
  (interactive)
  (let ((tag (find-tag-default)))
    (if tag
        (etags-select-find tag)
      (etags-select-find-tag))))

;; Alternate escape to normal mode
(define-key evil-insert-state-map (kbd "C-k") 'evil-force-normal-state)
(define-key evil-normal-state-map (kbd "C-k") 'evil-force-normal-state)

;; Not sure why this is necessary but various insert mode keys don't work
;; without it.
(evil-update-insert-state-bindings nil nil t)

;; Useful bindings
(define-key evil-normal-state-map (kbd "C-]") 'etags-select-ultimate-find-tag)
(define-key evil-normal-state-map (kbd "M-]") 'etags-select-find-tag)

(require 'text-misc)
(define-key evil-normal-state-map "S" 'replace-symbol-at-point)

(require 'evil-numbers)
(define-key evil-normal-state-map "=" 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map "-" 'evil-numbers/dec-at-pt)

(require 'elemental)
(define-key evil-normal-state-map ")" 'elem/forward-one)
(define-key evil-normal-state-map "(" 'elem/backward-one)
(define-key evil-normal-state-map "gs" 'elem/transpose)
(define-key evil-normal-state-map "gS" '(lambda ()
                                          (interactive)
                                          (elem/transpose -1)))

(provide 'evil-config)
