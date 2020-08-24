(use-package evil
  :straight t
  :demand t

  :init
  (define-key input-decode-map "\C-i" [C-i])  ; convert C-i to avoid confict with TAB
  (setq evil-shift-width 4)
  (setq evil-search-module 'evil-search)

  :bind (:map evil-normal-state-map
         ("gr" . replace-symbol-at-point)

         :map evil-motion-state-map
         ("C-u" . 'evil-scroll-up)
         ([C-i] . 'evil-jump-forward)  ; see above for why this is [C-i]

         ;; Bind universal-argument on C-p since C-u is used for scrolling (think p == 'prefix')
         ("C-p" . universal-argument)

         :map evil-insert-state-map
         ("C-p" . universal-argument)

         :map evil-visual-state-map
         ("C-p" . universal-argument))

  :config
  ; evil likes undo-tree but it's buggy. Use undo-fu instead.
  (global-undo-tree-mode -1)

  ;; Use hippie-expand instead of evil's own completion for C-n
  ; XXX should this be company?
  (setq evil-complete-next-func 'hippie-expand)

  ;; A;; Not sure why this is necessary but various insert mode keys don't work
  ;; without it.
  (evil-update-insert-state-bindings nil nil t)

  ;; Enable evil-mode in all buffers.
  (evil-mode 1))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

(use-package evil-numbers
  :straight t
  :bind (:map evil-normal-state-map
         ("=" . evil-numbers/inc-at-pt)
         ("-" . evil-numbers/dec-at-pt)))

(use-package undo-fu
  :straight t

  :bind (:map evil-normal-state-map
         ("u" . undo-fu-only-undo)
         ("C-r" . undo-fu-only-redo)))

(use-package evil-args
  :straight t
  :bind (:map evil-inner-text-objects-map
         ("a" . evil-inner-arg)
         :map evil-outer-text-objects-map
         ("a" . evil-outer-arg)

         :map evil-normal-state-map
         ("L" . evil-forward-arg)
         ("K" . evil-jump-out-args)

         :map evil-normal-state-map
         ("H" . evil-backward-arg)
         ("L" . evil-forward-arg)

         :map evil-motion-state-map
         ("H" . evil-backward-arg)
         ("L" . evil-forward-arg)))


(use-package evil-exchange
  :straight t
  :demand t
  :config
  (evil-exchange-install)

  (defun evil-arg-swap-forward ()
    (interactive)
    (apply 'evil-exchange (evil-inner-arg))
    (call-interactively 'evil-forward-arg)
    (apply 'evil-exchange (evil-inner-arg)))

  (defun evil-arg-swap-backward ()
    (interactive)
    (apply 'evil-exchange (evil-inner-arg))
    (evil-forward-arg 1)
    (evil-backward-arg 2)
    (apply 'evil-exchange (evil-inner-arg)))

  :bind (:map evil-normal-state-map
         ("gs" . evil-arg-swap-forward)
         ("gl" . evil-arg-swap-forward)
         ("gS" . evil-arg-swap-backward)
         ("gh" . evil-arg-swap-backward)))

;; Provide a menu of tags when there's multiple matches
(use-package etags-select
  :straight t
  :config
  (defun etags-select-ultimate-find-tag ()
    "Try to find the tag at point but if there isn't one then prompt"
    (interactive)
    (let ((tag (find-tag-default)))
      (if tag
          (etags-select-find tag)
        (etags-select-find-tag))))

  :bind (:map evil-normal-state-map
         ("C-]" . etags-select-ultimate-find-tag)
         ("M-]" . etags-select-find-tag)))

(provide 'evil-config)
