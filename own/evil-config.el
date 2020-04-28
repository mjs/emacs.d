(use-package evil
  :straight t
  :init

  (setq evil-esc-delay 0.001)           ; avoid ESC/meta mixups
  (keyboard-translate ?\C-i ?\H-i)      ; map C-i to Hyper-i to avoid conflict with TAB
  (setq evil-want-C-i-jump nil)         ; don't let Evil set the binding, we'll do this ourselves
  (setq evil-want-C-u-scroll t)
  (setq evil-shift-width 4)
  (setq evil-search-module 'evil-search)

  :config

  ;; Use hippie-expand instead of evil's own completion for C-n
  ; XXX should this be company?
  (setq evil-complete-next-func 'hippie-expand)

  ;; A;; Not sure why this is necessary but various insert mode keys don't work
  ;; without it.
  (evil-update-insert-state-bindings nil nil t)

  :bind (
         :map evil-normal-state-map
         ("H-i" . evil-jump-forward)   ; Now set up C-i / H-i
         ("S" . replace-symbol-at-point)

         ("C-]" . etags-select-ultimate-find-tag)
         ("M-]" . etags-select-find-tag)

         ("=" . evil-numbers/inc-at-pt)
         ("-" . evil-numbers/dec-at-pt)

         (")" . elem/forward-one)
         ("(" . elem/backward-one)
         ("gs" . elem/transpose)
         ("gS" . (lambda ()
                  (interactive)
                  (elem/transpose -1)))

         ;; Bind this elsewhere since Evil uses C-u (think p == 'prefix')
         ("C-p" . universal-argument)

         :map evil-insert-state-map
         ("C-p" . universal-argument)

         :map evil-visual-state-map
         ("C-p" . universal-argument)

         )
)

(evil-mode 1)

(use-package evil-numbers
  :straight t)
(use-package elemental)

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
        (etags-select-find-tag)))))

(provide 'evil-config)
