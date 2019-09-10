;-----------------------------------------------------------
; Org-mode settings
;-----------------------------------------------------------
(require 'org-utils)
(require 'org-re-reveal)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

; Default location of Org files
(setq org-directory "~/Dropbox/Notes")

; Friendlier TODO insert, can do it from any point on a line and plays nice with Evil
(defun my-org-insert-todo-heading ()
  (interactive)
  (move-beginning-of-line nil)
  (org-insert-todo-heading 0)
  (evil-insert 0)
  (insert "[#2] "))

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "<tab>") 'org-cycle)
            (define-key org-mode-map (kbd "<M-S-return>") 'my-org-insert-todo-heading)))

(provide 'org-config)
