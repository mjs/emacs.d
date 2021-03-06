(use-package org
  :straight t
  :mode ("\\.org$" . org-mode)
  :config
  (setq org-directory "~/Dropbox/Notes")

  ; Friendlier TODO insert, can do it from any point on a line and plays nice with Evil
  (defun my-org-insert-todo-heading ()
    (interactive)
    (move-beginning-of-line nil)
    (org-insert-todo-heading 0)
    (evil-insert 0)
    (insert "[#2] "))

  :bind (:map org-mode-map
         ("<tab>" . org-cycle)
         ("<M-S-return>" . my-org-insert-todo-heading)))

(use-package org-re-reveal
  :straight t)

(provide 'org-config)
