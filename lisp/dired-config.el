;; get rid of the multiple dired buffer problem
(use-package dired-single
  :straight t
  :config
  (defun dired-single-up ()
    (interactive)
    (joc-dired-single-buffer ".."))
  :bind (:map dired-mode-map
         ("Q" . dired-do-query-replace-regexp)
         ("RET" . joc-dired-single-buffer)
         ("<mouse-1>" . joc-dired-single-buffer-mouse)
         ("^" . dired-single-up)))

(provide 'dired-config)
