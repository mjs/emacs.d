;; Lisp related customisations

(defun eval-buffer-and-run-ert ()
  (interactive)
  (eval-buffer)
  (ert "t"))

(defun lisp-mode-setup()
  (define-key lisp-mode-map [f5] 'eval-buffer-and-run-ert))
(add-hook 'lisp-mode-hook 'lisp-mode-setup)

(provide 'lisp-config)
