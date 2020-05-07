(defun eval-buffer-and-run-ert ()
  (interactive)
  (eval-buffer)
  (ert "t"))

(define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-buffer-and-run-ert)

(provide 'lisp-config)
