(let* ((go-path (expand-file-name "~/go"))
       (go-bin (concat go-path "/bin")))
  (setenv "GOPATH" go-path)
  (setenv "PATH" (format "%s:%s" go-bin (getenv "PATH")))
  (setq exec-path (cons go-bin exec-path)))

(setenv "GO111MODULE" "on")

(use-package go-mode
  :straight t
  :init
  (defun init-go-mode ()
    (interactive)
    (init-lsp-for-buffer)
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (setq tab-width 4)
    (ethan-wspace-mode -1))
  :hook (go-mode . init-go-mode)
  :bind (:map go-mode-map
         ("<f9>" . compile)))

(provide 'go-config)
