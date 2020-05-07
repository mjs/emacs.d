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
    (lsp-deferred)
    (setq tab-width 4)
    (ethan-wspace-mode -1))
  :hook (go-mode . init-go-mode)
  :hook (before-save . gofmt-before-save)
  :config (setq gofmt-command "gofmt")
  :bind (:map go-mode-map
         ("<f9>" . compile)))

(provide 'go-config)
