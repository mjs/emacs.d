(setenv "PATH" (format "%s:%s"
                       (expand-file-name "~/go/bin")
                       (getenv "PATH")))
(setenv "GOPATH" (expand-file-name "~/go"))


(require 'go-mode)
(require 'go-guru)
(require 'go-projectile)
(require 'lsp-config)
(require 'company)
(require 'company-lsp)

; Use goimports instead of go-fmt
(setq gofmt-command "my-go-fmt")

(defun my-go-mode-hook ()
  (lsp-deferred)

  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go test "))

  (setq tab-width 4)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (ethan-wspace-mode -1))

(add-hook 'go-mode-hook 'my-go-mode-hook)

(evil-define-key 'normal go-mode-map [f9] 'compile)

(provide 'go-config)
;;; go-config.el ends here
