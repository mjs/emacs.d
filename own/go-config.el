(setenv "PATH" (format "%s:%s"
                       (expand-file-name "~/go/bin")
                       (getenv "PATH")))
(setenv "GOPATH" (expand-file-name "~/go"))


(require 'go-mode)
(require 'go-guru)
(require 'go-projectile)

; Use goimports instead of go-fmt
(setq gofmt-command "goimports")

(defun my-go-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-go))

  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go test "))

  (setq tab-width 4)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (ethan-wspace-mode -1)

  (with-eval-after-load 'evil
    (define-key evil-normal-state-local-map (kbd "M-}") 'godoc-at-point)))

(add-hook 'go-mode-hook 'my-go-mode-hook)

(define-key go-mode-map [f9] 'compile)

(require 'company)    ; load company mode
(require 'company-go) ; load company mode go backend

(provide 'go-config)
