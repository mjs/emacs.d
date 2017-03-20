(setenv "PATH" (format "%s:%s"
                       (expand-file-name "~/go/bin")
                       (getenv "PATH")))

(setenv "GOROOT" "/snap/go/current") ; XXX not ideal
(setenv "GOPATH" (expand-file-name "~/go"))


(require 'go-mode)
(require 'go-guru)

; Use goimports instead of go-fmt
(setq gofmt-command "goimports")

(defun my-go-mode-hook ()
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "gochecker -v"))
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 4)
  (ethan-wspace-mode -1))

(add-hook 'go-mode-hook 'my-go-mode-hook)

(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

(provide 'go-config)
