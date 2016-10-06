(setenv "PATH" (format "%s:%s"
                       (expand-file-name "~/go/bin")
                       (getenv "PATH")))

(setenv "GOPATH" (expand-file-name "~/go"))


(require 'go-mode)

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

;; Seems to cause startup to abort early and the keybindings suck
;; (require 'oracle)
;; (setq go-oracle-command "~/go/bin/oracle")
;; (setq go-oracle-scope "github.com/juju/juju/cmd/juju github.com/juju/juju/cmd/jujud")

(provide 'go-config)
