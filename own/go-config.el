(setenv "PATH" (format "%s:%s"
                       (expand-file-name "~/go/bin")
                       (getenv "PATH")))

(setenv "GOPATH" (expand-file-name "~/go"))

(require 'go-mode)

(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 4)
  (ethan-wspace-mode -1))

(add-hook 'go-mode-hook 'my-go-mode-hook)

(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

(require 'go-oracle)
(setq go-oracle-command "~/go/bin/oracle")
(setq go-oracle-scope "github.com/juju/juju/cmd/juju github.com/juju/juju/cmd/jujud")

(provide 'go-config)
