(require 'go-mode)

(setenv "PATH" (format "%s:%s"
                       (expand-file-name "~/go/bin")
                       (getenv "PATH")))

(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 4)
  (ethan-wspace-mode -1))

(add-hook 'go-mode-hook 'my-go-mode-hook)

(require 'go-flymake)

(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

(provide 'go-config)
