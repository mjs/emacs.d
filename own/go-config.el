;;; go-config.el --- Customisations for Go programming
;;
;;; Commentary:
;;
;;
;;; Code:

; Ensure $GOPATH and $PATH are set up. exec-path is important for lsp-mode.
(let* ((go-path (expand-file-name "~/go"))
       (go-bin (concat go-path "/bin")))
  (setenv "GOPATH" go-path)
  (setenv "PATH" (format "%s:%s" go-bin (getenv "PATH")))
  (setq exec-path (cons go-bin exec-path)))

(setenv "GO111MODULE" "on")

(require 'go-mode)
(require 'lsp)
(require 'ethan-wspace)

(setq gofmt-command "gofmt")

(defun go-mode-customisations ()
  "Set up go-mode to suit my tastes."
  (lsp-deferred)

  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command) "go test "))

  (setq tab-width 4)
  (ethan-wspace-mode -1)
  (add-hook 'before-save-hook 'gofmt-before-save))

(add-hook 'go-mode-hook 'go-mode-customisations)

(evil-define-key 'normal go-mode-map [f9] 'compile)

(provide 'go-config)
;;; go-config.el ends here
