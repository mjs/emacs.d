(setenv "PATH" (format "%s:%s"
                       "/opt/node-v8.16.1-linux-x64/bin"
                       (getenv "PATH")))

(setq exec-path (cons "/opt/node-v8.16.1-linux-x64/bin" exec-path))

(use-package js
  :straight (:type built-in)
  :mode ("\\.js$" . js-mode)
  :hook (js-mode . lsp-deferred)
  :config
  (setq js-indent-level 2)
  :bind ("C-c b" . web-beautify-js))

(use-package typescript-mode
  :straight t
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package prettier-js
  :straight t)

(use-package web-beautify
  :straight t
  :config

  (defun html-init-beautify ()
    (interactive)
    (html-mode)
    (web-beautify-html))

  :bind ("<f6>" . 'html-init-beautify))

(provide 'js-config)
