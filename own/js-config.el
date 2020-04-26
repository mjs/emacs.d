;;; js-config.el --- Set up for JS and TypeScript
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(setenv "PATH" (format "%s:%s"
                       "/opt/node-v8.16.1-linux-x64/bin"
                       (getenv "PATH")))

(setq exec-path (cons "/opt/node-v8.16.1-linux-x64/bin" exec-path))

(require 'js)
(require 'prettier-js)
(require 'web-beautify)

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

(defun my-js-mode-hook ()
  (lsp-deferred))

(eval-after-load 'js
  (progn
    (setq js-indent-level 2)
    '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js)
    (add-hook 'js-mode-hook 'my-js-mode-hook)))


(defun my-ts-mode-hook ()
  (lsp-deferred)
  (setq typescript-indent-level 2))

(eval-after-load 'typescript
  (add-hook 'typescript-mode-hook 'my-ts-mode-hook))


(defun html-init-beautify ()
  (interactive)
  (html-mode)
  (web-beautify-html))
(global-set-key (kbd "<f6>") 'html-init-beautify)


(provide 'js-config)
;;; js-config.el ends here
