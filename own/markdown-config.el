;;; markdown-config.el --- Config for markdown files
;;;
;;; Commentary:
;;
;;; Code:

(defun my-markdown-mode-hook ()
  "Markdown-mode customisations."
  (kill-local-variable 'paragraph-start)) ; prevent unexpected paragraph movement

(eval-after-load "markdown"
  (add-hook 'markdown-mode-hook 'my-markdown-mode-hook))

(provide 'markdown-config)

;;; markdown-config.el ends here
