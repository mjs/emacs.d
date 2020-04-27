;;; lsp-config.el --- Language Server Protocol support
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package lsp
  :bind (:map evil-normal-state-map
         ("M-." . lsp-find-definition)
         ("M-?" . lsp-find-references)
         ("M-," . pop-tag-mark)
         ("M-l r" . lsp-rename)))

(provide 'lsp-config)
;;; lsp-config.el ends here
