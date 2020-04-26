;;; lsp-config.el --- Language Server Protocol support
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'evil)
(require 'lsp)
(require 'company)
(require 'company-lsp)

(evil-global-set-key 'normal (kbd "M-.") 'lsp-find-definition)
(evil-global-set-key 'normal (kbd "M-?") 'lsp-find-references)
(evil-global-set-key 'normal (kbd "M-,") 'pop-tag-mark)
(evil-global-set-key 'normal (kbd "M-l r") 'lsp-rename)

(provide 'lsp-config)
;;; lsp-config.el ends here
