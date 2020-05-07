;; flycheck-config.el --- Personal configuration for flycheck

;;; Commentary:

;; Minor configuration tweaks for flycheck.

;;; Code:
(use-package flycheck
  :straight t)

(global-flycheck-mode 1)

(global-set-key (kbd "C-c C-n") 'flycheck-next-error)
(global-set-key (kbd "C-c C-p") 'flycheck-previous-error)

(provide 'flycheck-config)
;;; flycheck-config.el ends here
