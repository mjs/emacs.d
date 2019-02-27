;;; projectile-config.el -- personal config for projectile

;;; Commentary:

;;; Code:
(require 'projectile)
(require 'projectile-ripgrep)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c C-r") 'projectile-ripgrep)

(provide 'projectile-config)
;;; projectile-config.el ends here
