;;; projectile-config.el -- personal config for projectile

;;; Commentary:

;;; Code:
(require 'projectile)
(require 'projectile-ripgrep)

(projectile-global-mode 1)
(global-set-key (kbd "C-c C-r") 'projectile-ripgrep)

(provide 'projectile-config)
;;; projectile-config.el ends here
