;;; magit-config.el --- Configuration for magit and friends
;;
;;; Commentary:
;;
;;; Code:


(require 'magit)
(require 'evil-magit)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-status) ; easier to type

(provide 'magit-config)

;;; magit-config.el ends here
