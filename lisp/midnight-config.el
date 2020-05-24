(require 'midnight)

;; Also save the recent file list every day
(add-hook 'midnight-hook 'recentf-save-list)
(add-hook 'midnight-hook 'ffc-refresh)

(provide 'midnight-config)
