
(global-set-key (kbd "C-x C-#") 'bury-buffer)

;; Less convoluted key for window switching
(global-set-key (kbd "C-'") 'other-window)

(require 'midnight)
;; Also save the recent file list every day
(add-hook 'midnight-hook 'recentf-save-list)
(add-hook 'midnight-hook 'refresh-file-cache)

(provide 'misc-misc)
