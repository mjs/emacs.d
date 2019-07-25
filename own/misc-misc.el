
;;; misc-misc.el --- Random stuff
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defun run-terminal ()
  "Spawn an interactive terminal."
  (interactive)
  (start-process "terminal" nil "term"))

(global-set-key (kbd "C-c t") 'run-terminal)

(global-set-key (kbd "C-x C-#") 'bury-buffer)

;; Less convoluted key for window switching
(global-set-key (kbd "C-'") 'other-window)

(require 'midnight)
;; Also save the recent file list every day
(add-hook 'midnight-hook 'recentf-save-list)
(add-hook 'midnight-hook 'refresh-file-cache)

;; If the compilation buffer is displayed already, keep using that
;; frame and window. If it isn't displayed, pop it up in a new frame.
(add-to-list
 'display-buffer-alist
  '("\\*compilation\\*"
    (display-buffer-reuse-window display-buffer-pop-up-frame)
    (reusable-frames . t)))

(provide 'misc-misc)
;;; misc-misc.el ends here
