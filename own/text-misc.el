(defun replace-symbol-at-point (with)
  (interactive "sReplace with: ")
  (let ((sym (thing-at-point 'symbol)))
    (save-excursion
      (beginning-of-buffer)
      (query-replace sym with))))


;; Add a per-buffer hook to automatically remove trailing whitespace on write
(defun auto-del-trailing-whitespace ()
  (interactive)
  (add-hook 'write-contents-hooks 'delete-trailing-whitespace t)
  (message "Trailing whitespace will be removed on write"))

(global-set-key (kbd "C-c C-w") 'auto-del-trailing-whitespace)


(provide 'text-misc)
