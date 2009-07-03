(defun replace-symbol-at-point (with)
  (interactive "sReplace with: ")
  (let ((sym (thing-at-point 'symbol)))
    (save-excursion
      (beginning-of-buffer)
      (query-replace sym with))))

(provide 'text-misc)
