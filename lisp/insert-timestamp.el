
(defun insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun insert-time ()
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(provide 'insert-timestamp)
