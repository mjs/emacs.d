(require 'bugzilla)

(defun bats-insert-bug-title (bug-id)
  (interactive "NBug ID:")
  (let ((bug (bugzilla-read-bug bug-id)))
    (insert (format "Bug %s - %s"
                  (cdr (assq 'id bug))
                  (cdr (assq 'desc bug))))
    ))

(global-set-key (kbd "C-# C-t") 'bats-insert-bug-title)


(provide 'bats)
