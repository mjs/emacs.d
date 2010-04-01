(require 'egit)

(defun egit-current ()
  (interactive)
  (let* ((branch-state (egit-get-branches))
         (current-branch (car branch-state))
         (buffer (get-buffer-create (format "*egit:%s*" current-branch))))
    (switch-to-buffer buffer)
    (cd (git-get-top-dir default-directory))
    (egit-mode (egit-parse-log current-branch 250) current-branch default-directory 250)))

(provide 'egit-extras)
