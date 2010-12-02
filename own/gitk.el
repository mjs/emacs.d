
(defun gitk ()
  "Run gitk with no arguments"
  (interactive)
  (gitk-run nil))

(defun gitk-file ()
  "Run gitk for the current file"
  (interactive)
  (gitk-run (file-name-or-error)))

(defun gitk-dir ()
  "Run gitk for the directory of the current file"
  (interactive)
  (gitk-run (file-name-directory (file-name-or-error))))

(defun gitk-run (target)
  (unless target (setq target ""))
  (start-process "gitk" nil "gitk" target))

(defun file-name-or-error ()
  (let ((fn (buffer-file-name)))
    (unless fn (error "No file associated with buffer"))
    fn))


(provide 'gitk)
