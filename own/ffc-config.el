
(require 'text-misc)
(require 'ffc)

(add-to-list 'ffc-directories "~/Dropbox/Notes")
(add-to-list 'ffc-directories "~/config")
(add-to-list 'ffc-directories "~/.emacs.d")

(add-to-list 'ffc-filter-regexps "/[0-9]+$")
(add-to-list 'ffc-filter-regexps "/.emacs.d/external/")
(add-to-list 'ffc-filter-regexps "/.emacs.d/elpa/")

(defun ffc-find-file-at-point ()
  "Using the filename at the point, open it using the file cache"
  (interactive)
  (let ((filename (filename-near-point)))
    (if (string= filename "")
        (message "No filename at point")
      (ffc-ido-find-file filename))))

(global-set-key (kbd "C-x f") 'ffc-ido-find-file)
(global-set-key (kbd "C-x F") 'ffc-find-file-at-point)

(ffc-refresh)

(provide 'ffc-config)
