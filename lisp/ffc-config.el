(use-package ffc
  :straight (ffc :type git :host github :repo "mjs/ffc")
  :demand t
  :config

  (global-set-key (kbd "C-x f") 'ffc-find-file)

  (add-to-list 'ffc-directories "~/Dropbox/Notes")
  (add-to-list 'ffc-directories "~/config")
  (add-to-list 'ffc-directories "~/.emacs.d")
  (add-to-list 'ffc-filter-regexps "/[0-9]+$")
  (add-to-list 'ffc-filter-regexps "/.emacs.d/straight/build")
  (add-to-list 'ffc-filter-regexps "/.emacs.d/straight/repos")
  (ffc-refresh)

  (require 'text-utils)

  (defun ffc-find-file-at-point ()
    "Using the filename at the point, open it using the file cache"
    (interactive)
    (let ((filename (filename-near-point)))
      (if (string= filename "")
          (message "No filename at point")
        (ffc-find-file filename))))

  (global-set-key (kbd "C-x F") 'ffc-find-file-at-point))

(provide 'ffc-config)
