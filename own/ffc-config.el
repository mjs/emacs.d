;; Fast, async file name caching

(require 'ffc)

(add-to-list 'ffc-directories "~/Dropbox/Notes")
(add-to-list 'ffc-directories "~/config")
(add-to-list 'ffc-directories "~/.emacs.d")

(add-to-list 'ffc-filter-regexps "/[0-9]+$")
(add-to-list 'ffc-filter-regexps "/.emacs.d/external/")
(add-to-list 'ffc-filter-regexps "/.emacs.d/elpa/")

(global-set-key (kbd "C-x f") 'ffc-ido-find-file)
(global-set-key (kbd "C-x F") 'ffc-find-file-at-point)

(ffc-refresh)

(provide 'ffc-config)
