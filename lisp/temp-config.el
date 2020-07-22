;; temp-config.el --- Keep temporary and backup files in sane places

;;; Commentary:

;; Set up where temporary, backup and auto-save files go.

;;; Code:
(setq emacsd-directory (expand-file-name "~/.emacs.d"))
(setq temporary-file-directory (concat emacsd-directory "/var/"))
(defvar auto-save-dir (concat temporary-file-directory "auto-save/"))
(setq auto-save-list-file-prefix (concat auto-save-dir ".saves-"))
(setq auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
(setq backup-directory-alist `(("." . ,(concat temporary-file-directory "backup"))))
(setq delete-old-versions t
  kept-new-versions 3
  kept-old-versions 2
  version-control t)

(require 'tramp)
(setq tramp-auto-save-directory auto-save-dir)
(setq tramp-backup-directory-alist backup-directory-alist)

(provide 'temp-config)
;;; temp-config.el ends here
