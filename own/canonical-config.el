(setq org-agenda-files (list "~/canonical/TODO.org"))

(setq juju-tags-dir (expand-file-name "~/go/src/github.com/juju"))

(defun rebuild-juju-tags ()
  (interactive)
  (let ((out-file (concat (file-name-as-directory juju-tags-dir) "TAGS")))
    (shell-command (format "ctags --recurse -e -f %s %s" out-file juju-tags-dir))))

(add-hook 'midnight-hook 'rebuild-juju-tags)

(provide 'canonical-config)
