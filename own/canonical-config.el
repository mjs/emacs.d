(require 'go-config)

(setq org-agenda-files (list "~/canonical/TODO.org"))

(setq juju-tags-dir (expand-file-name "~/go/src/github.com/juju"))

(defun rebuild-juju-tags ()
  (interactive)
  (let ((tags-file (concat (file-name-as-directory juju-tags-dir) "TAGS")))
    (shell-command (format "ctags --recurse -e -f %s %s" tags-file juju-tags-dir))
    (visit-tags-table tags-file)))

(add-hook 'midnight-hook 'rebuild-juju-tags)
(rebuild-juju-tags)

;; TODO make this non-juju specific by hunting back towards $GOPATH for the root of the repo
(defun compile-juju ()
  (interactive)
  (let ((default-directory (expand-file-name "~/go/src/github.com/juju/juju")))
    (cd default-directory)
    (call-interactively 'compile)))

(defun canonical-go-mode-hook ()
  (define-key go-mode-map (kbd "<f9>") 'compile-juju))

(add-hook 'go-mode-hook 'canonical-go-mode-hook)

(provide 'canonical-config)
