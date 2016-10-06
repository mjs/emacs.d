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


;; This tracks the compile command used for any .go file
(setq go-compile-command "gochecker -v")

;; TODO make this non-juju specific by hunting back towards $GOPATH for the root of the repo
(defun compile-juju ()
  (interactive)
  (setq compile-command go-compile-command)
  (let ((default-directory (expand-file-name "~/go/src/github.com/juju/juju")))
    (cd default-directory)
    (call-interactively 'compile))
  (setq go-compile-command compile-command))

;; Have compile save automatically
(setq compilation-ask-about-save nil)

(defun refresh-juju-bits ()
  (interactive)
  (revert-all-buffers)
  (rebuild-juju-tags)
  (refresh-file-cache))

(defun canonical-go-mode-hook ()
  (define-key go-mode-map (kbd "<f9>") 'compile-juju)
  (define-key go-mode-map (kbd "<f5>") 'refresh-juju-bits))

(add-hook 'go-mode-hook 'canonical-go-mode-hook)

(provide 'canonical-config)
