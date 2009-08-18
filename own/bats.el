(require 'bugzilla)
(require 'magit)
(require 'org-config)

(defun bats-insert-bug-title (bug-id)
  (interactive "NBug ID: ")
  (let ((bug (bugzilla-read-bug bug-id)))
    (insert (format "Bug %s - %s"
                  (cdr (assq 'id bug))
                  (cdr (assq 'desc bug))))
    ))

(defun bats-insert-org-bug (bug-id)
  (interactive "NBug ID: ")
  (let ((bug (bugzilla-read-bug bug-id)))
    (insert (format "%s [[bug:%s]]"
                  (cdr (assq 'desc bug))
                  (cdr (assq 'id bug))))
    ))


(defun bats-release-notes (last-rev)
  (interactive "NLast revision: ")
  (let ((buffer (create-file-buffer "changelog")))
    (shell-command (format "get-changelog %s" last-rev) buffer)
    (switch-to-buffer buffer)
    (delete-other-windows)
    (insert "\
Highlights
----------
* 

Schema Versions
---------------
None

Data Deltas
-----------
None

")
    (beginning-of-buffer)
    (split-window-vertically 20)
    (viper-mode)
    (search-forward "*")
    (viper-insert 0)))


;; ---------------------------------------------------------
;; Bindings
;; ---------------------------------------------------------
(global-set-key (kbd "C-# C-t") 'bats-insert-bug-title)
(global-set-key (kbd "C-# C-r") 'bats-release-notes)
(global-set-key (kbd "C-# g") 'magit-status)

;; BATS only bindings for org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-; b") 'bats-insert-org-bug)))

(provide 'bats)
