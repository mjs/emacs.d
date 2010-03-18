(require 'magit)
(require 'org-config)


(defun bats-deploy-crontab ()
  "Deploy the current buffer containing a BATS crontab"
  (interactive)
  (save-buffer)
  (let ((dest (bats-dest-from-crontab-name (buffer-file-name))))
    (bats-deploy-crontab-to-host dest)))


(defun bats-deploy-crontab-to-host (dest)
  (interactive "sDestination [host/user@host]: ")
  (let* ((write-dest (format "/ssh:%s:newcron" dest))
         (diff-buffer (create-file-buffer "crontab-diff")))
    (copy-file (buffer-file-name) write-dest t)
    (shell-command (format "ssh %s 'crontab -l | diff - newcron'" dest) diff-buffer)
    (switch-to-buffer-other-window diff-buffer)
    (when (yes-or-no-p "Ok to deploy? ")
      (shell-command (format "ssh %s 'crontab newcron'" dest))
      (kill-buffer diff-buffer))
    (delete-file write-dest)))


(defun bats-dest-from-crontab-name (filename)
  "Extract the username and host from a crontab filename in user@host form"
  (unless (string-match "\\.crontab$" filename) (throw 'invalid-filename t))
  (let* ((basename (file-name-nondirectory filename))
         (user-host (car (split-string basename "\\."))))
    (replace-regexp-in-string "_" "@" user-host)))


(defun bats-insert-bug-title (bug-id)
  (interactive "NBug ID: ")
  (let ((bug (bugz-get-bug bug-id)))
    (insert (format "Bug %s - %s"
                  (cadr (assq 'bug_id bug))
                  (cadr (assq 'short_desc bug))))
    ))


;; XXX lists of bugs
(defun bats-insert-org-bug (bug-id)
  (interactive "NBug ID: ")
  (let ((bug (bugz-get-bug bug-id)))
    (insert (format "%s [[bug:%s]]"
                  (cadr (assq 'short_desc bug))
                  (cadr (assq 'bug_id bug))))
    ))


(defun bats-extract-org-bug-id ()
  "Pull the bug ID out of the current line"
  (save-excursion 
    (beginning-of-line)
    (search-forward "[[bug:")
    (let ((startp (point)))
      (search-forward "]")
      (backward-char 1)
      (buffer-substring-no-properties startp (point)))))


;; XXX allow for interactive selection of resolution
(defun bats-close-org-bug ()
  (interactive)
  (bugz-close-bug (bats-extract-org-bug-id))
  (org-todo 'done))


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
(global-set-key (kbd "C-. s") 'magit-status)
(global-set-key (kbd "C-. l") 'magit-log)

;; BATS only bindings for org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c C-i") 'bats-insert-org-bug)
            (define-key org-mode-map (kbd "C-c C-c") 'bats-close-org-bug)))

(provide 'bats)
