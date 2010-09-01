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


;;XXX - breaks after bugzilla interaction
;;XXX - needs a buffer
(defun bats-create-org-bug ()
  (interactive)
  (let* ((title (read-string "Bug title: "))
        (description (read-string "Desc: "))
        (priority (completing-read "Priority: " '("P1" "P2" "P3" "P4" "P5") nil t "P3"))
        (severity (completing-read "Severity: " 
                                   '("blocker" "critical" "major" "normal" "minor" "trivial" "enhancement") 
                                   nil t "normal"))
        (bug-id (bats-create-bug title description priority severity)))
    (open-line)
    (insert (format "* TODO %s [[bug:%s]]\n%s" title bug-id description))))
     

;; works
(defun bats-create-bug (title desc priority severity)
  (bugz-create-bug "BATS Internal"
                    "General"
                    title
                    desc
                    "msmits@batstrading.com"
                    priority
                    severity))

     
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


Crontab Deltas
--------------
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

;; BATS only bindings for org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c b i") 'bats-insert-org-bug)
            (define-key org-mode-map (kbd "C-c b c") 'bats-close-org-bug)))

(provide 'bats)
