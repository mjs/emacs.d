;-----------------------------------------------------------
; Org-mode settings
;-----------------------------------------------------------
(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/Notes/TODO.org"))

(setq org-link-abbrev-alist
      '(("bug" . "http://bugzilla/show_bug.cgi?id=")))


(defun my-org-insert-todo-heading ()
  (interactive)
  (move-beginning-of-line nil)
  (org-insert-todo-heading 0)
  (viper-insert 0))

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "<M-S-return>") 'my-org-insert-todo-heading)))


;-----------------------------------------------------------
; org integration with appointments (appt)
;-----------------------------------------------------------
(require 'appt)

; Erase all reminders and rebuilt reminders for today from the agenda
(defun my-org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'my-org-agenda-to-appt)

; Set up appointments when this loads
(my-org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'my-org-agenda-to-appt)


(provide 'org-config)
