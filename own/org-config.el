;-----------------------------------------------------------
; Org-mode settings
;-----------------------------------------------------------
(require 'org-install)
(require 'org-utils)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-log-done t)

; Default location of Org files
(setq org-directory "~/Dropbox/Notes")

; Friendlier TODO insert, can do it from any point on a line and plays nice with Evil
(defun my-org-insert-todo-heading ()
  (interactive)
  (move-beginning-of-line nil)
  (org-insert-todo-heading 0)
  (evil-insert 0)
  (insert "[#2] "))

(defun org-move-to-done-tree ()
  "Move the current subtree to a monthly DONE tree at the bottom of the file"
  (interactive)
  ; Hijack the org-archive-sibling-heading functionality by
  ; temporarily shadowing the settings it uses
  (let ((org-archive-sibling-heading (format-time-string "DONE: %Y %b"))
        (org-archive-tag "ARCH"))
    (org-archive-to-archive-sibling)))

; Modified version that takes a time to pass to org-deadline as well as the "remove" argument
(defun org-agenda-deadline (arg &optional time)
  "Schedule the item at point.
Arg is passed through to `org-deadline'."
  (interactive "P")
  (org-agenda-check-type t 'agenda 'timeline 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker))
         (org-insert-labeled-timestamps-at-point nil)
         ts)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (setq ts (org-deadline arg time)))   ; this line changed
      (org-agenda-show-new-time marker ts "D"))
        (message "Deadline for this item set to %s" ts)))

(defun org-deadline-today ()
  "Set an org mode item to have a deadline of today"
  (interactive)
  (org-deadline nil (org-today-in-internal-format)))

(defun org-agenda-deadline-today ()
  "Set an org mode agenda item to have a deadline of today"
  (interactive)
  (org-agenda-deadline nil (org-today-in-internal-format)))

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-agenda-mode-map (kbd "C-c D") 'org-agenda-deadline-today)
            (define-key org-mode-map (kbd "C-c D") 'org-deadline-today)
            (define-key org-mode-map (kbd "<tab>") 'org-cycle)
            (define-key org-mode-map (kbd "<M-S-return>") 'my-org-insert-todo-heading)
            (define-key org-mode-map (kbd "C-c C-0") 'org-move-to-done-tree)
            (define-key org-mode-map (kbd "C-c t t") 'org-table-inplace-to-tsv)
            (define-key org-mode-map (kbd "C-c t c") 'org-table-inplace-to-csv)))

; Use 4 numeric priorities
(setq org-highest-priority ?1)
(setq org-lowest-priority ?4)
(setq org-default-priority ?4)

(setq org-todo-keywords
       '((sequence "TODO" "WAITING" "|" "DONE")))

; Custom agenda view: agenda + tasks
(setq org-agenda-custom-commands
      '(("0" "Agenda + Tasks"
         ((agenda "" ((org-agenda-start-on-weekday nil)
                      (org-agenda-ndays 1)))
          (todo)))))


; Erase all reminders and rebuilt reminders for today from the agenda
(defun my-org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'my-org-agenda-to-appt 'append)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'my-org-agenda-to-appt)

(defun zip (&rest seqs)
  (apply 'mapcar* 'list seqs))

; Show appointment reminders using notify-send (goes over DBUS to whatever displays them)
(defun my-appt-disp-window (minutes-to-appt new-time appt-msg)
  (when (server-running-p)
    (when (not (listp minutes-to-appt))
      (setq minutes-to-appt (list minutes-to-appt))
      (setq appt-msg (list appt-msg)))
    (dolist (details (zip appt-msg minutes-to-appt))
      (apply 'my-appt-notify details))))

(defun my-appt-notify (msg minutes-to-appt)
  (call-process "notify-send" nil 0 nil
                "-t" "99999999"
                "-i" "/usr/share/pixmaps/gnome-calendar.png"
                "Reminder"
                (format "%s in %s minutes" msg minutes-to-appt)))


(setq appt-disp-window-function 'my-appt-disp-window)

(provide 'org-config)
