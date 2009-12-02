;-----------------------------------------------------------
; Org-mode settings - somewhat BATS specific
;-----------------------------------------------------------
(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/Notes/TODO.org"))

(setq org-link-abbrev-alist
      '(("bug" . "http://bugzilla/show_bug.cgi?id=")))


; Friendlier TODO insert, can do it from any point on a line and plays nice with viper mode
(defun my-org-insert-todo-heading ()
  (interactive)
  (move-beginning-of-line nil)
  (org-insert-todo-heading 0)
  (viper-insert 0))

(defun org-move-to-done-tree ()
  "Move the current subtree to a monthly DONE tree at the bottom of the file" 
  (interactive)
  ; Hijack the org-archive-sibling-heading functionality by
  ; temporarily shadowing the settings it uses
  (let ((org-archive-sibling-heading (format-time-string "DONE: %Y %b"))
        (org-archive-tag "ARCH"))
    (org-archive-to-archive-sibling)))


(defun org-meeting-timeline ()
  (interactive)
  (org-timeline)
  (universal-argument)
  (universal-argument)
  (org-agenda-log-mode)
  (end-of-buffer))


(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "<M-S-return>") 'my-org-insert-todo-heading)
            (define-key org-mode-map (kbd "C-c C-0") 'org-move-to-done-tree)
            (define-key org-mode-map (kbd "C-c M-m") 'org-meeting-timeline)))

; Use 4 numeric priorities
(setq org-highest-priority ?1)
(setq org-lowest-priority ?4)
(setq org-default-priority ?4)

(setq org-tag-alist '(("@bluesky" . ?b)
                      ("@cleanup" . ?c)
                      ("@research" . ?r)
                      ("@secondary" . ?s)))

(setq org-todo-keywords
       '((sequence "TODO" "WAITING" "|" "DONE")))


(setq org-agenda-custom-commands
      '(("0" "Agenda + Tasks"
         ((agenda "" ((org-agenda-start-on-weekday nil)
                      (org-agenda-ndays 1)))
          (todo)))
        ("c" tags "@cleanup")
        ("b" tags "@bluesky")
        ("r" tags "@research")))



(provide 'org-config)
