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

; Move a subtree to the bottom of the buffer, good for low priority or DONE items
; TODO: figure out how to not leave the second last subtree open
(defun org-move-subtree-to-bottom ()
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (org-cut-subtree)
    (goto-char (point-max))
    (show-subtree)
    (goto-char (point-max))
    (yank)
    (org-up-heading-all 99)
    (hide-subtree)))

(defun org-meeting-timeline ()
  (interactive)
  (org-timeline)
  (universal-argument)
  (universal-argument)
  (org-agenda-log-mode))


(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "<M-S-return>") 'my-org-insert-todo-heading)
            (define-key org-mode-map (kbd "C-c C-0") 'org-move-subtree-to-bottom)
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
       '((sequence "TODO" "STARTED" "WAITING" "|" "DONE" "DELEGATED")))


(setq org-agenda-custom-commands
      '(("0" "Agenda + Tasks"
         ((agenda)
          (todo)))
        ("c" tags "@cleanup")
        ("b" tags "@bluesky")
        ("r" tags "@research")))



(provide 'org-config)
