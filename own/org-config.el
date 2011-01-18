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

; Sync any agenda files and files in ~/Dropbox/Notes to MobileOrg
(setq org-mobile-files (list 'org-agenda-files "~/Dropbox/Notes"))

; Where notes entered via MobileOrg go
(setq org-mobile-inbox-for-pull "~/Dropbox/Notes/new.org")

; MobileOrg's staging directory
(setq org-mobile-directory "~/Dropbox/MobileOrg")

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

;; (defun org-today-in-internal-format ()
;;   (let ((now (decode-time)))
;;     (encode-time 0 0 0 (nth 3 now) (nth 4 now) (nth 5 now))))

;; (defun org-agenda-deadline-today ()
;;   (interactive)
;;   (org-agenda-deadline nil (org-today-in-internal-format)))

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "<M-S-return>") 'my-org-insert-todo-heading)
            (define-key org-mode-map (kbd "C-c C-0") 'org-move-to-done-tree)
            (define-key org-mode-map (kbd "C-c \\") 'org-table-inplace-to-tsv)))

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



(provide 'org-config)
