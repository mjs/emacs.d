;;; magithub-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "magithub" "magithub.el" (23133 3678 80547
;;;;;;  685000))
;;; Generated autoloads from magithub.el
 (autoload 'magithub-dispatch-popup "magithub" nil t)

;;;***

;;;### (autoloads nil "magithub-comment" "magithub-comment.el" (23133
;;;;;;  3678 84547 184000))
;;; Generated autoloads from magithub-comment.el

(autoload 'magithub-comment-new "magithub-comment" "\
Comment on ISSUE in a new buffer.
If prefix argument DISCARD-DRAFT is specified, the draft will not
be considered.

If INITIAL-CONTENT is specified, it will be inserted as the
initial contents of the reply if there is no draft.

\(fn ISSUE &optional DISCARD-DRAFT INITIAL-CONTENT)" t nil)

;;;***

;;;### (autoloads nil "magithub-dash" "magithub-dash.el" (23133 3678
;;;;;;  104544 678000))
;;; Generated autoloads from magithub-dash.el

(autoload 'magithub-dashboard "magithub-dash" "\
View your Github dashboard.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "magithub-edit-mode" "magithub-edit-mode.el"
;;;;;;  (23133 3678 60550 191000))
;;; Generated autoloads from magithub-edit-mode.el

(autoload 'magithub-edit-mode "magithub-edit-mode" "\
Major mode for editing Github issues and pull requests.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "magithub-issue-tricks" "magithub-issue-tricks.el"
;;;;;;  (23133 3678 108544 177000))
;;; Generated autoloads from magithub-issue-tricks.el

(autoload 'magithub-pull-request-merge "magithub-issue-tricks" "\
Merge PULL-REQUEST with ARGS.
See `magithub-pull-request--completing-read'.  If point is on a
pull-request object, that object is selected by default.

\(fn PULL-REQUEST &optional ARGS)" t nil)

;;;***

;;;### (autoloads nil "magithub-issue-view" "magithub-issue-view.el"
;;;;;;  (23133 3678 92546 182000))
;;; Generated autoloads from magithub-issue-view.el

(autoload 'magithub-issue-view "magithub-issue-view" "\
View ISSUE in a new buffer.

\(fn ISSUE)" t nil)

;;;***

;;;### (autoloads nil nil ("magithub-ci.el" "magithub-core.el" "magithub-faces.el"
;;;;;;  "magithub-issue-post.el" "magithub-issue.el" "magithub-label.el"
;;;;;;  "magithub-notification.el" "magithub-orgs.el" "magithub-pkg.el"
;;;;;;  "magithub-proxy.el" "magithub-repo.el" "magithub-user.el")
;;;;;;  (23133 3678 116543 175000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; magithub-autoloads.el ends here
