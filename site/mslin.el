(require 'bats-relnotes)
(require 'bats-crontab)
(require 'org-bats-config)

;; XXX this should be smart and just issue a message if the tag file
;; isn't there
(visit-tags-table "~/source/TAGS")

;; Turn off vc-mode on this host. Doesn't work well over NFS
(setq vc-handled-backends nil)

(setq file-cache-site-directories '("~/config"))

;; Allow emacsclient to work from remote hosts (really just scomslin)
(setq server-host "10.64.15.188")
(setq server-use-tcp t)
(setq server-window 'switch-to-buffer-other-frame)  ; open in new frame

(defadvice server-start (after copy-server-file activate)
  "Copy the server file to scomslin once the server has started"
  (interactive)
  (copy-file "~/.emacs.d/server/server" "~/scohome/tmp/server" t))
