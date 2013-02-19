(require 'bats-relnotes)
(require 'bats-crontab)
(require 'org-bats-config)

;; XXX this should be smart and just issue a message if the tag file
;; isn't there
(visit-tags-table "~/source/TAGS")

;; Turn off vc-mode on this host. Doesn't work well over NFS
(setq vc-handled-backends nil)

(setq file-cache-site-directories '("~/scohome/git.0/source"
                                    "~/puppet-trunk"
                                    "~/Notes"))

;; Allow emacsclient to work from remote hosts (really just scomslin)
(setq server-host "10.64.15.188")
(setq server-use-tcp t)
; XXX need to post advise server-start to copy the server auth file to scohome
