(require 'bats-relnotes)
(require 'bats-crontab)
(require 'org-bats-config)

;; Turn off vc-mode on this host. Doesn't work well over NFS
(setq vc-handled-backends nil)

(setq file-cache-site-directories '("~/config"))

