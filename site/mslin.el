(require 'bats-relnotes)
(require 'bats-crontab)
(require 'org-bats-config)

;; Turn off vc-mode on this host. Doesn't work well over NFS
(setq vc-handled-backends nil)

(setq file-cache-site-directories '("~/Notes"))

;; Allow emacsclient to work from scomslin
(setq server-host "10.64.15.188")
(setq server-use-tcp t)
(setq server-window 'switch-to-buffer-other-frame)  ; open in new frame

(defadvice server-start (after copy-server-file activate)
  "Copy the server file to scomslin once the server has started"
  (interactive)
  (copy-file "~/.emacs.d/server/server" "~/scohome/tmp/server" t))
