(require 'bats-relnotes)
(require 'bats-crontab)
(require 'org-bats-config)

;; XXX this should be smart and just issue a message if the tag file isn't there
(visit-tags-table "~/source/TAGS")

(setq file-cache-site-directories '("~/git.0/source"
                                    "~/puppet-trunk"))
