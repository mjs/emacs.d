(require 'bats)
(require 'org-config)

(visit-tags-table "~/source/TAGS")

(setq jabber-account-list '(("msmits@batsutil")))
(jabber-connect-all)
