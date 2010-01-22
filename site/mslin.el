(require 'bats)
(require 'org-config)

(visit-tags-table "~/source/TAGS")

(setq jabber-account-list '(("msmits@batsutil")
                            ("menno.smits@gmail.com"
                              (:network-server . "talk.google.com")
                              (:port . 443)
                              (:connection-type . ssl))
                            ))

;;(jabber-connect-all)
