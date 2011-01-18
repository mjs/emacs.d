(require 'bats)
(require 'org-bats-config)

(visit-tags-table "~/source/TAGS")

(setq jabber-account-list '(("msmits@lxchat" (:connection-type . ssl))
                            ("menno.smits@gmail.com"
                              (:network-server . "talk.google.com")
                              (:port . 443)
                              (:connection-type . ssl))
                            ))
