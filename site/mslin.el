(require 'bats)
(require 'org-bats-config)

;; XXX this should be smart and just issue a message if the tag file
;; isn't there
(visit-tags-table "~/source/TAGS")

(setq jabber-account-list '(("msmits@lxchat" (:connection-type . ssl))
                            ("menno.smits@gmail.com"
                              (:network-server . "talk.google.com")
                              (:port . 443)
                              (:connection-type . ssl))
                            ))
