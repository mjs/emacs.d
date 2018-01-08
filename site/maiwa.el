(require 'ffc)

;; run midnight mode hooks a bit more often given that this machine
;; isn't on at night
(setq midnight-period (* 6 60 60))

(add-to-list 'ffc-directories "~/go/src")
(add-to-list 'ffc-directories "~/cacophony")
(add-to-list 'ffc-directories "~/Dropbox/Presentations")
