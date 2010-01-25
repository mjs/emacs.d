(require 'post)

(setq auto-mode-alist (append '(("/tmp/mutt-" . post-mode)) auto-mode-alist))

(provide 'post-config)
