(defun pastebinit ()
  "run pastebinit on current region or buffer, put the resulting url in default kill-ring."
  (interactive)
  (with-region-or-buffer (beg end)
   (shell-command-on-region beg end "pastebinit -a menn0"))
  (set-buffer "*Shell Command Output*")
  (copy-region-as-kill (point-min) (point-max)))

(provide 'pastebinit)
