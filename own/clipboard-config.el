;; Use the primary selection by default
(setq x-select-enable-clipboard nil)
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region t)

(defun clipboard-copy ()
  "Copy the region to the clipboard.  If no region is selected, copy the buffer."
  (interactive)
  (if (use-region-p)
      (clipboard-kill-ring-save (region-beginning) (region-end))
    (clipboard-kill-ring-save (point-min) (point-max))))

;; Bindings for clipboard interaction
(global-set-key "\C-cc" 'clipboard-copy)
(global-set-key "\C-cx" 'clipboard-kill-region)
(global-set-key "\C-cv" 'clipboard-yank)


(provide 'clipboard-config)
