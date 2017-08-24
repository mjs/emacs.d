(require 'rust-mode)

(setq rust-format-on-save t)
(setq company-tooltip-align-annotations t)

(defun rust-customizations ()
  "Additional customizations for rust-mode"
  (racer-mode)
  (eldoc-mode)
  (evil-define-key 'normal rust-mode-map (kbd "M-.") 'racer-find-definition)
  (evil-define-key 'normal rust-mode-map (kbd "M-,") 'pop-tag-mark))
(add-hook 'rust-mode-hook 'rust-customizations)

(defun racer-customizations ()
  (company-mode)
  (eldoc-mode))
(add-hook 'racer-mode-hook 'racer-customizations)

(provide 'rust-config)
