(require 'rust-mode)

(setq rust-format-on-save t)
(setq company-tooltip-align-annotations t)

(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(define-key rust-mode-map (kbd "M-.") 'racer-find-definition)

(defun rust-customizations ()
  "Additional customizations for rust-mode"
  (racer-mode)
  (company-mode)
  (eldoc-mode))

(add-hook 'rust-mode-hook 'rust-customizations)

(provide 'rust-config)
