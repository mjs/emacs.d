; This is requires so that racer can find Rust's stdlib code.
(setenv "RUST_SRC_PATH" "/home/menno/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")


(require 'rust-mode)
(require 'company)

(setq rust-format-on-save t)
(setq company-tooltip-align-annotations t)

(defun rust-customizations ()
  "Additional customizations for rust-mode."
  (racer-mode)
  (flycheck-rust-setup)
  (evil-define-key 'normal rust-mode-map (kbd "M-.") 'racer-find-definition)
  (evil-define-key 'normal rust-mode-map (kbd "M-,") 'pop-tag-mark)
  (evil-define-key 'normal rust-mode-map (kbd "C-c C-d") 'racer-describe))
(add-hook 'rust-mode-hook 'rust-customizations)

(defun racer-customizations ()
  "Additional customizations for racer."
  (eldoc-mode)
  (company-mode))
(add-hook 'racer-mode-hook 'racer-customizations)

(provide 'rust-config)
