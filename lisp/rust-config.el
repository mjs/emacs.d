; This is requires so that racer can find Rust's stdlib code.
(setenv "RUST_SRC_PATH" "/home/menno/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")

(use-package rust-mode
  :straight t
  :config
  (setq rust-format-on-save t))

(use-package racer
  :straight t
  :hook (rust-mode . racer-mode)
  :hook (racer-mode . eldoc-mode)
  :hook (racer-mode . company-mode)
  :config
  (evil-define-key 'normal rust-mode-map (kbd "M-.") 'racer-find-definition)
  (evil-define-key 'normal rust-mode-map (kbd "M-,") 'pop-tag-mark)
  (evil-define-key 'normal rust-mode-map (kbd "C-c C-d") 'racer-describe))

(use-package flycheck-rust
  :straight t
  :hook (rust-mode . flycheck-rust-setup))

(provide 'rust-config)
