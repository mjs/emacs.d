;; Move all the customize stuff elsewhere to reduce clutter.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Switch to sane starting directory (no matter where the daemon
;; starts from)
(cd (expand-file-name "~"))

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'package-config)

(require 'temp-config) ;; Put temporary files in sane locations.
(require 'gui-config)
(require 'elisp-utils)
(require 'text-utils)
(require 'evil-config)
(require 'text-config)
(require 'flycheck-config)
(require 'magit-config)
(require 'projectile-config)
(require 'company-config)
(require 'clipboard-config)
(require 'lsp-config)
(require 'markdown-config)
(require 'lisp-config)
(require 'python-config)
(require 'go-config)
(require 'rust-config)
(require 'yaml-config)
(require 'js-config)
(require 'c-config)
(require 'insert-timestamp)
(require 'org-config)
(require 'dired-config)
(require 'gpg-config)
(require 'yasnippet-config)
(require 'csharp-config)
(require 'grep-config)
(require 'file-config)
(require 'midnight-config)

(require 'calc)
(require 'uniquify)

(use-package lua-mode :straight t)
(use-package toml-mode :straight t)

(let ((site-lib (expand-file-name "~/.emacs.d/site.el")))
  (message "loading site.el")
  (if (file-exists-p site-lib) (load-file site-lib)))


(require 'ffc-config)    ; load after site config

(server-start)

;; Edit textareas in browsers in Emacs.
(use-package atomic-chrome
  :straight t
  :demand t
  :config
  (setq atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-start-server))
