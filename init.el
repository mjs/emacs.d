;(package-initialize)
;; Move all the customize stuff elsewhere to reduce clutter.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Switch to sane starting directory (no matter where the daemon
;; starts from)
(cd (expand-file-name "~"))

(add-to-list 'load-path "~/.emacs.d/own")

(require 'package-config)

(require 'temp-config) ;; Put temporary files in sane locations.
(require 'elisp-utils)
(require 'evil-config)
(require 'text-misc)
(require 'text-config)
(require 'gui-config)
(require 'company-config)
(require 'clipboard-config)
(require 'lsp-config)
(require 'markdown-config)
(require 'magit-config)
(require 'uniquify)
(require 'projectile-config)
(require 'flycheck-config)
(require 'lisp-config)
(require 'python-config)
(require 'go-config)
(require 'rust-config)
(require 'yaml-config)
(require 'js-config)
(require 'c-misc)
(require 'misc-misc)
(require 'insert-timestamp)
(require 'org-config)
(require 'calc)
(require 'pastebinit)
(require 'dired-config)
(require 'gpg-config)
(require 'yasnippet-config)
(require 'csharp-config)
(require 'grep-config)
(require 'file-config)
(require 'misc-config)
(use-package lua-mode :straight t)
(use-package toml-mode :straight t)

(let ((site-lib (expand-file-name "~/.emacs.d/site.el")))
  (message "loading site.el")
  (if (file-exists-p site-lib) (load-file site-lib)))


(require 'ffc-config)    ; load after site config

(server-start)

(use-package edit-server :straight t)
(edit-server-start)
