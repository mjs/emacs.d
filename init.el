;(package-initialize)
;; Move all the customize stuff elsewhere to reduce clutter.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Switch to sane starting directory (no matter where the daemon
;; starts from)
(cd (expand-file-name "~"))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
j      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-to-list 'load-path "~/.emacs.d/own")
(add-to-list 'load-path "~/.emacs.d/external")
(add-to-list 'load-path "~/.emacs.d/external/use-package")

(require 'use-package)

;; Put temporary files in sane locations.
(require 'temp-config)

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
(require 'toml-mode)
(require 'js-config)
(require 'c-misc)
(require 'misc-misc)
(require 'insert-timestamp)
(require 'lua-mode)
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

(let ((site-lib (expand-file-name "~/.emacs.d/site.el")))
  (message "loading site.el")
  (if (file-exists-p site-lib) (load-file site-lib)))


(require 'ffc-config)    ; load after site config

(server-start)

(require 'edit-server)
(edit-server-start)
