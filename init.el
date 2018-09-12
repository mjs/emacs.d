;; Move all the customize stuff elsewhere to reduce clutter.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Switch to sane starting directory (no matter where the daemon
;; starts from)
(cd (expand-file-name "~"))

;; Make more elisp mods available.
(add-to-list 'load-path "~/.emacs.d/own")
(add-to-list 'load-path "~/.emacs.d/external")
;; TODO: use officially packaged orgmode
(add-to-list 'load-path "~/.emacs.d/external/orgmode/core")
(add-to-list 'load-path "~/.emacs.d/external/orgmode/contrib")

;; Put temporary files in sane locations.
(require 'temp-config)

;; Allow the reset of setup to use elpa packages
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa stable" . "https://stable.melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(require 'elisp-utils)
(require 'evil-config)   ; Become like Vim!
(require 'gui-config)
(require 'company-config)
(require 'clipboard-config)
(require 'text-config)
(require 'magit-config)
(require 'gitk)         ;; spawn gitk for the current file etc
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
