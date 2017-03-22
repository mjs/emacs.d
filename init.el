;; Set true to skip non-essential (and slow) startup items
;; useful when restarting Emacs a lot to test some new config.
(defvar quick-start nil)

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
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(require 'elisp-utils)
(require 'evil-config)   ; Become like Vim!
(require 'gui-config)
(require 'clipboard-config)
(require 'magit-config)
(require 'gitk)         ;; spawn gitk for the current file etc
(require 'uniquify)
(require 'lisp-config)
(require 'flymake-config)
(require 'python-config)
(require 'go-config)
(require 'yaml-config)
(require 'js-config)
(require 'c-misc)
(require 'misc-misc)
(require 'insert-timestamp)
(require 'lua-mode)
(require 'org-config)
(require 'calc)
(require 'php-mode)
(require 'pastebinit)
(require 'dired-config)
(require 'gpg-config)
(require 'yasnippet-config)
(require 'csharp-config)
(require 'markdown-config)
(require 'text-config)
(require 'grep-config)
(require 'file-config)
(require 'misc-config)

(let ((site-lib (expand-file-name "~/.emacs.d/site.el")))
  (message "loading site.el")
  (if (file-exists-p site-lib) (load-file site-lib)))

(unless quick-start
  (require 'filecache-config)    ; load after site config

  (server-start)

  ;; Allow editing from Chrome
  (require 'edit-server)
  (edit-server-start))

