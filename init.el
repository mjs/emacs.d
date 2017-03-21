;; Set true to skip non-essential (and slow) startup items
;; useful when restarting Emacs a lot to test some new config.
(defvar quick-start nil)

;; Move all the customize stuff elsewhere to reduce clutter.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Switch to the home directory (no matter where the daemon starts from)
(cd (expand-file-name "~"))

;; Make more elisp mods available.
(add-to-list 'load-path "~/.emacs.d/own")
(add-to-list 'load-path "~/.emacs.d/external")
;; TODO: use officially packaged orgmode
(add-to-list 'load-path "~/.emacs.d/external/orgmode/core")
(add-to-list 'load-path "~/.emacs.d/external/orgmode/contrib")

; XXX is this necessary?
(add-to-list 'load-path "~/.emacs.d/elpa") 

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
(require 'flymake-config)
(require 'lisp-config)
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
(require 'text-config)
(require 'grep-config)
(require 'file-config)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; TODO: remove?
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; GPG assistant
(require 'epa-file)
(epa-file-enable)

(global-set-key "\C-x9" 'bury-buffer)

;; nice completion
(global-set-key (kbd "M-/") 'hippie-expand)

;; yasnippet
;; TODO: use package
(add-to-list 'load-path "~/.emacs.d/external/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)
(setf yas/indent-line nil)  ; prevent annoying auto-indent behaviour

;; Haskell support
;; XXX: remove
(load "~/.emacs.d/external/haskell/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(let ((site-lib (expand-file-name "~/.emacs.d/site.el")))
  (message "loading site.el")
  (if (file-exists-p site-lib) (load-file site-lib)))

;; (defun vc-git-annotate-command (file buf &optional rev)
;;   (let ((name (file-relative-name file)))
;;     (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))

(unless quick-start
  (require 'filecache-config)    ; load after site config

  (server-start)

  ;; Allow editing from Chrome
  (require 'edit-server)
  (edit-server-start))

(message "init.el: end")
