;; set true to skip non-essential (and slow) startup items
;; useful when restarting Emacs a lot to test some new config
(setq quick-start nil)

;; Move all the customize stuff elsewhere to reduce clutter.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Switch to the home directory (no matter where the daemon starts from)
(cd (expand-file-name "~"))

(setq temporary-file-directory "~/.emacs.d/tmp/")

;; make more elisp mods available
(add-to-list 'load-path "~/.emacs.d/own")
(add-to-list 'load-path "~/.emacs.d/external")
(add-to-list 'load-path "~/.emacs.d/external/orgmode/core")
(add-to-list 'load-path "~/.emacs.d/external/orgmode/contrib")
(add-to-list 'load-path "~/.emacs.d/elpa")

;; Allow the reset of setup to use packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(require 'elisp-utils)
(require 'evil-config)   ; Become like Vim!
(require 'tramp)
(require 'gui-config)
(require 'magit-config)
(require 'uniquify)
(require 'flymake-config)
(require 'lisp-config)
(require 'python-config)
(require 'go-config)
(require 'yaml-config)
(require 'js-config)
(require 'c-misc)
(require 'file-misc)
(require 'misc-misc)
(require 'insert-timestamp)
(require 'lua-mode)
(require 'org-config)
(require 'calc)
(require 'php-mode)
(require 'pastebinit)

(require 'longlines)
(defalias 'll 'longlines-mode)
(defalias 'tt 'toggle-truncate-lines)

(defalias 'rg 'ripgrep-regexp)

;; Crontab support
(autoload 'crontab-mode "crontab-mode")
(add-to-list 'auto-mode-alist '("\\.crontab$" . crontab-mode))

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'gitk)         ;; spawn gitk for the current file etc

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
(global-set-key (kbd "C-c g") 'show-file-name)
(global-set-key (kbd "C-c C-g") 'show-file-name)

;; Keep temporary and backup files in a sane place
(setq backup-directory-alist '(("." . "~/.emacs.d/tmp/backup")))
(setq tramp-auto-save-directory "~/.emacs.d/tmp/autosave")
(setq tramp-backup-directory-alist backup-directory-alist)
;; (setq auto-save-file-name-transforms `((".*" "~/.emacs.d/tmp/autosave" t)))

;; remember what I was doing before
(recentf-mode 1)
(defalias 'rf 'recentf-open-files)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(defalias 'rb 'revert-buffer)

(put 'narrow-to-region 'disabled nil)

;; Use the primary selection by default
(setq x-select-enable-clipboard nil)
(setq x-select-enable-primary t)
(setq mouse-drag-copy-region t)

;; Bindings for clipboard interaction
(global-set-key "\C-cc" 'clipboard-kill-ring-save)
(global-set-key "\C-cx" 'clipboard-kill-region)
(global-set-key "\C-cv" 'clipboard-yank)

(global-set-key "\C-x9" 'bury-buffer)

;; Navigation of grep results/errors
(global-set-key "\M-n" 'next-error)
(global-set-key "\M-p" 'previous-error)

(defun ff-find-other-file-other-window ()
  (interactive)
  (ff-find-other-file t))
(global-set-key (kbd "C-c O") 'ff-find-other-file-other-window)

(defun ff-delete-windows-and-find-other-file-other-window ()
  (interactive)
  (delete-other-windows)
  (ff-find-other-file-other-window))
(global-set-key (kbd "C-c C-o") 'ff-delete-windows-and-find-other-file-other-window)

;; Unique grep buffer per search
(require 'grep-a-lot)
(grep-a-lot-setup-keys)

;; highlighting to see all occurrences of a word in the buffer
(setq highlighted-word "")
(make-variable-buffer-local 'highlighted-word)

(defun highlight-word (word)
 (interactive (list (thing-at-point 'symbol)))
 (unhighlight-regexp (regexp-quote highlighted-word))
 (if (not (equal highlighted-word word))
     (progn
       (highlight-regexp (regexp-quote word))
       (setq highlighted-word word))
   (setq highlighted-word "")))

(global-set-key (kbd "C-x C-h") 'highlight-word)


; (pymacs-load "bugzilla" "bugz-")

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

(setq auto-mode-alist (cons '("\\.sqli$" . sql-mode) auto-mode-alist))

;; nice completion
(global-set-key (kbd "M-/") 'hippie-expand)

;; enable case-transformers
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(require 'epa-file)
(epa-file-enable)

;; tabs are for babies and Aztecs (and Makefiles, I guess)
(setq-default indent-tabs-mode nil)

;; Get OCD about whitespace
(setq ethan-wspace-face '(t (:background "#05ff00")))
(setq ethan-wspace-face-customized t)
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)

;; get rid of the multiple dired buffer problem
(require 'dired-single)

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'joc-dired-single-buffer)
  (define-key dired-mode-map [Mouse-1] 'joc-dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (joc-dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/external/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)
(setf yas/indent-line nil)  ; prevent annoying auto-indent behaviour

;; Haskell support
;; TODO: autoload
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
