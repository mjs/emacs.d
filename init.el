(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-firefox))
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(cua-remap-control-v nil)
 '(cua-remap-control-z nil)
 '(dabbrev-case-fold-search nil)
 '(frame-background-mode (quote dark))
 '(global-hl-line-mode t)
 '(hippie-expand-try-functions-list (quote (try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-complete-lisp-symbol-partially try-complete-lisp-symbol try-expand-line)))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(initial-buffer-choice t)
 '(jabber-chat-fill-long-lines nil)
 '(js2-auto-indent-flag nil)
 '(js2-basic-offset 4)
 '(js2-electric-keys (quote nil))
 '(js2-mirror-mode nil)
 '(mouse-avoidance-mode nil nil (avoid))
 '(org-hide-leading-stars t)
 '(org-odd-levels-only t)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(visible-bell t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#eeeeee" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil))))
 '(cursor ((t (:background "yellow"))))
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(font-lock-comment-face ((nil (:foreground "#99968b"))))
 '(font-lock-function-name-face ((t (:foreground "#cae682" :weight normal))))
 '(font-lock-keyword-face ((nil (:foreground "#8ac6f2"))))
 '(font-lock-string-face ((nil (:foreground "#95e454"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:foreground "ForestGreen" :weight bold))))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background dark)) nil)))
 '(hl-line ((t (:background "#222222"))))
 '(magit-diff-add ((((class color) (background dark)) (:foreground "lime green"))))
 '(magit-diff-del ((((class color) (background dark)) (:foreground "IndianRed"))))
 '(magit-diff-hunk-header ((t (:inherit magit-header :foreground "yellow" :slant italic))))
 '(magit-item-highlight ((((class color) (background dark)) (:background "#070707"))))
 '(org-level-1 ((t (:inherit outline-1 :overline "darkgray"))))
 '(py-builtins-face ((t (:foreground "orange red"))) t)
 '(py-pseudo-keyword-face ((t (:foreground "dark orange"))) t))

;; get rid of useless chrome
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; make more elisp mods available
(add-to-list 'load-path "~/.emacs.d/own")
(add-to-list 'load-path "~/.emacs.d/external")
(add-to-list 'load-path "~/.emacs.d/external/jabber")

;; allow access to dependent Python libraries (Pymacs etc)
(setenv "PYTHONPATH" (expand-file-name "~/.emacs.d/pylib"))

(require 'filecache)
(require 'flymake-pyflakes)
(require 'flymake-config)
(require 'file-misc)
(require 'c-misc)
(require 'misc-misc)
(require 'insert-timestamp)
(require 'csv-mode)
(require 'jabber-autoloads)
(require 'org-config)

;; Provide a menu of tags when there's multiple matches
(require 'etags-select)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

;; make switch buffer and find file much nicer
(ido-mode 1)
(ido-everywhere 1)

;; buffer list on crank
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Keep temporary and backup files in a sane place
(setq backup-directory-alist '(("." . "~/.emacs.d/tmp/backup")))
(setq tramp-auto-save-directory "~/.emacs.d/tmp/autosave")
(setq tramp-backup-directory-alist backup-directory-alist)
;; (setq auto-save-file-name-transforms `((".*" "~/.emacs.d/tmp/autosave" t)))

;; remember what I was doing before
(recentf-mode 1)
(defalias 'rf 'recentf-open-files)
(global-set-key (kbd "C-x C-r") 'recent-open-files)

;; Show trailing whitespace when working with Python files
(add-hook 'python-mode-hook
          (lambda () (setq show-trailing-whitespace t)))

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


(setq viper-mode t)                ; enable Viper at load time
;; These 2 are special and need to be set before viper is loaded. They don't
;; work if set in the customize block.
(setq viper-ex-style-editing nil)  ; can backspace past start of insert / line
(setq viper-ex-style-motion nil)   ; can move past end of line
(require 'viper)                   ; load Viper
(require 'vimlike)                 ; vim emulation

(defadvice viper-maybe-checkout (around viper-checkin-fix activate)
  "Stop viper from trying to do anything VC related"
  nil)


;; python mode settings
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
				   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; Pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

;; Enable PySmell
;;(require 'pysmell)
;;(add-hook 'python-mode-hook (lambda () (pysmell-mode 1)))

;; nice completion
(global-set-key (kbd "M-/") 'hippie-expand)

;; enable case-transformers
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; tabs are for babies and Aztecs (and Makefiles, I guess)
(setq-default indent-tabs-mode nil)

;; Show the column number in the mode line
(column-number-mode)

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

;; IPython support!
(setq ipython-command "ipython")
(require 'ipython)

;; yasnippet
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;; javascript mode from stevey
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; load host/site specific config if it exists
(let ((site-lib "~/.emacs.d/site.el"))
  (message "loading site.el")
  (if (file-exists-p site-lib) (load-file site-lib)))

(server-start)
