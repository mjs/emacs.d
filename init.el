(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(appt-display-duration 30)
 '(appt-display-format (quote window))
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "run-chrome")
 '(clean-buffer-list-kill-buffer-names (quote ("*Help*" "*Apropos*" "*Man " "*Buffer List*" "*Compile-Log*" "*info*" "*vc*" "*vc-diff*" "*diff*" "*Org Agenda*")))
 '(clean-buffer-list-kill-never-buffer-names (quote ("*scratch*" "*Messages*" "*server*" "TODO.org" "*Org Agenda*")))
 '(clean-buffer-list-kill-never-regexps (quote ("^ \\*Minibuf-.*\\*$")))
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(cua-remap-control-v nil)
 '(cua-remap-control-z nil)
 '(dabbrev-case-fold-search nil)
 '(frame-background-mode (quote dark))
 '(global-hl-line-mode t)
 '(grep-files-aliases (quote (("all" . "* .*") ("el" . "*.el") ("c" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++") ("h" . "*.h") ("l" . "[Cc]hange[Ll]og*") ("m" . "[Mm]akefile*") ("tex" . "*.tex") ("texi" . "*.texi") ("asm" . "*.[sS]"))))
 '(hippie-expand-try-functions-list (quote (try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-complete-lisp-symbol-partially try-complete-lisp-symbol try-expand-line)))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(inferior-lisp-program "java -cp /home/msmits/downloads/clojure-1.1.0/clojure.jar clojure.main")
 '(initial-buffer-choice t)
 '(ispell-program-name "/usr/bin/ispell")
 '(js2-auto-indent-flag nil)
 '(js2-basic-offset 4)
 '(js2-electric-keys (quote nil))
 '(js2-mirror-mode nil)
 '(midnight-mode t nil (midnight))
 '(mouse-avoidance-mode nil nil (avoid))
 '(org-agenda-files nil)
 '(org-hide-leading-stars t)
 '(org-odd-levels-only t)
 '(recentf-max-saved-items 100)
 '(ropemacs-enable-shortcuts nil)
 '(shell-file-name "/bin/bash")
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(sml-modeline-borders nil)
 '(sml-modeline-len 14)
 '(sml-modeline-mode t)
 '(sml-modeline-numbers (quote line-numbers))
 '(tags-revert-without-query t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(uniquify-min-dir-content 0)
 '(visible-bell t)
 '(x-select-enable-primary t)
 '(xgit-use-index (quote always)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#eeeeee" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil))))
 '(cursor ((t (:background "yellow"))))
 '(diff-added ((t (:inherit diff-changed :foreground "#00dd00"))))
 '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:weight bold))))
 '(diff-header ((t (:background "grey11"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "#dd0000"))))
 '(error ((t (:foreground "Pink"))))
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
 '(org-level-1 ((t (:inherit outline-1))))
 '(org-todo ((t (:foreground "goldenrod"))))
 '(org-upcoming-deadline ((t (:foreground "dark goldenrod"))))
 '(org-warning ((t (:foreground "light goldenrod"))))
 '(post-emoticon-face ((t (:background "black" :foreground "white"))))
 '(post-signature-text-face ((((class color) (background dark)) (:foreground "lightblue"))))
 '(py-builtins-face ((t (:foreground "orange red"))) t)
 '(py-pseudo-keyword-face ((t (:foreground "dark orange"))) t)
 '(sml-modeline-end-face ((t (:inherit match :foreground "white"))))
 '(sml-modeline-vis-face ((t (:inherit region :foreground "white"))))
 '(tex-verbatim ((t (:foreground "gray")))))

;; set true to skip non-essential (and slow) startup items
;; useful when restarting Emacs a lot to test some new config
(setq quick-start nil)

;; make more elisp mods available
(add-to-list 'load-path "~/.emacs.d/own")
(add-to-list 'load-path "~/.emacs.d/external")
(add-to-list 'load-path "~/.emacs.d/external/orgmode/core")
(add-to-list 'load-path "~/.emacs.d/external/orgmode/contrib")
(add-to-list 'load-path "~/.emacs.d/elpa")

;; get rid of useless chrome
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Add some useful chrome
(require 'sml-modeline)
(sml-modeline-mode t)

;; TODO: more autoloads!
(require 'tramp)
(require 'evil-config)   ; Become like Vim!
(require 'uniquify)
(require 'flymake-config)
(require 'python-config)
(require 'js-config)
(require 'c-misc)
(require 'file-misc)
(require 'misc-misc)
(require 'insert-timestamp)
(require 'lua-mode)
(require 'org-config)
(unless quick-start
  (require 'filecache-config))

(require 'package)
(package-initialize)

(defalias 'll 'longlines-mode)
(defalias 'tt 'toggle-truncate-lines)

(defun email-config ()
  (text-mode)
  (set-fill-column 72)
  (longlines-mode)
  (flyspell-mode))

(add-to-list 'auto-mode-alist '("\\.eml$" . email-config))

;; Crontab support
(autoload 'crontab-mode "crontab-mode")
(add-to-list 'auto-mode-alist '("\\.crontab$" . crontab-mode))

;; Git support
(add-to-list 'load-path "~/.emacs.d/external/magit")
(require 'magit)
(require 'magit-svn)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x l") 'magit-log)
(require 'gitk)         ;; spawn gitk for the current file etc

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
(global-set-key (kbd "C-c g") 'show-file-name)
(global-set-key (kbd "C-c C-g") 'show-file-name)

;; make switch buffer and find file much nicer
(ido-mode 1)
(ido-everywhere 1)

;; Set up smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)    ;; This is the default M-x.

;; buffer list on crank
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Essential for C/C++ programming
(global-set-key (kbd "C-c o") 'ff-find-other-file)

;; Keep temporary and backup files in a sane place
(setq backup-directory-alist '(("." . "~/.emacs.d/tmp/backup")))
(setq tramp-auto-save-directory "~/.emacs.d/tmp/autosave")
(setq tramp-backup-directory-alist backup-directory-alist)
;; (setq auto-save-file-name-transforms `((".*" "~/.emacs.d/tmp/autosave" t)))

;; remember what I was doing before
(recentf-mode 1)
(defalias 'rf 'recentf-open-files)

(defalias 'rb 'revert-buffer)

(put 'narrow-to-region 'disabled nil)

;; Interaction with the system clipboard
(global-set-key "\C-cc" 'clipboard-kill-ring-save)
(global-set-key "\C-cx" 'clipboard-kill-region)
(global-set-key "\C-cv" 'clipboard-yank)

(global-set-key "\C-x9" 'bury-buffer)

;; Navigation of grep results/errors
(global-set-key "\M-n" 'next-error)
(global-set-key "\M-p" 'previous-error)

;; Unique grep buffer per search
(require 'grep-a-lot)
(grep-a-lot-setup-keys)

;; Possibly useful
;; (setq x-select-enable-clipboard t)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

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

;; Show the column number in the mode line and line numbers down the side
(column-number-mode)

;; Get OCD about whitespace
(setq ethan-wspace-face '(t (:background "#05ff00")))
(setq ethan-wspace-face-customized t)
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)

;; Make window sizing saner
(defun enlarge-window-horiz-quick (arg)
  (interactive "P")
  (enlarge-window-horizontally 3))

(defun shrink-window-horiz-quick (arg)
  (interactive "P")
  (shrink-window-horizontally 3))

(global-set-key (kbd "C-_") 'shrink-window-horiz-quick)
(global-set-key (kbd "C-+") 'enlarge-window-horiz-quick)
(global-set-key (kbd "C--") 'shrink-window)
(global-set-key (kbd "C-=") 'enlarge-window)

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

;; Haskell support
;; TODO: autoload
(load "~/.emacs.d/external/haskell/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(defun eval-buffer-and-run-ert ()
  (interactive)
  (eval-buffer)
  (ert "t"))
(global-set-key [f5] 'eval-buffer-and-run-ert)

(let ((site-lib "~/.emacs.d/site.el"))
  (message "loading site.el")
  (if (file-exists-p site-lib) (load-file site-lib)))

(unless quick-start
  (server-start)
  ;; Allow editing from Chrome
  (require 'edit-server)
  (edit-server-start))
