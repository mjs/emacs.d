(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(appt-display-duration 30)
 '(appt-display-format (quote window))
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "browser-nw")
 '(clean-buffer-list-kill-buffer-names
   (quote
    ("*Help*" "*Apropos*" "*Man " "*Buffer List*" "*Compile-Log*" "*info*" "*vc*" "*vc-diff*" "*diff*" "*Org Agenda*")))
 '(clean-buffer-list-kill-never-buffer-names
   (quote
    ("*scratch*" "*Messages*" "*server*" "TODO.org" "*Org Agenda*")))
 '(clean-buffer-list-kill-never-regexps (quote ("^ \\*Minibuf-.*\\*$")))
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(compile-command "waf-build.sh")
 '(dabbrev-case-fold-search nil)
 '(flymake-number-of-errors-to-display 3)
 '(frame-background-mode (quote dark))
 '(global-hl-line-mode t)
 '(grep-command "rg --no-heading -e")
 '(grep-files-aliases
   (quote
    (("all" . "* .*")
     ("el" . "*.el")
     ("c" . "*.cc *.[ch]xx *.[ch]pp *.[cChH] *.CC *.HH *.[ch]++ *.inc")
     ("h" . "*.h")
     ("l" . "[Cc]hange[Ll]og*")
     ("m" . "[Mm]akefile*")
     ("tex" . "*.tex")
     ("texi" . "*.texi")
     ("asm" . "*.[sS]"))))
 '(grep-find-template
   "find . <X> -type f <F> -print0 | xargs -0 rg <C> --no-heading --maxdepth 1 -e <R>")
 '(grep-template "rg <C> --no-heading --maxdepth 1 -e <R> <F>")
 '(hippie-expand-try-functions-list
   (quote
    (try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-complete-lisp-symbol-partially try-complete-lisp-symbol try-expand-line)))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(initial-buffer-choice t)
 '(ispell-program-name "/usr/bin/ispell")
 '(jedi:complete-on-dot nil)
 '(jedi:tooltip-method nil)
 '(js2-auto-indent-flag nil)
 '(js2-basic-offset 4)
 '(js2-electric-keys (quote nil))
 '(js2-mirror-mode nil)
 '(magit-push-arguments (quote ("--force-with-lease")))
 '(menu-bar-mode nil)
 '(midnight-hook
   (quote
    (refresh-file-cache recentf-save-list clean-buffer-list tramp-cleanup-all-connections)))
 '(midnight-mode t nil (midnight))
 '(mouse-avoidance-mode nil nil (avoid))
 '(org-agenda-files nil)
 '(org-hide-leading-stars t)
 '(org-odd-levels-only t)
 '(pastebin-default-domain "paste.ubuntu.com")
 '(pastebin-domain-versions
   (quote
    (("pastebin.com" "/api_public.php")
     ("pastebin.example.com" "/pastebin.php")
     ("paste.ubuntu.com" ""))))
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
 '(tool-bar-mode nil)
 '(tramp-ssh-controlmaster-options "")
 '(transient-mark-mode (quote (only . t)))
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
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#eeeeee" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(compilation-line-number ((t (:inherit font-lock-keyword-face :underline nil))))
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
 '(magit-item-highlight ((t (:background "#333333"))))
 '(org-level-1 ((t (:inherit outline-1))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "gold"))))
 '(org-todo ((t (:foreground "goldenrod"))))
 '(org-upcoming-deadline ((t (:foreground "dark goldenrod"))))
 '(org-warning ((t (:foreground "light goldenrod"))))
 '(post-emoticon-face ((t (:background "black" :foreground "white"))))
 '(post-signature-text-face ((((class color) (background dark)) (:foreground "lightblue"))))
 '(py-builtins-face ((t (:foreground "orange red"))) t)
 '(py-pseudo-keyword-face ((t (:foreground "dark orange"))) t)
 '(ripgrep-hit-face ((t (:inherit compilation-info :underline nil))))
 '(sml-modeline-end-face ((t (:inherit match :foreground "white"))))
 '(sml-modeline-vis-face ((t (:inherit region :foreground "white"))))
 '(tex-verbatim ((t (:foreground "gray")))))
