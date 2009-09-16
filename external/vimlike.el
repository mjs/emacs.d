;-*-Emacs-Lisp-*-
; Extensions to Viper that make it more Vim-like
;
; Heavily customised version of:
; http://common-lisp.net/project/vial/darcs/extended-viper/vimlike.el
;
; The major changes are removal of visual mode. Visual selection are
; lightweight and tie in with more tightly with normal Emacs region
; operations. This is often done by overriding Viper bindings with
; functions that call standard Emacs functions.

(require 'advice)
(require 'redo)
(require 'cua-base)

;;;;;;;;; TODO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; * rather than override all the viper stuff to make marks work better try advicing away viper-deactivate-mark
; * when selecting a region, include the char under the cursor (or move one further)
; * text object motions and commands
; * rename module to avoid confusion
; * rectange support using rect-mark (CTRL-V to start, CTRL-P or something yank)

; Viper setup
(viper-buffer-search-enable)
(setq viper-syntax-preference 'emacs)
(setq viper-auto-indent t) 
(setq viper-ex-style-motion nil)        ; can move past the end of the line

; make :n cycle through buffers on the current window
(setq ex-cycle-other-window nil) 

; Keymaps to make Viper more Vim-like
; Command mode keys
(define-key viper-vi-global-user-map "gf"   'find-file-at-point)
(define-key viper-vi-global-user-map "gg"   'vimlike-beginning-of-buffer)
(define-key viper-vi-global-user-map "G"    'vimlike-end-of-buffer)
(define-key viper-vi-global-user-map "zt"   'viper-line-to-top)
(define-key viper-vi-global-user-map "zb"   'viper-line-to-bottom)
(define-key viper-vi-global-user-map "zz"   'viper-line-to-middle)
(define-key viper-vi-global-user-map "*"    'vimlike-search-forward-for-word-at-point) 
(define-key viper-vi-global-user-map "#"    'vimlike-search-backward-for-word-at-point) 
(define-key viper-vi-global-user-map "\C-]" 'vimlike-jump-to-tag-at-point)
(define-key viper-vi-global-user-map "\C-t" 'pop-tag-mark)
(define-key viper-vi-global-user-map "v"    'cua-set-mark)
(define-key viper-vi-global-user-map "V"    'vimlike-select-line)
(define-key viper-vi-global-user-map "x"    'vimlike-delete-char)
(define-key viper-vi-global-user-map "d"    'vimlike-delete)
(define-key viper-vi-global-user-map "y"    'vimlike-yank)
(define-key viper-vi-global-user-map "}"    'vimlike-forward-paragraph)
(define-key viper-vi-global-user-map "{"    'vimlike-backward-paragraph)

; Map undo and redo from XEmacs' redo.el
(define-key viper-vi-global-user-map "u"    'undo)
(define-key viper-vi-global-user-map "\C-r" 'redo)

; Window manipulation
(define-key global-map "\C-w" (make-sparse-keymap))
(define-key global-map "\C-w\C-w" 'viper-cycle-windows)
(define-key global-map "\C-wo" 'delete-other-windows)
(define-key global-map "\C-wc" 'delete-window)

; Insert mode keys
; Vim-like completion key
(define-key viper-insert-global-user-map "\C-n" 'dabbrev-expand)

;;; Additional Ex mode features.
;;; ex-token-alist is defined as a constant, but it appears I can safely push values to it!
(defvar viper-extra-ex-commands '(
      ("sp" "split")
      ("split" (viper-split-window))
      ("b" "buffer")
      ("bd" "bdelete")
      ("bdelete" (viper-kill-current-buffer))
      ("bn" "next")
      ("vs" "vsplit")   ; Emacs and Vim use inverted naming conventions for splits :)
      ("vsplit" (viper-split-window-horizontally))
))
(setq ex-token-alist (append viper-extra-ex-commands ex-token-alist))

(defun viper-split-window (&optional file)
  (split-window)
  (ex-edit file))

(defun viper-split-window-horizontally (&optional file)
  (split-window-horizontally)
  (ex-edit file))

;;; Functions that the new key mappings use
(defun vimlike-jump-to-tag-at-point ()
  (interactive)
  (let ((tag (thing-at-point 'word)))
    (find-tag tag)))

(defun viper-goto-first-line ()
  "Send point to the start of the first line."
  (interactive)
  (viper-goto-line 1)) 

(defun viper-kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil)) 

(defun viper-cycle-windows ()
  "Cycle point to another window."
  (interactive) 
  (select-window (next-window)))

(defun viper-search-for-word-at-point (forward)
  "Search forwards or bacward for the word under point."
  (let ((word (thing-at-point 'word)))
    (setq viper-s-string word)
    (setq viper-s-forward forward)
    (viper-search word forward 1))) 

(defun vimlike-search-forward-for-word-at-point ()
  (interactive)
  (viper-search-for-word-at-point t))

(defun vimlike-search-backward-for-word-at-point ()
  (interactive)
  (viper-search-for-word-at-point nil))

;;; Make some Emacs Lisp mode stuff more like Vim
;;; This hook function will run when lisp mode is invoked
(defun viper-mode-lisp-hook ()
  "let '-' be part of words, so word commands will work properly"
  (modify-syntax-entry ?- "word"))
  
(add-hook 'lisp-mode-hook 'viper-mode-lisp-hook)
(add-hook 'emacs-lisp-mode-hook 'viper-mode-lisp-hook) 

; Function to make brace highlighting like Vim's
; Contributed by Alessandro Piras
; XXX buggy - now not activated by default
(defadvice show-paren-function (around viper-shop-paren-function disable)
  (if viper-vi-basic-minor-mode
      (cond
       ((= (char-after (point)) ?\))
	(forward-char)
	ad-do-it
	(backward-char))
       ((= (char-after (- (point) 1)) ?\)) nil)
       (t ad-do-it))
    ad-do-it))

(defadvice viper-deactivate-mark (around vimlike-stop-viper-deactivating-mark activate)
  (message "blocked mark DEactivate"))

(defadvice viper-activate-mark (around vimlike-stop-viper-activating-mark activate)
  (message "blocked mark activate"))

(defun vimlike-delete-char (arg)
  (interactive "P")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (viper-delete-char arg)))

(defun vimlike-delete (arg)
  (interactive "P")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (viper-command-argument arg)))

(defun vimlike-yank (arg)
  (interactive "P")
  (if (use-region-p)
      (copy-region-as-kill (region-beginning) (region-end))
    (viper-command-argument arg)))

(defun vimlike-select-line ()
  (interactive)
  (if (use-region-p)
      (cua-set-mark)
    (progn 
      (beginning-of-line)
      (cua-set-mark)
      (end-of-line)
      (forward-line))))

(defun vimlike-beginning-of-buffer ()
  (interactive)
  (goto-char (point-min)))

(defun vimlike-end-of-buffer ()
  (interactive)
  (goto-char (point-max)))

(defun vimlike-forward-paragraph (arg)
  (interactive "P")
  (forward-paragraph (viper-p-val arg)))

(defun vimlike-backward-paragraph (arg)
  (interactive "P")
  (backward-paragraph (viper-p-val arg)))


(provide 'vimlike)
