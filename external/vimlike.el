;-*-Emacs-Lisp-*-
; Extensions to Viper that make it more Vim-like
;
; Heavily customised version of:
; http://common-lisp.net/project/vial/darcs/extended-viper/vimlike.el

(require 'advice)
(require 'redo)
(require 'cua-base)

;;;;;;;;; TODO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; * dG doesn't work
; * text object motions and commands: steal from modal-mode or vimpulse?
; * rename module to avoid confusion

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

; Map undo and redo from XEmacs' redo.el
(define-key viper-vi-global-user-map "u"    'undo)
(define-key viper-vi-global-user-map "\C-r" 'redo)

; Window manipulation
(define-key global-map "\C-w" (make-sparse-keymap))
(define-key global-map "\C-w\C-w" 'vimlike-cycle-windows)
(define-key global-map "\C-wo" 'delete-other-windows)
(define-key global-map "\C-wc" 'delete-window)

(defun vimlike-cycle-windows ()
  "Cycle point to another window."
  (interactive) 
  (select-window (next-window)))

(defun vimlike-jump-to-tag-at-point ()
  (interactive)
  (let ((tag (thing-at-point 'word)))
    (find-tag tag)))

(defun viper-search-for-word-at-point (forward)
  "Search forwards or backwards for the word under point."
  (let ((word (thing-at-point 'symbol)))
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

(defun vimlike-select-line ()
  "Select the whole current line - poor man's Vim 'V' binding"
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

;;
;; Override the default forward and backward paragraph functions to
;; act on whole lines and to remove the mark fiddling
;;
;; Hopefully I can ween myself off these at some point :)
;;

(defun viper-forward-paragraph (arg)
  "Forward paragraph."
  (interactive "P")
  ;(or (eq last-command this-command)
  ;    (push-mark nil t))
  (let ((val (viper-p-val arg))
	;; if you want d} operate on whole lines, change viper-getcom to
	;; viper-getCom below
	(com (viper-getCom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (forward-paragraph val)
    (if com
	(progn
	  (backward-char 1)
	  (viper-execute-com 'viper-forward-paragraph nil com)))))

(defun viper-backward-paragraph (arg)
  "Backward paragraph."
  (interactive "P")
  ;(or (eq last-command this-command)
  ;    (push-mark nil t))
  (let ((val (viper-p-val arg))
	;; if you want d{ operate on whole lines, change viper-getcom to
	;; viper-getCom below
	(com (viper-getCom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (backward-paragraph val)
    (if com
	(progn
	  (forward-char 1)
	  (viper-execute-com 'viper-backward-paragraph nil com)
	  (backward-char 1)))))

(provide 'vimlike)
