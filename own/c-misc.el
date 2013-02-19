;; Basic customisation for C/C++ and related

; Use an up to date cc-mode
(add-to-list 'load-path "~/.emacs.d/cc-mode")

; Essential for C/C++ programming
(global-set-key (kbd "C-c o") 'ff-find-other-file)

(add-to-list 'auto-mode-alist '("\\.h$" . cc-mode))
(setq c-basic-offset 4)
(setq c-default-style '((c++-mode . "k&r") 
                        (java-mode . "java") 
                        (awk-mode . "awk") 
                        (other . "gnu")))

;(setq c-indent-comment-alist '((anchored-comment column . 0) (end-block space . 1) (cpp-end-block space . 2) (other align space . 1)))
;(setq c-offsets-alist '((case-label . 0) (arglist-close . 0) (innamespace . 0)))

(provide 'c-misc)
