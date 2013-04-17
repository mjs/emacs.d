;; Basic customisation for C/C++ and related

; Essential for C/C++ programming
(global-set-key (kbd "C-c o") 'ff-find-other-file)

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(setq c-basic-offset 4)
(setq c-default-style '((c++-mode . "k&r") 
                        (java-mode . "java") 
                        (awk-mode . "awk") 
                        (other . "gnu")))


(defun c-customizations ()
  "Additional customizations for C like modes"
  (define-key c-mode-base-map (kbd "<f9>") 'compile))

(eval-after-load 'cc-mode '(c-customizations))
  
(provide 'c-misc)
