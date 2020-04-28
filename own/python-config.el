;; allow access to dependent Python libraries
(setenv "PYTHONPATH" (format "%s:%s"
                             (expand-file-name "~/.emacs.d/pylib")
                             (getenv "PYTHONPATH")))

(use-package python
  :straight (:type built-in)  ; built-in Emacs version (good as of Emacs 24.3)

  :config

  ;; Include underscores when matching words (not sure why this isn't the default)
  (modify-syntax-entry ?_ "w" python-mode-syntax-table)

  :bind (:map python-mode-map
         ("\C-c C-p" . flycheck-previous-error)
         ("\C-c C-n" . flycheck-next-error)
         ("C-c C-r" . projectile-ripgrep)))


(use-package jedi
  :straight t
  :bind (:map python-mode-map
              ("M-." . jedi:goto-definition)
              ("M-," . jedi:goto-definition-pop-marker)))


(use-package company-jedi
  :straight t
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package pungi
  :straight t
  :hook (python-mode . pungi:setup-jedi))

(use-package pyvenv
  :straight t)


;; XXX
;; (use-package flycheck-virtualenv
;;   :straight t
;;   :hook (python-mode . flycheck-virtualenv-setup))

(use-package blacken
  :straight t
  :config

  (defun py-enable-blacken ()
    "Run the blacken autoformatter on the buffer and enable blacken-mode."
    (interactive)
    (blacken-buffer t)
    (blacken-mode)
    (message "Black formatting enabled for buffer"))

  :bind (:map python-mode-map
         ("<f6>" . py-enable-blacken)))

(use-package cython-mode
  :straight t)

(provide 'python-config)
