;; allow access to dependent Python libraries
(setenv "PYTHONPATH" (format "%s:%s"
                             (expand-file-name "~/.emacs.d/pylib")
                             (getenv "PYTHONPATH")))

(use-package python
  :straight (:type built-in)  ; built-in Emacs version (good as of Emacs 24.3)
  :hook (python-mode . init-lsp-for-buffer)
  :config
  ;; Include underscores when matching words (not sure why this isn't the default)
  (modify-syntax-entry ?_ "w" python-mode-syntax-table))


;; (use-package pungi
;;   :straight t
;;   :hook (python-mode . pungi:setup-jedi))

(use-package pyvenv
  :straight t)

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
