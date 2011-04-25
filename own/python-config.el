;; Customisations related to editing Python files

(require 'text-misc)
(require 'flymake-pyflakes)

;; python mode settings
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
				   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

(defun py-which-thing (thing-type)
  (save-excursion
    (re-search-backward (format "^ *%s " thing-type))
    (forward-word)
    (forward-word)
    (message (plain-thing-at-point 'symbol))))

(defun py-which-class ()
  "Display name of the current Python class"
  (interactive)
  (py-which-thing "class"))

(defun py-which-function ()
  "Display name of the current Python function/method"
  (interactive)
  (py-which-thing "def"))

(defun python-customizations ()
  "Additional customizations for python mode"
  (define-key py-mode-map "\C-cwc"  'py-which-class)
  (define-key py-mode-map "\C-cwf"  'py-which-function))

(add-hook 'python-mode-hook 'python-customizations)

;; IPython support
(setq ipython-command "ipython")
(require 'ipython)

(provide 'python-config)
