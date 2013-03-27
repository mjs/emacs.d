;; Customisations related to editing Python files

;; allow access to dependent Python libraries (Pymacs etc)
(setenv "PYTHONPATH" (format "%s:%s"
                             (expand-file-name "~/.emacs.d/pylib")
                             (getenv "PYTHONPATH")))

(require 'python)   ; built-in Emacs version (good as of Emacs 24.3)
(require 'text-misc)
(require 'flymake-pyflakes)
(require 'python-pylint)

;; Pymacs and ropemacs
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

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

(defun py-pdbrc-breakpoint ()
  "Set breakpoint for current line for pdb to see"
  (interactive)
  (save-selected-window
    (let ((current-file buffer-file-name)
          (current-line (number-to-string (line-number-at-pos))))
      (find-file-other-window "~/.pdbrc")
      (end-of-buffer)
      (insert "break " current-file ":" current-line "\n")
      (save-buffer))))

(defun python-customizations ()
  "Additional customizations for python mode"
  (define-key python-mode-map "\C-c\C-c" 'python-pylint)
  (define-key python-mode-map "\C-cb"   'py-pdbrc-breakpoint)
  (define-key python-mode-map "\C-cwc"  'py-which-class)
  (define-key python-mode-map "\C-cwf"  'py-which-function))

(add-hook 'python-mode-hook 'python-customizations)

(provide 'python-config)
