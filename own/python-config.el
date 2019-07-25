;; python-config.el --- Configuration for editing of Python files

;;; Commentary:

;;; Code:

;; allow access to dependent Python libraries
(setenv "PYTHONPATH" (format "%s:%s"
                             (expand-file-name "~/.emacs.d/pylib")
                             (getenv "PYTHONPATH")))

(require 'python)   ; built-in Emacs version (good as of Emacs 24.3)
(require 'pyvenv)
(require 'flycheck-virtualenv)
(require 'blacken)
(require 'cython-mode)
(require 'text-misc)

;; Include underscores when matching words (not sure why this isn't the default)
(modify-syntax-entry ?_ "w" python-mode-syntax-table)

(defun py-which-thing (thing-type)
  "Display name of the current Python THING-TYPE thing.

THING-TYPE might be 'class', 'def' etc."
  (save-excursion
    (re-search-backward (format "^ *%s " thing-type))
    (forward-word)
    (forward-word)
    (message (plain-thing-at-point 'symbol))))

(defun py-which-class ()
  "Display name of the current Python class."
  (interactive)
  (py-which-thing "class"))

(defun py-which-function ()
  "Display name of the current Python function/method."
  (interactive)
  (py-which-thing "def"))

(defun py-pdbrc-breakpoint ()
  "Set breakpoint for current line for pdb to see."
  (interactive)
  (save-selected-window
    (let ((current-file buffer-file-name)
          (current-line (number-to-string (line-number-at-pos))))
      (find-file-other-window "~/.pdbrc")
      (end-of-buffer)
      (insert "break " current-file ":" current-line "\n")
      (save-buffer))))

(defun py-enable-blacken ()
  "Run the blacken autoformatter on the buffer and enable blacken-mode."
  (interactive)
  (blacken-buffer t)
  (blacken-mode)
  (message "Black formatting enabled for buffer"))

(defun python-customizations ()
  "Additional customizations for python mode."
  (add-to-list 'company-backends 'company-jedi)
  (flycheck-virtualenv-setup)
  (pungi:setup-jedi))

(add-hook 'python-mode-hook 'python-customizations)


(define-key python-mode-map "\C-cb"   'py-pdbrc-breakpoint)
(define-key python-mode-map "\C-cwc"  'py-which-class)
(define-key python-mode-map "\C-cwf"  'py-which-function)

(define-key python-mode-map (kbd "\C-c C-p") 'flycheck-previous-error)
(define-key python-mode-map (kbd "\C-c C-n") 'flycheck-next-error)

(define-key python-mode-map (kbd "C-c C-r") 'projectile-ripgrep)

(define-key python-mode-map (kbd "\C-c C-b") 'py-enable-blacken)

; Jump to definition and back again
(evil-define-key 'normal python-mode-map (kbd "M-.") 'jedi:goto-definition)
(evil-define-key 'normal python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)

(provide 'python-config)
;;; python-config.el ends here
