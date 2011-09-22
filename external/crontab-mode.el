;;; crontab-mode.el --- Mode for editing crontab files
;;
;; Original Author:    Harley Gorrell <harley@mahalito.net>
;; Original URL:       http://www.mahalito.net/~harley/elisp/crontab-mode.el
;; License:            GPL v2

(defvar crontab-suffix ".crontab"
  "*Suffix for crontab buffers.")


;; Would be better to have "\\([0-9]\\([-,][0-9]+\\)+\\|...
(defvar crontab-unit-regexp "\\(\\(?:[-,0-9]+\\|\\*\\)\\(?:/[0-9]+\\)?\\)"
  "A regexp which matches a cron time unit.")

(defvar crontab-sep-regexp "[ \t]+"
  "A regexp to match whitespace seperating cron time units.")

(defvar crontab-mode-hook nil
  "*Hook for customising `crontab-mode'.")

(defvar crontab-load-hook nil
  "*Hook run when the `crontab-mode' is loaded.")

;;
(defvar crontab-font-lock-keywords
  (list
   ;; Comments
   '("^#.*$" . font-lock-comment-face)
   ;; Blank lines are bad!
   '("^[ \t]+$" . highlight)
   ;; Variable defs
   '("^\\([A-Z_]+\\)=\\(.*\\)$" .
     ((1 font-lock-keyword-face)
      (2 font-lock-string-face)) )
   ;; Cron lines
   ;; 50 * * * * /usr/gnu/bin/bash
   (cons
    (concat "^"
	    crontab-unit-regexp crontab-sep-regexp
	    crontab-unit-regexp crontab-sep-regexp
	    crontab-unit-regexp crontab-sep-regexp
	    crontab-unit-regexp crontab-sep-regexp
	    crontab-unit-regexp crontab-sep-regexp
	    "\\(.*\\)$")
    '((1 font-lock-keyword-face)
      (2 font-lock-keyword-face)
      (3 font-lock-keyword-face)
      (4 font-lock-keyword-face)
      (5 font-lock-keyword-face)
      (6 font-lock-string-face))) )
  "Info for function `font-lock-mode'.")

(defvar crontab-mode-map nil
  "Keymap used in `crontab-mode'.")

(unless crontab-mode-map
  (setq crontab-mode-map (make-sparse-keymap)))

(defun crontab-mode ()
  "Major mode for editing crontabs.
Defines commands for getting and applying crontabs for hosts.
Sets up command `font-lock-mode'.

\\{crontab-mode-map}"
  (interactive)

  (message "loading crontab-mode")

  (kill-all-local-variables)
  (setq mode-name "crontab")
  (setq major-mode 'crontab-mode)
  (use-local-map crontab-mode-map)

  (setq comment-start "#")
  (setq comment-start-skip "#+ *")

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(crontab-font-lock-keywords))

  (run-hooks 'crontab-mode-hook))

(provide 'crontab-mode)
(run-hooks 'crontab-load-hook)
