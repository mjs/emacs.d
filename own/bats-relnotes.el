; TODO
; - highlights helpers (extract text and allow editing, gets shoved up the top)
; - crontab helpers
; - SQL delta helpers
; - basic syntax highlighting

(defconst bats-relnotes-title-regex "^  Rev  ")
(defconst bats-relnotes-header-line-regex "^---------------------")

(defun bats-relnotes-add-titles ()
  (unless (bats-relnotes-has-titles)
    (goto-char 1)
    (insert "\
============
 Highlights
============



============
 SQL Deltas
============
None


=================
 Crontab Updates
=================
None


")
    (bats-relnotes-create-next-point-release)))


(defun bats-relnotes-has-titles ()
  (save-excursion
    (goto-line 2)
    (string= (current-word) "Highlights")))

(defun bats-relnotes-next-rev ()
  (interactive)
  (re-search-forward bats-relnotes-title-regex)
  (bats-relnotes-rev-sync))

(defun bats-relnotes-prev-rev ()
  (interactive)
  (re-search-backward "^$")
  (re-search-backward bats-relnotes-title-regex)
  (bats-relnotes-rev-sync))

(defun bats-relnotes-rev-sync ()
  (beginning-of-line)
  (next-line)
  (next-line)
  (recenter))

(defun bats-relnotes-find-max-point-release ()
  (defun find-max-point-release (release)
    (save-excursion
      (beginning-of-buffer)
      (if (search-forward-regexp (format "^Point %d$" release) nil 't)
          (max release (find-max-point-release (+ release 1)))
        0)))
  (find-max-point-release 1))

(defun bats-relnotes-add-point-release (release)
  (save-excursion
    (beginning-of-buffer)
    (search-forward " SQL Deltas")
    (beginning-of-line)
    (forward-line -2)
    (insert (format "Point %d\n-------\n\n" release))
    (end-of-buffer)
    (insert (format "\nPoint %d\n-------\n\n" release)))  ; XXX ugly
  (end-of-buffer))

(defun bats-relnotes-create-next-point-release ()
  (interactive)
  (bats-relnotes-add-point-release (+ (bats-relnotes-find-max-point-release) 1)))


;; (defun bats-relnotes-prev-release ()
;;   (interactive)
;;   (let* ((parts (bats-relnotes-split-filename (buffer-file-name)))
;;          (new-filename (format "%s_%s_%d_%s.txt"
;;                            (car parts) (cadr parts)
;;                            (+ (string-to-number (caddr parts)) 1)
;;                            (cadddr parts))))
;;        (write-file new-filename t)))


;; (defun bats-relnotes-bump ()
;;   "Bump minor version in release notes file"
;;   (interactive)
;;   (let* ((parts (bats-relnotes-split-filename (buffer-file-name)))
;;          (new-filename (format "%s_%s_%d_%s.txt"
;;                            (car parts) (cadr parts)
;;                            (+ (string-to-number (caddr parts)) 1)
;;                            (cadddr parts))))
;;        (write-file new-filename t)))

(defvar bats-relnotes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'bats-relnotes-next-rev)
    (define-key map (kbd "M-p") 'bats-relnotes-prev-rev)
    (define-key map (kbd "C-c C-c") 'bats-relnotes-create-next-point-release)
    map)
  "Keymap for bats-relnotes-mode")

(defun bats-relnotes-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map bats-relnotes-mode-map)
  (setq major-mode 'bats-relnotes-mode)
  (setq mode-name "BATS-Release-Notes")
  (toggle-truncate-lines 1)
  (bats-relnotes-add-titles))

(add-to-list 'auto-mode-alist '("bld.+\\.txt$" . bats-relnotes-mode))

(defun bats-relnotes-new ()
  "Create a new release notes file with today's date"
  (interactive)
  (find-file (expand-file-name (format "~/Notes/releases/%s" (bats-relnotes-new-filename)))))

(defun bats-relnotes-new-filename ()
  "Generate the filename for a new release notes file"
  (multiple-value-bind
      (second minute hour day month year day-of-week dst-p tz) (decode-time)
    (bats-relnotes-make-filename "bldS" year month day)))

(defun bats-relnotes-make-filename (prefix year month day)
  (format "%s_%d%02d%02d.txt" prefix year month day))

(provide 'bats-relnotes)
