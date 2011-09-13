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
Highlights
----------


SQL Deltas
----------
None.


Crontab Updates
---------------
None.


")))


(defun bats-relnotes-has-titles ()
  (save-excursion
    (goto-char 1)
    (string= (current-word) "Highlights")))

(defun bats-relnotes-next-rev ()
  (interactive)
  (re-search-forward bats-relnotes-title-regex)
  (beginning-of-line)
  (next-line)
  (next-line))

(defun bats-relnotes-prev-rev ()
  (interactive)
  (re-search-backward "^$")
  (re-search-backward bats-relnotes-title-regex)
  (beginning-of-line)
  (next-line)
  (next-line))

(defvar bats-relnotes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'bats-relnotes-next-rev)
    (define-key map (kbd "M-p") 'bats-relnotes-prev-rev)
    (define-key map (kbd "C-c b") 'bats-relnotes-bump)
    map)
  "Keymap for bats-relnotes-mode")

(defun bats-relnotes-bump ()
  "Bump minor version in release notes file"
  (interactive)
  (let* ((parts (bats-relnotes-parts-from-filename (buffer-file-name)))
         (new-filename (format "%s_%s_%d_%s.txt"
                           (car parts) (cadr parts)
                           (+ (string-to-number (caddr parts)) 1)
                           (cadddr parts))))
       (write-file new-filename t)))

(defun bats-relnotes-parts-from-filename (filename)
  "Extract parts of the release notes filename"
  (unless (string-match "bld.+\\.txt$" filename) (throw 'invalid-filename t))
  (let ((basename (file-name-nondirectory filename)))
    (split-string (car (split-string basename "\\.")) "_")))

(defun bats-relnotes-mode ()
  "Major mode for processing BATS software release notes"
  (interactive)
  (kill-all-local-variables)
  (use-local-map bats-relnotes-mode-map)
  (setq major-mode 'bats-relnotes-mode)
  (setq mode-name "BATS-Release-Notes")
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
    (format "bldS_%d%02d%02d_1_mtf.txt" year month day)))

(provide 'bats-relnotes)
