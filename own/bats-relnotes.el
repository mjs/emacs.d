
(defun bats-relnotes-narrow-to-revs ()
  (widen)
  (beginning-of-buffer)
  (re-search-forward "^---------------------")
  (beginning-of-line)
  (let ((start-point (point)))
    (re-search-forward "^Checking for existence of trunk")
    (narrow-to-region start-point (match-beginning 0)))
  (beginning-of-buffer))

(defun bats-relnotes-next-rev ()
  (interactive)
  (re-search-forward "^  Rev  ")
  (beginning-of-line)
  (next-line)
  (next-line))

(defun bats-relnotes-prev-rev ()
  (interactive)
  (re-search-backward "^$")
  (re-search-backward "^  Rev  ")
  (beginning-of-line)
  (next-line)
  (next-line))

(defvar bats-relnotes-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cn" 'bats-relnotes-next-rev)
    (define-key map "\C-cp" 'bats-relnotes-prev-rev)
    map)
  "Keymap for bats-relnotes-mode")

(defun bats-relnotes-mode ()
  "Major mode for processing BATS software release notes"
  (interactive)
  (kill-all-local-variables)
  (use-local-map bats-relnotes-mode-map)
  (bats-relnotes-narrow-to-revs)
  (setq major-mode 'bats-relnotes-mode)
  (setq mode-name "BATS-Release-Notes"))

(provide 'bats-relnotes)
