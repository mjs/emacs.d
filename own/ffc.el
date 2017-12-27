(require 'text-misc)
(require 'subr-x)

(defvar ffc-directories '()
  "Directories to be included in the fast file cache")

(defvar ffc-filter-regexps
  '("/\\.#"
    "\\.class$"
    "#$"
    "\\.$"
    "\\.output$"
    ",v$"
    "\\.elc$"
    "\\.a$"
    "\\.exe$"
    "\\.o$"
    "~$"
    "/[.]git"
    "/[.]svn"
    "/[.]hg"
    "[.]i$"
    "/[.]bzr"
    "[.]pyc$")
  "Regexes of file names which shouldn't be cached")

; XXX doc
(defvar ffc-cache (make-hash-table :test 'equal :size 2048))

; XXX doc
(defun ffc-clear-cache ()
  (interactive)
  (clrhash ffc-cache))

(defun ffc-refresh ()
  (interactive)
  (message "Loading file cache...")
  (ffc-clear-cache)
  (ffc-add-dirs (mapcar 'expand-file-name ffc-directories)))

; XXX doc
(defun ffc-add-dirs (dirs)
  (make-process
   :name "ffc-load"
   :filter (ffc-make-filter)
   :command (append '("find") dirs '("-ignore_readdir_race"))
   :stderr "ffc-add-dirs-error"))

(defun ffc-make-filter ()
  "Return a new filter function for use with ffc-add-dirs. The
  returned function expects to be given filenames - one per line
  - and handles the case of filenames being split between calls."
  (lexical-let ((remainder ""))
    (lambda (proc output)
      (let ((lines (split-string output)))
        (ffc-add-file (concat remainder (pop lines)))
        (if (string-suffix-p "\n" output)
            (setq remainder "")
          (progn
            (setq remainder (car (last lines)))  ; XXX slow?
            (nbutlast lines)))
        (mapc 'ffc-add-file lines)))))

; XXX
(cl-defun ffc-add-file (file)
  (dolist (regexp ffc-filter-regexps)
    (when (string-match regexp file)
      (return-from ffc-add-file)))

  (let* ((file-name (file-name-nondirectory file))
         (dir-name  (file-name-directory file))
         (entry (gethash file-name ffc-cache)))
    (cond ((null entry)
           ;; Entry wasn't in the cache, add it.
           (puthash file-name (list dir-name) ffc-cache))
          ((not (member dir-name entry))
           ;; Directory wasn't in entry's directory list
           (setcdr entry (cons dir-name (cdr entry)))))))

(defun ffc-ido-find-file (file)
  "Using ido, interactively open file from the ffc cache'.
First select a file, matched using ido-switch-buffer against the
contents in `ffc-cache'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
  (interactive (list (ffc-ido-read "File: " (hash-table-keys ffc-cache))))
  (let* ((dirs (gethash file ffc-cache)))
    (find-file
     (expand-file-name
      file
      (if (= (length dirs) 1)
          (car dirs)
        (ffc-ido-read
         (format "Find %s in dir: " file) dirs))))))

(defun ffc-find-file-at-point ()
  "Using the filename at the point, open it using the file cache"
  (interactive)
  (let ((filename (filename-near-point)))
    (if (string= filename "")
        (message "No filename at point")
      (ffc-ido-find-file filename))))

(defun ffc-ido-read (prompt choices)
  (let ((ido-make-buffer-list-hook
         (lambda ()
           (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))


(provide 'ffc)
