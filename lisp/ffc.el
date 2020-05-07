;;; ffc.el -- (fast file cache) fast, asynchronous file name caching

;;; Commentary:

;; This package implements similar functionality to the standard Emacs
;; file-cache package but with two important differences:
;;
;; 1. It is *much* more efficent for large caches because it uses hash
;;    maps instead of association lists.
;; 2. The cache is rebuilt asynchronously so the user does not have to
;;    wait for it to be built when as Emacs starts or when it is
;;    refreshed later.
;;
;; Installation:
;; - Emacs 25 or later is required.
;; - Drop ffc.el somewhere in your load path.
;;
;; Example configuration:
;;
;; (require 'ffc)
;;
;; (global-set-key (kbd "C-x f") 'ffc-ido-find-file)
;;
;; (add-to-list 'ffc-directories "~/some/path")
;; (add-to-list 'ffc-directories "~/another/path")
;; (ffc-refresh)

;;; Code:

(require 'subr-x)
(require 'ido)
(eval-when-compile (require 'cl))

;; XXX autoloads

;; XXX use defcustom
(defvar ffc-directories '()
  "Directories to be included in the fast file cache.")

;; XXX use defcustom
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
  "Regexes of file names which shouldn't be cached.")

(defvar ffc-cache (make-hash-table :test 'equal :size 8192)
  "This hash table holds the ffc file name cache.")

(defun ffc-clear-cache ()
  "Empty the file name cache."
  (interactive)
  (clrhash ffc-cache))

(defun ffc-refresh ()
  "Recreate the file name cache using the directories in ffc-directories."
  (interactive)
  (message "Loading file cache...")
  (ffc-clear-cache)
  (ffc-add-dirs (mapcar 'expand-file-name ffc-directories)))

(defun ffc-add-dirs (dirs)
  "Asynchronously add the filenames in DIRS to the ffc cache."
  (make-process
   :name "ffc-load"
   :filter (ffc-make-filter)
   :command (append '("find") dirs '("-ignore_readdir_race")) ; XXX will break on Windows
   :stderr "ffc-add-dirs-error"))

(defun ffc-make-filter ()
  "Return a new filter function for use with ffc-add-dirs.
The returned function expects to be given filenames - one per line -
and handles the case of filenames being split between calls."
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

(cl-defun ffc-add-file (file)
  "Add a single FILE to the ffc cache.
The file won't be added if it is matched by an entry in
'ffc-filter-regexps'."
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
  "Using ido, interactively open FILE using the ffc cache'.
First select a file, matched using 'ido-switch-buffer' against the
contents in `ffc-cache'.  If the file exist in more than one
directory, select directory.  Finally, the file is opened."
  (interactive (list (ffc-ido-read "File: " (hash-table-keys ffc-cache))))
  (let* ((dirs (gethash file ffc-cache)))
    (find-file
     (expand-file-name
      file
      (if (= (length dirs) 1)
          (car dirs)
        (ffc-ido-read
         (format "Find %s in dir: " file) dirs))))))

(defun ffc-ido-read (prompt choices)
  "Call 'ido-read-buffer' with PROMPT and possible CHOICES specified."
  (let ((ido-make-buffer-list-hook
         (lambda ()
           (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))


(provide 'ffc)
;;; ffc.el ends here
