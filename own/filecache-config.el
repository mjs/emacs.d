;; Filecache configuration

(require 'filecache)
(require 'text-misc)

(add-to-list 'file-cache-filter-regexps "/[.]git")
(add-to-list 'file-cache-filter-regexps "/[.]svn")
(add-to-list 'file-cache-filter-regexps "/[.]hg")
(add-to-list 'file-cache-filter-regexps "[.]i$")    ;; created by hg
(add-to-list 'file-cache-filter-regexps "/[.]bzr")
(add-to-list 'file-cache-filter-regexps "[.]pyc$")
(add-to-list 'file-cache-filter-regexps "/.emacs.d/external/")

; Directories we expect to exist and indexed on all hosts
; XXX why not just have one that gets appended to?
(setq file-cache-common-directories '("~/Dropbox/Notes"
                                      "~/config"
                                      "~/.emacs.d"
                                      ))

; site specific configuration can set file-cache-site-directories
(unless (boundp 'file-cache-site-directories)
  (setq file-cache-site-directories ()))

;; XXX extract async core and helpers to a separate (releasable) package
;; - have a single path which holds the interesting directories
;; - split config from functionality

(defun file-cache-refresh ()
  (interactive)
  (message "Loading file cache...")
  (file-cache-clear-cache)
  (let ((all-cache-directories (mapcar 'expand-file-name (append file-cache-site-directories file-cache-common-directories))))
    (file-cache-add-dirs-async all-cache-directories)))

; XXX doc
; TODO: apply the exclusion regexes here as part of the find command line
(defun file-cache-add-dirs-async (dirs)
  (make-process
   :name "file-cache-load-async"
   :filter (file-cache-make-async-filter)
   :command (append '("find") dirs '("-ignore_readdir_race"))
   :stderr "file-cache-add-dirs-error"))

(defun file-cache-make-async-filter ()
  "Return a new filter function for use with
  file-cache-add-dirs-async. The returned function expects to be given
  filenames - one per line - and handles the case of filenames being
  split between calls."
  (lexical-let ((remainder ""))
    (lambda (proc output)
      (let ((lines (split-string output)))
        (file-cache-add-file-filtered (concat remainder (pop lines)))
        (unless (string-match-p "\n$" output)
          (setq remainder (car (last lines)))
          (nbutlast lines))
        (dolist (line lines)
          (file-cache-add-file-filtered line))))))

(cl-defun file-cache-add-file-filtered (filename)
  (dolist (regexp file-cache-filter-regexps)
    (when (string-match regexp filename)
      (return-from file-cache-add-file-filtered)))
  ; Prevent file-cache-add-file from blowing up on dangling symlinks
  ; and deleted files.
  (ignore-errors
    (file-cache-add-file filename)))

(file-cache-refresh)

(defun file-cache-ido-find-file (file)
  "Using ido, interactively open file from file cache'.
First select a file, matched using ido-switch-buffer against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
  (interactive (list (file-cache-ido-read "File: "
                                          (mapcar
                                           (lambda (x)
                                             (car x))
                                           file-cache-alist))))
  (let* ((record (assoc file file-cache-alist)))
    (find-file
     (expand-file-name
      file
      (if (= (length record) 2)
          (car (cdr record))
        (file-cache-ido-read
         (format "Find %s in dir: " file) (cdr record)))))))

(defun file-cache-ido-read (prompt choices)
  (let ((ido-make-buffer-list-hook
         (lambda ()
           (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))

(defun file-cache-find-file-at-point ()
  "Using the filename at the point, open it using the file cache"
  (interactive)
  (let ((filename (filename-near-point)))
    (if (string= filename "")
        (message "No filename at point")
      (file-cache-ido-find-file filename))))


(global-set-key (kbd "C-x f") 'file-cache-ido-find-file)
(global-set-key (kbd "C-x F") 'file-cache-find-file-at-point)

(provide 'filecache-config)
