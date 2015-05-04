;; Filecache configuration

(require 'filecache)
(require 'text-misc)

(add-to-list 'file-cache-filter-regexps "/[.]git")  ;; git directories
(add-to-list 'file-cache-filter-regexps "/[.]svn")  ;; svn directories 
(add-to-list 'file-cache-filter-regexps "[.]pyc$")
(add-to-list 'file-cache-filter-regexps "[.]i$")    ;; hg files

; Directories we expect to exist and indexed on all hosts
(setq file-cache-common-directories '("~/Dropbox/Notes"
                                      "~/config"))

; site specific configuration can set file-cache-site-directories
(unless (boundp 'file-cache-site-directories)
  (setq file-cache-site-directories ()))

(defun refresh-file-cache ()
  (interactive)
  (message "Loading file cache...")
  (file-cache-clear-cache)
  (with-demoted-errors "Error: %S"
      (loop for dir in (append file-cache-site-directories file-cache-common-directories)
            do (file-cache-add-directory-using-find dir))))

(refresh-file-cache)

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
