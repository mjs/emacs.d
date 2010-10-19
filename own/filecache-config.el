;; Filecache configuration

(require 'filecache)

;; ignore git directories in my file cache
(add-to-list 'file-cache-filter-regexps "/[.]git")

;; ignore svn directories in my file cache
(add-to-list 'file-cache-filter-regexps "/[.]svn")

;; ignore pyc directories in my file cache
(add-to-list 'file-cache-filter-regexps "[.]pyc$")

(defun refresh-file-cache ()
  (message "Loading file cache...")
  (file-cache-clear-cache)
  (file-cache-add-directory-using-find "~/ecn/0/source")
  (file-cache-add-directory-using-find "~/sql-mtf"))

(eval-after-load "filecache" '(refresh-file-cache))


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

(defun plain-thing-at-point (thing-type)
  (let ((thing (thing-at-point thing-type)))
      (set-text-properties 0 (length thing) nil thing)
      thing))

(defun filename-at-point ()
  (save-excursion
    (let ((orig-col (current-column)))
      (beginning-of-line)
      (if (looking-at "#include") 
          (re-search-forward "[<\"]")
        (move-to-column orig-col))
      (plain-thing-at-point 'filename))))
  
(defun file-cache-find-file-at-point ()
  "Using the filename at the point, open it using the file cache"
  (interactive)
  (let ((filename (filename-at-point)))
    (if (string= filename "") 
        (message "No filename at point")
      (file-cache-ido-find-file filename))))


(global-set-key (kbd "C-x f") 'file-cache-ido-find-file)
(global-set-key (kbd "C-x F") 'file-cache-find-file-at-point)

(provide 'filecache-config)
