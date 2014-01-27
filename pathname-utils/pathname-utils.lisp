(in-package :pathname-utils)

(defun pathname-absolute-p (pathname-or-string)
  (let* ((pathname (pathname pathname-or-string))
         (directory (pathname-directory pathname)))
    (when directory
      (equal (first directory)
             :absolute))))

(defun pathname-relative-p (pathname-or-string)
  (not (pathname-absolute-p pathname-or-string)))

(defun ->absolute-pathname (pathname-or-string)
  (let ((pathname (pathname pathname-or-string)))
    (if (pathname-relative-p pathname)
        ;; handle relative
        (merge-pathnames pathname)
        pathname)))
