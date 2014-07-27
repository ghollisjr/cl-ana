(in-package :logres)

;;;; logres is a result logging tool for automating the storage and
;;;; retrieval of computed results via makeres.  Typical use is in
;;;; data analysis where there is a mountain of results needing to be
;;;; organized.
;;;;
;;;; Example: Tables & reductions: makeres can be told how to optimize
;;;; looping over tables, generating result tables along with any
;;;; other reductions according to the dependency graph.  Via macros,
;;;; operators can be defined which define logres entries for
;;;; reductions with ids created automatically via whatever convention
;;;; you wish (I like to use lists which denote the chain of
;;;; tables/filters/etc along with a reduction id).

(defgeneric load-object (type path)
  (:documentation "Generic function which loads an object from a file
  located at path of type type")
  (:method (type path)
    (with-open-file (file path
                          :direction :input
                          :if-does-not-exist :error)
      (read file))))

(defgeneric save-object (object path)
  (:documentation "Generic function which saves an object to a file
  located at path")
  (:method (obj path)
    (with-open-file (file path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (format file "~s~%" obj))))

(defvar *project-paths*
  (make-hash-table :test 'equal)
  "Map from project name to output path")

(defvar *proj->res->lid*
  (make-hash-table :test 'equal)
  "Map from project id to map to result id to log id (used as
pathname)")

(defun set-project-path (pathname-or-string)
  "Sets the output directory path for current project and ensures that
the necessary subdirectories are present."
  (let ((pathname (mkdirpath pathname-or-string)))
    (setf (gethash (project) *project-paths*)
          (merge-pathnames pathname))
    (ensure-directories-exist
     (merge-pathnames "work/"
                      (gethash (project) *project-paths*)))
    (ensure-directories-exist
     (merge-pathnames "versions/"
                      (gethash (project) *project-paths*)))
    ;; initialize project vars:
    (setf (gethash (project) *proj->res->lid*)
          (make-hash-table :test 'equal))))

(defvar *last-id* -1
  "id used for automatically naming stored results")

(defun next-log-id ()
  (incf *last-id*))

(defun reset-log-id ()
  (setf *last-id* -1))

(defun logres-work-path (res-id)
  "Gets the path where result res-id should be stored in working
  directory."
  (let* ((res->lid (gethash (project) *proj->res->lid*))
         (lid (gethash res-id res->lid)))
    (when (not lid)
      (setf lid (next-log-id))
      (setf (gethash res-id res->lid) lid))
    (make-pathname :directory
                   (list :absolute
                         (namestring (gethash (project) *project-paths*))
                         "work")
                   :name (mkstr lid))))

(defun directory-pathname-p (pathname-or-string)
  "Returns t iff pathname-or-string refers to a directory"
  (string= (file-namestring (pathname pathname-or-string))
           ""))

(defun mkdirpath (pathname-or-string)
  "Returns a path which always refers to a directory (platform
independent)"
  (let ((pn (merge-pathnames pathname-or-string)))
    (if (directory-pathname-p pn)
        pn
        (let ((dirname (directory-namestring pn))
              (filename (file-namestring pn)))
          (make-pathname :directory
                         (list :absolute
                               dirname
                               filename))))))

(defun id-string-p (string)
  "t iff all characters are digit-char-p"
  (every #'digit-char-p string))

(defun load-last-id (path)
  "Reads the filenames directly inside path and returns the highest id
stored so that (next-log-id) returns an available id"
  (setf *last-id*
        (maximum
         (mapcar
          (lambda (x)
            (read-from-string x))
          (remove-if-not
           #'id-string-p
           (remove nil
                   (mapcar #'pathname-name
                           (directory (merge-pathnames path
                                                       "*")))))))))

;; only save results which have t-stat
(defun save-project (version-string
                     &key (if-exists :error))
  "Saves a project to a path project-path/version-string; if-exists
can be nil, :error or :supersede with behavior analogous to
open/with-open-file."
  (let* ((project-path (gethash (project) *project-paths*))
         (tartab (gethash (project) *target-tables*)))
    ;; are we in a project?
    (when (null project-path)
      (error "logres: No project path set"))
    ;; output directory handling
    (let ((save-path (make-pathname
                      :directory (list :absolute
                                       (namestring project-path)
                                       "versions"
                                       version-string)))
          (res->lid (gethash (project) *proj->res->lid*)))
      (when (probe-file save-path)
        (case if-exists
          (:error (error "save-project: Project save ~a exists"
                         save-path))
          (nil (return-from save-project nil))
          (:supersede
           ;; delete save-path recursively
           #+sbcl
           (sb-ext:delete-directory save-path
                                    :recursive t))))
      ;; need save-path to exist before writing:
      (ensure-directories-exist save-path)
      ;; allocate indices when necessary:
      (loop
         for res being the hash-keys in tartab
         do (when (null (gethash res res->lid))
              (setf (gethash res res->lid)
                    (next-log-id))))
      ;; save index file
      (with-open-file (index-file (merge-pathnames "index"
                                                   save-path)
                                  :direction :output
                                  :if-exists if-exists
                                  :if-does-not-exist :create)
        (loop
           for res being the hash-keys in res->lid
           for lid being the hash-values in res->lid
           do (let ((type (type-of (resfn res))))
                (format index-file
                        "~a ~a ~a~%"
                        res lid type))))
      ;; save all results:
      (loop
         for id being the hash-keys in tartab
         do (let* ((lid (gethash id res->lid))
                   (path (merge-pathnames (mkstr lid)
                                          save-path)))
              (format t "Saving ~a~%" id)
              (save-object (resfn id)
                           path)))
      ;; copy all file results stored in work/ to the version/work
      ;; directory
      (let ((work-to-path (merge-pathnames "work/"
                                           save-path))
            (work-from-path (merge-pathnames "work/"
                                             project-path)))
        (when (probe-file work-to-path)
          (sb-ext:delete-directory work-to-path :recursive t))
        (format t "Saving work/ files~%")
        (run "cp"
             (list "-r"
                   (namestring work-from-path)
                   (namestring work-to-path))))))
  nil)

;; Use sb-mop:compute-applicable-methods-using-classes (SBCL only),
;; cl:find-class and a text file storing the class names of each of
;; the objects saved to keep track of and load results.

;; make sure to call load-last-id in the version directory
(defun load-project (version)
  ;; reset res->lid
  (setf (gethash (project) *proj->res->lid*)
        (make-hash-table :test 'equal))
  (let* ((project-path (gethash (project) *project-paths*))
         (tartab (gethash (project) *target-tables*))
         (res->lid (gethash (project) *proj->res->lid*))
         (load-path (make-pathname
                     :directory (list :absolute
                                      (namestring project-path)
                                      "versions"
                                      (namestring version)))))
    ;; load *last-id*:
    (load-last-id load-path)
    ;; load working files:
    (format t "Loading work/ files~%")
    (let ((work-from-path (merge-pathnames "work/"
                                           load-path))
          (work-to-path (merge-pathnames "work/"
                                         project-path)))
      (when (probe-file work-to-path)
        (sb-ext:delete-directory work-to-path :recursive t))
      (run "cp"
           (list "-r"
                 (namestring work-from-path)
                 (namestring work-to-path))))
    ;; load results:
    (let ((index-lines
           (read-lines-from-pathname
            (merge-pathnames "index"
                             load-path))))
      (loop
         for line in index-lines
         do (with-input-from-string (s line)
              (let ((id (read s))
                    (lid (read s))
                    (type (read s)))
                (format t "Loading ~a~%" id)
                (setf (gethash id res->lid) lid)
                (when (not (gethash id tartab))
                  (format t "Warning: result ~a not present in target table~%"
                          id))
                (setresfn id
                          (load-object type
                                       (merge-pathnames (mkstr lid)
                                                        load-path))))))))
  nil)
