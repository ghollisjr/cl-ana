(declaim (optimize (debug 3)))

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

(defgeneric load-target (type path)
  (:documentation "Generic function which loads an object from a file
  located at path of type type")
  (:method (type path)
    (with-open-file (file path
                          :direction :input
                          :if-does-not-exist :error)
      (read file))))

(defgeneric save-target (lid object path)
  (:documentation "Generic function which saves an object to a file
  located at path")
  (:method (lid obj path)
    (with-open-file (file path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (format file "~s~%" obj))))

(defun target-type (object)
  "Returns type of object with exception for vectors/arrays"
  (cond
    ((stringp object)
     'string)
    ((arrayp object)
     'array)
    (t (type-of object))))

(defvar *project-paths*
  (make-hash-table :test 'equal)
  "Map from project name to output path")

(defvar *proj->res->lid*
  (make-hash-table :test 'equal)
  "Map from project id to map to result id to log id (used as
pathname)")

(defvar *proj->ignore*
  (make-hash-table :test 'equal)
  "Map from project id to list of result ids to ignore (never log)")

(defvar *proj->ignore-filters*
  (make-hash-table :test 'equal)
  "Map from project id to list of filter functions which should return
  t for ignored targets")

(defvar *proj->par->lid*
  (make-hash-table :test 'equal))

(defvar *proj->lid->sublids*
  (make-hash-table :test 'equal))

(defun load-sublid-map (version)
  "Loads the sublid map from logged version"
  (symbol-macrolet ((sublid-map
                     (gethash *project-id* *proj->lid->sublids*)))
    (clrhash sublid-map)
    (with-open-file (sublid-file (merge-pathnames "sublid-map"
                                                  (ensure-absolute-pathname version))
                                 :direction :input)
      (do ((line (read-line sublid-file nil nil)
                 (read-line sublid-file nil nil)))
          ((null line))
        (with-input-from-string (s line)
          (let ((lid (read s)))
            (do ((sublid (read s nil nil)
                         (read s nil nil)))
                ((null sublid))
              (push sublid
                    (gethash lid sublid-map)))))))))

(defun save-sublid-map (version)
  "Saves the sublid map for logged version"
  (symbol-macrolet ((sublid-map
                     (gethash *project-id* *proj->lid->sublids*)))
    (with-open-file (sublid-file (merge-pathnames "sublid-map"
                                                  (ensure-absolute-pathname version))
                                 :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :supersede)
      (loop
         for k being the hash-keys in sublid-map
         for v being the hash-values in sublid-map
         do (progn
              (format sublid-file "~{~a~^ ~}~%" (cons k v)))))))

(defun logres-ignorefn (res)
  "function version of logres-ignore"
  (symbol-macrolet ((ignore
                     (gethash (project) *proj->ignore*)))
    (setf ignore
          (adjoin res ignore
                  :test #'equal))))

(defmacro logres-ignore (res)
  "Ignores result with id res when logging (loading or saving)"
  `(logres-ignorefn ',res))

(defun logres-ignore-by (filter)
  "Ignores any result with id for which (funcall filter id) returns
t (filter can use target table to get information from id)"
  (symbol-macrolet ((filters
                     (gethash (project) *proj->ignore-filters*)))
    (setf filters
          (adjoin filter filters))))

(defun logres-trackfn (res)
  "function version of logres-track"
  (symbol-macrolet ((ignore
                     (gethash (project) *proj->ignore*)))
    (setf ignore
          (remove res ignore
                  :test #'equal))))

(defmacro logres-track (res)
  "Ensures that a result is not ignored"
  `(logres-trackfn ',res))

(defmacro logres-track-by (filter)
  "Tracks any result with id for which (funcall filter id) returns
t (filter can use target table to get information from id)"
  (symbol-macrolet ((filters
                     (gethash (project) *proj->ignore-filters*)))
    (setf filters
          (remove filter filters))))

(defun function-target? (id)
  (let ((tar (gethash id (target-table))))
    (and (target-stat tar)
         (functionp (target-val tar)))))

(defun ignored? (id)
  "Returns true if a result target is ignored by logres"
  (symbol-macrolet ((ignores
                     (gethash (project) *proj->ignore*))
                    (ignore-filters
                     (gethash (project) *proj->ignore-filters*)))
    (labels ((rec (filters)
               (if filters
                   (let ((f (first filters)))
                     (if (funcall f id)
                         t
                         (rec (rest filters))))
                   nil)))
      (or (member id ignores
                  :test #'equal)
          (rec ignore-filters)))))

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
          (make-hash-table :test 'equal))
    ;; and sublid map:
    (setf (gethash (project) *proj->lid->sublids*)
          (make-hash-table :test 'equal))))

(defun project-path ()
  "Returns path for current project, nil when not set or in nil
project"
  (values (gethash (project) *project-paths*)))

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

(defun ensure-absolute-pathname (pathname-or-string)
  (if (pathname-absolute-p pathname-or-string)
      (pathname pathname-or-string)
      (make-pathname
       :directory (list :absolute
                        (namestring (project-path))
                        "versions"
                        pathname-or-string))))

(defun checkout-version (version-string)
  "Sets symlink in project-path/versions/current to point to
version-string as absolute path or relative path from project
directory."
  (let ((version-path (ensure-absolute-pathname version-string))
        (current-path
         (make-pathname :name "current"
                        :directory (list :absolute
                                         (namestring (project-path))
                                         "versions"))))
    (when (probe-file current-path)
      (delete-file current-path))
    (run "ln"
         (list "-s"
               "-T"
               version-path
               current-path)))
  nil)

;; only save results which have t-stat and non-default parameters
(defun save-project (version-string
                     &key
                       (if-exists :error)
                       (current-p t))
  "Saves a project to a path project-path/version-string; if-exists
can be nil, :error or :supersede with behavior analogous to
open/with-open-file.  If current-p is t, sets symlink current to point
to this project."
  (let* ((project-path (gethash (project) *project-paths*))
         (tartab (gethash (project) *target-tables*)))
    ;; are we in a project?
    (when (null project-path)
      (error "logres: No project path set"))
    ;; set current:
    (when current-p
      (checkout-version version-string))
    ;; output directory handling
    (let ((save-path (ensure-absolute-pathname version-string))
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
         do (when (and (null (gethash res res->lid))
                       (not (ignored? res)))
              (setf (gethash res res->lid)
                    (next-log-id))))
      ;; and for parameters:
      (setf (gethash *project-id* *proj->par->lid*)
            (make-hash-table :test 'eq))
      (loop
         for pid being the hash-keys in (gethash *project-id* *makeres-args*)
         for pval being the hash-values in (gethash *project-id* *makeres-args*)
         do (setf (gethash pid
                           (gethash *project-id* *proj->par->lid*))
                  (next-log-id)))
      ;; save index file
      (with-open-file (index-file (merge-pathnames "index"
                                                   save-path)
                                  :direction :output
                                  :if-exists if-exists
                                  :if-does-not-exist :create)
        (loop
           for res being the hash-keys in res->lid
           for lid being the hash-values in res->lid
           when (and (not (ignored? res))
                     (target-stat (gethash res tartab)))
           do (let ((type (target-type (resfn res))))
                (format index-file
                        "~a ~a ~a~%"
                        res lid type))
           when (not (target-stat (gethash res tartab)))
           do (format t "WARNING: ~a stat is null, not saving~%"
                      res))
        ;; parameters start after empty line:
        (format index-file "~%")
        (loop
           for pid being the hash-keys in (gethash *project-id* *proj->par->lid*)
           for plid being the hash-values in (gethash *project-id* *proj->par->lid*)
           do (let* ((pval (parfn pid))
                     (type (target-type pval)))
                (format index-file "~a ~a ~a~%"
                        pid plid type))))
      ;; save all results:
      (loop
         for id being the hash-keys in tartab
         when (and (not (ignored? id))
                   (target-stat (gethash id tartab)))
         do (let* ((lid (gethash id res->lid))
                   (path (merge-pathnames (mkstr lid)
                                          save-path)))
              (format t "Saving ~a~%" id)
              (save-target lid
                           (resfn id)
                           path)))
      ;; and for parameters
      ;;
      ;; Currently parameters cannot be ignored for saving/loading,
      ;; this limitation means that at least functions are not
      ;; suitable parameters for use with logres.  This may be
      ;; resolved at some future point.
      (loop
         for pid being the hash-keys in (gethash *project-id*
                                                 *proj->par->lid*)
         for plid being the hash-values in (gethash *project-id*
                                                    *proj->par->lid*)
         do (let ((pval (parfn pid)))
              (format t "Saving parameter ~a~%" pid)
              (save-target plid
                           pval
                           (merge-pathnames (mkstr plid)
                                            save-path))))

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
                   (namestring work-to-path)))))
    ;; Save sublid map:
    (save-sublid-map version-string))
  nil)

;; Use sb-mop:compute-applicable-methods-using-classes (SBCL only),
;; cl:find-class and a text file storing the class names of each of
;; the objects saved to keep track of and load results.

;; make sure to call load-last-id in the version directory
(defun load-project (version &key (work-p t))
  "work-p nil = assume work files already present"
  ;; reset res->lid
  (setf (gethash (project) *proj->res->lid*)
        (make-hash-table :test 'equal))
  (let* ((project-path (gethash (project) *project-paths*))
         (tartab (gethash (project) *target-tables*))
         (res->lid (gethash (project) *proj->res->lid*))
         (load-path (ensure-absolute-pathname version)))
    ;; load *last-id*:
    (load-last-id load-path)
    ;; load working files:
    (when work-p
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
                   (namestring work-to-path)))))
    ;; load results:
    (let ((index-lines
           (read-lines-from-pathname
            (merge-pathnames "index"
                             load-path))))
      (destructuring-bind (res-lines par-lines)
          (split-sequence:split-sequence "" index-lines
                                         :test #'equal)
        (loop
           for line in res-lines
           do
             (with-input-from-string (s line)
               (let ((id (read s))
                     (lid (read s))
                     (type (read s)))
                 (cond
                   ((not (gethash id tartab))
                    (format
                     t
                     "Warning: result ~a not present in target table, skipping~%"
                     id))
                   ((ignored? id)
                    (format t "Warning: result ~a is ignored, skipping~%" id))
                   (t
                    (format t "Loading ~a~%" id)
                    (setf (gethash id res->lid) lid)
                    (when (not (ignored? id))
                      (setresfn id
                                (load-target type
                                             (merge-pathnames (mkstr lid)
                                                              load-path)))))))))
        ;; parameters:
        (setf (gethash *project-id* *makeres-args*)
              (make-hash-table :test 'eq))
        (loop
           for line in par-lines
           do
             (with-input-from-string (s line)
               (let ((id (read s))
                     (lid (read s))
                     (type (read s)))
                 (if (member id (gethash *project-id* *params-table*)
                             :key #'car)
                     (progn
                       (format t "Loading parameter ~a~%" id)
                       (setf (gethash id
                                      (gethash *project-id*
                                               *makeres-args*))
                             (load-target type
                                          (merge-pathnames (mkstr lid)
                                                           load-path))))
                     (format
                      t
                      "WARNING: Parameter ~a not in project, skipping~%"
                      id))))))))
  ;; load sublid-map:
  (load-sublid-map version)
  nil)

;;;; At the moment, this function appears unnecessary due to the
;;;; ability to restart a session, load & save a project to
;;;; automatically prune the log.
;;;;
;;;; However: I'm leaving the code commented as a reminder of the
;;;; issue for future readers.

;; (defun prune-log (version &key (remove-ignored t))
;;   "Removes any parameters and results from log version which are not
;; present in the current project specification.  If remove-ignored is
;; non-nil, then ignored results are removed from the log as well.

;; Note that any files in the work/ directory must be manually deleted
;; until files in work/ are managed by logres (pending)."
;;   (let* ((project-path (gethash (project) *project-paths*))
;;          (tartab (gethash (project) *target-tables*))
;;          (version-path (make-pathname
;;                         :directory (list :absolute
;;                                          (namestring project-path)
;;                                          "versions"
;;                                          (namestring version))))
;;          (index-lines
;;           (read-lines-from-pathname
;;            (merge-pathnames "index"
;;                             version-path)))
;;          (split-lines (split-sequence:split-sequence "" index-lines
;;                                                      :test #'equal))
;;          (res-lines (first split-lines))
;;          (par-lines (second split-lines))
;;          (ignore-res-fn (if remove-ignored
;;                             (lambda (id)
;;                               (or (ignored? id)
;;                                   (not (gethash id
;;                                                 tartab))))
;;                             (lambda (id)
;;                               (not (gethash id tartab)))))
;;          (ignore-par-fn (lambda (id)
;;                           (not (member id
;;                                        (gethash *project-id* *params-table*)
;;                                        :key #'car))))
;;          (unremoved-res-lines
;;           (remove-if
;;            (lambda (line)
;;              (let ((id (read-from-string line)))
;;                (funcall ignore-res-fn id)))
;;            res-lines))
;;          (unremoved-par-lines
;;           (remove-if
;;            (lambda (line)
;;              (let ((id (read-from-string line)))
;;                (funcall ignore-par-fn id)))
;;            par-lines))
;;          (res-ids
;;           (mapcar
;;            (lambda (line)
;;              (read-from-string line))
;;            res-lines))
;;          (par-ids
;;           (mapcar
;;            (lambda (line)
;;              (read-from-string line))
;;            par-lines))
;;          (removed-res-ids
;;           (remove-if-not
;;            (lambda (id)
;;              (funcall ignore-res-fn id))
;;            res-ids))
;;          (removed-res-lids
;;           (mapcar (lambda (id)
;;                     (gethash id
;;                              (gethash *project-id*
;;                                       *proj->res->lid*)))
;;                   removed-res-ids))
;;          (removed-par-ids
;;           (remove-if-not
;;            (lambda (id)
;;              (funcall ignore-par-fn id))
;;            par-ids))
;;          (removed-par-lids
;;           (mapcar (lambda (id)
;;                     (gethash id
;;                              (gethash *project-id*
;;                                       *proj->par->lid*)))
;;                   removed-par-ids)))
;;     ;; overwrite index file:
;;     (with-open-file (index-file (merge-pathnames "index"
;;                                                  version-path)
;;                                 :direction :output
;;                                 :if-does-not-exist :create
;;                                 :if-exists :supersede)
;;       (loop
;;          for line in res-lines
;;          do (format index-file "~a~%" line))
;;       (format index-file "~%")
;;       (loop
;;          for line in par-lines
;;          do (format index-file "~a~%" line)))
;;     ;; remove content:
;;     (labels ((remove-target-files (lid)
;;                (let ((sublids
;;     (loop
;;        for line in (append (set-difference res-lines
;;                                            unremoved-res-lines
;;                                            :test #'equal)
;;                            (set-difference par-lines
;;                                            unremoved-par-lines
;;                                            :test #'equal))
;;        do (let ((lid (progn
;;                        (with-input-from-string (s line)
;;                          (read s)
;;                          (read s)))))
;;             (delete-file (merge-pathnames (mkstr lid)
;;                                           version-path))))))
