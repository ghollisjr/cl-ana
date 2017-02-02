;;;; cl-ana is a Common Lisp data analysis library.
;;;; Copyright 2013-2015 Gary Hollis
;;;;
;;;; This file is part of cl-ana.
;;;;
;;;; cl-ana is free software: you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; cl-ana is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with cl-ana.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;; You may contact Gary Hollis (me!) via email at
;;;; ghollisjr@gmail.com

(in-package :cl-ana.makeres)

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

(defun run-prog (program args)
  (run program args
       ;; :input *standard-input*
       :output *standard-output*
       :error *error-output*))

(defvar *load-function-map*
  (make-hash-table)
  "Map from method-symbol to cons pair of target id test function and
  load method")

(defvar *save-function-map*
  (make-hash-table)
  "Map from method-symbol to cons pair of target id test function and
  save method")

(defmacro define-save-target-method (method-symbol id test &body save-body)
  (alexandria:with-gensyms (test-fn save-fn)
    `(let ((,test-fn (lambda (,id)
                       ,test))
           (,save-fn (lambda (,id)
                       ,@save-body)))
       (setf (gethash ',method-symbol *save-function-map*)
             (cons ,test-fn ,save-fn)))))

(defmacro define-load-target-method (method-symbol id test &body load-body)
  (alexandria:with-gensyms (test-fn load-fn)
    `(let ((,test-fn (lambda (,id)
                       ,test))
           (,load-fn (lambda (,id)
                       ,@load-body)))
       (setf (gethash ',method-symbol *load-function-map*)
             (cons ,test-fn ,load-fn)))))

(defun load-target (id)
  "Loads a target given the target id."
  (let ((*print-pretty* nil))
    (cond
      ((not (or (gethash id
                         (target-table))
                (gethash id (gethash *project-id*
                                     *fin-target-tables*))))
       (format t "Warning: ~s not found in target table~%"
               id)
       nil)
      ((ignored? id)
       nil)
      (;; Specific method found:
       (loop
          for sym being the hash-keys in *load-function-map*
          for (test . load) being the hash-values in *load-function-map*
          when (and (funcall test id)
                    (probe-file (target-path id)))
          do
            (funcall load id)
            (setf (target-load-stat (gethash id (target-table)))
                  t)
            (setf (target-load-stat (gethash id
                                             (gethash *project-id*
                                                      *fin-target-tables*)))
                  t)
            (return nil)
          finally (return t))
       ;; Default execution when no specific method found:

       (when (probe-file (target-path id))
         (flet ((read-from-path (path)
                  (with-open-file (file path
                                        :direction :input)
                    (read file))))
           (let ((type (read-from-path (target-path id "type")))
                 ;; (form (read-from-path (target-path id "form")))
                 (timestamp (read-from-path (target-path id "timestamp"))))
             (if (and (gethash id (target-table))
                      (not (logged-form-equal id)))
                 (format t
                         "Warning: ~s logged target expression not equal to~%~
                       expression in target table, skipping.~%"
                         id)
                 (progn
                   ;; final target value
                   (setf (target-val (gethash id
                                              (gethash *project-id*
                                                       *fin-target-tables*)))
                         (load-object type (target-path id "data")))
                   (setf (target-load-stat
                          (gethash id
                                   (gethash *project-id*
                                            *fin-target-tables*)))
                         t)
                   (setf (target-timestamp
                          (gethash id
                                   (gethash *project-id*
                                            *fin-target-tables*)))
                         timestamp)
                   (when (gethash id (target-table))
                     (setf (target-val (gethash id (target-table)))
                           (target-val
                            (gethash
                            id
                            (gethash *project-id* *fin-target-tables*))))
                     (setf (target-load-stat (gethash id (target-table)))
                           t)
                     (setf (target-timestamp (gethash id (target-table)))
                           timestamp))
                   ))))))
      (t nil))))

(defun save-target (id)
  "Saves a target given the target id.  When destruct-on-save? is T
  for result value, reloads the result value after saving."
  ;; debug: trying nil for now
  (let ((*print-pretty* nil))
    (cond
      ((not (or (gethash id
                         (target-table))
                (gethash id
                         (gethash *project-id*
                                  *fin-target-tables*))))
       (format t "Warning: ~s not found in target table~%"
               id)
       nil)
      ((ignored? id)
       nil)
      (;; Specific method found:
       (loop
          for sym being the hash-keys in *save-function-map*
          for (test . save) being the hash-values in *save-function-map*
          when (funcall test id)
          do
          ;; delete save-path recursively
            (when (probe-file (target-path id))
              #+sbcl
              (sb-ext:delete-directory (target-path id)
                                       :recursive t))
            (ensure-directories-exist (target-path id))
            (let ((destruct-on-save?
                   (destruct-on-save?
                    (target-val (or (gethash id (target-table))
                                    (gethash id
                                             (gethash *project-id*
                                                      *fin-target-tables*)))))))
              
              (flet ((write-to-path (object path)
                       (with-open-file (file path
                                             :direction :output
                                             :if-does-not-exist :create
                                             :if-exists :supersede)
                         (format file "~s~%" object))))
                (let ((tar (or (gethash id (target-table))
                               (gethash id
                                        (gethash *project-id*
                                                 *fin-target-tables*)))))
                  (write-to-path (target-type (target-val tar))
                                 (target-path id "type"))
                  (with-open-file (form-file (target-path id "form")
                                             :direction :output
                                             :if-does-not-exist :create
                                             :if-exists :supersede)


                    (format form-file "~s~%"
                            (with-output-to-string (s)
                              (format s "~s" (target-expr tar)))))
                  (write-to-path (target-timestamp tar)
                                 (target-path id "timestamp"))
                  (funcall save id)))
              (when destruct-on-save?
                (load-target id)))
            (return nil)
          finally (return t))
       ;; delete save-path recursively
       (when (probe-file (target-path id))
         #+sbcl
         (sb-ext:delete-directory (target-path id)
                                  :recursive t))
       (ensure-directories-exist (target-path id))
       (let ((destruct-on-save?
              (destruct-on-save?
               (target-val (or (gethash id (target-table))
                               (gethash id
                                        (gethash *project-id*
                                                 *fin-target-tables*)))))))
         (flet ((write-to-path (object path)
                  (with-open-file (file path
                                        :direction :output
                                        :if-does-not-exist :create
                                        :if-exists :supersede)
                    (format file "~s~%" object))))
           (let ((tar (or (gethash id (target-table))
                          (gethash id
                                   (gethash *project-id*
                                            *fin-target-tables*)))))
             (write-to-path (target-type (target-val tar))
                            (target-path id "type"))
             (with-open-file (form-file (target-path id "form")
                                        :direction :output
                                        :if-does-not-exist :create
                                        :if-exists :supersede)
               (format form-file "~s~%"
                       (with-output-to-string (s)
                         (format s "~s" (target-expr tar)))))
             (write-to-path (target-timestamp tar)
                            (target-path id "timestamp"))
             (save-object (target-val tar)
                          (target-path id "data"))))
         (when destruct-on-save?
           (load-target id))))
      (t nil)))
  nil)

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

(defgeneric printable (object)
  (:documentation "Returns true only for objects which can safely be
  printed")
  (:method (object)
    t))

(defgeneric cleanup (object)
  (:documentation "Clears any resources needing manual intervention
  for object.  Examples would include files and tables")
  (:method (object)
    nil))

(defgeneric destruct-on-save? (object)
  (:documentation "Returns true if target needs re-opening after
  saving (e.g. tables, files)")
  (:method (object)
    nil))

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
  (symbol-macrolet ((tar (gethash id (target-table)))
                    (fintar
                     (gethash id
                              (gethash *project-id*
                                       *fin-target-tables*))))
    (or (and tar
             (target-stat tar)
             (functionp (target-val tar)))
        (and fintar
             (target-stat fintar)
             (functionp (target-val fintar))))))

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
     (current-path))
    ;; initialize project vars:
    (setf (gethash (project) *proj->res->lid*)
          (make-hash-table :test 'equal))
    ;; and sublid map:
    (setf (gethash (project) *proj->lid->sublids*)
          (make-hash-table :test 'equal))))

(defun load-project (&key ignore-form-p)
  "Searches for log directory for each target in the target-table,
setting the target-stat to t for any targets which are found in the
log."
  (let ((unset (make-hash-table :test 'equal)))
    (when (probe-file (computation-stat-path))
      (format t "Warning: Previous computation failed, recovering~%")
      (let (timestamp
            ids
            ts)
        (with-open-file (stat-file (computation-stat-path)
                                   :direction :input)
          (setf timestamp (read stat-file))
          (setf ids (read stat-file)))
        (loop
           for id in ids
           do
             (if (probe-file (target-path id "timestamp"))
                 (with-open-file (file (target-path id "timestamp")
                                       :direction :input)
                   (setf ts (read file)))
                 (setf ts nil))
             (when (or (not ts)
                       (> timestamp ts))
               (format t "Warning: Unsetting ~a from failed computation~%"
                       id)
               (setf (gethash id unset)
                     t)))))
    (loop
       for id being the hash-keys in (target-table)
       for tar being the hash-values in (target-table)
       do
         (cond
           ((ignored? id)
            (when (probe-file (target-path id))
              (format t
                      "Warning: ~s is ignored, but log present; skipping.~%"
                      id)))
           ((and (probe-file (target-path id))
                 (not ignore-form-p)
                 (not (logged-form-equal id)))
            (format t "Warning: ~s logged target expression not equal to~%~
                    expression in target table, unsetting dependents.~%"
                    id)
            ;; (unsetresfn id)
            (loop
               for r in (cons id (res-dependents id (target-table)))
               do (setf (gethash r unset)
                        t))
            nil)
           ((and (probe-file (target-path id))
                 (not (gethash id unset)))
            (setf (target-stat tar) t))
           (t nil)))
    (loop
       for r being the hash-keys in unset
       when (gethash r (target-table))
       do (unsetresfn r))
    nil))

;; utility for use in load-project and others
(defun logged-form-equal (id)
  "Checks to see if logged expression for id is the same as the one
loaded into the Lisp image"
  (let ((*print-pretty* nil))
    (when (probe-file (target-path id "form"))
      (with-open-file (form-file (target-path id "form")
                                 :direction :input)
        (let ((form (read form-file)))
          (string= form
                   (with-output-to-string (s)
                     (format s "~s"
                             (target-expr (gethash id (target-table)))))))))))

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
    (run-prog "/usr/bin/ln"
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
  (let* ((*print-pretty* nil)
         (project-path (gethash (project) *project-paths*))
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
                       (handler-case (not (ignored? res))
                         (error nil nil)))
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
           when (and (handler-case (not (ignored? res))
                       (error nil nil))
                     (handler-case (target-stat (gethash res tartab))
                       (error nil nil)))
           do (let ((type (target-type (resfn res)))
                    (timestamp (target-timestamp
                                (gethash res tartab)))
                    (form (target-expr (gethash res tartab))))
                (format index-file
                        "~a ~a ~a ~a ~s~%"
                        res lid type timestamp
                        (with-output-to-string (s)
                          (format s "~s" form))))
           when (not (handler-case (target-stat (gethash res tartab))
                       (error nil nil)))
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
         when (and (handler-case (not (ignored? id))
                     (error nil nil))
                   (handler-case (target-stat (gethash id tartab))
                     (error nil nil)))
         do (let* ((lid (gethash id res->lid))
                   (path (merge-pathnames (mkstr lid)
                                          save-path)))
              (format t "Saving ~a~%" id)
              (save-object lid
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
              (save-object plid
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
          #+sbcl (sb-ext:delete-directory work-to-path :recursive t))
        (format t "Saving work/ files~%")
        (run-prog "/usr/bin/cp"
                  (list "-r"
                        (namestring work-from-path)
                        (namestring work-to-path)))))
    ;; Save sublid map:
    (save-sublid-map version-string))
  nil)

;; (defun load-project (version &key
;;                                (work-p t)
;;                                load-changed-targets-p)
;;   "work-p nil = assume work files already present"
;;   ;; unset target statuses
;;   (loop
;;      for res being the hash-keys in (target-table)
;;      do (unsetresfn res))
;;   ;; reset res->lid
;;   (setf (gethash (project) *proj->res->lid*)
;;         (make-hash-table :test 'equal))
;;   (let* ((*print-pretty* nil)
;;          (project-path (gethash (project) *project-paths*))
;;          (tartab (gethash (project) *target-tables*))
;;          (res->lid (gethash (project) *proj->res->lid*))
;;          (load-path (ensure-absolute-pathname version)))
;;     ;; load *last-id*:
;;     (load-last-id load-path)
;;     ;; load working files:
;;     (when work-p
;;       (format t "Loading work/ files~%")
;;       (let ((work-from-path (merge-pathnames "work/"
;;                                              load-path))
;;             (work-to-path (merge-pathnames "work/"
;;                                            project-path)))
;;         (when (probe-file work-to-path)
;;           #+sbcl (sb-ext:delete-directory work-to-path :recursive t))
;;         (run-prog "cp"
;;              (list "-r"
;;                    (namestring work-from-path)
;;                    (namestring work-to-path)))))
;;     ;; load results:
;;     (let ((index-lines
;;            (read-lines-from-pathname
;;             (merge-pathnames "index"
;;                              load-path))))
;;       (destructuring-bind (res-lines par-lines)
;;           (split-sequence:split-sequence "" index-lines
;;                                          :test #'equal)
;;         (loop
;;            for line in res-lines
;;            do
;;              (with-input-from-string (s line)
;;                (let ((id (read s))
;;                      (lid (read s))
;;                      (type (read s))
;;                      ;; These two can be handled for legacy purposes
;;                      ;; if not present in index file so they won't
;;                      ;; throw errors
;;                      (timestamp (read s nil nil))
;;                      (form (read s nil nil)))
;;                  (cond
;;                    ((not (gethash id tartab))
;;                     (format
;;                      t
;;                      "Warning: result ~a not present in target table, skipping~%"
;;                      id))
;;                    ((ignored? id)
;;                     (format t "Warning: result ~a is ignored, skipping~%" id)
;;                     ;; Unset dependencies of this target, as otherwise
;;                     ;; there may be inconsistent values
;;                     (loop
;;                        for r in (res-dependents id (target-table))
;;                        do (unsetresfn r)))
;;                    ((and (not (string= form
;;                                        (with-output-to-string (s)
;;                                          (format s "~s"
;;                                                  (target-expr (gethash id tartab))))))
;;                          (not load-changed-targets-p))
;;                     (format
;;                      t
;;                      "Warning: ~s logged target expression not equal to~%~
;;                      expression in target table, skipping.~%"
;;                      id))
;;                    (t
;;                     (format t "Loading ~a~%" id)
;;                     (setf (gethash id res->lid) lid)
;;                     (when (not (ignored? id))
;;                       (setresfn id
;;                                 (load-object type
;;                                              (merge-pathnames (mkstr lid)
;;                                                               load-path))
;;                                 timestamp)))))))
;;         ;; parameters:
;;         (setf (gethash *project-id* *makeres-args*)
;;               (make-hash-table :test 'eq))
;;         (loop
;;            for line in par-lines
;;            do
;;              (with-input-from-string (s line)
;;                (let ((id (read s))
;;                      (lid (read s))
;;                      (type (read s)))
;;                  (if (member id (gethash *project-id* *params-table*)
;;                              :key #'car)
;;                      (progn
;;                        (format t "Loading parameter ~a~%" id)
;;                        (setf (gethash id
;;                                       (gethash *project-id*
;;                                                *makeres-args*))
;;                              (load-object type
;;                                           (merge-pathnames (mkstr lid)
;;                                                            load-path))))
;;                      (format
;;                       t
;;                       "WARNING: Parameter ~a not in project, skipping~%"
;;                       id))))))))
;;   ;; load sublid-map:
;;   (load-sublid-map version)
;;   nil)

;;;; Utility functions:

(defun current-path ()
  "Returns the path to the current log"
  (merge-pathnames (make-pathname :directory '(:relative "current"))
                   (project-path)))

(defun target-path (id &optional subpath)
  "Returns the path to the current log location for given target, or
optionally a subpath formed by concatenating subpath to the target's
log directory."
  (let ((*print-pretty* nil))
    (let ((id-path
           (format nil "~s" id)))
      (when (pathname-directory id-path)
        (error "ID ~s yields illegal pathname" id))
      (setf id-path
            (make-pathname :directory (list :relative id-path)))
      (when (and subpath
                 (pathname-directory subpath))
        (error "subpath ~s yields illegal path" subpath))
      (let* ((id+sub-path
              (if subpath
                  (merge-pathnames subpath
                                   (namestring id-path))
                  (namestring id-path)))
             (result
              (merge-pathnames
               (merge-pathnames id+sub-path
                                (make-pathname
                                 :directory '(:relative "targets")))
               (current-path))))
        (namestring result)))))

(defun work-path (path-or-format-recipe &rest args)
  "Returns namestring for a path in the project work directory.
path-or-format-recipe can be a pathname directly, in which case the
rest of the arguments are unused.  Or, it can be a format string which
when format is supplied with both the recipe and the rest of the
arguments should return a namestring for a valid pathname.  In either
case, ensure-directories-exist will be called to ensure that the path
is ready for use.

If for whatever reason work-path is given an absolute pathname, it
will be returned as-is.  If the result of a format processing a format
string and the rest of the arguments is an absolute pathname, this
will be returned."
  (let ((*print-pretty* nil))
    (let ((namestring
           (namestring
            (cond
              ((stringp path-or-format-recipe)
               (work-path
                (pathname (apply #'format nil path-or-format-recipe args))))
              ((pathnamep path-or-format-recipe)
               (if (pathname-absolute-p path-or-format-recipe)
                   path-or-format-recipe
                   (merge-pathnames path-or-format-recipe
                                    (merge-pathnames "work/"
                                                     (pathname (current-path))))))
              (t (error "work-path accepts strings or pathnames only for first argument"))))))
      (ensure-directories-exist namestring)
      namestring)))

;;;; Snapshot control:

(defun save-snapshot (name)
  "Saves a copy of the current analysis under (project-path)/name"
  (let ((destpath (merge-pathnames name
                                   (project-path))))
    (when (not (equal (pathname-directory destpath)
                      (pathname-directory (project-path))))
      (error "Snapshot path points outside project path"))
    (setf destpath
          (merge-pathnames (make-pathname :directory (list :relative name))
                           (project-path)))
    (when (equal (pathname destpath)
                 (current-path))
      (error "Snapshot cannot be named \"current\""))
    (when (probe-file destpath)
      (format t "Overwriting old snapshot at ~a~%"
              destpath)
      (sb-ext:delete-directory destpath :recursive t))
    (format t "Copying ~a to ~a~%"
            (current-path) destpath)
    (run "cp" (list "-r"
                    (namestring (current-path))
                    (namestring destpath))
         :output *standard-output*
         :error *error-output*)
    nil))

(defun load-snapshot (name backup
                      &key (makeres-p t))
  "Loads snapshot given by name and either copies current to backup if
backup is a valid path, throws an error for invalid backup path, or
does not backup if backup is NIL."
  (let ((sourcepath (merge-pathnames name
                                     (project-path))))
    (when (not (equal (pathname-directory sourcepath)
                      (pathname-directory (project-path))))
      (error "Snapshot path points outside project path"))
    (setf sourcepath
          (merge-pathnames (make-pathname :directory (list :relative name))
                           (project-path)))
    (when (equal (pathname sourcepath)
                 (current-path))
      (return-from load-snapshot nil))
    ;; make backup of current
    (when backup
      (let ((backuppath (merge-pathnames backup
                                         (project-path))))
        (when (not (equal (pathname-directory backuppath)
                          (pathname-directory (project-path))))
          (error "Backup path points outside project path"))
        (setf backuppath (merge-pathnames (make-pathname
                                           :directory (list :relative backup))
                                          (project-path)))
        (when (probe-file backuppath)
          (sb-ext:delete-directory backuppath :recursive t))
        (run-prog "/usr/bin/cp" (list "-r"
                                      (current-path)
                                      backuppath))))
    (sb-ext:delete-directory (current-path) :recursive t)
    (run-prog "/usr/bin/cp" (list "-r"
                                  sourcepath
                                  (current-path)))
    (loop
       for id being the hash-keys in (target-table)
       do
         (setf (target-stat (gethash id (target-table)))
               nil)
       ;;(cleanup (resfn id))
         (unload-target id))
    (load-project)
    (when makeres-p
      (makeres)))
  nil)
