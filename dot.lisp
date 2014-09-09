(in-package :makeres-graphviz)

(defun dot-compile (path &key (if-exists :error))
  "Writes target graph into a file located at path.  Returns path of
dot output file."
  (let ((*print-pretty* nil))
    (with-open-file (file path
                          :direction :output
                          :if-exists if-exists)
      (format file "digraph \"~a\" {~%"
              (project))
      (loop
         for id being the hash-keys in (target-table)
         for tar being the hash-values in (target-table)
         do (let ((deps (target-deps tar)))
              (loop
                 for d in deps
                 do (format file "  \"~a\" -> \"~a\";~%"
                            d id))))
      (format file "}~%")
      path)))

(defun dot->ps (from-path to-path)
  "Runs dot command to convert dot code in from-path to a ps file at
to-path"
  (run "dot"
       (list from-path
             "-Tps"
             "-o"
             to-path)))

(defun dot->png (from-path to-path)
  "Runs dot command to convert dot code in from-path to a png file at
to-path"
  (run "dot"
       (list from-path
             "-Tpng"
             "-o"
             to-path)))
