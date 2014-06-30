;;;; Implements a directed graph with content at each node along with
;;;; id.
;;;;
;;;; Each node specifies nodes connected with "away" arrows from
;;;; itself.

(in-package :depgraph)

(defun node (id deps content)
  "Creates a node with id, dependencies deps, and content.

deps is a list of ids which are dependencies of this node."
  (list id deps content))

(defun node? (x)
  "Determines whether it is possible to interpret a datum as a node"
  (and (listp x)
       (length-equal x 3)
       (listp (second x))))

(defun node-id (node)
  (first node))

(defun (setf node-id) (value node)
  (setf (first node)
        value))

(defun node-deps (node)
  (second node))

(defun (setf node-deps) (value node)
  (setf (second node)
        value))

(defun node-content (node)
  (third node))

(defun (setf node-content) (value node)
  (setf (third node) value))

(defun hash-graph (&key nodes (test 'equal))
  "Creates a graph implemented via hash table with optional initial
list of nodes and test for hash table."
  (let ((res
         (make-hash-table :test test)))
    (loop
       for n in nodes
       do (setf (gethash (node-id n) res)
                n))
    res))

(defun hg (&rest args)
  "Shorthand for hash-graph"
  (apply #'hash-graph args))

(defun hash-graph->arrows (hg)
  "Returns alist mapping node to dependent node for all arrows in
hash-graph hg"
  (let ((res nil))
    (maphash (lambda (from n)
               (let ((tos (node-deps n)))
                 (loop
                    for to in tos
                    do (push (cons from to)
                             res))))
             hg)
    res))

(defun hg->arr (&rest args)
  "Shorthand for hash-graph->arrows"
  (apply #'hash-graph->arrows args))

(defun hgref (id hg)
  "Looks up node with id in hash-graph hg"
  (gethash id hg))

(defun (setf hgref) (value id hg)
  "It does not appear possible to use setf to change the id of a node
automatically (at least not that I have found), so if you wish to
change the id you must remove the old node and add a new one with the
updated id."
  (setf (gethash id hg)
        value))

(defun hgrem! (id hg)
  "Removes a node with id from hash-graph hg"
  (remhash id hg))
