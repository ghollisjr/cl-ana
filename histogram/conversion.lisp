(in-package :histogram)

(defun sparse->contiguous (histogram)
  "converts a sparse histogram into a contiguous one.  Note that this
is dangerous in cases where sparse histograms are actually necessary."
  (if (typep histogram 'sparse-histogram)
      (let* ((dim-spec-plists
              (bin-spec-plists
               histogram))
             (bin-values
              (hist-bin-values histogram))
             (hist
              (make-contiguous-hist dim-spec-plists)))
        (loop
           for datum in bin-values
           do (hist-insert hist (rest datum) (first datum)))
        hist)
      histogram))

(defun contiguous->sparse (histogram)
  "converts a sparse histogram into a contiguous one.  Note that this
is dangerous in cases where sparse histograms are actually necessary."
  (if (typep histogram 'contiguous-histogram)
      (let* ((dim-spec-plists
              (bin-spec-plists
               histogram))
             (bin-values
              (hist-bin-values histogram))
             (hist
              (make-sparse-hist dim-spec-plists)))
        (loop
           for datum in bin-values
           do (hist-insert hist (rest datum) (first datum)))
        hist)
      histogram))
