(require 'statistics)

(in-package :statistics)

(print
 (moving-average (list 1f0 2f0 3f0 4f0 5f0 6f0)
                 2))
