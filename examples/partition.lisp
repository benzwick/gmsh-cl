;;; partition.lisp — Mesh partitioning

(gmsh:add "test")
(occ:rectangle 0 0 0 1 1)
(occ:synchronize)
(mesh:generate :dim 2)

;; Partition using the SimplePartition plugin (chessboard-like)
(plugin:set-number "SimplePartition" "NumSlicesX" 3.0)
(plugin:run "SimplePartition")

;; Iterate over partitioned entities and print some info
(dolist (e (gmsh:get-entities))
  (let ((partitions (gmsh:get-partitions (car e) (cdr e))))
    (when (plusp (length partitions))
      (format t "Entity ~A of type ~A~%" e (gmsh:get-type (car e) (cdr e)))
      (format t " - Partition(s): ~A~%" partitions)
      (format t " - Parent: ~A~%" (gmsh:get-parent (car e) (cdr e)))
      (format t " - Boundary: ~A~%" (gmsh:get-boundary (list e))))))
