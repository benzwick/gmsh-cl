;;; raw_tetrahedralization.lisp — Tetrahedralize random 3D points

(opt:set-number "Mesh.Algorithm3D" 10)

(let* ((n 100)
       ;; Generate random 3D points
       (points (loop repeat (* 3 n) collect (- (random 2.0) 1.0))))

  (multiple-value-bind (tets neighbors)
      (algorithm:tetrahedralize points)
    (declare (ignore neighbors))

    ;; Create a discrete volume to visualize
    (let ((vol (gmsh:add-discrete-entity 3)))
      (mesh:add-nodes 3 vol (loop for i from 1 to n collect i) points)
      (mesh:add-elements-by-type vol 4 '() tets))))
