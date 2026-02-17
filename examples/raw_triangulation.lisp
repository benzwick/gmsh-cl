;;; raw_triangulation.lisp — Triangulate random 2D points

(let* ((n 100)
       ;; Generate random 2D points
       (points (loop repeat (* 2 n) collect (- (random 2.0) 1.0))))

  (let ((tris (algorithm:triangulate points)))

    ;; Create a discrete surface to visualize
    (let ((surf (gmsh:add-discrete-entity 2))
          ;; Build xyz coordinates (add z=0)
          (xyz (loop for i from 0 below (length points) by 2
                     append (list (nth i points) (nth (1+ i) points) 0.0))))
      (mesh:add-nodes 2 surf (loop for i from 1 to n collect i) xyz)
      (mesh:add-elements-by-type surf 2 '() tris))))
