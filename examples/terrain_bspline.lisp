;;; terrain_bspline.lisp — Terrain surface using a BSpline surface

(gmsh:add "terrain")

;; Create terrain surface from N x N input data points
(let* ((n 100)
       (ps (loop for i below n
                 append (loop for j below n
                              collect (occ:point (/ (coerce i 'double-float) (1- n))
                                                 (/ (coerce j 'double-float) (1- n))
                                                 (* 0.05 (sin (* 10 (/ (coerce (+ i j) 'double-float) (1- n))))))))))
  (let ((s (occ:b-spline-surface ps n)))

    ;; Create a box
    (let ((v (occ:box 0 0 -0.5 1 1 1)))

      ;; Fragment the box with the bspline surface
      (occ:fragment (list (cons 2 s)) (list (cons 3 v))))))

(occ:synchronize)

(opt:set-number "Mesh.MeshSizeMin" 0.05)
(opt:set-number "Mesh.MeshSizeMax" 0.05)
