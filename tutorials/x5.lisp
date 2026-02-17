;;; x5.lisp — Additional geometrical data: parametrizations, normals, curvatures

;; The API provides access to geometrical data in a CAD kernel agnostic manner.

;; Create a simple CAD model by fusing a sphere and a cube, then mesh the
;; surfaces:
(gmsh:add "x5")
(let ((s (occ:sphere 0 0 0 1))
      (b (occ:box 0.5 0 0 1.3 2 3)))
  (occ:fuse (list (cons 3 s)) (list (cons 3 b))))
(occ:synchronize)
(mesh:generate :dim 2)

;; We can retrieve the exact normals and the curvature at all the mesh nodes
;; (i.e. not normals and curvatures computed from the mesh, but directly
;; evaluated on the geometry), by querying the CAD kernels at the corresponding
;; parametric coordinates.
(let ((normals '())
      (curvatures '()))

  ;; For each surface in the model:
  (dolist (e (gmsh:get-entities :dim 2))
    (let ((s (cdr e)))

      ;; Get the mesh nodes on the surface, including those on the boundary
      (multiple-value-bind (tags coord param)
          (mesh:get-nodes :dim 2 :tag s :include-boundary t)
        (declare (ignore tags))

        ;; Get the surface normals at the parametric coordinates of the nodes
        (let ((norm (gmsh:get-normal s param))
              (curv (gmsh:get-curvature 2 s param)))

          ;; Store the normals and curvatures for display as list-based
          ;; post-processing views
          (loop for i below (length coord) by 3 do
            (push (nth i coord) normals)
            (push (nth (+ i 1) coord) normals)
            (push (nth (+ i 2) coord) normals)
            (push (nth i norm) normals)
            (push (nth (+ i 1) norm) normals)
            (push (nth (+ i 2) norm) normals)
            (push (nth i coord) curvatures)
            (push (nth (+ i 1) coord) curvatures)
            (push (nth (+ i 2) coord) curvatures)
            (push (nth (/ i 3) curv) curvatures))))))

  (setf normals (nreverse normals))
  (setf curvatures (nreverse curvatures))

  ;; Create a list-based vector view on points to display the normals, and a
  ;; scalar view on points to display the curvatures
  (let ((vn (view:add "normals")))
    (view:add-list-data vn "VP" (/ (length normals) 6) normals)
    (view:option-set-number vn "ShowScale" 0)
    (view:option-set-number vn "ArrowSizeMax" 30)
    (view:option-set-number vn "ColormapNumber" 19))
  (let ((vc (view:add "curvatures")))
    (view:add-list-data vc "SP" (/ (length curvatures) 4) curvatures)
    (view:option-set-number vc "ShowScale" 0)))

;; We can also retrieve the parametrization bounds of model entities, e.g. of
;; curve 5, and evaluate the parametrization for several parameter values:
(multiple-value-bind (pmin pmax)
    (gmsh:get-parametrization-bounds 1 5)
  (let* ((n 20)
         (params (loop for i below n
                       collect (+ (first pmin)
                                  (* i (/ (- (first pmax) (first pmin)) n)))))
         (xyz1 (gmsh:get-value 1 5 params)))

    ;; We can also reparametrize curve 5 on surface 1, and evaluate the points
    ;; in the parametric plane of the surface:
    (let* ((uv (gmsh:reparametrize-on-surface 1 5 params 1))
           (xyz2 (gmsh:get-value 2 1 uv)))

      ;; Hopefully we get the same x, y, z coordinates!
      (if (< (reduce #'max (mapcar (lambda (a b) (abs (- a b))) xyz1 xyz2)) 1e-12)
          (logger:write "Evaluation on curve and surface match!")
          (logger:write "Evaluation on curve and surface do not match!"
                        :level "error")))))
