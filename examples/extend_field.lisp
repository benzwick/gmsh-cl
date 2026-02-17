;;; extend_field.lisp — Extended mesh size field

(let* ((b (occ:box 0 0 0 1 1 0.5))
       (s (occ:sphere 1 1 0.5 0.4))
       (c (occ:cut (list (cons 3 b)) (list (cons 3 s))))
       (b1 (occ:box 0.3 0.3 0.4 0.1 0.1 0.1))
       (b2 (occ:box 0.5 0.5 0.4 0.1 0.1 0.1)))
  (occ:fragment (list (cons 3 b1) (cons 3 b2)) c)
  (occ:synchronize)

  (let ((size-bulk 0.04)
        (size-small 0.002)
        (dist-max 0.2)
        (power 2))

    (mesh:set-size (gmsh:get-entities :dim 0) size-bulk)
    (mesh:set-size (gmsh:get-boundary (list (cons 3 b1) (cons 3 b2)) :recursive t) size-small)

    ;; "Extend" field
    (let ((f (mesh:field-add "Extend")))
      (mesh:field-set-numbers f "SurfacesList"
                              (mapcar #'cdr (gmsh:get-entities :dim 2)))
      (mesh:field-set-numbers f "CurvesList"
                              (mapcar #'cdr (gmsh:get-entities :dim 1)))
      (mesh:field-set-number f "DistMax" dist-max)
      (mesh:field-set-number f "SizeMax" size-bulk)
      (mesh:field-set-number f "Power" power)
      (mesh:field-set-as-background-mesh f)))

  (opt:set-number "Mesh.MeshSizeExtendFromBoundary" 0)
  (opt:set-number "Mesh.Algorithm3D" 10))
