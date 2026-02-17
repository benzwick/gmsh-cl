;;; aneurysm.lisp — Remesh an STL aneurysm with boundary layers
;;;
;;; This example requires the aneurysm_data.stl file from the gmsh examples

(let ((path (namestring (merge-pathnames "_reference/gmsh/examples/api/aneurysm_data.stl"
                                         (asdf:system-source-directory :gmsh-cl)))))
  (when (probe-file path)
    (gmsh:merge path)
    (mesh:classify-surfaces pi :for-reparametrization t)
    (mesh:create-geometry)

    ;; Make extrusions only return "top" surfaces and volumes
    (opt:set-number "Geometry.ExtrudeReturnLateralEntities" 0)

    ;; Extrude a boundary layer of 4 elements using mesh normals
    (let ((e1 (geo:extrude-boundary-layer (gmsh:get-entities :dim 2)
                                          :num-elements '(4) :heights '(0.5)
                                          )))

      ;; Extrude a second boundary layer in the opposite direction
      (let ((e2 (geo:extrude-boundary-layer (gmsh:get-entities :dim 2)
                                            :num-elements '(4) :heights '(-0.5)
                                             :second t)))

        ;; Get "top" surfaces created by extrusion
        (let ((top-surf (mapcar #'cdr (remove-if-not (lambda (s) (= (car s) 2)) e2))))
          (geo:synchronize)

          ;; Get boundary of top surfaces (boundaries of holes)
          (let* ((bnd-ent (gmsh:get-boundary (remove-if-not (lambda (s) (= (car s) 2)) e2)))
                 (bnd-curv (mapcar #'cdr bnd-ent))
                 (loops (geo:curve-loops bnd-curv))
                 (bnd-surf (mapcar (lambda (l) (geo:plane-surface (list l))) loops)))

            ;; Create the inner volume
            (let ((vf (geo:volume (list (geo:surface-loop (append top-surf bnd-surf))))))
              (geo:synchronize)

              (gmsh:add-physical-group 3 (mapcar #'cdr (remove-if-not (lambda (v) (= (car v) 3)) e1))
                                      :name "solid")
              (gmsh:add-physical-group 3 (mapcar #'cdr (remove-if-not (lambda (v) (= (car v) 3)) e2))
                                      :name "fluid bl")
              (gmsh:add-physical-group 3 (list vf) :name "fluid"))))))

    ;; Use MeshAdapt for the not-so-smooth parametrizations
    (opt:set-number "Mesh.Algorithm" 1)
    (opt:set-number "Mesh.MeshSizeFactor" 0.1)))
