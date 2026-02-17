;;; glue_and_remesh_stl.lisp — Glue two STL surfaces and remesh as volume
;;;
;;; This example requires surface1.stl and surface2.stl from the gmsh examples

(let ((path1 (namestring (merge-pathnames "_reference/gmsh/examples/api/surface1.stl"
                                          (asdf:system-source-directory :gmsh-cl))))
      (path2 (namestring (merge-pathnames "_reference/gmsh/examples/api/surface2.stl"
                                          (asdf:system-source-directory :gmsh-cl)))))
  (when (and (probe-file path1) (probe-file path2))
    (gmsh:merge path1)
    (gmsh:merge path2)

    ;; Merge nodes at the same position
    (opt:set-number "Geometry.Tolerance" 1e-4)
    (mesh:remove-duplicate-nodes)

    ;; Classify surface mesh and create discrete entities
    (mesh:classify-surfaces (/ pi 2))

    ;; Create geometry for discrete curves and surfaces
    (mesh:create-geometry)

    ;; Get all surfaces and create a volume
    (let* ((s (gmsh:get-entities :dim 2))
           (l (geo:surface-loop (mapcar #'cdr s))))
      (geo:volume (list l)))

    (geo:synchronize)

    (opt:set-number "Mesh.Algorithm" 6)
    (opt:set-number "Mesh.MeshSizeMin" 0.4)
    (opt:set-number "Mesh.MeshSizeMax" 0.4)
    (mesh:generate :dim 3)))
