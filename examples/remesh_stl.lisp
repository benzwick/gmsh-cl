;;; remesh_stl.lisp — Remesh an STL surface and create a volume
;;;
;;; This example requires the object.stl file from the gmsh examples

(let ((path (namestring (merge-pathnames "_reference/gmsh/examples/api/object.stl"
                                         (asdf:system-source-directory :gmsh-cl)))))
  (when (probe-file path)
    (gmsh:merge path)

    (opt:set-number "Mesh.Algorithm" 6)
    (opt:set-number "Mesh.MeshSizeMin" 0.75)
    (opt:set-number "Mesh.MeshSizeMax" 0.75)

    ;; Split input surface mesh based on angle threshold and create discrete surfaces
    (mesh:classify-surfaces (* 40 (/ pi 180)) :for-reparametrization t)

    ;; Create geometry for all discrete curves and surfaces
    (mesh:create-geometry)

    ;; Get all surfaces, create a surface loop and add a volume
    (let* ((s (gmsh:get-entities :dim 2))
           (l (geo:surface-loop (mapcar #'cdr s))))
      (geo:volume (list l)))

    (geo:synchronize)
    (mesh:generate :dim 3)))
