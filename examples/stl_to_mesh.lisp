;;; stl_to_mesh.lisp — Import STEP geometry as STL mesh and recombine
;;;
;;; This example requires the as1-tu-203.stp file from the gmsh examples

(let ((path (namestring (merge-pathnames "_reference/gmsh/examples/api/as1-tu-203.stp"
                                         (asdf:system-source-directory :gmsh-cl)))))
  (when (probe-file path)
    (gmsh:open path)

    (occ:remove-all-duplicates)
    (occ:synchronize)

    ;; Set STL generation options
    (opt:set-number "Mesh.StlLinearDeflection" 1)
    (opt:set-number "Mesh.StlLinearDeflectionRelative" 0)
    (opt:set-number "Mesh.StlAngularDeflection" 0.5)

    ;; Import the model STL as a mesh
    (mesh:import-stl)
    (mesh:remove-duplicate-nodes)

    ;; Create quads
    (opt:set-number "Mesh.RecombinationAlgorithm" 0)
    (opt:set-number "Mesh.RecombineOptimizeTopology" 0)
    (opt:set-number "Mesh.RecombineNodeRepositioning" 0)
    (opt:set-number "Mesh.RecombineMinimumQuality" 1e-3)
    (mesh:recombine)))
