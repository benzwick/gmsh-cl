;;; x3d_export.lisp — Exporting meshes to X3D format

;; This example requires the as1-tu-203.stp file from the gmsh examples
(let ((path (namestring (merge-pathnames "_reference/gmsh/examples/api/as1-tu-203.stp"
                                         (asdf:system-source-directory :gmsh-cl)))))
  (when (probe-file path)
    (gmsh:open path)

    (opt:set-number "Print.X3dSurfaces" 2)
    (opt:set-number "Print.X3dVolumes" 0)
    (opt:set-number "Print.X3dColorize" 1)

    ;; (gmsh:write "/tmp/out.x3d")
    (gmsh:clear)))
