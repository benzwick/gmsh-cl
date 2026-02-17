;;; heal.lisp — Heal shapes from a STEP file
;;;
;;; This example requires the as1-tu-203.stp file from the gmsh examples

(let ((path (namestring (merge-pathnames "_reference/gmsh/examples/api/as1-tu-203.stp"
                                         (asdf:system-source-directory :gmsh-cl)))))
  (when (probe-file path)
    (gmsh:open path)
    (occ:heal-shapes)
    (occ:synchronize)))
