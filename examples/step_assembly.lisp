;;; step_assembly.lisp — Load a STEP assembly and create physical groups from labels
;;;
;;; This example requires the as1-tu-203.stp file from the gmsh examples

(let ((path (namestring (merge-pathnames "_reference/gmsh/examples/api/as1-tu-203.stp"
                                         (asdf:system-source-directory :gmsh-cl)))))
  (when (probe-file path)
    (gmsh:open path)

    (opt:set-number "Mesh.MeshSizeFromCurvature" 15)
    (opt:set-number "Mesh.MeshSizeMax" 8)

    ;; Get all model entities and group by STEP labels
    (let ((physicals (make-hash-table :test 'equal)))
      (dolist (e (gmsh:get-entities))
        (let ((n (gmsh:get-entity-name (car e) (cdr e))))
          (when (and n (plusp (length n)))
            (format t "Entity ~A has label ~A (and mass ~A)~%"
                    e n (occ:get-mass (car e) (cdr e)))
            (let ((parts (uiop:split-string n :separator "/")))
              (when (and (= (car e) 3) (> (length parts) 3))
                (push (cdr e) (gethash (nth 2 parts) physicals '())))))))

      ;; Create the physical groups
      (maphash (lambda (name tags)
                 (let ((p (gmsh:add-physical-group 3 tags)))
                   (gmsh:set-physical-name 3 p name)))
               physicals))))
