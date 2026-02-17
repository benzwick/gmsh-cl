;;; terrain_stl.lisp — Build a volume below a terrain from an STL surface
;;;
;;; This example requires the terrain_stl_data.stl file from the gmsh examples

(let ((path (namestring (merge-pathnames "_reference/gmsh/examples/api/terrain_stl_data.stl"
                                         (asdf:system-source-directory :gmsh-cl)))))
  (when (probe-file path)
    (gmsh:merge path)

    ;; Classify the surface mesh and create discrete model entities
    (mesh:classify-surfaces pi :curve-angle (/ pi 3))

    ;; Create geometry for the discrete curves and surfaces
    (mesh:create-geometry)

    ;; Retrieve the surface and its boundary curves
    (let ((s (gmsh:get-entities :dim 2))
          (z -1000))
      (let ((c (gmsh:get-boundary s)))
        (when (/= (length c) 4)
          (logger:write "Should have 4 boundary curves!" :level "error"))

        ;; Get corner points and their coordinates
        (let ((p '())
              (xyz '()))
          (dolist (e c)
            (let ((pt (gmsh:get-boundary (list e) :combined nil)))
              (push (cdr (first pt)) p)
              (setf xyz (nconc xyz (gmsh:get-value 0 (cdr (first pt)) '())))))
          (setf p (nreverse p))

          ;; Create CAD entities for the volume below the terrain
          (let ((p1 (geo:point (nth 0 xyz) (nth 1 xyz) z))
                (p2 (geo:point (nth 3 xyz) (nth 4 xyz) z))
                (p3 (geo:point (nth 6 xyz) (nth 7 xyz) z))
                (p4 (geo:point (nth 9 xyz) (nth 10 xyz) z)))

            (let ((c1 (geo:line p1 p2)) (c2 (geo:line p2 p3))
                  (c3 (geo:line p3 p4)) (c4 (geo:line p4 p1))
                  (c10 (geo:line p1 (nth 0 p))) (c11 (geo:line p2 (nth 1 p)))
                  (c12 (geo:line p3 (nth 2 p))) (c13 (geo:line p4 (nth 3 p))))

              (let* ((ll1 (geo:curve-loop (list c1 c2 c3 c4)))
                     (s1 (geo:plane-surface (list ll1)))
                     (s3 (geo:plane-surface
                          (list (geo:curve-loop (list c1 c11 (- (cdr (nth 0 c))) (- c10))))))
                     (s4 (geo:plane-surface
                          (list (geo:curve-loop (list c2 c12 (- (cdr (nth 1 c))) (- c11))))))
                     (s5 (geo:plane-surface
                          (list (geo:curve-loop (list c3 c13 (- (cdr (nth 2 c))) (- c12))))))
                     (s6 (geo:plane-surface
                          (list (geo:curve-loop (list c4 c10 (- (cdr (nth 3 c))) (- c13)))))))

                (let ((sl1 (geo:surface-loop (list s1 s3 s4 s5 s6 (cdr (first s))))))
                  (geo:volume (list sl1)))))))

        (geo:synchronize)

        (opt:set-number "Mesh.MeshSizeMin" 100)
        (opt:set-number "Mesh.MeshSizeMax" 100)))))
