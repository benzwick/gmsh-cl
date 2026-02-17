;;; edges.lisp — Extract edge nodes from 2D elements
;;;
;;; Demonstrates mesh:get-element-edge-nodes to extract 1D edge elements
;;; from a 2D mesh, creating discrete entities for the extracted edges.
;;; Also shows get-integration-points and get-basis-functions.
;;; Port of api/edges.cpp

(gmsh:add "edges")

;; Create two rectangles and fragment them for a conformal mesh
(occ:rectangle 0 0 0 1 1 :tag 1)
(occ:rectangle 0.5 0 0 1 1 :tag 2)
(occ:fragment '((2 . 1)) '((2 . 2)))

(occ:synchronize)
(mesh:generate :dim 2)

;; Explore the mesh: what type of 2D elements do we have?
(let* ((ele-types (mesh:get-element-types :dim 2)))
  (when (> (length ele-types) 1)
    (error "Hybrid meshes not handled in this example!"))

  (let ((ele-type-2d (first ele-types)))
    (multiple-value-bind (name dim order num-nodes param-coord primary-nodes)
        (mesh:get-element-properties ele-type-2d)
      (declare (ignore dim param-coord primary-nodes))
      (format t "2D elements are of type '~A' (type = ~A)~%" name ele-type-2d)

      ;; Iterate over all surfaces and extract edge nodes
      (let ((entities (gmsh:get-entities :dim 2)))
        (dolist (ent entities)
          (let ((s (cdr ent)))
            (multiple-value-bind (elem-tags node-tags)
                (mesh:get-elements-by-type ele-type-2d :tag s)
              (format t "- ~A elements on surface ~A~%" (length elem-tags) s)

              ;; Get the nodes on the edges of the 2D elements
              (let* ((nodes (mesh:get-element-edge-nodes ele-type-2d :tag s))
                     ;; Create a new discrete entity of dimension 1
                     (c (gmsh:add-discrete-entity 1))
                     ;; Get the corresponding 1D element type
                     (ele-type-1d (mesh:get-element-type "line" order)))
                (mesh:add-elements-by-type c ele-type-1d '() nodes))))))

      ;; Get integration points and basis functions for 1D elements
      (let* ((ele-types-1d (mesh:get-element-types :dim 1))
             (ele-type-1d (first ele-types-1d)))
        (multiple-value-bind (uvw q)
            (mesh:get-integration-points ele-type-1d "Gauss3")
          (multiple-value-bind (num-comp bf num-orient)
              (mesh:get-basis-functions ele-type-1d uvw "Lagrange")
            (declare (ignore bf num-orient))
            (format t "1D integration: ~A points, ~A components~%"
                    (/ (length uvw) 3) num-comp))

          ;; Get Jacobians for each curve entity
          (dolist (ent (gmsh:get-entities :dim 1))
            (let ((c (cdr ent)))
              (multiple-value-bind (elem-tags node-tags)
                  (mesh:get-elements-by-type ele-type-1d :tag c)
                (declare (ignore node-tags))
                (format t "- ~A elements on curve ~A~%" (length elem-tags) c)
                (multiple-value-bind (jac det pts)
                    (mesh:get-jacobians ele-type-1d uvw :tag c)
                  (declare (ignore jac det pts)))))))))))

;; (gmsh:write "/tmp/edges.msh")
