;;; faces.lisp — Extract face nodes from 3D elements
;;;
;;; Demonstrates mesh:get-element-face-nodes to extract 2D face elements
;;; from a 3D mesh, creating discrete entities for the extracted faces.
;;; Also shows get-integration-points and get-basis-functions on faces.
;;; Port of api/faces.cpp

(gmsh:add "faces")

;; Create three solids and fragment them for a conformal mesh
(let ((v1 (occ:box 0 0 0 1 1 1))
      (v2 (occ:box 1 0 0 1 1 1))
      (v3 (occ:sphere 1.5 0.5 0.5 0.25)))
  (occ:fragment (list (cons 3 v1) (cons 3 v2) (cons 3 v3)) '()))

(occ:synchronize)
(mesh:generate :dim 3)

;; Explore the mesh: what type of 3D elements do we have?
(let* ((ele-types (mesh:get-element-types :dim 3)))
  (when (> (length ele-types) 1)
    (error "Hybrid meshes not handled in this example!"))

  (let ((ele-type-3d (first ele-types)))
    (multiple-value-bind (name dim order num-nodes param-coord primary-nodes)
        (mesh:get-element-properties ele-type-3d)
      (declare (ignore dim param-coord primary-nodes))
      (format t "3D elements are of type '~A' (type = ~A)~%" name ele-type-3d)

      ;; Iterate over all volumes, get 3D elements and create new 2D face elements
      (let ((entities (gmsh:get-entities :dim 3)))
        (dolist (ent entities)
          (let ((v (cdr ent)))
            (multiple-value-bind (elem-tags node-tags)
                (mesh:get-elements-by-type ele-type-3d :tag v)
              (declare (ignore node-tags))
              (format t "- ~A elements in volume ~A~%" (length elem-tags) v)

              ;; Get the nodes on the triangular faces of the 3D elements
              (let* ((nodes (mesh:get-element-face-nodes ele-type-3d 3 :tag v))
                     ;; Create a new discrete entity of dimension 2
                     (s (gmsh:add-discrete-entity 2))
                     ;; Get the corresponding 2D element type
                     (ele-type-2d (mesh:get-element-type "triangle" order)))
                (mesh:add-elements-by-type s ele-type-2d '() nodes))))))

      ;; Get integration points and basis functions for 2D face elements
      (let* ((ele-types-2d (mesh:get-element-types :dim 2))
             (ele-type-2d (first ele-types-2d)))
        (multiple-value-bind (uvw q)
            (mesh:get-integration-points ele-type-2d "Gauss3")
          (multiple-value-bind (num-comp bf num-orient)
              (mesh:get-basis-functions ele-type-2d uvw "Lagrange")
            (declare (ignore bf num-orient))
            (format t "2D integration: ~A points, ~A components~%"
                    (/ (length uvw) 3) num-comp))

          ;; Get Jacobians for each surface entity
          (dolist (ent (gmsh:get-entities :dim 2))
            (let ((s (cdr ent)))
              (multiple-value-bind (elem-tags node-tags)
                  (mesh:get-elements-by-type ele-type-2d :tag s)
                (declare (ignore node-tags))
                (format t "- ~A elements on surface ~A~%" (length elem-tags) s)
                (multiple-value-bind (jac det pts)
                    (mesh:get-jacobians ele-type-2d uvw :tag s)
                  (declare (ignore jac det pts)))))))))))

;; (gmsh:write "/tmp/faces.msh")
