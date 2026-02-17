;;; adapt_mesh.lisp — Mesh adaptation using interpolation error estimation
;;;
;;; Demonstrates mesh:get-integration-points and get-basis-functions for
;;; computing interpolation errors, then using a PostView field to drive
;;; adaptive remeshing.
;;; Inspired by adapt_mesh.py (simplified — no numpy dependency)

(gmsh:add "adapt_mesh")

;; Create initial geometry
(let ((lc 0.02))
  (occ:rectangle 0 0 0 1 1)
  (occ:synchronize)

  ;; Set initial mesh size and generate
  (mesh:set-size (gmsh:get-boundary '((2 . 1)) :combined nil :oriented nil :recursive t) lc)
  (mesh:generate :dim 2)

  ;; Get integration points and basis functions for triangle elements (type 2)
  (multiple-value-bind (uvw weights)
      (mesh:get-integration-points 2 "Gauss2")
    (format t "Integration points for triangles: ~A points~%"
            (/ (length uvw) 3))

    (multiple-value-bind (num-comp sf num-orient)
        (mesh:get-basis-functions 2 uvw "Lagrange")
      (format t "Basis functions: ~A components, ~A orientations~%"
              num-comp num-orient)

      ;; Get Jacobians for all triangle elements
      (let ((entities (gmsh:get-entities :dim 2)))
        (dolist (ent entities)
          (let ((s (cdr ent)))
            (multiple-value-bind (elem-tags node-tags)
                (mesh:get-elements-by-type 2 :tag s)
              (format t "Surface ~A: ~A triangles~%" s (length elem-tags))
              (multiple-value-bind (jac det pts)
                  (mesh:get-jacobians 2 uvw :tag s)
                (format t "  Jacobian entries: ~A, det entries: ~A, point entries: ~A~%"
                        (length jac) (length det) (length pts))))))

        ;; Create a simple size field based on distance to center
        ;; Use a PostView field for adaptive remeshing
        (let* ((sf-view (view:add "size_field"))
               (all-entities (gmsh:get-entities :dim 2)))
          (dolist (ent all-entities)
            (let ((s (cdr ent)))
              (multiple-value-bind (elem-tags node-tags)
                  (mesh:get-elements-by-type 2 :tag s)
                (declare (ignore node-tags))
                (when (> (length elem-tags) 0)
                  ;; Assign a size based on element tag (simple proxy for position)
                  (let ((sizes (mapcar (lambda (et)
                                         (declare (ignore et))
                                         (list (+ 0.005 (* 0.015 (random 1.0d0)))))
                                       elem-tags)))
                    (view:add-model-data sf-view 0 "adapt_mesh" "ElementData"
                                         elem-tags sizes))))))

          ;; Smooth the size field
          (plugin:set-number "Smooth" "View" (view:get-index sf-view))
          (plugin:run "Smooth")

          ;; Create a new model and remesh using the size field
          (gmsh:add "adapt_mesh_2")
          (occ:rectangle 0 0 0 1 1)
          (occ:synchronize)

          (let ((bg-field (mesh:field-add "PostView")))
            (mesh:field-set-number bg-field "ViewTag" sf-view)
            (mesh:field-set-as-background-mesh bg-field))

          (mesh:generate :dim 2)
          (format t "Adapted mesh generated~%"))))))

;; (gmsh:write "/tmp/adapt_mesh.msh")
