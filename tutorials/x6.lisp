;;; x6.lisp — Additional mesh data: integration points, Jacobians and basis functions
;;;
;;; Demonstrates FEM building blocks: Gauss integration points, Lagrange
;;; and GradLagrange basis functions, and Jacobian evaluation.
;;;
;;; Key API: mesh:get-integration-points, mesh:get-basis-functions,
;;; mesh:get-jacobians, mesh:get-element-properties
;;;
;;; Equivalent Python: gmsh/tutorials/python/x6.py

;; The API provides access to all the elementary building blocks required to
;; implement finite-element-type numerical methods.

(gmsh:add "x6")

;; Create a simple 2D model and mesh it:
(occ:rectangle 0 0 0 1 0.1)
(occ:synchronize)
(mesh:set-transfinite-automatic)
(mesh:generate :dim 2)

;; Set the element order and the desired interpolation order:
(let ((element-order 1)
      (interpolation-order 2))

  (mesh:set-order element-order)

  ;; Iterate over all the element types present in the mesh:
  (dolist (et (mesh:get-element-types))

    ;; Retrieve properties for the given element type
    (multiple-value-bind (element-name dim order num-nodes local-node-coord num-prim-nodes)
        (mesh:get-element-properties et)
      (declare (ignore dim order num-prim-nodes))
      (format t "~%** ~A **~%~%" element-name)

      ;; Retrieve integration points for that element type
      (multiple-value-bind (local-coords weights)
          (mesh:get-integration-points
           et (format nil "Gauss~A" interpolation-order))
        (format t " * ~A integration points~%" (/ (length local-coords) 3))

        ;; Return the basis functions evaluated at the integration points
        (multiple-value-bind (num-components basis-functions num-orientations)
            (mesh:get-basis-functions et local-coords "Lagrange")
          (declare (ignore num-orientations))
          (format t " * ~A basis functions at integration points~%"
                  (length basis-functions)))

        (multiple-value-bind (num-components basis-functions num-orientations)
            (mesh:get-basis-functions et local-coords "GradLagrange")
          (declare (ignore num-orientations))
          (format t " * ~A basis function gradients at integration points~%"
                  (/ (length basis-functions) 3)))

        ;; Compute the Jacobians at the integration points
        (multiple-value-bind (jacobians determinants coords)
            (mesh:get-jacobians et local-coords)
          (declare (ignore jacobians coords))
          (format t " * ~A Jacobian determinants at integration points~%"
                  (length determinants)))))))
