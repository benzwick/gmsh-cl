;;; step_boundary_colors.lisp — Read boundary colors from a STEP file
;;;
;;; This example requires the step_boundary_colors.stp file from the gmsh examples

(let ((path (namestring (merge-pathnames "_reference/gmsh/examples/api/step_boundary_colors.stp"
                                         (asdf:system-source-directory :gmsh-cl)))))
  (when (probe-file path)
    (gmsh:merge path)

    (dolist (e (gmsh:get-entities))
      (multiple-value-bind (r g b a) (gmsh:get-color (car e) (cdr e))
        (unless (and (= r 0) (= g 0) (= b 255) (= a 0))
          (format t "entity ~A color (~A ~A ~A ~A)~%" e r g b a))))))
