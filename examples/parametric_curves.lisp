;;; parametric_curves.lisp — Parametric curve sampling and reparametrization
;;;
;;; Demonstrates sampling points on parametric curves and using
;;; reparametrization to map between parameter and physical space.
;;; Inspired by Julia tutorial x5.jl

(gmsh:add "parametric_curves")

;; Create a circle arc using OCC
(occ:circle 0 0 0 1 :tag 1)
(occ:synchronize)

;; Query the parametrization bounds of the circle
(multiple-value-bind (param-min param-max)
    (gmsh:get-parametrization-bounds 1 1)
  (format t "Circle parametrization bounds: ~A to ~A~%" param-min param-max)

  ;; Sample points along the curve using the parametrization
  (let ((n-samples 20))
    (loop for i from 0 below n-samples
          for t-param = (+ (first param-min)
                           (* (/ i (1- n-samples))
                              (- (first param-max) (first param-min))))
          do (let ((value (gmsh:get-value 1 1 (list t-param))))
               (format t "  t=~6,3F -> (~6,3F, ~6,3F, ~6,3F)~%"
                       t-param
                       (first value) (second value) (third value)))))

  ;; Query curvature at several points
  (loop for i from 0 to 4
        for t-param = (+ (first param-min)
                         (* (/ i 4)
                            (- (first param-max) (first param-min))))
        do (let ((curv (gmsh:get-curvature 1 1 (list t-param))))
             (format t "  curvature at t=~6,3F: ~6,3F~%" t-param (first curv)))))

;; Create an ellipse for comparison
(occ:ellipse 3 0 0 2 1 :tag 2)
(occ:synchronize)

;; Sample the ellipse
(multiple-value-bind (param-min param-max)
    (gmsh:get-parametrization-bounds 1 2)
  (format t "~%Ellipse parametrization bounds: ~A to ~A~%" param-min param-max)
  (let ((n-samples 10))
    (loop for i from 0 below n-samples
          for t-param = (+ (first param-min)
                           (* (/ i (1- n-samples))
                              (- (first param-max) (first param-min))))
          do (let ((value (gmsh:get-value 1 2 (list t-param))))
               (format t "  t=~6,3F -> (~6,3F, ~6,3F, ~6,3F)~%"
                       t-param
                       (first value) (second value) (third value))))))

;; Mesh the curves
(opt:set-number "Mesh.MeshSizeMin" 0.1)
(opt:set-number "Mesh.MeshSizeMax" 0.1)
(mesh:generate :dim 1)
;; (gmsh:write "/tmp/parametric_curves.msh")
