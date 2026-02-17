;;; surface_filling.lisp — Various surface filling methods

(opt:set-number "Mesh.MeshSizeMin" 0.02)
(opt:set-number "Mesh.MeshSizeMax" 0.02)

(occ:point -0.8 0.1 -0.2 :tag 1)
(occ:point -0.5 0.1 -0.2 :tag 2)
(occ:point -0.6 0 -0.1 :tag 3)
(occ:point -0.7 0 -0.1 :tag 4)
(occ:point -0.7 0.2 -0.2 :tag 5)
(occ:point -0.6 0.1 -0.1 :tag 6)

(occ:spline '(1 5 2) :tag 1)
(occ:spline '(2 6 3) :tag 2)
(occ:spline '(3 4 1) :tag 3)
(occ:curve-loop '(1 2 3) :tag 1)

;; BSpline surface bounded by curve loop 1, constructed by optimization
(occ:surface-filling 1 :tag 1)

;; BSpline filling (try "Stretch" or "Curved")
(occ:b-spline-filling 1 :tag 2 :type "Curved")

;; Same as 1, but passing through points 7 and 8
(occ:point -0.7 0.1 -0.2 :tag 7)
(occ:point -0.67 0.1 -0.2 :tag 8)
(occ:surface-filling 1 :tag 4 :point-tags '(7 8))

(occ:synchronize)
