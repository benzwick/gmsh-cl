;;; spline.lisp — Various spline and B-spline curves

(gmsh:add "spline")

(loop for i from 1 to 10 do
  (occ:point (coerce i 'double-float)
             (sin (* (/ i 9.0d0) 2.0d0 pi))
             0
             :mesh-size 0.1 :tag i))

(occ:spline (loop for i from 1 to 10 collect i) :tag 1)
(occ:b-spline (loop for i from 1 to 10 collect i) :tag 2)
(occ:bezier (loop for i from 1 to 10 collect i) :tag 3)

;; with begin/end tangents
(occ:spline (loop for i from 1 to 10 collect i)
            :tag 4 :tangents '(0 1 0  0 1 0))

;; with tangents at each point
(occ:spline (loop for i from 1 to 10 collect i)
            :tag 5
            :tangents '(1 0 0  1 0 0  1 0 0  1 0 0  1 0 0
                        1 0 0  1 0 0  1 0 0  1 0 0  1 0 0))

(occ:point 0.2 -1.6 0 :mesh-size 0.1 :tag 101)
(occ:point 1.2 -1.6 0 :mesh-size 0.1 :tag 102)
(occ:point 1.2 -1.1 0 :mesh-size 0.1 :tag 103)
(occ:point 0.3 -1.1 0 :mesh-size 0.1 :tag 104)
(occ:point 0.7 -1.0 0 :mesh-size 0.1 :tag 105)

;; periodic bspline through the control points
(occ:spline '(103 102 101 104 105 103) :tag 100)

;; periodic bspline from given control points and default parameters
(occ:b-spline '(103 102 101 104 105 103) :tag 101)

;; general bspline with explicit degree, knots and multiplicities
(occ:point 0 -2 0 :mesh-size 0.1 :tag 201)
(occ:point 1 -2 0 :mesh-size 0.1 :tag 202)
(occ:point 1 -3 0 :mesh-size 0.1 :tag 203)
(occ:point 0 -3 0 :mesh-size 0.1 :tag 204)
(occ:b-spline '(201 202 203 204) :tag 200 :degree 2
              :knots '(0 0.5 1) :multiplicities '(3 1 3))

(occ:synchronize)
