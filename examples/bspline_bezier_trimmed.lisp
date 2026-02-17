;;; bspline_bezier_trimmed.lisp — BSpline surface with trimming wires

(occ:point 0.1 0 0.75)    ; 1
(occ:point 1 0 0.65)       ; 2
(occ:point 2 0 0.5)        ; 3
(occ:point 3 0 0.2)        ; 4
(occ:point 4 0 0)           ; 5
(occ:point 0.1 1 0.1)      ; 6
(occ:point 1 1 0)           ; 7
(occ:point 2 1 0)           ; 8
(occ:point 3 1 0)           ; 9
(occ:point 4 1 0)           ; 10
(occ:point 0 2 0.2)        ; 11
(occ:point 1 2 0)           ; 12
(occ:point 2 2 0.1)        ; 13
(occ:point 3 2 0)           ; 14
(occ:point 4 2 0)           ; 15
(occ:point 0 3 0.1)        ; 16
(occ:point 1 3 0)           ; 17
(occ:point 2 3 0)           ; 18
(occ:point 3 3 0)           ; 19
(occ:point 4 3 0)           ; 20

(let* ((c (occ:circle 0.5 0.5 0 0.4))
       (w (occ:wire (list c)))
       (c2 (occ:circle 0.5 0.5 0 0.2))
       (w2 (occ:wire (list c2))))

  ;; With wire3d nil, use x,y coordinates of the curves as parametric coordinates
  (occ:b-spline-surface (loop for i from 1 to 20 collect i) 5
                        :wire-tags (list w w2) :wire3-d nil))

(occ:synchronize)
