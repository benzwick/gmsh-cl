;;; bspline_bezier_patches.lisp — Two Bezier surface patches glued together

;; Right patch control points
(occ:point 0.1 0 -0.1)    ; 1
(occ:point 1 0 0)          ; 2
(occ:point 2 0 0)          ; 3
(occ:point 3 0 0)          ; 4
(occ:point 4 0 0)          ; 5
(occ:point 0.1 1 0.1)      ; 6
(occ:point 1 1 0)          ; 7
(occ:point 2 1 0)          ; 8
(occ:point 3 1 0)          ; 9
(occ:point 4 1 0)          ; 10
(occ:point 0 2 0.2)        ; 11
(occ:point 1 2 0)          ; 12
(occ:point 2 2 1.5)        ; 13
(occ:point 3 2 0)          ; 14
(occ:point 4 2 0)          ; 15
(occ:point 0 3 0.1)        ; 16
(occ:point 1 3 0)          ; 17
(occ:point 2 3 0)          ; 18
(occ:point 3 3 0)          ; 19
(occ:point 4 3 0)          ; 20

;; Left patch control points
(occ:point 0.1 0 -0.1)     ; 21
(occ:point -1 0 0)          ; 22
(occ:point -2 0 0)          ; 23
(occ:point -3 0 0)          ; 24
(occ:point -4 0 0)          ; 25
(occ:point 0.1 1 0.1)      ; 26
(occ:point -1 1 0)          ; 27
(occ:point -2 1 0)          ; 28
(occ:point -3 1 0)          ; 29
(occ:point -4 1 0)          ; 30
(occ:point 0 2 0.2)        ; 31
(occ:point -1 2 0)          ; 32
(occ:point -2 2 1.5)        ; 33
(occ:point -3 2 0)          ; 34
(occ:point -4 2 0)          ; 35
(occ:point 0 3 0.1)        ; 36
(occ:point -1 3 0)          ; 37
(occ:point -2 3 0)          ; 38
(occ:point -3 3 0)          ; 39
(occ:point -4 3 0)          ; 40

(occ:bezier-surface (loop for i from 1 to 20 collect i) 5)
(occ:bezier-surface (loop for i from 21 to 40 collect i) 5)

;; healShapes performs surface sewing, which glues the two patches;
;; it will also reorient the left patch so that its normal points along +z
(occ:heal-shapes)

(occ:synchronize)
