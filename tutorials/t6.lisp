;;; t6.lisp — Transfinite meshes, deleting entities

(gmsh:add "t6")

;; Copied from t1.lisp...
(let ((lc 1e-2))
  (geo:point 0 0 0 :mesh-size lc :tag 1)
  (geo:point 0.1 0 0 :mesh-size lc :tag 2)
  (geo:point 0.1 0.3 0 :mesh-size lc :tag 3)
  (geo:point 0 0.3 0 :mesh-size lc :tag 4)
  (geo:line 1 2 :tag 1)
  (geo:line 3 2 :tag 2)
  (geo:line 3 4 :tag 3)
  (geo:line 4 1 :tag 4)
  (geo:curve-loop '(4 1 -2 3) :tag 1)
  (geo:plane-surface '(1) :tag 1)

  ;; Delete the surface and the left line, and replace the line with 3 new ones:
  (geo:remove '((2 . 1) (1 . 4)))

  (let* ((p1 (geo:point -0.05 0.05 0 :mesh-size lc))
         (p2 (geo:point -0.05 0.1 0 :mesh-size lc))
         (l1 (geo:line 1 p1))
         (l2 (geo:line p1 p2))
         (l3 (geo:line p2 4)))

    ;; Create surface
    (geo:curve-loop (list 2 -1 l1 l2 l3 -3) :tag 2)
    (geo:plane-surface '(-2) :tag 1)

    ;; The setTransfiniteCurve() meshing constraint explicitly specifies the
    ;; location of the nodes on the curve. For example, the following command
    ;; forces 20 uniformly placed nodes on curve 2 (including the nodes on
    ;; the two end points):
    (geo:mesh-set-transfinite-curve 2 20)

    ;; Let's put 20 points total on combination of curves l1, l2 and l3
    ;; (beware that the points p1 and p2 are shared by the curves, so we do
    ;; not create 6 + 6 + 10 = 22 nodes, but 20!)
    (geo:mesh-set-transfinite-curve l1 6)
    (geo:mesh-set-transfinite-curve l2 6)
    (geo:mesh-set-transfinite-curve l3 10)

    ;; Finally, we put 30 nodes following a geometric progression on curve 1
    ;; (reversed) and on curve 3:
    (geo:mesh-set-transfinite-curve 1 30 :mesh-type "Progression" :coef -1.2)
    (geo:mesh-set-transfinite-curve 3 30 :mesh-type "Progression" :coef 1.2)

    ;; The setTransfiniteSurface() meshing constraint uses a transfinite
    ;; interpolation algorithm in the parametric plane of the surface to
    ;; connect the nodes on the boundary using a structured grid. If the
    ;; surface has more than 4 corner points, the corners of the transfinite
    ;; interpolation have to be specified by hand:
    (geo:mesh-set-transfinite-surface 1 :arrangement "Left" :corner-tags '(1 2 3 4))

    ;; To create quadrangles instead of triangles, one can use the
    ;; setRecombine constraint:
    (geo:mesh-set-recombine 2 1)))

;; When the surface has only 3 or 4 points on its boundary the list of
;; corners can be omitted in the setTransfiniteSurface() call:
(geo:point 0.2 0.2 0 :mesh-size 1.0 :tag 7)
(geo:point 0.2 0.1 0 :mesh-size 1.0 :tag 8)
(geo:point 0.25 0.2 0 :mesh-size 1.0 :tag 9)
(geo:point 0.3 0.1 0 :mesh-size 1.0 :tag 10)
(geo:line 8 10 :tag 10)
(geo:line 10 9 :tag 11)
(geo:line 9 7 :tag 12)
(geo:line 7 8 :tag 13)
(geo:curve-loop '(13 10 11 12) :tag 14)
(geo:plane-surface '(14) :tag 15)
(loop for i from 10 to 13
      do (geo:mesh-set-transfinite-curve i 10))
(geo:mesh-set-transfinite-surface 15)

;; The way triangles are generated can be controlled by specifying "Left",
;; "Right" or "Alternate" in setTransfiniteSurface(). Try e.g.
;;
;; (geo:mesh-set-transfinite-surface 15 :arrangement "Alternate")

(geo:synchronize)

;; Finally we apply an elliptic smoother to the grid to have a more regular
;; mesh:
(opt:set-number "Mesh.Smoothing" 100)

(mesh:generate :dim 2)
;; (gmsh:write "/tmp/t6.msh")
