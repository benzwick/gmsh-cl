;;; t5.lisp — Mesh sizes, holes in volumes

(gmsh:add "t5")

(defparameter *lcar1* 0.1)
(defparameter *lcar2* 0.0005)
(defparameter *lcar3* 0.055)

;; If we wanted to change these mesh sizes globally (without changing the above
;; definitions), we could give a global scaling factor for all mesh sizes with
;; e.g.
;;
;;   (opt:set-number "Mesh.MeshSizeFactor" 0.1)
;;
;; See t10.lisp for more information about mesh sizes.

;; We proceed by defining some elementary entities describing a truncated cube:

(geo:point 0.5 0.5 0.5 :mesh-size *lcar2* :tag 1)
(geo:point 0.5 0.5 0   :mesh-size *lcar1* :tag 2)
(geo:point 0   0.5 0.5 :mesh-size *lcar1* :tag 3)
(geo:point 0   0   0.5 :mesh-size *lcar1* :tag 4)
(geo:point 0.5 0   0.5 :mesh-size *lcar1* :tag 5)
(geo:point 0.5 0   0   :mesh-size *lcar1* :tag 6)
(geo:point 0   0.5 0   :mesh-size *lcar1* :tag 7)
(geo:point 0   1   0   :mesh-size *lcar1* :tag 8)
(geo:point 1   1   0   :mesh-size *lcar1* :tag 9)
(geo:point 0   0   1   :mesh-size *lcar1* :tag 10)
(geo:point 0   1   1   :mesh-size *lcar1* :tag 11)
(geo:point 1   1   1   :mesh-size *lcar1* :tag 12)
(geo:point 1   0   1   :mesh-size *lcar1* :tag 13)
(geo:point 1   0   0   :mesh-size *lcar1* :tag 14)

(geo:line 8  9  :tag 1)
(geo:line 9  12 :tag 2)
(geo:line 12 11 :tag 3)
(geo:line 11 8  :tag 4)
(geo:line 9  14 :tag 5)
(geo:line 14 13 :tag 6)
(geo:line 13 12 :tag 7)
(geo:line 11 10 :tag 8)
(geo:line 10 13 :tag 9)
(geo:line 10 4  :tag 10)
(geo:line 4  5  :tag 11)
(geo:line 5  6  :tag 12)
(geo:line 6  2  :tag 13)
(geo:line 2  1  :tag 14)
(geo:line 1  3  :tag 15)
(geo:line 3  7  :tag 16)
(geo:line 7  2  :tag 17)
(geo:line 3  4  :tag 18)
(geo:line 5  1  :tag 19)
(geo:line 7  8  :tag 20)
(geo:line 6  14 :tag 21)

(geo:curve-loop '(-11 -19 -15 -18) :tag 22)
(geo:plane-surface '(22) :tag 23)
(geo:curve-loop '(16 17 14 15) :tag 24)
(geo:plane-surface '(24) :tag 25)
(geo:curve-loop '(-17 20 1 5 -21 13) :tag 26)
(geo:plane-surface '(26) :tag 27)
(geo:curve-loop '(-4 -1 -2 -3) :tag 28)
(geo:plane-surface '(28) :tag 29)
(geo:curve-loop '(-7 2 -5 -6) :tag 30)
(geo:plane-surface '(30) :tag 31)
(geo:curve-loop '(6 -9 10 11 12 21) :tag 32)
(geo:plane-surface '(32) :tag 33)
(geo:curve-loop '(7 3 8 9) :tag 34)
(geo:plane-surface '(34) :tag 35)
(geo:curve-loop '(-10 18 -16 -20 4 -8) :tag 36)
(geo:plane-surface '(36) :tag 37)
(geo:curve-loop '(-14 -13 -12 19) :tag 38)
(geo:plane-surface '(38) :tag 39)

(defparameter *shells* '())

(let ((sl (geo:surface-loop '(35 31 29 37 33 23 39 25 27))))
  (push sl *shells*))

;; This function creates a spherical hole in a volume. We don't specify tags
;; manually, and let the functions return them automatically:

(defun cheese-hole (x y z r lc)
  (let* ((p1 (geo:point x       y       z       :mesh-size lc))
         (p2 (geo:point (+ x r) y       z       :mesh-size lc))
         (p3 (geo:point x       (+ y r) z       :mesh-size lc))
         (p4 (geo:point x       y       (+ z r) :mesh-size lc))
         (p5 (geo:point (- x r) y       z       :mesh-size lc))
         (p6 (geo:point x       (- y r) z       :mesh-size lc))
         (p7 (geo:point x       y       (- z r) :mesh-size lc))

         (c1  (geo:circle-arc p2 p1 p7))
         (c2  (geo:circle-arc p7 p1 p5))
         (c3  (geo:circle-arc p5 p1 p4))
         (c4  (geo:circle-arc p4 p1 p2))
         (c5  (geo:circle-arc p2 p1 p3))
         (c6  (geo:circle-arc p3 p1 p5))
         (c7  (geo:circle-arc p5 p1 p6))
         (c8  (geo:circle-arc p6 p1 p2))
         (c9  (geo:circle-arc p7 p1 p3))
         (c10 (geo:circle-arc p3 p1 p4))
         (c11 (geo:circle-arc p4 p1 p6))
         (c12 (geo:circle-arc p6 p1 p7))

         (l1 (geo:curve-loop (list c5 c10 c4)))
         (l2 (geo:curve-loop (list c9 (- c5) c1)))
         (l3 (geo:curve-loop (list c12 (- c8) (- c1))))
         (l4 (geo:curve-loop (list c8 (- c4) c11)))
         (l5 (geo:curve-loop (list (- c10) c6 c3)))
         (l6 (geo:curve-loop (list (- c11) (- c3) c7)))
         (l7 (geo:curve-loop (list (- c2) (- c7) (- c12))))
         (l8 (geo:curve-loop (list (- c6) (- c9) c2)))

         ;; We need non-plane surfaces to define the spherical holes. Here we
         ;; use the surface-filling function, which can be used for surfaces
         ;; with 3 or 4 curves on their boundary. If the curves are circle arcs
         ;; with the same center, a spherical patch is created.
         (s1 (geo:surface-filling (list l1)))
         (s2 (geo:surface-filling (list l2)))
         (s3 (geo:surface-filling (list l3)))
         (s4 (geo:surface-filling (list l4)))
         (s5 (geo:surface-filling (list l5)))
         (s6 (geo:surface-filling (list l6)))
         (s7 (geo:surface-filling (list l7)))
         (s8 (geo:surface-filling (list l8)))

         (sl (geo:surface-loop (list s1 s2 s3 s4 s5 s6 s7 s8)))
         (v  (geo:volume (list sl))))
    (push sl *shells*)
    v))

;; We create five holes in the cube:
(let ((x 0)
      (y 0.75)
      (z 0)
      (r 0.09))
  (loop for tt from 1 to 5
        do (incf x 0.166)
           (incf z 0.166)
           (let ((v (cheese-hole x y z r *lcar3*)))
             (geo:physical-group 3 (list v) :tag tt))))

;; The volume of the cube, without the 5 holes, is defined by 6 surface loops:
;; the first surface loop defines the exterior surface; the surface loops other
;; than the first one define holes:
(geo:volume (reverse *shells*) :tag 186)

(geo:synchronize)

;; Note that using solid modelling with the OpenCASCADE CAD kernel, the same
;; geometry could be built quite differently: see t16.lisp.

;; We finally define a physical volume for the elements discretizing the cube,
;; without the holes (for which physical groups were already defined in the
;; cheese-hole function):
(gmsh:add-physical-group 3 '(186) :tag 10)

;; We could make only part of the model visible to only mesh this subset:
;; (let ((ent (gmsh:get-entities)))
;;   (gmsh:set-visibility ent nil)
;;   (gmsh:set-visibility '((3 . 5)) t :recursive t)
;;   (opt:set-number "Mesh.MeshOnlyVisible" 1))

;; Meshing algorithms can be changed globally using options:
(opt:set-number "Mesh.Algorithm" 6)  ; Frontal-Delaunay for 2D meshes

;; They can also be set for individual surfaces, e.g. for using MeshAdapt on
;; surface 33:
(mesh:set-algorithm 2 33 1)

;; To generate a curvilinear mesh and optimize it to produce provably valid
;; curved elements, you can uncomment the following lines:
;;
;; (opt:set-number "Mesh.ElementOrder" 2)
;; (opt:set-number "Mesh.HighOrderOptimize" 2)

(mesh:generate :dim 3)
;; (gmsh:write "/tmp/t5.msh")
