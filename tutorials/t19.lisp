;;; t19.lisp — Thrusections, fillets, pipes, mesh size from curvature

;; The OpenCASCADE geometry kernel supports several useful features for solid
;; modelling.

(gmsh:add "t19")

;; Volumes can be constructed from (closed) curve loops thanks to the
;; `thru-sections' function
(occ:circle 0 0 0 0.5 :tag 1)
(occ:curve-loop '(1) :tag 1)
(occ:circle 0.1 0.05 1 0.1 :tag 2)
(occ:curve-loop '(2) :tag 2)
(occ:circle -0.1 -0.1 2 0.3 :tag 3)
(occ:curve-loop '(3) :tag 3)
(occ:thru-sections '(1 2 3) :tag 1)
(occ:synchronize)

;; We can also force the creation of ruled surfaces:
(occ:circle 2.0 0 0 0.5 :tag 11)
(occ:curve-loop '(11) :tag 11)
(occ:circle 2.1 0.05 1 0.1 :tag 12)
(occ:curve-loop '(12) :tag 12)
(occ:circle 1.9 -0.1 2 0.3 :tag 13)
(occ:curve-loop '(13) :tag 13)
(occ:thru-sections '(11 12 13) :tag 11 :make-solid t :make-ruled t)
(occ:synchronize)

;; We copy the first volume, and fillet all its edges:
(let ((out (occ:copy '((3 . 1)))))
  (occ:translate out 4 0 0)
  (occ:synchronize)
  (let* ((e (gmsh:get-boundary (gmsh:get-boundary out) :combined nil)))
    (occ:fillet (list (cdr (first out)))
                (loop for i in e collect (abs (cdr i)))
                '(0.1))))
(occ:synchronize)

;; OpenCASCADE also allows general extrusions along a smooth path. Let's first
;; define a spline curve:
(let* ((nturns 1.0)
       (npts 20)
       (r 1.0)
       (h (* 1.0 nturns))
       (p (loop for i from 0 below npts
                for theta = (* i 2 pi nturns (/ 1.0 npts))
                do (occ:point (* r (cos theta)) (* r (sin theta))
                              (* i (/ h npts))
                              :mesh-size 1.0 :tag (+ 1000 i))
                collect (+ 1000 i))))
  (occ:spline p :tag 1000)

  ;; A wire is like a curve loop, but open:
  (occ:wire '(1000) :tag 1000)

  ;; We define the shape we would like to extrude along the spline (a disk):
  (occ:disk 1 0 0 0.2 0.2 :tag 1000)
  (occ:rotate '((2 . 1000)) 0 0 0 1 0 0 (/ pi 2))

  ;; We extrude the disk along the spline to create a pipe (other sweeping types
  ;; can be specified; try e.g. "Frenet" instead of "DiscreteTrihedron"):
  (occ:pipe '((2 . 1000)) 1000 :trihedron "DiscreteTrihedron")

  ;; We delete the source surface, and increase the number of sub-edges for a
  ;; nicer display of the geometry:
  (occ:remove '((2 . 1000)))
  (opt:set-number "Geometry.NumSubEdges" 1000))

(occ:synchronize)

;; We can activate the calculation of mesh element sizes based on curvature
;; (here with a target of 20 elements per 2*Pi radians):
(opt:set-number "Mesh.MeshSizeFromCurvature" 20)

;; We can constraint the min and max element sizes to stay within reasonable
;; values (see t10.lisp for more details):
(opt:set-number "Mesh.MeshSizeMin" 0.001)
(opt:set-number "Mesh.MeshSizeMax" 0.3)

(mesh:generate :dim 3)
;; (gmsh:write "/tmp/t19.msh")
