;;; full_quad.lisp — Full quad meshing with Recombine
;;;
;;; Creates overlapping rectangles, Boolean-subtracts the smaller from the
;;; larger, then generates a full-quad mesh using field-based mesh sizing
;;; and recombination.
;;; Port of boolean/full_quad.geo

(gmsh:add "full_quad")

;; Create geometry with some mesh sizes on boundaries
(occ:rectangle 0 0 0 10 10 :tag 1)
(occ:rectangle 2 4 0 2 1 :tag 2)
(occ:rectangle 5 5 0 1 2 :tag 3)
(occ:rectangle 2 6 0 1 1 :tag 4)
(occ:rectangle 7 2 0 2 2 :tag 5)

;; Rotate some of the interior rectangles
(occ:rotate '((2 . 3)) 5 5 0 0 0 1 (/ pi 4))
(occ:rotate '((2 . 4)) 2 6 0 0 0 1 (/ pi 3))

(occ:synchronize)

;; Set mesh sizes on boundary points
(mesh:set-size (gmsh:get-boundary '((2 . 1)) :combined nil :oriented nil :recursive t) 0.5)
(loop for s in '(2 3 4 5)
      do (mesh:set-size (gmsh:get-boundary (list (cons 2 s))
                                           :combined nil :oriented nil :recursive t)
                        0.05))

;; Boolean difference: cut holes from the big rectangle
(occ:cut '((2 . 1)) '((2 . 2) (2 . 3) (2 . 4) (2 . 5)))

(occ:synchronize)

;; Replace default mesh size interpolation with "Extend" field
(opt:set-number "Mesh.MeshSizeExtendFromBoundary" 0)

(let ((all-curves (gmsh:tags-of (gmsh:get-entities :dim 1))))
  (mesh:field "Extend" :tag 1
    :number-lists `(("CurvesList" ,all-curves))
    :numbers '(("DistMax" 2) ("SizeMax" 0.5) ("Power" 1)))
  (mesh:field-set-as-background-mesh 1))

;; Recombine all surfaces into quads
(dolist (s (gmsh:tags-of (gmsh:get-entities :dim 2)))
  (mesh:set-recombine 2 s))

;; Use frontal-Delaunay for quads with simple full-quad subdivision
(opt:set-number "Mesh.Algorithm" 8)
(opt:set-number "Mesh.RecombinationAlgorithm" 3)

(mesh:generate :dim 2)
;; (gmsh:write "/tmp/full_quad.msh")
