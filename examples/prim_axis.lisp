;;; prim_axis.lisp — OCC primitives with custom axis orientations

(occ:circle 0 0 0 0.2)
(occ:circle 1 0 0 0.2 :z-axis '(0 0 1))
(occ:circle 2 0 0 0.2 :z-axis '(0 0 -1) :x-axis '(1 0 0))
(occ:circle 3 0 0 0.2 :z-axis '(1 1 0) :x-axis '(0 1 0))

(occ:ellipse 0 -1 0 0.2 0.1)
(occ:ellipse 1 -1 0 0.2 0.1 :z-axis '(1 1 0) :x-axis '(0 1 0))
(occ:ellipse 2 -1 0 0.2 0.1 :z-axis '(1 0 0) :x-axis '(0 1 0))

(occ:disk 0 -2 0 0.2 0.1)
(occ:disk 1 -2 0 0.2 0.1 :z-axis '(1 1 0) :x-axis '(0 1 0))

(occ:torus 0 -3 0 0.3 0.1)
(occ:torus 1 -3 0 0.3 0.1 :z-axis '(1 1 0))

(occ:wedge 0 -4 0 0.4 0.2 0.4)
(occ:wedge 1 -4 0 0.4 0.2 0.4 :z-axis '(0.2 0 1))

(occ:synchronize)

(opt:set-number "Mesh.MeshSizeFromCurvature" 10)
