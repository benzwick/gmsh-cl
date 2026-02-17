;;; t12.lisp — Cross-patch meshing with compounds

;; "Compound" meshing constraints allow to generate meshes across surface
;; boundaries, which can be useful e.g. for imported CAD models (e.g. STEP) with
;; undesired small features.

;; When a setCompound() meshing constraint is given, at mesh generation time
;; Gmsh
;;  1. meshes the underlying elementary geometrical entities, individually
;;  2. creates a discrete entity that combines all the individual meshes
;;  3. computes a discrete parametrization (i.e. a piece-wise linear mapping)
;;     on this discrete entity
;;  4. meshes the discrete entity using this discrete parametrization instead
;;     of the underlying geometrical description of the underlying elementary
;;     entities making up the compound
;;  5. optionally, reclassifies the mesh elements and nodes on the original
;;     entities

;; Step 3. above can only be performed if the mesh resulting from the
;; combination of the individual meshes can be reparametrized, i.e. if the shape
;; is "simple enough". If the shape is not amenable to reparametrization, you
;; should create a full mesh of the geometry and first re-classify it to
;; generate patches amenable to reparametrization (see t13.lisp).

;; The mesh of the individual entities performed in Step 1. should usually be
;; finer than the desired final mesh; this can be controlled with the
;; Mesh.CompoundMeshSizeFactor option.

;; The optional reclassification on the underlying elementary entities in Step
;; 5. is governed by the Mesh.CompoundClassify option.

(let ((lc 0.1))
  (geo:point 0 0 0 :mesh-size lc :tag 1)
  (geo:point 1 0 0 :mesh-size lc :tag 2)
  (geo:point 1 1 0.5 :mesh-size lc :tag 3)
  (geo:point 0 1 0.4 :mesh-size lc :tag 4)
  (geo:point 0.3 0.2 0 :mesh-size lc :tag 5)
  (geo:point 0 0.01 0.01 :mesh-size lc :tag 6)
  (geo:point 0 0.02 0.02 :mesh-size lc :tag 7)
  (geo:point 1 0.05 0.02 :mesh-size lc :tag 8)
  (geo:point 1 0.32 0.02 :mesh-size lc :tag 9))

(geo:line 1 2 :tag 1)
(geo:line 2 8 :tag 2)
(geo:line 8 9 :tag 3)
(geo:line 9 3 :tag 4)
(geo:line 3 4 :tag 5)
(geo:line 4 7 :tag 6)
(geo:line 7 6 :tag 7)
(geo:line 6 1 :tag 8)
(geo:spline '(7 5 9) :tag 9)
(geo:line 6 8 :tag 10)

(geo:curve-loop '(5 6 9 4) :tag 11)
(geo:surface-filling '(11) :tag 1)

(geo:curve-loop '(-9 3 10 7) :tag 13)
(geo:surface-filling '(13) :tag 5)

(geo:curve-loop '(-10 2 1 8) :tag 15)
(geo:surface-filling '(15) :tag 10)

(geo:synchronize)

;; Treat curves 2, 3 and 4 as a single curve when meshing (i.e. mesh across
;; points 6 and 7)
(mesh:set-compound 1 '(2 3 4))

;; Idem with curves 6, 7 and 8
(mesh:set-compound 1 '(6 7 8))

;; Treat surfaces 1, 5 and 10 as a single surface when meshing (i.e. mesh across
;; curves 9 and 10)
(mesh:set-compound 2 '(1 5 10))

(mesh:generate :dim 2)
;; (gmsh:write "/tmp/t12.msh")
