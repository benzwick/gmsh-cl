;;; t1.lisp — Geometry basics, elementary entities, physical groups

(gmsh:add "t1")

(let ((lc 1e-2))
  (geo:point 0 0 0 :mesh-size lc :tag 1)
  (geo:point 0.1 0 0 :mesh-size lc :tag 2)
  (geo:point 0.1 0.3 0 :mesh-size lc :tag 3)
  (let ((p4 (geo:point 0 0.3 0 :mesh-size lc)))

    (geo:line 1 2 :tag 1)
    (geo:line 3 2 :tag 2)
    (geo:line 3 p4 :tag 3)
    (geo:line p4 1 :tag 4)))

(geo:curve-loop '(4 1 -2 3) :tag 1)
(geo:plane-surface '(1) :tag 1)

(geo:synchronize)

(gmsh:add-physical-group 1 '(1 2 4) :tag 5)
(gmsh:add-physical-group 2 '(1) :name "My surface")

(mesh:generate :dim 2)
;; (gmsh:write "/tmp/t1.msh")
