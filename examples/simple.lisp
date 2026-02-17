;;; simple.lisp — A simple square mesh

(gmsh:add "square")

(geo:point 0 0 0 :mesh-size 0.1 :tag 1)
(geo:point 1 0 0 :mesh-size 0.1 :tag 2)
(geo:point 1 1 0 :mesh-size 0.1 :tag 3)
(geo:point 0 1 0 :mesh-size 0.1 :tag 4)

(geo:line 1 2 :tag 1)
(geo:line 2 3 :tag 2)
(geo:line 3 4 :tag 3)
;; try automatic assignment of tag
(let ((line4 (geo:line 4 1)))
  (geo:curve-loop (list 1 2 3 line4) :tag 1))

(geo:plane-surface '(1) :tag 6)

(geo:synchronize)

(mesh:generate :dim 2)
;; (gmsh:write "square.msh")
