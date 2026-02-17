;;; opt.lisp — Mesh optimization

;; Create a mesh to optimize
(gmsh:add "opt")
(occ:box 0 0 0 1 1 1)
(occ:synchronize)
(mesh:generate :dim 3)

;; Optimize mesh (second arg forces optimization of discrete volumes)
(mesh:optimize :force t)
;; (mesh:optimize :method "Netgen" :force t)
