;;; hybrid_order.lisp — Hybrid mesh order (2nd order on one volume only)

(occ:box 0 0 0 1 1 1)
(occ:box 1 0 0 1 1 1)
(occ:remove-all-duplicates)
(occ:synchronize)

(mesh:generate :dim 3)

;; Mesh.MeshOnlyVisible can be used to selectively set the order
(opt:set-number "Mesh.MeshOnlyVisible" 1)
(gmsh:set-visibility '((3 . 2)) 0 :recursive t)

(mesh:set-order 2)  ; only on volume 1
(gmsh:set-visibility '((3 . 2)) 1 :recursive t)
