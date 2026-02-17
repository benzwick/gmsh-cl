;;; transfinite.lisp — Transfinite meshing with Progression
;;;
;;; Creates a box with a cylindrical hole, then applies transfinite
;;; meshing constraints with both simple and explicit-corner approaches.
;;; Port of boolean/transfinite.geo

(gmsh:add "transfinite")

(opt:set-number "Mesh.Algorithm" 6)
(opt:set-number "Mesh.MeshSizeMin" 1)
(opt:set-number "Mesh.MeshSizeMax" 1)

(let ((n 10))
  ;; Create a box with a cylindrical hole
  (occ:box 0 0 0 1 1 1 :tag 1)
  (occ:cylinder 0.5 0 0 0 1 0 0.7 :tag 2)
  (occ:cut '((3 . 1)) '((3 . 2)) :tag 3)

  (occ:synchronize)

  ;; Get the boundary curves of volume 3
  (let* ((surfs (gmsh:get-boundary '((3 . 3))))
         (curves (gmsh:get-boundary surfs :combined nil :oriented nil))
         (unique-curves (remove-duplicates (gmsh:tags-of curves))))

    ;; Simple transfinite mesh: uniform N points on all curves
    (dolist (c unique-curves)
      (geo:mesh-set-transfinite-curve c n))

    ;; Transfinite surface 5 (simple, auto corners)
    (geo:mesh-set-transfinite-surface 5)

    ;; Transfinite with explicit corners on surface 4
    ;; First, set a different N on one curve to demonstrate Progression
    (geo:mesh-set-transfinite-curve 9 (1- (* 2 n)))

    ;; Get the boundary of surface 4 and find corner points
    (let* ((l4 (gmsh:get-boundary '((2 . 4))))
           (p4 (remove-duplicates
                (gmsh:tags-of
                 (gmsh:get-boundary l4 :combined nil :oriented nil)))))
      ;; Use first 4 corner points for explicit transfinite
      (geo:mesh-set-transfinite-surface 4 :corner-tags (subseq p4 0 4)))))

(geo:synchronize)
(mesh:generate :dim 2)
;; (gmsh:write "/tmp/transfinite.msh")
