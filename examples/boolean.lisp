;;; boolean.lisp — Constructive solid geometry with boolean operations

(gmsh:add "boolean")

;; from http://en.wikipedia.org/wiki/Constructive_solid_geometry
(opt:set-number "Mesh.Algorithm" 6)
(opt:set-number "Mesh.MeshSizeMin" 0.4)
(opt:set-number "Mesh.MeshSizeMax" 0.4)

(let* ((r 1.4)
       (rs (* r 0.7))
       (rt (* r 1.25)))

  (occ:box (- r) (- r) (- r) (* 2 r) (* 2 r) (* 2 r) :tag 1)
  (occ:sphere 0 0 0 rt :tag 2)
  (occ:intersect (gmsh:volume-tags '(1)) (gmsh:volume-tags '(2)) :tag 3)
  (occ:cylinder (* -2 r) 0 0 (* 4 r) 0 0 rs :tag 4)
  (occ:cylinder 0 (* -2 r) 0 0 (* 4 r) 0 rs :tag 5)
  (occ:cylinder 0 0 (* -2 r) 0 0 (* 4 r) rs :tag 6)
  (occ:fuse (gmsh:volume-tags '(4 5)) (gmsh:volume-tags '(6)) :tag 7)
  (occ:cut (gmsh:volume-tags '(3)) (gmsh:volume-tags '(7)) :tag 8))

(occ:synchronize)

(mesh:generate :dim 3)
;; (gmsh:write "boolean.msh")
