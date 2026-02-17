;;; spherical_surf.lisp — Cut a sphere octant with boxes

(gmsh:add "sphere_cut")

(let ((r 1)
      (r1 0.95))
  (let ((sph (occ:sphere 0 0 0 r :angle1 0 :angle2 (/ pi 2) :angle3 (/ pi 2)))
        (b1 (occ:box r1 0 0 r r r))
        (b2 (occ:box 0 r1 0 r r r))
        (b3 (occ:box 0 0 r1 r r r)))
    (occ:cut (list (cons 3 sph))
             (list (cons 3 b1) (cons 3 b2) (cons 3 b3)))
    (occ:synchronize)

    (gmsh:remove-entities (list (cons 3 sph)))
    (gmsh:remove-entities '((2 . 2) (2 . 4) (2 . 6)) :recursive t)))
