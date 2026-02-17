;;; flatten2.lisp — Flatten a mesh using affine transform (simpler approach)

;; Create some geometry to have a mesh to work with
(gmsh:add "flatten2")
(occ:sphere 0 0 0 1)
(occ:synchronize)
(mesh:generate :dim 2)

;; Simply use an affine transform to set all z coordinates to 0
(mesh:affine-transform '(1 0 0 0
                         0 1 0 0
                         0 0 0 0))
