;;; t1.lisp — Geometry basics, elementary entities, physical groups
;;;
;;; This first tutorial covers the basics of creating a simple mesh with
;;; the built-in CAD kernel. It introduces:
;;;   - Points, lines, curve loops, and plane surfaces
;;;   - Physical groups for labeling mesh entities
;;;   - Mesh generation
;;;
;;; CL bindings use package nicknames: geo = gmsh/geo, occ = gmsh/occ,
;;; mesh = gmsh/mesh, opt = gmsh/option, view = gmsh/view, etc.
;;; See src/generated/packages.lisp for the full list.
;;;
;;; Equivalent Python: gmsh/tutorials/python/t1.py

;; Create a new model named "t1". In gmsh, models are separate geometry
;; and mesh containers. gmsh:add creates one and makes it current.
(gmsh:add "t1")

;; Define the characteristic mesh size. This controls how fine the mesh
;; will be near each point. A smaller value = more elements.
(let ((lc 1e-2))

  ;; Create four corner points of a rectangle.
  ;; geo:point takes (x y z &key mesh-size tag).
  ;; Tags are explicit integers that identify entities in gmsh.
  (geo:point 0 0 0 :mesh-size lc :tag 1)
  (geo:point 0.1 0 0 :mesh-size lc :tag 2)
  (geo:point 0.1 0.3 0 :mesh-size lc :tag 3)

  ;; Here we let gmsh auto-assign the tag (returned by the function):
  (let ((p4 (geo:point 0 0.3 0 :mesh-size lc)))

    ;; Create four lines connecting the points.
    ;; geo:line takes (start-tag end-tag &key tag).
    (geo:line 1 2 :tag 1)
    (geo:line 3 2 :tag 2)
    (geo:line 3 p4 :tag 3)
    (geo:line p4 1 :tag 4)))

;; Create a curve loop from the lines. Negative tags indicate reverse
;; orientation. The order matters: the loop must be oriented consistently.
(geo:curve-loop '(4 1 -2 3) :tag 1)

;; Create a plane surface bounded by the curve loop.
(geo:plane-surface '(1) :tag 1)

;; The built-in CAD kernel requires an explicit synchronize call to
;; transfer geometry to the gmsh model before meshing or querying.
(geo:synchronize)

;; Physical groups assign labels to collections of mesh entities.
;; They're essential for FEM: they define boundary conditions and material
;; regions. The first argument is the dimension (0=point, 1=curve,
;; 2=surface, 3=volume).
(gmsh:add-physical-group 1 '(1 2 4) :tag 5)
(gmsh:add-physical-group 2 '(1) :name "My surface")

;; Generate the 2D mesh.
(mesh:generate :dim 2)

;; Alternative: the same rectangle could be created in one call using the
;; geo:polygon helper (see src/api/geo.lisp):
;;   (geo:polygon '((0 0 0) (0.1 0 0) (0.1 0.3 0) (0 0.3 0)) :size 1e-2)
;; This creates points, lines, a curve loop, and a plane surface.

;; To write the mesh to a file:
;; (gmsh:write "/tmp/t1.msh")
