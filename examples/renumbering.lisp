;;; renumbering.lisp — Mesh node renumbering with different algorithms

(opt:set-number "Mesh.MeshSizeMax" 0.2)
(opt:set-number "Mesh.MeshSizeMin" 0.2)

(occ:rectangle 0 0 0 1 1)
(occ:synchronize)

(mesh:generate)

(multiple-value-bind (old new) (mesh:compute-renumbering :method "RCMK")
  (mesh:renumber-nodes :old-tags old :new-tags new))

(multiple-value-bind (old new) (mesh:compute-renumbering :method "Hilbert")
  (mesh:renumber-nodes :old-tags old :new-tags new))

(multiple-value-bind (old new) (mesh:compute-renumbering :method "Metis")
  (mesh:renumber-nodes :old-tags old :new-tags new))
