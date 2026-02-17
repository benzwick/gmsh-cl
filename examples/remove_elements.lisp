;;; remove_elements.lisp — Removing mesh elements and reclassifying nodes

(occ:rectangle 0 0 0 1 1 :tag 1)
(occ:rectangle 2 0 0 1 1 :tag 2)
(occ:synchronize)
(mesh:generate :dim 2)

;; It is only possible to delete the mesh of an entity if the entity is not on
;; the boundary of another entity with a non-empty mesh
(mesh:clear :dim-tags '((2 . 2)))  ; ok to remove mesh of surface 2
(mesh:clear :dim-tags '((1 . 1)))  ; not ok to remove mesh of curve 1

;; It is however possible to remove all the *elements* from an entity
(mesh:remove-elements 1 1)

;; It is also possible to remove specific elements
(multiple-value-bind (types tags node-tags)
    (mesh:get-elements :dim 1 :tag 2)
  (declare (ignore types node-tags))
  (mesh:remove-elements 1 2 :element-tags (subseq (first tags) 0 (min 3 (length (first tags))))))


;; To get a model that conforms to the Gmsh data model, nodes should be
;; reclassified on the entity of lowest dimension
(mesh:reclassify-nodes)
