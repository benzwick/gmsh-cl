;;; view_element_size.lisp — Visualizing element sizes

(gmsh:add "my model")
(occ:box 0 0 0 1 1 1)
(occ:synchronize)
(mesh:set-size (gmsh:get-entities :dim 0) 0.1)
(mesh:set-size '((0 . 1)) 0.01)
(mesh:generate :dim 3)

;; Get element volumes
(multiple-value-bind (types ele-tags node-tags)
    (mesh:get-elements :dim 3)
  (declare (ignore types node-tags))
  (let* ((tags (first ele-tags))
         (q (mesh:get-element-qualities tags :quality-name "volume"))
         (v (view:add "Element sizes")))
    (view:add-homogeneous-model-data v 0 "my model" "ElementData" tags q)))
