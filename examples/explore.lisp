;;; explore.lisp — Explore a mesh: nodes, elements, types, boundary

;; Create a mesh to explore
(gmsh:add "explore")
(occ:box 0 0 0 1 1 1)
(occ:synchronize)
(mesh:generate :dim 3)

(format t "Model name: ~A~%" (gmsh:get-current))

;; Get all elementary entities in the model
(dolist (e (gmsh:get-entities))
  (format t "Entity ~A of type ~A~%" e (gmsh:get-type (car e) (cdr e)))

  ;; Get mesh nodes for each entity
  (multiple-value-bind (node-tags node-coords node-params)
      (mesh:get-nodes :dim (car e) :tag (cdr e))
    (declare (ignore node-params))

    ;; Get mesh elements for each entity
    (multiple-value-bind (elem-types elem-tags elem-node-tags)
        (mesh:get-elements :dim (car e) :tag (cdr e))
      (let ((num-elem (reduce #'+ (mapcar #'length elem-tags))))
        (format t " - mesh has ~A nodes and ~A elements~%"
                (length node-tags) num-elem))

      (let ((boundary (gmsh:get-boundary (list e))))
        (format t " - boundary entities ~A~%" boundary))

      (let ((partitions (gmsh:get-partitions (car e) (cdr e))))
        (when (plusp (length partitions))
          (format t " - Partition tag(s): ~A - parent entity ~A~%"
                  partitions (gmsh:get-parent (car e) (cdr e)))))

      (dolist (typ elem-types)
        (multiple-value-bind (name dim order numv parv)
            (mesh:get-element-properties typ)
          (format t " - Element type: ~A, order ~A (~A nodes in param coord: ~A)~%"
                  name order numv parv))))))
