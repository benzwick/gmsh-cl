;;; x1.lisp — Geometry and mesh data

;; The API allows to do much more than what can be done in .geo files. These
;; additional features are introduced gradually in the extended tutorials,
;; starting with x1.lisp.

;; In this first extended tutorial, we start by using the API to access basic
;; geometrical and mesh data.

;; Create and mesh a simple geometry
(occ:cone 1 0 0 1 0 0 0.5 0.1)
(occ:synchronize)
(mesh:generate)

;; Print the model name and dimension:
(format t "Model ~A (~AD)~%"
        (gmsh:get-current)
        (gmsh:get-dimension))

;; Geometrical data is made of elementary model `entities', called `points'
;; (entities of dimension 0), `curves' (entities of dimension 1), `surfaces'
;; (entities of dimension 2) and `volumes' (entities of dimension 3). Elementary
;; model entities are identified by their dimension and by a `tag': a strictly
;; positive identification number.

;; Get all the elementary entities in the model, as a list of (dim . tag) pairs:
(let ((entities (gmsh:get-entities)))
  (dolist (e entities)
    ;; Dimension and tag of the entity:
    (let ((dim (car e))
          (tag (cdr e)))

      ;; Get the mesh nodes for the entity (dim, tag):
      (multiple-value-bind (node-tags node-coords node-params)
          (mesh:get-nodes :dim dim :tag tag)
        (declare (ignore node-params))

        ;; Get the mesh elements for the entity (dim, tag):
        (multiple-value-bind (elem-types elem-tags elem-node-tags)
            (mesh:get-elements :dim dim :tag tag)

          ;; Print a summary of the information available on the entity and its
          ;; mesh.

          ;; Type and name of the entity:
          (let ((entity-type (gmsh:get-type dim tag))
                (name (gmsh:get-entity-name dim tag)))
            (when (plusp (length name))
              (setf name (concatenate 'string name " ")))
            (format t "Entity ~A(~A . ~A) of type ~A~%"
                    name dim tag entity-type))

          ;; Number of mesh nodes and elements:
          (let ((num-elem (reduce #'+ (mapcar #'length elem-tags))))
            (format t " - Mesh has ~A nodes and ~A elements~%"
                    (length node-tags) num-elem))

          ;; Upward and downward adjacencies:
          (multiple-value-bind (up down) (gmsh:get-adjacencies dim tag)
            (when (plusp (length up))
              (format t " - Upward adjacencies: ~A~%" up))
            (when (plusp (length down))
              (format t " - Downward adjacencies: ~A~%" down)))

          ;; Does the entity belong to physical groups?
          (let ((physical-tags (gmsh:get-physical-groups-for-entity dim tag)))
            (when (plusp (length physical-tags))
              (let ((s ""))
                (dolist (p physical-tags)
                  (let ((n (gmsh:get-physical-name dim p)))
                    (when (plusp (length n))
                      (setf n (concatenate 'string n " ")))
                    (setf s (concatenate 'string s n
                                         "(" (write-to-string dim) ", "
                                         (write-to-string p) ") "))))
                (format t " - Physical groups: ~A~%" s))))

          ;; Is the entity a partition entity? If so, what is its parent entity?
          (let ((partitions (gmsh:get-partitions dim tag)))
            (when (plusp (length partitions))
              (format t " - Partition tags: ~A - parent entity ~A~%"
                      partitions
                      (multiple-value-list (gmsh:get-parent dim tag)))))

          ;; List all types of elements making up the mesh of the entity:
          (dolist (et elem-types)
            (multiple-value-bind (ename edim eorder enumv eparv enumprim)
                (mesh:get-element-properties et)
              (declare (ignore enumprim))
              (format t " - Element type: ~A, order ~A (~A nodes in param coord: ~A)~%"
                      ename eorder enumv eparv))))))))

;; Clear all the model data:
(gmsh:clear)
