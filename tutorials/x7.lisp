;;; x7.lisp — Additional mesh data: internal edges and faces

;; Meshes are fully described in Gmsh by nodes and elements, both associated to
;; model entities. The API can be used to generate and handle other mesh
;; entities, i.e. mesh edges and faces, which are not stored by default.

(gmsh:add "x7")

;; Create a simple model and mesh it:
(occ:box 0 0 0 1 1 1)
(occ:synchronize)
(opt:set-number "Mesh.MeshSizeMin" 2.0)
(mesh:generate :dim 3)

;; Like elements, mesh edges and faces are described by (an ordered list of)
;; their nodes. Retrieve the edges and the (triangular) faces of all the first
;; order tetrahedra in the mesh:
(let* ((element-type (mesh:get-element-type "tetrahedron" 1))
       (edge-nodes (mesh:get-element-edge-nodes element-type))
       (face-nodes (mesh:get-element-face-nodes element-type 3)))

  ;; Gmsh can also identify unique edges and faces and assign them a unique tag:
  (mesh:create-edges)
  (mesh:create-faces)

  ;; Edge and face tags can then be retrieved by providing their nodes:
  (multiple-value-bind (edge-tags edge-orientations)
      (mesh:get-edges edge-nodes)
    (declare (ignore edge-orientations))

    (multiple-value-bind (face-tags face-orientations)
        (mesh:get-faces 3 face-nodes)
      (declare (ignore face-orientations))

      ;; Since element edge and face nodes are returned in the same order as the
      ;; elements, one can easily keep track of which element(s) each edge or
      ;; face is connected to:
      (multiple-value-bind (element-tags element-node-tags)
          (mesh:get-elements-by-type element-type)
        (declare (ignore element-node-tags))

        (let ((edges-to-elements (make-hash-table))
              (faces-to-elements (make-hash-table)))

          ;; 6 edges per tetrahedron
          (loop for i below (length edge-tags) do
            (let ((et (nth i edge-tags))
                  (el (nth (floor i 6) element-tags)))
              (push el (gethash et edges-to-elements))))

          ;; 4 faces per tetrahedron
          (loop for i below (length face-tags) do
            (let ((ft (nth i face-tags))
                  (el (nth (floor i 4) element-tags)))
              (push el (gethash ft faces-to-elements))))

          ;; Create a new discrete surface and fill it with unique triangles
          ;; corresponding to the faces of the tetrahedra:
          (let ((s (gmsh:add-discrete-entity 2))
                (max-element-tag (mesh:get-max-element-tag))
                (unique-face-tags (make-hash-table))
                (tags-for-triangles '())
                (face-nodes-for-triangles '()))

            (loop for i below (length face-tags) do
              (let ((ft (nth i face-tags)))
                (unless (gethash ft unique-face-tags)
                  (setf (gethash ft unique-face-tags) t)
                  (push (+ ft max-element-tag) tags-for-triangles)
                  (push (nth (* 3 i) face-nodes) face-nodes-for-triangles)
                  (push (nth (+ (* 3 i) 1) face-nodes) face-nodes-for-triangles)
                  (push (nth (+ (* 3 i) 2) face-nodes) face-nodes-for-triangles))))

            (setf tags-for-triangles (nreverse tags-for-triangles))
            (setf face-nodes-for-triangles (nreverse face-nodes-for-triangles))

            (let ((element-type-2d (mesh:get-element-type "triangle" 1)))
              (mesh:add-elements-by-type s element-type-2d
                                         tags-for-triangles
                                         face-nodes-for-triangles))

            ;; Print connectivity info for the new triangles:
            (dolist (tri-tag tags-for-triangles)
              (format t "triangle ~A is connected to tetrahedra ~A~%"
                      tri-tag
                      (gethash (- tri-tag max-element-tag) faces-to-elements)))))))))

;; If all you need is the list of all edges or faces in terms of their nodes,
;; you can also directly call:
(multiple-value-bind (all-edge-tags all-edge-nodes) (mesh:get-all-edges)
  (declare (ignore all-edge-nodes))
  (format t "~A unique edges~%" (length all-edge-tags)))
(multiple-value-bind (all-face-tags all-face-nodes) (mesh:get-all-faces 3)
  (declare (ignore all-face-nodes))
  (format t "~A unique triangular faces~%" (length all-face-tags)))
