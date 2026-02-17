;;; copy_mesh.lisp — Copy a mesh to a new discrete model

;; Create a model with OCC and mesh it
(gmsh:add "model1")
(occ:box 0 0 0 1 1 1)
(occ:synchronize)
(mesh:generate :dim 3)

;; Store the mesh
(let ((m (make-hash-table :test 'equal)))
  (dolist (e (gmsh:get-entities))
    (setf (gethash e m)
          (list (gmsh:get-boundary (list e))
                (multiple-value-list (mesh:get-nodes :dim (car e) :tag (cdr e)))
                (multiple-value-list (mesh:get-elements :dim (car e) :tag (cdr e))))))

  ;; Create a new model
  (gmsh:add "model2")

  ;; Create discrete entities in the new model and copy the mesh
  (let ((sorted-keys (sort (loop for k being the hash-keys of m collect k)
                           (lambda (a b)
                             (or (< (car a) (car b))
                                 (and (= (car a) (car b))
                                      (< (cdr a) (cdr b))))))))
    (dolist (e sorted-keys)
      (destructuring-bind (bnd nod ele) (gethash e m)
        (gmsh:add-discrete-entity (car e) :tag (cdr e)
                                  :boundary (mapcar #'cdr bnd))
        (mesh:add-nodes (car e) (cdr e) (first nod) (second nod))
        (mesh:add-elements (car e) (cdr e) (first ele) (second ele) (third ele)))))

  ;; Create a post-processing view based on the mesh copy
  (let* ((v (view:add "bgView"))
         (nodes (multiple-value-list (mesh:get-nodes)))
         (node-tags (first nodes))
         (coords (second nodes))
         (sizes (loop for i from 0 below (length coords) by 3
                      collect (+ (/ (nth i coords) 10.0) 0.01))))
    (view:add-homogeneous-model-data v 0 "model2" "NodeData"
                                     node-tags sizes))

  ;; Create the mesh size field for model1
  (gmsh:set-current "model1")
  (let ((field (mesh:field-add "PostView")))
    (mesh:field-set-number field "ViewTag" 0)
    (mesh:field-set-as-background-mesh field))

  ;; Mesh model1 again with the mesh size field
  (opt:set-number "Mesh.MeshSizeExtendFromBoundary" 0)
  (opt:set-number "Mesh.Algorithm3D" 10)
  (mesh:clear)
  (mesh:generate :dim 3))
