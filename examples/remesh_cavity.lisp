;;; remesh_cavity.lisp — Remove elements to create a cavity and remesh it

;; Create a cube and mesh it
(let ((cube 1))
  (occ:box 0 0 0 10 10 10 :tag cube)
  (occ:synchronize)
  (opt:set-number "Mesh.Algorithm3D" 10)
  (opt:set-number "Mesh.MeshSizeMax" 0.3)
  (mesh:generate :dim 3)

  ;; Remove some tetrahedra from the volume mesh
  (plugin:set-number "Invisible" "DeleteElements" 1.0)
  (plugin:set-number "Invisible" "Inside" 1)
  (plugin:set-number "Invisible" "XMin" 2)
  (plugin:set-number "Invisible" "YMin" 4)
  (plugin:set-number "Invisible" "ZMin" 2)
  (plugin:set-number "Invisible" "XMax" 8)
  (plugin:set-number "Invisible" "YMax" 5)
  (plugin:set-number "Invisible" "ZMax" 8)
  (plugin:run "Invisible")
  (plugin:set-number "Invisible" "XMin" 2)
  (plugin:set-number "Invisible" "YMin" 2)
  (plugin:set-number "Invisible" "ZMin" 4)
  (plugin:set-number "Invisible" "XMax" 3)
  (plugin:set-number "Invisible" "YMax" 8)
  (plugin:set-number "Invisible" "ZMax" 6)
  (plugin:run "Invisible")

  ;; Get faces from remaining tets and find the cavity boundary
  (let ((facenodes (mesh:get-element-face-nodes 4 3 :tag cube)))
    (mesh:create-faces :dim-tags (list (cons 3 cube)))
    (multiple-value-bind (facetags faceori)
        (mesh:get-faces 3 facenodes)

      ;; Build face->nodes map
      (let ((facemap (make-hash-table)))
        (loop for i below (length facetags)
              for ft = (nth i facetags)
              do (setf (gethash ft facemap)
                       (list (nth (* 3 i) facenodes)
                             (nth (1+ (* 3 i)) facenodes)
                             (nth (+ (* 3 i) 2) facenodes))))

        ;; Count faces: boundary faces appear once, interior faces twice
        (let ((face-count (make-hash-table)))
          (dolist (ft facetags)
            (incf (gethash ft face-count 0)))
          (let ((bndfaces (make-hash-table)))
            (maphash (lambda (ft count)
                       (when (= count 1)
                         (setf (gethash ft bndfaces) t)))
                     face-count)

            ;; Remove faces on the boundary of the cube
            (let ((bnd (gmsh:get-boundary (list (cons 3 cube)) :oriented nil)))
              (dolist (b bnd)
                (let ((bfn (mesh:get-element-face-nodes 2 3 :tag (cdr b))))
                  (multiple-value-bind (bft bfo)
                      (mesh:get-faces 3 bfn)
                    (declare (ignore bfo))
                    (dolist (f bft)
                      (remhash f bndfaces))))))

            ;; Create discrete surface with the cavity boundary triangles
            (let ((news (gmsh:add-discrete-entity 2))
                  (nn '()))
              (maphash (lambda (f v)
                         (declare (ignore v))
                         (setf nn (nconc nn (gethash f facemap))))
                       bndfaces)
              (mesh:add-elements-by-type news 2 '() nn)

              ;; Reclassify nodes and create volume
              (mesh:reclassify-nodes)
              (let ((newv (gmsh:add-discrete-entity 3 :boundary (list news))))
                (mesh:create-geometry :dim-tags (list (cons 3 newv)))

                ;; Only mesh entities that have no mesh
                (opt:set-number "Mesh.MeshOnlyEmpty" 1)
                (mesh:generate :dim 3)))))))))
