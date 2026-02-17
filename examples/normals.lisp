;;; normals.lisp — Computing surface normals and curvatures

(gmsh:add "normals")
(occ:sphere 0 0 0 1)
(occ:box 2 0 0 1 1 1)
(occ:synchronize)
(mesh:generate :dim 2)

(let ((nn '())
      (cc '()))

  ;; Get all surfaces
  (dolist (e (gmsh:get-entities :dim 2))
    (let ((surf (cdr e)))
      ;; Get nodes on surface, including boundary nodes
      (multiple-value-bind (tags coord param)
          (mesh:get-nodes :dim 2 :tag surf :include-boundary t)
        (declare (ignore tags))
        ;; Get surface normals on all nodes
        (let ((normals (gmsh:get-normal surf param))
              (curv (gmsh:get-curvature 2 surf param)))
          (loop for i from 0 below (length coord) by 3 do
            (setf nn (nconc nn (list (nth i coord) (nth (1+ i) coord) (nth (+ i 2) coord)
                                     (nth i normals) (nth (1+ i) normals) (nth (+ i 2) normals))))
            (setf cc (nconc cc (list (nth i coord) (nth (1+ i) coord) (nth (+ i 2) coord)
                                     (nth (/ i 3) curv)))))))))

  (let ((t-norm (view:add "normals")))
    (view:add-list-data t-norm "VP" (/ (length nn) 6) nn)
    ;; (view:write t-norm "normals.pos")
    )

  (let ((t-curv (view:add "curvatures")))
    (view:add-list-data t-curv "SP" (/ (length cc) 4) cc)
    ;; (view:write t-curv "curvatures.pos")
    ))
