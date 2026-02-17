;;; test.lisp — Basic API test: geometry, meshing, and element inspection

(gmsh:add "square")

(geo:point 0 0 0 :mesh-size 0.1 :tag 1)
(geo:point 1 0 0 :mesh-size 0.1 :tag 2)
(geo:point 1 1 0 :mesh-size 0.1 :tag 3)
(geo:point 0 1 0 :mesh-size 0.1 :tag 4)
(geo:line 1 2 :tag 1)
(geo:line 2 3 :tag 2)
(geo:line 3 4 :tag 3)
(let ((line4 (geo:line 4 1)))
  (format t "line4 received tag ~A~%" line4)
  (geo:curve-loop (list 1 2 3 line4) :tag 1))

(geo:plane-surface '(1) :tag 6)
(geo:synchronize)

(let ((ptag (gmsh:add-physical-group 1 '(1 2 3 4))))
  (let ((ent (gmsh:get-entities-for-physical-group 1 ptag)))
    (format t "new physical group ~A: ~A~%" ptag ent)))

(gmsh:add-physical-group 2 '(6))

(format t "Algorithm: ~A~%" (opt:get-number "Mesh.Algorithm"))
(opt:set-number "Mesh.Algorithm" 3.0)
(format t "Algorithm: ~A~%" (opt:get-number "Mesh.Algorithm"))
(mesh:generate :dim 2)

(format t "Entities~%")
(dolist (e (gmsh:get-entities))
  (format t "entity ~A~%" e)
  (multiple-value-bind (types tags nodes)
      (mesh:get-elements :dim (car e) :tag (cdr e))
    (loop for i below (length types) do
      (format t "type ~A~%" (nth i types))
      (format t "tags: ~A~%" (nth i tags))
      (format t "nodes: ~A~%" (nth i nodes)))))

(format t "Nodes~%")
(multiple-value-bind (tags coord)
    (mesh:get-nodes :dim 2 :tag 6)
  (format t "~A~%" tags)
  (format t "~A~%" coord))
