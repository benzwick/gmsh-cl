;;; view.lisp — Discrete mesh view creation and model-based data

(gmsh:add "test")
(gmsh:add-discrete-entity 2 :tag 1)
(mesh:add-nodes 2 1 '(1 2 3 4)
                '(0.0 0.0 0.0  1.0 0.0 0.0  1.0 1.0 0.0  0.0 1.0 0.0))
(mesh:add-elements 2 1 '(2) '((1 2)) '((1 2 3 1 3 4)))

;; Create a new post-processing view
(let ((v (view:add "some data")))
  ;; Add 10 steps of model-based data, on the nodes of the mesh
  (loop for step below 10 do
    (view:add-model-data
     v step "test" "NodeData"
     '(1 2 3 4)
     (list (list 10.0) (list 10.0) (list (+ 12.0 step)) (list (+ 13.0 step)))))
  ;; (view:write v "data.msh")
  )
