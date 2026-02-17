;;; view_renumbering.lisp — View renumbering with mesh renumbering

(gmsh:add "simple model")
(let ((surf (gmsh:add-discrete-entity 2)))
  (mesh:add-nodes 2 surf '(11 12 13 14)
                  '(0.0 0.0 0.0  1.0 0.0 0.0  1.0 1.0 0.0  0.0 1.0 0.0))
  (mesh:add-elements-by-type surf 2 '(100 102) '(11 12 13 11 13 14))

  (let ((t1 (view:add "A nodal view")))
    (loop for step below 10 do
      (view:add-homogeneous-model-data
       t1 step "simple model" "NodeData"
       '(11 12 13 14)
       (list 10.0 10.0 (+ 12.0 step) (+ 13.0 step))))

    (let ((t2 (view:add "An element view")))
      (view:add-homogeneous-model-data
       t2 0 "simple model" "ElementData"
       '(100 102)
       (list 3.14 6.28))

      ;; Renumber nodes and elements - the views should be automatically
      ;; renumbered as well
      (mesh:renumber-nodes)
      (mesh:renumber-elements))))
