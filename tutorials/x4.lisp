;;; x4.lisp — Post-processing data import: model-based

;; Contrary to list-based views (see x3.lisp), model-based views are based on
;; one or more meshes. Compared to list-based views, they are thus linked to one
;; model (per step). Post-processing data stored in MSH files create such
;; model-based views.

;; Let's create a first model-based view using a simple mesh constructed by hand.
;; We create a model with a discrete surface
(gmsh:add "simple model")
(let ((surf (gmsh:add-discrete-entity 2)))

  ;; We add 4 nodes and 2 3-node triangles (element type "2")
  (mesh:add-nodes 2 surf '(1 2 3 4)
                  '(0.0 0.0 0.0  1.0 0.0 0.0  1.0 1.0 0.0  0.0 1.0 0.0))
  (mesh:add-elements-by-type surf 2 '(1 2) '(1 2 3 1 3 4))

  ;; We can now create a new model-based view, to which we add 10 steps of
  ;; node-based data:
  (let ((t1 (view:add "Continuous")))
    (loop for step below 10 do
      (view:add-homogeneous-model-data
       t1 step "simple model" "NodeData"
       '(1 2 3 4)                                      ; tags of nodes
       (list 10.0 10.0 (+ 12.0 step) (+ 13.0 step))))  ; data, per node

    ;; Besides node-based data, which result in continuous fields, one can also
    ;; add general discontinuous fields defined at the nodes of each element,
    ;; using "ElementNodeData":
    (let ((t2 (view:add "Discontinuous")))
      (loop for step below 10 do
        (view:add-homogeneous-model-data
         t2 step "simple model" "ElementNodeData"
         '(1 2)                                                 ; tags of elements
         (list 10.0 10.0 (+ 12.0 step) 14.0 15.0 (+ 13.0 step)))) ; data per element nodes

      ;; Each step of a model-based view can be defined on a different model,
      ;; i.e. on a different mesh. Let's define a second model and mesh it
      (gmsh:add "another model")
      (occ:box 0 0 0 1 1 1)
      (occ:synchronize)
      (mesh:generate :dim 3)

      ;; We can add other steps to view t1 based on this new mesh:
      (multiple-value-bind (nodes coord parametric-coord)
          (mesh:get-nodes)
        (declare (ignore parametric-coord))
        (loop for step from 11 below 20 do
          (view:add-homogeneous-model-data
           t1 step "another model" "NodeData" nodes
           (loop for i below (length coord) by 3
                 collect (* step (nth i coord))))))

      ;; Model-based views can be saved to disk using view:write; note that
      ;; saving a view based on multiple meshes (like the view t1) will
      ;; automatically create several files.
      ;; (view:write t1 "/tmp/x4_t1.msh")
      ;; (view:write t2 "/tmp/x4_t2.msh")
      )))
