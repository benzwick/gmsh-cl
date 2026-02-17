;;; plugin.lisp — Using Gmsh plugins

(gmsh:add "test")
(gmsh:add-discrete-entity 2 :tag 1)
(mesh:add-nodes 2 1 '(1 2 3 4)
                '(0.0 0.0 0.0  1.0 0.0 0.0  1.0 1.0 0.0  0.0 1.0 0.0))
(mesh:add-elements 2 1 '(2) '((1 2)) '((1 2 3 1 3 4)))

;; Create a view with some data
(let ((v (view:add "some data")))
  (view:add-model-data v 0 "test" "NodeData" '(1 2 3 4)
                       '((1.0) (10.0) (20.0) (1.0)))

  ;; Test getting data back
  (multiple-value-bind (data-type tags data time num-comp)
      (view:get-model-data v 0)
    (format t "~A ~A~%" data-type tags))

  ;; Compute the iso-curve at value 11
  (plugin:set-number "Isosurface" "Value" 11.0)
  (plugin:run "Isosurface")

  ;; Delete the source view
  (view:remove v)

  ;; Check how many views the plugin created
  (let ((tags (view:get-tags)))
    (when (= 1 (length tags))
      ;; (view:write (first tags) "iso.msh")
      (multiple-value-bind (data-types num-elements data)
          (view:get-list-data (first tags))
        (format t "~A ~A~%" data-types num-elements)))))
