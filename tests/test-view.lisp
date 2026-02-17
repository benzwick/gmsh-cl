;;;; test-view.lisp — Post-processing view tests

(in-package :gmsh-cl/tests)
(in-suite :gmsh-cl/view)

(test view-add-returns-tag
  "view:add returns a non-negative tag"
  (with-gmsh-test ()
    (let ((tag (view:add "test-view")))
      (is (integerp tag))
      (is (>= tag 0)))))

(test view-add-explicit-tag
  "view:add respects explicit :tag"
  (with-gmsh-test ()
    (is (= 5 (view:add "test-view" :tag 5)))))

(test view-get-tags
  "view:get-tags returns created views"
  (with-gmsh-test ()
    (view:add "v1")
    (view:add "v2")
    (let ((tags (view:get-tags)))
      (is (= 2 (length tags))))))

(test view-remove
  "view:remove deletes a view"
  (with-gmsh-test ()
    (let ((tag (view:add "v1")))
      (is (= 1 (length (view:get-tags))))
      (view:remove tag)
      (is (= 0 (length (view:get-tags)))))))

(test view-add-list-data
  "view:add-list-data adds data to a view"
  (with-gmsh-test ()
    (let ((tag (view:add "test")))
      ;; Scalar point "SP": 3 coords + 1 value, flat list
      (view:add-list-data tag "SP" 1 '(0.0 0.0 0.0 1.0))
      (is (= 1 (length (view:get-tags)))))))

(test view-add-model-data
  "view:add-model-data works with a meshed model"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:rectangle 0 0 0 1 1)
    (occ:synchronize)
    (mesh:generate :dim 2)
    (let* ((tag (view:add "node-data"))
           (nodes (mesh:get-nodes))
           ;; Each node gets a single scalar value (its tag as a double)
           (data (mapcar (lambda (n) (list (coerce n 'double-float))) nodes)))
      (view:add-model-data tag 0 "test" "NodeData" nodes data)
      (is (= 1 (length (view:get-tags)))))))

(test view-option-set-get-number
  "View options can be set and retrieved"
  (with-gmsh-test ()
    (let ((tag (view:add "test")))
      (declare (ignore tag))
      (view:option-set-number 0 "Visible" 0)
      (is (= 0.0 (view:option-get-number 0 "Visible"))))))
