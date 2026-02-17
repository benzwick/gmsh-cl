;;;; test-geo.lisp — Built-in geometry kernel tests

(in-package :gmsh-cl/tests)
(in-suite :gmsh-cl/geo)

(test geo-point-returns-tag
  "geo:point returns a positive integer tag"
  (with-gmsh-test ()
    (gmsh:add "test")
    (let ((tag (geo:point 0 0 0)))
      (is (integerp tag))
      (is (plusp tag)))))

(test geo-point-explicit-tag
  "geo:point respects explicit :tag"
  (with-gmsh-test ()
    (gmsh:add "test")
    (is (= 42 (geo:point 0 0 0 :tag 42)))))

(test geo-point-with-mesh-size
  "geo:point accepts :mesh-size"
  (with-gmsh-test ()
    (gmsh:add "test")
    (let ((tag (geo:point 0 0 0 :mesh-size 0.1)))
      (is (plusp tag)))))

(test geo-line-returns-tag
  "geo:line returns a positive integer tag"
  (with-gmsh-test ()
    (gmsh:add "test")
    (geo:point 0 0 0 :tag 1)
    (geo:point 1 0 0 :tag 2)
    (let ((tag (geo:line 1 2)))
      (is (integerp tag))
      (is (plusp tag)))))

(test geo-line-explicit-tag
  "geo:line respects explicit :tag"
  (with-gmsh-test ()
    (gmsh:add "test")
    (geo:point 0 0 0 :tag 1)
    (geo:point 1 0 0 :tag 2)
    (is (= 10 (geo:line 1 2 :tag 10)))))

(test geo-circle-arc
  "geo:circle-arc creates a curve"
  (with-gmsh-test ()
    (gmsh:add "test")
    (geo:point 1 0 0 :tag 1)
    (geo:point 0 0 0 :tag 2)
    (geo:point 0 1 0 :tag 3)
    (let ((tag (geo:circle-arc 1 2 3)))
      (is (plusp tag)))))

(test geo-spline
  "geo:spline creates a curve from point tags"
  (with-gmsh-test ()
    (gmsh:add "test")
    (geo:point 0 0 0 :tag 1)
    (geo:point 1 1 0 :tag 2)
    (geo:point 2 0 0 :tag 3)
    (let ((tag (geo:spline '(1 2 3))))
      (is (plusp tag)))))

(test geo-curve-loop-and-surface
  "geo:curve-loop and geo:plane-surface work together"
  (with-gmsh-test ()
    (gmsh:add "test")
    (geo:point 0 0 0 :tag 1)
    (geo:point 1 0 0 :tag 2)
    (geo:point 1 1 0 :tag 3)
    (geo:point 0 1 0 :tag 4)
    (geo:line 1 2 :tag 1)
    (geo:line 2 3 :tag 2)
    (geo:line 3 4 :tag 3)
    (geo:line 4 1 :tag 4)
    (let ((cl-tag (geo:curve-loop '(1 2 3 4))))
      (is (plusp cl-tag))
      (let ((s-tag (geo:plane-surface (list cl-tag))))
        (is (plusp s-tag))))))

(test geo-synchronize-creates-entities
  "geo:synchronize makes entities visible to the model"
  (with-gmsh-test ()
    (gmsh:add "test")
    (geo:point 0 0 0 :tag 1)
    (geo:point 1 0 0 :tag 2)
    (geo:line 1 2 :tag 1)
    (geo:synchronize)
    (is (= 1 (length (gmsh:get-entities :dim 1))))))

(test geo-extrude
  "geo:extrude creates a volume from a surface"
  (with-gmsh-test ()
    (gmsh:add "test")
    (geo:point 0 0 0 :tag 1)
    (geo:point 1 0 0 :tag 2)
    (geo:point 1 1 0 :tag 3)
    (geo:point 0 1 0 :tag 4)
    (geo:line 1 2 :tag 1)
    (geo:line 2 3 :tag 2)
    (geo:line 3 4 :tag 3)
    (geo:line 4 1 :tag 4)
    (geo:curve-loop '(1 2 3 4) :tag 1)
    (geo:plane-surface '(1) :tag 1)
    (geo:extrude '((2 . 1)) 0 0 1)
    (geo:synchronize)
    (is (plusp (length (gmsh:get-entities :dim 3))))))

(test geo-translate
  "geo:translate moves entities"
  (with-gmsh-test ()
    (gmsh:add "test")
    (geo:point 0 0 0 :tag 1)
    (geo:translate '((0 . 1)) 1 0 0)
    (geo:synchronize)
    ;; Point still exists (translation is in-place)
    (is (= 1 (length (gmsh:get-entities :dim 0))))))

(test geo-copy
  "geo:copy duplicates entities"
  (with-gmsh-test ()
    (gmsh:add "test")
    (geo:point 0 0 0 :tag 1)
    (geo:point 1 0 0 :tag 2)
    (geo:line 1 2 :tag 1)
    (let ((copies (geo:copy '((1 . 1)))))
      (is (plusp (length copies))))
    (geo:synchronize)
    (is (= 2 (length (gmsh:get-entities :dim 1))))))

(test geo-physical-group
  "Physical groups can be created and queried"
  (with-gmsh-test ()
    (gmsh:add "test")
    (geo:point 0 0 0 :tag 1)
    (geo:point 1 0 0 :tag 2)
    (geo:line 1 2 :tag 1)
    (geo:synchronize)
    (let ((pg (gmsh:add-physical-group 1 '(1) :tag 1)))
      (is (= 1 pg)))
    (is (= 1 (length (gmsh:get-physical-groups :dim 1))))))

(test geo-transfinite
  "Transfinite meshing constraints can be set"
  (with-gmsh-test ()
    (gmsh:add "test")
    (geo:point 0 0 0 :tag 1)
    (geo:point 1 0 0 :tag 2)
    (geo:point 1 1 0 :tag 3)
    (geo:point 0 1 0 :tag 4)
    (geo:line 1 2 :tag 1)
    (geo:line 2 3 :tag 2)
    (geo:line 3 4 :tag 3)
    (geo:line 4 1 :tag 4)
    (geo:curve-loop '(1 2 3 4) :tag 1)
    (geo:plane-surface '(1) :tag 1)
    (geo:mesh-set-transfinite-curve 1 10)
    (geo:mesh-set-transfinite-curve 2 10)
    (geo:mesh-set-transfinite-curve 3 10)
    (geo:mesh-set-transfinite-curve 4 10)
    (geo:mesh-set-transfinite-surface 1)
    (geo:synchronize)
    (mesh:generate :dim 2)
    (is (plusp (length (mesh:get-nodes))))))
