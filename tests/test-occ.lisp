;;;; test-occ.lisp — OpenCASCADE geometry kernel tests

(in-package :gmsh-cl/tests)
(in-suite :gmsh-cl/occ)

(test occ-box-creates-volume
  "occ:box creates a single volume"
  (with-gmsh-test ()
    (gmsh:add "test")
    (let ((tag (occ:box 0 0 0 1 1 1)))
      (is (plusp tag)))
    (occ:synchronize)
    (is (= 1 (length (gmsh:get-entities :dim 3))))))

(test occ-box-explicit-tag
  "occ:box respects explicit :tag"
  (with-gmsh-test ()
    (gmsh:add "test")
    (is (= 5 (occ:box 0 0 0 1 1 1 :tag 5)))))

(test occ-sphere
  "occ:sphere creates a volume"
  (with-gmsh-test ()
    (gmsh:add "test")
    (let ((tag (occ:sphere 0 0 0 1)))
      (is (plusp tag)))
    (occ:synchronize)
    (is (= 1 (length (gmsh:get-entities :dim 3))))))

(test occ-cylinder
  "occ:cylinder creates a volume"
  (with-gmsh-test ()
    (gmsh:add "test")
    (let ((tag (occ:cylinder 0 0 0 0 0 1 0.5)))
      (is (plusp tag)))
    (occ:synchronize)
    (is (= 1 (length (gmsh:get-entities :dim 3))))))

(test occ-cone
  "occ:cone creates a volume"
  (with-gmsh-test ()
    (gmsh:add "test")
    (let ((tag (occ:cone 0 0 0 0 0 1 0.5 0.1)))
      (is (plusp tag)))
    (occ:synchronize)
    (is (= 1 (length (gmsh:get-entities :dim 3))))))

(test occ-torus
  "occ:torus creates a volume"
  (with-gmsh-test ()
    (gmsh:add "test")
    (let ((tag (occ:torus 0 0 0 1 0.3)))
      (is (plusp tag)))
    (occ:synchronize)
    (is (= 1 (length (gmsh:get-entities :dim 3))))))

(test occ-rectangle
  "occ:rectangle creates a surface"
  (with-gmsh-test ()
    (gmsh:add "test")
    (let ((tag (occ:rectangle 0 0 0 1 1)))
      (is (plusp tag)))
    (occ:synchronize)
    (is (= 1 (length (gmsh:get-entities :dim 2))))))

(test occ-disk
  "occ:disk creates a surface"
  (with-gmsh-test ()
    (gmsh:add "test")
    (let ((tag (occ:disk 0 0 0 1 1)))
      (is (plusp tag)))
    (occ:synchronize)
    (is (= 1 (length (gmsh:get-entities :dim 2))))))

(test occ-boolean-fuse
  "occ:fuse merges two volumes"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:box 0 0 0 1 1 1 :tag 1)
    (occ:box 0.5 0 0 1 1 1 :tag 2)
    (multiple-value-bind (out map) (occ:fuse '((3 . 1)) '((3 . 2)))
      (is (plusp (length out)))
      (is (listp map)))
    (occ:synchronize)
    (is (= 1 (length (gmsh:get-entities :dim 3))))))

(test occ-boolean-cut
  "occ:cut subtracts tool from object"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:box 0 0 0 1 1 1 :tag 1)
    (occ:sphere 0.5 0.5 0.5 0.3 :tag 2)
    (occ:cut '((3 . 1)) '((3 . 2)))
    (occ:synchronize)
    (is (= 1 (length (gmsh:get-entities :dim 3))))))

(test occ-boolean-fragment
  "occ:fragment creates conformal mesh interfaces"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:box 0 0 0 1 1 1 :tag 1)
    (occ:box 0.5 0 0 1 1 1 :tag 2)
    (occ:fragment '((3 . 1)) '((3 . 2)))
    (occ:synchronize)
    (is (= 3 (length (gmsh:get-entities :dim 3))))))

(test occ-extrude
  "occ:extrude creates volume from surface"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:rectangle 0 0 0 1 1 :tag 1)
    (occ:extrude '((2 . 1)) 0 0 0.5)
    (occ:synchronize)
    (is (plusp (length (gmsh:get-entities :dim 3))))))

(test occ-revolve
  "occ:revolve creates volume by rotation"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:rectangle 1 0 0 1 1 :tag 1)
    (occ:revolve '((2 . 1)) 0 0 0 0 1 0 (/ pi 2))
    (occ:synchronize)
    (is (plusp (length (gmsh:get-entities :dim 3))))))

(test occ-translate-and-copy
  "occ:copy + occ:translate works"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:box 0 0 0 1 1 1 :tag 1)
    (let ((copies (occ:copy '((3 . 1)))))
      (is (plusp (length copies)))
      (occ:translate copies 2 0 0))
    (occ:synchronize)
    (is (= 2 (length (gmsh:get-entities :dim 3))))))

(test occ-fillet
  "occ:fillet rounds edges of a box"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:box 0 0 0 1 1 1 :tag 1)
    (occ:synchronize)
    (let ((edges (mapcar #'cdr (gmsh:get-entities :dim 1))))
      (occ:fillet '(1) edges '(0.1)))
    (occ:synchronize)
    (is (plusp (length (gmsh:get-entities :dim 3))))))

(test occ-import-step
  "occ:import-shapes loads a STEP file"
  (with-gmsh-test ()
    (gmsh:add "test")
    (let ((path (tutorial-data-path "t20_data.step")))
      (when (probe-file path)
        (occ:import-shapes path)
        (occ:synchronize)
        (is (plusp (length (gmsh:get-entities :dim 3))))))))
