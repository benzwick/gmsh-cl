;;;; test-options.lisp — Options tests

(in-package :gmsh-cl/tests)
(in-suite :gmsh-cl/options)

(test option-set-get-number
  "opt:set-number and opt:get-number round-trip"
  (with-gmsh-test ()
    (opt:set-number "Mesh.Algorithm" 6)
    (is (= 6.0 (opt:get-number "Mesh.Algorithm")))))

(test option-set-get-string
  "opt:set-string and opt:get-string round-trip"
  (with-gmsh-test ()
    (opt:set-string "General.DefaultFileName" "test.msh")
    (is (string= "test.msh" (opt:get-string "General.DefaultFileName")))))

(test option-set-get-color
  "opt:set-color and opt:get-color round-trip"
  (with-gmsh-test ()
    (opt:set-color "Geometry.Points" 255 0 0)
    (multiple-value-bind (r g b a) (opt:get-color "Geometry.Points")
      (is (= 255 r))
      (is (= 0 g))
      (is (= 0 b))
      (is (= 255 a)))))

(test option-restore-defaults
  "opt:restore-defaults resets options"
  (with-gmsh-test ()
    (let ((original (opt:get-number "Mesh.Algorithm")))
      (opt:set-number "Mesh.Algorithm" 9)
      (is (= 9.0 (opt:get-number "Mesh.Algorithm")))
      (opt:restore-defaults)
      (is (= original (opt:get-number "Mesh.Algorithm"))))))

(test option-mesh-save-all
  "Mesh.SaveAll option can be toggled"
  (with-gmsh-test ()
    (opt:set-number "Mesh.SaveAll" 1)
    (is (= 1.0 (opt:get-number "Mesh.SaveAll")))
    (opt:set-number "Mesh.SaveAll" 0)
    (is (= 0.0 (opt:get-number "Mesh.SaveAll")))))
