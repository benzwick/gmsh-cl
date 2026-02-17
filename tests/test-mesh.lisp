;;;; test-mesh.lisp — Meshing tests

(in-package :gmsh-cl/tests)
(in-suite :gmsh-cl/mesh)

(test mesh-generate-2d
  "mesh:generate produces nodes for a 2D mesh"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:rectangle 0 0 0 1 1)
    (occ:synchronize)
    (mesh:generate :dim 2)
    (is (plusp (length (mesh:get-nodes))))))

(test mesh-generate-3d
  "mesh:generate produces nodes for a 3D mesh"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:box 0 0 0 1 1 1)
    (occ:synchronize)
    (mesh:generate :dim 3)
    (is (plusp (length (mesh:get-nodes))))))

(test mesh-get-elements
  "mesh:get-elements returns element data"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:rectangle 0 0 0 1 1)
    (occ:synchronize)
    (mesh:generate :dim 2)
    (is (plusp (length (mesh:get-elements))))))

(test mesh-get-element-types
  "mesh:get-element-types returns element types"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:rectangle 0 0 0 1 1)
    (occ:synchronize)
    (mesh:generate :dim 2)
    (let ((types (mesh:get-element-types)))
      (is (plusp (length types))))))

(test mesh-set-order
  "mesh:set-order changes element order"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:rectangle 0 0 0 1 1)
    (occ:synchronize)
    (mesh:generate :dim 2)
    (let ((nodes-before (length (mesh:get-nodes))))
      (mesh:set-order 2)
      (let ((nodes-after (length (mesh:get-nodes))))
        (is (> nodes-after nodes-before))))))

(test mesh-refine
  "mesh:refine increases element count"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:rectangle 0 0 0 1 1)
    (occ:synchronize)
    (mesh:generate :dim 2)
    (let ((n-before (length (mesh:get-nodes))))
      (mesh:refine)
      (let ((n-after (length (mesh:get-nodes))))
        (is (> n-after n-before))))))

(test mesh-recombine
  "mesh:recombine converts triangles to quads"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:rectangle 0 0 0 1 1)
    (occ:synchronize)
    (mesh:generate :dim 2)
    (mesh:recombine)
    ;; After recombine, should have quad elements (type 3)
    (let ((types (mesh:get-element-types :dim 2 :tag 1)))
      (is (member 3 types)))))

(test mesh-field-distance
  "Mesh size fields can be created"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:rectangle 0 0 0 1 1)
    (occ:synchronize)
    (let ((f (mesh:field-add "Distance")))
      (is (plusp f)))))

(test mesh-partition
  "mesh:partition splits mesh into parts"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:box 0 0 0 1 1 1)
    (occ:synchronize)
    (mesh:generate :dim 3)
    (mesh:partition 4)
    (is (= 4 (gmsh:get-number-of-partitions)))))

(test mesh-write-read-msh
  "Can write and read .msh files"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:rectangle 0 0 0 1 1)
    (occ:synchronize)
    (mesh:generate :dim 2)
    (let ((path "/tmp/gmsh-cl-test-mesh.msh"))
      (gmsh:write path)
      (is (probe-file path))
      (when (probe-file path) (delete-file path)))))
