;;;; test-core.lisp — Core lifecycle and error handling tests

(in-package :gmsh-cl/tests)
(in-suite :gmsh-cl/core)

(test initialize-finalize
  "gmsh can be initialized and finalized"
  (gmsh:initialize)
  (is (= 1 (gmsh:is-initialized)))
  (gmsh:finalize)
  (is (= 0 (gmsh:is-initialized))))

(test with-gmsh-lifecycle
  "with-gmsh properly manages lifecycle"
  (gmsh:with-gmsh ()
    (is (= 1 (gmsh:is-initialized))))
  (is (= 0 (gmsh:is-initialized))))

(test with-gmsh-cleanup-on-error
  "with-gmsh finalizes even when body signals"
  (ignore-errors
    (gmsh:with-gmsh ()
      (error "deliberate")))
  (is (= 0 (gmsh:is-initialized))))

(test add-model
  "Can add and switch models"
  (with-gmsh-test ()
    (gmsh:add "model-a")
    (is (string= "model-a" (gmsh:get-current)))
    (gmsh:add "model-b")
    (is (string= "model-b" (gmsh:get-current)))
    (gmsh:set-current "model-a")
    (is (string= "model-a" (gmsh:get-current)))))

(test clear-resets-state
  "gmsh:clear removes all models"
  (with-gmsh-test ()
    (gmsh:add "test")
    (occ:box 0 0 0 1 1 1)
    (occ:synchronize)
    (is (plusp (length (gmsh:get-entities))))
    (gmsh:clear)
    (is (zerop (length (gmsh:get-entities))))))
