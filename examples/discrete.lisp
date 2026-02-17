;;; discrete.lisp — Create a discrete surface with nodes and elements

(gmsh:add "test")

;; Add discrete surface with tag 1
(gmsh:add-discrete-entity 2 :tag 1)

;; Add 4 mesh nodes
(mesh:add-nodes
 2 1
 '(1 2 3 4)
 '(0.0 0.0 0.0
   1.0 0.0 0.0
   1.0 1.0 0.0
   0.0 1.0 0.0))

;; Add 2 triangles
(mesh:add-elements
 2 1
 '(2)                           ; single type: 3-node triangle
 '((1 2))                       ; triangle tags: 1 and 2
 '((1 2 3 1 3 4)))             ; triangle 1: nodes 1,2,3; triangle 2: nodes 1,3,4

;; (gmsh:write "/tmp/discrete.msh")
