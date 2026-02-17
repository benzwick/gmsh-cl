;;; mesh_from_discrete_curve.lisp — 2D surface mesh with a discrete boundary

(gmsh:add "2d surface mesh with purely discrete boundary")

;; Create a discrete curve with N nodes and N line elements
(gmsh:add-discrete-entity 1 :tag 100)
(let* ((n 50)
       (dt (/ (* 2 pi) n))
       (pts (loop for i below n
                  append (list (cos (* i dt)) (sin (* i dt)) 0.0)))
       (node-tags (loop for i from 1 to n collect i))
       ;; Build element connectivity: each line segment connects consecutive nodes
       (connectivity (loop for i from 1 to n
                          append (list i (if (= i n) 1 (1+ i))))))

  (mesh:add-nodes 1 100 node-tags pts)
  (mesh:add-elements 1 100 '(1)
                     (list (loop for i from 1 to n collect i))
                     (list connectivity)))

;; Create a plane surface from the discrete curve
(geo:curve-loop '(100) :tag 101)
(geo:plane-surface '(101) :tag 102)
(geo:synchronize)

(mesh:generate :dim 2)
