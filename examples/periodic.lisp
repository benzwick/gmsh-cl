;;; periodic.lisp — Periodic mesh constraints

(gmsh:add "periodic")

(let ((r 2))
  (occ:box 0 0 0 r r r)
  (occ:synchronize)

  (let ((ent (gmsh:get-entities :dim 0)))
    (mesh:set-size ent 1))
  (mesh:set-size '((0 . 1)) 0.01)
  (mesh:set-periodic 2 '(2) '(1)
                     (list 1 0 0 r  0 1 0 0  0 0 1 0  0 0 0 1)))

(mesh:generate :dim 2)

(multiple-value-bind (master-tag node-tags node-master-tags tfo)
    (mesh:get-periodic-nodes 2 2)
  (format t "~A ~A ~A ~A~%" master-tag node-tags node-master-tags tfo))

;; (gmsh:write "/tmp/periodic.msh")
