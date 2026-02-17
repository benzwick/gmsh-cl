;;; relocate_nodes.lisp — Relocate mesh nodes on perturbed geometry

(labels ((add-house (a b c)
           (let* ((p1 (occ:point 0 0 0))
                  (p2 (occ:point a 0 0))
                  (p3 (occ:point a b 0))
                  (p4 (occ:point (/ a 2) (+ b c) 0))
                  (p5 (occ:point 0 b 0))
                  (l1 (occ:line p1 p2))
                  (l2 (occ:line p2 p3))
                  (l3 (occ:line p3 p4))
                  (l4 (occ:line p4 p5))
                  (l5 (occ:line p5 p1))
                  (cl (occ:curve-loop (list l1 l2 l3 l4 l5))))
             ;; BSpline surface - inner nodes will move
             (occ:surface-filling cl)
             (occ:synchronize))))

  ;; Create initial geometry
  (gmsh:add "house1")
  (add-house 10 8 5)

  ;; Mesh it and store the mesh
  (mesh:generate :dim 2)
  (let ((m (make-hash-table :test 'equal)))
    (dolist (e (gmsh:get-entities))
      (setf (gethash e m)
            (list (multiple-value-list (mesh:get-nodes :dim (car e) :tag (cdr e)))
                  (multiple-value-list (mesh:get-elements :dim (car e) :tag (cdr e))))))

    ;; Create perturbed geometry
    (gmsh:add "house2")
    (add-house 9.5 8.2 5.1)

    ;; Copy old mesh into perturbed geometry
    (maphash (lambda (e data)
               (let ((nod (first data))
                     (el (second data)))
                 (mesh:add-nodes (car e) (cdr e)
                                 (first nod) (second nod)
                                 :parametric-coord (third nod))
                 (mesh:add-elements (car e) (cdr e)
                                    (first el) (second el) (third el))))
             m)

    ;; Relocate mesh nodes on the perturbed geometry
    (mesh:relocate-nodes)))
