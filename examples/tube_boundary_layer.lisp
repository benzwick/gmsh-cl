;;; tube_boundary_layer.lisp — Tube with boundary layer extrusion

(gmsh:add "Tube boundary layer")

(opt:set-number "Mesh.MeshSizeMax" 0.1)

;; Fuse 2 cylinders and only keep outside shell
(let ((c1 (occ:cylinder 0 0 0 5 0 0 0.5))
      (c2 (occ:cylinder 2 0 -2 0 0 2 0.3)))
  (occ:fuse (list (cons 3 c1)) (list (cons 3 c2)))
  (occ:remove (occ:get-entities :dim 3))
  (occ:remove '((2 . 2) (2 . 3) (2 . 5))))

(occ:synchronize)

;; Create boundary layer extrusion
(opt:set-number "Geometry.ExtrudeReturnLateralEntities" 0)

;; Logarithmic spacing for boundary layer: 5 layers from 1e-3 to 1e-1
(let* ((n-layers 5)
       (num-elts (make-list n-layers :initial-element 1))
       (depths (loop for i below n-layers
                     collect (- (expt 10 (+ -3 (* i (/ 2.0 (1- n-layers)))))))))

  (let ((e (geo:extrude-boundary-layer (gmsh:get-entities :dim 2)
                                       :num-elements num-elts
                                       :heights depths
                                       )))

    ;; Get "top" surfaces created by extrusion
    (let ((top-surf (mapcar #'cdr (remove-if-not (lambda (s) (= (car s) 2)) e))))
      (geo:synchronize)

      ;; Get boundary of top surfaces (boundaries of holes)
      (let* ((bnd-ent (gmsh:get-boundary (remove-if-not (lambda (s) (= (car s) 2)) e)))
             (bnd-curv (mapcar #'cdr bnd-ent))
             (loops (geo:curve-loops bnd-curv)))

        (dolist (l loops)
          (push (geo:plane-surface (list l)) top-surf))

        ;; Create the inner volume
        (geo:volume (list (geo:surface-loop top-surf)))
        (geo:synchronize)

        (mesh:generate :dim 3)))))
