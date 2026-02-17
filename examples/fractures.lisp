;;; fractures.lisp — Identify fracture parts after boolean fragments

(occ:rectangle -0.6 -0.6 0 2.2 2.2 :tag 1)

(let ((n 20))
  (loop for i from 2 to (1+ n)
        for h = (+ 0.05 (* (random 1.0) 0.5))
        for w = (+ 0.01 (* (random 1.0) 0.1))
        for x = (random 1.0)
        for y = (random 1.0)
        for alpha = (* (random 1.0) 2 pi)
        do (occ:rectangle x y 0 w h :tag i)
           (occ:rotate (list (cons 2 i)) x y 0 0 0 1 alpha))

  (let ((rin (loop for i from 1 to (1+ n) collect (cons 2 i))))
    (multiple-value-bind (rout rinout)
        (occ:fragment rin '())
      (declare (ignore rout))
      (occ:synchronize)

      ;; Create physical groups for each fracture
      (loop for r in rin
            for rm in rinout
            when (/= (cdr r) 1)
            do (format t "fracture ~A -> surfaces ~A~%" (cdr r) rm)
               (gmsh:add-physical-group 2 (mapcar #'cdr rm)
                                        :name (format nil "fracture~A" (cdr r)))))))

(opt:set-number "Mesh.MeshSizeMin" 0.05)
(opt:set-number "Mesh.MeshSizeMax" 0.05)
(mesh:generate)
