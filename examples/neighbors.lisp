;;; neighbors.lisp — Compute tet neighbors by face

(gmsh:add "my test model")
(occ:box 0 0 0 1 1 1)
(occ:synchronize)
(mesh:generate :dim 3)

(format t "--- getting tets and face nodes~%")
(multiple-value-bind (tets)
    (mesh:get-elements-by-type 4)
  (let ((fnodes (mesh:get-element-face-nodes 4 3)))

    (format t "--- computing face x tet incidence~%")
    (let ((faces '())
          (fxt (make-hash-table :test 'equal)))
      (loop for i from 0 below (length fnodes) by 3
            for face = (sort (list (nth i fnodes) (nth (1+ i) fnodes) (nth (+ i 2) fnodes)) #'<)
            for tet = (nth (floor i 12) tets)
            do (push face faces)
               (if (gethash face fxt)
                   (push tet (gethash face fxt))
                   (setf (gethash face fxt) (list tet))))

      (format t "--- computing neighbors by face~%")
      (let ((txt (make-hash-table)))
        (loop for i from 0 below (length faces)
              for face = (nth (- (length faces) 1 i) faces)  ; faces were pushed in reverse
              for tet = (nth (floor i 4) tets)
              do (unless (gethash tet txt)
                   (setf (gethash tet txt) (make-hash-table)))
                 (dolist (tt (gethash face fxt))
                   (unless (= tt tet)
                     (setf (gethash tt (gethash tet txt)) t))))

        (format t "--- done: ~A tets with neighbors computed~%"
                (hash-table-count txt))))))
