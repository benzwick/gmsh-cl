;;; reparam_on_face.lisp — Reparametrize curves on a surface

(occ:sphere 0 0 0 1 :tag 10)
(occ:box 0.5 0 0 1.3 2 4 :tag 11)
(occ:fragment '((3 . 10)) '((3 . 11)))
(occ:synchronize)

(let ((dim 2)
      (tag 6)
      (n 20))

  (let ((bnd (gmsh:get-boundary (list (cons dim tag)) :combined nil)))
    (dolist (c bnd)
      (format t "~A~%" c)
      (multiple-value-bind (min-bounds max-bounds)
          (gmsh:get-parametrization-bounds (car c) (abs (cdr c)))
        (let* ((t-vals (loop for i below n
                             collect (+ (first min-bounds)
                                        (* i (/ (- (first max-bounds)
                                                    (first min-bounds))
                                                n)))))
               (uv (gmsh:reparametrize-on-surface 1 (abs (cdr c)) t-vals tag))
               (xyz (gmsh:get-value dim tag uv)))
          (loop for i from 0 below (length xyz) by 3
                do (let ((p (gmsh:add-discrete-entity 0)))
                     (gmsh:set-coordinates p
                                           (nth i xyz)
                                           (nth (1+ i) xyz)
                                           (nth (+ i 2) xyz)))))))))
