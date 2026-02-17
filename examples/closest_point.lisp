;;; closest_point.lisp — Find closest point on a curve by projection

;; Add a circle
(let ((c (occ:circle 0 0 0 1)))
  (occ:synchronize)

  ;; Find closest point to (1.3, 1.3) by orthogonal projection on curve c
  (multiple-value-bind (p param)
      (gmsh:get-closest-point 1 c '(1.3 1.3 0))
    (declare (ignore param))

    ;; Add a point on the projection
    (let ((pp (occ:point (nth 0 p) (nth 1 p) (nth 2 p))))

      ;; Fragment the curve with the new point
      (occ:fragment (list (cons 0 pp)) (list (cons 1 c))))))

(occ:synchronize)
