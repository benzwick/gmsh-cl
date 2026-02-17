;;; bspline_filling.lisp — BSpline surface filling 4 boundary curves

(let ((lc 2e-2)
      (a 0.25))

  ;; South B-Spline curve
  (let ((p1 (occ:point 0.00 0.00 0.00 :mesh-size lc))
        (p2 (occ:point 0.33 (+ 0.00 a) (+ 0.00 a) :mesh-size lc))
        (p3 (occ:point 0.66 (- 0.00 a) (+ 0.00 a) :mesh-size lc))
        (p4 (occ:point 1.00 0.00 0.00 :mesh-size lc)))
    (let ((c1 (occ:b-spline (list p1 p2 p3 p4) :degree 3)))

      ;; North B-Spline curve
      (let ((p5 (occ:point 0.00 1.00 0.00 :mesh-size lc))
            (p6 (occ:point 0.33 (- 1.00 a) (- 0.00 a) :mesh-size lc))
            (p7 (occ:point 0.66 (+ 1.00 a) (- 0.00 a) :mesh-size lc))
            (p8 (occ:point 1.00 1.00 0.00 :mesh-size lc)))
        (let ((c2 (occ:b-spline (list p5 p6 p7 p8) :degree 3)))

          ;; East B-Spline curve
          (let ((p9 (occ:point (- 0.00 a) 0.50 (+ 0.00 a) :mesh-size lc)))
            (let ((c3 (occ:b-spline (list p1 p9 p5) :degree 2)))

              ;; West B-Spline curve
              (let ((p10 (occ:point (+ 1.00 a) 0.50 (- 0.00 a) :mesh-size lc)))
                (let ((c4 (occ:b-spline (list p4 p10 p8) :degree 3)))

                  ;; Create a BSpline surface filling the 4 curves
                  (let ((w1 (occ:wire (list c1 c3 c2 c4))))
                    (occ:b-spline-filling w1 :type "Curved")))))))))))

(occ:synchronize)
