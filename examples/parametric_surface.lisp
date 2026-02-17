;;; parametric_surface.lisp — Parametric surface geometry in the built-in kernel

;; This shows how alternative non-plane geometries can be used in the
;; built-in kernel; here using a parametric surface
(let ((g (geo:geometry "ParametricSurface" :strings '("u" "v" "v^2"))))

  (let ((p1 (geo:point-on-geometry g 0 0 :tag 1))
        (p2 (geo:point-on-geometry g 1 0 :tag 2))
        (p3 (geo:point-on-geometry g 1 1 :tag 3))
        (p4 (geo:point-on-geometry g 0 1 :tag 4)))

    (let ((l1 (geo:line p1 p2))
          (l2 (geo:line p2 p3))
          (l3 (geo:line p3 p4))
          (l4 (geo:line p4 p1)))

      (let ((cl (geo:curve-loop (list l1 l2 l3 l4))))
        (geo:plane-surface (list cl))))))

(geo:synchronize)
