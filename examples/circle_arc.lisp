;;; circle_arc.lisp — OCC circle arcs (center vs through-point)

(let ((beg (occ:point 0 0 0))
      (end (occ:point 1 1 0))
      (mid (occ:point 1 0 0)))
  ;; mid is center
  (occ:circle-arc beg mid end)
  ;; arc goes through mid
  (occ:circle-arc beg mid end :center nil))

(occ:synchronize)
