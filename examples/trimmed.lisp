;;; trimmed.lisp — Trimmed surfaces from a sphere

(let ((v0 (occ:sphere 0 0 0 2)))

  ;; Create wires in the parametric plane of the spherical surface
  (let* ((c1 (occ:circle 0 0 0 0.4))
         (w1 (occ:wire (list c1)))
         (c2 (occ:circle 0 0 0 0.2))
         (w2 (occ:wire (list c2)))
         (s3 (occ:rectangle 0 0.5 0 5 0.5)))

    (occ:synchronize)

    (let* ((b3 (gmsh:get-boundary (list (cons 2 s3))))
           (w3 (occ:wire (mapcar #'cdr b3))))

      ;; Get spherical surface
      (let ((s0 (cdr (first (gmsh:get-boundary (list (cons 3 v0)))))))

        ;; Create trimmed surfaces from the spherical surface
        (occ:trimmed-surface s0 :wire-tags (list w1 w2))
        (occ:trimmed-surface s0 :wire-tags (list w3))

        ;; Remove the sphere
        (occ:remove (list (cons 3 v0)) :recursive t)))))

(occ:synchronize)
