;;; pipe.lisp — Pipe (sweep) along a helical spline

(let* ((nturns 2)
       (npts (* 100 nturns))
       (r 1.0d0)
       (rd 0.1d0)
       (h (* 1.0d0 nturns)))

  (loop for i below npts
        for theta = (* i 2.0d0 pi nturns (/ 1.0d0 npts))
        do (occ:point (* r (cos theta)) (* r (sin theta))
                      (* i (/ h npts))
                      :tag (1+ i)))

  (occ:spline (loop for i from 1 below npts collect i) :tag 1)
  (occ:wire '(1) :tag 1)

  (occ:disk 1 0 0 rd rd :tag 1)

  (occ:rectangle (+ 1 (* 2 rd)) (- rd) 0 (* 2 rd) (* 2 rd) :tag 2
                 :rounded-radius (/ rd 5))
  (occ:rotate '((2 . 1) (2 . 2)) 0 0 0 1 0 0 (/ pi 2))

  (occ:pipe '((2 . 1) (2 . 2)) 1 :trihedron "Frenet")

  (occ:remove '((2 . 1) (2 . 2) (1 . 1))))

(occ:synchronize)

(opt:set-number "Mesh.MeshSizeMin" 0.1)
(opt:set-number "Mesh.MeshSizeMax" 0.1)
