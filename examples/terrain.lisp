;;; terrain.lisp — Create a terrain surface from simulated data points

(gmsh:add "terrain")

;; Create the terrain surface from N x N input data points
(let* ((n 100)
       (coords '())
       (nodes '())
       (tris '())
       (lin (list nil nil nil nil)))

  (labels ((tag (i j) (+ (* (1+ n) i) j 1)))

    (loop for i to n do
      (loop for j to n do
        (push (tag i j) nodes)
        (setf coords (nconc coords (list (/ (coerce i 'double-float) n)
                                         (/ (coerce j 'double-float) n)
                                         (* 0.05 (sin (* 10 (/ (coerce (+ i j) 'double-float) n)))))))
        (when (and (> i 0) (> j 0))
          (setf tris (nconc tris (list (tag (1- i) (1- j)) (tag i (1- j)) (tag (1- i) j))))
          (setf tris (nconc tris (list (tag i (1- j)) (tag i j) (tag (1- i) j)))))
        (when (and (or (= i 0) (= i n)) (> j 0))
          (let ((idx (if (= i 0) 3 1)))
            (setf (nth idx lin) (nconc (nth idx lin)
                                       (list (tag i (1- j)) (tag i j))))))
        (when (and (or (= j 0) (= j n)) (> i 0))
          (let ((idx (if (= j 0) 0 2)))
            (setf (nth idx lin) (nconc (nth idx lin)
                                       (list (tag (1- i) j) (tag i j))))))))

    (setf nodes (nreverse nodes))
    (let ((pnt (list (tag 0 0) (tag n 0) (tag n n) (tag 0 n))))

      ;; Create 4 corner points
      (geo:point 0 0 (nth (1- (* 3 (tag 0 0))) coords) :tag 1)
      (geo:point 1 0 (nth (1- (* 3 (tag n 0))) coords) :tag 2)
      (geo:point 1 1 (nth (1- (* 3 (tag n n))) coords) :tag 3)
      (geo:point 0 1 (nth (1- (* 3 (tag 0 n))) coords) :tag 4)
      (geo:synchronize)

      ;; Create 4 discrete bounding curves
      (loop for i below 4 do
        (gmsh:add-discrete-entity 1 :tag (1+ i)
                                  :boundary (list (1+ i) (if (< i 3) (+ i 2) 1))))

      ;; Create one discrete surface
      (gmsh:add-discrete-entity 2 :tag 1 :boundary '(1 2 -3 -4))

      ;; Add all the nodes on the surface
      (mesh:add-nodes 2 1 nodes coords)

      ;; Add elements on the 4 points, the 4 curves and the surface
      (loop for i below 4 do
        (mesh:add-elements-by-type (1+ i) 15 '() (list (nth i pnt)))
        (mesh:add-elements-by-type (1+ i) 1 '() (nth i lin)))
      (mesh:add-elements-by-type 1 2 '() tris)

      ;; Reclassify the nodes on the curves and the points
      (mesh:reclassify-nodes)

      ;; Create geometry for the discrete curves and surfaces
      (mesh:create-geometry)

      ;; Create other CAD entities to form volumes below/above the terrain
      (let ((p1 (geo:point 0 0 -0.5))
            (p2 (geo:point 1 0 -0.5))
            (p3 (geo:point 1 1 -0.5))
            (p4 (geo:point 0 1 -0.5))
            (p5 (geo:point 0 0 0.5))
            (p6 (geo:point 1 0 0.5))
            (p7 (geo:point 1 1 0.5))
            (p8 (geo:point 0 1 0.5)))

        (let ((c1 (geo:line p1 p2)) (c2 (geo:line p2 p3))
              (c3 (geo:line p3 p4)) (c4 (geo:line p4 p1))
              (c5 (geo:line p5 p6)) (c6 (geo:line p6 p7))
              (c7 (geo:line p7 p8)) (c8 (geo:line p8 p5))
              (c10 (geo:line p1 1)) (c11 (geo:line p2 2))
              (c12 (geo:line p3 3)) (c13 (geo:line p4 4))
              (c14 (geo:line 1 p5)) (c15 (geo:line 2 p6))
              (c16 (geo:line 3 p7)) (c17 (geo:line 4 p8)))

          ;; Bottom and top
          (let* ((ll1 (geo:curve-loop (list c1 c2 c3 c4)))
                 (s1 (geo:plane-surface (list ll1)))
                 (ll2 (geo:curve-loop (list c5 c6 c7 c8)))
                 (s2 (geo:plane-surface (list ll2))))

            ;; Lower sides
            (let ((s3 (geo:plane-surface (list (geo:curve-loop (list c1 c11 -1 (- c10))))))
                  (s4 (geo:plane-surface (list (geo:curve-loop (list c2 c12 -2 (- c11))))))
                  (s5 (geo:plane-surface (list (geo:curve-loop (list c3 c13 3 (- c12))))))
                  (s6 (geo:plane-surface (list (geo:curve-loop (list c4 c10 4 (- c13)))))))

              (let ((sl1 (geo:surface-loop (list s1 s3 s4 s5 s6 1))))
                (geo:volume (list sl1)))

              ;; Upper sides
              (let ((s7 (geo:plane-surface (list (geo:curve-loop (list c5 (- c15) -1 c14)))))
                    (s8 (geo:plane-surface (list (geo:curve-loop (list c6 (- c16) -2 c15)))))
                    (s9 (geo:plane-surface (list (geo:curve-loop (list c7 (- c17) 3 c16)))))
                    (s10 (geo:plane-surface (list (geo:curve-loop (list c8 (- c14) 4 c17))))))

                (let ((sl2 (geo:surface-loop (list s2 s7 s8 s9 s10 1))))
                  (geo:volume (list sl2))))))))

      (geo:synchronize)

      (opt:set-number "Mesh.MeshSizeMin" 0.05)
      (opt:set-number "Mesh.MeshSizeMax" 0.05))))
