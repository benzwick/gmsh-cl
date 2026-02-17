;;; view_adaptive_to_mesh.lisp — Convert adaptive view to mesh

(let ((t2 (view:add "Second order quad")))
  ;; Coordinates of 4 quad nodes
  (let ((quad (list 0.0 1.0 1.0 0.0       ; x
                    -1.2 -1.2 -0.2 -0.2   ; y
                    0.0 0.0 0.0 0.0)))     ; z
    ;; 9 values to be interpolated by 2nd order basis functions
    (setf quad (nconc quad (list 1.0 1.0 1.0 1.0 3.0 3.0 3.0 3.0 -3.0)))

    ;; Interpolation matrices
    (view:set-interpolation-matrices
     t2 "Quadrangle" 9
     (list 0 0 0.25 0 0 -0.25 -0.25 0 0.25
           0 0 0.25 0 0 -0.25 0.25 0 -0.25
           0 0 0.25 0 0 0.25 0.25 0 0.25
           0 0 0.25 0 0 0.25 -0.25 0 -0.25
           0 0 -0.5 0.5 0 0.5 0 -0.5 0
           0 0.5 -0.5 0 0.5 0 -0.5 0 0
           0 0 -0.5 0.5 0 -0.5 0 0.5 0
           0 0.5 -0.5 0 -0.5 0 0.5 0 0
           1 -1 1 -1 0 0 0 0 0)
     (list 0 0 0  2 0 0  2 2 0  0 2 0
           1 0 0  2 1 0  1 2 0  0 1 0  1 1 0))

    (view:add-list-data t2 "SQ" 1 quad))

  ;; Adaptive visualization
  (view:option-set-number t2 "AdaptVisualizationGrid" 1)
  (view:option-set-number t2 "TargetError" 1e-2)
  (view:option-set-number t2 "MaxRecursionLevel" 6)

  ;; Get adaptive visualization data
  (multiple-value-bind (data-type num-elements data)
      (view:get-list-data t2 :return-adaptive t)

    ;; Create discrete surface
    (let ((surf (gmsh:add-discrete-entity 2))
          (n 1))

      ;; Create nodes and elements from adaptive data
      (loop for idx below (length data-type) do
        (when (string= (nth idx data-type) "SQ")
          (let ((coord '())
                (tags '())
                (ele '())
                (ne (nth idx num-elements))
                (d (nth idx data)))
            (loop for q below ne do
              (setf coord (nconc coord
                                  (list (nth (+ (* 16 q) 0) d)
                                        (nth (+ (* 16 q) 4) d)
                                        (nth (+ (* 16 q) 8) d))))
              (setf coord (nconc coord
                                  (list (nth (+ (* 16 q) 1) d)
                                        (nth (+ (* 16 q) 5) d)
                                        (nth (+ (* 16 q) 9) d))))
              (setf coord (nconc coord
                                  (list (nth (+ (* 16 q) 2) d)
                                        (nth (+ (* 16 q) 6) d)
                                        (nth (+ (* 16 q) 10) d))))
              (setf coord (nconc coord
                                  (list (nth (+ (* 16 q) 3) d)
                                        (nth (+ (* 16 q) 7) d)
                                        (nth (+ (* 16 q) 11) d))))
              (setf tags (nconc tags (list n (1+ n) (+ n 2) (+ n 3))))
              (setf ele (nconc ele (list n (1+ n) (+ n 2) (+ n 3))))
              (incf n 4))
            (mesh:add-nodes 2 1 tags coord)
            (mesh:add-elements-by-type surf 3 '() ele))))

      ;; Remove duplicate nodes
      (mesh:remove-duplicate-nodes)
      ;; (gmsh:write "test.msh")
      )))
