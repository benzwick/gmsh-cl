;;; x2.lisp — Mesh import, discrete entities, hybrid models, terrain meshing

;; The API can be used to import a mesh without reading it from a file, by
;; creating nodes and elements on the fly and storing them in model entities.
;; These model entities can be existing CAD entities, or can be discrete
;; entities, entirely defined by the mesh.
;;
;; We combine all these features in this tutorial to perform terrain meshing,
;; where the terrain is described by a discrete surface (that we then
;; reparametrize) combined with a CAD representation of the underground.

(gmsh:add "x2")

;; We will create the terrain surface mesh from N x N input data points:
(let* ((n 100)

       ;; Helper function to return a node tag given two indices i and j:
       (tag-fn (lambda (i j) (+ (* (1+ n) i) j 1)))

       ;; The x, y, z coordinates of all the nodes:
       (coords '())

       ;; The tags of the corresponding nodes:
       (nodes '())

       ;; The connectivities of the triangle elements (3 node tags per triangle)
       ;; on the terrain surface:
       (tris '())

       ;; The connectivities of the line elements on the 4 boundaries (2 node
       ;; tags for each line element):
       (lin (vector '() '() '() '()))

       ;; The connectivities of the point elements on the 4 corners (1 node tag
       ;; for each point element):
       (pnt (list (funcall tag-fn 0 0)
                  (funcall tag-fn n 0)
                  (funcall tag-fn n n)
                  (funcall tag-fn 0 n))))

  (loop for i from 0 to n do
    (loop for j from 0 to n do
      (push (funcall tag-fn i j) nodes)
      (let ((x (/ (float i) n))
            (y (/ (float j) n))
            (z (* 0.05 (sin (* 10 (/ (float (+ i j)) n))))))
        (setf coords (nconc coords (list x y z))))
      (when (and (> i 0) (> j 0))
        (setf tris (nconc tris (list (funcall tag-fn (1- i) (1- j))
                                     (funcall tag-fn i (1- j))
                                     (funcall tag-fn (1- i) j))))
        (setf tris (nconc tris (list (funcall tag-fn i (1- j))
                                     (funcall tag-fn i j)
                                     (funcall tag-fn (1- i) j)))))
      (when (and (or (= i 0) (= i n)) (> j 0))
        (let ((idx (if (= i 0) 3 1)))
          (setf (aref lin idx)
                (nconc (aref lin idx)
                       (list (funcall tag-fn i (1- j))
                             (funcall tag-fn i j))))))
      (when (and (or (= j 0) (= j n)) (> i 0))
        (let ((idx (if (= j 0) 0 2)))
          (setf (aref lin idx)
                (nconc (aref lin idx)
                       (list (funcall tag-fn (1- i) j)
                             (funcall tag-fn i j))))))))

  (setf nodes (nreverse nodes))

  ;; Create 4 discrete points for the 4 corners of the terrain surface:
  (loop for i below 4 do
    (gmsh:add-discrete-entity 0 :tag (1+ i)))
  (gmsh:set-coordinates 1 0 0 (nth (1- (* 3 (funcall tag-fn 0 0))) coords))
  (gmsh:set-coordinates 2 1 0 (nth (1- (* 3 (funcall tag-fn n 0))) coords))
  (gmsh:set-coordinates 3 1 1 (nth (1- (* 3 (funcall tag-fn n n))) coords))
  (gmsh:set-coordinates 4 0 1 (nth (1- (* 3 (funcall tag-fn 0 n))) coords))

  ;; Create 4 discrete bounding curves, with their boundary points:
  (loop for i below 4 do
    (gmsh:add-discrete-entity 1 :tag (1+ i)
                                :boundary (list (1+ i) (if (< i 3) (+ i 2) 1))))

  ;; Create one discrete surface, with its bounding curves:
  (gmsh:add-discrete-entity 2 :tag 1 :boundary '(1 2 -3 -4))

  ;; Add all the nodes on the surface (for simplicity... see below):
  (mesh:add-nodes 2 1 nodes coords)

  ;; Add point elements on the 4 points, line elements on the 4 curves, and
  ;; triangle elements on the surface:
  (loop for i below 4 do
    ;; Type 15 for point elements:
    (mesh:add-elements-by-type (1+ i) 15 '() (list (nth i pnt)))
    ;; Type 1 for 2-node line elements:
    (mesh:add-elements-by-type (1+ i) 1 '() (aref lin i)))
  ;; Type 2 for 3-node triangle elements:
  (mesh:add-elements-by-type 1 2 '() tris)

  ;; Reclassify the nodes on the curves and the points (since we put them all on
  ;; the surface before with add-nodes for simplicity)
  (mesh:reclassify-nodes)

  ;; Create a geometry for the discrete curves and surfaces, so that we can
  ;; remesh them later on:
  (mesh:create-geometry)

  ;; Create other built-in CAD entities to form one volume below the terrain
  ;; surface. Beware that only built-in CAD entities can be hybrid, i.e. have
  ;; discrete entities on their boundary: OpenCASCADE does not support this
  ;; feature.
  (let* ((p1 (geo:point 0 0 -0.5))
         (p2 (geo:point 1 0 -0.5))
         (p3 (geo:point 1 1 -0.5))
         (p4 (geo:point 0 1 -0.5))
         (c1 (geo:line p1 p2))
         (c2 (geo:line p2 p3))
         (c3 (geo:line p3 p4))
         (c4 (geo:line p4 p1))
         (c10 (geo:line p1 1))
         (c11 (geo:line p2 2))
         (c12 (geo:line p3 3))
         (c13 (geo:line p4 4))
         (ll1 (geo:curve-loop (list c1 c2 c3 c4)))
         (s1 (geo:plane-surface (list ll1)))
         (ll3 (geo:curve-loop (list c1 c11 -1 (- c10))))
         (s3 (geo:plane-surface (list ll3)))
         (ll4 (geo:curve-loop (list c2 c12 -2 (- c11))))
         (s4 (geo:plane-surface (list ll4)))
         (ll5 (geo:curve-loop (list c3 c13 3 (- c12))))
         (s5 (geo:plane-surface (list ll5)))
         (ll6 (geo:curve-loop (list c4 c10 4 (- c13))))
         (s6 (geo:plane-surface (list ll6)))
         (sl1 (geo:surface-loop (list s1 s3 s4 s5 s6 1)))
         (v1 (geo:volume (list sl1))))
    (declare (ignore v1))
    (geo:synchronize)

    ;; Set this to t to build a fully hex mesh:
    (let ((transfinite nil)
          (transfinite-auto nil))
      (cond
        (transfinite
         (let ((nn 30))
           (dolist (c (gmsh:get-entities :dim 1))
             (mesh:set-transfinite-curve (cdr c) nn))
           (dolist (s (gmsh:get-entities :dim 2))
             (mesh:set-transfinite-surface (cdr s))
             (mesh:set-recombine (car s) (cdr s))
             (mesh:set-smoothing (car s) (cdr s) 100))
           (mesh:set-transfinite-volume (cdr (first (gmsh:get-entities :dim 3))))))
        (transfinite-auto
         (opt:set-number "Mesh.MeshSizeMin" 0.5)
         (opt:set-number "Mesh.MeshSizeMax" 0.5)
         ;; set-transfinite-automatic uses the sizing constraints to set the
         ;; number of points
         (mesh:set-transfinite-automatic))
        (t
         (opt:set-number "Mesh.MeshSizeMin" 0.05)
         (opt:set-number "Mesh.MeshSizeMax" 0.05)))))

  (mesh:generate :dim 3)
  ;; (gmsh:write "/tmp/x2.msh")
  )
