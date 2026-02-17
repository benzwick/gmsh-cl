;;; x3.lisp — Post-processing data import: list-based

;; Gmsh supports two types of post-processing data: "list-based" and
;; "model-based". Both types of data are handled through the `view' interface.

;; List-based views are completely independent from any model and any mesh: they
;; are self-contained and simply contain lists of coordinates and values, element
;; by element, for 3 types of fields (scalar "S", vector "V" and tensor "T") and
;; several types of element shapes (point "P", line "L", triangle "T", quadrangle
;; "Q", tetrahedron "S", hexahedron "H", prism "I" and pyramid "Y"). (See
;; x4.lisp for a tutorial on model-based views.)

;; To create a list-based view one should first create a view:
(let ((t1 (view:add "A list-based view")))

  ;; List-based data is then added by specifying the type as a 2 character string
  ;; that combines a field type and an element shape (e.g. "ST" for a scalar
  ;; field on triangles), the number of elements to be added, and the
  ;; concatenated list of coordinates and values for each element.

  ;; Let's create two triangles...
  (let ((triangle1 (list 0.0 1.0 1.0   ; x coordinates of the 3 triangle nodes
                         0.0 0.0 1.0   ; y coordinates of the 3 triangle nodes
                         0.0 0.0 0.0)) ; z coordinates of the 3 triangle nodes
        (triangle2 (list 0.0 1.0 0.0  0.0 1.0 1.0  0.0 0.0 0.0)))

    ;; ... and append values for 10 time steps
    (loop for step below 10 do
      (setf triangle1 (nconc triangle1 (list 10.0 (- 11.0 step) 12.0)))
      (setf triangle2 (nconc triangle2 (list 11.0 12.0 (+ 13.0 step)))))

    ;; List-based data is just added by concatenating the data for all the
    ;; triangles:
    (view:add-list-data t1 "ST" 2 (append triangle1 triangle2)))

  ;; Vector or tensor fields can be imported in the same way. For example a
  ;; vector field on a line element can be added as follows:
  (let ((line (list 0.0 1.0       ; x coordinate of the 2 line nodes
                    1.2 1.2       ; y coordinate of the 2 line nodes
                    0.0 0.0)))    ; z coordinate of the 2 line nodes
    (loop for step below 10 do
      ;; 3 vector components for each node (2 nodes here), for each step
      (setf line (nconc line (list (+ 10.0 step) 0.0 0.0
                                   (+ 10.0 step) 0.0 0.0))))
    (view:add-list-data t1 "VL" 1 line))

  ;; List-based data can also hold 2D (in window coordinates) and 3D (in model
  ;; coordinates) strings. Here we add a 2D string located on the bottom-left of
  ;; the window (with a 20 pixels offset), as well as a 3D string located at
  ;; model coordinates (0.5, 0.5, 0):
  (view:add-list-data-string t1 '(20.0 -20.0) '("Created with Gmsh"))
  (view:add-list-data-string t1 '(0.5 1.5 0.0)
                             '("A multi-step list-based view")
                             :style '("Align" "Center" "Font" "Helvetica"))

  ;; The various attributes of the view can be queried and changed using the
  ;; option interface:
  (view:option-set-number t1 "TimeStep" 5)
  (view:option-set-number t1 "IntervalsType" 3)
  (let ((ns (view:option-get-number t1 "NbTimeStep")))
    (format t "View ~A has ~A time steps~%" t1 (truncate ns)))

  ;; Views can be probed directly using view:probe -- here at point (0.9, 0.1, 0):
  (format t "Value at (0.9, 0.1, 0) ~A~%" (view:probe t1 0.9 0.1 0))

  ;; Views can be saved to disk using view:write:
  ;; (view:write t1 "/tmp/x3.pos")
  )

;; High-order datasets can be provided by setting the interpolation matrices
;; explicitly. Let's create a second view with second order interpolation on a
;; 4-node quadrangle.

;; Add a new view:
(let ((t2 (view:add "Second order quad")))

  ;; Set the node coordinates:
  (let ((quad (list 0.0 1.0 1.0 0.0           ; x coordinates of the 4 quad nodes
                    -1.2 -1.2 -0.2 -0.2       ; y coordinates of the 4 quad nodes
                    0.0 0.0 0.0 0.0)))         ; z coordinates of the 4 quad nodes

    ;; Add nine values that will be interpolated by second order basis functions
    (setf quad (nconc quad (list 1.0 1.0 1.0 1.0 3.0 3.0 3.0 3.0 -3.0)))

    ;; Set the two interpolation matrices c[i][j] and e[i][j] defining the d = 9
    ;; basis functions:
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
     (list 0 0 0
           2 0 0
           2 2 0
           0 2 0
           1 0 0
           2 1 0
           1 2 0
           0 1 0
           1 1 0))

    ;; Add the data to the view:
    (view:add-list-data t2 "SQ" 1 quad))

  ;; In order to visualize the high-order field, one must activate adaptive
  ;; visualization, set a visualization error threshold and a maximum subdivision
  ;; level:
  (view:option-set-number t2 "AdaptVisualizationGrid" 1)
  (view:option-set-number t2 "TargetError" 1e-2)
  (view:option-set-number t2 "MaxRecursionLevel" 5))
