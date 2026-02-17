;;; t10.lisp — Mesh size fields

;; In addition to specifying target mesh sizes at the points of the geometry (see
;; t1.lisp) or using a background mesh (see t7.lisp), you can use general mesh
;; size "Fields".

(gmsh:add "t10")

;; Let's create a simple rectangular geometry:
(let ((lc 0.15))
  (geo:point 0.0 0.0 0 :mesh-size lc :tag 1)
  (geo:point 1 0.0 0 :mesh-size lc :tag 2)
  (geo:point 1 1 0 :mesh-size lc :tag 3)
  (geo:point 0 1 0 :mesh-size lc :tag 4)
  (geo:point 0.2 0.5 0 :mesh-size lc :tag 5)

  (geo:line 1 2 :tag 1)
  (geo:line 2 3 :tag 2)
  (geo:line 3 4 :tag 3)
  (geo:line 4 1 :tag 4)

  (geo:curve-loop '(1 2 3 4) :tag 5)
  (geo:plane-surface '(5) :tag 6)

  (geo:synchronize)

  ;; Say we would like to obtain mesh elements with size lc/30 near curve 2 and
  ;; point 5, and size lc elsewhere. To achieve this, we can use two fields:
  ;; "Distance", and "Threshold". We first define a Distance field (Field[1]) on
  ;; points 5 and on curve 2. This field returns the distance to point 5 and to
  ;; (100 equidistant points on) curve 2.
  (mesh:field-add "Distance" :tag 1)
  (mesh:field-set-numbers 1 "PointsList" '(5))
  (mesh:field-set-numbers 1 "CurvesList" '(2))
  (mesh:field-set-number 1 "Sampling" 100)

  ;; We then define a Threshold field, which uses the return value of the
  ;; Distance field 1 in order to define a simple change in element size
  ;; depending on the computed distances
  ;;
  ;; SizeMax -                     /------------------
  ;;                              /
  ;;                             /
  ;;                            /
  ;; SizeMin -o----------------/
  ;;          |                |    |
  ;;        Point         DistMin  DistMax
  (mesh:field-add "Threshold" :tag 2)
  (mesh:field-set-number 2 "InField" 1)
  (mesh:field-set-number 2 "SizeMin" (/ lc 30))
  (mesh:field-set-number 2 "SizeMax" lc)
  (mesh:field-set-number 2 "DistMin" 0.15)
  (mesh:field-set-number 2 "DistMax" 0.5)

  ;; Say we want to modulate the mesh element sizes using a mathematical function
  ;; of the spatial coordinates. We can do this with the MathEval field:
  (mesh:field-add "MathEval" :tag 3)
  (mesh:field-set-string 3 "F"
                         "cos(4*3.14*x) * sin(4*3.14*y) / 10 + 0.101")

  ;; We could also combine MathEval with values coming from other fields. For
  ;; example, let's define a Distance field around point 1
  (mesh:field-add "Distance" :tag 4)
  (mesh:field-set-numbers 4 "PointsList" '(1))

  ;; We can then create a MathEval field with a function that depends on the
  ;; return value of the Distance field 4, i.e., depending on the distance to
  ;; point 1 (here using a cubic law, with minimum element size = lc / 100)
  (mesh:field-add "MathEval" :tag 5)
  (mesh:field-set-string 5 "F" (format nil "F4^3 + ~A" (/ lc 100)))

  ;; We could also use a Box field to impose a step change in element sizes
  ;; inside a box
  (mesh:field-add "Box" :tag 6)
  (mesh:field-set-number 6 "VIn" (/ lc 15))
  (mesh:field-set-number 6 "VOut" lc)
  (mesh:field-set-number 6 "XMin" 0.3)
  (mesh:field-set-number 6 "XMax" 0.6)
  (mesh:field-set-number 6 "YMin" 0.3)
  (mesh:field-set-number 6 "YMax" 0.6)
  (mesh:field-set-number 6 "Thickness" 0.3))

;; Many other types of fields are available: see the reference manual for a
;; complete list. You can also create fields directly in the graphical user
;; interface by selecting Define->Size fields in the Mesh module.

;; Let's use the minimum of all the fields as the mesh size field:
(mesh:field-add "Min" :tag 7)
(mesh:field-set-numbers 7 "FieldsList" '(2 3 5 6))

(mesh:field-set-as-background-mesh 7)

;; The API also allows to set a global mesh size callback, which is called each
;; time the mesh size is queried
(cffi:defcallback t10-size-callback :double ((dim :int) (tag :int)
                                             (x :double) (y :double)
                                             (z :double) (lc :double))
  (declare (ignore dim tag y z))
  (min lc (+ (* 0.02d0 x) 0.01d0)))
(mesh:set-size-callback (cffi:callback t10-size-callback))

;; To determine the size of mesh elements, Gmsh locally computes the minimum of
;;
;; 1) the size of the model bounding box;
;; 2) if Mesh.MeshSizeFromPoints is set, the mesh size specified at geometrical
;;    points;
;; 3) if Mesh.MeshSizeFromCurvature is positive, the mesh size based on
;;    curvature (the value specifying the number of elements per 2 * pi rad);
;; 4) the background mesh size field;
;; 5) any per-entity mesh size constraint;
;;
;; The value can then be further modified by the mesh size callback, if any,
;; before being constrained in the interval [Mesh.MeshSizeMin,
;; Mesh.MeshSizeMax] and multiplied by Mesh.MeshSizeFactor. In addition,
;; boundary mesh sizes are interpolated inside surfaces and/or volumes depending
;; on the value of Mesh.MeshSizeExtendFromBoundary (which is set by default).
;;
;; When the element size is fully specified by a mesh size field (as it is in
;; this example), it is thus often desirable to set

(opt:set-number "Mesh.MeshSizeExtendFromBoundary" 0)
(opt:set-number "Mesh.MeshSizeFromPoints" 0)
(opt:set-number "Mesh.MeshSizeFromCurvature" 0)

;; This will prevent over-refinement due to small mesh sizes on the boundary.

;; Finally, while the default "Frontal-Delaunay" 2D meshing algorithm
;; (Mesh.Algorithm = 6) usually leads to the highest quality meshes, the
;; "Delaunay" algorithm (Mesh.Algorithm = 5) will handle complex mesh size fields
;; better - in particular size fields with large element size gradients:

(opt:set-number "Mesh.Algorithm" 5)

(mesh:generate :dim 2)
;; (gmsh:write "/tmp/t10.msh")
