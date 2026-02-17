;;; t2.lisp — Transformations, extruded geometries, volumes

(gmsh:add "t2")

;; Copied from t1.lisp...
(let ((lc 1e-2))
  (geo:point 0 0 0 :mesh-size lc :tag 1)
  (geo:point 0.1 0 0 :mesh-size lc :tag 2)
  (geo:point 0.1 0.3 0 :mesh-size lc :tag 3)
  (geo:point 0 0.3 0 :mesh-size lc :tag 4)
  (geo:line 1 2 :tag 1)
  (geo:line 3 2 :tag 2)
  (geo:line 3 4 :tag 3)
  (geo:line 4 1 :tag 4)
  (geo:curve-loop '(4 1 -2 3) :tag 1)
  (geo:plane-surface '(1) :tag 1)
  (geo:synchronize)
  (gmsh:add-physical-group 1 '(1 2 4) :tag 5)
  (gmsh:add-physical-group 2 '(1) :name "My surface")

  ;; We can then add new points and curves in the same way as we did in t1.lisp:
  (geo:point 0 0.4 0 :mesh-size lc :tag 5)
  (geo:line 4 5 :tag 5)

  ;; But Gmsh also provides tools to transform (translate, rotate, etc.)
  ;; elementary entities or copies of elementary entities.  Geometrical
  ;; transformations take a list of (dim . tag) pairs as first argument,
  ;; which contains the list of entities.  For example, the point 5
  ;; (dimension=0, tag=5) can be moved by 0.02 to the left
  ;; (dx=-0.02, dy=0, dz=0) with
  (geo:translate '((0 . 5)) -0.02 0 0)

  ;; And it can be further rotated by -Pi/4 around (0, 0.3, 0) (with the
  ;; rotation along the z axis) with:
  (geo:rotate '((0 . 5)) 0 0.3 0 0 0 1 (- (/ pi 4)))

  ;; Note that there are no units in Gmsh: coordinates are just numbers -
  ;; it's up to the user to associate a meaning to them.

  ;; Point 3 can be duplicated and translated by 0.05 along the y axis by
  ;; using the copy() function, which takes a list of (dim . tag) pairs
  ;; as input, and returns another list of (dim . tag) pairs:
  (let* ((ov (geo:copy '((0 . 3))))
         (_ (geo:translate ov 0 0.05 0))
         ;; The new point tag is available as (cdr (first ov)), and can be
         ;; used to create new lines:
         (new-point (cdr (first ov))))
    (declare (ignore _))
    (geo:line 3 new-point :tag 7)
    (geo:line new-point 5 :tag 8)
    (geo:curve-loop '(5 -8 -7 3) :tag 10)
    (geo:plane-surface '(10) :tag 11)

    ;; In the same way, we can translate copies of the two surfaces 1 and 11
    ;; to the right with the following command:
    (let ((ov (geo:copy '((2 . 1) (2 . 11)))))
      (geo:translate ov 0.12 0 0)

      (format t "New surfaces ~a and ~a~%"
              (cdr (first ov)) (cdr (second ov)))

      ;; Volumes are the fourth type of elementary entities in Gmsh. In the
      ;; same way one defines curve loops to build surfaces, one has to
      ;; define surface loops (i.e. 'shells') to build volumes. The following
      ;; volume does not have holes and thus consists of a single surface loop:
      (geo:point 0.0 0.3 0.12 :mesh-size lc :tag 100)
      (geo:point 0.1 0.3 0.12 :mesh-size lc :tag 101)
      (geo:point 0.1 0.35 0.12 :mesh-size lc :tag 102)

      ;; We would like to retrieve the coordinates of point 5 to create
      ;; point 103, so we synchronize the model, and use getValue()
      (geo:synchronize)
      (let ((xyz (gmsh:get-value 0 5 '())))
        (geo:point (first xyz) (second xyz) 0.12 :mesh-size lc :tag 103))

      (geo:line 4 100 :tag 110)
      (geo:line 3 101 :tag 111)
      (geo:line new-point 102 :tag 112)
      (geo:line 5 103 :tag 113)
      (geo:line 103 100 :tag 114)
      (geo:line 100 101 :tag 115)
      (geo:line 101 102 :tag 116)
      (geo:line 102 103 :tag 117)

      (geo:curve-loop '(115 -111 3 110) :tag 118)
      (geo:plane-surface '(118) :tag 119)
      (geo:curve-loop '(111 116 -112 -7) :tag 120)
      (geo:plane-surface '(120) :tag 121)
      (geo:curve-loop '(112 117 -113 -8) :tag 122)
      (geo:plane-surface '(122) :tag 123)
      (geo:curve-loop '(114 -110 5 113) :tag 124)
      (geo:plane-surface '(124) :tag 125)
      (geo:curve-loop '(115 116 117 114) :tag 126)
      (geo:plane-surface '(126) :tag 127)

      (geo:surface-loop '(127 119 121 123 125 11) :tag 128)
      (geo:volume '(128) :tag 129)

      ;; When a volume can be extruded from a surface, it is usually easier
      ;; to use the extrude() function directly instead of creating all the
      ;; points, curves and surfaces by hand. For example, the following
      ;; command extrudes the surface 11 along the z axis and automatically
      ;; creates a new volume (as well as all the needed points, curves and
      ;; surfaces). As expected, the function takes a list of (dim . tag)
      ;; pairs as input as well as the translation vector, and returns a
      ;; list of (dim . tag) pairs as output:
      (geo:extrude (list (second ov)) 0 0 0.12)

      ;; Mesh sizes associated to geometrical points can be set by passing
      ;; a list of (dim . tag) pairs for the corresponding points:
      (geo:mesh-set-size '((0 . 103) (0 . 105) (0 . 109) (0 . 102)
                           (0 . 28) (0 . 24) (0 . 6) (0 . 5))
                         (* lc 3))

      ;; We finish by synchronizing the data from the built-in CAD kernel
      ;; with the Gmsh model:
      (geo:synchronize)

      ;; We group volumes 129 and 130 in a single physical group with
      ;; tag 1 and name "The volume":
      (gmsh:add-physical-group 3 '(129 130) :tag 1 :name "The volume")

      ;; We finally generate the mesh:
      (mesh:generate :dim 3)
      ;; (gmsh:write "/tmp/t2.msh")
      )))
