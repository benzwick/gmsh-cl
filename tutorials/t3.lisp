;;; t3.lisp — Extruded meshes, ONELAB parameters, options

;; Let us now change some options... Since all interactive options are accessible
;; through the API, we can for example make point tags visible or redefine some
;; colors:
(opt:set-number "Geometry.PointNumbers" 1)
(opt:set-color "Geometry.Color.Points" 255 165 0)
(opt:set-color "General.Color.Text" 255 255 255)
(opt:set-color "Mesh.Color.Points" 255 0 0)

;; Note that for conciseness "Color." can be omitted in color options:
(multiple-value-bind (r g b a) (opt:get-color "Geometry.Points")
  (opt:set-color "Geometry.Surfaces" r g b :a a))

;; We create a ONELAB parameter to define the angle of the twist. ONELAB
;; parameters can be modified interactively in the GUI, and can be exchanged with
;; other codes connected to the same ONELAB database. The database can be
;; accessed through the Gmsh API using JSON-formatted strings (see
;; https://gitlab.onelab.info/doc/tutorials/-/wikis/ONELAB-JSON-interface for
;; more information):
(onelab:set "[
  {
    \"type\":\"number\",
    \"name\":\"Parameters/Twisting angle\",
    \"values\":[90],
    \"min\":0,
    \"max\":120,
    \"step\":1
  }
]")

;; Clear all models and create a new one
(gmsh:clear)
(gmsh:add "t3")

;; Copied from t1.lisp...
(let ((lc 1e-2))
  (geo:point 0 0 0 :mesh-size lc :tag 1)
  (geo:point 0.1 0 0 :mesh-size lc :tag 2)
  (geo:point 0.1 0.3 0 :mesh-size lc :tag 3)
  (geo:point 0 0.3 0 :mesh-size lc :tag 4))

(geo:line 1 2 :tag 1)
(geo:line 3 2 :tag 2)
(geo:line 3 4 :tag 3)
(geo:line 4 1 :tag 4)

(geo:curve-loop '(4 1 -2 3) :tag 1)
(geo:plane-surface '(1) :tag 1)

(geo:synchronize)

(gmsh:add-physical-group 1 '(1 2 4) :tag 5)
(gmsh:add-physical-group 2 '(1) :name "My surface")

;; As in t2, we plan to perform an extrusion along the z axis. But here,
;; instead of only extruding the geometry, we also want to extrude the 2D
;; mesh. This is done with the same extrude function, but by specifying
;; element 'Layers' (2 layers in this case, the first one with 8 subdivisions
;; and the second one with 2 subdivisions, both with a height of h/2). The
;; number of elements for each layer and the (end) height of each layer are
;; specified in two vectors:
(let* ((h 0.1)
       (ov (geo:extrude '((2 . 1)) 0 0 h
                         :num-elements '(8 2)
                         :heights '(0.5 1.0)))

       ;; The extrusion can also be performed with a rotation instead of a
       ;; translation, and the resulting mesh can be recombined into prisms (we
       ;; use only one layer here, with 7 subdivisions). All rotations are
       ;; specified by an axis point (-0.1, 0, 0.1), an axis direction (0, 1, 0),
       ;; and a rotation angle (-Pi/2):
       (ov2 (geo:revolve '((2 . 28)) -0.1 0 0.1 0 1 0 (- (/ pi 2))
                          :num-elements '(7)))

       ;; Using the built-in geometry kernel, only rotations with angles < Pi are
       ;; supported. To do a full turn, you will thus need to apply at least 3
       ;; rotations. The OpenCASCADE geometry kernel does not have this limitation.

       ;; A translation (-2 * h, 0, 0) and a rotation ((0, 0.15, 0.25), (1, 0, 0),
       ;; angle * Pi / 180) can also be combined to form a "twist". The last
       ;; (optional) argument for the extrude and twist functions specifies whether
       ;; the extruded mesh should be recombined or not. The angle parameter is
       ;; retrieved from the ONELAB database (it can be set interactively in the
       ;; GUI):
       (angle (first (onelab:get-number "Parameters/Twisting angle")))
       (ov3 (geo:twist '((2 . 50)) 0 0.15 0.25 (* -2 h) 0 0 1 0 0
                        (* angle (/ pi 180.0))
                        :num-elements '(10)
                        :recombine t)))

  (declare (ignore ov2 ov3))

  (geo:synchronize)

  ;; All the extrusion functions return a list of extruded entities: the "top"
  ;; of the extruded surface (in element 0), the newly created volume (in
  ;; element 1) and the tags of the lateral surfaces (in elements 2, 3, ...).

  ;; We can then define a new physical volume (with tag 101) to group all the
  ;; elementary volumes:
  (gmsh:add-physical-group 3 (list 1 2 (cdr (nth 1 ov))) :tag 101))

(mesh:generate :dim 3)
;; (gmsh:write "/tmp/t3.msh")
