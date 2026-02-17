;;; t4.lisp — Holes in surfaces, annotations, entity colors

(gmsh:add "t4")

(let* ((cm 1e-02)
       (e1 (* 4.5 cm))
       (e2 (/ (* 6 cm) 2))
       (e3 (/ (* 5 cm) 2))
       (h1 (* 5 cm))
       (h2 (* 10 cm))
       (h3 (* 5 cm))
       (h4 (* 2 cm))
       (h5 (* 4.5 cm))
       (r1 (* 1 cm))
       (r2 (* 1.5 cm))
       (r (* 1 cm))
       (lc1 0.01)
       (lc2 0.003)
       (ccos (/ (+ (* (- h5) r1) (* e2 (sqrt (+ (* h5 h5) (* e2 e2) (* r1 r1)))))
              (+ (* h5 h5) (* e2 e2))))
       (ssin (sqrt (- 1 (* ccos ccos)))))

  ;; We start by defining some points and some lines.
  (geo:point (- (- e1) e2) 0 0 :mesh-size lc1 :tag 1)
  (geo:point (- (- e1) e2) h1 0 :mesh-size lc1 :tag 2)
  (geo:point (- (- e3) r) h1 0 :mesh-size lc2 :tag 3)
  (geo:point (- (- e3) r) (+ h1 r) 0 :mesh-size lc2 :tag 4)
  (geo:point (- e3) (+ h1 r) 0 :mesh-size lc2 :tag 5)
  (geo:point (- e3) (+ h1 h2) 0 :mesh-size lc1 :tag 6)
  (geo:point e3 (+ h1 h2) 0 :mesh-size lc1 :tag 7)
  (geo:point e3 (+ h1 r) 0 :mesh-size lc2 :tag 8)
  (geo:point (+ e3 r) (+ h1 r) 0 :mesh-size lc2 :tag 9)
  (geo:point (+ e3 r) h1 0 :mesh-size lc2 :tag 10)
  (geo:point (+ e1 e2) h1 0 :mesh-size lc1 :tag 11)
  (geo:point (+ e1 e2) 0 0 :mesh-size lc1 :tag 12)
  (geo:point e2 0 0 :mesh-size lc1 :tag 13)

  (geo:point (/ r1 ssin) (+ h5 (* r1 ccos)) 0 :mesh-size lc2 :tag 14)
  (geo:point 0 h5 0 :mesh-size lc2 :tag 15)
  (geo:point (/ (- r1) ssin) (+ h5 (* r1 ccos)) 0 :mesh-size lc2 :tag 16)
  (geo:point (- e2) 0.0 0 :mesh-size lc1 :tag 17)

  (geo:point (- r2) (+ h1 h3) 0 :mesh-size lc2 :tag 18)
  (geo:point (- r2) (+ h1 h3 h4) 0 :mesh-size lc2 :tag 19)
  (geo:point 0 (+ h1 h3 h4) 0 :mesh-size lc2 :tag 20)
  (geo:point r2 (+ h1 h3 h4) 0 :mesh-size lc2 :tag 21)
  (geo:point r2 (+ h1 h3) 0 :mesh-size lc2 :tag 22)
  (geo:point 0 (+ h1 h3) 0 :mesh-size lc2 :tag 23)

  (geo:point 0 (+ h1 h3 h4 r2) 0 :mesh-size lc2 :tag 24)
  (geo:point 0 (- (+ h1 h3) r2) 0 :mesh-size lc2 :tag 25))

(geo:line 1 17 :tag 1)
(geo:line 17 16 :tag 2)

;; Gmsh provides other curve primitives than straight lines: splines, B-splines,
;; circle arcs, ellipse arcs, etc. Here we define a new circle arc, starting at
;; point 14 and ending at point 16, with the circle's center being the point 15:
(geo:circle-arc 14 15 16 :tag 3)

;; Note that, in Gmsh, circle arcs should always be smaller than Pi. The
;; OpenCASCADE geometry kernel does not have this limitation.

;; We can then define additional lines and circles, as well as a new surface:
(geo:line 14 13 :tag 4)
(geo:line 13 12 :tag 5)
(geo:line 12 11 :tag 6)
(geo:line 11 10 :tag 7)
(geo:circle-arc 8 9 10 :tag 8)
(geo:line 8 7 :tag 9)
(geo:line 7 6 :tag 10)
(geo:line 6 5 :tag 11)
(geo:circle-arc 3 4 5 :tag 12)
(geo:line 3 2 :tag 13)
(geo:line 2 1 :tag 14)
(geo:line 18 19 :tag 15)
(geo:circle-arc 21 20 24 :tag 16)
(geo:circle-arc 24 20 19 :tag 17)
(geo:circle-arc 18 23 25 :tag 18)
(geo:circle-arc 25 23 22 :tag 19)
(geo:line 21 22 :tag 20)

(geo:curve-loop '(17 -15 18 19 -20 16) :tag 21)
(geo:plane-surface '(21) :tag 22)

;; But we still need to define the exterior surface. Since this surface has a
;; hole, its definition now requires two curve loops:
(geo:curve-loop '(11 -12 13 14 1 2 -3 4 5 6 7 -8 9 10) :tag 23)
(geo:plane-surface '(23 21) :tag 24)

;; As a general rule, if a surface has N holes, it is defined by N+1 curve loops:
;; the first loop defines the exterior boundary; the other loops define the
;; boundaries of the holes.

(geo:synchronize)

;; Finally, we can add some comments by creating a post-processing view
;; containing some strings:
(let ((v (view:add "comments")))

  ;; Add a text string in window coordinates, 10 pixels from the left and 10
  ;; pixels from the bottom:
  (view:add-list-data-string v '(10 -10) '("Created with Gmsh"))

  ;; Add a text string in model coordinates centered at (X,Y,Z) = (0, 0.11, 0),
  ;; with some style attributes:
  (view:add-list-data-string v '(0 0.11 0) '("Hole")
                             :style '("Align" "Center" "Font" "Helvetica"))

  ;; If a string starts with `file://', the rest is interpreted as an image
  ;; file. For 3D annotations, the size in model coordinates can be specified
  ;; after a `@' symbol in the form `widthxheight' (if one of `width' or
  ;; `height' is zero, natural scaling is used; if both are zero, original image
  ;; dimensions in pixels are used):
  (let ((png (namestring
              (merge-pathnames "../t4_image.png"
                               (asdf:system-relative-pathname :gmsh-cl "tutorials/")))))
    (view:add-list-data-string v '(0 0.09 0)
                               (list (format nil "file://~A@0.01x0" png))
                               :style '("Align" "Center"))

    ;; The 3D orientation of the image can be specified by providing the direction
    ;; of the bottom and left edge of the image in model space:
    (view:add-list-data-string v '(-0.01 0.09 0)
                               (list (format nil "file://~A@0.01x0,0,0,1,0,1,0" png)))

    ;; The image can also be drawn in "billboard" mode, i.e. always parallel to
    ;; the camera, by using the `#' symbol:
    (view:add-list-data-string v '(0 0.12 0)
                               (list (format nil "file://~A@0.01x0#" png))
                               :style '("Align" "Center"))

    ;; The size of 2D annotations is given directly in pixels:
    (view:add-list-data-string v '(150 -7)
                               (list (format nil "file://~A@20x0" png))))

  ;; These annotations are handled by a list-based post-processing view. For
  ;; large post-processing datasets, that contain actual field values defined on
  ;; a mesh, you should use model-based post-processing views instead, which
  ;; allow to efficiently store continuous or discontinuous scalar, vector and
  ;; tensor fields, or arbitrary polynomial order.

  ;; Views and geometrical entities can be made to respond to double-click
  ;; events, here to print some messages to the console:
  (view:option-set-string v "DoubleClickedCommand"
                          "Printf('View[0] has been double-clicked!');")
  (opt:set-string "Geometry.DoubleClickedLineCommand"
                  "Printf('Curve %g has been double-clicked!', Geometry.DoubleClickedEntityTag);"))

;; We can also change the color of some entities:
(gmsh:set-color '((2 . 22)) 127 127 127)  ; Gray50
(gmsh:set-color '((2 . 24)) 160 32 240)   ; Purple
(gmsh:set-color (loop for i from 1 to 14 collect (cons 1 i)) 255 0 0)    ; Red
(gmsh:set-color (loop for i from 15 to 20 collect (cons 1 i)) 255 255 0) ; Yellow

(mesh:generate :dim 2)
;; (gmsh:write "/tmp/t4.msh")
