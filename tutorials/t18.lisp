;;; t18.lisp — Periodic meshes

;; Periodic meshing constraints can be imposed on surfaces and curves.

(gmsh:add "t18")

;; Let's use the OpenCASCADE geometry kernel to build two geometries.

;; The first geometry is very simple: a unit cube with a non-uniform mesh size
;; constraint (set on purpose to be able to verify visually that the periodicity
;; constraint works!):

(occ:box 0 0 0 1 1 1 :tag 1)
(occ:synchronize)

(mesh:set-size (gmsh:get-entities :dim 0) 0.1)
(mesh:set-size '((0 . 1)) 0.02)

;; To impose that the mesh on surface 2 (the right side of the cube) should
;; match the mesh from surface 1 (the left side), the following periodicity
;; constraint is set:
(defparameter *translation* '(1 0 0 1 0 1 0 0 0 0 1 0 0 0 0 1))
(mesh:set-periodic 2 '(2) '(1) *translation*)

;; The periodicity transform is provided as a 4x4 affine transformation matrix,
;; given by row.

;; During mesh generation, the mesh on surface 2 will be created by copying
;; the mesh from surface 1.

;; Multiple periodicities can be imposed in the same way:
(mesh:set-periodic 2 '(6) '(5) '(1 0 0 0 0 1 0 0 0 0 1 1 0 0 0 1))
(mesh:set-periodic 2 '(4) '(3) '(1 0 0 0 0 1 0 1 0 0 1 0 0 0 0 1))

;; For more complicated cases, finding the corresponding surfaces by hand can
;; be tedious, especially when geometries are created through solid
;; modelling. Let's construct a slightly more complicated geometry.

;; We start with a cube and some spheres:
(occ:box 2 0 0 1 1 1 :tag 10)
(let ((x (- 2 0.3))
      (y 0)
      (z 0))
  (occ:sphere x y z 0.35 :tag 11)
  (occ:sphere (+ x 1) y z 0.35 :tag 12)
  (occ:sphere x (+ y 1) z 0.35 :tag 13)
  (occ:sphere x y (+ z 1) 0.35 :tag 14)
  (occ:sphere (+ x 1) (+ y 1) z 0.35 :tag 15)
  (occ:sphere x (+ y 1) (+ z 1) 0.35 :tag 16)
  (occ:sphere (+ x 1) y (+ z 1) 0.35 :tag 17)
  (occ:sphere (+ x 1) (+ y 1) (+ z 1) 0.35 :tag 18))

;; We first fragment all the volumes, which will leave parts of spheres
;; protruding outside the cube:
(let ((out (occ:fragment '((3 . 10))
                         (loop for i from 11 to 18
                               collect (cons 3 i)))))
  (occ:synchronize)

  ;; Ask OpenCASCADE to compute more accurate bounding boxes of entities using
  ;; the STL mesh:
  (opt:set-number "Geometry.OCCBoundsUseStl" 1)

  ;; We then retrieve all the volumes in the bounding box of the original cube,
  ;; and delete all the parts outside it:
  (let* ((eps 1e-3)
         (vin (gmsh:get-entities-in-bounding-box (- 2 eps) (- eps) (- eps)
                                                 (+ 2 1 eps) (+ 1 eps) (+ 1 eps)
                                                 :dim 3))
         (outside (cl:remove-if (lambda (v) (member v vin :test #'equal)) out)))
    (gmsh:remove-entities outside :recursive t)

    ;; We now set a non-uniform mesh size constraint (again to check results
    ;; visually):
    (let ((p (gmsh:get-boundary vin :combined nil :oriented nil :recursive t)))
      (mesh:set-size p 0.1))
    (let ((p (gmsh:get-entities-in-bounding-box (- 2 eps) (- eps) (- eps)
                                                (+ 2 eps) eps eps
                                                :dim 0)))
      (mesh:set-size p 0.001))

    ;; We now identify corresponding surfaces on the left and right sides of the
    ;; geometry automatically.

    ;; First we get all surfaces on the left:
    (let ((sxmin (gmsh:get-entities-in-bounding-box (- 2 eps) (- eps) (- eps)
                                                    (+ 2 eps) (+ 1 eps) (+ 1 eps)
                                                    :dim 2)))
      (dolist (i sxmin)
        ;; Then we get the bounding box of each left surface
        (multiple-value-bind (xmin ymin zmin xmax ymax zmax)
            (gmsh:get-bounding-box (car i) (cdr i))
          ;; We translate the bounding box to the right and look for surfaces
          ;; inside it:
          (let ((sxmax (gmsh:get-entities-in-bounding-box
                        (+ xmin (- eps) 1) (- ymin eps) (- zmin eps)
                        (+ xmax eps 1) (+ ymax eps) (+ zmax eps)
                        :dim 2)))
            ;; For all the matches, we compare the corresponding bounding
            ;; boxes...
            (dolist (j sxmax)
              (multiple-value-bind (xmin2 ymin2 zmin2 xmax2 ymax2 zmax2)
                  (gmsh:get-bounding-box (car j) (cdr j))
                (let ((xmin2 (- xmin2 1))
                      (xmax2 (- xmax2 1)))
                  ;; ...and if they match, we apply the periodicity constraint
                  (when (and (< (abs (- xmin2 xmin)) eps)
                             (< (abs (- xmax2 xmax)) eps)
                             (< (abs (- ymin2 ymin)) eps)
                             (< (abs (- ymax2 ymax)) eps)
                             (< (abs (- zmin2 zmin)) eps)
                             (< (abs (- zmax2 zmax)) eps))
                    (mesh:set-periodic 2 (list (cdr j)) (list (cdr i))
                                       *translation*)))))))))))

(mesh:generate :dim 3)
;; (gmsh:write "/tmp/t18.msh")
