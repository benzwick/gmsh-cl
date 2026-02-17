;;; t16.lisp — Constructive Solid Geometry, OpenCASCADE geometry kernel
;;;
;;; Demonstrates CSG with OCC: boolean cut/fragment, conformal meshing,
;;; physical group assignment, and boundary identification via
;;; get-closest-entities. Compare with t5.lisp (same geometry, built-in kernel).
;;;
;;; Key API: occ:box, occ:sphere, occ:cut, occ:fragment, gmsh:get-boundary,
;;; occ:get-closest-entities
;;;
;;; Equivalent Python: gmsh/tutorials/python/t16.py

;; Instead of constructing a model in a bottom-up fashion with Gmsh's built-in
;; geometry kernel, starting with version 3 Gmsh allows you to directly use
;; alternative geometry kernels. Here we will use the OpenCASCADE kernel.

(gmsh:add "t16")

;; Let's build the same model as in t5.lisp, but using constructive solid
;; geometry.

;; We can log all messages for further processing with:
(logger:start)

;; We first create two cubes:
(occ:box 0 0 0 1 1 1 :tag 1)
(occ:box 0 0 0 0.5 0.5 0.5 :tag 2)

;; We apply a boolean difference to create the "cube minus one eighth" shape:
;; gmsh:volume-tags builds the required dim-tag pairs from tag lists
(occ:cut (gmsh:volume-tags '(1)) (gmsh:volume-tags '(2)) :tag 3)

;; Boolean operations with OpenCASCADE always create new entities. By default
;; the extra arguments remove-object and remove-tool in cut() are set to t,
;; which will delete the original entities.

;; We then create the five spheres:
(let ((x 0)
      (y 0.75)
      (z 0)
      (r 0.09)
      (holes '()))
  (loop for tt from 1 to 5
        do (incf x 0.166)
           (incf z 0.166)
           (occ:sphere x y z r :tag (+ 3 tt))
           (push (gmsh:volume-tag (+ 3 tt)) holes))
  (setf holes (nreverse holes))

  ;; If we had wanted five empty holes we would have used cut() again. Here we
  ;; want five spherical inclusions, whose mesh should be conformal with the
  ;; mesh of the cube: we thus use fragment(), which intersects all volumes in
  ;; a conformal manner (without creating duplicate interfaces):
  (multiple-value-bind (ov ovv) (occ:fragment (gmsh:volume-tags '(3)) holes)

    ;; ov contains all the generated entities of the same dimension as the
    ;; input entities:
    (format t "fragment produced volumes:~%")
    (loop for e in ov do (format t "~A~%" e))

    ;; ovv contains the parent-child relationships for all the input entities:
    (format t "before/after fragment relations:~%")
    (loop for parent in (cons '(3 . 3) holes)
          for child in ovv
          do (format t "parent ~A -> child ~A~%" parent child))

    (occ:synchronize)

    ;; When the boolean operation leads to simple modifications of entities,
    ;; and if one deletes the original entities, Gmsh tries to assign the same
    ;; tag to the new entities. (This behavior is governed by the
    ;; Geometry.OCCBooleanPreserveNumbering option.)

    ;; Here the Physical Volume definitions can thus be made for the 5 spheres
    ;; directly, as the five spheres (volumes 4, 5, 6, 7 and 8), which will be
    ;; deleted by the fragment operations, will be recreated identically (albeit
    ;; with new surfaces) with the same tags:
    (loop for i from 1 to 5
          do (gmsh:add-physical-group 3 (list (+ 3 i)) :tag i))

    ;; The tag of the cube will change though, so we need to access it
    ;; programmatically — gmsh:tag extracts the tag from a dim-tag pair:
    (gmsh:add-physical-group 3 (list (gmsh:tag (first ov))) :tag 10)

    ;; Creating entities using constructive solid geometry is very powerful,
    ;; but can lead to practical issues for e.g. setting mesh sizes at points,
    ;; or identifying boundaries.

    ;; To identify points or other bounding entities you can take advantage of
    ;; the get-entities, get-boundary, get-closest-entities and
    ;; get-entities-in-bounding-box functions:

    ;; Define a physical surface for the top and right-most surfaces, by
    ;; finding amongst the surfaces making up the boundary of the model, the
    ;; two closest to point (1, 1, 0.5):
    (let* ((bnd (gmsh:get-boundary (gmsh:get-entities :dim 3)))
           (closest (occ:get-closest-entities 1 1 0.5 bnd :n 2)))
      ;; gmsh:tags-of extracts all tags from a list of dim-tag pairs
      (gmsh:add-physical-group 2 (gmsh:tags-of closest)
                               :tag 100
                               :name "Top & right surfaces"))

    ;; Assign a mesh size to all the points:
    (let ((lcar1 0.1)
          (lcar2 0.0005)
          (lcar3 0.055))
      (mesh:set-size (gmsh:get-entities :dim 0) lcar1)

      ;; Override this constraint on the points of the five spheres:
      (mesh:set-size (gmsh:get-boundary holes :combined nil :oriented nil :recursive t)
                     lcar3)

      ;; Select the corner point by searching for it geometrically using a
      ;; bounding box (get-closest-entities could have been used as well):
      (let* ((eps 1e-3)
             (ov2 (gmsh:get-entities-in-bounding-box
                   (- 0.5 eps) (- 0.5 eps) (- 0.5 eps)
                   (+ 0.5 eps) (+ 0.5 eps) (+ 0.5 eps) :dim 0)))
        (mesh:set-size ov2 lcar2)))))

(mesh:generate :dim 3)
;; (gmsh:write "/tmp/t16.msh")

;; Additional examples created with the OpenCASCADE geometry kernel are
;; available in t18.lisp, t19.lisp and t20.lisp, as well as in the
;; examples/api directory.

;; Inspect the log:
(let ((log (logger:get)))
  (format t "Logger has recorded ~A lines~%" (length log)))
(logger:stop)
