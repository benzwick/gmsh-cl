;;; t20.lisp — STEP import and manipulation, geometry partitioning

;; The OpenCASCADE CAD kernel allows to import STEP files and to modify them. In
;; this tutorial we will load a STEP geometry and partition it into slices.

(gmsh:add "t20")

;; Load a STEP file (using `import-shapes' instead of `merge' allows to directly
;; retrieve the tags of the highest dimensional imported entities):
(let* ((step-path (namestring
                   (merge-pathnames "_reference/gmsh/tutorials/t20_data.step"
                                    (asdf:system-source-directory :gmsh-cl))))
       (v (occ:import-shapes step-path)))

  ;; If we had specified
  ;;
  ;;   (opt:set-string "Geometry.OCCTargetUnit" "M")
  ;;
  ;; before importing the STEP file, OpenCASCADE would have converted the units
  ;; to meters (instead of the default, which is millimeters).

  ;; Get the bounding box of the volume:
  (multiple-value-bind (xmin ymin zmin xmax ymax zmax)
      (occ:get-bounding-box (car (first v)) (cdr (first v)))

    ;; We want to slice the model into N slices, and either keep the volume
    ;; slices or just the surfaces obtained by the cutting:

    (let* ((n 5)            ; Number of slices
           (dir "X")        ; Direction: "X", "Y" or "Z"
           (surf nil)       ; Keep only surfaces?
           (dx (- xmax xmin))
           (dy (- ymax ymin))
           (dz (- zmax zmin))
           (big-l (if (string= dir "X") dz dx))
           (big-h (if (string= dir "Y") dz dy))
           (s '()))

      ;; Create the first cutting plane:
      (push (cons 2 (occ:rectangle xmin ymin zmin big-l big-h)) s)
      (cond
        ((string= dir "X")
         (occ:rotate (list (first s)) xmin ymin zmin 0 1 0 (/ (- pi) 2)))
        ((string= dir "Y")
         (occ:rotate (list (first s)) xmin ymin zmin 1 0 0 (/ pi 2))))
      (let ((tx (if (string= dir "X") (/ dx n) 0))
            (ty (if (string= dir "Y") (/ dy n) 0))
            (tz (if (string= dir "Z") (/ dz n) 0)))
        (occ:translate (list (first s)) tx ty tz)

        ;; Create the other cutting planes:
        (loop for i from 1 below (1- n)
              do (let ((new (occ:copy (list (first s)))))
                   (occ:translate new (* i tx) (* i ty) (* i tz))
                   (setf s (append s new))))

        ;; Fragment (i.e. intersect) the volume with all the cutting planes:
        (occ:fragment v s)

        ;; Now remove all the surfaces (and their bounding entities) that are not
        ;; on the boundary of a volume, i.e. the parts of the cutting planes that
        ;; "stick out" of the volume:
        (occ:remove (occ:get-entities :dim 2) :recursive t)

        (occ:synchronize)

        (when surf
          ;; If we want to only keep the surfaces, retrieve the surfaces in
          ;; bounding boxes around the cutting planes...
          (let ((eps 1e-4)
                (keep '()))
            (loop for i from 1 below n
                  do (let ((xx (if (string= dir "X") xmin xmax))
                           (yy (if (string= dir "Y") ymin ymax))
                           (zz (if (string= dir "Z") zmin zmax)))
                       (setf keep
                             (append keep
                                     (gmsh:get-entities-in-bounding-box
                                      (+ xmin (- eps) (* i tx))
                                      (+ ymin (- eps) (* i ty))
                                      (+ zmin (- eps) (* i tz))
                                      (+ xx eps (* i tx))
                                      (+ yy eps (* i ty))
                                      (+ zz eps (* i tz))
                                      :dim 2)))))
            ;; ...and remove all the other entities (here directly in the model,
            ;; as we won't modify any OpenCASCADE entities later on):
            (let ((dels (cl:remove-if (lambda (e) (member e keep :test #'equal))
                                      (gmsh:get-entities :dim 2))))
              (gmsh:remove-entities (gmsh:get-entities :dim 3))
              (gmsh:remove-entities dels)
              (gmsh:remove-entities (gmsh:get-entities :dim 1))
              (gmsh:remove-entities (gmsh:get-entities :dim 0)))))

        ;; Finally, let's specify a global mesh size and mesh the partitioned
        ;; model:
        (opt:set-number "Mesh.MeshSizeMin" 3)
        (opt:set-number "Mesh.MeshSizeMax" 3)
        (mesh:generate :dim 3)
        ;; (gmsh:write "/tmp/t20.msh")
        ))))
