;;; select_elements.lisp — Interactive element selection
;;;
;;; Demonstrates fltk:select-elements for interactive mesh selection.
;;; Note: This example creates the mesh but does not start the GUI
;;; (would need fltk:run for interactive use).
;;; Inspired by select_elements.py

(gmsh:add "select_elements")

;; Create a simple geometry
(occ:box 0 0 0 1 1 1 :tag 1)
(occ:synchronize)

(mesh:generate :dim 3)

;; Report mesh statistics
(let ((entities-3d (gmsh:get-entities :dim 3))
      (entities-2d (gmsh:get-entities :dim 2)))
  (format t "3D entities: ~A~%" (length entities-3d))
  (format t "2D entities: ~A~%" (length entities-2d))

  ;; Count elements per entity
  (dolist (ent entities-2d)
    (let ((s (cdr ent)))
      (multiple-value-bind (elem-tags node-tags)
          (mesh:get-elements-by-type 2 :tag s)
        (declare (ignore node-tags))
        (format t "  Surface ~A: ~A triangles~%" s (length elem-tags)))))

  (dolist (ent entities-3d)
    (let ((v (cdr ent)))
      (multiple-value-bind (elem-tags node-tags)
          (mesh:get-elements-by-type 4 :tag v)
        (declare (ignore node-tags))
        (format t "  Volume ~A: ~A tetrahedra~%" v (length elem-tags))))))

;; To actually select elements interactively, uncomment:
;; (fltk:initialize)
;; (let ((selected (fltk:select-elements)))
;;   (format t "Selected ~A elements~%" (length selected)))
;; (fltk:run)

;; (gmsh:write "/tmp/select_elements.msh")
