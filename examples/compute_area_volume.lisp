;;; compute_area_volume.lisp — Compute mesh area/volume via Plugin(Integrate)
;;;
;;; Creates a sphere, meshes it, and uses the Integrate plugin to compute
;;; total surface area and volume.

(gmsh:add "compute_area_volume")

;; Create a sphere and mesh it in 3D
(occ:sphere 0 0 0 1 :tag 1)
(occ:synchronize)

(mesh:generate :dim 3)

;; Create a view with constant 1.0 on all elements to integrate area/volume
;; We'll use element data on all 2D elements for surface area
(let* ((surfs (gmsh:get-entities :dim 2))
       (v-area (view:add "constant_surface"))
       (v-vol (view:add "constant_volume")))

  ;; Add constant 1.0 data on all triangular surface elements
  (dolist (ent surfs)
    (let ((s (cdr ent)))
      (multiple-value-bind (elem-tags node-tags)
          (mesh:get-elements-by-type 2 :tag s)
        (declare (ignore node-tags))
        (when (> (length elem-tags) 0)
          (let ((ones (make-list (length elem-tags) :initial-element 1.0d0)))
            (view:add-model-data v-area 0 "compute_area_volume" "ElementData"
                                 elem-tags
                                 (mapcar #'list ones)))))))

  ;; Add constant 1.0 data on all tetrahedral volume elements
  (let ((vols (gmsh:get-entities :dim 3)))
    (dolist (ent vols)
      (let ((v (cdr ent)))
        (multiple-value-bind (elem-tags node-tags)
            (mesh:get-elements-by-type 4 :tag v)
          (declare (ignore node-tags))
          (when (> (length elem-tags) 0)
            (let ((ones (make-list (length elem-tags) :initial-element 1.0d0)))
              (view:add-model-data v-vol 0 "compute_area_volume" "ElementData"
                                   elem-tags
                                   (mapcar #'list ones))))))))

  ;; Run the Integrate plugin on the surface view
  (plugin:set-number "Integrate" "View" (view:get-index v-area))
  (plugin:run "Integrate")
  (format t "Surface integration view created (should approximate 4*pi for unit sphere)~%")

  ;; Run the Integrate plugin on the volume view
  (plugin:set-number "Integrate" "View" (view:get-index v-vol))
  (plugin:run "Integrate")
  (format t "Volume integration view created (should approximate 4/3*pi for unit sphere)~%"))

;; (gmsh:write "/tmp/compute_area_volume.msh")
