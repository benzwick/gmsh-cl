;;; volume.lisp — Computing mesh volume using the MeshVolume plugin

(let ((s (occ:rectangle 0 0 0 3 2)))
  (occ:synchronize)

  (let ((m (occ:get-mass 2 s)))
    (format t "mass from occ = ~A~%" m))

  (let ((p (gmsh:add-physical-group 2 (list s))))
    (mesh:generate :dim 2)

    (plugin:set-number "MeshVolume" "Dimension" 2)
    (plugin:set-number "MeshVolume" "PhysicalGroup" p)
    (let ((t-view (plugin:run "MeshVolume")))

      (multiple-value-bind (data-types num-elements data)
          (view:get-list-data t-view)
        (declare (ignore data-types num-elements))
        (format t "volume from mesh = ~A~%" (nth 3 (first data)))))))
