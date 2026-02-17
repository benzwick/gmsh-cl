;;; crack3d.lisp — 3D crack using the Crack plugin

;; Create 2 adjacent boxes + a smaller surface on the interface
(let ((v1 (occ:box 0 0 0 1 1 1))
      (v2 (occ:box 0 0 -1 1 1 1))
      (s1 (occ:rectangle 0.25 0.25 0 0.5 0.5)))
  (declare (ignore v1 v2))

  ;; Fragment the model to make the geometry conformal
  (multiple-value-bind (out out-map)
      (occ:fragment (list (cons 2 s1))
                    '((3 . 1) (3 . 2)))
    (declare (ignore out))
    (occ:synchronize)

    ;; out-map[0] contains entities replacing the surface s1
    (let ((phys (gmsh:add-physical-group 2 (list (cdr (first (first out-map)))))))
      (mesh:generate :dim 3)

      ;; "Crack" the mesh by duplicating elements and nodes on the surface
      (plugin:set-number "Crack" "Dimension" 2)
      (plugin:set-number "Crack" "PhysicalGroup" phys)
      (plugin:set-number "Crack" "DebugView" 1)
      (plugin:run "Crack"))))

(opt:set-number "Mesh.SaveAll" 1)
;; (gmsh:write "/tmp/crack3d.msh")
