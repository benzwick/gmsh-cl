;;; crack.lisp — Square with cracks using the Crack plugin

(gmsh:add "square with cracks")

(let ((surf1 1))
  (occ:rectangle 0 0 0 1 1 :tag surf1)

  (let* ((pt1 (occ:point 0.2 0.2 0))
         (pt2 (occ:point 0.4 0.4 0))
         (line1 (occ:line pt1 pt2))
         (pt3 (occ:point 0.4 0.4 0))
         (pt4 (occ:point 0.4 0.9 0))
         (line2 (occ:line pt3 pt4)))

    (multiple-value-bind (o m)
        (occ:fragment (list (cons 2 surf1))
                      (list (cons 1 line1) (cons 1 line2)))
      (declare (ignore o))
      (occ:synchronize)

      ;; m contains, for each input entity, the child entities after fragmentation
      (let ((new-surf (cdr (first (first m))))
            (new-lines (loop for sublist in (rest m)
                             append (mapcar #'cdr sublist))))
        (gmsh:add-physical-group 2 (list new-surf) :tag 100)
        (gmsh:add-physical-group 1 new-lines :tag 101)))))

(mesh:generate :dim 2)

(plugin:set-number "Crack" "Dimension" 1)
(plugin:set-number "Crack" "PhysicalGroup" 101)
(plugin:set-number "Crack" "DebugView" 1)
(plugin:run "Crack")

(opt:set-number "Mesh.SaveAll" 1)
;; (gmsh:write "/tmp/crack.msh")
