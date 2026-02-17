;;; fragment_surfaces.lisp — Surface fragmentation with parent-child tracking
;;;
;;; Demonstrates occ:fragment on surfaces, showing how to track the
;;; parent-child relationships between input and output entities.

(gmsh:add "fragment_surfaces")

;; Create overlapping disks
(occ:disk 0 0 0 1 1 :tag 1)
(occ:disk 0.8 0 0 1 1 :tag 2)
(occ:disk 0.4 0.7 0 1 1 :tag 3)

;; Fragment all surfaces to create a conformal decomposition
(multiple-value-bind (ov ovv) (occ:fragment '((2 . 1) (2 . 2)) '((2 . 3)))

  ;; ov contains all the generated surfaces
  (format t "fragment produced ~A surfaces:~%" (length ov))
  (loop for e in ov do (format t "  (~A . ~A)~%" (car e) (cdr e)))

  ;; ovv contains the parent-child relationships
  (format t "~%before/after fragment relations:~%")
  (loop for parent in '((2 . 1) (2 . 2) (2 . 3))
        for child in ovv
        do (format t "  parent ~A -> children ~A~%" parent child))

  (occ:synchronize)

  ;; Assign physical groups to each fragment
  (loop for i from 0 below (length ov)
        for dt = (nth i ov)
        do (gmsh:add-physical-group 2 (list (cdr dt)) :tag (1+ i)
                                    :name (format nil "Fragment ~A" (1+ i)))))

(mesh:generate :dim 2)
;; (gmsh:write "/tmp/fragment_surfaces.msh")
