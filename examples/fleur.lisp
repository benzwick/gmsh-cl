;;; fleur.lisp — Flower pattern via rotational symmetry and Boolean ops
;;;
;;; Creates a flower-like pattern using rotated elliptical disks, Boolean
;;; union to merge petals, and Boolean difference to cut from a square.
;;; Demonstrates mesh sizing from curvature.
;;; Port of boolean/fleur.geo

(gmsh:add "fleur")

(let ((a 0.71)
      (b 0.145)
      (nr-petals 12)
      (l 4))

  ;; Create the outer square
  (occ:rectangle (/ l -2) (/ l -2) 0 l l :tag 1)

  ;; Create the first petal (an elliptical disk)
  (occ:disk a 0 0 a b :tag 2)

  ;; Create rotated copies of the petal
  (let ((petals (list (cons 2 2))))
    (loop for tt from 1 below nr-petals
          for angle = (* (/ (* 2 pi) nr-petals) tt)
          do (let ((copies (occ:copy '((2 . 2)))))
               (occ:rotate copies 0 0 0 0 0 1 angle)
               (push (first copies) petals)))
    (setf petals (nreverse petals))

    ;; Boolean union of all petals into one flower shape
    (occ:fuse (list (first petals)) (rest petals) :tag 100)

    ;; Boolean difference: cut the flower from the square
    (occ:cut '((2 . 1)) '((2 . 100)) :tag 200)))

(occ:synchronize)

(gmsh:add-physical-group 2 '(200) :tag 1000)

;; Use mesh size from curvature for automatic refinement near curves
(opt:set-number "Mesh.MeshSizeFromCurvature" 15)
(opt:set-number "Mesh.MeshSizeMax" 0.1)

(mesh:generate :dim 2)
;; (gmsh:write "/tmp/fleur.msh")
