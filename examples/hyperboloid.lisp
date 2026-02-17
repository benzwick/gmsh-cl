;;; hyperboloid.lisp — Mathematical surfaces via revolution and ThruSections
;;;
;;; Demonstrates three methods to create hyperboloid surfaces:
;;; 1. Revolution of a hyperbola curve
;;; 2. Rotating a line with ThruSections
;;; 3. Extrusion of spline curves
;;; Port of boolean/hyperboloid.geo

(gmsh:add "hyperboloid")

(let ((n 20)
      (a 1)
      (c 2)
      (umin -1)
      (umax 1))

  ;; Method 1: One-sheet hyperboloid by revolving a hyperbola
  (let* ((x0 0)
         (pts (loop for i from 0 below n
                    for u = (+ umin (* (/ (- umax umin) (1- n)) i))
                    for x = (+ x0 (* a (sqrt (+ 1 (* u u)))))
                    for y = (* c u)
                    collect (occ:point x y 0)))
         (p-start (occ:point x0 (- c) 0))
         (p-end (occ:point x0 c 0))
         (l1 (occ:line p-start (first pts)))
         (spl (occ:spline pts))
         (l3 (occ:line (car (last pts)) p-end))
         (l4 (occ:line p-end p-start))
         (cl (occ:curve-loop (list l1 spl l3 l4)))
         (s (occ:plane-surface (list cl))))
    ;; Revolve the surface around the y-axis to create the hyperboloid
    (occ:revolve (list (cons 2 s)) 0 0 0 0 1 0 (* 2 pi))
    ;; Delete construction surfaces, keep volumes
    (let ((surfs (gmsh:tags-of (occ:get-entities :dim 2))))
      (occ:remove (mapcar (lambda (s) (cons 2 s)) surfs) :recursive nil)))

  ;; Method 2: Rotate a line and use ThruSections
  (let* ((x0 3)
         (p1 (occ:point (- x0 (* (sqrt 2) a)) (- c) 0))
         (p2 (occ:point x0 c (- (* (sqrt 2) a))))
         (base-line (occ:line p1 p2))
         (wires (list (occ:wire (list base-line)))))
    ;; Create rotated copies of the line as wires
    (loop for i from 1 below n
          for angle = (* i (/ (* 2 pi) (1- n)))
          do (let ((copied (occ:copy (list (cons 1 base-line)))))
               (occ:rotate copied x0 0 0 0 1 0 angle)
               (push (occ:wire (list (cdr (first copied)))) wires)))
    (occ:thru-sections (nreverse wires) :make-solid nil))

  ;; Method 3: Two-sheet hyperboloid by revolution of sinh/cosh curves
  (let ((x0 6))
    (let* ((pts-top (loop for i from 0 below n
                          for u = (* (/ umax (1- n)) i)
                          for x = (+ x0 (* a (sinh u)))
                          for y = (* c (cosh u))
                          collect (occ:point x y 0)))
           (pts-bot (loop for i from 0 below n
                          for u = (* (/ umax (1- n)) i)
                          for x = (+ x0 (* a (sinh u)))
                          for y = (- (* c (cosh u)))
                          collect (occ:point x y 0)))
           (spl-top (occ:spline pts-top))
           (spl-bot (occ:spline pts-bot)))
      (occ:revolve (list (cons 1 spl-top) (cons 1 spl-bot))
                   x0 0 0 0 1 0 (* 2 pi)))))

(occ:synchronize)
(opt:set-number "Mesh.MeshSizeFromCurvature" 20)
(mesh:generate :dim 2)
;; (gmsh:write "/tmp/hyperboloid.msh")
