;;; thrusections.lisp — Lofting volumes and surfaces through curve loops
;;;
;;; Demonstrates occ:thru-sections for creating volumes and surfaces through
;;; a series of closed curve loops, both smooth and ruled.
;;; Port of boolean/thrusections.geo

(gmsh:add "thrusections")

(opt:set-number "Mesh.Algorithm" 6)
(opt:set-number "Mesh.MeshSizeMin" 0.1)
(opt:set-number "Mesh.MeshSizeMax" 0.1)

;; Build volumes through closed curve loops
(loop for i from 0 to 1
      for make-ruled = (= i 1)
      do (let* ((c1 (occ:circle (+ i 0.0) 0 0 0.2))
                (c2 (occ:circle (+ i 0.1) 0.04 1 0.3))
                (c3 (occ:circle (+ i 0.03) -0.08 2 0.25))
                (cl1 (occ:curve-loop (list c1)))
                (cl2 (occ:curve-loop (list c2)))
                (cl3 (occ:curve-loop (list c3))))
           (occ:thru-sections (list cl1 cl2 cl3) :make-ruled make-ruled)))

;; Build surfaces through closed curve loops (not solid)
(loop for i from 0 to 1
      for make-ruled = (= i 1)
      do (let* ((c1 (occ:circle (+ i 2.0) 0 0 0.2))
                (c2 (occ:circle (+ i 2.1) 0.04 1 0.3))
                (c3 (occ:circle (+ i 2.03) -0.08 2 0.25))
                (cl1 (occ:curve-loop (list c1)))
                (cl2 (occ:curve-loop (list c2)))
                (cl3 (occ:curve-loop (list c3))))
           (occ:thru-sections (list cl1 cl2 cl3)
                              :make-solid nil :make-ruled make-ruled)))

;; Build surfaces through open wires (partial arcs)
(loop for i from 0 to 1
      for make-ruled = (= i 1)
      do (let* ((c1 (occ:circle (+ i 4.0) 0 0 0.2 :angle2 (/ pi 3)))
                (c2 (occ:circle (+ i 4.1) 0.04 1 0.3 :angle2 (/ pi 2)))
                (c3 (occ:circle (+ i 4.03) -0.08 2 0.25 :angle2 (/ pi 3)))
                (w1 (occ:wire (list c1)))
                (w2 (occ:wire (list c2)))
                (w3 (occ:wire (list c3))))
           (occ:thru-sections (list w1 w2 w3)
                              :make-solid nil :make-ruled make-ruled)))

(occ:synchronize)
(mesh:generate :dim 2)
;; (gmsh:write "/tmp/thrusections.msh")
