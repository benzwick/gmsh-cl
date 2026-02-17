;;; hex.lisp — Hex mesh from extruded geometry

(gmsh:add "hex")

(opt:set-number "Mesh.MshFileVersion" 2.2)

(let* ((n 4)
       (p (geo:point 0 0 0))
       (l (geo:extrude (list (cons 0 p)) 1 0 0
                       :num-elements (list n) :heights '(1)))
       (s (geo:extrude (list (second l)) 0 1 0
                       :num-elements (list n) :heights '(1) :recombine t))
       (v (geo:extrude (list (second s)) 0 0 1
                       :num-elements (list n) :heights '(1) :recombine t)))
  (geo:synchronize)
  (gmsh:add-physical-group 3 (list (cdr (second v)))))

(mesh:generate :dim 3)
;; (gmsh:write "/tmp/hex.msh")
