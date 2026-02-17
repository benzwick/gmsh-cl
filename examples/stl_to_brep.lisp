;;; stl_to_brep.lisp — Convert a discrete mesh to BREP geometry
;;;
;;; Creates a simple discrete mesh (a triangulated box), then uses
;;; mesh:classify-surfaces and mesh:create-geometry to convert it
;;; to a BREP representation that can be remeshed.
;;; Inspired by stl_to_brep.py

(gmsh:add "stl_to_brep")

;; Create a simple box and mesh it
(occ:box 0 0 0 1 1 1 :tag 1)
(occ:synchronize)
(mesh:generate :dim 2)

;; Get the current surface mesh to use as a discrete starting point
;; Save and reload as STL to simulate importing an STL
(let ((tmpfile (namestring (merge-pathnames "stl_to_brep_tmp.stl"
                                            (uiop:temporary-directory)))))
  (gmsh:write tmpfile)

  ;; Start fresh with a new model
  (gmsh:clear)
  (gmsh:add "stl_to_brep_converted")

  ;; Merge the STL file
  (gmsh:merge tmpfile)

  ;; Classify the surface mesh to recover geometry
  ;; The angle threshold (in radians) controls how sharp edges are detected
  (mesh:classify-surfaces (* 40 (/ pi 180))  ; 40 degree angle threshold
                          :boundary t
                          :for-reparametrization t
                          :curve-angle (/ pi 3))

  ;; Create geometry from the classified mesh
  (mesh:create-geometry)

  ;; Now we can remesh with the recovered geometry
  (mesh:generate :dim 2)

  ;; Clean up temp file
  (ignore-errors (delete-file tmpfile)))

(format t "STL to BREP conversion complete~%")
;; (gmsh:write "/tmp/stl_to_brep.msh")
