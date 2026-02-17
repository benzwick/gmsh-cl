;;; t17.lisp — Anisotropic background mesh
;;;
;;; Demonstrates anisotropic meshing using a metric tensor field as a
;;; background mesh. Requires the BAMG algorithm (Mesh.Algorithm = 7).
;;;
;;; Key API: mesh:field-add "PostView", opt:set-number "Mesh.Algorithm" 7
;;;
;;; Equivalent Python: gmsh/tutorials/python/t17.py

;; As seen in t7.lisp, mesh sizes can be specified very accurately by providing
;; a background mesh, i.e., a post-processing view that contains the target
;; mesh sizes.

;; Here, the background mesh is represented as a metric tensor field defined on
;; a square. One should use bamg as 2d mesh generator to enable anisotropic
;; meshes in 2D.

(gmsh:add "t17")

;; Create a square
(occ:rectangle -2 -2 0 4 4)
(occ:synchronize)

;; Merge a post-processing view containing the target anisotropic mesh sizes
(gmsh:merge (namestring (merge-pathnames "_reference/gmsh/tutorials/t17_bgmesh.pos"
                                          (asdf:system-source-directory :gmsh-cl))))

;; Apply the view as the current background mesh
(let ((bg-field (mesh:field-add "PostView")))
  (mesh:field-set-number bg-field "ViewIndex" 0)
  (mesh:field-set-as-background-mesh bg-field))

;; Use bamg
(opt:set-number "Mesh.SmoothRatio" 3)
(opt:set-number "Mesh.AnisoMax" 1000)
(opt:set-number "Mesh.Algorithm" 7)

(mesh:generate :dim 2)
;; (gmsh:write "/tmp/t17.msh")
