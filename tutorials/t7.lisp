;;; t7.lisp — Background meshes

;; Mesh sizes can be specified very accurately by providing a background mesh,
;; i.e., a post-processing view that contains the target mesh sizes.

;; Merge a list-based post-processing view containing the target mesh sizes:
(let ((bg-mesh-path (namestring (merge-pathnames "_reference/gmsh/tutorials/t7_bgmesh.pos"
                                                 (asdf:system-source-directory :gmsh-cl)))))
  (gmsh:merge bg-mesh-path))

;; If the post-processing view was model-based instead of list-based (i.e. if it
;; was based on an actual mesh), we would need to create a new model to contain
;; the geometry so that meshing it does not destroy the background mesh. It's not
;; necessary here since the view is list-based, but it does no harm:
(gmsh:add "t7")

;; Create a simple rectangular geometry:
(let ((lc 1e-2))
  (geo:point 0 0 0 :mesh-size lc :tag 1)
  (geo:point 0.1 0 0 :mesh-size lc :tag 2)
  (geo:point 0.1 0.3 0 :mesh-size lc :tag 3)
  (geo:point 0 0.3 0 :mesh-size lc :tag 4))

(geo:line 1 2 :tag 1)
(geo:line 3 2 :tag 2)
(geo:line 3 4 :tag 3)
(geo:line 4 1 :tag 4)

(geo:curve-loop '(4 1 -2 3) :tag 1)
(geo:plane-surface '(1) :tag 1)

(geo:synchronize)

;; Add the post-processing view as a new size field:
(let ((bg-field (mesh:field-add "PostView")))
  (mesh:field-set-number bg-field "ViewIndex" 0)

  ;; Apply the view as the current background mesh size field:
  (mesh:field-set-as-background-mesh bg-field))

;; In order to compute the mesh sizes from the background mesh only, and
;; disregard any other size constraints, one can set:
(opt:set-number "Mesh.MeshSizeExtendFromBoundary" 0)
(opt:set-number "Mesh.MeshSizeFromPoints" 0)
(opt:set-number "Mesh.MeshSizeFromCurvature" 0)

;; See t10.lisp for additional information: background meshes are actually a
;; particular case of general "mesh size fields".

(mesh:generate :dim 2)
;; (gmsh:write "/tmp/t7.msh")
