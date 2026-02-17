;;;; occ-mesh.lisp — Generated wrappers for gmsh/occ
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(in-package :gmsh/occ)

(defun mesh-set-size (dim-tags size)
  "Set a mesh size constraint on the entities `dimTags' (given as a
vector of (dim, tag) pairs) in the OpenCASCADE CAD representation.
Currently only entities of dimension 0 (points) are handled."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-mesh-set-size dim-tags-ptr dim-tags-n (to-double size) ierr))))

