;;;; geo-mesh.lisp — Generated wrappers for gmsh/geo
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(in-package :gmsh/geo)

(defun mesh-set-size (dim-tags size)
  "Set a mesh size constraint on the entities `dimTags' (given as a
vector of (dim, tag) pairs) in the built-in CAD kernel representation.
Currently only entities of dimension 0 (points) are handled."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-mesh-set-size dim-tags-ptr dim-tags-n (to-double size) ierr))))

(defun mesh-set-transfinite-curve (tag n-points &key (mesh-type "Progression") (coef 1.0))
  "Set a transfinite meshing constraint on the curve `tag' in the built-
in CAD kernel representation, with `numNodes' nodes distributed
according to `meshType' and `coef'. Currently supported types are
\"Progression\" (geometrical progression with power `coef') and
\"Bump\" (refinement toward both extremities of the curve)."
  (with-ierr (ierr)
      (gmsh/internal::%model-geo-mesh-set-transfinite-curve tag n-points mesh-type (to-double coef) ierr)))

(defun mesh-set-transfinite-surface (tag &key (arrangement "Left") (corner-tags '()))
  "Set a transfinite meshing constraint on the surface `tag' in the
built-in CAD kernel representation. `arrangement' describes the
arrangement of the triangles when the surface is not flagged as
recombined: currently supported values are \"Left\", \"Right\",
\"AlternateLeft\" and \"AlternateRight\". `cornerTags' can be used to
specify the (3 or 4) corners of the transfinite interpolation
explicitly; specifying the corners explicitly is mandatory if the
surface has more that 3 or 4 points on its boundary."
  (with-foreign-array (corner-tags-ptr corner-tags-n corner-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-mesh-set-transfinite-surface tag arrangement corner-tags-ptr corner-tags-n ierr))))

(defun mesh-set-transfinite-volume (tag &key (corner-tags '()))
  "Set a transfinite meshing constraint on the surface `tag' in the
built-in CAD kernel representation. `cornerTags' can be used to
specify the (6 or 8) corners of the transfinite interpolation
explicitly."
  (with-foreign-array (corner-tags-ptr corner-tags-n corner-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-mesh-set-transfinite-volume tag corner-tags-ptr corner-tags-n ierr))))

(defun mesh-set-recombine (dim tag &key (angle 45.0))
  "Set a recombination meshing constraint on the entity of dimension
`dim' and tag `tag' in the built-in CAD kernel representation.
Currently only entities of dimension 2 (to recombine triangles into
quadrangles) are supported; `angle' specifies the threshold angle for
the simple recombination algorithm."
  (with-ierr (ierr)
      (gmsh/internal::%model-geo-mesh-set-recombine dim tag (to-double angle) ierr)))

(defun mesh-set-smoothing (dim tag val)
  "Set a smoothing meshing constraint on the entity of dimension `dim'
and tag `tag' in the built-in CAD kernel representation. `val'
iterations of a Laplace smoother are applied."
  (with-ierr (ierr)
      (gmsh/internal::%model-geo-mesh-set-smoothing dim tag val ierr)))

(defun mesh-set-reverse (dim tag &key (val t))
  "Set a reverse meshing constraint on the entity of dimension `dim' and
tag `tag' in the built-in CAD kernel representation. If `val' is true,
the mesh orientation will be reversed with respect to the natural mesh
orientation (i.e. the orientation consistent with the orientation of
the geometry). If `val' is false, the mesh is left as-is."
  (with-ierr (ierr)
      (gmsh/internal::%model-geo-mesh-set-reverse dim tag (if val 1 0) ierr)))

(defun mesh-set-algorithm (dim tag val)
  "Set the meshing algorithm on the entity of dimension `dim' and tag
`tag' in the built-in CAD kernel representation. Currently only
supported for `dim' == 2."
  (with-ierr (ierr)
      (gmsh/internal::%model-geo-mesh-set-algorithm dim tag val ierr)))

(defun mesh-set-size-from-boundary (dim tag val)
  "Force the mesh size to be extended from the boundary, or not, for the
entity of dimension `dim' and tag `tag' in the built-in CAD kernel
representation. Currently only supported for `dim' == 2."
  (with-ierr (ierr)
      (gmsh/internal::%model-geo-mesh-set-size-from-boundary dim tag val ierr)))

