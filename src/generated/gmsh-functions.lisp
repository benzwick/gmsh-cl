;;;; gmsh-functions.lisp — Generated wrappers for gmsh
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(in-package :gmsh)

(defun initialize (&key (read-config-files t) (run nil))
  "Initialize the Gmsh API. This must be called before any call to the
other functions in the API. If `argc' and `argv' (or just `argv' in
Python or Julia) are provided, they will be handled in the same way as
the command line arguments in the Gmsh app. If `readConfigFiles' is
set, read system Gmsh configuration files (gmshrc and gmsh-options).
If `run' is set, run in the same way as the Gmsh app, either
interactively or in batch mode depending on the command line
arguments. If `run' is not set, initializing the API sets the options
\"General.AbortOnError\" to 2 and \"General.Terminal\" to 1."
  (with-ierr (ierr)
      (gmsh/internal::%initialize 0 (cffi:null-pointer) (if read-config-files 1 0) (if run 1 0) ierr)))

(defun is-initialized ()
  "Return 1 if the Gmsh API is initialized, and 0 if not."
  (with-ierr (ierr)
      (gmsh/internal::%is-initialized  ierr)))

(defun finalize ()
  "Finalize the Gmsh API. This must be called when you are done using the
Gmsh API."
  (with-ierr (ierr)
      (gmsh/internal::%finalize  ierr)))

(defun open (file-name)
  "Open a file. Equivalent to the `File->Open' menu in the Gmsh app.
Handling of the file depends on its extension and/or its contents:
opening a file with model data will create a new model."
  (with-ierr (ierr)
      (gmsh/internal::%open file-name ierr)))

(defun merge (file-name)
  "Merge a file. Equivalent to the `File->Merge' menu in the Gmsh app.
Handling of the file depends on its extension and/or its contents.
Merging a file with model data will add the data to the current model."
  (with-ierr (ierr)
      (gmsh/internal::%merge file-name ierr)))

(defun write (file-name)
  "Write a file. The export format is determined by the file extension."
  (with-ierr (ierr)
      (gmsh/internal::%write file-name ierr)))

(defun clear ()
  "Clear all loaded models and post-processing data, and add a new empty
model."
  (with-ierr (ierr)
      (gmsh/internal::%clear  ierr)))

(defun add (name)
  "Add a new model, with name `name', and set it as the current model."
  (with-ierr (ierr)
      (gmsh/internal::%model-add name ierr)))

(defun remove ()
  "Remove the current model."
  (with-ierr (ierr)
      (gmsh/internal::%model-remove  ierr)))

(defun list ()
  "List the names of all models."
  (cffi:with-foreign-objects ((names-out :pointer) (names-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-list names-out names-n-out ierr))
    (foreign-string-array-to-list (cffi:mem-ref names-out :pointer) (cffi:mem-ref names-n-out :unsigned-long))))

(defun get-current ()
  "Get the name of the current model."
  (cffi:with-foreign-object (name-out :pointer)
      (with-ierr (ierr)
      (gmsh/internal::%model-get-current name-out ierr))
    (foreign-string-result (cffi:mem-ref name-out :pointer))))

(defun set-current (name)
  "Set the current model to the model with name `name'. If several models
have the same name, select the one that was added first."
  (with-ierr (ierr)
      (gmsh/internal::%model-set-current name ierr)))

(defun get-file-name ()
  "Get the file name (if any) associated with the current model. A file
name is associated when a model is read from a file on disk."
  (cffi:with-foreign-object (file-name-out :pointer)
      (with-ierr (ierr)
      (gmsh/internal::%model-get-file-name file-name-out ierr))
    (foreign-string-result (cffi:mem-ref file-name-out :pointer))))

(defun set-file-name (file-name)
  "Set the file name associated with the current model."
  (with-ierr (ierr)
      (gmsh/internal::%model-set-file-name file-name ierr)))

(defun get-entities (&key (dim -1))
  "Get all the entities in the current model. A model entity is
represented by two integers: its dimension (dim == 0, 1, 2 or 3) and
its tag (its unique, strictly positive identifier). If `dim' is >= 0,
return only the entities of the specified dimension (e.g. points if
`dim' == 0). The entities are returned as a vector of (dim, tag)
pairs."
  (cffi:with-foreign-objects ((dim-tags-out :pointer) (dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-entities dim-tags-out dim-tags-n-out dim ierr))
    (dim-tags-to-pairs (cffi:mem-ref dim-tags-out :pointer) (cffi:mem-ref dim-tags-n-out :unsigned-long))))

(defun set-entity-name (dim tag name)
  "Set the name of the entity of dimension `dim' and tag `tag'."
  (with-ierr (ierr)
      (gmsh/internal::%model-set-entity-name dim tag name ierr)))

(defun get-entity-name (dim tag)
  "Get the name of the entity of dimension `dim' and tag `tag'."
  (cffi:with-foreign-object (name-out :pointer)
      (with-ierr (ierr)
      (gmsh/internal::%model-get-entity-name dim tag name-out ierr))
    (foreign-string-result (cffi:mem-ref name-out :pointer))))

(defun remove-entity-name (name)
  "Remove the entity name `name' from the current model."
  (with-ierr (ierr)
      (gmsh/internal::%model-remove-entity-name name ierr)))

(defun get-physical-groups (&key (dim -1))
  "Get the physical groups in the current model. The physical groups are
returned as a vector of (dim, tag) pairs. If `dim' is >= 0, return
only the groups of the specified dimension (e.g. physical points if
`dim' == 0)."
  (cffi:with-foreign-objects ((dim-tags-out :pointer) (dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-physical-groups dim-tags-out dim-tags-n-out dim ierr))
    (dim-tags-to-pairs (cffi:mem-ref dim-tags-out :pointer) (cffi:mem-ref dim-tags-n-out :unsigned-long))))

(defun get-physical-groups-entities (&key (dim -1))
  "Get the physical groups in the current model as well as the model
entities that make them up. The physical groups are returned as the
vector of (dim, tag) pairs `dimTags'. The model entities making up the
corresponding physical groups are returned in `entities'. If `dim' is
>= 0, return only the groups of the specified dimension (e.g. physical
points if `dim' == 0)."
  (cffi:with-foreign-objects ((dim-tags-out :pointer) (dim-tags-n-out :unsigned-long) (entities-out :pointer) (entities-n-out :pointer) (entities-nn-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-physical-groups-entities dim-tags-out dim-tags-n-out entities-out entities-n-out entities-nn-out dim ierr))
    (values (dim-tags-to-pairs (cffi:mem-ref dim-tags-out :pointer) (cffi:mem-ref dim-tags-n-out :unsigned-long)) (foreign-vector-pairs-to-list (cffi:mem-ref entities-out :pointer) (cffi:mem-ref entities-n-out :pointer) (cffi:mem-ref entities-nn-out :unsigned-long)))))

(defun get-entities-for-physical-group (dim tag)
  "Get the tags of the model entities making up the physical group of
dimension `dim' and tag `tag'."
  (cffi:with-foreign-objects ((tags-out :pointer) (tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-entities-for-physical-group dim tag tags-out tags-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref tags-out :pointer) (cffi:mem-ref tags-n-out :unsigned-long) :int)))

(defun get-entities-for-physical-name (name)
  "Get the model entities (as a vector (dim, tag) pairs) making up the
physical group with name `name'."
  (cffi:with-foreign-objects ((dim-tags-out :pointer) (dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-entities-for-physical-name name dim-tags-out dim-tags-n-out ierr))
    (dim-tags-to-pairs (cffi:mem-ref dim-tags-out :pointer) (cffi:mem-ref dim-tags-n-out :unsigned-long))))

(defun get-physical-groups-for-entity (dim tag)
  "Get the tags of the physical groups (if any) to which the model entity
of dimension `dim' and tag `tag' belongs."
  (cffi:with-foreign-objects ((physical-tags-out :pointer) (physical-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-physical-groups-for-entity dim tag physical-tags-out physical-tags-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref physical-tags-out :pointer) (cffi:mem-ref physical-tags-n-out :unsigned-long) :int)))

(defun add-physical-group (dim tags &key (tag -1) (name ""))
  "Add a physical group of dimension `dim', grouping the model entities
with tags `tags'. Return the tag of the physical group, equal to `tag'
if `tag' is positive, or a new tag if `tag' < 0. Set the name of the
physical group if `name' is not empty."
  (with-foreign-array (tags-ptr tags-n tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-add-physical-group dim tags-ptr tags-n tag name ierr))))

(defun remove-physical-groups (&key (dim-tags '()))
  "Remove the physical groups `dimTags' (given as a vector of (dim, tag)
pairs) from the current model. If `dimTags' is empty, remove all
groups."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-remove-physical-groups dim-tags-ptr dim-tags-n ierr))))

(defun set-physical-name (dim tag name)
  "Set the name of the physical group of dimension `dim' and tag `tag'."
  (with-ierr (ierr)
      (gmsh/internal::%model-set-physical-name dim tag name ierr)))

(defun get-physical-name (dim tag)
  "Get the name of the physical group of dimension `dim' and tag `tag'."
  (cffi:with-foreign-object (name-out :pointer)
      (with-ierr (ierr)
      (gmsh/internal::%model-get-physical-name dim tag name-out ierr))
    (foreign-string-result (cffi:mem-ref name-out :pointer))))

(defun remove-physical-name (name)
  "Remove the physical name `name' from the current model."
  (with-ierr (ierr)
      (gmsh/internal::%model-remove-physical-name name ierr)))

(defun set-tag (dim tag new-tag)
  "Set the tag of the entity of dimension `dim' and tag `tag' to the new
value `newTag'."
  (with-ierr (ierr)
      (gmsh/internal::%model-set-tag dim tag new-tag ierr)))

(defun get-boundary (dim-tags &key (combined t) (oriented nil) (recursive nil))
  "Get the boundary of the model entities `dimTags', given as a vector of
(dim, tag) pairs. Return in `outDimTags' the boundary of the
individual entities (if `combined' is false) or the boundary of the
combined geometrical shape formed by all input entities (if `combined'
is true). Return tags multiplied by the sign of the boundary entity if
`oriented' is true. Apply the boundary operator recursively down to
dimension 0 (i.e. to points) if `recursive' is true."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-boundary dim-tags-ptr dim-tags-n out-dim-tags-out out-dim-tags-n-out (if combined 1 0) (if oriented 1 0) (if recursive 1 0) ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)))))

(defun get-adjacencies (dim tag)
  "Get the upward and downward adjacencies of the model entity of
dimension `dim' and tag `tag'. The `upward' vector returns the tags of
adjacent entities of dimension `dim' + 1; the `downward' vector
returns the tags of adjacent entities of dimension `dim' - 1."
  (cffi:with-foreign-objects ((upward-out :pointer) (upward-n-out :unsigned-long) (downward-out :pointer) (downward-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-adjacencies dim tag upward-out upward-n-out downward-out downward-n-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref upward-out :pointer) (cffi:mem-ref upward-n-out :unsigned-long) :int) (foreign-array-to-list (cffi:mem-ref downward-out :pointer) (cffi:mem-ref downward-n-out :unsigned-long) :int))))

(defun is-entity-orphan (dim tag)
  "Return whether the model entity of dimension `dim' and tag `tag' is an
orphan, i.e. is not connected to any entity of the highest dimension
in the model."
  (with-ierr (ierr)
      (gmsh/internal::%model-is-entity-orphan dim tag ierr)))

(defun get-entities-in-bounding-box (xmin ymin zmin xmax ymax zmax &key (dim -1))
  "Get the model entities in the bounding box defined by the two points
(`xmin', `ymin', `zmin') and (`xmax', `ymax', `zmax'). If `dim' is >=
0, return only the entities of the specified dimension (e.g. points if
`dim' == 0)."
  (cffi:with-foreign-objects ((dim-tags-out :pointer) (dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-entities-in-bounding-box (to-double xmin) (to-double ymin) (to-double zmin) (to-double xmax) (to-double ymax) (to-double zmax) dim-tags-out dim-tags-n-out dim ierr))
    (dim-tags-to-pairs (cffi:mem-ref dim-tags-out :pointer) (cffi:mem-ref dim-tags-n-out :unsigned-long))))

(defun get-bounding-box (dim tag)
  "Get the bounding box (`xmin', `ymin', `zmin'), (`xmax', `ymax',
`zmax') of the model entity of dimension `dim' and tag `tag'. If `dim'
and `tag' are negative, get the bounding box of the whole model."
  (cffi:with-foreign-objects ((xmin-out :double) (ymin-out :double) (zmin-out :double) (xmax-out :double) (ymax-out :double) (zmax-out :double))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-bounding-box dim tag xmin-out ymin-out zmin-out xmax-out ymax-out zmax-out ierr))
    (values (cffi:mem-ref xmin-out :double) (cffi:mem-ref ymin-out :double) (cffi:mem-ref zmin-out :double) (cffi:mem-ref xmax-out :double) (cffi:mem-ref ymax-out :double) (cffi:mem-ref zmax-out :double))))

(defun get-dimension ()
  "Return the geometrical dimension of the current model."
  (with-ierr (ierr)
      (gmsh/internal::%model-get-dimension  ierr)))

(defun add-discrete-entity (dim &key (tag -1) (boundary '()))
  "Add a discrete model entity (defined by a mesh) of dimension `dim' in
the current model. Return the tag of the new discrete entity, equal to
`tag' if `tag' is positive, or a new tag if `tag' < 0. `boundary'
specifies the tags of the entities on the boundary of the discrete
entity, if any. Specifying `boundary' allows Gmsh to construct the
topology of the overall model."
  (with-foreign-array (boundary-ptr boundary-n boundary :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-add-discrete-entity dim tag boundary-ptr boundary-n ierr))))

(defun remove-entities (dim-tags &key (recursive nil))
  "Remove the entities `dimTags' (given as a vector of (dim, tag) pairs)
of the current model, provided that they are not on the boundary of
(or embedded in) higher-dimensional entities. If `recursive' is true,
remove all the entities on their boundaries, down to dimension 0."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-remove-entities dim-tags-ptr dim-tags-n (if recursive 1 0) ierr))))

(defun get-entity-type (dim tag)
  "Get the type of the entity of dimension `dim' and tag `tag'."
  (cffi:with-foreign-object (entity-type-out :pointer)
      (with-ierr (ierr)
      (gmsh/internal::%model-get-entity-type dim tag entity-type-out ierr))
    (foreign-string-result (cffi:mem-ref entity-type-out :pointer))))

(defun get-type (dim tag)
  "Get the type of the entity of dimension `dim' and tag `tag'. (This is
a deprecated synonym for `getType'.)"
  (cffi:with-foreign-object (entity-type-out :pointer)
      (with-ierr (ierr)
      (gmsh/internal::%model-get-type dim tag entity-type-out ierr))
    (foreign-string-result (cffi:mem-ref entity-type-out :pointer))))

(defun get-entity-properties (dim tag)
  "Get the properties of the entity of dimension `dim' and tag `tag'. The
`reals' vector contains the 4 coefficients of the cartesian equation
for a plane surface; the center coordinates, axis direction, major
radius and minor radius for a torus; the center coordinates, axis
direction and radius for a cylinder; the center coordinates, axis
direction, radius and semi-angle for surfaces of revolution; the
center coordinates and the radius for a sphere."
  (cffi:with-foreign-objects ((integers-out :pointer) (integers-n-out :unsigned-long) (reals-out :pointer) (reals-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-entity-properties dim tag integers-out integers-n-out reals-out reals-n-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref integers-out :pointer) (cffi:mem-ref integers-n-out :unsigned-long) :int) (foreign-array-to-list (cffi:mem-ref reals-out :pointer) (cffi:mem-ref reals-n-out :unsigned-long) :double))))

(defun get-parent (dim tag)
  "In a partitioned model, get the parent of the entity of dimension
`dim' and tag `tag', i.e. from which the entity is a part of, if any.
`parentDim' and `parentTag' are set to -1 if the entity has no parent."
  (cffi:with-foreign-objects ((parent-dim-out :int) (parent-tag-out :int))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-parent dim tag parent-dim-out parent-tag-out ierr))
    (values (cffi:mem-ref parent-dim-out :int) (cffi:mem-ref parent-tag-out :int))))

(defun get-number-of-partitions ()
  "Return the number of partitions in the model."
  (with-ierr (ierr)
      (gmsh/internal::%model-get-number-of-partitions  ierr)))

(defun get-partitions (dim tag)
  "In a partitioned model, return the tags of the partition(s) to which
the entity belongs."
  (cffi:with-foreign-objects ((partitions-out :pointer) (partitions-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-partitions dim tag partitions-out partitions-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref partitions-out :pointer) (cffi:mem-ref partitions-n-out :unsigned-long) :int)))

(defun get-value (dim tag parametric-coord)
  "Evaluate the parametrization of the entity of dimension `dim' and tag
`tag' at the parametric coordinates `parametricCoord'. Only valid for
`dim' equal to 0 (with empty `parametricCoord'), 1 (with
`parametricCoord' containing parametric coordinates on the curve) or 2
(with `parametricCoord' containing u, v parametric coordinates on the
surface, concatenated: [p1u, p1v, p2u, ...]). Return x, y, z
coordinates in `coord', concatenated: [p1x, p1y, p1z, p2x, ...]."
  (with-foreign-array (parametric-coord-ptr parametric-coord-n parametric-coord :double)
      (cffi:with-foreign-objects ((coord-out :pointer) (coord-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-value dim tag parametric-coord-ptr parametric-coord-n coord-out coord-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref coord-out :pointer) (cffi:mem-ref coord-n-out :unsigned-long) :double))))

(defun get-derivative (dim tag parametric-coord)
  "Evaluate the derivative of the parametrization of the entity of
dimension `dim' and tag `tag' at the parametric coordinates
`parametricCoord'. Only valid for `dim' equal to 1 (with
`parametricCoord' containing parametric coordinates on the curve) or 2
(with `parametricCoord' containing u, v parametric coordinates on the
surface, concatenated: [p1u, p1v, p2u, ...]). For `dim' equal to 1
return the x, y, z components of the derivative with respect to u
[d1ux, d1uy, d1uz, d2ux, ...]; for `dim' equal to 2 return the x, y, z
components of the derivative with respect to u and v: [d1ux, d1uy,
d1uz, d1vx, d1vy, d1vz, d2ux, ...]."
  (with-foreign-array (parametric-coord-ptr parametric-coord-n parametric-coord :double)
      (cffi:with-foreign-objects ((derivatives-out :pointer) (derivatives-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-derivative dim tag parametric-coord-ptr parametric-coord-n derivatives-out derivatives-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref derivatives-out :pointer) (cffi:mem-ref derivatives-n-out :unsigned-long) :double))))

(defun get-second-derivative (dim tag parametric-coord)
  "Evaluate the second derivative of the parametrization of the entity of
dimension `dim' and tag `tag' at the parametric coordinates
`parametricCoord'. Only valid for `dim' equal to 1 (with
`parametricCoord' containing parametric coordinates on the curve) or 2
(with `parametricCoord' containing u, v parametric coordinates on the
surface, concatenated: [p1u, p1v, p2u, ...]). For `dim' equal to 1
return the x, y, z components of the second derivative with respect to
u [d1uux, d1uuy, d1uuz, d2uux, ...]; for `dim' equal to 2 return the
x, y, z components of the second derivative with respect to u and v,
and the mixed derivative with respect to u and v: [d1uux, d1uuy,
d1uuz, d1vvx, d1vvy, d1vvz, d1uvx, d1uvy, d1uvz, d2uux, ...]."
  (with-foreign-array (parametric-coord-ptr parametric-coord-n parametric-coord :double)
      (cffi:with-foreign-objects ((derivatives-out :pointer) (derivatives-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-second-derivative dim tag parametric-coord-ptr parametric-coord-n derivatives-out derivatives-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref derivatives-out :pointer) (cffi:mem-ref derivatives-n-out :unsigned-long) :double))))

(defun get-curvature (dim tag parametric-coord)
  "Evaluate the (maximum) curvature of the entity of dimension `dim' and
tag `tag' at the parametric coordinates `parametricCoord'. Only valid
for `dim' equal to 1 (with `parametricCoord' containing parametric
coordinates on the curve) or 2 (with `parametricCoord' containing u, v
parametric coordinates on the surface, concatenated: [p1u, p1v, p2u,
...])."
  (with-foreign-array (parametric-coord-ptr parametric-coord-n parametric-coord :double)
      (cffi:with-foreign-objects ((curvatures-out :pointer) (curvatures-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-curvature dim tag parametric-coord-ptr parametric-coord-n curvatures-out curvatures-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref curvatures-out :pointer) (cffi:mem-ref curvatures-n-out :unsigned-long) :double))))

(defun get-principal-curvatures (tag parametric-coord)
  "Evaluate the principal curvatures of the surface with tag `tag' at the
parametric coordinates `parametricCoord', as well as their respective
directions. `parametricCoord' are given by pair of u and v
coordinates, concatenated: [p1u, p1v, p2u, ...]."
  (with-foreign-array (parametric-coord-ptr parametric-coord-n parametric-coord :double)
      (cffi:with-foreign-objects ((curvature-max-out :pointer) (curvature-max-n-out :unsigned-long) (curvature-min-out :pointer) (curvature-min-n-out :unsigned-long) (direction-max-out :pointer) (direction-max-n-out :unsigned-long) (direction-min-out :pointer) (direction-min-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-principal-curvatures tag parametric-coord-ptr parametric-coord-n curvature-max-out curvature-max-n-out curvature-min-out curvature-min-n-out direction-max-out direction-max-n-out direction-min-out direction-min-n-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref curvature-max-out :pointer) (cffi:mem-ref curvature-max-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref curvature-min-out :pointer) (cffi:mem-ref curvature-min-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref direction-max-out :pointer) (cffi:mem-ref direction-max-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref direction-min-out :pointer) (cffi:mem-ref direction-min-n-out :unsigned-long) :double)))))

(defun get-normal (tag parametric-coord)
  "Get the normal to the surface with tag `tag' at the parametric
coordinates `parametricCoord'. The `parametricCoord' vector should
contain u and v coordinates, concatenated: [p1u, p1v, p2u, ...].
`normals' are returned as a vector of x, y, z components,
concatenated: [n1x, n1y, n1z, n2x, ...]."
  (with-foreign-array (parametric-coord-ptr parametric-coord-n parametric-coord :double)
      (cffi:with-foreign-objects ((normals-out :pointer) (normals-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-normal tag parametric-coord-ptr parametric-coord-n normals-out normals-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref normals-out :pointer) (cffi:mem-ref normals-n-out :unsigned-long) :double))))

(defun get-parametrization (dim tag coord)
  "Get the parametric coordinates `parametricCoord' for the points
`coord' on the entity of dimension `dim' and tag `tag'. `coord' are
given as x, y, z coordinates, concatenated: [p1x, p1y, p1z, p2x, ...].
`parametricCoord' returns the parametric coordinates t on the curve
(if `dim' = 1) or u and v coordinates concatenated on the surface (if
`dim' == 2), i.e. [p1t, p2t, ...] or [p1u, p1v, p2u, ...]."
  (with-foreign-array (coord-ptr coord-n coord :double)
      (cffi:with-foreign-objects ((parametric-coord-out :pointer) (parametric-coord-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-parametrization dim tag coord-ptr coord-n parametric-coord-out parametric-coord-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref parametric-coord-out :pointer) (cffi:mem-ref parametric-coord-n-out :unsigned-long) :double))))

(defun get-parametrization-bounds (dim tag)
  "Get the `min' and `max' bounds of the parametric coordinates for the
entity of dimension `dim' and tag `tag'."
  (cffi:with-foreign-objects ((min-out :pointer) (min-n-out :unsigned-long) (max-out :pointer) (max-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-parametrization-bounds dim tag min-out min-n-out max-out max-n-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref min-out :pointer) (cffi:mem-ref min-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref max-out :pointer) (cffi:mem-ref max-n-out :unsigned-long) :double))))

(defun is-inside (dim tag coord &key (parametric nil))
  "Check if the coordinates (or the parametric coordinates if
`parametric' is set) provided in `coord' correspond to points inside
the entity of dimension `dim' and tag `tag', and return the number of
points inside. This feature is only available for a subset of
entities, depending on the underlying geometrical representation."
  (with-foreign-array (coord-ptr coord-n coord :double)
      (with-ierr (ierr)
      (gmsh/internal::%model-is-inside dim tag coord-ptr coord-n (if parametric 1 0) ierr))))

(defun get-closest-point (dim tag coord)
  "Get the points `closestCoord' on the entity of dimension `dim' (1 or
2) and tag `tag' to the points `coord', by orthogonal projection.
`coord' and `closestCoord' are given as x, y, z coordinates,
concatenated: [p1x, p1y, p1z, p2x, ...]. `parametricCoord' returns the
parametric coordinates t on the curve (if `dim' == 1) or u and v
coordinates concatenated on the surface (if `dim' = 2), i.e. [p1t,
p2t, ...] or [p1u, p1v, p2u, ...]. The closest points can lie outside
the (trimmed) entities: use `isInside()' to check."
  (with-foreign-array (coord-ptr coord-n coord :double)
      (cffi:with-foreign-objects ((closest-coord-out :pointer) (closest-coord-n-out :unsigned-long) (parametric-coord-out :pointer) (parametric-coord-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-closest-point dim tag coord-ptr coord-n closest-coord-out closest-coord-n-out parametric-coord-out parametric-coord-n-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref closest-coord-out :pointer) (cffi:mem-ref closest-coord-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref parametric-coord-out :pointer) (cffi:mem-ref parametric-coord-n-out :unsigned-long) :double)))))

(defun reparametrize-on-surface (dim tag parametric-coord surface-tag &key (which 0))
  "Reparametrize the boundary entity (point or curve, i.e. with `dim' ==
0 or `dim' == 1) of tag `tag' on the surface `surfaceTag'. If `dim' ==
1, reparametrize all the points corresponding to the parametric
coordinates `parametricCoord'. Multiple matches in case of periodic
surfaces can be selected with `which'. This feature is only available
for a subset of entities, depending on the underlying geometrical
representation."
  (with-foreign-array (parametric-coord-ptr parametric-coord-n parametric-coord :double)
      (cffi:with-foreign-objects ((surface-parametric-coord-out :pointer) (surface-parametric-coord-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-reparametrize-on-surface dim tag parametric-coord-ptr parametric-coord-n surface-tag surface-parametric-coord-out surface-parametric-coord-n-out which ierr))
    (foreign-array-to-list (cffi:mem-ref surface-parametric-coord-out :pointer) (cffi:mem-ref surface-parametric-coord-n-out :unsigned-long) :double))))

(defun set-visibility (dim-tags value &key (recursive nil))
  "Set the visibility of the model entities `dimTags' (given as a vector
of (dim, tag) pairs) to `value'. Apply the visibility setting
recursively if `recursive' is true."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-set-visibility dim-tags-ptr dim-tags-n value (if recursive 1 0) ierr))))

(defun get-visibility (dim tag)
  "Get the visibility of the model entity of dimension `dim' and tag
`tag'."
  (cffi:with-foreign-object (value-out :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-get-visibility dim tag value-out ierr))
    (cffi:mem-ref value-out :int)))

(defun set-visibility-per-window (value &key (window-index 0))
  "Set the global visibility of the model per window to `value', where
`windowIndex' identifies the window in the window list."
  (with-ierr (ierr)
      (gmsh/internal::%model-set-visibility-per-window value window-index ierr)))

(defun set-color (dim-tags r g b &key (a 255) (recursive nil))
  "Set the color of the model entities `dimTags' (given as a vector of
(dim, tag) pairs) to the RGBA value (`r', `g', `b', `a'), where `r',
`g', `b' and `a' should be integers between 0 and 255. Apply the color
setting recursively if `recursive' is true."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-set-color dim-tags-ptr dim-tags-n r g b a (if recursive 1 0) ierr))))

(defun get-color (dim tag)
  "Get the color of the model entity of dimension `dim' and tag `tag'. If
no color is specified for the entity, return fully transparent blue,
i.e. (0, 0, 255, 0)."
  (cffi:with-foreign-objects ((r-out :int) (g-out :int) (b-out :int) (a-out :int))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-color dim tag r-out g-out b-out a-out ierr))
    (values (cffi:mem-ref r-out :int) (cffi:mem-ref g-out :int) (cffi:mem-ref b-out :int) (cffi:mem-ref a-out :int))))

(defun set-coordinates (tag x y z)
  "Set the `x', `y', `z' coordinates of a geometrical point."
  (with-ierr (ierr)
      (gmsh/internal::%model-set-coordinates tag (to-double x) (to-double y) (to-double z) ierr)))

(defun set-attribute (name values)
  "Set the values of the attribute with name `name'."
  (with-string-array (values-ptr values-n values)
      (with-ierr (ierr)
      (gmsh/internal::%model-set-attribute name values-ptr values-n ierr))))

(defun get-attribute (name)
  "Get the values of the attribute with name `name'."
  (cffi:with-foreign-objects ((values-out :pointer) (values-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-attribute name values-out values-n-out ierr))
    (foreign-string-array-to-list (cffi:mem-ref values-out :pointer) (cffi:mem-ref values-n-out :unsigned-long))))

(defun get-attribute-names ()
  "Get the names of any optional attributes stored in the model."
  (cffi:with-foreign-objects ((names-out :pointer) (names-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-get-attribute-names names-out names-n-out ierr))
    (foreign-string-array-to-list (cffi:mem-ref names-out :pointer) (cffi:mem-ref names-n-out :unsigned-long))))

(defun remove-attribute (name)
  "Remove the attribute with name `name'."
  (with-ierr (ierr)
      (gmsh/internal::%model-remove-attribute name ierr)))

(defun draw ()
  "Draw all the OpenGL scenes."
  (with-ierr (ierr)
      (gmsh/internal::%graphics-draw  ierr)))

