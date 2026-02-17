;;;; mesh-functions.lisp — Generated wrappers for gmsh/mesh
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(in-package :gmsh/mesh)

(defun generate (&key (dim 3))
  "Generate a mesh of the current model, up to dimension `dim' (0, 1, 2
or 3)."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-generate dim ierr)))

(defun partition (num-part &key (element-tags '()) (partitions '()))
  "Partition the mesh of the current model into `numPart' partitions.
Optionally, `elementTags' and `partitions' can be provided to specify
the partition of each element explicitly."
  (with-foreign-array (element-tags-ptr element-tags-n element-tags :unsigned-long)
      (with-foreign-array (partitions-ptr partitions-n partitions :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-partition num-part element-tags-ptr element-tags-n partitions-ptr partitions-n ierr)))))

(defun create-overlaps (&key (layers 1) (create-boundaries t))
  "Generate node-based overlaps (of highest dimension) for all
partitions, with a number of layers equal to `layers'. If
`createBoundaries' is set, build the overlaps for the entities
bounding the highest-dimensional entities (i.e. \"boundary
overlaps\"), as well as the inner boundaries of the overlaps (i.e.
\"overlap boundaries\")."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-create-overlaps layers (if create-boundaries 1 0) ierr)))

(defun get-partition-entities (dim tag partition)
  "Get the tags of the partitioned entities of dimension `dim' whose
parent has dimension `dim' and tag `tag', and which belong to the
partition `partition'. If overlaps are present, fill `overlapEntities'
with the tags of the entities that are in the overlap of the
partition. Works for entities of the same dimension as the model as
well as for entities one dimension below (boundary overlaps)."
  (cffi:with-foreign-objects ((entity-tags-out :pointer) (entity-tags-n-out :unsigned-long) (overlap-entities-out :pointer) (overlap-entities-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-partition-entities dim tag partition entity-tags-out entity-tags-n-out overlap-entities-out overlap-entities-n-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref entity-tags-out :pointer) (cffi:mem-ref entity-tags-n-out :unsigned-long) :int) (foreign-array-to-list (cffi:mem-ref overlap-entities-out :pointer) (cffi:mem-ref overlap-entities-n-out :unsigned-long) :int))))

(defun get-overlap-boundary (dim tag partition)
  "Get the tags of the entities making up the overlap boundary of
partition `partition' inside the (non-partitioned) entity of dimension
`dim' and tag `tag'."
  (cffi:with-foreign-objects ((entity-tags-out :pointer) (entity-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-overlap-boundary dim tag partition entity-tags-out entity-tags-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref entity-tags-out :pointer) (cffi:mem-ref entity-tags-n-out :unsigned-long) :int)))

(defun get-boundary-overlap-parent (dim tag)
  "If the entity of dimension `dim' and tag `tag' is a boundary overlap,
get the entity of dimension `dim+1' that created it. Sets `parentTag'
to -1 on error."
  (cffi:with-foreign-object (parent-tag-out :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-boundary-overlap-parent dim tag parent-tag-out ierr))
    (cffi:mem-ref parent-tag-out :int)))

(defun unpartition ()
  "Unpartition the mesh of the current model."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-unpartition  ierr)))

(defun optimize (&key (method "") (force nil) (niter 1) (dim-tags '()))
  "Optimize the mesh of the current model using `method' (empty for
default tetrahedral mesh optimizer, \"Netgen\" for Netgen optimizer,
\"HighOrder\" for direct high-order mesh optimizer,
\"HighOrderElastic\" for high-order elastic smoother,
\"HighOrderFastCurving\" for fast curving algorithm, \"Laplace2D\" for
Laplace smoothing, \"Relocate2D\" and \"Relocate3D\" for node
relocation, \"QuadQuasiStructured\" for quad mesh optimization,
\"UntangleMeshGeometry\" for untangling). If `force' is set apply the
optimization also to discrete entities. If `dimTags' (given as a
vector of (dim, tag) pairs) is given, only apply the optimizer to the
given entities."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-optimize method (if force 1 0) niter dim-tags-ptr dim-tags-n ierr))))

(defun recombine ()
  "Recombine the mesh of the current model."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-recombine  ierr)))

(defun refine ()
  "Refine the mesh of the current model by uniformly splitting the
elements. This resets any high-order elements to order 1."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-refine  ierr)))

(defun set-order (order)
  "Change the order of the elements in the mesh of the current model to
`order'."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-order order ierr)))

(defun get-last-entity-error ()
  "Get the last entities `dimTags' (as a vector of (dim, tag) pairs)
where a meshing error occurred. Currently only populated by the new 3D
meshing algorithms."
  (cffi:with-foreign-objects ((dim-tags-out :pointer) (dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-last-entity-error dim-tags-out dim-tags-n-out ierr))
    (dim-tags-to-pairs (cffi:mem-ref dim-tags-out :pointer) (cffi:mem-ref dim-tags-n-out :unsigned-long))))

(defun get-last-node-error ()
  "Get the last node tags `nodeTags' where a meshing error occurred.
Currently only populated by the new 3D meshing algorithms."
  (cffi:with-foreign-objects ((node-tags-out :pointer) (node-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-last-node-error node-tags-out node-tags-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref node-tags-out :pointer) (cffi:mem-ref node-tags-n-out :unsigned-long) :unsigned-long)))

(defun clear (&key (dim-tags '()))
  "Clear the mesh, i.e. delete all the nodes and elements, for the
entities `dimTags', given as a vector of (dim, tag) pairs. If
`dimTags' is empty, clear the whole mesh. Note that the mesh of an
entity can only be cleared if this entity is not on the boundary of
another entity with a non-empty mesh."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-clear dim-tags-ptr dim-tags-n ierr))))

(defun remove-elements (dim tag &key (element-tags '()))
  "Remove the elements with tags `elementTags' from the entity of
dimension `dim' and tag `tag'. If `elementTags' is empty, remove all
the elements classified on the entity. To get consistent node
classification on model entities, `reclassifyNodes()' should be called
afterwards."
  (with-foreign-array (element-tags-ptr element-tags-n element-tags :unsigned-long)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-remove-elements dim tag element-tags-ptr element-tags-n ierr))))

(defun reverse (&key (dim-tags '()))
  "Reverse the orientation of the elements in the entities `dimTags',
given as a vector of (dim, tag) pairs. If `dimTags' is empty, reverse
the orientation of the elements in the whole mesh."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-reverse dim-tags-ptr dim-tags-n ierr))))

(defun reverse-elements (element-tags)
  "Reverse the orientation of the elements with tags `elementTags'."
  (with-foreign-array (element-tags-ptr element-tags-n element-tags :unsigned-long)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-reverse-elements element-tags-ptr element-tags-n ierr))))

(defun affine-transform (affine-transform &key (dim-tags '()))
  "Apply the affine transformation `affineTransform' (16 entries of a 4x4
matrix, by row; only the 12 first can be provided for convenience) to
the coordinates of the nodes classified on the entities `dimTags',
given as a vector of (dim, tag) pairs. If `dimTags' is empty,
transform all the nodes in the mesh."
  (with-foreign-array (affine-transform-ptr affine-transform-n affine-transform :double)
      (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-affine-transform affine-transform-ptr affine-transform-n dim-tags-ptr dim-tags-n ierr)))))

(defun get-nodes (&key (dim -1) (tag -1) (include-boundary nil) (return-parametric-coord t))
  "Get the nodes classified on the entity of dimension `dim' and tag
`tag'. If `tag' < 0, get the nodes for all entities of dimension
`dim'. If `dim' and `tag' are negative, get all the nodes in the mesh.
`nodeTags' contains the node tags (their unique, strictly positive
identification numbers). `coord' is a vector of length 3 times the
length of `nodeTags' that contains the x, y, z coordinates of the
nodes, concatenated: [n1x, n1y, n1z, n2x, ...]. If `dim' >= 0 and
`returnParamtricCoord' is set, `parametricCoord' contains the
parametric coordinates ([u1, u2, ...] or [u1, v1, u2, ...]) of the
nodes, if available. The length of `parametricCoord' can be 0 or `dim'
times the length of `nodeTags'. If `includeBoundary' is set, also
return the nodes classified on the boundary of the entity (which will
be reparametrized on the entity if `dim' >= 0 in order to compute
their parametric coordinates)."
  (cffi:with-foreign-objects ((node-tags-out :pointer) (node-tags-n-out :unsigned-long) (coord-out :pointer) (coord-n-out :unsigned-long) (parametric-coord-out :pointer) (parametric-coord-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-nodes node-tags-out node-tags-n-out coord-out coord-n-out parametric-coord-out parametric-coord-n-out dim tag (if include-boundary 1 0) (if return-parametric-coord 1 0) ierr))
    (values (foreign-array-to-list (cffi:mem-ref node-tags-out :pointer) (cffi:mem-ref node-tags-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref coord-out :pointer) (cffi:mem-ref coord-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref parametric-coord-out :pointer) (cffi:mem-ref parametric-coord-n-out :unsigned-long) :double))))

(defun get-nodes-by-element-type (element-type &key (tag -1) (return-parametric-coord t))
  "Get the nodes classified on the entity of tag `tag', for all the
elements of type `elementType'. The other arguments are treated as in
`getNodes'."
  (cffi:with-foreign-objects ((node-tags-out :pointer) (node-tags-n-out :unsigned-long) (coord-out :pointer) (coord-n-out :unsigned-long) (parametric-coord-out :pointer) (parametric-coord-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-nodes-by-element-type element-type node-tags-out node-tags-n-out coord-out coord-n-out parametric-coord-out parametric-coord-n-out tag (if return-parametric-coord 1 0) ierr))
    (values (foreign-array-to-list (cffi:mem-ref node-tags-out :pointer) (cffi:mem-ref node-tags-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref coord-out :pointer) (cffi:mem-ref coord-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref parametric-coord-out :pointer) (cffi:mem-ref parametric-coord-n-out :unsigned-long) :double))))

(defun get-node (node-tag)
  "Get the coordinates and the parametric coordinates (if any) of the
node with tag `tag', as well as the dimension `dim' and tag `tag' of
the entity on which the node is classified. This function relies on an
internal cache (a vector in case of dense node numbering, a map
otherwise); for large meshes accessing nodes in bulk is often
preferable."
  (cffi:with-foreign-objects ((coord-out :pointer) (coord-n-out :unsigned-long) (parametric-coord-out :pointer) (parametric-coord-n-out :unsigned-long) (dim-out :int) (tag-out :int))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-node node-tag coord-out coord-n-out parametric-coord-out parametric-coord-n-out dim-out tag-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref coord-out :pointer) (cffi:mem-ref coord-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref parametric-coord-out :pointer) (cffi:mem-ref parametric-coord-n-out :unsigned-long) :double) (cffi:mem-ref dim-out :int) (cffi:mem-ref tag-out :int))))

(defun set-node (node-tag coord parametric-coord)
  "Set the coordinates and the parametric coordinates (if any) of the
node with tag `tag'. This function relies on an internal cache (a
vector in case of dense node numbering, a map otherwise); for large
meshes accessing nodes in bulk is often preferable."
  (with-foreign-array (coord-ptr coord-n coord :double)
      (with-foreign-array (parametric-coord-ptr parametric-coord-n parametric-coord :double)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-node node-tag coord-ptr coord-n parametric-coord-ptr parametric-coord-n ierr)))))

(defun rebuild-node-cache (&key (only-if-necessary t))
  "Rebuild the node cache."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-rebuild-node-cache (if only-if-necessary 1 0) ierr)))

(defun rebuild-element-cache (&key (only-if-necessary t))
  "Rebuild the element cache."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-rebuild-element-cache (if only-if-necessary 1 0) ierr)))

(defun get-nodes-for-physical-group (dim tag)
  "Get the nodes from all the elements belonging to the physical group of
dimension `dim' and tag `tag'. `nodeTags' contains the node tags;
`coord' is a vector of length 3 times the length of `nodeTags' that
contains the x, y, z coordinates of the nodes, concatenated: [n1x,
n1y, n1z, n2x, ...]."
  (cffi:with-foreign-objects ((node-tags-out :pointer) (node-tags-n-out :unsigned-long) (coord-out :pointer) (coord-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-nodes-for-physical-group dim tag node-tags-out node-tags-n-out coord-out coord-n-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref node-tags-out :pointer) (cffi:mem-ref node-tags-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref coord-out :pointer) (cffi:mem-ref coord-n-out :unsigned-long) :double))))

(defun get-max-node-tag ()
  "Get the maximum tag `maxTag' of a node in the mesh."
  (cffi:with-foreign-object (max-tag-out :unsigned-long)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-max-node-tag max-tag-out ierr))
    (cffi:mem-ref max-tag-out :unsigned-long)))

(defun add-nodes (dim tag node-tags coord &key (parametric-coord '()))
  "Add nodes classified on the model entity of dimension `dim' and tag
`tag'. `nodeTags' contains the node tags (their unique, strictly
positive identification numbers). `coord' is a vector of length 3
times the length of `nodeTags' that contains the x, y, z coordinates
of the nodes, concatenated: [n1x, n1y, n1z, n2x, ...]. The optional
`parametricCoord' vector contains the parametric coordinates of the
nodes, if any. The length of `parametricCoord' can be 0 or `dim' times
the length of `nodeTags'. If the `nodeTags' vector is empty, new tags
are automatically assigned to the nodes."
  (with-foreign-array (node-tags-ptr node-tags-n node-tags :unsigned-long)
      (with-foreign-array (coord-ptr coord-n coord :double)
      (with-foreign-array (parametric-coord-ptr parametric-coord-n parametric-coord :double)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-add-nodes dim tag node-tags-ptr node-tags-n coord-ptr coord-n parametric-coord-ptr parametric-coord-n ierr))))))

(defun reclassify-nodes ()
  "Reclassify all nodes on their associated model entity, based on the
elements. Can be used when importing nodes in bulk (e.g. by
associating them all to a single volume), to reclassify them correctly
on model surfaces, curves, etc. after the elements have been set."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-reclassify-nodes  ierr)))

(defun relocate-nodes (&key (dim -1) (tag -1))
  "Relocate the nodes classified on the entity of dimension `dim' and tag
`tag' using their parametric coordinates. If `tag' < 0, relocate the
nodes for all entities of dimension `dim'. If `dim' and `tag' are
negative, relocate all the nodes in the mesh."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-relocate-nodes dim tag ierr)))

(defun get-elements (&key (dim -1) (tag -1))
  "Get the elements classified on the entity of dimension `dim' and tag
`tag'. If `tag' < 0, get the elements for all entities of dimension
`dim'. If `dim' and `tag' are negative, get all the elements in the
mesh. `elementTypes' contains the MSH types of the elements (e.g. `2'
for 3-node triangles: see `getElementProperties' to obtain the
properties for a given element type). `elementTags' is a vector of the
same length as `elementTypes'; each entry is a vector containing the
tags (unique, strictly positive identifiers) of the elements of the
corresponding type. `nodeTags' is also a vector of the same length as
`elementTypes'; each entry is a vector of length equal to the number
of elements of the given type times the number N of nodes for this
type of element, that contains the node tags of all the elements of
the given type, concatenated: [e1n1, e1n2, ..., e1nN, e2n1, ...]."
  (cffi:with-foreign-objects ((element-types-out :pointer) (element-types-n-out :unsigned-long) (element-tags-out :pointer) (element-tags-n-out :pointer) (element-tags-nn-out :unsigned-long) (node-tags-out :pointer) (node-tags-n-out :pointer) (node-tags-nn-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-elements element-types-out element-types-n-out element-tags-out element-tags-n-out element-tags-nn-out node-tags-out node-tags-n-out node-tags-nn-out dim tag ierr))
    (values (foreign-array-to-list (cffi:mem-ref element-types-out :pointer) (cffi:mem-ref element-types-n-out :unsigned-long) :int) (foreign-vectors-to-list (cffi:mem-ref element-tags-out :pointer) (cffi:mem-ref element-tags-n-out :pointer) (cffi:mem-ref element-tags-nn-out :unsigned-long) :unsigned-long) (foreign-vectors-to-list (cffi:mem-ref node-tags-out :pointer) (cffi:mem-ref node-tags-n-out :pointer) (cffi:mem-ref node-tags-nn-out :unsigned-long) :unsigned-long))))

(defun get-element (element-tag)
  "Get the type and node tags of the element with tag `elementTag', as
well as the dimension `dim' and tag `tag' of the entity on which the
element is classified. This function relies on an internal cache (a
vector in case of dense element numbering, a map otherwise); for large
meshes accessing elements in bulk is often preferable."
  (cffi:with-foreign-objects ((element-type-out :int) (node-tags-out :pointer) (node-tags-n-out :unsigned-long) (dim-out :int) (tag-out :int))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-element element-tag element-type-out node-tags-out node-tags-n-out dim-out tag-out ierr))
    (values (cffi:mem-ref element-type-out :int) (foreign-array-to-list (cffi:mem-ref node-tags-out :pointer) (cffi:mem-ref node-tags-n-out :unsigned-long) :unsigned-long) (cffi:mem-ref dim-out :int) (cffi:mem-ref tag-out :int))))

(defun get-element-by-coordinates (x y z &key (dim -1) (strict nil))
  "Search the mesh for an element located at coordinates (`x', `y', `z').
This function performs a search in a spatial octree. If an element is
found, return its tag, type and node tags, as well as the local
coordinates (`u', `v', `w') within the reference element corresponding
to search location. If `dim' is >= 0, only search for elements of the
given dimension. If `strict' is not set, use a tolerance to find
elements near the search location."
  (cffi:with-foreign-objects ((element-tag-out :unsigned-long) (element-type-out :int) (node-tags-out :pointer) (node-tags-n-out :unsigned-long) (u-out :double) (v-out :double) (w-out :double))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-element-by-coordinates (to-double x) (to-double y) (to-double z) element-tag-out element-type-out node-tags-out node-tags-n-out u-out v-out w-out dim (if strict 1 0) ierr))
    (values (cffi:mem-ref element-tag-out :unsigned-long) (cffi:mem-ref element-type-out :int) (foreign-array-to-list (cffi:mem-ref node-tags-out :pointer) (cffi:mem-ref node-tags-n-out :unsigned-long) :unsigned-long) (cffi:mem-ref u-out :double) (cffi:mem-ref v-out :double) (cffi:mem-ref w-out :double))))

(defun get-elements-by-coordinates (x y z &key (dim -1) (strict nil))
  "Search the mesh for element(s) located at coordinates (`x', `y', `z').
This function performs a search in a spatial octree. Return the tags
of all found elements in `elementTags'. Additional information about
the elements can be accessed through `getElement' and
`getLocalCoordinatesInElement'. If `dim' is >= 0, only search for
elements of the given dimension. If `strict' is not set, use a
tolerance to find elements near the search location."
  (cffi:with-foreign-objects ((element-tags-out :pointer) (element-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-elements-by-coordinates (to-double x) (to-double y) (to-double z) element-tags-out element-tags-n-out dim (if strict 1 0) ierr))
    (foreign-array-to-list (cffi:mem-ref element-tags-out :pointer) (cffi:mem-ref element-tags-n-out :unsigned-long) :unsigned-long)))

(defun get-local-coordinates-in-element (element-tag x y z)
  "Return the local coordinates (`u', `v', `w') within the element
`elementTag' corresponding to the model coordinates (`x', `y', `z').
This function relies on an internal cache (a vector in case of dense
element numbering, a map otherwise); for large meshes accessing
elements in bulk is often preferable."
  (cffi:with-foreign-objects ((u-out :double) (v-out :double) (w-out :double))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-local-coordinates-in-element element-tag (to-double x) (to-double y) (to-double z) u-out v-out w-out ierr))
    (values (cffi:mem-ref u-out :double) (cffi:mem-ref v-out :double) (cffi:mem-ref w-out :double))))

(defun get-element-types (&key (dim -1) (tag -1))
  "Get the types of elements in the entity of dimension `dim' and tag
`tag'. If `tag' < 0, get the types for all entities of dimension
`dim'. If `dim' and `tag' are negative, get all the types in the mesh."
  (cffi:with-foreign-objects ((element-types-out :pointer) (element-types-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-element-types element-types-out element-types-n-out dim tag ierr))
    (foreign-array-to-list (cffi:mem-ref element-types-out :pointer) (cffi:mem-ref element-types-n-out :unsigned-long) :int)))

(defun get-element-type (family-name order &key (serendip nil))
  "Return an element type given its family name `familyName' (\"Point\",
\"Line\", \"Triangle\", \"Quadrangle\", \"Tetrahedron\", \"Pyramid\",
\"Prism\", \"Hexahedron\") and polynomial order `order'. If `serendip'
is true, return the corresponding serendip element type (element
without interior nodes)."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-element-type family-name order (if serendip 1 0) ierr)))

(defun get-element-properties (element-type)
  "Get the properties of an element of type `elementType': its name
(`elementName'), dimension (`dim'), order (`order'), number of nodes
(`numNodes'), local coordinates of the nodes in the reference element
(`localNodeCoord' vector, of length `dim' times `numNodes') and number
of primary (first order) nodes (`numPrimaryNodes')."
  (cffi:with-foreign-objects ((element-name-out :pointer) (dim-out :int) (order-out :int) (num-nodes-out :int) (local-node-coord-out :pointer) (local-node-coord-n-out :unsigned-long) (num-primary-nodes-out :int))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-element-properties element-type element-name-out dim-out order-out num-nodes-out local-node-coord-out local-node-coord-n-out num-primary-nodes-out ierr))
    (values (foreign-string-result (cffi:mem-ref element-name-out :pointer)) (cffi:mem-ref dim-out :int) (cffi:mem-ref order-out :int) (cffi:mem-ref num-nodes-out :int) (foreign-array-to-list (cffi:mem-ref local-node-coord-out :pointer) (cffi:mem-ref local-node-coord-n-out :unsigned-long) :double) (cffi:mem-ref num-primary-nodes-out :int))))

(defun get-elements-by-type (element-type &key (tag -1) (task 0) (num-tasks 1))
  "Get the elements of type `elementType' classified on the entity of tag
`tag'. If `tag' < 0, get the elements for all entities. `elementTags'
is a vector containing the tags (unique, strictly positive
identifiers) of the elements of the corresponding type. `nodeTags' is
a vector of length equal to the number of elements of the given type
times the number N of nodes for this type of element, that contains
the node tags of all the elements of the given type, concatenated:
[e1n1, e1n2, ..., e1nN, e2n1, ...]. If `numTasks' > 1, only compute
and return the part of the data indexed by `task' (for C++ only;
output vectors must be preallocated)."
  (cffi:with-foreign-objects ((element-tags-out :pointer) (element-tags-n-out :unsigned-long) (node-tags-out :pointer) (node-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-elements-by-type element-type element-tags-out element-tags-n-out node-tags-out node-tags-n-out tag task num-tasks ierr))
    (values (foreign-array-to-list (cffi:mem-ref element-tags-out :pointer) (cffi:mem-ref element-tags-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref node-tags-out :pointer) (cffi:mem-ref node-tags-n-out :unsigned-long) :unsigned-long))))

(defun get-max-element-tag ()
  "Get the maximum tag `maxTag' of an element in the mesh."
  (cffi:with-foreign-object (max-tag-out :unsigned-long)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-max-element-tag max-tag-out ierr))
    (cffi:mem-ref max-tag-out :unsigned-long)))

(defun preallocate-elements-by-type (element-type element-tag node-tag &key (tag -1))
  "Preallocate data before calling `getElementsByType' with `numTasks' >
1. For C++ only."
  (cffi:with-foreign-objects ((element-tags-out :pointer) (element-tags-n-out :unsigned-long) (node-tags-out :pointer) (node-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-preallocate-elements-by-type element-type (if element-tag 1 0) (if node-tag 1 0) element-tags-out element-tags-n-out node-tags-out node-tags-n-out tag ierr))
    (values (foreign-array-to-list (cffi:mem-ref element-tags-out :pointer) (cffi:mem-ref element-tags-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref node-tags-out :pointer) (cffi:mem-ref node-tags-n-out :unsigned-long) :unsigned-long))))

(defun get-element-qualities (element-tags &key (quality-name "minSICN") (task 0) (num-tasks 1))
  "Get the quality `elementQualities' of the elements with tags
`elementTags'. `qualityType' is the requested quality measure:
\"minDetJac\" and \"maxDetJac\" for the adaptively computed minimal
and maximal Jacobian determinant, \"minSJ\" for the sampled minimal
scaled jacobien, \"minSICN\" for the sampled minimal signed inverted
condition number, \"minSIGE\" for the sampled signed inverted gradient
error, \"gamma\" for the ratio of the inscribed to circumcribed sphere
radius, \"innerRadius\" for the inner radius, \"outerRadius\" for the
outerRadius, \"minIsotropy\" for the minimum isotropy measure,
\"angleShape\" for the angle shape measure, \"minEdge\" for the
minimum straight edge length, \"maxEdge\" for the maximum straight
edge length, \"volume\" for the volume. If `numTasks' > 1, only
compute and return the part of the data indexed by `task' (for C++
only; output vector must be preallocated)."
  (with-foreign-array (element-tags-ptr element-tags-n element-tags :unsigned-long)
      (cffi:with-foreign-objects ((elements-quality-out :pointer) (elements-quality-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-element-qualities element-tags-ptr element-tags-n elements-quality-out elements-quality-n-out quality-name task num-tasks ierr))
    (foreign-array-to-list (cffi:mem-ref elements-quality-out :pointer) (cffi:mem-ref elements-quality-n-out :unsigned-long) :double))))

(defun add-elements (dim tag element-types element-tags node-tags)
  "Add elements classified on the entity of dimension `dim' and tag
`tag'. `types' contains the MSH types of the elements (e.g. `2' for
3-node triangles: see the Gmsh reference manual). `elementTags' is a
vector of the same length as `types'; each entry is a vector
containing the tags (unique, strictly positive identifiers) of the
elements of the corresponding type. `nodeTags' is also a vector of the
same length as `types'; each entry is a vector of length equal to the
number of elements of the given type times the number N of nodes per
element, that contains the node tags of all the elements of the given
type, concatenated: [e1n1, e1n2, ..., e1nN, e2n1, ...]."
  (with-foreign-array (element-types-ptr element-types-n element-types :int)
      (with-vector-vector-size (element-tags-ptrs element-tags-sizes element-tags-nn element-tags)
      (with-vector-vector-size (node-tags-ptrs node-tags-sizes node-tags-nn node-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-add-elements dim tag element-types-ptr element-types-n element-tags-ptrs element-tags-sizes element-tags-nn node-tags-ptrs node-tags-sizes node-tags-nn ierr))))))

(defun add-elements-by-type (tag element-type element-tags node-tags)
  "Add elements of type `elementType' classified on the entity of tag
`tag'. `elementTags' contains the tags (unique, strictly positive
identifiers) of the elements of the corresponding type. `nodeTags' is
a vector of length equal to the number of elements times the number N
of nodes per element, that contains the node tags of all the elements,
concatenated: [e1n1, e1n2, ..., e1nN, e2n1, ...]. If the `elementTag'
vector is empty, new tags are automatically assigned to the elements."
  (with-foreign-array (element-tags-ptr element-tags-n element-tags :unsigned-long)
      (with-foreign-array (node-tags-ptr node-tags-n node-tags :unsigned-long)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-add-elements-by-type tag element-type element-tags-ptr element-tags-n node-tags-ptr node-tags-n ierr)))))

(defun get-integration-points (element-type integration-type)
  "Get the numerical quadrature information for the given element type
`elementType' and integration rule `integrationType', where
`integrationType' concatenates the integration rule family name with
the desired order (e.g. \"Gauss4\" for a quadrature suited for
integrating 4th order polynomials). The \"CompositeGauss\" family uses
tensor-product rules based the 1D Gauss-Legendre rule; the \"Gauss\"
family uses an economic scheme when available (i.e. with a minimal
number of points), and falls back to \"CompositeGauss\" otherwise.
Note that integration points for the \"Gauss\" family can fall outside
of the reference element for high-order rules. `localCoord' contains
the u, v, w coordinates of the G integration points in the reference
element: [g1u, g1v, g1w, ..., gGu, gGv, gGw]. `weights' contains the
associated weights: [g1q, ..., gGq]."
  (cffi:with-foreign-objects ((local-coord-out :pointer) (local-coord-n-out :unsigned-long) (weights-out :pointer) (weights-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-integration-points element-type integration-type local-coord-out local-coord-n-out weights-out weights-n-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref local-coord-out :pointer) (cffi:mem-ref local-coord-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref weights-out :pointer) (cffi:mem-ref weights-n-out :unsigned-long) :double))))

(defun get-jacobians (element-type local-coord &key (tag -1) (task 0) (num-tasks 1))
  "Get the Jacobians of all the elements of type `elementType' classified
on the entity of tag `tag', at the G evaluation points `localCoord'
given as concatenated u, v, w coordinates in the reference element
[g1u, g1v, g1w, ..., gGu, gGv, gGw]. Data is returned by element, with
elements in the same order as in `getElements' and
`getElementsByType'. `jacobians' contains for each element the 9
entries of the 3x3 Jacobian matrix at each evaluation point. The
matrix is returned by column: [e1g1Jxu, e1g1Jyu, e1g1Jzu, e1g1Jxv,
..., e1g1Jzw, e1g2Jxu, ..., e1gGJzw, e2g1Jxu, ...], with Jxu = dx/du,
Jyu = dy/du, etc. `determinants' contains for each element the
determinant of the Jacobian matrix at each evaluation point: [e1g1,
e1g2, ... e1gG, e2g1, ...]. `coord' contains for each element the x,
y, z coordinates of the evaluation points. If `tag' < 0, get the
Jacobian data for all entities. If `numTasks' > 1, only compute and
return the part of the data indexed by `task' (for C++ only; output
vectors must be preallocated)."
  (with-foreign-array (local-coord-ptr local-coord-n local-coord :double)
      (cffi:with-foreign-objects ((jacobians-out :pointer) (jacobians-n-out :unsigned-long) (determinants-out :pointer) (determinants-n-out :unsigned-long) (coord-out :pointer) (coord-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-jacobians element-type local-coord-ptr local-coord-n jacobians-out jacobians-n-out determinants-out determinants-n-out coord-out coord-n-out tag task num-tasks ierr))
    (values (foreign-array-to-list (cffi:mem-ref jacobians-out :pointer) (cffi:mem-ref jacobians-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref determinants-out :pointer) (cffi:mem-ref determinants-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref coord-out :pointer) (cffi:mem-ref coord-n-out :unsigned-long) :double)))))

(defun preallocate-jacobians (element-type num-evaluation-points allocate-jacobians allocate-determinants allocate-coord &key (tag -1))
  "Preallocate data before calling `getJacobians' with `numTasks' > 1.
For C++ only."
  (cffi:with-foreign-objects ((jacobians-out :pointer) (jacobians-n-out :unsigned-long) (determinants-out :pointer) (determinants-n-out :unsigned-long) (coord-out :pointer) (coord-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-preallocate-jacobians element-type num-evaluation-points (if allocate-jacobians 1 0) (if allocate-determinants 1 0) (if allocate-coord 1 0) jacobians-out jacobians-n-out determinants-out determinants-n-out coord-out coord-n-out tag ierr))
    (values (foreign-array-to-list (cffi:mem-ref jacobians-out :pointer) (cffi:mem-ref jacobians-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref determinants-out :pointer) (cffi:mem-ref determinants-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref coord-out :pointer) (cffi:mem-ref coord-n-out :unsigned-long) :double))))

(defun get-jacobian (element-tag local-coord)
  "Get the Jacobian for a single element `elementTag', at the G
evaluation points `localCoord' given as concatenated u, v, w
coordinates in the reference element [g1u, g1v, g1w, ..., gGu, gGv,
gGw]. `jacobians' contains the 9 entries of the 3x3 Jacobian matrix at
each evaluation point. The matrix is returned by column: [e1g1Jxu,
e1g1Jyu, e1g1Jzu, e1g1Jxv, ..., e1g1Jzw, e1g2Jxu, ..., e1gGJzw,
e2g1Jxu, ...], with Jxu = dx/du, Jyu = dy/du, etc. `determinants'
contains the determinant of the Jacobian matrix at each evaluation
point. `coord' contains the x, y, z coordinates of the evaluation
points. This function relies on an internal cache (a vector in case of
dense element numbering, a map otherwise); for large meshes accessing
Jacobians in bulk is often preferable."
  (with-foreign-array (local-coord-ptr local-coord-n local-coord :double)
      (cffi:with-foreign-objects ((jacobians-out :pointer) (jacobians-n-out :unsigned-long) (determinants-out :pointer) (determinants-n-out :unsigned-long) (coord-out :pointer) (coord-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-jacobian element-tag local-coord-ptr local-coord-n jacobians-out jacobians-n-out determinants-out determinants-n-out coord-out coord-n-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref jacobians-out :pointer) (cffi:mem-ref jacobians-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref determinants-out :pointer) (cffi:mem-ref determinants-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref coord-out :pointer) (cffi:mem-ref coord-n-out :unsigned-long) :double)))))

(defun get-basis-functions (element-type local-coord function-space-type &key (wanted-orientations '()))
  "Get the basis functions of the element of type `elementType' at the
evaluation points `localCoord' (given as concatenated u, v, w
coordinates in the reference element [g1u, g1v, g1w, ..., gGu, gGv,
gGw]), for the function space `functionSpaceType'. Currently supported
function spaces include \"Lagrange\" and \"GradLagrange\" for
isoparametric Lagrange basis functions and their gradient in the u, v,
w coordinates of the reference element; \"LagrangeN\" and
\"GradLagrangeN\", with N = 1, 2, ..., for N-th order Lagrange basis
functions; \"H1LegendreN\" and \"GradH1LegendreN\", with N = 1, 2,
..., for N-th order hierarchical H1 Legendre functions;
\"HcurlLegendreN\" and \"CurlHcurlLegendreN\", with N = 1, 2, ..., for
N-th order curl-conforming basis functions. `numComponents' returns
the number C of components of a basis function (e.g. 1 for scalar
functions and 3 for vector functions). `basisFunctions' returns the
value of the N basis functions at the evaluation points, i.e. [g1f1,
g1f2, ..., g1fN, g2f1, ...] when C == 1 or [g1f1u, g1f1v, g1f1w,
g1f2u, ..., g1fNw, g2f1u, ...] when C == 3. For basis functions that
depend on the orientation of the elements, all values for the first
orientation are returned first, followed by values for the second,
etc. `numOrientations' returns the overall number of orientations. If
the `wantedOrientations' vector is not empty, only return the values
for the desired orientation indices."
  (with-foreign-array (local-coord-ptr local-coord-n local-coord :double)
      (with-foreign-array (wanted-orientations-ptr wanted-orientations-n wanted-orientations :int)
      (cffi:with-foreign-objects ((num-components-out :int) (basis-functions-out :pointer) (basis-functions-n-out :unsigned-long) (num-orientations-out :int))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-basis-functions element-type local-coord-ptr local-coord-n function-space-type num-components-out basis-functions-out basis-functions-n-out num-orientations-out wanted-orientations-ptr wanted-orientations-n ierr))
    (values (cffi:mem-ref num-components-out :int) (foreign-array-to-list (cffi:mem-ref basis-functions-out :pointer) (cffi:mem-ref basis-functions-n-out :unsigned-long) :double) (cffi:mem-ref num-orientations-out :int))))))

(defun get-basis-functions-orientation (element-type function-space-type &key (tag -1) (task 0) (num-tasks 1))
  "Get the orientation index of the elements of type `elementType' in the
entity of tag `tag'. The arguments have the same meaning as in
`getBasisFunctions'. `basisFunctionsOrientation' is a vector giving
for each element the orientation index in the values returned by
`getBasisFunctions'. For Lagrange basis functions the call is
superfluous as it will return a vector of zeros. If `numTasks' > 1,
only compute and return the part of the data indexed by `task' (for
C++ only; output vector must be preallocated)."
  (cffi:with-foreign-objects ((basis-functions-orientation-out :pointer) (basis-functions-orientation-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-basis-functions-orientation element-type function-space-type basis-functions-orientation-out basis-functions-orientation-n-out tag task num-tasks ierr))
    (foreign-array-to-list (cffi:mem-ref basis-functions-orientation-out :pointer) (cffi:mem-ref basis-functions-orientation-n-out :unsigned-long) :int)))

(defun get-basis-functions-orientation-for-element (element-tag function-space-type)
  "Get the orientation of a single element `elementTag'."
  (cffi:with-foreign-object (basis-functions-orientation-out :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-basis-functions-orientation-for-element element-tag function-space-type basis-functions-orientation-out ierr))
    (cffi:mem-ref basis-functions-orientation-out :int)))

(defun get-number-of-orientations (element-type function-space-type)
  "Get the number of possible orientations for elements of type
`elementType' and function space named `functionSpaceType'."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-number-of-orientations element-type function-space-type ierr)))

(defun preallocate-basis-functions-orientation (element-type &key (tag -1))
  "Preallocate data before calling `getBasisFunctionsOrientation' with
`numTasks' > 1. For C++ only."
  (cffi:with-foreign-objects ((basis-functions-orientation-out :pointer) (basis-functions-orientation-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-preallocate-basis-functions-orientation element-type basis-functions-orientation-out basis-functions-orientation-n-out tag ierr))
    (foreign-array-to-list (cffi:mem-ref basis-functions-orientation-out :pointer) (cffi:mem-ref basis-functions-orientation-n-out :unsigned-long) :int)))

(defun get-edges (node-tags)
  "Get the global unique mesh edge identifiers `edgeTags' and
orientations `edgeOrientation' for an input list of node tag pairs
defining these edges, concatenated in the vector `nodeTags'. Mesh
edges are created e.g. by `createEdges()', `getKeys()' or
`addEdges()'. The reference positive orientation is n1 < n2, where n1
and n2 are the tags of the two edge nodes, which corresponds to the
local orientation of edge-based basis functions as well."
  (with-foreign-array (node-tags-ptr node-tags-n node-tags :unsigned-long)
      (cffi:with-foreign-objects ((edge-tags-out :pointer) (edge-tags-n-out :unsigned-long) (edge-orientations-out :pointer) (edge-orientations-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-edges node-tags-ptr node-tags-n edge-tags-out edge-tags-n-out edge-orientations-out edge-orientations-n-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref edge-tags-out :pointer) (cffi:mem-ref edge-tags-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref edge-orientations-out :pointer) (cffi:mem-ref edge-orientations-n-out :unsigned-long) :int)))))

(defun get-faces (face-type node-tags)
  "Get the global unique mesh face identifiers `faceTags' and
orientations `faceOrientations' for an input list of a multiple of
three (if `faceType' == 3) or four (if `faceType' == 4) node tags
defining these faces, concatenated in the vector `nodeTags'. Mesh
faces are created e.g. by `createFaces()', `getKeys()' or
`addFaces()'."
  (with-foreign-array (node-tags-ptr node-tags-n node-tags :unsigned-long)
      (cffi:with-foreign-objects ((face-tags-out :pointer) (face-tags-n-out :unsigned-long) (face-orientations-out :pointer) (face-orientations-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-faces face-type node-tags-ptr node-tags-n face-tags-out face-tags-n-out face-orientations-out face-orientations-n-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref face-tags-out :pointer) (cffi:mem-ref face-tags-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref face-orientations-out :pointer) (cffi:mem-ref face-orientations-n-out :unsigned-long) :int)))))

(defun create-edges (&key (dim-tags '()))
  "Create unique mesh edges for the entities `dimTags', given as a vector
of (dim, tag) pairs."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-create-edges dim-tags-ptr dim-tags-n ierr))))

(defun create-faces (&key (dim-tags '()))
  "Create unique mesh faces for the entities `dimTags', given as a vector
of (dim, tag) pairs."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-create-faces dim-tags-ptr dim-tags-n ierr))))

(defun get-all-edges ()
  "Get the global unique identifiers `edgeTags' and the nodes `edgeNodes'
of the edges in the mesh. Mesh edges are created e.g. by
`createEdges()', `getKeys()' or addEdges()."
  (cffi:with-foreign-objects ((edge-tags-out :pointer) (edge-tags-n-out :unsigned-long) (edge-nodes-out :pointer) (edge-nodes-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-all-edges edge-tags-out edge-tags-n-out edge-nodes-out edge-nodes-n-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref edge-tags-out :pointer) (cffi:mem-ref edge-tags-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref edge-nodes-out :pointer) (cffi:mem-ref edge-nodes-n-out :unsigned-long) :unsigned-long))))

(defun get-all-faces (face-type)
  "Get the global unique identifiers `faceTags' and the nodes `faceNodes'
of the faces of type `faceType' in the mesh. Mesh faces are created
e.g. by `createFaces()', `getKeys()' or addFaces()."
  (cffi:with-foreign-objects ((face-tags-out :pointer) (face-tags-n-out :unsigned-long) (face-nodes-out :pointer) (face-nodes-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-all-faces face-type face-tags-out face-tags-n-out face-nodes-out face-nodes-n-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref face-tags-out :pointer) (cffi:mem-ref face-tags-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref face-nodes-out :pointer) (cffi:mem-ref face-nodes-n-out :unsigned-long) :unsigned-long))))

(defun add-edges (edge-tags edge-nodes)
  "Add mesh edges defined by their global unique identifiers `edgeTags'
and their nodes `edgeNodes'."
  (with-foreign-array (edge-tags-ptr edge-tags-n edge-tags :unsigned-long)
      (with-foreign-array (edge-nodes-ptr edge-nodes-n edge-nodes :unsigned-long)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-add-edges edge-tags-ptr edge-tags-n edge-nodes-ptr edge-nodes-n ierr)))))

(defun add-faces (face-type face-tags face-nodes)
  "Add mesh faces of type `faceType' defined by their global unique
identifiers `faceTags' and their nodes `faceNodes'."
  (with-foreign-array (face-tags-ptr face-tags-n face-tags :unsigned-long)
      (with-foreign-array (face-nodes-ptr face-nodes-n face-nodes :unsigned-long)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-add-faces face-type face-tags-ptr face-tags-n face-nodes-ptr face-nodes-n ierr)))))

(defun get-keys (element-type function-space-type &key (tag -1) (return-coord t))
  "Generate the pair of keys for the elements of type `elementType' in
the entity of tag `tag', for the `functionSpaceType' function space.
Each pair (`typeKey', `entityKey') uniquely identifies a basis
function in the function space. If `returnCoord' is set, the `coord'
vector contains the x, y, z coordinates locating basis functions for
sorting purposes. Warning: this is an experimental feature and will
probably change in a future release."
  (cffi:with-foreign-objects ((type-keys-out :pointer) (type-keys-n-out :unsigned-long) (entity-keys-out :pointer) (entity-keys-n-out :unsigned-long) (coord-out :pointer) (coord-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-keys element-type function-space-type type-keys-out type-keys-n-out entity-keys-out entity-keys-n-out coord-out coord-n-out tag (if return-coord 1 0) ierr))
    (values (foreign-array-to-list (cffi:mem-ref type-keys-out :pointer) (cffi:mem-ref type-keys-n-out :unsigned-long) :int) (foreign-array-to-list (cffi:mem-ref entity-keys-out :pointer) (cffi:mem-ref entity-keys-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref coord-out :pointer) (cffi:mem-ref coord-n-out :unsigned-long) :double))))

(defun get-keys-for-element (element-tag function-space-type &key (return-coord t))
  "Get the pair of keys for a single element `elementTag'."
  (cffi:with-foreign-objects ((type-keys-out :pointer) (type-keys-n-out :unsigned-long) (entity-keys-out :pointer) (entity-keys-n-out :unsigned-long) (coord-out :pointer) (coord-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-keys-for-element element-tag function-space-type type-keys-out type-keys-n-out entity-keys-out entity-keys-n-out coord-out coord-n-out (if return-coord 1 0) ierr))
    (values (foreign-array-to-list (cffi:mem-ref type-keys-out :pointer) (cffi:mem-ref type-keys-n-out :unsigned-long) :int) (foreign-array-to-list (cffi:mem-ref entity-keys-out :pointer) (cffi:mem-ref entity-keys-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref coord-out :pointer) (cffi:mem-ref coord-n-out :unsigned-long) :double))))

(defun get-number-of-keys (element-type function-space-type)
  "Get the number of keys by elements of type `elementType' for function
space named `functionSpaceType'."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-number-of-keys element-type function-space-type ierr)))

(defun get-keys-information (type-keys entity-keys element-type function-space-type)
  "Get information about the pair of `keys'. `infoKeys' returns
information about the functions associated with the pairs (`typeKeys',
`entityKey'). `infoKeys[0].first' describes the type of function (0
for  vertex function, 1 for edge function, 2 for face function and 3
for bubble function). `infoKeys[0].second' gives the order of the
function associated with the key. Warning: this is an experimental
feature and will probably change in a future release."
  (with-foreign-array (type-keys-ptr type-keys-n type-keys :int)
      (with-foreign-array (entity-keys-ptr entity-keys-n entity-keys :unsigned-long)
      (cffi:with-foreign-objects ((info-keys-out :pointer) (info-keys-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-keys-information type-keys-ptr type-keys-n entity-keys-ptr entity-keys-n element-type function-space-type info-keys-out info-keys-n-out ierr))
    (dim-tags-to-pairs (cffi:mem-ref info-keys-out :pointer) (cffi:mem-ref info-keys-n-out :unsigned-long))))))

(defun get-barycenters (element-type tag fast primary &key (task 0) (num-tasks 1))
  "Get the barycenters of all elements of type `elementType' classified
on the entity of tag `tag'. If `primary' is set, only the primary
nodes of the elements are taken into account for the barycenter
calculation. If `fast' is set, the function returns the sum of the
primary node coordinates (without normalizing by the number of nodes).
If `tag' < 0, get the barycenters for all entities. If `numTasks' > 1,
only compute and return the part of the data indexed by `task' (for
C++ only; output vector must be preallocated)."
  (cffi:with-foreign-objects ((barycenters-out :pointer) (barycenters-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-barycenters element-type tag (if fast 1 0) (if primary 1 0) barycenters-out barycenters-n-out task num-tasks ierr))
    (foreign-array-to-list (cffi:mem-ref barycenters-out :pointer) (cffi:mem-ref barycenters-n-out :unsigned-long) :double)))

(defun preallocate-barycenters (element-type &key (tag -1))
  "Preallocate data before calling `getBarycenters' with `numTasks' > 1.
For C++ only."
  (cffi:with-foreign-objects ((barycenters-out :pointer) (barycenters-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-preallocate-barycenters element-type barycenters-out barycenters-n-out tag ierr))
    (foreign-array-to-list (cffi:mem-ref barycenters-out :pointer) (cffi:mem-ref barycenters-n-out :unsigned-long) :double)))

(defun get-element-edge-nodes (element-type &key (tag -1) (primary nil) (task 0) (num-tasks 1))
  "Get the nodes on the edges of all elements of type `elementType'
classified on the entity of tag `tag'. `nodeTags' contains the node
tags of the edges for all the elements: [e1a1n1, e1a1n2, e1a2n1, ...].
Data is returned by element, with elements in the same order as in
`getElements' and `getElementsByType'. If `primary' is set, only the
primary (begin/end) nodes of the edges are returned. If `tag' < 0, get
the edge nodes for all entities. If `numTasks' > 1, only compute and
return the part of the data indexed by `task' (for C++ only; output
vector must be preallocated)."
  (cffi:with-foreign-objects ((node-tags-out :pointer) (node-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-element-edge-nodes element-type node-tags-out node-tags-n-out tag (if primary 1 0) task num-tasks ierr))
    (foreign-array-to-list (cffi:mem-ref node-tags-out :pointer) (cffi:mem-ref node-tags-n-out :unsigned-long) :unsigned-long)))

(defun get-element-face-nodes (element-type face-type &key (tag -1) (primary nil) (task 0) (num-tasks 1))
  "Get the nodes on the faces of type `faceType' (3 for triangular faces,
4 for quadrangular faces) of all elements of type `elementType'
classified on the entity of tag `tag'. `nodeTags' contains the node
tags of the faces for all elements: [e1f1n1, ..., e1f1nFaceType,
e1f2n1, ...]. Data is returned by element, with elements in the same
order as in `getElements' and `getElementsByType'. If `primary' is
set, only the primary (corner) nodes of the faces are returned. If
`tag' < 0, get the face nodes for all entities. If `numTasks' > 1,
only compute and return the part of the data indexed by `task' (for
C++ only; output vector must be preallocated)."
  (cffi:with-foreign-objects ((node-tags-out :pointer) (node-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-element-face-nodes element-type face-type node-tags-out node-tags-n-out tag (if primary 1 0) task num-tasks ierr))
    (foreign-array-to-list (cffi:mem-ref node-tags-out :pointer) (cffi:mem-ref node-tags-n-out :unsigned-long) :unsigned-long)))

(defun get-ghost-elements (dim tag)
  "Get the ghost elements `elementTags' and their associated `partitions'
stored in the ghost entity of dimension `dim' and tag `tag'."
  (cffi:with-foreign-objects ((element-tags-out :pointer) (element-tags-n-out :unsigned-long) (partitions-out :pointer) (partitions-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-ghost-elements dim tag element-tags-out element-tags-n-out partitions-out partitions-n-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref element-tags-out :pointer) (cffi:mem-ref element-tags-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref partitions-out :pointer) (cffi:mem-ref partitions-n-out :unsigned-long) :int))))

(defun set-size (dim-tags size)
  "Set a mesh size constraint on the model entities `dimTags', given as a
vector of (dim, tag) pairs. Currently only entities of dimension 0
(points) are handled."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-size dim-tags-ptr dim-tags-n (to-double size) ierr))))

(defun get-sizes (dim-tags)
  "Get the mesh size constraints (if any) associated with the model
entities `dimTags', given as a vector of (dim, tag) pairs. A zero
entry in the output `sizes' vector indicates that no size constraint
is specified on the corresponding entity."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (cffi:with-foreign-objects ((sizes-out :pointer) (sizes-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-sizes dim-tags-ptr dim-tags-n sizes-out sizes-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref sizes-out :pointer) (cffi:mem-ref sizes-n-out :unsigned-long) :double))))

(defun set-size-at-parametric-points (dim tag parametric-coord sizes)
  "Set mesh size constraints at the given parametric points
`parametricCoord' on the model entity of dimension `dim' and tag
`tag'. Currently only entities of dimension 1 (lines) are handled."
  (with-foreign-array (parametric-coord-ptr parametric-coord-n parametric-coord :double)
      (with-foreign-array (sizes-ptr sizes-n sizes :double)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-size-at-parametric-points dim tag parametric-coord-ptr parametric-coord-n sizes-ptr sizes-n ierr)))))

(defun set-size-callback (callback)
  "Set a mesh size callback for the current model. The callback function
should take six arguments as input (`dim', `tag', `x', `y', `z' and
`lc'). The first two integer arguments correspond to the dimension
`dim' and tag `tag' of the entity being meshed. The next four double
precision arguments correspond to the coordinates `x', `y' and `z'
around which to prescribe the mesh size and to the mesh size `lc' that
would be prescribed if the callback had not been called. The callback
function should return a double precision number specifying the
desired mesh size; returning `lc' is equivalent to a no-op."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-size-callback callback (cffi:null-pointer) ierr)))

(defun remove-size-callback ()
  "Remove the mesh size callback from the current model."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-remove-size-callback  ierr)))

(defun set-transfinite-curve (tag num-nodes &key (mesh-type "Progression") (coef 1.0))
  "Set a transfinite meshing constraint on the curve `tag', with
`numNodes' nodes distributed according to `meshType' and `coef'.
Currently supported types are \"Progression\" (geometrical progression
with power `coef'), \"Bump\" (refinement toward both extremities of
the curve) and \"Beta\" (beta law)."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-transfinite-curve tag num-nodes mesh-type (to-double coef) ierr)))

(defun set-transfinite-surface (tag &key (arrangement "Left") (corner-tags '()))
  "Set a transfinite meshing constraint on the surface `tag'.
`arrangement' describes the arrangement of the triangles when the
surface is not flagged as recombined: currently supported values are
\"Left\", \"Right\", \"AlternateLeft\" and \"AlternateRight\".
`cornerTags' can be used to specify the (3 or 4) corners of the
transfinite interpolation explicitly; specifying the corners
explicitly is mandatory if the surface has more that 3 or 4 points on
its boundary."
  (with-foreign-array (corner-tags-ptr corner-tags-n corner-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-transfinite-surface tag arrangement corner-tags-ptr corner-tags-n ierr))))

(defun set-transfinite-volume (tag &key (corner-tags '()))
  "Set a transfinite meshing constraint on the surface `tag'.
`cornerTags' can be used to specify the (6 or 8) corners of the
transfinite interpolation explicitly."
  (with-foreign-array (corner-tags-ptr corner-tags-n corner-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-transfinite-volume tag corner-tags-ptr corner-tags-n ierr))))

(defun set-transfinite-automatic (&key (dim-tags '()) (corner-angle 2.35) (recombine t))
  "Set transfinite meshing constraints on the model entities in
`dimTags', given as a vector of (dim, tag) pairs. Transfinite meshing
constraints are added to the curves of the quadrangular surfaces and
to the faces of 6-sided volumes. Quadragular faces with a corner angle
superior to `cornerAngle' (in radians) are ignored. The number of
points is automatically determined from the sizing constraints. If
`dimTag' is empty, the constraints are applied to all entities in the
model. If `recombine' is true, the recombine flag is automatically set
on the transfinite surfaces."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-transfinite-automatic dim-tags-ptr dim-tags-n (to-double corner-angle) (if recombine 1 0) ierr))))

(defun set-recombine (dim tag &key (angle 45.0))
  "Set a recombination meshing constraint on the model entity of
dimension `dim' and tag `tag'. Currently only entities of dimension 2
(to recombine triangles into quadrangles) are supported; `angle'
specifies the threshold angle for the simple recombination algorithm.."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-recombine dim tag (to-double angle) ierr)))

(defun set-smoothing (dim tag val)
  "Set a smoothing meshing constraint on the model entity of dimension
`dim' and tag `tag'. `val' iterations of a Laplace smoother are
applied."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-smoothing dim tag val ierr)))

(defun set-reverse (dim tag &key (val t))
  "Set a reverse meshing constraint on the model entity of dimension
`dim' and tag `tag'. If `val' is true, the mesh orientation will be
reversed with respect to the natural mesh orientation (i.e. the
orientation consistent with the orientation of the geometry). If `val'
is false, the mesh is left as-is."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-reverse dim tag (if val 1 0) ierr)))

(defun set-algorithm (dim tag val)
  "Set the meshing algorithm on the model entity of dimension `dim' and
tag `tag'. Supported values are those of the `Mesh.Algorithm' option,
as listed in the Gmsh reference manual. Currently only supported for
`dim' == 2."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-algorithm dim tag val ierr)))

(defun set-size-from-boundary (dim tag val)
  "Force the mesh size to be extended from the boundary, or not, for the
model entity of dimension `dim' and tag `tag'. Currently only
supported for `dim' == 2."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-size-from-boundary dim tag val ierr)))

(defun set-compound (dim tags)
  "Set a compound meshing constraint on the model entities of dimension
`dim' and tags `tags'. During meshing, compound entities are treated
as a single discrete entity, which is automatically reparametrized."
  (with-foreign-array (tags-ptr tags-n tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-compound dim tags-ptr tags-n ierr))))

(defun set-outward-orientation (tag)
  "Set meshing constraints on the bounding surfaces of the volume of tag
`tag' so that all surfaces are oriented with outward pointing normals;
and if a mesh already exists, reorient it. Currently only available
with the OpenCASCADE kernel, as it relies on the STL triangulation."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-outward-orientation tag ierr)))

(defun remove-constraints (&key (dim-tags '()))
  "Remove all meshing constraints from the model entities `dimTags',
given as a vector of (dim, tag) pairs. If `dimTags' is empty, remove
all constraings."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-remove-constraints dim-tags-ptr dim-tags-n ierr))))

(defun embed (dim tags in-dim in-tag)
  "Embed the model entities of dimension `dim' and tags `tags' in the
(`inDim', `inTag') model entity. The dimension `dim' can 0, 1 or 2 and
must be strictly smaller than `inDim', which must be either 2 or 3.
The embedded entities should not intersect each other or be part of
the boundary of the entity `inTag', whose mesh will conform to the
mesh of the embedded entities. With the OpenCASCADE kernel, if the
`fragment' operation is applied to entities of different dimensions,
the lower dimensional entities will be automatically embedded in the
higher dimensional entities if they are not on their boundary."
  (with-foreign-array (tags-ptr tags-n tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-embed dim tags-ptr tags-n in-dim in-tag ierr))))

(defun remove-embedded (dim-tags &key (dim -1))
  "Remove embedded entities from the model entities `dimTags', given as a
vector of (dim, tag) pairs. if `dim' is >= 0, only remove embedded
entities of the given dimension (e.g. embedded points if `dim' == 0)."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-remove-embedded dim-tags-ptr dim-tags-n dim ierr))))

(defun get-embedded (dim tag)
  "Get the entities (if any) embedded in the model entity of dimension
`dim' and tag `tag'."
  (cffi:with-foreign-objects ((dim-tags-out :pointer) (dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-embedded dim tag dim-tags-out dim-tags-n-out ierr))
    (dim-tags-to-pairs (cffi:mem-ref dim-tags-out :pointer) (cffi:mem-ref dim-tags-n-out :unsigned-long))))

(defun reorder-elements (element-type tag ordering)
  "Reorder the elements of type `elementType' classified on the entity of
tag `tag' according to the `ordering' vector."
  (with-foreign-array (ordering-ptr ordering-n ordering :unsigned-long)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-reorder-elements element-type tag ordering-ptr ordering-n ierr))))

(defun compute-renumbering (&key (method "RCMK") (element-tags '()))
  "Compute a renumbering vector `newTags' corresponding to the input tags
`oldTags' for a given list of element tags `elementTags'. If
`elementTags' is empty, compute the renumbering on the full mesh. If
`method' is equal to \"RCMK\", compute a node renumering with Reverse
Cuthill McKee. If `method' is equal to \"Hilbert\", compute a node
renumering along a Hilbert curve. If `method' is equal to \"Metis\",
compute a node renumering using Metis. Element renumbering is not
available yet."
  (with-foreign-array (element-tags-ptr element-tags-n element-tags :unsigned-long)
      (cffi:with-foreign-objects ((old-tags-out :pointer) (old-tags-n-out :unsigned-long) (new-tags-out :pointer) (new-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-compute-renumbering old-tags-out old-tags-n-out new-tags-out new-tags-n-out method element-tags-ptr element-tags-n ierr))
    (values (foreign-array-to-list (cffi:mem-ref old-tags-out :pointer) (cffi:mem-ref old-tags-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref new-tags-out :pointer) (cffi:mem-ref new-tags-n-out :unsigned-long) :unsigned-long)))))

(defun renumber-nodes (&key (old-tags '()) (new-tags '()))
  "Renumber the node tags. If no explicit renumbering is provided through
the `oldTags' and `newTags' vectors, renumber the nodes in a
continuous sequence, taking into account the subset of elements to be
saved later on if the option \"Mesh.SaveAll\" is not set."
  (with-foreign-array (old-tags-ptr old-tags-n old-tags :unsigned-long)
      (with-foreign-array (new-tags-ptr new-tags-n new-tags :unsigned-long)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-renumber-nodes old-tags-ptr old-tags-n new-tags-ptr new-tags-n ierr)))))

(defun renumber-elements (&key (old-tags '()) (new-tags '()))
  "Renumber the element tags in a continuous sequence. If no explicit
renumbering is provided through the `oldTags' and `newTags' vectors,
renumber the elements in a continuous sequence, taking into account
the subset of elements to be saved later on if the option
\"Mesh.SaveAll\" is not set."
  (with-foreign-array (old-tags-ptr old-tags-n old-tags :unsigned-long)
      (with-foreign-array (new-tags-ptr new-tags-n new-tags :unsigned-long)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-renumber-elements old-tags-ptr old-tags-n new-tags-ptr new-tags-n ierr)))))

(defun set-periodic (dim tags tags-master affine-transform)
  "Set the meshes of the entities of dimension `dim' and tag `tags' as
periodic copies of the meshes of entities `tagsMaster', using the
affine transformation specified in `affineTransformation' (16 entries
of a 4x4 matrix, by row). If used after meshing, generate the periodic
node correspondence information assuming the meshes of entities `tags'
effectively match the meshes of entities `tagsMaster' (useful for
structured and extruded meshes). Currently only available for
@code{dim} == 1 and @code{dim} == 2."
  (with-foreign-array (tags-ptr tags-n tags :int)
      (with-foreign-array (tags-master-ptr tags-master-n tags-master :int)
      (with-foreign-array (affine-transform-ptr affine-transform-n affine-transform :double)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-periodic dim tags-ptr tags-n tags-master-ptr tags-master-n affine-transform-ptr affine-transform-n ierr))))))

(defun get-periodic (dim tags)
  "Get master entities `tagsMaster' for the entities of dimension `dim'
and tags `tags'."
  (with-foreign-array (tags-ptr tags-n tags :int)
      (cffi:with-foreign-objects ((tag-master-out :pointer) (tag-master-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-periodic dim tags-ptr tags-n tag-master-out tag-master-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref tag-master-out :pointer) (cffi:mem-ref tag-master-n-out :unsigned-long) :int))))

(defun get-periodic-nodes (dim tag &key (include-high-order-nodes nil))
  "Get the master entity `tagMaster', the node tags `nodeTags' and their
corresponding master node tags `nodeTagsMaster', and the affine
transform `affineTransform' for the entity of dimension `dim' and tag
`tag'. If `includeHighOrderNodes' is set, include high-order nodes in
the returned data."
  (cffi:with-foreign-objects ((tag-master-out :int) (node-tags-out :pointer) (node-tags-n-out :unsigned-long) (node-tags-master-out :pointer) (node-tags-master-n-out :unsigned-long) (affine-transform-out :pointer) (affine-transform-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-periodic-nodes dim tag tag-master-out node-tags-out node-tags-n-out node-tags-master-out node-tags-master-n-out affine-transform-out affine-transform-n-out (if include-high-order-nodes 1 0) ierr))
    (values (cffi:mem-ref tag-master-out :int) (foreign-array-to-list (cffi:mem-ref node-tags-out :pointer) (cffi:mem-ref node-tags-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref node-tags-master-out :pointer) (cffi:mem-ref node-tags-master-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref affine-transform-out :pointer) (cffi:mem-ref affine-transform-n-out :unsigned-long) :double))))

(defun get-periodic-keys (element-type function-space-type tag &key (return-coord t))
  "Get the master entity `tagMaster' and the key pairs (`typeKeyMaster',
`entityKeyMaster') corresponding to the entity `tag' and the key pairs
(`typeKey', `entityKey') for the elements of type `elementType' and
function space type `functionSpaceType'. If `returnCoord' is set, the
`coord' and `coordMaster' vectors contain the x, y, z coordinates
locating basis functions for sorting purposes."
  (cffi:with-foreign-objects ((tag-master-out :int) (type-keys-out :pointer) (type-keys-n-out :unsigned-long) (type-keys-master-out :pointer) (type-keys-master-n-out :unsigned-long) (entity-keys-out :pointer) (entity-keys-n-out :unsigned-long) (entity-keys-master-out :pointer) (entity-keys-master-n-out :unsigned-long) (coord-out :pointer) (coord-n-out :unsigned-long) (coord-master-out :pointer) (coord-master-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-periodic-keys element-type function-space-type tag tag-master-out type-keys-out type-keys-n-out type-keys-master-out type-keys-master-n-out entity-keys-out entity-keys-n-out entity-keys-master-out entity-keys-master-n-out coord-out coord-n-out coord-master-out coord-master-n-out (if return-coord 1 0) ierr))
    (values (cffi:mem-ref tag-master-out :int) (foreign-array-to-list (cffi:mem-ref type-keys-out :pointer) (cffi:mem-ref type-keys-n-out :unsigned-long) :int) (foreign-array-to-list (cffi:mem-ref type-keys-master-out :pointer) (cffi:mem-ref type-keys-master-n-out :unsigned-long) :int) (foreign-array-to-list (cffi:mem-ref entity-keys-out :pointer) (cffi:mem-ref entity-keys-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref entity-keys-master-out :pointer) (cffi:mem-ref entity-keys-master-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref coord-out :pointer) (cffi:mem-ref coord-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref coord-master-out :pointer) (cffi:mem-ref coord-master-n-out :unsigned-long) :double))))

(defun import-stl ()
  "Import the model STL representation (if available) as the current
mesh."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-import-stl  ierr)))

(defun get-duplicate-nodes (&key (dim-tags '()))
  "Get the `tags' of any duplicate nodes in the mesh of the entities
`dimTags', given as a vector of (dim, tag) pairs. If `dimTags' is
empty, consider the whole mesh."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (cffi:with-foreign-objects ((tags-out :pointer) (tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-duplicate-nodes tags-out tags-n-out dim-tags-ptr dim-tags-n ierr))
    (foreign-array-to-list (cffi:mem-ref tags-out :pointer) (cffi:mem-ref tags-n-out :unsigned-long) :unsigned-long))))

(defun remove-duplicate-nodes (&key (dim-tags '()))
  "Remove duplicate nodes in the mesh of the entities `dimTags', given as
a vector of (dim, tag) pairs. If `dimTags' is empty, consider the
whole mesh."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-remove-duplicate-nodes dim-tags-ptr dim-tags-n ierr))))

(defun remove-duplicate-elements (&key (dim-tags '()))
  "Remove duplicate elements (defined by the same nodes, in the same
entity) in the mesh of the entities `dimTags', given as a vector of
(dim, tag) pairs. If `dimTags' is empty, consider the whole mesh."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-remove-duplicate-elements dim-tags-ptr dim-tags-n ierr))))

(defun split-quadrangles (&key (quality 1.0) (tag -1))
  "Split (into two triangles) all quadrangles in surface `tag' whose
quality is lower than `quality'. If `tag' < 0, split quadrangles in
all surfaces."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-split-quadrangles (to-double quality) tag ierr)))

(defun set-visibility (element-tags value)
  "Set the visibility of the elements of tags `elementTags' to `value'."
  (with-foreign-array (element-tags-ptr element-tags-n element-tags :unsigned-long)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-set-visibility element-tags-ptr element-tags-n value ierr))))

(defun get-visibility (element-tags)
  "Get the visibility of the elements of tags `elementTags'."
  (with-foreign-array (element-tags-ptr element-tags-n element-tags :unsigned-long)
      (cffi:with-foreign-objects ((values-out :pointer) (values-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-get-visibility element-tags-ptr element-tags-n values-out values-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref values-out :pointer) (cffi:mem-ref values-n-out :unsigned-long) :int))))

(defun classify-surfaces (angle &key (boundary t) (for-reparametrization nil) (curve-angle pi) (export-discrete t))
  "Classify (\"color\") the surface mesh based on the angle threshold
`angle' (in radians), and create new discrete surfaces, curves and
points accordingly. If `boundary' is set, also create discrete curves
on the boundary if the surface is open. If `forReparametrization' is
set, create curves and surfaces that can be reparametrized using a
single map. If `curveAngle' is less than Pi, also force curves to be
split according to `curveAngle'. If `exportDiscrete' is set, clear any
built-in CAD kernel entities and export the discrete entities in the
built-in CAD kernel."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-classify-surfaces (to-double angle) (if boundary 1 0) (if for-reparametrization 1 0) (to-double curve-angle) (if export-discrete 1 0) ierr)))

(defun create-geometry (&key (dim-tags '()))
  "Create a geometry for the discrete entities `dimTags' (given as a
vector of (dim, tag) pairs) represented solely by a mesh (without an
underlying CAD description), i.e. create a parametrization for
discrete curves and surfaces, assuming that each can be parametrized
with a single map. If `dimTags' is empty, create a geometry for all
the discrete entities."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-create-geometry dim-tags-ptr dim-tags-n ierr))))

(defun create-topology (&key (make-simply-connected t) (export-discrete t))
  "Create a boundary representation from the mesh if the model does not
have one (e.g. when imported from mesh file formats with no BRep
representation of the underlying model). If `makeSimplyConnected' is
set, enforce simply connected discrete surfaces and volumes. If
`exportDiscrete' is set, clear any built-in CAD kernel entities and
export the discrete entities in the built-in CAD kernel."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-create-topology (if make-simply-connected 1 0) (if export-discrete 1 0) ierr)))

(defun add-homology-request (&key (type "Homology") (domain-tags '()) (subdomain-tags '()) (dims '()))
  "Add a request to compute a basis representation for homology spaces
(if `type' == \"Homology\") or cohomology spaces (if `type' ==
\"Cohomology\"). The computation domain is given in a list of physical
group tags `domainTags'; if empty, the whole mesh is the domain. The
computation subdomain for relative (co)homology computation is given
in a list of physical group tags `subdomainTags'; if empty, absolute
(co)homology is computed. The dimensions of the (co)homology bases to
be computed are given in the list `dim'; if empty, all bases are
computed. Resulting basis representation (co)chains are stored as
physical groups in the mesh. If the request is added before mesh
generation, the computation will be performed at the end of the
meshing pipeline."
  (with-foreign-array (domain-tags-ptr domain-tags-n domain-tags :int)
      (with-foreign-array (subdomain-tags-ptr subdomain-tags-n subdomain-tags :int)
      (with-foreign-array (dims-ptr dims-n dims :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-add-homology-request type domain-tags-ptr domain-tags-n subdomain-tags-ptr subdomain-tags-n dims-ptr dims-n ierr))))))

(defun clear-homology-requests ()
  "Clear all (co)homology computation requests."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-clear-homology-requests  ierr)))

(defun compute-homology ()
  "Perform the (co)homology computations requested by
addHomologyRequest(). The newly created physical groups are returned
in `dimTags' as a vector of (dim, tag) pairs."
  (cffi:with-foreign-objects ((dim-tags-out :pointer) (dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-compute-homology dim-tags-out dim-tags-n-out ierr))
    (dim-tags-to-pairs (cffi:mem-ref dim-tags-out :pointer) (cffi:mem-ref dim-tags-n-out :unsigned-long))))

(defun compute-cross-field ()
  "Compute a cross field for the current mesh. The function creates 3
views: the H function, the Theta function and cross directions. Return
the tags of the views."
  (cffi:with-foreign-objects ((view-tags-out :pointer) (view-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-compute-cross-field view-tags-out view-tags-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref view-tags-out :pointer) (cffi:mem-ref view-tags-n-out :unsigned-long) :int)))

(defun field-add (field-type &key (tag -1))
  "Add a new mesh size field of type `fieldType'. If `tag' is positive,
assign the tag explicitly; otherwise a new tag is assigned
automatically. Return the field tag. Available field types are listed
in the \"Gmsh mesh size fields\" chapter of the Gmsh reference manual
(https://gmsh.info/doc/texinfo/gmsh.html#Gmsh-mesh-size-fields)."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-field-add field-type tag ierr)))

(defun field-remove (tag)
  "Remove the field with tag `tag'."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-field-remove tag ierr)))

(defun field-list ()
  "Get the list of all fields."
  (cffi:with-foreign-objects ((tags-out :pointer) (tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-field-list tags-out tags-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref tags-out :pointer) (cffi:mem-ref tags-n-out :unsigned-long) :int)))

(defun field-get-type (tag)
  "Get the type `fieldType' of the field with tag `tag'."
  (cffi:with-foreign-object (file-type-out :pointer)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-field-get-type tag file-type-out ierr))
    (foreign-string-result (cffi:mem-ref file-type-out :pointer))))

(defun field-set-number (tag option value)
  "Set the numerical option `option' to value `value' for field `tag'."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-field-set-number tag option (to-double value) ierr)))

(defun field-get-number (tag option)
  "Get the value of the numerical option `option' for field `tag'."
  (cffi:with-foreign-object (value-out :double)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-field-get-number tag option value-out ierr))
    (cffi:mem-ref value-out :double)))

(defun field-set-string (tag option value)
  "Set the string option `option' to value `value' for field `tag'."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-field-set-string tag option value ierr)))

(defun field-get-string (tag option)
  "Get the value of the string option `option' for field `tag'."
  (cffi:with-foreign-object (value-out :pointer)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-field-get-string tag option value-out ierr))
    (foreign-string-result (cffi:mem-ref value-out :pointer))))

(defun field-set-numbers (tag option values)
  "Set the numerical list option `option' to value `values' for field
`tag'."
  (with-foreign-array (values-ptr values-n values :double)
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-field-set-numbers tag option values-ptr values-n ierr))))

(defun field-get-numbers (tag option)
  "Get the value of the numerical list option `option' for field `tag'."
  (cffi:with-foreign-objects ((values-out :pointer) (values-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-mesh-field-get-numbers tag option values-out values-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref values-out :pointer) (cffi:mem-ref values-n-out :unsigned-long) :double)))

(defun field-set-as-background-mesh (tag)
  "Set the field `tag' as the background mesh size field."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-field-set-as-background-mesh tag ierr)))

(defun field-set-as-boundary-layer (tag)
  "Set the field `tag' as a boundary layer size field."
  (with-ierr (ierr)
      (gmsh/internal::%model-mesh-field-set-as-boundary-layer tag ierr)))

