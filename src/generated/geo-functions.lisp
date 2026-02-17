;;;; geo-functions.lisp — Generated wrappers for gmsh/geo
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(in-package :gmsh/geo)

(defun point (x y z &key (mesh-size 0.0) (tag -1))
  "Add a geometrical point in the built-in CAD representation, at
coordinates (`x', `y', `z'). If `meshSize' is > 0, add a meshing
constraint at that point. If `tag' is positive, set the tag
explicitly; otherwise a new tag is selected automatically. Return the
tag of the point. (Note that the point will be added in the current
model only after `synchronize' is called. This behavior holds for all
the entities added in the geo module.)"
  (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-point (to-double x) (to-double y) (to-double z) (to-double mesh-size) tag ierr)))

(defun line (start-tag end-tag &key (tag -1))
  "Add a straight line segment in the built-in CAD representation,
between the two points with tags `startTag' and `endTag'. If `tag' is
positive, set the tag explicitly; otherwise a new tag is selected
automatically. Return the tag of the line."
  (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-line start-tag end-tag tag ierr)))

(defun circle-arc (start-tag center-tag end-tag &key (tag -1) (nx 0.0) (ny 0.0) (nz 0.0))
  "Add a circle arc (strictly smaller than Pi) in the built-in CAD
representation, between the two points with tags `startTag' and
`endTag', and with center `centerTag'. If `tag' is positive, set the
tag explicitly; otherwise a new tag is selected automatically. If
(`nx', `ny', `nz') != (0, 0, 0), explicitly set the plane of the
circle arc. Return the tag of the circle arc."
  (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-circle-arc start-tag center-tag end-tag tag (to-double nx) (to-double ny) (to-double nz) ierr)))

(defun ellipse-arc (start-tag center-tag major-tag end-tag &key (tag -1) (nx 0.0) (ny 0.0) (nz 0.0))
  "Add an ellipse arc (strictly smaller than Pi) in the built-in CAD
representation, between the two points `startTag' and `endTag', and
with center `centerTag' and major axis point `majorTag'. If `tag' is
positive, set the tag explicitly; otherwise a new tag is selected
automatically. If (`nx', `ny', `nz') != (0, 0, 0), explicitly set the
plane of the circle arc. Return the tag of the ellipse arc."
  (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-ellipse-arc start-tag center-tag major-tag end-tag tag (to-double nx) (to-double ny) (to-double nz) ierr)))

(defun spline (point-tags &key (tag -1))
  "Add a spline (Catmull-Rom) curve in the built-in CAD representation,
going through the points `pointTags'. If `tag' is positive, set the
tag explicitly; otherwise a new tag is selected automatically. Create
a periodic curve if the first and last points are the same. Return the
tag of the spline curve."
  (with-foreign-array (point-tags-ptr point-tags-n point-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-spline point-tags-ptr point-tags-n tag ierr))))

(defun b-spline (point-tags &key (tag -1))
  "Add a cubic b-spline curve in the built-in CAD representation, with
`pointTags' control points. If `tag' is positive, set the tag
explicitly; otherwise a new tag is selected automatically. Creates a
periodic curve if the first and last points are the same. Return the
tag of the b-spline curve."
  (with-foreign-array (point-tags-ptr point-tags-n point-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-b-spline point-tags-ptr point-tags-n tag ierr))))

(defun bezier (point-tags &key (tag -1))
  "Add a Bezier curve in the built-in CAD representation, with
`pointTags' control points. If `tag' is positive, set the tag
explicitly; otherwise a new tag is selected automatically.  Return the
tag of the Bezier curve."
  (with-foreign-array (point-tags-ptr point-tags-n point-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-bezier point-tags-ptr point-tags-n tag ierr))))

(defun polyline (point-tags &key (tag -1))
  "Add a polyline curve in the built-in CAD representation, going through
the points `pointTags'. If `tag' is positive, set the tag explicitly;
otherwise a new tag is selected automatically. Create a periodic curve
if the first and last points are the same. Return the tag of the
polyline curve."
  (with-foreign-array (point-tags-ptr point-tags-n point-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-polyline point-tags-ptr point-tags-n tag ierr))))

(defun compound-spline (curve-tags &key (num-intervals 5) (tag -1))
  "Add a spline (Catmull-Rom) curve in the built-in CAD representation,
going through points sampling the curves in `curveTags'. The density
of sampling points on each curve is governed by `numIntervals'. If
`tag' is positive, set the tag explicitly; otherwise a new tag is
selected automatically. Return the tag of the spline."
  (with-foreign-array (curve-tags-ptr curve-tags-n curve-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-compound-spline curve-tags-ptr curve-tags-n num-intervals tag ierr))))

(defun compound-b-spline (curve-tags &key (num-intervals 20) (tag -1))
  "Add a b-spline curve in the built-in CAD representation, with control
points sampling the curves in `curveTags'. The density of sampling
points on each curve is governed by `numIntervals'. If `tag' is
positive, set the tag explicitly; otherwise a new tag is selected
automatically. Return the tag of the b-spline."
  (with-foreign-array (curve-tags-ptr curve-tags-n curve-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-compound-b-spline curve-tags-ptr curve-tags-n num-intervals tag ierr))))

(defun curve-loop (curve-tags &key (tag -1) (reorient nil))
  "Add a curve loop (a closed wire) in the built-in CAD representation,
formed by the curves `curveTags'. `curveTags' should contain (signed)
tags of model entities of dimension 1 forming a closed loop: a
negative tag signifies that the underlying curve is considered with
reversed orientation. If `tag' is positive, set the tag explicitly;
otherwise a new tag is selected automatically. If `reorient' is set,
automatically reorient the curves if necessary. Return the tag of the
curve loop."
  (with-foreign-array (curve-tags-ptr curve-tags-n curve-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-curve-loop curve-tags-ptr curve-tags-n tag (if reorient 1 0) ierr))))

(defun curve-loops (curve-tags)
  "Add curve loops in the built-in CAD representation based on the curves
`curveTags'. Return the `tags' of found curve loops, if any."
  (with-foreign-array (curve-tags-ptr curve-tags-n curve-tags :int)
      (cffi:with-foreign-objects ((tags-out :pointer) (tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-curve-loops curve-tags-ptr curve-tags-n tags-out tags-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref tags-out :pointer) (cffi:mem-ref tags-n-out :unsigned-long) :int))))

(defun plane-surface (wire-tags &key (tag -1))
  "Add a plane surface in the built-in CAD representation, defined by one
or more curve loops `wireTags'. The first curve loop defines the
exterior contour; additional curve loop define holes. If `tag' is
positive, set the tag explicitly; otherwise a new tag is selected
automatically. Return the tag of the surface."
  (with-foreign-array (wire-tags-ptr wire-tags-n wire-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-plane-surface wire-tags-ptr wire-tags-n tag ierr))))

(defun surface-filling (wire-tags &key (tag -1) (sphere-center-tag -1))
  "Add a surface in the built-in CAD representation, filling the curve
loops in `wireTags' using transfinite interpolation. Currently only a
single curve loop is supported; this curve loop should be composed by
3 or 4 curves only. If `tag' is positive, set the tag explicitly;
otherwise a new tag is selected automatically. Return the tag of the
surface."
  (with-foreign-array (wire-tags-ptr wire-tags-n wire-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-surface-filling wire-tags-ptr wire-tags-n tag sphere-center-tag ierr))))

(defun surface-loop (surface-tags &key (tag -1))
  "Add a surface loop (a closed shell) formed by `surfaceTags' in the
built-in CAD representation.  If `tag' is positive, set the tag
explicitly; otherwise a new tag is selected automatically. Return the
tag of the shell."
  (with-foreign-array (surface-tags-ptr surface-tags-n surface-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-surface-loop surface-tags-ptr surface-tags-n tag ierr))))

(defun volume (shell-tags &key (tag -1))
  "Add a volume (a region) in the built-in CAD representation, defined by
one or more shells `shellTags'. The first surface loop defines the
exterior boundary; additional surface loop define holes. If `tag' is
positive, set the tag explicitly; otherwise a new tag is selected
automatically. Return the tag of the volume."
  (with-foreign-array (shell-tags-ptr shell-tags-n shell-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-volume shell-tags-ptr shell-tags-n tag ierr))))

(defun geometry (geometry &key (numbers '()) (strings '()) (tag -1))
  "Add a `geometry' in the built-in CAD representation. `geometry' can
currently be one of \"Sphere\" or \"PolarSphere\" (where `numbers'
should contain the x, y, z coordinates of the center, followed by the
radius), or \"ParametricSurface\" (where `strings' should contains
three expression evaluating to the x, y and z coordinates in terms of
parametric coordinates u and v). If `tag' is positive, set the tag of
the geometry explicitly; otherwise a new tag is selected
automatically. Return the tag of the geometry."
  (with-foreign-array (numbers-ptr numbers-n numbers :double)
      (with-string-array (strings-ptr strings-n strings)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-geometry geometry numbers-ptr numbers-n strings-ptr strings-n tag ierr)))))

(defun point-on-geometry (geometry-tag x y &key (z 0.0) (mesh-size 0.0) (tag -1))
  "Add a point in the built-in CAD representation, at coordinates (`x',
`y', `z') on the geometry `geometryTag'. If `meshSize' is > 0, add a
meshing constraint at that point. If `tag' is positive, set the tag
explicitly; otherwise a new tag is selected automatically. Return the
tag of the point. For surface geometries, only the `x' and `y'
coordinates are used."
  (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-point-on-geometry geometry-tag (to-double x) (to-double y) (to-double z) (to-double mesh-size) tag ierr)))

(defun extrude (dim-tags dx dy dz &key (num-elements '()) (heights '()) (recombine nil))
  "Extrude the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the built-in CAD representation, using a translation along (`dx',
`dy', `dz'). Return extruded entities in `outDimTags'. If the
`numElements' vector is not empty, also extrude the mesh: the entries
in `numElements' give the number of elements in each layer. If the
`height' vector is not empty, it provides the (cumulative) height of
the different layers, normalized to 1. If `recombine' is set,
recombine the mesh in the layers."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-foreign-array (num-elements-ptr num-elements-n num-elements :int)
      (with-foreign-array (heights-ptr heights-n heights :double)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-extrude dim-tags-ptr dim-tags-n (to-double dx) (to-double dy) (to-double dz) out-dim-tags-out out-dim-tags-n-out num-elements-ptr num-elements-n heights-ptr heights-n (if recombine 1 0) ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)))))))

(defun revolve (dim-tags x y z ax ay az angle &key (num-elements '()) (heights '()) (recombine nil))
  "Extrude the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the built-in CAD representation, using a rotation of `angle'
radians around the axis of revolution defined by the point (`x', `y',
`z') and the direction (`ax', `ay', `az'). The angle should be
strictly smaller than Pi. Return extruded entities in `outDimTags'. If
the `numElements' vector is not empty, also extrude the mesh: the
entries in `numElements' give the number of elements in each layer. If
the `height' vector is not empty, it provides the (cumulative) height
of the different layers, normalized to 1. If `recombine' is set,
recombine the mesh in the layers."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-foreign-array (num-elements-ptr num-elements-n num-elements :int)
      (with-foreign-array (heights-ptr heights-n heights :double)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-revolve dim-tags-ptr dim-tags-n (to-double x) (to-double y) (to-double z) (to-double ax) (to-double ay) (to-double az) (to-double angle) out-dim-tags-out out-dim-tags-n-out num-elements-ptr num-elements-n heights-ptr heights-n (if recombine 1 0) ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)))))))

(defun twist (dim-tags x y z dx dy dz ax ay az angle &key (num-elements '()) (heights '()) (recombine nil))
  "Extrude the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the built-in CAD representation, using a combined translation and
rotation of `angle' radians, along (`dx', `dy', `dz') and around the
axis of revolution defined by the point (`x', `y', `z') and the
direction (`ax', `ay', `az'). The angle should be strictly smaller
than Pi. Return extruded entities in `outDimTags'. If the
`numElements' vector is not empty, also extrude the mesh: the entries
in `numElements' give the number of elements in each layer. If the
`height' vector is not empty, it provides the (cumulative) height of
the different layers, normalized to 1. If `recombine' is set,
recombine the mesh in the layers."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-foreign-array (num-elements-ptr num-elements-n num-elements :int)
      (with-foreign-array (heights-ptr heights-n heights :double)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-twist dim-tags-ptr dim-tags-n (to-double x) (to-double y) (to-double z) (to-double dx) (to-double dy) (to-double dz) (to-double ax) (to-double ay) (to-double az) (to-double angle) out-dim-tags-out out-dim-tags-n-out num-elements-ptr num-elements-n heights-ptr heights-n (if recombine 1 0) ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)))))))

(defun extrude-boundary-layer (dim-tags &key (num-elements '()) (heights '()) (recombine nil) (second nil) (view-index -1))
  "Extrude the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the built-in CAD representation along the normals of the mesh,
creating discrete boundary layer entities. Return extruded entities in
`outDimTags'. The entries in `numElements' give the number of elements
in each layer. If the `height' vector is not empty, it provides the
(cumulative) height of the different layers. If `recombine' is set,
recombine the mesh in the layers. A second boundary layer can be
created from the same entities if `second' is set. If `viewIndex' is
>= 0, use the corresponding view to either specify the normals (if the
view contains a vector field) or scale the normals (if the view is
scalar)."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-foreign-array (num-elements-ptr num-elements-n num-elements :int)
      (with-foreign-array (heights-ptr heights-n heights :double)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-extrude-boundary-layer dim-tags-ptr dim-tags-n out-dim-tags-out out-dim-tags-n-out num-elements-ptr num-elements-n heights-ptr heights-n (if recombine 1 0) (if second 1 0) view-index ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)))))))

(defun translate (dim-tags dx dy dz)
  "Translate the entities `dimTags' (given as a vector of (dim, tag)
pairs) in the built-in CAD representation along (`dx', `dy', `dz')."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-translate dim-tags-ptr dim-tags-n (to-double dx) (to-double dy) (to-double dz) ierr))))

(defun rotate (dim-tags x y z ax ay az angle)
  "Rotate the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the built-in CAD representation by `angle' radians around the axis
of revolution defined by the point (`x', `y', `z') and the direction
(`ax', `ay', `az')."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-rotate dim-tags-ptr dim-tags-n (to-double x) (to-double y) (to-double z) (to-double ax) (to-double ay) (to-double az) (to-double angle) ierr))))

(defun dilate (dim-tags x y z a b c)
  "Scale the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the built-in CAD representation by factors `a', `b' and `c' along
the three coordinate axes; use (`x', `y', `z') as the center of the
homothetic transformation."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-dilate dim-tags-ptr dim-tags-n (to-double x) (to-double y) (to-double z) (to-double a) (to-double b) (to-double c) ierr))))

(defun mirror (dim-tags a b c d)
  "Mirror the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the built-in CAD representation, with respect to the plane of
equation `a' * x + `b' * y + `c' * z + `d' = 0."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-mirror dim-tags-ptr dim-tags-n (to-double a) (to-double b) (to-double c) (to-double d) ierr))))

(defun symmetrize (dim-tags a b c d)
  "Mirror the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the built-in CAD representation, with respect to the plane of
equation `a' * x + `b' * y + `c' * z + `d' = 0. (This is a deprecated
synonym for `mirror'.)"
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-symmetrize dim-tags-ptr dim-tags-n (to-double a) (to-double b) (to-double c) (to-double d) ierr))))

(defun copy (dim-tags)
  "Copy the entities `dimTags' (given as a vector of (dim, tag) pairs) in
the built-in CAD representation; the new entities are returned in
`outDimTags'."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-copy dim-tags-ptr dim-tags-n out-dim-tags-out out-dim-tags-n-out ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)))))

(defun remove (dim-tags &key (recursive nil))
  "Remove the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the built-in CAD representation, provided that they are not on the
boundary of higher-dimensional entities. If `recursive' is true,
remove all the entities on their boundaries, down to dimension 0."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-remove dim-tags-ptr dim-tags-n (if recursive 1 0) ierr))))

(defun remove-all-duplicates ()
  "Remove all duplicate entities in the built-in CAD representation
(different entities at the same geometrical location)."
  (with-ierr (ierr)
      (gmsh/internal::%model-geo-remove-all-duplicates  ierr)))

(defun split-curve (tag point-tags)
  "Split the curve of tag `tag' in the built-in CAD representation, on
the specified control points `pointTags'. This feature is only
available for splines and b-splines. Return the tag(s) `curveTags' of
the newly created curve(s)."
  (with-foreign-array (point-tags-ptr point-tags-n point-tags :int)
      (cffi:with-foreign-objects ((curve-tags-out :pointer) (curve-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-split-curve tag point-tags-ptr point-tags-n curve-tags-out curve-tags-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref curve-tags-out :pointer) (cffi:mem-ref curve-tags-n-out :unsigned-long) :int))))

(defun get-max-tag (dim)
  "Get the maximum tag of entities of dimension `dim' in the built-in CAD
representation."
  (with-ierr (ierr)
      (gmsh/internal::%model-geo-get-max-tag dim ierr)))

(defun set-max-tag (dim max-tag)
  "Set the maximum tag `maxTag' for entities of dimension `dim' in the
built-in CAD representation."
  (with-ierr (ierr)
      (gmsh/internal::%model-geo-set-max-tag dim max-tag ierr)))

(defun physical-group (dim tags &key (tag -1) (name ""))
  "Add a physical group of dimension `dim', grouping the entities with
tags `tags' in the built-in CAD representation. Return the tag of the
physical group, equal to `tag' if `tag' is positive, or a new tag if
`tag' < 0. Set the name of the physical group if `name' is not empty."
  (with-foreign-array (tags-ptr tags-n tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-add-physical-group dim tags-ptr tags-n tag name ierr))))

(defun remove-physical-groups (&key (dim-tags '()))
  "Remove the physical groups `dimTags' (given as a vector of (dim, tag)
pairs) from the built-in CAD representation. If `dimTags' is empty,
remove all groups."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-geo-remove-physical-groups dim-tags-ptr dim-tags-n ierr))))

(defun synchronize ()
  "Synchronize the built-in CAD representation with the current Gmsh
model. This can be called at any time, but since it involves a non
trivial amount of processing, the number of synchronization points
should normally be minimized. Without synchronization the entities in
the built-in CAD representation are not available to any function
outside of the built-in CAD kernel functions."
  (with-ierr (ierr)
      (gmsh/internal::%model-geo-synchronize  ierr)))

