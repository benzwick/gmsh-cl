;;;; occ-functions.lisp — Generated wrappers for gmsh/occ
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(in-package :gmsh/occ)

(defun point (x y z &key (mesh-size 0.0) (tag -1))
  "Add a geometrical point in the OpenCASCADE CAD representation, at
coordinates (`x', `y', `z'). If `meshSize' is > 0, add a meshing
constraint at that point. If `tag' is positive, set the tag
explicitly; otherwise a new tag is selected automatically. Return the
tag of the point. (Note that the point will be added in the current
model only after `synchronize' is called. This behavior holds for all
the entities added in the occ module.)"
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-point (to-double x) (to-double y) (to-double z) (to-double mesh-size) tag ierr)))

(defun line (start-tag end-tag &key (tag -1))
  "Add a straight line segment in the OpenCASCADE CAD representation,
between the two points with tags `startTag' and `endTag'. If `tag' is
positive, set the tag explicitly; otherwise a new tag is selected
automatically. Return the tag of the line."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-line start-tag end-tag tag ierr)))

(defun circle-arc (start-tag middle-tag end-tag &key (tag -1) (center t))
  "Add a circle arc in the OpenCASCADE CAD representation, between the
two points with tags `startTag' and `endTag', with middle point
`middleTag'. If `center' is true, the middle point is the center of
the circle; otherwise the circle goes through the middle point. If
`tag' is positive, set the tag explicitly; otherwise a new tag is
selected automatically. Return the tag of the circle arc."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-circle-arc start-tag middle-tag end-tag tag (if center 1 0) ierr)))

(defun circle (x y z r &key (tag -1) (angle1 0.0) (angle2 (* 2 pi)) (z-axis '()) (x-axis '()))
  "Add a circle of center (`x', `y', `z') and radius `r' in the
OpenCASCADE CAD representation. If `tag' is positive, set the tag
explicitly; otherwise a new tag is selected automatically. If `angle1'
and `angle2' are specified, create a circle arc between the two
angles. If a vector `zAxis' of size 3 is provided, use it as the
normal to the circle plane (z-axis). If a vector `xAxis' of size 3 is
provided in addition to `zAxis', use it to define the x-axis. Return
the tag of the circle."
  (with-foreign-array (z-axis-ptr z-axis-n z-axis :double)
      (with-foreign-array (x-axis-ptr x-axis-n x-axis :double)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-circle (to-double x) (to-double y) (to-double z) (to-double r) tag (to-double angle1) (to-double angle2) z-axis-ptr z-axis-n x-axis-ptr x-axis-n ierr)))))

(defun ellipse-arc (start-tag center-tag major-tag end-tag &key (tag -1))
  "Add an ellipse arc in the OpenCASCADE CAD representation, between the
two points `startTag' and `endTag', and with center `centerTag' and
major axis point `majorTag'. If `tag' is positive, set the tag
explicitly; otherwise a new tag is selected automatically. Return the
tag of the ellipse arc. Note that OpenCASCADE does not allow creating
ellipse arcs with the major radius smaller than the minor radius."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-ellipse-arc start-tag center-tag major-tag end-tag tag ierr)))

(defun ellipse (x y z r1 r2 &key (tag -1) (angle1 0.0) (angle2 (* 2 pi)) (z-axis '()) (x-axis '()))
  "Add an ellipse of center (`x', `y', `z') and radii `r1' and `r2' (with
`r1' >= `r2') along the x- and y-axes, respectively, in the
OpenCASCADE CAD representation. If `tag' is positive, set the tag
explicitly; otherwise a new tag is selected automatically. If `angle1'
and `angle2' are specified, create an ellipse arc between the two
angles. If a vector `zAxis' of size 3 is provided, use it as the
normal to the ellipse plane (z-axis). If a vector `xAxis' of size 3 is
provided in addition to `zAxis', use it to define the x-axis. Return
the tag of the ellipse."
  (with-foreign-array (z-axis-ptr z-axis-n z-axis :double)
      (with-foreign-array (x-axis-ptr x-axis-n x-axis :double)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-ellipse (to-double x) (to-double y) (to-double z) (to-double r1) (to-double r2) tag (to-double angle1) (to-double angle2) z-axis-ptr z-axis-n x-axis-ptr x-axis-n ierr)))))

(defun spline (point-tags &key (tag -1) (tangents '()))
  "Add a spline (C2 b-spline) curve in the OpenCASCADE CAD
representation, going through the points `pointTags'. If `tag' is
positive, set the tag explicitly; otherwise a new tag is selected
automatically. Create a periodic curve if the first and last points
are the same. Return the tag of the spline curve. If the `tangents'
vector contains 6 entries, use them as concatenated x, y, z components
of the initial and final tangents of the b-spline; if it contains 3
times as many entries as the number of points, use them as
concatenated x, y, z components of the tangents at each point, unless
the norm of the tangent is zero."
  (with-foreign-array (point-tags-ptr point-tags-n point-tags :int)
      (with-foreign-array (tangents-ptr tangents-n tangents :double)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-spline point-tags-ptr point-tags-n tag tangents-ptr tangents-n ierr)))))

(defun b-spline (point-tags &key (tag -1) (degree 3) (weights '()) (knots '()) (multiplicities '()))
  "Add a b-spline curve of degree `degree' in the OpenCASCADE CAD
representation, with `pointTags' control points. If `weights', `knots'
or `multiplicities' are not provided, default parameters are computed
automatically. If `tag' is positive, set the tag explicitly; otherwise
a new tag is selected automatically. Create a periodic curve if the
first and last points are the same. Return the tag of the b-spline
curve."
  (with-foreign-array (point-tags-ptr point-tags-n point-tags :int)
      (with-foreign-array (weights-ptr weights-n weights :double)
      (with-foreign-array (knots-ptr knots-n knots :double)
      (with-foreign-array (multiplicities-ptr multiplicities-n multiplicities :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-b-spline point-tags-ptr point-tags-n tag degree weights-ptr weights-n knots-ptr knots-n multiplicities-ptr multiplicities-n ierr)))))))

(defun bezier (point-tags &key (tag -1))
  "Add a Bezier curve in the OpenCASCADE CAD representation, with
`pointTags' control points. If `tag' is positive, set the tag
explicitly; otherwise a new tag is selected automatically. Return the
tag of the Bezier curve."
  (with-foreign-array (point-tags-ptr point-tags-n point-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-bezier point-tags-ptr point-tags-n tag ierr))))

(defun wire (curve-tags &key (tag -1) (check-closed nil))
  "Add a wire (open or closed) in the OpenCASCADE CAD representation,
formed by the curves `curveTags'. Note that an OpenCASCADE wire can be
made of curves that share geometrically identical (but topologically
different) points. If `tag' is positive, set the tag explicitly;
otherwise a new tag is selected automatically. Return the tag of the
wire."
  (with-foreign-array (curve-tags-ptr curve-tags-n curve-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-wire curve-tags-ptr curve-tags-n tag (if check-closed 1 0) ierr))))

(defun curve-loop (curve-tags &key (tag -1))
  "Add a curve loop (a closed wire) in the OpenCASCADE CAD
representation, formed by the curves `curveTags'. `curveTags' should
contain tags of curves forming a closed loop. Negative tags can be
specified for compatibility with the built-in kernel, but are simply
ignored: the wire is oriented according to the orientation of its
first curve. Note that an OpenCASCADE curve loop can be made of curves
that share geometrically identical (but topologically different)
points. If `tag' is positive, set the tag explicitly; otherwise a new
tag is selected automatically. Return the tag of the curve loop."
  (with-foreign-array (curve-tags-ptr curve-tags-n curve-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-curve-loop curve-tags-ptr curve-tags-n tag ierr))))

(defun rectangle (x y z dx dy &key (tag -1) (rounded-radius 0.0))
  "Add a rectangle in the OpenCASCADE CAD representation, with lower left
corner at (`x', `y', `z') and upper right corner at (`x' + `dx', `y' +
`dy', `z'). If `tag' is positive, set the tag explicitly; otherwise a
new tag is selected automatically. Round the corners if
`roundedRadius' is nonzero. Return the tag of the rectangle."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-rectangle (to-double x) (to-double y) (to-double z) (to-double dx) (to-double dy) tag (to-double rounded-radius) ierr)))

(defun disk (xc yc zc rx ry &key (tag -1) (z-axis '()) (x-axis '()))
  "Add a disk in the OpenCASCADE CAD representation, with center (`xc',
`yc', `zc') and radius `rx' along the x-axis and `ry' along the y-axis
(`rx' >= `ry'). If `tag' is positive, set the tag explicitly;
otherwise a new tag is selected automatically. If a vector `zAxis' of
size 3 is provided, use it as the normal to the disk (z-axis). If a
vector `xAxis' of size 3 is provided in addition to `zAxis', use it to
define the x-axis. Return the tag of the disk."
  (with-foreign-array (z-axis-ptr z-axis-n z-axis :double)
      (with-foreign-array (x-axis-ptr x-axis-n x-axis :double)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-disk (to-double xc) (to-double yc) (to-double zc) (to-double rx) (to-double ry) tag z-axis-ptr z-axis-n x-axis-ptr x-axis-n ierr)))))

(defun plane-surface (wire-tags &key (tag -1))
  "Add a plane surface in the OpenCASCADE CAD representation, defined by
one or more curve loops (or closed wires) `wireTags'. The first curve
loop defines the exterior contour; additional curve loop define holes.
If `tag' is positive, set the tag explicitly; otherwise a new tag is
selected automatically. Return the tag of the surface."
  (with-foreign-array (wire-tags-ptr wire-tags-n wire-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-plane-surface wire-tags-ptr wire-tags-n tag ierr))))

(defun surface-filling (wire-tag &key (tag -1) (point-tags '()) (degree 2) (num-points-on-curves 15) (num-iter 2) (anisotropic nil) (tol2d 0.00001) (tol3d 0.0001) (tol-ang 0.01) (tol-curv 0.1) (max-degree 8) (max-segments 9))
  "Add a surface in the OpenCASCADE CAD representation, filling the curve
loop `wireTag'. If `tag' is positive, set the tag explicitly;
otherwise a new tag is selected automatically. Return the tag of the
surface. If `pointTags' are provided, force the surface to pass
through the given points. The other optional arguments are `degree'
(the degree of the energy criterion to minimize for computing the
deformation of the surface), `numPointsOnCurves' (the average number
of points for discretisation of the bounding curves), `numIter' (the
maximum number of iterations of the optimization process),
`anisotropic' (improve performance when the ratio of the length along
the two parametric coordinates of the surface is high), `tol2d'
(tolerance to the constraints in the parametric plane of the surface),
`tol3d' (the maximum distance allowed between the support surface and
the constraints), `tolAng' (the maximum angle allowed between the
normal of the surface and the constraints), `tolCurv' (the maximum
difference of curvature allowed between the surface and the
constraint), `maxDegree' (the highest degree which the polynomial
defining the filling surface can have) and, `maxSegments' (the largest
number of segments which the filling surface can have)."
  (with-foreign-array (point-tags-ptr point-tags-n point-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-surface-filling wire-tag tag point-tags-ptr point-tags-n degree num-points-on-curves num-iter (if anisotropic 1 0) (to-double tol2d) (to-double tol3d) (to-double tol-ang) (to-double tol-curv) max-degree max-segments ierr))))

(defun b-spline-filling (wire-tag &key (tag -1) (type ""))
  "Add a BSpline surface in the OpenCASCADE CAD representation, filling
the curve loop `wireTag'. The curve loop should be made of 2, 3 or 4
curves. The optional `type' argument specifies the type of filling:
\"Stretch\" creates the flattest patch, \"Curved\" (the default)
creates the most rounded patch, and \"Coons\" creates a rounded patch
with less depth than \"Curved\". If `tag' is positive, set the tag
explicitly; otherwise a new tag is selected automatically. Return the
tag of the surface."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-b-spline-filling wire-tag tag type ierr)))

(defun bezier-filling (wire-tag &key (tag -1) (type ""))
  "Add a Bezier surface in the OpenCASCADE CAD representation, filling
the curve loop `wireTag'. The curve loop should be made of 2, 3 or 4
Bezier curves. The optional `type' argument specifies the type of
filling: \"Stretch\" creates the flattest patch, \"Curved\" (the
default) creates the most rounded patch, and \"Coons\" creates a
rounded patch with less depth than \"Curved\". If `tag' is positive,
set the tag explicitly; otherwise a new tag is selected automatically.
Return the tag of the surface."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-bezier-filling wire-tag tag type ierr)))

(defun b-spline-surface (point-tags num-points-u &key (tag -1) (degree-u 3) (degree-v 3) (weights '()) (knots-u '()) (knots-v '()) (multiplicities-u '()) (multiplicities-v '()) (wire-tags '()) (wire3-d nil))
  "Add a b-spline surface of degree `degreeU' x `degreeV' in the
OpenCASCADE CAD representation, with `pointTags' control points given
as a single vector [Pu1v1, ... Pu`numPointsU'v1, Pu1v2, ...]. If
`weights', `knotsU', `knotsV', `multiplicitiesU' or `multiplicitiesV'
are not provided, default parameters are computed automatically. If
`tag' is positive, set the tag explicitly; otherwise a new tag is
selected automatically. If `wireTags' is provided, trim the b-spline
patch using the provided wires: the first wire defines the external
contour, the others define holes. If `wire3D' is set, consider wire
curves as 3D curves and project them on the b-spline surface;
otherwise consider the wire curves as defined in the parametric space
of the surface. Return the tag of the b-spline surface."
  (with-foreign-array (point-tags-ptr point-tags-n point-tags :int)
      (with-foreign-array (weights-ptr weights-n weights :double)
      (with-foreign-array (knots-u-ptr knots-u-n knots-u :double)
      (with-foreign-array (knots-v-ptr knots-v-n knots-v :double)
      (with-foreign-array (multiplicities-u-ptr multiplicities-u-n multiplicities-u :int)
      (with-foreign-array (multiplicities-v-ptr multiplicities-v-n multiplicities-v :int)
      (with-foreign-array (wire-tags-ptr wire-tags-n wire-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-b-spline-surface point-tags-ptr point-tags-n num-points-u tag degree-u degree-v weights-ptr weights-n knots-u-ptr knots-u-n knots-v-ptr knots-v-n multiplicities-u-ptr multiplicities-u-n multiplicities-v-ptr multiplicities-v-n wire-tags-ptr wire-tags-n (if wire3-d 1 0) ierr))))))))))

(defun bezier-surface (point-tags num-points-u &key (tag -1) (wire-tags '()) (wire3-d nil))
  "Add a Bezier surface in the OpenCASCADE CAD representation, with
`pointTags' control points given as a single vector [Pu1v1, ...
Pu`numPointsU'v1, Pu1v2, ...]. If `tag' is positive, set the tag
explicitly; otherwise a new tag is selected automatically. If
`wireTags' is provided, trim the Bezier patch using the provided
wires: the first wire defines the external contour, the others define
holes. If `wire3D' is set, consider wire curves as 3D curves and
project them on the Bezier surface; otherwise consider the wire curves
as defined in the parametric space of the surface. Return the tag of
the Bezier surface."
  (with-foreign-array (point-tags-ptr point-tags-n point-tags :int)
      (with-foreign-array (wire-tags-ptr wire-tags-n wire-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-bezier-surface point-tags-ptr point-tags-n num-points-u tag wire-tags-ptr wire-tags-n (if wire3-d 1 0) ierr)))))

(defun trimmed-surface (surface-tag &key (wire-tags '()) (wire3-d nil) (tag -1))
  "Trim the surface `surfaceTag' with the wires `wireTags', replacing any
existing trimming curves. The first wire defines the external contour,
the others define holes. If `wire3D' is set, consider wire curves as
3D curves and project them on the surface; otherwise consider the wire
curves as defined in the parametric space of the surface. If `tag' is
positive, set the tag explicitly; otherwise a new tag is selected
automatically. Return the tag of the trimmed surface."
  (with-foreign-array (wire-tags-ptr wire-tags-n wire-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-trimmed-surface surface-tag wire-tags-ptr wire-tags-n (if wire3-d 1 0) tag ierr))))

(defun surface-loop (surface-tags &key (tag -1) (sewing nil))
  "Add a surface loop (a closed shell) in the OpenCASCADE CAD
representation, formed by `surfaceTags'.  If `tag' is positive, set
the tag explicitly; otherwise a new tag is selected automatically.
Return the tag of the surface loop. Setting `sewing' allows one to
build a shell made of surfaces that share geometrically identical (but
topologically different) curves."
  (with-foreign-array (surface-tags-ptr surface-tags-n surface-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-surface-loop surface-tags-ptr surface-tags-n tag (if sewing 1 0) ierr))))

(defun volume (shell-tags &key (tag -1))
  "Add a volume (a region) in the OpenCASCADE CAD representation, defined
by one or more surface loops `shellTags'. The first surface loop
defines the exterior boundary; additional surface loop define holes.
If `tag' is positive, set the tag explicitly; otherwise a new tag is
selected automatically. Return the tag of the volume."
  (with-foreign-array (shell-tags-ptr shell-tags-n shell-tags :int)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-volume shell-tags-ptr shell-tags-n tag ierr))))

(defun sphere (xc yc zc radius &key (tag -1) (angle1 (/ (- pi) 2)) (angle2 (/ pi 2)) (angle3 (* 2 pi)))
  "Add a sphere of center (`xc', `yc', `zc') and radius `r' in the
OpenCASCADE CAD representation. The optional `angle1' and `angle2'
arguments define the polar angle opening (from -Pi/2 to Pi/2). The
optional `angle3' argument defines the azimuthal opening (from 0 to
2*Pi). If `tag' is positive, set the tag explicitly; otherwise a new
tag is selected automatically. Return the tag of the sphere."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-sphere (to-double xc) (to-double yc) (to-double zc) (to-double radius) tag (to-double angle1) (to-double angle2) (to-double angle3) ierr)))

(defun box (x y z dx dy dz &key (tag -1))
  "Add a parallelepipedic box in the OpenCASCADE CAD representation,
defined by a point (`x', `y', `z') and the extents along the x-, y-
and z-axes. If `tag' is positive, set the tag explicitly; otherwise a
new tag is selected automatically. Return the tag of the box."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-box (to-double x) (to-double y) (to-double z) (to-double dx) (to-double dy) (to-double dz) tag ierr)))

(defun cylinder (x y z dx dy dz r &key (tag -1) (angle (* 2 pi)))
  "Add a cylinder in the OpenCASCADE CAD representation, defined by the
center (`x', `y', `z') of its first circular face, the 3 components
(`dx', `dy', `dz') of the vector defining its axis and its radius `r'.
The optional `angle' argument defines the angular opening (from 0 to
2*Pi). If `tag' is positive, set the tag explicitly; otherwise a new
tag is selected automatically. Return the tag of the cylinder."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-cylinder (to-double x) (to-double y) (to-double z) (to-double dx) (to-double dy) (to-double dz) (to-double r) tag (to-double angle) ierr)))

(defun cone (x y z dx dy dz r1 r2 &key (tag -1) (angle (* 2 pi)))
  "Add a cone in the OpenCASCADE CAD representation, defined by the
center (`x', `y', `z') of its first circular face, the 3 components of
the vector (`dx', `dy', `dz') defining its axis and the two radii `r1'
and `r2' of the faces (these radii can be zero). If `tag' is positive,
set the tag explicitly; otherwise a new tag is selected automatically.
`angle' defines the optional angular opening (from 0 to 2*Pi). Return
the tag of the cone."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-cone (to-double x) (to-double y) (to-double z) (to-double dx) (to-double dy) (to-double dz) (to-double r1) (to-double r2) tag (to-double angle) ierr)))

(defun wedge (x y z dx dy dz &key (tag -1) (ltx 0.0) (z-axis '()))
  "Add a right angular wedge in the OpenCASCADE CAD representation,
defined by the right-angle point (`x', `y', `z') and the 3 extends
along the x-, y- and z-axes (`dx', `dy', `dz'). If `tag' is positive,
set the tag explicitly; otherwise a new tag is selected automatically.
The optional argument `ltx' defines the top extent along the x-axis.
If a vector `zAxis' of size 3 is provided, use it to define the
z-axis. Return the tag of the wedge."
  (with-foreign-array (z-axis-ptr z-axis-n z-axis :double)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-wedge (to-double x) (to-double y) (to-double z) (to-double dx) (to-double dy) (to-double dz) tag (to-double ltx) z-axis-ptr z-axis-n ierr))))

(defun torus (x y z r1 r2 &key (tag -1) (angle (* 2 pi)) (z-axis '()))
  "Add a torus in the OpenCASCADE CAD representation, defined by its
center (`x', `y', `z') and its 2 radii `r' and `r2'. If `tag' is
positive, set the tag explicitly; otherwise a new tag is selected
automatically. The optional argument `angle' defines the angular
opening (from 0 to 2*Pi). If a vector `zAxis' of size 3 is provided,
use it to define the z-axis. Return the tag of the torus."
  (with-foreign-array (z-axis-ptr z-axis-n z-axis :double)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-torus (to-double x) (to-double y) (to-double z) (to-double r1) (to-double r2) tag (to-double angle) z-axis-ptr z-axis-n ierr))))

(defun thru-sections (wire-tags &key (tag -1) (make-solid t) (make-ruled nil) (max-degree -1) (continuity "") (parametrization "") (smoothing nil))
  "Add a volume (if the optional argument `makeSolid' is set) or surfaces
in the OpenCASCADE CAD representation, defined through the open or
closed wires `wireTags'. If `tag' is positive, set the tag explicitly;
otherwise a new tag is selected automatically. The new entities are
returned in `outDimTags' as a vector of (dim, tag) pairs. If the
optional argument `makeRuled' is set, the surfaces created on the
boundary are forced to be ruled surfaces. If `maxDegree' is positive,
set the maximal degree of resulting surface. The optional argument
`continuity' allows to specify the continuity of the resulting shape
(\"C0\", \"G1\", \"C1\", \"G2\", \"C2\", \"C3\", \"CN\"). The optional
argument `parametrization' sets the parametrization type
(\"ChordLength\", \"Centripetal\", \"IsoParametric\"). The optional
argument `smoothing' determines if smoothing is applied."
  (with-foreign-array (wire-tags-ptr wire-tags-n wire-tags :int)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-thru-sections wire-tags-ptr wire-tags-n out-dim-tags-out out-dim-tags-n-out tag (if make-solid 1 0) (if make-ruled 1 0) max-degree continuity parametrization (if smoothing 1 0) ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)))))

(defun thick-solid (volume-tag exclude-surface-tags offset &key (tag -1))
  "Add a hollowed volume in the OpenCASCADE CAD representation, built
from an initial volume `volumeTag' and a set of faces from this volume
`excludeSurfaceTags', which are to be removed. The remaining faces of
the volume become the walls of the hollowed solid, with thickness
`offset'. If `tag' is positive, set the tag explicitly; otherwise a
new tag is selected automatically."
  (with-foreign-array (exclude-surface-tags-ptr exclude-surface-tags-n exclude-surface-tags :int)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-thick-solid volume-tag exclude-surface-tags-ptr exclude-surface-tags-n (to-double offset) out-dim-tags-out out-dim-tags-n-out tag ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)))))

(defun extrude (dim-tags dx dy dz &key (num-elements '()) (heights '()) (recombine nil))
  "Extrude the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the OpenCASCADE CAD representation, using a translation along
(`dx', `dy', `dz'). Return extruded entities in `outDimTags'. If the
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
      (gmsh/internal::%model-occ-extrude dim-tags-ptr dim-tags-n (to-double dx) (to-double dy) (to-double dz) out-dim-tags-out out-dim-tags-n-out num-elements-ptr num-elements-n heights-ptr heights-n (if recombine 1 0) ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)))))))

(defun revolve (dim-tags x y z ax ay az angle &key (num-elements '()) (heights '()) (recombine nil))
  "Extrude the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the OpenCASCADE CAD representation, using a rotation of `angle'
radians around the axis of revolution defined by the point (`x', `y',
`z') and the direction (`ax', `ay', `az'). Return extruded entities in
`outDimTags'. If the `numElements' vector is not empty, also extrude
the mesh: the entries in `numElements' give the number of elements in
each layer. If the `height' vector is not empty, it provides the
(cumulative) height of the different layers, normalized to 1. When the
mesh is extruded the angle should be strictly smaller than 2*Pi. If
`recombine' is set, recombine the mesh in the layers."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-foreign-array (num-elements-ptr num-elements-n num-elements :int)
      (with-foreign-array (heights-ptr heights-n heights :double)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-revolve dim-tags-ptr dim-tags-n (to-double x) (to-double y) (to-double z) (to-double ax) (to-double ay) (to-double az) (to-double angle) out-dim-tags-out out-dim-tags-n-out num-elements-ptr num-elements-n heights-ptr heights-n (if recombine 1 0) ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)))))))

(defun pipe (dim-tags wire-tag &key (trihedron ""))
  "Add a pipe in the OpenCASCADE CAD representation, by extruding the
entities `dimTags' (given as a vector of (dim, tag) pairs) along the
wire `wireTag'. The type of sweep can be specified with `trihedron'
(possible values: \"DiscreteTrihedron\", \"CorrectedFrenet\",
\"Fixed\", \"Frenet\", \"ConstantNormal\", \"Darboux\", \"GuideAC\",
\"GuidePlan\", \"GuideACWithContact\", \"GuidePlanWithContact\"). If
`trihedron' is not provided, \"DiscreteTrihedron\" is assumed. Return
the pipe in `outDimTags'."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-add-pipe dim-tags-ptr dim-tags-n wire-tag out-dim-tags-out out-dim-tags-n-out trihedron ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)))))

(defun fillet (volume-tags curve-tags radii &key (remove-volume t))
  "Fillet the volumes `volumeTags' on the curves `curveTags' with radii
`radii'. The `radii' vector can either contain a single radius, as
many radii as `curveTags', or twice as many as `curveTags' (in which
case different radii are provided for the begin and end points of the
curves). Return the filleted entities in `outDimTags' as a vector of
(dim, tag) pairs. Remove the original volume if `removeVolume' is set."
  (with-foreign-array (volume-tags-ptr volume-tags-n volume-tags :int)
      (with-foreign-array (curve-tags-ptr curve-tags-n curve-tags :int)
      (with-foreign-array (radii-ptr radii-n radii :double)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-fillet volume-tags-ptr volume-tags-n curve-tags-ptr curve-tags-n radii-ptr radii-n out-dim-tags-out out-dim-tags-n-out (if remove-volume 1 0) ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)))))))

(defun chamfer (volume-tags curve-tags surface-tags distances &key (remove-volume t))
  "Chamfer the volumes `volumeTags' on the curves `curveTags' with
distances `distances' measured on surfaces `surfaceTags'. The
`distances' vector can either contain a single distance, as many
distances as `curveTags' and `surfaceTags', or twice as many as
`curveTags' and `surfaceTags' (in which case the first in each pair is
measured on the corresponding surface in `surfaceTags', the other on
the other adjacent surface). Return the chamfered entities in
`outDimTags'. Remove the original volume if `removeVolume' is set."
  (with-foreign-array (volume-tags-ptr volume-tags-n volume-tags :int)
      (with-foreign-array (curve-tags-ptr curve-tags-n curve-tags :int)
      (with-foreign-array (surface-tags-ptr surface-tags-n surface-tags :int)
      (with-foreign-array (distances-ptr distances-n distances :double)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-chamfer volume-tags-ptr volume-tags-n curve-tags-ptr curve-tags-n surface-tags-ptr surface-tags-n distances-ptr distances-n out-dim-tags-out out-dim-tags-n-out (if remove-volume 1 0) ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long))))))))

(defun defeature (volume-tags surface-tags &key (remove-volume t))
  "Defeature the volumes `volumeTags' by removing the surfaces
`surfaceTags'. Return the defeatured entities in `outDimTags'. Remove
the original volume if `removeVolume' is set."
  (with-foreign-array (volume-tags-ptr volume-tags-n volume-tags :int)
      (with-foreign-array (surface-tags-ptr surface-tags-n surface-tags :int)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-defeature volume-tags-ptr volume-tags-n surface-tags-ptr surface-tags-n out-dim-tags-out out-dim-tags-n-out (if remove-volume 1 0) ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long))))))

(defun fillet2-d (edge-tag1 edge-tag2 radius &key (tag -1) (point-tag -1) (reverse nil))
  "Create a fillet edge between edges `edgeTag1' and `edgeTag2' with
radius `radius'. The modifed edges keep their tag. If `tag' is
positive, set the tag explicitly; otherwise a new tag is selected
automatically. If `pointTag' is positive, set the point on the edge at
which the fillet is created. If `reverse' is set, the normal of the
plane through the two planes is reversed before the fillet is created."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-fillet2-d edge-tag1 edge-tag2 (to-double radius) tag point-tag (if reverse 1 0) ierr)))

(defun chamfer2-d (edge-tag1 edge-tag2 distance1 distance2 &key (tag -1))
  "Create a chamfer edge between edges `edgeTag1' and `edgeTag2' with
distance1 `distance1' and distance2 `distance2'. The modifed edges
keep their tag. If `tag' is positive, set the tag explicitly;
otherwise a new tag is selected automatically."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-chamfer2-d edge-tag1 edge-tag2 (to-double distance1) (to-double distance2) tag ierr)))

(defun offset-curve (curve-loop-tag offset)
  "Create an offset curve based on the curve loop `curveLoopTag' with
offset `offset'. Return the offset curves in `outDimTags' as a vector
of (dim, tag) pairs."
  (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-offset-curve curve-loop-tag (to-double offset) out-dim-tags-out out-dim-tags-n-out ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long))))

(defun get-distance (dim1 tag1 dim2 tag2)
  "Find the minimal distance between shape with `dim1' and `tag1' and
shape with `dim2' and `tag2' and the according coordinates. Return the
distance in `distance' and the coordinates of the points as `x1',
`y1', `z1' and `x2', `y2', `z2'. A negative `distance' indicates
failure."
  (cffi:with-foreign-objects ((distance-out :double) (x1-out :double) (y1-out :double) (z1-out :double) (x2-out :double) (y2-out :double) (z2-out :double))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-get-distance dim1 tag1 dim2 tag2 distance-out x1-out y1-out z1-out x2-out y2-out z2-out ierr))
    (values (cffi:mem-ref distance-out :double) (cffi:mem-ref x1-out :double) (cffi:mem-ref y1-out :double) (cffi:mem-ref z1-out :double) (cffi:mem-ref x2-out :double) (cffi:mem-ref y2-out :double) (cffi:mem-ref z2-out :double))))

(defun get-closest-entities (x y z dim-tags &key (n 1))
  "Find the `n' closest entities to point (`x', `y', `z') amongst the
entities `dimTags'. Return the entities in `outDimTags' sorted by
increasing distance, the corresponding distances in `distances', and
the correspdonding closest x, y, z coordinates, concatenated, in
`coord'."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long) (distances-out :pointer) (distances-n-out :unsigned-long) (coord-out :pointer) (coord-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-get-closest-entities (to-double x) (to-double y) (to-double z) dim-tags-ptr dim-tags-n out-dim-tags-out out-dim-tags-n-out distances-out distances-n-out coord-out coord-n-out n ierr))
    (values (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)) (foreign-array-to-list (cffi:mem-ref distances-out :pointer) (cffi:mem-ref distances-n-out :unsigned-long) :double) (foreign-array-to-list (cffi:mem-ref coord-out :pointer) (cffi:mem-ref coord-n-out :unsigned-long) :double)))))

(defun fuse (object-dim-tags tool-dim-tags &key (tag -1) (remove-object t) (remove-tool t))
  "Compute the boolean union (the fusion) of the entities `objectDimTags'
and `toolDimTags' (vectors of (dim, tag) pairs) in the OpenCASCADE CAD
representation. Return the resulting entities in `outDimTags', and the
correspondance between input and resulting entities in
`outDimTagsMap'. If `tag' is positive, try to set the tag explicitly
(only valid if the boolean operation results in a single entity).
Remove the object if `removeObject' is set. Remove the tool if
`removeTool' is set."
  (with-pairs-array (object-dim-tags-ptr object-dim-tags-n object-dim-tags)
      (with-pairs-array (tool-dim-tags-ptr tool-dim-tags-n tool-dim-tags)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long) (out-dim-tags-map-out :pointer) (out-dim-tags-map-n-out :pointer) (out-dim-tags-map-nn-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-fuse object-dim-tags-ptr object-dim-tags-n tool-dim-tags-ptr tool-dim-tags-n out-dim-tags-out out-dim-tags-n-out out-dim-tags-map-out out-dim-tags-map-n-out out-dim-tags-map-nn-out tag (if remove-object 1 0) (if remove-tool 1 0) ierr))
    (values (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)) (foreign-vector-pairs-to-list (cffi:mem-ref out-dim-tags-map-out :pointer) (cffi:mem-ref out-dim-tags-map-n-out :pointer) (cffi:mem-ref out-dim-tags-map-nn-out :unsigned-long)))))))

(defun intersect (object-dim-tags tool-dim-tags &key (tag -1) (remove-object t) (remove-tool t))
  "Compute the boolean intersection (the common parts) of the entities
`objectDimTags' and `toolDimTags' (vectors of (dim, tag) pairs) in the
OpenCASCADE CAD representation. Return the resulting entities in
`outDimTags', and the correspondance between input and resulting
entities in `outDimTagsMap'. If `tag' is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if `removeObject' is set. Remove the tool
if `removeTool' is set."
  (with-pairs-array (object-dim-tags-ptr object-dim-tags-n object-dim-tags)
      (with-pairs-array (tool-dim-tags-ptr tool-dim-tags-n tool-dim-tags)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long) (out-dim-tags-map-out :pointer) (out-dim-tags-map-n-out :pointer) (out-dim-tags-map-nn-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-intersect object-dim-tags-ptr object-dim-tags-n tool-dim-tags-ptr tool-dim-tags-n out-dim-tags-out out-dim-tags-n-out out-dim-tags-map-out out-dim-tags-map-n-out out-dim-tags-map-nn-out tag (if remove-object 1 0) (if remove-tool 1 0) ierr))
    (values (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)) (foreign-vector-pairs-to-list (cffi:mem-ref out-dim-tags-map-out :pointer) (cffi:mem-ref out-dim-tags-map-n-out :pointer) (cffi:mem-ref out-dim-tags-map-nn-out :unsigned-long)))))))

(defun cut (object-dim-tags tool-dim-tags &key (tag -1) (remove-object t) (remove-tool t))
  "Compute the boolean difference between the entities `objectDimTags'
and `toolDimTags' (given as vectors of (dim, tag) pairs) in the
OpenCASCADE CAD representation. Return the resulting entities in
`outDimTags', and the correspondance between input and resulting
entities in `outDimTagsMap'. If `tag' is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if `removeObject' is set. Remove the tool
if `removeTool' is set."
  (with-pairs-array (object-dim-tags-ptr object-dim-tags-n object-dim-tags)
      (with-pairs-array (tool-dim-tags-ptr tool-dim-tags-n tool-dim-tags)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long) (out-dim-tags-map-out :pointer) (out-dim-tags-map-n-out :pointer) (out-dim-tags-map-nn-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-cut object-dim-tags-ptr object-dim-tags-n tool-dim-tags-ptr tool-dim-tags-n out-dim-tags-out out-dim-tags-n-out out-dim-tags-map-out out-dim-tags-map-n-out out-dim-tags-map-nn-out tag (if remove-object 1 0) (if remove-tool 1 0) ierr))
    (values (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)) (foreign-vector-pairs-to-list (cffi:mem-ref out-dim-tags-map-out :pointer) (cffi:mem-ref out-dim-tags-map-n-out :pointer) (cffi:mem-ref out-dim-tags-map-nn-out :unsigned-long)))))))

(defun fragment (object-dim-tags tool-dim-tags &key (tag -1) (remove-object t) (remove-tool t))
  "Compute the boolean fragments (general fuse) resulting from the
intersection of the entities `objectDimTags' and `toolDimTags' (given
as vectors of (dim, tag) pairs) in the OpenCASCADE CAD representation,
making all interfaces conformal. When applied to entities of different
dimensions, the lower dimensional entities will be automatically
embedded in the higher dimensional entities if they are not on their
boundary. Return the resulting entities in `outDimTags', and the
correspondance between input and resulting entities in
`outDimTagsMap'. If `tag' is positive, try to set the tag explicitly
(only valid if the boolean operation results in a single entity).
Remove the object if `removeObject' is set. Remove the tool if
`removeTool' is set."
  (with-pairs-array (object-dim-tags-ptr object-dim-tags-n object-dim-tags)
      (with-pairs-array (tool-dim-tags-ptr tool-dim-tags-n tool-dim-tags)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long) (out-dim-tags-map-out :pointer) (out-dim-tags-map-n-out :pointer) (out-dim-tags-map-nn-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-fragment object-dim-tags-ptr object-dim-tags-n tool-dim-tags-ptr tool-dim-tags-n out-dim-tags-out out-dim-tags-n-out out-dim-tags-map-out out-dim-tags-map-n-out out-dim-tags-map-nn-out tag (if remove-object 1 0) (if remove-tool 1 0) ierr))
    (values (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)) (foreign-vector-pairs-to-list (cffi:mem-ref out-dim-tags-map-out :pointer) (cffi:mem-ref out-dim-tags-map-n-out :pointer) (cffi:mem-ref out-dim-tags-map-nn-out :unsigned-long)))))))

(defun translate (dim-tags dx dy dz)
  "Translate the entities `dimTags' (given as a vector of (dim, tag)
pairs) in the OpenCASCADE CAD representation along (`dx', `dy', `dz')."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-translate dim-tags-ptr dim-tags-n (to-double dx) (to-double dy) (to-double dz) ierr))))

(defun rotate (dim-tags x y z ax ay az angle)
  "Rotate the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the OpenCASCADE CAD representation by `angle' radians around the
axis of revolution defined by the point (`x', `y', `z') and the
direction (`ax', `ay', `az')."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-rotate dim-tags-ptr dim-tags-n (to-double x) (to-double y) (to-double z) (to-double ax) (to-double ay) (to-double az) (to-double angle) ierr))))

(defun dilate (dim-tags x y z a b c)
  "Scale the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the OpenCASCADE CAD representation by factors `a', `b' and `c'
along the three coordinate axes; use (`x', `y', `z') as the center of
the homothetic transformation."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-dilate dim-tags-ptr dim-tags-n (to-double x) (to-double y) (to-double z) (to-double a) (to-double b) (to-double c) ierr))))

(defun mirror (dim-tags a b c d)
  "Mirror the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the OpenCASCADE CAD representation, with respect to the plane of
equation `a' * x + `b' * y + `c' * z + `d' = 0."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-mirror dim-tags-ptr dim-tags-n (to-double a) (to-double b) (to-double c) (to-double d) ierr))))

(defun symmetrize (dim-tags a b c d)
  "Mirror the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the OpenCASCADE CAD representation, with respect to the plane of
equation `a' * x + `b' * y + `c' * z + `d' = 0. (This is a deprecated
synonym for `mirror'.)"
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-symmetrize dim-tags-ptr dim-tags-n (to-double a) (to-double b) (to-double c) (to-double d) ierr))))

(defun affine-transform (dim-tags affine-transform)
  "Apply a general affine transformation matrix `affineTransform' (16
entries of a 4x4 matrix, by row; only the 12 first can be provided for
convenience) to the entities `dimTags' (given as a vector of (dim,
tag) pairs) in the OpenCASCADE CAD representation."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-foreign-array (affine-transform-ptr affine-transform-n affine-transform :double)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-affine-transform dim-tags-ptr dim-tags-n affine-transform-ptr affine-transform-n ierr)))))

(defun copy (dim-tags)
  "Copy the entities `dimTags' in the OpenCASCADE CAD representation; the
new entities are returned in `outDimTags'."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-copy dim-tags-ptr dim-tags-n out-dim-tags-out out-dim-tags-n-out ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)))))

(defun remove (dim-tags &key (recursive nil))
  "Remove the entities `dimTags' (given as a vector of (dim, tag) pairs)
in the OpenCASCADE CAD representation, provided that they are not on
the boundary of higher-dimensional entities. If `recursive' is true,
remove all the entities on their boundaries, down to dimension 0."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-remove dim-tags-ptr dim-tags-n (if recursive 1 0) ierr))))

(defun remove-all-duplicates ()
  "Remove all duplicate entities in the OpenCASCADE CAD representation
(different entities at the same geometrical location) after
intersecting (using boolean fragments) all highest dimensional
entities."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-remove-all-duplicates  ierr)))

(defun heal-shapes (&key (dim-tags '()) (tolerance 1e-8.0) (fix-degenerated t) (fix-small-edges t) (fix-small-faces t) (sew-faces t) (make-solids t))
  "Apply various healing procedures to the entities `dimTags' (given as a
vector of (dim, tag) pairs), or to all the entities in the model if
`dimTags' is empty, in the OpenCASCADE CAD representation. Return the
healed entities in `outDimTags'."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-heal-shapes out-dim-tags-out out-dim-tags-n-out dim-tags-ptr dim-tags-n (to-double tolerance) (if fix-degenerated 1 0) (if fix-small-edges 1 0) (if fix-small-faces 1 0) (if sew-faces 1 0) (if make-solids 1 0) ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long)))))

(defun convert-to-nurbs (dim-tags)
  "Convert the entities `dimTags' to NURBS."
  (with-pairs-array (dim-tags-ptr dim-tags-n dim-tags)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-convert-to-nurbs dim-tags-ptr dim-tags-n ierr))))

(defun import-shapes (file-name &key (highest-dim-only t) (format ""))
  "Import BREP, STEP or IGES shapes from the file `fileName' in the
OpenCASCADE CAD representation. The imported entities are returned in
`outDimTags', as a vector of (dim, tag) pairs. If the optional
argument `highestDimOnly' is set, only import the highest dimensional
entities in the file. The optional argument `format' can be used to
force the format of the file (currently \"brep\", \"step\" or
\"iges\")."
  (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-import-shapes file-name out-dim-tags-out out-dim-tags-n-out (if highest-dim-only 1 0) format ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long))))

(defun import-shapes-native-pointer (shape &key (highest-dim-only t))
  "Import an OpenCASCADE `shape' by providing a pointer to a native
OpenCASCADE `TopoDS_Shape' object (passed as a pointer to void). The
imported entities are returned in `outDimTags' as a vector of (dim,
tag) pairs. If the optional argument `highestDimOnly' is set, only
import the highest dimensional entities in `shape'. In Python, this
function can be used for integration with PythonOCC, in which the
SwigPyObject pointer of `TopoDS_Shape' must be passed as an int to
`shape', i.e., `shape = int(pythonocc_shape.this)'. Warning: this
function is unsafe, as providing an invalid pointer will lead to
undefined behavior."
  (cffi:with-foreign-objects ((out-dim-tags-out :pointer) (out-dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-import-shapes-native-pointer shape out-dim-tags-out out-dim-tags-n-out (if highest-dim-only 1 0) ierr))
    (dim-tags-to-pairs (cffi:mem-ref out-dim-tags-out :pointer) (cffi:mem-ref out-dim-tags-n-out :unsigned-long))))

(defun get-entities (&key (dim -1))
  "Get all the OpenCASCADE entities. If `dim' is >= 0, return only the
entities of the specified dimension (e.g. points if `dim' == 0). The
entities are returned as a vector of (dim, tag) pairs."
  (cffi:with-foreign-objects ((dim-tags-out :pointer) (dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-get-entities dim-tags-out dim-tags-n-out dim ierr))
    (dim-tags-to-pairs (cffi:mem-ref dim-tags-out :pointer) (cffi:mem-ref dim-tags-n-out :unsigned-long))))

(defun get-entities-in-bounding-box (xmin ymin zmin xmax ymax zmax &key (dim -1))
  "Get the OpenCASCADE entities in the bounding box defined by the two
points (`xmin', `ymin', `zmin') and (`xmax', `ymax', `zmax'). If `dim'
is >= 0, return only the entities of the specified dimension (e.g.
points if `dim' == 0)."
  (cffi:with-foreign-objects ((dim-tags-out :pointer) (dim-tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-get-entities-in-bounding-box (to-double xmin) (to-double ymin) (to-double zmin) (to-double xmax) (to-double ymax) (to-double zmax) dim-tags-out dim-tags-n-out dim ierr))
    (dim-tags-to-pairs (cffi:mem-ref dim-tags-out :pointer) (cffi:mem-ref dim-tags-n-out :unsigned-long))))

(defun get-bounding-box (dim tag)
  "Get the bounding box (`xmin', `ymin', `zmin'), (`xmax', `ymax',
`zmax') of the OpenCASCADE entity of dimension `dim' and tag `tag'."
  (cffi:with-foreign-objects ((xmin-out :double) (ymin-out :double) (zmin-out :double) (xmax-out :double) (ymax-out :double) (zmax-out :double))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-get-bounding-box dim tag xmin-out ymin-out zmin-out xmax-out ymax-out zmax-out ierr))
    (values (cffi:mem-ref xmin-out :double) (cffi:mem-ref ymin-out :double) (cffi:mem-ref zmin-out :double) (cffi:mem-ref xmax-out :double) (cffi:mem-ref ymax-out :double) (cffi:mem-ref zmax-out :double))))

(defun get-curve-loops (surface-tag)
  "Get the tags `curveLoopTags' of the curve loops making up the surface
of tag `surfaceTag', as well as the tags `curveTags' of the curves
making up each curve loop."
  (cffi:with-foreign-objects ((curve-loop-tags-out :pointer) (curve-loop-tags-n-out :unsigned-long) (curve-tags-out :pointer) (curve-tags-n-out :pointer) (curve-tags-nn-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-get-curve-loops surface-tag curve-loop-tags-out curve-loop-tags-n-out curve-tags-out curve-tags-n-out curve-tags-nn-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref curve-loop-tags-out :pointer) (cffi:mem-ref curve-loop-tags-n-out :unsigned-long) :int) (foreign-vectors-to-list (cffi:mem-ref curve-tags-out :pointer) (cffi:mem-ref curve-tags-n-out :pointer) (cffi:mem-ref curve-tags-nn-out :unsigned-long) :int))))

(defun get-surface-loops (volume-tag)
  "Get the tags `surfaceLoopTags' of the surface loops making up the
volume of tag `volumeTag', as well as the tags `surfaceTags' of the
surfaces making up each surface loop."
  (cffi:with-foreign-objects ((surface-loop-tags-out :pointer) (surface-loop-tags-n-out :unsigned-long) (surface-tags-out :pointer) (surface-tags-n-out :pointer) (surface-tags-nn-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-get-surface-loops volume-tag surface-loop-tags-out surface-loop-tags-n-out surface-tags-out surface-tags-n-out surface-tags-nn-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref surface-loop-tags-out :pointer) (cffi:mem-ref surface-loop-tags-n-out :unsigned-long) :int) (foreign-vectors-to-list (cffi:mem-ref surface-tags-out :pointer) (cffi:mem-ref surface-tags-n-out :pointer) (cffi:mem-ref surface-tags-nn-out :unsigned-long) :int))))

(defun get-mass (dim tag)
  "Get the mass of the OpenCASCADE entity of dimension `dim' and tag
`tag'. If no density is attached to the entity (the default), the
value corresponds respectively to the length, area and volume for
`dim' = 1, 2 and 3."
  (cffi:with-foreign-object (mass-out :double)
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-get-mass dim tag mass-out ierr))
    (cffi:mem-ref mass-out :double)))

(defun get-center-of-mass (dim tag)
  "Get the center of mass of the OpenCASCADE entity of dimension `dim'
and tag `tag'."
  (cffi:with-foreign-objects ((x-out :double) (y-out :double) (z-out :double))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-get-center-of-mass dim tag x-out y-out z-out ierr))
    (values (cffi:mem-ref x-out :double) (cffi:mem-ref y-out :double) (cffi:mem-ref z-out :double))))

(defun get-matrix-of-inertia (dim tag)
  "Get the matrix of inertia (by row) of the OpenCASCADE entity of
dimension `dim' and tag `tag'."
  (cffi:with-foreign-objects ((mat-out :pointer) (mat-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%model-occ-get-matrix-of-inertia dim tag mat-out mat-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref mat-out :pointer) (cffi:mem-ref mat-n-out :unsigned-long) :double)))

(defun get-max-tag (dim)
  "Get the maximum tag of entities of dimension `dim' in the OpenCASCADE
CAD representation."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-get-max-tag dim ierr)))

(defun set-max-tag (dim max-tag)
  "Set the maximum tag `maxTag' for entities of dimension `dim' in the
OpenCASCADE CAD representation."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-set-max-tag dim max-tag ierr)))

(defun synchronize ()
  "Synchronize the OpenCASCADE CAD representation with the current Gmsh
model. This can be called at any time, but since it involves a non
trivial amount of processing, the number of synchronization points
should normally be minimized. Without synchronization the entities in
the OpenCASCADE CAD representation are not available to any function
outside of the OpenCASCADE CAD kernel functions."
  (with-ierr (ierr)
      (gmsh/internal::%model-occ-synchronize  ierr)))

