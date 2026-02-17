;;;; view-functions.lisp — Generated wrappers for gmsh/view
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(in-package :gmsh/view)

(defun add (name &key (tag -1))
  "Add a new post-processing view, with name `name'. If `tag' is positive
use it (and remove the view with that tag if it already exists),
otherwise associate a new tag. Return the view tag."
  (with-ierr (ierr)
      (gmsh/internal::%view-add name tag ierr)))

(defun remove (tag)
  "Remove the view with tag `tag'."
  (with-ierr (ierr)
      (gmsh/internal::%view-remove tag ierr)))

(defun get-index (tag)
  "Get the index of the view with tag `tag' in the list of currently
loaded views. This dynamic index (it can change when views are
removed) is used to access view options."
  (with-ierr (ierr)
      (gmsh/internal::%view-get-index tag ierr)))

(defun get-tags ()
  "Get the tags of all views."
  (cffi:with-foreign-objects ((tags-out :pointer) (tags-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%view-get-tags tags-out tags-n-out ierr))
    (foreign-array-to-list (cffi:mem-ref tags-out :pointer) (cffi:mem-ref tags-n-out :unsigned-long) :int)))

(defun add-model-data (tag step model-name data-type tags data &key (time 0.0) (num-components -1) (partition 0))
  "Add model-based post-processing data to the view with tag `tag'.
`modelName' identifies the model the data is attached to. `dataType'
specifies the type of data, currently either \"NodeData\",
\"ElementData\" or \"ElementNodeData\". `step' specifies the
identifier (>= 0) of the data in a sequence. `tags' gives the tags of
the nodes or elements in the mesh to which the data is associated.
`data' is a vector of the same length as `tags': each entry is the
vector of double precision numbers representing the data associated
with the corresponding tag. The optional `time' argument associate a
time value with the data. `numComponents' gives the number of data
components (1 for scalar data, 3 for vector data, etc.) per entity; if
negative, it is automatically inferred (when possible) from the input
data. `partition' allows one to specify data in several sub-sets."
  (with-foreign-array (tags-ptr tags-n tags :unsigned-long)
      (with-vector-vector-double (data-ptrs data-sizes data-nn data)
      (with-ierr (ierr)
      (gmsh/internal::%view-add-model-data tag step model-name data-type tags-ptr tags-n data-ptrs data-sizes data-nn (to-double time) num-components partition ierr)))))

(defun add-homogeneous-model-data (tag step model-name data-type tags data &key (time 0.0) (num-components -1) (partition 0))
  "Add homogeneous model-based post-processing data to the view with tag
`tag'. The arguments have the same meaning as in `addModelData',
except that `data' is supposed to be homogeneous and is thus flattened
in a single vector. For data types that can lead to different data
sizes per tag (like \"ElementNodeData\"), the data should be padded."
  (with-foreign-array (tags-ptr tags-n tags :unsigned-long)
      (with-foreign-array (data-ptr data-n data :double)
      (with-ierr (ierr)
      (gmsh/internal::%view-add-homogeneous-model-data tag step model-name data-type tags-ptr tags-n data-ptr data-n (to-double time) num-components partition ierr)))))

(defun get-model-data (tag step)
  "Get model-based post-processing data from the view with tag `tag' at
step `step'. Return the `data' associated to the nodes or the elements
with tags `tags', as well as the `dataType' and the number of
components `numComponents'."
  (cffi:with-foreign-objects ((data-type-out :pointer) (tags-out :pointer) (tags-n-out :unsigned-long) (data-out :pointer) (data-n-out :pointer) (data-nn-out :unsigned-long) (time-out :double) (num-components-out :int))
      (with-ierr (ierr)
      (gmsh/internal::%view-get-model-data tag step data-type-out tags-out tags-n-out data-out data-n-out data-nn-out time-out num-components-out ierr))
    (values (foreign-string-result (cffi:mem-ref data-type-out :pointer)) (foreign-array-to-list (cffi:mem-ref tags-out :pointer) (cffi:mem-ref tags-n-out :unsigned-long) :unsigned-long) (foreign-vectors-to-list (cffi:mem-ref data-out :pointer) (cffi:mem-ref data-n-out :pointer) (cffi:mem-ref data-nn-out :unsigned-long) :double) (cffi:mem-ref time-out :double) (cffi:mem-ref num-components-out :int))))

(defun get-homogeneous-model-data (tag step)
  "Get homogeneous model-based post-processing data from the view with
tag `tag' at step `step'. The arguments have the same meaning as in
`getModelData', except that `data' is returned flattened in a single
vector, with the appropriate padding if necessary."
  (cffi:with-foreign-objects ((data-type-out :pointer) (tags-out :pointer) (tags-n-out :unsigned-long) (data-out :pointer) (data-n-out :unsigned-long) (time-out :double) (num-components-out :int))
      (with-ierr (ierr)
      (gmsh/internal::%view-get-homogeneous-model-data tag step data-type-out tags-out tags-n-out data-out data-n-out time-out num-components-out ierr))
    (values (foreign-string-result (cffi:mem-ref data-type-out :pointer)) (foreign-array-to-list (cffi:mem-ref tags-out :pointer) (cffi:mem-ref tags-n-out :unsigned-long) :unsigned-long) (foreign-array-to-list (cffi:mem-ref data-out :pointer) (cffi:mem-ref data-n-out :unsigned-long) :double) (cffi:mem-ref time-out :double) (cffi:mem-ref num-components-out :int))))

(defun add-list-data (tag data-type num-ele data)
  "Add list-based post-processing data to the view with tag `tag'. List-
based datasets are independent from any model and any mesh. `dataType'
identifies the data by concatenating the field type (\"S\" for scalar,
\"V\" for vector, \"T\" for tensor) and the element type (\"P\" for
point, \"L\" for line, \"T\" for triangle, \"S\" for tetrahedron,
\"I\" for prism, \"H\" for hexaHedron, \"Y\" for pyramid). For example
`dataType' should be \"ST\" for a scalar field on triangles. `numEle'
gives the number of elements in the data. `data' contains the data for
the `numEle' elements, concatenated, with node coordinates followed by
values per node, repeated for each step: [e1x1, ..., e1xn, e1y1, ...,
e1yn, e1z1, ..., e1zn, e1v1..., e1vN, e2x1, ...]."
  (with-foreign-array (data-ptr data-n data :double)
      (with-ierr (ierr)
      (gmsh/internal::%view-add-list-data tag data-type num-ele data-ptr data-n ierr))))

(defun get-list-data (tag &key (return-adaptive nil))
  "Get list-based post-processing data from the view with tag `tag'.
Return the types `dataTypes', the number of elements `numElements' for
each data type and the `data' for each data type. If `returnAdaptive'
is set, return the data obtained after adaptive refinement, if
available."
  (cffi:with-foreign-objects ((data-type-out :pointer) (data-type-n-out :unsigned-long) (num-elements-out :pointer) (num-elements-n-out :unsigned-long) (data-out :pointer) (data-n-out :pointer) (data-nn-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%view-get-list-data tag data-type-out data-type-n-out num-elements-out num-elements-n-out data-out data-n-out data-nn-out (if return-adaptive 1 0) ierr))
    (values (foreign-string-array-to-list (cffi:mem-ref data-type-out :pointer) (cffi:mem-ref data-type-n-out :unsigned-long)) (foreign-array-to-list (cffi:mem-ref num-elements-out :pointer) (cffi:mem-ref num-elements-n-out :unsigned-long) :int) (foreign-vectors-to-list (cffi:mem-ref data-out :pointer) (cffi:mem-ref data-n-out :pointer) (cffi:mem-ref data-nn-out :unsigned-long) :double))))

(defun add-list-data-string (tag coord data &key (style '()))
  "Add a string to a list-based post-processing view with tag `tag'. If
`coord' contains 3 coordinates the string is positioned in the 3D
model space (\"3D string\"); if it contains 2 coordinates it is
positioned in the 2D graphics viewport (\"2D string\"). `data'
contains one or more (for multistep views) strings. `style' contains
key-value pairs of styling parameters, concatenated. Available keys
are \"Font\" (possible values: \"Times-Roman\", \"Times-Bold\",
\"Times-Italic\", \"Times-BoldItalic\", \"Helvetica\", \"Helvetica-
Bold\", \"Helvetica-Oblique\", \"Helvetica-BoldOblique\", \"Courier\",
\"Courier-Bold\", \"Courier-Oblique\", \"Courier-BoldOblique\",
\"Symbol\", \"ZapfDingbats\", \"Screen\"), \"FontSize\" and \"Align\"
(possible values: \"Left\" or \"BottomLeft\", \"Center\" or
\"BottomCenter\", \"Right\" or \"BottomRight\", \"TopLeft\",
\"TopCenter\", \"TopRight\", \"CenterLeft\", \"CenterCenter\",
\"CenterRight\")."
  (with-foreign-array (coord-ptr coord-n coord :double)
      (with-string-array (data-ptr data-n data)
      (with-string-array (style-ptr style-n style)
      (with-ierr (ierr)
      (gmsh/internal::%view-add-list-data-string tag coord-ptr coord-n data-ptr data-n style-ptr style-n ierr))))))

(defun get-list-data-strings (tag dim)
  "Get list-based post-processing data strings (2D strings if `dim' == 2,
3D strings if `dim' = 3) from the view with tag `tag'. Return the
coordinates in `coord', the strings in `data' and the styles in
`style'."
  (cffi:with-foreign-objects ((coord-out :pointer) (coord-n-out :unsigned-long) (data-out :pointer) (data-n-out :unsigned-long) (style-out :pointer) (style-n-out :unsigned-long))
      (with-ierr (ierr)
      (gmsh/internal::%view-get-list-data-strings tag dim coord-out coord-n-out data-out data-n-out style-out style-n-out ierr))
    (values (foreign-array-to-list (cffi:mem-ref coord-out :pointer) (cffi:mem-ref coord-n-out :unsigned-long) :double) (foreign-string-array-to-list (cffi:mem-ref data-out :pointer) (cffi:mem-ref data-n-out :unsigned-long)) (foreign-string-array-to-list (cffi:mem-ref style-out :pointer) (cffi:mem-ref style-n-out :unsigned-long)))))

(defun set-interpolation-matrices (tag type d coef exp &key (d-geo 0) (coef-geo '()) (exp-geo '()))
  "Set interpolation matrices for the element family `type' (\"Line\",
\"Triangle\", \"Quadrangle\", \"Tetrahedron\", \"Hexahedron\",
\"Prism\", \"Pyramid\") in the view `tag'. The approximation of the
values over an element is written as a linear combination of `d' basis
functions f_i(u, v, w) = sum_(j = 0, ..., `d' - 1) `coef'[i][j]
u^`exp'[j][0] v^`exp'[j][1] w^`exp'[j][2], i = 0, ..., `d'-1, with u,
v, w the coordinates in the reference element. The `coef' matrix (of
size `d' x `d') and the `exp' matrix (of size `d' x 3) are stored as
vectors, by row. If `dGeo' is positive, use `coefGeo' and `expGeo' to
define the interpolation of the x, y, z coordinates of the element in
terms of the u, v, w coordinates, in exactly the same way. If `d' < 0,
remove the interpolation matrices."
  (with-foreign-array (coef-ptr coef-n coef :double)
      (with-foreign-array (exp-ptr exp-n exp :double)
      (with-foreign-array (coef-geo-ptr coef-geo-n coef-geo :double)
      (with-foreign-array (exp-geo-ptr exp-geo-n exp-geo :double)
      (with-ierr (ierr)
      (gmsh/internal::%view-set-interpolation-matrices tag type d coef-ptr coef-n exp-ptr exp-n d-geo coef-geo-ptr coef-geo-n exp-geo-ptr exp-geo-n ierr)))))))

(defun add-alias (ref-tag &key (copy-options nil) (tag -1))
  "Add a post-processing view as an `alias' of the reference view with
tag `refTag'. If `copyOptions' is set, copy the options of the
reference view. If `tag' is positive use it (and remove the view with
that tag if it already exists), otherwise associate a new tag. Return
the view tag."
  (with-ierr (ierr)
      (gmsh/internal::%view-add-alias ref-tag (if copy-options 1 0) tag ierr)))

(defun combine (what how &key (remove t) (copy-options t))
  "Combine elements (if `what' == \"elements\") or steps (if `what' ==
\"steps\") of all views (`how' == \"all\"), all visible views (`how'
== \"visible\") or all views having the same name (`how' == \"name\").
Remove original views if `remove' is set."
  (with-ierr (ierr)
      (gmsh/internal::%view-combine what how (if remove 1 0) (if copy-options 1 0) ierr)))

(defun probe (tag x y z &key (step -1) (num-comp -1) (gradient nil) (distance-max 0.0) (x-elem-coord '()) (y-elem-coord '()) (z-elem-coord '()) (dim -1))
  "Probe the view `tag' for its `values' at point (`x', `y', `z'). If no
match is found, `value' is returned empty. Return only the value at
step `step' is `step' is positive. Return only values with `numComp'
if `numComp' is positive. Return the gradient of the `values' if
`gradient' is set. If `distanceMax' is zero, only return a result if
an exact match inside an element in the view is found; if
`distanceMax' is positive and an exact match is not found, return the
value at the closest node if it is closer than `distanceMax'; if
`distanceMax' is negative and an exact match is not found, always
return the value at the closest node. The distance to the match is
returned in `distance'. Return the result from the element described
by its coordinates if `xElementCoord', `yElementCoord' and
`zElementCoord' are provided. If `dim' is >= 0, return only matches
from elements of the specified dimension."
  (with-foreign-array (x-elem-coord-ptr x-elem-coord-n x-elem-coord :double)
      (with-foreign-array (y-elem-coord-ptr y-elem-coord-n y-elem-coord :double)
      (with-foreign-array (z-elem-coord-ptr z-elem-coord-n z-elem-coord :double)
      (cffi:with-foreign-objects ((values-out :pointer) (values-n-out :unsigned-long) (distance-out :double))
      (with-ierr (ierr)
      (gmsh/internal::%view-probe tag (to-double x) (to-double y) (to-double z) values-out values-n-out distance-out step num-comp (if gradient 1 0) (to-double distance-max) x-elem-coord-ptr x-elem-coord-n y-elem-coord-ptr y-elem-coord-n z-elem-coord-ptr z-elem-coord-n dim ierr))
    (values (foreign-array-to-list (cffi:mem-ref values-out :pointer) (cffi:mem-ref values-n-out :unsigned-long) :double) (cffi:mem-ref distance-out :double)))))))

(defun write (tag file-name &key (append nil))
  "Write the view to a file `fileName'. The export format is determined
by the file extension. Append to the file if `append' is set."
  (with-ierr (ierr)
      (gmsh/internal::%view-write tag file-name (if append 1 0) ierr)))

(defun set-visibility-per-window (tag value &key (window-index 0))
  "Set the global visibility of the view `tag' per window to `value',
where `windowIndex' identifies the window in the window list."
  (with-ierr (ierr)
      (gmsh/internal::%view-set-visibility-per-window tag value window-index ierr)))

(defun option-set-number (tag name value)
  "Set the numerical option `name' to value `value' for the view with tag
`tag'."
  (with-ierr (ierr)
      (gmsh/internal::%view-option-set-number tag name (to-double value) ierr)))

(defun option-get-number (tag name)
  "Get the `value' of the numerical option `name' for the view with tag
`tag'."
  (cffi:with-foreign-object (value-out :double)
      (with-ierr (ierr)
      (gmsh/internal::%view-option-get-number tag name value-out ierr))
    (cffi:mem-ref value-out :double)))

(defun option-set-string (tag name value)
  "Set the string option `name' to value `value' for the view with tag
`tag'."
  (with-ierr (ierr)
      (gmsh/internal::%view-option-set-string tag name value ierr)))

(defun option-get-string (tag name)
  "Get the `value' of the string option `name' for the view with tag
`tag'."
  (cffi:with-foreign-object (value-out :pointer)
      (with-ierr (ierr)
      (gmsh/internal::%view-option-get-string tag name value-out ierr))
    (foreign-string-result (cffi:mem-ref value-out :pointer))))

(defun option-set-color (tag name r g b &key (a 255))
  "Set the color option `name' to the RGBA value (`r', `g', `b', `a') for
the view with tag `tag', where where `r', `g', `b' and `a' should be
integers between 0 and 255."
  (with-ierr (ierr)
      (gmsh/internal::%view-option-set-color tag name r g b a ierr)))

(defun option-get-color (tag name)
  "Get the `r', `g', `b', `a' value of the color option `name' for the
view with tag `tag'."
  (cffi:with-foreign-objects ((r-out :int) (g-out :int) (b-out :int) (a-out :int))
      (with-ierr (ierr)
      (gmsh/internal::%view-option-get-color tag name r-out g-out b-out a-out ierr))
    (values (cffi:mem-ref r-out :int) (cffi:mem-ref g-out :int) (cffi:mem-ref b-out :int) (cffi:mem-ref a-out :int))))

(defun option-copy (ref-tag tag)
  "Copy the options from the view with tag `refTag' to the view with tag
`tag'."
  (with-ierr (ierr)
      (gmsh/internal::%view-option-copy ref-tag tag ierr)))

