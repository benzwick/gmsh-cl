;;;; packages.lisp — Package definitions for gmsh-cl
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(defpackage :gmsh/internal
  (:use :cl)
  (:export
   #:with-ierr
   #:to-double
   #:with-foreign-array
   #:with-pairs-array
   #:with-string-array
   #:with-vector-vector-int
   #:with-vector-vector-double
   #:with-vector-vector-size
   #:pairs-to-foreign
   #:foreign-array-to-list
   #:dim-tags-to-pairs
   #:foreign-string-result
   #:foreign-string-array-to-list
   #:foreign-vectors-to-list
   #:foreign-vector-pairs-to-list
   #:gmsh-error
   #:gmsh-error-code
   #:gmsh-error-message
   #:%free
   #:%malloc))

(defpackage :gmsh
  (:use :cl :gmsh/internal)
  (:export
   #:add
   #:add-discrete-entity
   #:add-physical-group
   #:clear
   #:draw
   #:finalize
   #:get-adjacencies
   #:get-attribute
   #:get-attribute-names
   #:get-boundary
   #:get-bounding-box
   #:get-closest-point
   #:get-color
   #:get-current
   #:get-curvature
   #:get-derivative
   #:get-dimension
   #:get-entities
   #:get-entities-for-physical-group
   #:get-entities-for-physical-name
   #:get-entities-in-bounding-box
   #:get-entity-name
   #:get-entity-properties
   #:get-entity-type
   #:get-file-name
   #:get-normal
   #:get-number-of-partitions
   #:get-parametrization
   #:get-parametrization-bounds
   #:get-parent
   #:get-partitions
   #:get-physical-groups
   #:get-physical-groups-entities
   #:get-physical-groups-for-entity
   #:get-physical-name
   #:get-principal-curvatures
   #:get-second-derivative
   #:get-type
   #:get-value
   #:get-visibility
   #:initialize
   #:is-entity-orphan
   #:is-initialized
   #:is-inside
   #:list
   #:merge
   #:open
   #:remove
   #:remove-attribute
   #:remove-entities
   #:remove-entity-name
   #:remove-physical-groups
   #:remove-physical-name
   #:reparametrize-on-surface
   #:set-attribute
   #:set-color
   #:set-coordinates
   #:set-current
   #:set-entity-name
   #:set-file-name
   #:set-physical-name
   #:set-tag
   #:set-visibility
   #:set-visibility-per-window
   #:write
  ))

(defpackage :gmsh/algorithm
  (:use :cl :gmsh/internal)
  (:nicknames :algorithm)
  (:export
   #:tetrahedralize
   #:triangulate
  ))

(defpackage :gmsh/fltk
  (:use :cl :gmsh/internal)
  (:nicknames :fltk)
  (:export
   #:awake
   #:close-tree-item
   #:finalize
   #:initialize
   #:is-available
   #:lock
   #:open-tree-item
   #:run
   #:select-elements
   #:select-entities
   #:select-views
   #:set-current-window
   #:set-status-message
   #:show-context-window
   #:split-current-window
   #:unlock
   #:update
   #:wait
  ))

(defpackage :gmsh/geo
  (:use :cl :gmsh/internal)
  (:nicknames :geo)
  (:export
   #:b-spline
   #:bezier
   #:circle-arc
   #:compound-b-spline
   #:compound-spline
   #:copy
   #:curve-loop
   #:curve-loops
   #:dilate
   #:ellipse-arc
   #:extrude
   #:extrude-boundary-layer
   #:geometry
   #:get-max-tag
   #:line
   #:mesh-set-algorithm
   #:mesh-set-recombine
   #:mesh-set-reverse
   #:mesh-set-size
   #:mesh-set-size-from-boundary
   #:mesh-set-smoothing
   #:mesh-set-transfinite-curve
   #:mesh-set-transfinite-surface
   #:mesh-set-transfinite-volume
   #:mirror
   #:physical-group
   #:plane-surface
   #:point
   #:point-on-geometry
   #:polyline
   #:remove
   #:remove-all-duplicates
   #:remove-physical-groups
   #:revolve
   #:rotate
   #:set-max-tag
   #:spline
   #:split-curve
   #:surface-filling
   #:surface-loop
   #:symmetrize
   #:synchronize
   #:translate
   #:twist
   #:volume
  ))

(defpackage :gmsh/logger
  (:use :cl :gmsh/internal)
  (:nicknames :logger)
  (:export
   #:get
   #:get-cpu-time
   #:get-last-error
   #:get-memory
   #:get-total-memory
   #:get-wall-time
   #:start
   #:stop
   #:write
  ))

(defpackage :gmsh/mesh
  (:use :cl :gmsh/internal)
  (:nicknames :mesh)
  (:export
   #:add-edges
   #:add-elements
   #:add-elements-by-type
   #:add-faces
   #:add-homology-request
   #:add-nodes
   #:affine-transform
   #:classify-surfaces
   #:clear
   #:clear-homology-requests
   #:compute-cross-field
   #:compute-homology
   #:compute-renumbering
   #:create-edges
   #:create-faces
   #:create-geometry
   #:create-overlaps
   #:create-topology
   #:embed
   #:field-add
   #:field-get-number
   #:field-get-numbers
   #:field-get-string
   #:field-get-type
   #:field-list
   #:field-remove
   #:field-set-as-background-mesh
   #:field-set-as-boundary-layer
   #:field-set-number
   #:field-set-numbers
   #:field-set-string
   #:generate
   #:get-all-edges
   #:get-all-faces
   #:get-barycenters
   #:get-basis-functions
   #:get-basis-functions-orientation
   #:get-basis-functions-orientation-for-element
   #:get-boundary-overlap-parent
   #:get-duplicate-nodes
   #:get-edges
   #:get-element
   #:get-element-by-coordinates
   #:get-element-edge-nodes
   #:get-element-face-nodes
   #:get-element-properties
   #:get-element-qualities
   #:get-element-type
   #:get-element-types
   #:get-elements
   #:get-elements-by-coordinates
   #:get-elements-by-type
   #:get-embedded
   #:get-faces
   #:get-ghost-elements
   #:get-integration-points
   #:get-jacobian
   #:get-jacobians
   #:get-keys
   #:get-keys-for-element
   #:get-keys-information
   #:get-last-entity-error
   #:get-last-node-error
   #:get-local-coordinates-in-element
   #:get-max-element-tag
   #:get-max-node-tag
   #:get-node
   #:get-nodes
   #:get-nodes-by-element-type
   #:get-nodes-for-physical-group
   #:get-number-of-keys
   #:get-number-of-orientations
   #:get-overlap-boundary
   #:get-partition-entities
   #:get-periodic
   #:get-periodic-keys
   #:get-periodic-nodes
   #:get-sizes
   #:get-visibility
   #:import-stl
   #:optimize
   #:partition
   #:preallocate-barycenters
   #:preallocate-basis-functions-orientation
   #:preallocate-elements-by-type
   #:preallocate-jacobians
   #:rebuild-element-cache
   #:rebuild-node-cache
   #:reclassify-nodes
   #:recombine
   #:refine
   #:relocate-nodes
   #:remove-constraints
   #:remove-duplicate-elements
   #:remove-duplicate-nodes
   #:remove-elements
   #:remove-embedded
   #:remove-size-callback
   #:renumber-elements
   #:renumber-nodes
   #:reorder-elements
   #:reverse
   #:reverse-elements
   #:set-algorithm
   #:set-compound
   #:set-node
   #:set-order
   #:set-outward-orientation
   #:set-periodic
   #:set-recombine
   #:set-reverse
   #:set-size
   #:set-size-at-parametric-points
   #:set-size-callback
   #:set-size-from-boundary
   #:set-smoothing
   #:set-transfinite-automatic
   #:set-transfinite-curve
   #:set-transfinite-surface
   #:set-transfinite-volume
   #:set-visibility
   #:split-quadrangles
   #:unpartition
  ))

(defpackage :gmsh/occ
  (:use :cl :gmsh/internal)
  (:nicknames :occ)
  (:export
   #:affine-transform
   #:b-spline
   #:b-spline-filling
   #:b-spline-surface
   #:bezier
   #:bezier-filling
   #:bezier-surface
   #:box
   #:chamfer
   #:chamfer2-d
   #:circle
   #:circle-arc
   #:cone
   #:convert-to-nurbs
   #:copy
   #:curve-loop
   #:cut
   #:cylinder
   #:defeature
   #:dilate
   #:disk
   #:ellipse
   #:ellipse-arc
   #:extrude
   #:fillet
   #:fillet2-d
   #:fragment
   #:fuse
   #:get-bounding-box
   #:get-center-of-mass
   #:get-closest-entities
   #:get-curve-loops
   #:get-distance
   #:get-entities
   #:get-entities-in-bounding-box
   #:get-mass
   #:get-matrix-of-inertia
   #:get-max-tag
   #:get-surface-loops
   #:heal-shapes
   #:import-shapes
   #:import-shapes-native-pointer
   #:intersect
   #:line
   #:mesh-set-size
   #:mirror
   #:offset-curve
   #:pipe
   #:plane-surface
   #:point
   #:rectangle
   #:remove
   #:remove-all-duplicates
   #:revolve
   #:rotate
   #:set-max-tag
   #:sphere
   #:spline
   #:surface-filling
   #:surface-loop
   #:symmetrize
   #:synchronize
   #:thick-solid
   #:thru-sections
   #:torus
   #:translate
   #:trimmed-surface
   #:volume
   #:wedge
   #:wire
  ))

(defpackage :gmsh/onelab
  (:use :cl :gmsh/internal)
  (:nicknames :onelab)
  (:export
   #:clear
   #:get
   #:get-changed
   #:get-names
   #:get-number
   #:get-string
   #:run
   #:set
   #:set-changed
   #:set-number
   #:set-string
  ))

(defpackage :gmsh/option
  (:use :cl :gmsh/internal)
  (:nicknames :opt)
  (:export
   #:get-color
   #:get-number
   #:get-string
   #:restore-defaults
   #:set-color
   #:set-number
   #:set-string
  ))

(defpackage :gmsh/parser
  (:use :cl :gmsh/internal)
  (:nicknames :parser)
  (:export
   #:clear
   #:get-names
   #:get-number
   #:get-string
   #:parse
   #:set-number
   #:set-string
  ))

(defpackage :gmsh/plugin
  (:use :cl :gmsh/internal)
  (:nicknames :plugin)
  (:export
   #:run
   #:set-number
   #:set-string
  ))

(defpackage :gmsh/view
  (:use :cl :gmsh/internal)
  (:nicknames :view)
  (:export
   #:add
   #:add-alias
   #:add-homogeneous-model-data
   #:add-list-data
   #:add-list-data-string
   #:add-model-data
   #:combine
   #:get-homogeneous-model-data
   #:get-index
   #:get-list-data
   #:get-list-data-strings
   #:get-model-data
   #:get-tags
   #:option-copy
   #:option-get-color
   #:option-get-number
   #:option-get-string
   #:option-set-color
   #:option-set-number
   #:option-set-string
   #:probe
   #:remove
   #:set-interpolation-matrices
   #:set-visibility-per-window
   #:write
  ))

