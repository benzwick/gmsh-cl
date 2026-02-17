;;;; bindings.lisp — CFFI defcfun for all gmsh C functions
;;;;
;;;; *** DO NOT EDIT — regenerate with: python generate.py ***

(in-package :gmsh/internal)

(cffi:defcfun ("gmshInitialize" %initialize) :void
  (argc :int)
  (argv :pointer)
  (read-config-files :int)
  (run :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshIsInitialized" %is-initialized) :int
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFinalize" %finalize) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOpen" %open) :void
  (file-name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshMerge" %merge) :void
  (file-name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshWrite" %write) :void
  (file-name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshClear" %clear) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOptionSetNumber" %option-set-number) :void
  (name :string)
  (value :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOptionGetNumber" %option-get-number) :void
  (name :string)
  (value (:pointer :double))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOptionSetString" %option-set-string) :void
  (name :string)
  (value :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOptionGetString" %option-get-string) :void
  (name :string)
  (value (:pointer :pointer))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOptionSetColor" %option-set-color) :void
  (name :string)
  (r :int)
  (g :int)
  (b :int)
  (a :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOptionGetColor" %option-get-color) :void
  (name :string)
  (r (:pointer :int))
  (g (:pointer :int))
  (b (:pointer :int))
  (a (:pointer :int))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOptionRestoreDefaults" %option-restore-defaults) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelAdd" %model-add) :void
  (name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelRemove" %model-remove) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelList" %model-list) :void
  (names (:pointer :pointer))
  (names-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetCurrent" %model-get-current) :void
  (name (:pointer :pointer))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelSetCurrent" %model-set-current) :void
  (name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetFileName" %model-get-file-name) :void
  (file-name (:pointer :pointer))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelSetFileName" %model-set-file-name) :void
  (file-name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetEntities" %model-get-entities) :void
  (dim-tags (:pointer :pointer))
  (dim-tags-n (:pointer :unsigned-long))
  (dim :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelSetEntityName" %model-set-entity-name) :void
  (dim :int)
  (tag :int)
  (name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetEntityName" %model-get-entity-name) :void
  (dim :int)
  (tag :int)
  (name (:pointer :pointer))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelRemoveEntityName" %model-remove-entity-name) :void
  (name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetPhysicalGroups" %model-get-physical-groups) :void
  (dim-tags (:pointer :pointer))
  (dim-tags-n (:pointer :unsigned-long))
  (dim :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetPhysicalGroupsEntities" %model-get-physical-groups-entities) :void
  (dim-tags (:pointer :pointer))
  (dim-tags-n (:pointer :unsigned-long))
  (entities (:pointer :pointer))
  (entities-n (:pointer :pointer))
  (entities-nn (:pointer :unsigned-long))
  (dim :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetEntitiesForPhysicalGroup" %model-get-entities-for-physical-group) :void
  (dim :int)
  (tag :int)
  (tags (:pointer :pointer))
  (tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetEntitiesForPhysicalName" %model-get-entities-for-physical-name) :void
  (name :string)
  (dim-tags (:pointer :pointer))
  (dim-tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetPhysicalGroupsForEntity" %model-get-physical-groups-for-entity) :void
  (dim :int)
  (tag :int)
  (physical-tags (:pointer :pointer))
  (physical-tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelAddPhysicalGroup" %model-add-physical-group) :int
  (dim :int)
  (tags :pointer)
  (tags-n :unsigned-long)
  (tag :int)
  (name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelRemovePhysicalGroups" %model-remove-physical-groups) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelSetPhysicalName" %model-set-physical-name) :void
  (dim :int)
  (tag :int)
  (name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetPhysicalName" %model-get-physical-name) :void
  (dim :int)
  (tag :int)
  (name (:pointer :pointer))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelRemovePhysicalName" %model-remove-physical-name) :void
  (name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelSetTag" %model-set-tag) :void
  (dim :int)
  (tag :int)
  (new-tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetBoundary" %model-get-boundary) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (combined :int)
  (oriented :int)
  (recursive :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetAdjacencies" %model-get-adjacencies) :void
  (dim :int)
  (tag :int)
  (upward (:pointer :pointer))
  (upward-n (:pointer :unsigned-long))
  (downward (:pointer :pointer))
  (downward-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelIsEntityOrphan" %model-is-entity-orphan) :int
  (dim :int)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetEntitiesInBoundingBox" %model-get-entities-in-bounding-box) :void
  (xmin :double)
  (ymin :double)
  (zmin :double)
  (xmax :double)
  (ymax :double)
  (zmax :double)
  (dim-tags (:pointer :pointer))
  (dim-tags-n (:pointer :unsigned-long))
  (dim :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetBoundingBox" %model-get-bounding-box) :void
  (dim :int)
  (tag :int)
  (xmin (:pointer :double))
  (ymin (:pointer :double))
  (zmin (:pointer :double))
  (xmax (:pointer :double))
  (ymax (:pointer :double))
  (zmax (:pointer :double))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetDimension" %model-get-dimension) :int
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelAddDiscreteEntity" %model-add-discrete-entity) :int
  (dim :int)
  (tag :int)
  (boundary :pointer)
  (boundary-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelRemoveEntities" %model-remove-entities) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (recursive :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetEntityType" %model-get-entity-type) :void
  (dim :int)
  (tag :int)
  (entity-type (:pointer :pointer))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetType" %model-get-type) :void
  (dim :int)
  (tag :int)
  (entity-type (:pointer :pointer))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetEntityProperties" %model-get-entity-properties) :void
  (dim :int)
  (tag :int)
  (integers (:pointer :pointer))
  (integers-n (:pointer :unsigned-long))
  (reals (:pointer :pointer))
  (reals-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetParent" %model-get-parent) :void
  (dim :int)
  (tag :int)
  (parent-dim (:pointer :int))
  (parent-tag (:pointer :int))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetNumberOfPartitions" %model-get-number-of-partitions) :int
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetPartitions" %model-get-partitions) :void
  (dim :int)
  (tag :int)
  (partitions (:pointer :pointer))
  (partitions-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetValue" %model-get-value) :void
  (dim :int)
  (tag :int)
  (parametric-coord :pointer)
  (parametric-coord-n :unsigned-long)
  (coord (:pointer :pointer))
  (coord-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetDerivative" %model-get-derivative) :void
  (dim :int)
  (tag :int)
  (parametric-coord :pointer)
  (parametric-coord-n :unsigned-long)
  (derivatives (:pointer :pointer))
  (derivatives-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetSecondDerivative" %model-get-second-derivative) :void
  (dim :int)
  (tag :int)
  (parametric-coord :pointer)
  (parametric-coord-n :unsigned-long)
  (derivatives (:pointer :pointer))
  (derivatives-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetCurvature" %model-get-curvature) :void
  (dim :int)
  (tag :int)
  (parametric-coord :pointer)
  (parametric-coord-n :unsigned-long)
  (curvatures (:pointer :pointer))
  (curvatures-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetPrincipalCurvatures" %model-get-principal-curvatures) :void
  (tag :int)
  (parametric-coord :pointer)
  (parametric-coord-n :unsigned-long)
  (curvature-max (:pointer :pointer))
  (curvature-max-n (:pointer :unsigned-long))
  (curvature-min (:pointer :pointer))
  (curvature-min-n (:pointer :unsigned-long))
  (direction-max (:pointer :pointer))
  (direction-max-n (:pointer :unsigned-long))
  (direction-min (:pointer :pointer))
  (direction-min-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetNormal" %model-get-normal) :void
  (tag :int)
  (parametric-coord :pointer)
  (parametric-coord-n :unsigned-long)
  (normals (:pointer :pointer))
  (normals-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetParametrization" %model-get-parametrization) :void
  (dim :int)
  (tag :int)
  (coord :pointer)
  (coord-n :unsigned-long)
  (parametric-coord (:pointer :pointer))
  (parametric-coord-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetParametrizationBounds" %model-get-parametrization-bounds) :void
  (dim :int)
  (tag :int)
  (min (:pointer :pointer))
  (min-n (:pointer :unsigned-long))
  (max (:pointer :pointer))
  (max-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelIsInside" %model-is-inside) :int
  (dim :int)
  (tag :int)
  (coord :pointer)
  (coord-n :unsigned-long)
  (parametric :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetClosestPoint" %model-get-closest-point) :void
  (dim :int)
  (tag :int)
  (coord :pointer)
  (coord-n :unsigned-long)
  (closest-coord (:pointer :pointer))
  (closest-coord-n (:pointer :unsigned-long))
  (parametric-coord (:pointer :pointer))
  (parametric-coord-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelReparametrizeOnSurface" %model-reparametrize-on-surface) :void
  (dim :int)
  (tag :int)
  (parametric-coord :pointer)
  (parametric-coord-n :unsigned-long)
  (surface-tag :int)
  (surface-parametric-coord (:pointer :pointer))
  (surface-parametric-coord-n (:pointer :unsigned-long))
  (which :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelSetVisibility" %model-set-visibility) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (value :int)
  (recursive :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetVisibility" %model-get-visibility) :void
  (dim :int)
  (tag :int)
  (value (:pointer :int))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelSetVisibilityPerWindow" %model-set-visibility-per-window) :void
  (value :int)
  (window-index :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelSetColor" %model-set-color) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (r :int)
  (g :int)
  (b :int)
  (a :int)
  (recursive :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetColor" %model-get-color) :void
  (dim :int)
  (tag :int)
  (r (:pointer :int))
  (g (:pointer :int))
  (b (:pointer :int))
  (a (:pointer :int))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelSetCoordinates" %model-set-coordinates) :void
  (tag :int)
  (x :double)
  (y :double)
  (z :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelSetAttribute" %model-set-attribute) :void
  (name :string)
  (values :pointer)
  (values-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetAttribute" %model-get-attribute) :void
  (name :string)
  (values (:pointer :pointer))
  (values-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGetAttributeNames" %model-get-attribute-names) :void
  (names (:pointer :pointer))
  (names-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelRemoveAttribute" %model-remove-attribute) :void
  (name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGenerate" %model-mesh-generate) :void
  (dim :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshPartition" %model-mesh-partition) :void
  (num-part :int)
  (element-tags :pointer)
  (element-tags-n :unsigned-long)
  (partitions :pointer)
  (partitions-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshCreateOverlaps" %model-mesh-create-overlaps) :void
  (layers :int)
  (create-boundaries :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetPartitionEntities" %model-mesh-get-partition-entities) :void
  (dim :int)
  (tag :int)
  (partition :int)
  (entity-tags (:pointer :pointer))
  (entity-tags-n (:pointer :unsigned-long))
  (overlap-entities (:pointer :pointer))
  (overlap-entities-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetOverlapBoundary" %model-mesh-get-overlap-boundary) :void
  (dim :int)
  (tag :int)
  (partition :int)
  (entity-tags (:pointer :pointer))
  (entity-tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetBoundaryOverlapParent" %model-mesh-get-boundary-overlap-parent) :void
  (dim :int)
  (tag :int)
  (parent-tag (:pointer :int))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshUnpartition" %model-mesh-unpartition) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshOptimize" %model-mesh-optimize) :void
  (method :string)
  (force :int)
  (niter :int)
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshRecombine" %model-mesh-recombine) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshRefine" %model-mesh-refine) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetOrder" %model-mesh-set-order) :void
  (order :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetLastEntityError" %model-mesh-get-last-entity-error) :void
  (dim-tags (:pointer :pointer))
  (dim-tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetLastNodeError" %model-mesh-get-last-node-error) :void
  (node-tags (:pointer :pointer))
  (node-tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshClear" %model-mesh-clear) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshRemoveElements" %model-mesh-remove-elements) :void
  (dim :int)
  (tag :int)
  (element-tags :pointer)
  (element-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshReverse" %model-mesh-reverse) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshReverseElements" %model-mesh-reverse-elements) :void
  (element-tags :pointer)
  (element-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshAffineTransform" %model-mesh-affine-transform) :void
  (affine-transform :pointer)
  (affine-transform-n :unsigned-long)
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetNodes" %model-mesh-get-nodes) :void
  (node-tags (:pointer :pointer))
  (node-tags-n (:pointer :unsigned-long))
  (coord (:pointer :pointer))
  (coord-n (:pointer :unsigned-long))
  (parametric-coord (:pointer :pointer))
  (parametric-coord-n (:pointer :unsigned-long))
  (dim :int)
  (tag :int)
  (include-boundary :int)
  (return-parametric-coord :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetNodesByElementType" %model-mesh-get-nodes-by-element-type) :void
  (element-type :int)
  (node-tags (:pointer :pointer))
  (node-tags-n (:pointer :unsigned-long))
  (coord (:pointer :pointer))
  (coord-n (:pointer :unsigned-long))
  (parametric-coord (:pointer :pointer))
  (parametric-coord-n (:pointer :unsigned-long))
  (tag :int)
  (return-parametric-coord :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetNode" %model-mesh-get-node) :void
  (node-tag :unsigned-long)
  (coord (:pointer :pointer))
  (coord-n (:pointer :unsigned-long))
  (parametric-coord (:pointer :pointer))
  (parametric-coord-n (:pointer :unsigned-long))
  (dim (:pointer :int))
  (tag (:pointer :int))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetNode" %model-mesh-set-node) :void
  (node-tag :unsigned-long)
  (coord :pointer)
  (coord-n :unsigned-long)
  (parametric-coord :pointer)
  (parametric-coord-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshRebuildNodeCache" %model-mesh-rebuild-node-cache) :void
  (only-if-necessary :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshRebuildElementCache" %model-mesh-rebuild-element-cache) :void
  (only-if-necessary :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetNodesForPhysicalGroup" %model-mesh-get-nodes-for-physical-group) :void
  (dim :int)
  (tag :int)
  (node-tags (:pointer :pointer))
  (node-tags-n (:pointer :unsigned-long))
  (coord (:pointer :pointer))
  (coord-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetMaxNodeTag" %model-mesh-get-max-node-tag) :void
  (max-tag (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshAddNodes" %model-mesh-add-nodes) :void
  (dim :int)
  (tag :int)
  (node-tags :pointer)
  (node-tags-n :unsigned-long)
  (coord :pointer)
  (coord-n :unsigned-long)
  (parametric-coord :pointer)
  (parametric-coord-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshReclassifyNodes" %model-mesh-reclassify-nodes) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshRelocateNodes" %model-mesh-relocate-nodes) :void
  (dim :int)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetElements" %model-mesh-get-elements) :void
  (element-types (:pointer :pointer))
  (element-types-n (:pointer :unsigned-long))
  (element-tags (:pointer :pointer))
  (element-tags-n (:pointer :pointer))
  (element-tags-nn (:pointer :unsigned-long))
  (node-tags (:pointer :pointer))
  (node-tags-n (:pointer :pointer))
  (node-tags-nn (:pointer :unsigned-long))
  (dim :int)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetElement" %model-mesh-get-element) :void
  (element-tag :unsigned-long)
  (element-type (:pointer :int))
  (node-tags (:pointer :pointer))
  (node-tags-n (:pointer :unsigned-long))
  (dim (:pointer :int))
  (tag (:pointer :int))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetElementByCoordinates" %model-mesh-get-element-by-coordinates) :void
  (x :double)
  (y :double)
  (z :double)
  (element-tag (:pointer :unsigned-long))
  (element-type (:pointer :int))
  (node-tags (:pointer :pointer))
  (node-tags-n (:pointer :unsigned-long))
  (u (:pointer :double))
  (v (:pointer :double))
  (w (:pointer :double))
  (dim :int)
  (strict :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetElementsByCoordinates" %model-mesh-get-elements-by-coordinates) :void
  (x :double)
  (y :double)
  (z :double)
  (element-tags (:pointer :pointer))
  (element-tags-n (:pointer :unsigned-long))
  (dim :int)
  (strict :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetLocalCoordinatesInElement" %model-mesh-get-local-coordinates-in-element) :void
  (element-tag :unsigned-long)
  (x :double)
  (y :double)
  (z :double)
  (u (:pointer :double))
  (v (:pointer :double))
  (w (:pointer :double))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetElementTypes" %model-mesh-get-element-types) :void
  (element-types (:pointer :pointer))
  (element-types-n (:pointer :unsigned-long))
  (dim :int)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetElementType" %model-mesh-get-element-type) :int
  (family-name :string)
  (order :int)
  (serendip :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetElementProperties" %model-mesh-get-element-properties) :void
  (element-type :int)
  (element-name (:pointer :pointer))
  (dim (:pointer :int))
  (order (:pointer :int))
  (num-nodes (:pointer :int))
  (local-node-coord (:pointer :pointer))
  (local-node-coord-n (:pointer :unsigned-long))
  (num-primary-nodes (:pointer :int))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetElementsByType" %model-mesh-get-elements-by-type) :void
  (element-type :int)
  (element-tags (:pointer :pointer))
  (element-tags-n (:pointer :unsigned-long))
  (node-tags (:pointer :pointer))
  (node-tags-n (:pointer :unsigned-long))
  (tag :int)
  (task :unsigned-long)
  (num-tasks :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetMaxElementTag" %model-mesh-get-max-element-tag) :void
  (max-tag (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshPreallocateElementsByType" %model-mesh-preallocate-elements-by-type) :void
  (element-type :int)
  (element-tag :int)
  (node-tag :int)
  (element-tags (:pointer :pointer))
  (element-tags-n (:pointer :unsigned-long))
  (node-tags (:pointer :pointer))
  (node-tags-n (:pointer :unsigned-long))
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetElementQualities" %model-mesh-get-element-qualities) :void
  (element-tags :pointer)
  (element-tags-n :unsigned-long)
  (elements-quality (:pointer :pointer))
  (elements-quality-n (:pointer :unsigned-long))
  (quality-name :string)
  (task :unsigned-long)
  (num-tasks :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshAddElements" %model-mesh-add-elements) :void
  (dim :int)
  (tag :int)
  (element-types :pointer)
  (element-types-n :unsigned-long)
  (element-tags :pointer)
  (element-tags-n :pointer)
  (element-tags-nn :unsigned-long)
  (node-tags :pointer)
  (node-tags-n :pointer)
  (node-tags-nn :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshAddElementsByType" %model-mesh-add-elements-by-type) :void
  (tag :int)
  (element-type :int)
  (element-tags :pointer)
  (element-tags-n :unsigned-long)
  (node-tags :pointer)
  (node-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetIntegrationPoints" %model-mesh-get-integration-points) :void
  (element-type :int)
  (integration-type :string)
  (local-coord (:pointer :pointer))
  (local-coord-n (:pointer :unsigned-long))
  (weights (:pointer :pointer))
  (weights-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetJacobians" %model-mesh-get-jacobians) :void
  (element-type :int)
  (local-coord :pointer)
  (local-coord-n :unsigned-long)
  (jacobians (:pointer :pointer))
  (jacobians-n (:pointer :unsigned-long))
  (determinants (:pointer :pointer))
  (determinants-n (:pointer :unsigned-long))
  (coord (:pointer :pointer))
  (coord-n (:pointer :unsigned-long))
  (tag :int)
  (task :unsigned-long)
  (num-tasks :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshPreallocateJacobians" %model-mesh-preallocate-jacobians) :void
  (element-type :int)
  (num-evaluation-points :int)
  (allocate-jacobians :int)
  (allocate-determinants :int)
  (allocate-coord :int)
  (jacobians (:pointer :pointer))
  (jacobians-n (:pointer :unsigned-long))
  (determinants (:pointer :pointer))
  (determinants-n (:pointer :unsigned-long))
  (coord (:pointer :pointer))
  (coord-n (:pointer :unsigned-long))
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetJacobian" %model-mesh-get-jacobian) :void
  (element-tag :unsigned-long)
  (local-coord :pointer)
  (local-coord-n :unsigned-long)
  (jacobians (:pointer :pointer))
  (jacobians-n (:pointer :unsigned-long))
  (determinants (:pointer :pointer))
  (determinants-n (:pointer :unsigned-long))
  (coord (:pointer :pointer))
  (coord-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetBasisFunctions" %model-mesh-get-basis-functions) :void
  (element-type :int)
  (local-coord :pointer)
  (local-coord-n :unsigned-long)
  (function-space-type :string)
  (num-components (:pointer :int))
  (basis-functions (:pointer :pointer))
  (basis-functions-n (:pointer :unsigned-long))
  (num-orientations (:pointer :int))
  (wanted-orientations :pointer)
  (wanted-orientations-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetBasisFunctionsOrientation" %model-mesh-get-basis-functions-orientation) :void
  (element-type :int)
  (function-space-type :string)
  (basis-functions-orientation (:pointer :pointer))
  (basis-functions-orientation-n (:pointer :unsigned-long))
  (tag :int)
  (task :unsigned-long)
  (num-tasks :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetBasisFunctionsOrientationForElement" %model-mesh-get-basis-functions-orientation-for-element) :void
  (element-tag :unsigned-long)
  (function-space-type :string)
  (basis-functions-orientation (:pointer :int))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetNumberOfOrientations" %model-mesh-get-number-of-orientations) :int
  (element-type :int)
  (function-space-type :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshPreallocateBasisFunctionsOrientation" %model-mesh-preallocate-basis-functions-orientation) :void
  (element-type :int)
  (basis-functions-orientation (:pointer :pointer))
  (basis-functions-orientation-n (:pointer :unsigned-long))
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetEdges" %model-mesh-get-edges) :void
  (node-tags :pointer)
  (node-tags-n :unsigned-long)
  (edge-tags (:pointer :pointer))
  (edge-tags-n (:pointer :unsigned-long))
  (edge-orientations (:pointer :pointer))
  (edge-orientations-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetFaces" %model-mesh-get-faces) :void
  (face-type :int)
  (node-tags :pointer)
  (node-tags-n :unsigned-long)
  (face-tags (:pointer :pointer))
  (face-tags-n (:pointer :unsigned-long))
  (face-orientations (:pointer :pointer))
  (face-orientations-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshCreateEdges" %model-mesh-create-edges) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshCreateFaces" %model-mesh-create-faces) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetAllEdges" %model-mesh-get-all-edges) :void
  (edge-tags (:pointer :pointer))
  (edge-tags-n (:pointer :unsigned-long))
  (edge-nodes (:pointer :pointer))
  (edge-nodes-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetAllFaces" %model-mesh-get-all-faces) :void
  (face-type :int)
  (face-tags (:pointer :pointer))
  (face-tags-n (:pointer :unsigned-long))
  (face-nodes (:pointer :pointer))
  (face-nodes-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshAddEdges" %model-mesh-add-edges) :void
  (edge-tags :pointer)
  (edge-tags-n :unsigned-long)
  (edge-nodes :pointer)
  (edge-nodes-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshAddFaces" %model-mesh-add-faces) :void
  (face-type :int)
  (face-tags :pointer)
  (face-tags-n :unsigned-long)
  (face-nodes :pointer)
  (face-nodes-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetKeys" %model-mesh-get-keys) :void
  (element-type :int)
  (function-space-type :string)
  (type-keys (:pointer :pointer))
  (type-keys-n (:pointer :unsigned-long))
  (entity-keys (:pointer :pointer))
  (entity-keys-n (:pointer :unsigned-long))
  (coord (:pointer :pointer))
  (coord-n (:pointer :unsigned-long))
  (tag :int)
  (return-coord :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetKeysForElement" %model-mesh-get-keys-for-element) :void
  (element-tag :unsigned-long)
  (function-space-type :string)
  (type-keys (:pointer :pointer))
  (type-keys-n (:pointer :unsigned-long))
  (entity-keys (:pointer :pointer))
  (entity-keys-n (:pointer :unsigned-long))
  (coord (:pointer :pointer))
  (coord-n (:pointer :unsigned-long))
  (return-coord :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetNumberOfKeys" %model-mesh-get-number-of-keys) :int
  (element-type :int)
  (function-space-type :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetKeysInformation" %model-mesh-get-keys-information) :void
  (type-keys :pointer)
  (type-keys-n :unsigned-long)
  (entity-keys :pointer)
  (entity-keys-n :unsigned-long)
  (element-type :int)
  (function-space-type :string)
  (info-keys (:pointer :pointer))
  (info-keys-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetBarycenters" %model-mesh-get-barycenters) :void
  (element-type :int)
  (tag :int)
  (fast :int)
  (primary :int)
  (barycenters (:pointer :pointer))
  (barycenters-n (:pointer :unsigned-long))
  (task :unsigned-long)
  (num-tasks :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshPreallocateBarycenters" %model-mesh-preallocate-barycenters) :void
  (element-type :int)
  (barycenters (:pointer :pointer))
  (barycenters-n (:pointer :unsigned-long))
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetElementEdgeNodes" %model-mesh-get-element-edge-nodes) :void
  (element-type :int)
  (node-tags (:pointer :pointer))
  (node-tags-n (:pointer :unsigned-long))
  (tag :int)
  (primary :int)
  (task :unsigned-long)
  (num-tasks :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetElementFaceNodes" %model-mesh-get-element-face-nodes) :void
  (element-type :int)
  (face-type :int)
  (node-tags (:pointer :pointer))
  (node-tags-n (:pointer :unsigned-long))
  (tag :int)
  (primary :int)
  (task :unsigned-long)
  (num-tasks :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetGhostElements" %model-mesh-get-ghost-elements) :void
  (dim :int)
  (tag :int)
  (element-tags (:pointer :pointer))
  (element-tags-n (:pointer :unsigned-long))
  (partitions (:pointer :pointer))
  (partitions-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetSize" %model-mesh-set-size) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (size :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetSizes" %model-mesh-get-sizes) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (sizes (:pointer :pointer))
  (sizes-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetSizeAtParametricPoints" %model-mesh-set-size-at-parametric-points) :void
  (dim :int)
  (tag :int)
  (parametric-coord :pointer)
  (parametric-coord-n :unsigned-long)
  (sizes :pointer)
  (sizes-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetSizeCallback" %model-mesh-set-size-callback) :void
  (callback :pointer)
  (callback-data :pointer)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshRemoveSizeCallback" %model-mesh-remove-size-callback) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetTransfiniteCurve" %model-mesh-set-transfinite-curve) :void
  (tag :int)
  (num-nodes :int)
  (mesh-type :string)
  (coef :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetTransfiniteSurface" %model-mesh-set-transfinite-surface) :void
  (tag :int)
  (arrangement :string)
  (corner-tags :pointer)
  (corner-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetTransfiniteVolume" %model-mesh-set-transfinite-volume) :void
  (tag :int)
  (corner-tags :pointer)
  (corner-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetTransfiniteAutomatic" %model-mesh-set-transfinite-automatic) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (corner-angle :double)
  (recombine :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetRecombine" %model-mesh-set-recombine) :void
  (dim :int)
  (tag :int)
  (angle :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetSmoothing" %model-mesh-set-smoothing) :void
  (dim :int)
  (tag :int)
  (val :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetReverse" %model-mesh-set-reverse) :void
  (dim :int)
  (tag :int)
  (val :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetAlgorithm" %model-mesh-set-algorithm) :void
  (dim :int)
  (tag :int)
  (val :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetSizeFromBoundary" %model-mesh-set-size-from-boundary) :void
  (dim :int)
  (tag :int)
  (val :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetCompound" %model-mesh-set-compound) :void
  (dim :int)
  (tags :pointer)
  (tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetOutwardOrientation" %model-mesh-set-outward-orientation) :void
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshRemoveConstraints" %model-mesh-remove-constraints) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshEmbed" %model-mesh-embed) :void
  (dim :int)
  (tags :pointer)
  (tags-n :unsigned-long)
  (in-dim :int)
  (in-tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshRemoveEmbedded" %model-mesh-remove-embedded) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (dim :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetEmbedded" %model-mesh-get-embedded) :void
  (dim :int)
  (tag :int)
  (dim-tags (:pointer :pointer))
  (dim-tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshReorderElements" %model-mesh-reorder-elements) :void
  (element-type :int)
  (tag :int)
  (ordering :pointer)
  (ordering-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshComputeRenumbering" %model-mesh-compute-renumbering) :void
  (old-tags (:pointer :pointer))
  (old-tags-n (:pointer :unsigned-long))
  (new-tags (:pointer :pointer))
  (new-tags-n (:pointer :unsigned-long))
  (method :string)
  (element-tags :pointer)
  (element-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshRenumberNodes" %model-mesh-renumber-nodes) :void
  (old-tags :pointer)
  (old-tags-n :unsigned-long)
  (new-tags :pointer)
  (new-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshRenumberElements" %model-mesh-renumber-elements) :void
  (old-tags :pointer)
  (old-tags-n :unsigned-long)
  (new-tags :pointer)
  (new-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetPeriodic" %model-mesh-set-periodic) :void
  (dim :int)
  (tags :pointer)
  (tags-n :unsigned-long)
  (tags-master :pointer)
  (tags-master-n :unsigned-long)
  (affine-transform :pointer)
  (affine-transform-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetPeriodic" %model-mesh-get-periodic) :void
  (dim :int)
  (tags :pointer)
  (tags-n :unsigned-long)
  (tag-master (:pointer :pointer))
  (tag-master-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetPeriodicNodes" %model-mesh-get-periodic-nodes) :void
  (dim :int)
  (tag :int)
  (tag-master (:pointer :int))
  (node-tags (:pointer :pointer))
  (node-tags-n (:pointer :unsigned-long))
  (node-tags-master (:pointer :pointer))
  (node-tags-master-n (:pointer :unsigned-long))
  (affine-transform (:pointer :pointer))
  (affine-transform-n (:pointer :unsigned-long))
  (include-high-order-nodes :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetPeriodicKeys" %model-mesh-get-periodic-keys) :void
  (element-type :int)
  (function-space-type :string)
  (tag :int)
  (tag-master (:pointer :int))
  (type-keys (:pointer :pointer))
  (type-keys-n (:pointer :unsigned-long))
  (type-keys-master (:pointer :pointer))
  (type-keys-master-n (:pointer :unsigned-long))
  (entity-keys (:pointer :pointer))
  (entity-keys-n (:pointer :unsigned-long))
  (entity-keys-master (:pointer :pointer))
  (entity-keys-master-n (:pointer :unsigned-long))
  (coord (:pointer :pointer))
  (coord-n (:pointer :unsigned-long))
  (coord-master (:pointer :pointer))
  (coord-master-n (:pointer :unsigned-long))
  (return-coord :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshImportStl" %model-mesh-import-stl) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetDuplicateNodes" %model-mesh-get-duplicate-nodes) :void
  (tags (:pointer :pointer))
  (tags-n (:pointer :unsigned-long))
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshRemoveDuplicateNodes" %model-mesh-remove-duplicate-nodes) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshRemoveDuplicateElements" %model-mesh-remove-duplicate-elements) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSplitQuadrangles" %model-mesh-split-quadrangles) :void
  (quality :double)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshSetVisibility" %model-mesh-set-visibility) :void
  (element-tags :pointer)
  (element-tags-n :unsigned-long)
  (value :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshGetVisibility" %model-mesh-get-visibility) :void
  (element-tags :pointer)
  (element-tags-n :unsigned-long)
  (values (:pointer :pointer))
  (values-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshClassifySurfaces" %model-mesh-classify-surfaces) :void
  (angle :double)
  (boundary :int)
  (for-reparametrization :int)
  (curve-angle :double)
  (export-discrete :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshCreateGeometry" %model-mesh-create-geometry) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshCreateTopology" %model-mesh-create-topology) :void
  (make-simply-connected :int)
  (export-discrete :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshAddHomologyRequest" %model-mesh-add-homology-request) :void
  (type :string)
  (domain-tags :pointer)
  (domain-tags-n :unsigned-long)
  (subdomain-tags :pointer)
  (subdomain-tags-n :unsigned-long)
  (dims :pointer)
  (dims-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshClearHomologyRequests" %model-mesh-clear-homology-requests) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshComputeHomology" %model-mesh-compute-homology) :void
  (dim-tags (:pointer :pointer))
  (dim-tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshComputeCrossField" %model-mesh-compute-cross-field) :void
  (view-tags (:pointer :pointer))
  (view-tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshFieldAdd" %model-mesh-field-add) :int
  (field-type :string)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshFieldRemove" %model-mesh-field-remove) :void
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshFieldList" %model-mesh-field-list) :void
  (tags (:pointer :pointer))
  (tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshFieldGetType" %model-mesh-field-get-type) :void
  (tag :int)
  (file-type (:pointer :pointer))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshFieldSetNumber" %model-mesh-field-set-number) :void
  (tag :int)
  (option :string)
  (value :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshFieldGetNumber" %model-mesh-field-get-number) :void
  (tag :int)
  (option :string)
  (value (:pointer :double))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshFieldSetString" %model-mesh-field-set-string) :void
  (tag :int)
  (option :string)
  (value :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshFieldGetString" %model-mesh-field-get-string) :void
  (tag :int)
  (option :string)
  (value (:pointer :pointer))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshFieldSetNumbers" %model-mesh-field-set-numbers) :void
  (tag :int)
  (option :string)
  (values :pointer)
  (values-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshFieldGetNumbers" %model-mesh-field-get-numbers) :void
  (tag :int)
  (option :string)
  (values (:pointer :pointer))
  (values-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshFieldSetAsBackgroundMesh" %model-mesh-field-set-as-background-mesh) :void
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelMeshFieldSetAsBoundaryLayer" %model-mesh-field-set-as-boundary-layer) :void
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddPoint" %model-geo-add-point) :int
  (x :double)
  (y :double)
  (z :double)
  (mesh-size :double)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddLine" %model-geo-add-line) :int
  (start-tag :int)
  (end-tag :int)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddCircleArc" %model-geo-add-circle-arc) :int
  (start-tag :int)
  (center-tag :int)
  (end-tag :int)
  (tag :int)
  (nx :double)
  (ny :double)
  (nz :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddEllipseArc" %model-geo-add-ellipse-arc) :int
  (start-tag :int)
  (center-tag :int)
  (major-tag :int)
  (end-tag :int)
  (tag :int)
  (nx :double)
  (ny :double)
  (nz :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddSpline" %model-geo-add-spline) :int
  (point-tags :pointer)
  (point-tags-n :unsigned-long)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddBSpline" %model-geo-add-b-spline) :int
  (point-tags :pointer)
  (point-tags-n :unsigned-long)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddBezier" %model-geo-add-bezier) :int
  (point-tags :pointer)
  (point-tags-n :unsigned-long)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddPolyline" %model-geo-add-polyline) :int
  (point-tags :pointer)
  (point-tags-n :unsigned-long)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddCompoundSpline" %model-geo-add-compound-spline) :int
  (curve-tags :pointer)
  (curve-tags-n :unsigned-long)
  (num-intervals :int)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddCompoundBSpline" %model-geo-add-compound-b-spline) :int
  (curve-tags :pointer)
  (curve-tags-n :unsigned-long)
  (num-intervals :int)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddCurveLoop" %model-geo-add-curve-loop) :int
  (curve-tags :pointer)
  (curve-tags-n :unsigned-long)
  (tag :int)
  (reorient :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddCurveLoops" %model-geo-add-curve-loops) :void
  (curve-tags :pointer)
  (curve-tags-n :unsigned-long)
  (tags (:pointer :pointer))
  (tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddPlaneSurface" %model-geo-add-plane-surface) :int
  (wire-tags :pointer)
  (wire-tags-n :unsigned-long)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddSurfaceFilling" %model-geo-add-surface-filling) :int
  (wire-tags :pointer)
  (wire-tags-n :unsigned-long)
  (tag :int)
  (sphere-center-tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddSurfaceLoop" %model-geo-add-surface-loop) :int
  (surface-tags :pointer)
  (surface-tags-n :unsigned-long)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddVolume" %model-geo-add-volume) :int
  (shell-tags :pointer)
  (shell-tags-n :unsigned-long)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddGeometry" %model-geo-add-geometry) :int
  (geometry :string)
  (numbers :pointer)
  (numbers-n :unsigned-long)
  (strings :pointer)
  (strings-n :unsigned-long)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddPointOnGeometry" %model-geo-add-point-on-geometry) :int
  (geometry-tag :int)
  (x :double)
  (y :double)
  (z :double)
  (mesh-size :double)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoExtrude" %model-geo-extrude) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (dx :double)
  (dy :double)
  (dz :double)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (num-elements :pointer)
  (num-elements-n :unsigned-long)
  (heights :pointer)
  (heights-n :unsigned-long)
  (recombine :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoRevolve" %model-geo-revolve) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (x :double)
  (y :double)
  (z :double)
  (ax :double)
  (ay :double)
  (az :double)
  (angle :double)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (num-elements :pointer)
  (num-elements-n :unsigned-long)
  (heights :pointer)
  (heights-n :unsigned-long)
  (recombine :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoTwist" %model-geo-twist) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (x :double)
  (y :double)
  (z :double)
  (dx :double)
  (dy :double)
  (dz :double)
  (ax :double)
  (ay :double)
  (az :double)
  (angle :double)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (num-elements :pointer)
  (num-elements-n :unsigned-long)
  (heights :pointer)
  (heights-n :unsigned-long)
  (recombine :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoExtrudeBoundaryLayer" %model-geo-extrude-boundary-layer) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (num-elements :pointer)
  (num-elements-n :unsigned-long)
  (heights :pointer)
  (heights-n :unsigned-long)
  (recombine :int)
  (second :int)
  (view-index :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoTranslate" %model-geo-translate) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (dx :double)
  (dy :double)
  (dz :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoRotate" %model-geo-rotate) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (x :double)
  (y :double)
  (z :double)
  (ax :double)
  (ay :double)
  (az :double)
  (angle :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoDilate" %model-geo-dilate) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (x :double)
  (y :double)
  (z :double)
  (a :double)
  (b :double)
  (c :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoMirror" %model-geo-mirror) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (a :double)
  (b :double)
  (c :double)
  (d :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoSymmetrize" %model-geo-symmetrize) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (a :double)
  (b :double)
  (c :double)
  (d :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoCopy" %model-geo-copy) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoRemove" %model-geo-remove) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (recursive :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoRemoveAllDuplicates" %model-geo-remove-all-duplicates) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoSplitCurve" %model-geo-split-curve) :void
  (tag :int)
  (point-tags :pointer)
  (point-tags-n :unsigned-long)
  (curve-tags (:pointer :pointer))
  (curve-tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoGetMaxTag" %model-geo-get-max-tag) :int
  (dim :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoSetMaxTag" %model-geo-set-max-tag) :void
  (dim :int)
  (max-tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoAddPhysicalGroup" %model-geo-add-physical-group) :int
  (dim :int)
  (tags :pointer)
  (tags-n :unsigned-long)
  (tag :int)
  (name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoRemovePhysicalGroups" %model-geo-remove-physical-groups) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoSynchronize" %model-geo-synchronize) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoMeshSetSize" %model-geo-mesh-set-size) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (size :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoMeshSetTransfiniteCurve" %model-geo-mesh-set-transfinite-curve) :void
  (tag :int)
  (n-points :int)
  (mesh-type :string)
  (coef :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoMeshSetTransfiniteSurface" %model-geo-mesh-set-transfinite-surface) :void
  (tag :int)
  (arrangement :string)
  (corner-tags :pointer)
  (corner-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoMeshSetTransfiniteVolume" %model-geo-mesh-set-transfinite-volume) :void
  (tag :int)
  (corner-tags :pointer)
  (corner-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoMeshSetRecombine" %model-geo-mesh-set-recombine) :void
  (dim :int)
  (tag :int)
  (angle :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoMeshSetSmoothing" %model-geo-mesh-set-smoothing) :void
  (dim :int)
  (tag :int)
  (val :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoMeshSetReverse" %model-geo-mesh-set-reverse) :void
  (dim :int)
  (tag :int)
  (val :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoMeshSetAlgorithm" %model-geo-mesh-set-algorithm) :void
  (dim :int)
  (tag :int)
  (val :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelGeoMeshSetSizeFromBoundary" %model-geo-mesh-set-size-from-boundary) :void
  (dim :int)
  (tag :int)
  (val :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddPoint" %model-occ-add-point) :int
  (x :double)
  (y :double)
  (z :double)
  (mesh-size :double)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddLine" %model-occ-add-line) :int
  (start-tag :int)
  (end-tag :int)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddCircleArc" %model-occ-add-circle-arc) :int
  (start-tag :int)
  (middle-tag :int)
  (end-tag :int)
  (tag :int)
  (center :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddCircle" %model-occ-add-circle) :int
  (x :double)
  (y :double)
  (z :double)
  (r :double)
  (tag :int)
  (angle1 :double)
  (angle2 :double)
  (z-axis :pointer)
  (z-axis-n :unsigned-long)
  (x-axis :pointer)
  (x-axis-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddEllipseArc" %model-occ-add-ellipse-arc) :int
  (start-tag :int)
  (center-tag :int)
  (major-tag :int)
  (end-tag :int)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddEllipse" %model-occ-add-ellipse) :int
  (x :double)
  (y :double)
  (z :double)
  (r1 :double)
  (r2 :double)
  (tag :int)
  (angle1 :double)
  (angle2 :double)
  (z-axis :pointer)
  (z-axis-n :unsigned-long)
  (x-axis :pointer)
  (x-axis-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddSpline" %model-occ-add-spline) :int
  (point-tags :pointer)
  (point-tags-n :unsigned-long)
  (tag :int)
  (tangents :pointer)
  (tangents-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddBSpline" %model-occ-add-b-spline) :int
  (point-tags :pointer)
  (point-tags-n :unsigned-long)
  (tag :int)
  (degree :int)
  (weights :pointer)
  (weights-n :unsigned-long)
  (knots :pointer)
  (knots-n :unsigned-long)
  (multiplicities :pointer)
  (multiplicities-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddBezier" %model-occ-add-bezier) :int
  (point-tags :pointer)
  (point-tags-n :unsigned-long)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddWire" %model-occ-add-wire) :int
  (curve-tags :pointer)
  (curve-tags-n :unsigned-long)
  (tag :int)
  (check-closed :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddCurveLoop" %model-occ-add-curve-loop) :int
  (curve-tags :pointer)
  (curve-tags-n :unsigned-long)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddRectangle" %model-occ-add-rectangle) :int
  (x :double)
  (y :double)
  (z :double)
  (dx :double)
  (dy :double)
  (tag :int)
  (rounded-radius :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddDisk" %model-occ-add-disk) :int
  (xc :double)
  (yc :double)
  (zc :double)
  (rx :double)
  (ry :double)
  (tag :int)
  (z-axis :pointer)
  (z-axis-n :unsigned-long)
  (x-axis :pointer)
  (x-axis-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddPlaneSurface" %model-occ-add-plane-surface) :int
  (wire-tags :pointer)
  (wire-tags-n :unsigned-long)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddSurfaceFilling" %model-occ-add-surface-filling) :int
  (wire-tag :int)
  (tag :int)
  (point-tags :pointer)
  (point-tags-n :unsigned-long)
  (degree :int)
  (num-points-on-curves :int)
  (num-iter :int)
  (anisotropic :int)
  (tol2d :double)
  (tol3d :double)
  (tol-ang :double)
  (tol-curv :double)
  (max-degree :int)
  (max-segments :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddBSplineFilling" %model-occ-add-b-spline-filling) :int
  (wire-tag :int)
  (tag :int)
  (type :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddBezierFilling" %model-occ-add-bezier-filling) :int
  (wire-tag :int)
  (tag :int)
  (type :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddBSplineSurface" %model-occ-add-b-spline-surface) :int
  (point-tags :pointer)
  (point-tags-n :unsigned-long)
  (num-points-u :int)
  (tag :int)
  (degree-u :int)
  (degree-v :int)
  (weights :pointer)
  (weights-n :unsigned-long)
  (knots-u :pointer)
  (knots-u-n :unsigned-long)
  (knots-v :pointer)
  (knots-v-n :unsigned-long)
  (multiplicities-u :pointer)
  (multiplicities-u-n :unsigned-long)
  (multiplicities-v :pointer)
  (multiplicities-v-n :unsigned-long)
  (wire-tags :pointer)
  (wire-tags-n :unsigned-long)
  (wire3-d :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddBezierSurface" %model-occ-add-bezier-surface) :int
  (point-tags :pointer)
  (point-tags-n :unsigned-long)
  (num-points-u :int)
  (tag :int)
  (wire-tags :pointer)
  (wire-tags-n :unsigned-long)
  (wire3-d :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddTrimmedSurface" %model-occ-add-trimmed-surface) :int
  (surface-tag :int)
  (wire-tags :pointer)
  (wire-tags-n :unsigned-long)
  (wire3-d :int)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddSurfaceLoop" %model-occ-add-surface-loop) :int
  (surface-tags :pointer)
  (surface-tags-n :unsigned-long)
  (tag :int)
  (sewing :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddVolume" %model-occ-add-volume) :int
  (shell-tags :pointer)
  (shell-tags-n :unsigned-long)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddSphere" %model-occ-add-sphere) :int
  (xc :double)
  (yc :double)
  (zc :double)
  (radius :double)
  (tag :int)
  (angle1 :double)
  (angle2 :double)
  (angle3 :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddBox" %model-occ-add-box) :int
  (x :double)
  (y :double)
  (z :double)
  (dx :double)
  (dy :double)
  (dz :double)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddCylinder" %model-occ-add-cylinder) :int
  (x :double)
  (y :double)
  (z :double)
  (dx :double)
  (dy :double)
  (dz :double)
  (r :double)
  (tag :int)
  (angle :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddCone" %model-occ-add-cone) :int
  (x :double)
  (y :double)
  (z :double)
  (dx :double)
  (dy :double)
  (dz :double)
  (r1 :double)
  (r2 :double)
  (tag :int)
  (angle :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddWedge" %model-occ-add-wedge) :int
  (x :double)
  (y :double)
  (z :double)
  (dx :double)
  (dy :double)
  (dz :double)
  (tag :int)
  (ltx :double)
  (z-axis :pointer)
  (z-axis-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddTorus" %model-occ-add-torus) :int
  (x :double)
  (y :double)
  (z :double)
  (r1 :double)
  (r2 :double)
  (tag :int)
  (angle :double)
  (z-axis :pointer)
  (z-axis-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddThruSections" %model-occ-add-thru-sections) :void
  (wire-tags :pointer)
  (wire-tags-n :unsigned-long)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (tag :int)
  (make-solid :int)
  (make-ruled :int)
  (max-degree :int)
  (continuity :string)
  (parametrization :string)
  (smoothing :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddThickSolid" %model-occ-add-thick-solid) :void
  (volume-tag :int)
  (exclude-surface-tags :pointer)
  (exclude-surface-tags-n :unsigned-long)
  (offset :double)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccExtrude" %model-occ-extrude) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (dx :double)
  (dy :double)
  (dz :double)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (num-elements :pointer)
  (num-elements-n :unsigned-long)
  (heights :pointer)
  (heights-n :unsigned-long)
  (recombine :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccRevolve" %model-occ-revolve) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (x :double)
  (y :double)
  (z :double)
  (ax :double)
  (ay :double)
  (az :double)
  (angle :double)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (num-elements :pointer)
  (num-elements-n :unsigned-long)
  (heights :pointer)
  (heights-n :unsigned-long)
  (recombine :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAddPipe" %model-occ-add-pipe) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (wire-tag :int)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (trihedron :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccFillet" %model-occ-fillet) :void
  (volume-tags :pointer)
  (volume-tags-n :unsigned-long)
  (curve-tags :pointer)
  (curve-tags-n :unsigned-long)
  (radii :pointer)
  (radii-n :unsigned-long)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (remove-volume :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccChamfer" %model-occ-chamfer) :void
  (volume-tags :pointer)
  (volume-tags-n :unsigned-long)
  (curve-tags :pointer)
  (curve-tags-n :unsigned-long)
  (surface-tags :pointer)
  (surface-tags-n :unsigned-long)
  (distances :pointer)
  (distances-n :unsigned-long)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (remove-volume :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccDefeature" %model-occ-defeature) :void
  (volume-tags :pointer)
  (volume-tags-n :unsigned-long)
  (surface-tags :pointer)
  (surface-tags-n :unsigned-long)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (remove-volume :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccFillet2D" %model-occ-fillet2-d) :int
  (edge-tag1 :int)
  (edge-tag2 :int)
  (radius :double)
  (tag :int)
  (point-tag :int)
  (reverse :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccChamfer2D" %model-occ-chamfer2-d) :int
  (edge-tag1 :int)
  (edge-tag2 :int)
  (distance1 :double)
  (distance2 :double)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccOffsetCurve" %model-occ-offset-curve) :void
  (curve-loop-tag :int)
  (offset :double)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccGetDistance" %model-occ-get-distance) :void
  (dim1 :int)
  (tag1 :int)
  (dim2 :int)
  (tag2 :int)
  (distance (:pointer :double))
  (x1 (:pointer :double))
  (y1 (:pointer :double))
  (z1 (:pointer :double))
  (x2 (:pointer :double))
  (y2 (:pointer :double))
  (z2 (:pointer :double))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccGetClosestEntities" %model-occ-get-closest-entities) :void
  (x :double)
  (y :double)
  (z :double)
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (distances (:pointer :pointer))
  (distances-n (:pointer :unsigned-long))
  (coord (:pointer :pointer))
  (coord-n (:pointer :unsigned-long))
  (n :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccFuse" %model-occ-fuse) :void
  (object-dim-tags :pointer)
  (object-dim-tags-n :unsigned-long)
  (tool-dim-tags :pointer)
  (tool-dim-tags-n :unsigned-long)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (out-dim-tags-map (:pointer :pointer))
  (out-dim-tags-map-n (:pointer :pointer))
  (out-dim-tags-map-nn (:pointer :unsigned-long))
  (tag :int)
  (remove-object :int)
  (remove-tool :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccIntersect" %model-occ-intersect) :void
  (object-dim-tags :pointer)
  (object-dim-tags-n :unsigned-long)
  (tool-dim-tags :pointer)
  (tool-dim-tags-n :unsigned-long)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (out-dim-tags-map (:pointer :pointer))
  (out-dim-tags-map-n (:pointer :pointer))
  (out-dim-tags-map-nn (:pointer :unsigned-long))
  (tag :int)
  (remove-object :int)
  (remove-tool :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccCut" %model-occ-cut) :void
  (object-dim-tags :pointer)
  (object-dim-tags-n :unsigned-long)
  (tool-dim-tags :pointer)
  (tool-dim-tags-n :unsigned-long)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (out-dim-tags-map (:pointer :pointer))
  (out-dim-tags-map-n (:pointer :pointer))
  (out-dim-tags-map-nn (:pointer :unsigned-long))
  (tag :int)
  (remove-object :int)
  (remove-tool :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccFragment" %model-occ-fragment) :void
  (object-dim-tags :pointer)
  (object-dim-tags-n :unsigned-long)
  (tool-dim-tags :pointer)
  (tool-dim-tags-n :unsigned-long)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (out-dim-tags-map (:pointer :pointer))
  (out-dim-tags-map-n (:pointer :pointer))
  (out-dim-tags-map-nn (:pointer :unsigned-long))
  (tag :int)
  (remove-object :int)
  (remove-tool :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccTranslate" %model-occ-translate) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (dx :double)
  (dy :double)
  (dz :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccRotate" %model-occ-rotate) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (x :double)
  (y :double)
  (z :double)
  (ax :double)
  (ay :double)
  (az :double)
  (angle :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccDilate" %model-occ-dilate) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (x :double)
  (y :double)
  (z :double)
  (a :double)
  (b :double)
  (c :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccMirror" %model-occ-mirror) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (a :double)
  (b :double)
  (c :double)
  (d :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccSymmetrize" %model-occ-symmetrize) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (a :double)
  (b :double)
  (c :double)
  (d :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccAffineTransform" %model-occ-affine-transform) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (affine-transform :pointer)
  (affine-transform-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccCopy" %model-occ-copy) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccRemove" %model-occ-remove) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (recursive :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccRemoveAllDuplicates" %model-occ-remove-all-duplicates) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccHealShapes" %model-occ-heal-shapes) :void
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (tolerance :double)
  (fix-degenerated :int)
  (fix-small-edges :int)
  (fix-small-faces :int)
  (sew-faces :int)
  (make-solids :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccConvertToNURBS" %model-occ-convert-to-nurbs) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccImportShapes" %model-occ-import-shapes) :void
  (file-name :string)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (highest-dim-only :int)
  (format :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccImportShapesNativePointer" %model-occ-import-shapes-native-pointer) :void
  (shape :pointer)
  (out-dim-tags (:pointer :pointer))
  (out-dim-tags-n (:pointer :unsigned-long))
  (highest-dim-only :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccGetEntities" %model-occ-get-entities) :void
  (dim-tags (:pointer :pointer))
  (dim-tags-n (:pointer :unsigned-long))
  (dim :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccGetEntitiesInBoundingBox" %model-occ-get-entities-in-bounding-box) :void
  (xmin :double)
  (ymin :double)
  (zmin :double)
  (xmax :double)
  (ymax :double)
  (zmax :double)
  (dim-tags (:pointer :pointer))
  (dim-tags-n (:pointer :unsigned-long))
  (dim :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccGetBoundingBox" %model-occ-get-bounding-box) :void
  (dim :int)
  (tag :int)
  (xmin (:pointer :double))
  (ymin (:pointer :double))
  (zmin (:pointer :double))
  (xmax (:pointer :double))
  (ymax (:pointer :double))
  (zmax (:pointer :double))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccGetCurveLoops" %model-occ-get-curve-loops) :void
  (surface-tag :int)
  (curve-loop-tags (:pointer :pointer))
  (curve-loop-tags-n (:pointer :unsigned-long))
  (curve-tags (:pointer :pointer))
  (curve-tags-n (:pointer :pointer))
  (curve-tags-nn (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccGetSurfaceLoops" %model-occ-get-surface-loops) :void
  (volume-tag :int)
  (surface-loop-tags (:pointer :pointer))
  (surface-loop-tags-n (:pointer :unsigned-long))
  (surface-tags (:pointer :pointer))
  (surface-tags-n (:pointer :pointer))
  (surface-tags-nn (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccGetMass" %model-occ-get-mass) :void
  (dim :int)
  (tag :int)
  (mass (:pointer :double))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccGetCenterOfMass" %model-occ-get-center-of-mass) :void
  (dim :int)
  (tag :int)
  (x (:pointer :double))
  (y (:pointer :double))
  (z (:pointer :double))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccGetMatrixOfInertia" %model-occ-get-matrix-of-inertia) :void
  (dim :int)
  (tag :int)
  (mat (:pointer :pointer))
  (mat-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccGetMaxTag" %model-occ-get-max-tag) :int
  (dim :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccSetMaxTag" %model-occ-set-max-tag) :void
  (dim :int)
  (max-tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccSynchronize" %model-occ-synchronize) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshModelOccMeshSetSize" %model-occ-mesh-set-size) :void
  (dim-tags :pointer)
  (dim-tags-n :unsigned-long)
  (size :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewAdd" %view-add) :int
  (name :string)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewRemove" %view-remove) :void
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewGetIndex" %view-get-index) :int
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewGetTags" %view-get-tags) :void
  (tags (:pointer :pointer))
  (tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewAddModelData" %view-add-model-data) :void
  (tag :int)
  (step :int)
  (model-name :string)
  (data-type :string)
  (tags :pointer)
  (tags-n :unsigned-long)
  (data :pointer)
  (data-n :pointer)
  (data-nn :unsigned-long)
  (time :double)
  (num-components :int)
  (partition :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewAddHomogeneousModelData" %view-add-homogeneous-model-data) :void
  (tag :int)
  (step :int)
  (model-name :string)
  (data-type :string)
  (tags :pointer)
  (tags-n :unsigned-long)
  (data :pointer)
  (data-n :unsigned-long)
  (time :double)
  (num-components :int)
  (partition :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewGetModelData" %view-get-model-data) :void
  (tag :int)
  (step :int)
  (data-type (:pointer :pointer))
  (tags (:pointer :pointer))
  (tags-n (:pointer :unsigned-long))
  (data (:pointer :pointer))
  (data-n (:pointer :pointer))
  (data-nn (:pointer :unsigned-long))
  (time (:pointer :double))
  (num-components (:pointer :int))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewGetHomogeneousModelData" %view-get-homogeneous-model-data) :void
  (tag :int)
  (step :int)
  (data-type (:pointer :pointer))
  (tags (:pointer :pointer))
  (tags-n (:pointer :unsigned-long))
  (data (:pointer :pointer))
  (data-n (:pointer :unsigned-long))
  (time (:pointer :double))
  (num-components (:pointer :int))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewAddListData" %view-add-list-data) :void
  (tag :int)
  (data-type :string)
  (num-ele :int)
  (data :pointer)
  (data-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewGetListData" %view-get-list-data) :void
  (tag :int)
  (data-type (:pointer :pointer))
  (data-type-n (:pointer :unsigned-long))
  (num-elements (:pointer :pointer))
  (num-elements-n (:pointer :unsigned-long))
  (data (:pointer :pointer))
  (data-n (:pointer :pointer))
  (data-nn (:pointer :unsigned-long))
  (return-adaptive :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewAddListDataString" %view-add-list-data-string) :void
  (tag :int)
  (coord :pointer)
  (coord-n :unsigned-long)
  (data :pointer)
  (data-n :unsigned-long)
  (style :pointer)
  (style-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewGetListDataStrings" %view-get-list-data-strings) :void
  (tag :int)
  (dim :int)
  (coord (:pointer :pointer))
  (coord-n (:pointer :unsigned-long))
  (data (:pointer :pointer))
  (data-n (:pointer :unsigned-long))
  (style (:pointer :pointer))
  (style-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewSetInterpolationMatrices" %view-set-interpolation-matrices) :void
  (tag :int)
  (type :string)
  (d :int)
  (coef :pointer)
  (coef-n :unsigned-long)
  (exp :pointer)
  (exp-n :unsigned-long)
  (d-geo :int)
  (coef-geo :pointer)
  (coef-geo-n :unsigned-long)
  (exp-geo :pointer)
  (exp-geo-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewAddAlias" %view-add-alias) :int
  (ref-tag :int)
  (copy-options :int)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewCombine" %view-combine) :void
  (what :string)
  (how :string)
  (remove :int)
  (copy-options :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewProbe" %view-probe) :void
  (tag :int)
  (x :double)
  (y :double)
  (z :double)
  (values (:pointer :pointer))
  (values-n (:pointer :unsigned-long))
  (distance (:pointer :double))
  (step :int)
  (num-comp :int)
  (gradient :int)
  (distance-max :double)
  (x-elem-coord :pointer)
  (x-elem-coord-n :unsigned-long)
  (y-elem-coord :pointer)
  (y-elem-coord-n :unsigned-long)
  (z-elem-coord :pointer)
  (z-elem-coord-n :unsigned-long)
  (dim :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewWrite" %view-write) :void
  (tag :int)
  (file-name :string)
  (append :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewSetVisibilityPerWindow" %view-set-visibility-per-window) :void
  (tag :int)
  (value :int)
  (window-index :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewOptionSetNumber" %view-option-set-number) :void
  (tag :int)
  (name :string)
  (value :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewOptionGetNumber" %view-option-get-number) :void
  (tag :int)
  (name :string)
  (value (:pointer :double))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewOptionSetString" %view-option-set-string) :void
  (tag :int)
  (name :string)
  (value :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewOptionGetString" %view-option-get-string) :void
  (tag :int)
  (name :string)
  (value (:pointer :pointer))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewOptionSetColor" %view-option-set-color) :void
  (tag :int)
  (name :string)
  (r :int)
  (g :int)
  (b :int)
  (a :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewOptionGetColor" %view-option-get-color) :void
  (tag :int)
  (name :string)
  (r (:pointer :int))
  (g (:pointer :int))
  (b (:pointer :int))
  (a (:pointer :int))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshViewOptionCopy" %view-option-copy) :void
  (ref-tag :int)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshAlgorithmTriangulate" %algorithm-triangulate) :void
  (coordinates :pointer)
  (coordinates-n :unsigned-long)
  (triangles (:pointer :pointer))
  (triangles-n (:pointer :unsigned-long))
  (edges :pointer)
  (edges-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshAlgorithmTetrahedralize" %algorithm-tetrahedralize) :void
  (coordinates :pointer)
  (coordinates-n :unsigned-long)
  (tetrahedra (:pointer :pointer))
  (tetrahedra-n (:pointer :unsigned-long))
  (steiner (:pointer :pointer))
  (steiner-n (:pointer :unsigned-long))
  (triangles :pointer)
  (triangles-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshPluginSetNumber" %plugin-set-number) :void
  (name :string)
  (option :string)
  (value :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshPluginSetString" %plugin-set-string) :void
  (name :string)
  (option :string)
  (value :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshPluginRun" %plugin-run) :int
  (name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshGraphicsDraw" %graphics-draw) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkInitialize" %fltk-initialize) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkFinalize" %fltk-finalize) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkWait" %fltk-wait) :void
  (time :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkUpdate" %fltk-update) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkAwake" %fltk-awake) :void
  (action :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkLock" %fltk-lock) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkUnlock" %fltk-unlock) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkRun" %fltk-run) :void
  (option-file-name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkIsAvailable" %fltk-is-available) :int
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkSelectEntities" %fltk-select-entities) :int
  (dim-tags (:pointer :pointer))
  (dim-tags-n (:pointer :unsigned-long))
  (dim :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkSelectElements" %fltk-select-elements) :int
  (element-tags (:pointer :pointer))
  (element-tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkSelectViews" %fltk-select-views) :int
  (view-tags (:pointer :pointer))
  (view-tags-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkSplitCurrentWindow" %fltk-split-current-window) :void
  (how :string)
  (ratio :double)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkSetCurrentWindow" %fltk-set-current-window) :void
  (window-index :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkSetStatusMessage" %fltk-set-status-message) :void
  (message :string)
  (graphics :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkShowContextWindow" %fltk-show-context-window) :void
  (dim :int)
  (tag :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkOpenTreeItem" %fltk-open-tree-item) :void
  (name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshFltkCloseTreeItem" %fltk-close-tree-item) :void
  (name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshParserGetNames" %parser-get-names) :void
  (names (:pointer :pointer))
  (names-n (:pointer :unsigned-long))
  (search :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshParserSetNumber" %parser-set-number) :void
  (name :string)
  (value :pointer)
  (value-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshParserSetString" %parser-set-string) :void
  (name :string)
  (value :pointer)
  (value-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshParserGetNumber" %parser-get-number) :void
  (name :string)
  (value (:pointer :pointer))
  (value-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshParserGetString" %parser-get-string) :void
  (name :string)
  (value (:pointer :pointer))
  (value-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshParserClear" %parser-clear) :void
  (name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshParserParse" %parser-parse) :void
  (file-name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOnelabSet" %onelab-set) :void
  (data :string)
  (format :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOnelabGet" %onelab-get) :void
  (data (:pointer :pointer))
  (name :string)
  (format :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOnelabGetNames" %onelab-get-names) :void
  (names (:pointer :pointer))
  (names-n (:pointer :unsigned-long))
  (search :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOnelabSetNumber" %onelab-set-number) :void
  (name :string)
  (value :pointer)
  (value-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOnelabSetString" %onelab-set-string) :void
  (name :string)
  (value :pointer)
  (value-n :unsigned-long)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOnelabGetNumber" %onelab-get-number) :void
  (name :string)
  (value (:pointer :pointer))
  (value-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOnelabGetString" %onelab-get-string) :void
  (name :string)
  (value (:pointer :pointer))
  (value-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOnelabGetChanged" %onelab-get-changed) :int
  (name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOnelabSetChanged" %onelab-set-changed) :void
  (name :string)
  (value :int)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOnelabClear" %onelab-clear) :void
  (name :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshOnelabRun" %onelab-run) :void
  (name :string)
  (command :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshLoggerWrite" %logger-write) :void
  (message :string)
  (level :string)
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshLoggerStart" %logger-start) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshLoggerGet" %logger-get) :void
  (log (:pointer :pointer))
  (log-n (:pointer :unsigned-long))
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshLoggerStop" %logger-stop) :void
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshLoggerGetWallTime" %logger-get-wall-time) :double
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshLoggerGetCpuTime" %logger-get-cpu-time) :double
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshLoggerGetMemory" %logger-get-memory) :double
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshLoggerGetTotalMemory" %logger-get-total-memory) :double
  (ierr (:pointer :int))
)

(cffi:defcfun ("gmshLoggerGetLastError" %logger-get-last-error) :void
  (error (:pointer :pointer))
  (ierr (:pointer :int))
)

