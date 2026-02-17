;;;; test-examples.lisp — Integration tests that load example files

(in-package :gmsh-cl/tests)
(in-suite :gmsh-cl/examples)

;;; Each test loads an example inside with-gmsh-test and verifies it
;;; completes without error. Examples that need external data files
;;; guard with probe-file and silently skip when files are absent.

;;; --- Geometry examples ---

(test example-simple
  (with-gmsh-test ()
    (finishes (load (example-path "simple.lisp")))))

(test example-boolean
  (with-gmsh-test ()
    (finishes (load (example-path "boolean.lisp")))))

(test example-circle-arc
  (with-gmsh-test ()
    (finishes (load (example-path "circle_arc.lisp")))))

(test example-spline
  (with-gmsh-test ()
    (finishes (load (example-path "spline.lisp")))))

(test example-pipe
  (with-gmsh-test ()
    (finishes (load (example-path "pipe.lisp")))))

(test example-prim-axis
  (with-gmsh-test ()
    (finishes (load (example-path "prim_axis.lisp")))))

(test example-closest-point
  (with-gmsh-test ()
    (finishes (load (example-path "closest_point.lisp")))))

(test example-spherical-surf
  (with-gmsh-test ()
    (finishes (load (example-path "spherical_surf.lisp")))))

(test example-parametric-surface
  (with-gmsh-test ()
    (finishes (load (example-path "parametric_surface.lisp")))))

(test example-surface-filling
  (with-gmsh-test ()
    (finishes (load (example-path "surface_filling.lisp")))))

(test example-trimmed
  (with-gmsh-test ()
    (finishes (load (example-path "trimmed.lisp")))))

(test example-bspline-filling
  (with-gmsh-test ()
    (finishes (load (example-path "bspline_filling.lisp")))))

(test example-bspline-bezier-patches
  (with-gmsh-test ()
    (finishes (load (example-path "bspline_bezier_patches.lisp")))))

(test example-bspline-bezier-trimmed
  (with-gmsh-test ()
    (finishes (load (example-path "bspline_bezier_trimmed.lisp")))))

(test example-reparam-on-face
  (with-gmsh-test ()
    (finishes (load (example-path "reparam_on_face.lisp")))))

;;; --- Mesh examples ---

(test example-hex
  (with-gmsh-test ()
    (finishes (load (example-path "hex.lisp")))))

(test example-discrete
  (with-gmsh-test ()
    (finishes (load (example-path "discrete.lisp")))))

(test example-crack
  (with-gmsh-test ()
    (finishes (load (example-path "crack.lisp")))))

(test example-crack3d
  (with-gmsh-test ()
    (finishes (load (example-path "crack3d.lisp")))))

(test example-partition
  (with-gmsh-test ()
    (finishes (load (example-path "partition.lisp")))))

(test example-periodic
  (with-gmsh-test ()
    (finishes (load (example-path "periodic.lisp")))))

(test example-flatten
  (with-gmsh-test ()
    (finishes (load (example-path "flatten.lisp")))))

(test example-flatten2
  (with-gmsh-test ()
    (finishes (load (example-path "flatten2.lisp")))))

(test example-raw-triangulation
  (with-gmsh-test ()
    (finishes (load (example-path "raw_triangulation.lisp")))))

(test example-raw-tetrahedralization
  (with-gmsh-test ()
    (finishes (load (example-path "raw_tetrahedralization.lisp")))))

(test example-relocate-nodes
  (with-gmsh-test ()
    (finishes (load (example-path "relocate_nodes.lisp")))))

(test example-remove-elements
  (with-gmsh-test ()
    (finishes (load (example-path "remove_elements.lisp")))))

(test example-renumbering
  (with-gmsh-test ()
    (finishes (load (example-path "renumbering.lisp")))))

(test example-mesh-quality
  (with-gmsh-test ()
    (finishes (load (example-path "mesh_quality.lisp")))))

(test example-min-edge
  (with-gmsh-test ()
    (finishes (load (example-path "min_edge.lisp")))))

(test example-mirror-mesh
  (with-gmsh-test ()
    (finishes (load (example-path "mirror_mesh.lisp")))))

(test example-extend-field
  (with-gmsh-test ()
    (finishes (load (example-path "extend_field.lisp")))))

(test example-msh-attributes
  (with-gmsh-test ()
    (finishes (load (example-path "msh_attributes.lisp")))))

(test example-hybrid-order
  (with-gmsh-test ()
    (finishes (load (example-path "hybrid_order.lisp")))))

(test example-mesh-from-discrete-curve
  (with-gmsh-test ()
    (finishes (load (example-path "mesh_from_discrete_curve.lisp")))))

(test example-normals
  (with-gmsh-test ()
    (finishes (load (example-path "normals.lisp")))))

(test example-neighbors
  (with-gmsh-test ()
    (finishes (load (example-path "neighbors.lisp")))))

(test example-explore
  (with-gmsh-test ()
    (finishes (load (example-path "explore.lisp")))))

(test example-opt
  (with-gmsh-test ()
    (finishes (load (example-path "opt.lisp")))))

(test example-volume
  (with-gmsh-test ()
    (finishes (load (example-path "volume.lisp")))))

(test example-copy-mesh
  (with-gmsh-test ()
    (finishes (load (example-path "copy_mesh.lisp")))))

(test example-test
  (with-gmsh-test ()
    (finishes (load (example-path "test.lisp")))))

;;; --- View/post-processing examples ---

(test example-view
  (with-gmsh-test ()
    (finishes (load (example-path "view.lisp")))))

(test example-viewlist
  (with-gmsh-test ()
    (finishes (load (example-path "viewlist.lisp")))))

(test example-view-combine
  (with-gmsh-test ()
    (finishes (load (example-path "view_combine.lisp")))))

(test example-view-element-size
  (with-gmsh-test ()
    (finishes (load (example-path "view_element_size.lisp")))))

(test example-view-renumbering
  (with-gmsh-test ()
    (finishes (load (example-path "view_renumbering.lisp")))))

(test example-view-list-isoparametric
  (with-gmsh-test ()
    (finishes (load (example-path "view_list_isoparametric.lisp")))))

(test example-view-list-subparametric
  (with-gmsh-test ()
    (finishes (load (example-path "view_list_subparametric.lisp")))))

(test example-view-list-superparametric
  (with-gmsh-test ()
    (finishes (load (example-path "view_list_superparametric.lisp")))))

(test example-view-adaptive-to-mesh
  (with-gmsh-test ()
    (finishes (load (example-path "view_adaptive_to_mesh.lisp")))))

(test example-plugin
  (with-gmsh-test ()
    (finishes (load (example-path "plugin.lisp")))))

;;; --- Data-dependent examples ---
;;; These guard with probe-file; they silently skip when data files are absent.

(test example-heal
  (with-gmsh-test ()
    (finishes (load (example-path "heal.lisp")))))

(test example-step-assembly
  (with-gmsh-test ()
    (finishes (load (example-path "step_assembly.lisp")))))

(test example-step-boundary-colors
  (with-gmsh-test ()
    (finishes (load (example-path "step_boundary_colors.lisp")))))

(test example-step-header-data
  (with-gmsh-test ()
    (finishes (load (example-path "step_header_data.lisp")))))

(test example-x3d-export
  (with-gmsh-test ()
    (finishes (load (example-path "x3d_export.lisp")))))

(test example-terrain
  (with-gmsh-test ()
    (finishes (load (example-path "terrain.lisp")))))

(test example-terrain-bspline
  (with-gmsh-test ()
    (finishes (load (example-path "terrain_bspline.lisp")))))

(test example-terrain-stl
  (with-gmsh-test ()
    (finishes (load (example-path "terrain_stl.lisp")))))

(test example-remesh-stl
  (with-gmsh-test ()
    (finishes (load (example-path "remesh_stl.lisp")))))

(test example-stl-to-mesh
  (with-gmsh-test ()
    (finishes (load (example-path "stl_to_mesh.lisp")))))

(test example-fractures
  (with-gmsh-test ()
    (finishes (load (example-path "fractures.lisp")))))

(test example-aneurysm
  (with-gmsh-test ()
    (finishes (load (example-path "aneurysm.lisp")))))

;;; remesh_cavity.lisp takes too long (~5 min) for CI; run manually
;; (test example-remesh-cavity
;;   (with-gmsh-test ()
;;     (finishes (load (example-path "remesh_cavity.lisp")))))

(test example-glue-and-remesh-stl
  (with-gmsh-test ()
    (finishes (load (example-path "glue_and_remesh_stl.lisp")))))

(test example-tube-boundary-layer
  (with-gmsh-test ()
    (finishes (load (example-path "tube_boundary_layer.lisp")))))

(test example-naca-boundary-layer-2d
  (with-gmsh-test ()
    (finishes (load (example-path "naca_boundary_layer_2d.lisp")))))

;;; --- New examples: geometry ---

(test example-thrusections
  (with-gmsh-test ()
    (finishes (load (example-path "thrusections.lisp")))))

(test example-full-quad
  (with-gmsh-test ()
    (finishes (load (example-path "full_quad.lisp")))))

(test example-primitives
  (with-gmsh-test ()
    (finishes (load (example-path "primitives.lisp")))))

(test example-transfinite
  (with-gmsh-test ()
    (finishes (load (example-path "transfinite.lisp")))))

(test example-hyperboloid
  (with-gmsh-test ()
    (finishes (load (example-path "hyperboloid.lisp")))))

(test example-fleur
  (with-gmsh-test ()
    (finishes (load (example-path "fleur.lisp")))))

(test example-fragment-surfaces
  (with-gmsh-test ()
    (finishes (load (example-path "fragment_surfaces.lisp")))))

;;; --- New examples: mesh ---

(test example-edges
  (with-gmsh-test ()
    (finishes (load (example-path "edges.lisp")))))

(test example-faces
  (with-gmsh-test ()
    (finishes (load (example-path "faces.lisp")))))

(test example-adapt-mesh
  (with-gmsh-test ()
    (finishes (load (example-path "adapt_mesh.lisp")))))

(test example-stl-to-brep
  (with-gmsh-test ()
    (finishes (load (example-path "stl_to_brep.lisp")))))

(test example-parametric-curves
  (with-gmsh-test ()
    (finishes (load (example-path "parametric_curves.lisp")))))

(test example-compute-area-volume
  (with-gmsh-test ()
    (finishes (load (example-path "compute_area_volume.lisp")))))

(test example-split-window
  (with-gmsh-test ()
    (finishes (load (example-path "split_window.lisp")))))

(test example-select-elements
  (with-gmsh-test ()
    (finishes (load (example-path "select_elements.lisp")))))

;;; naca_boundary_layer_3d takes a long time; run manually
;; (test example-naca-boundary-layer-3d
;;   (with-gmsh-test ()
;;     (finishes (load (example-path "naca_boundary_layer_3d.lisp")))))
