# Known Issues

## Missing API: `extrude-boundary-layer` `:by-normals`

The `geo:extrude-boundary-layer` function is missing the `:by-normals` keyword
parameter. This parameter (`byNormals` in the C API) was added in a newer Gmsh
version than we built against.

**Affected examples:** `tube_boundary_layer.lisp`, `aneurysm.lisp`

**Fix:** Rebuild Gmsh from a newer version and regenerate bindings.

## Slow example: `remesh_cavity.lisp`

This example meshes a 10x10x10 cube at MeshSizeMax=0.3 (~174k tets), removes
elements to create a cavity, then remeshes. It takes several minutes to run.
Skipped in CI tests.
