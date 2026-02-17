# Known Issues

## Slow example: `remesh_cavity.lisp`

This example meshes a 10x10x10 cube at MeshSizeMax=0.3 (~174k tets), removes
elements to create a cavity, then remeshes. It takes several minutes to run.
Skipped in CI tests.
