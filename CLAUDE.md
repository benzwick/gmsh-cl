# gmsh-cl Development Notes

## CL Bindings: Positional vs Keyword Arguments

When translating Python gmsh API calls to CL, carefully check the generated
function signatures in `src/generated/`. Python uses positional args extensively
while the CL bindings convert many of them to keyword args.

Common mistakes:
- Python `mesh.optimize("", force=True)` → CL `(mesh:optimize :force t)`, NOT `(mesh:optimize "" :force t)` — `method` is keyword, not positional
- Python `mesh.renumberNodes(old, new)` → CL `(mesh:renumber-nodes :old-tags old :new-tags new)` — both are keyword
- Python `extrudeBoundaryLayer(dimTags, n, h, True)` → CL `(geo:extrude-boundary-layer dimTags :num-elements n :heights h :recombine t)` — only `dimTags` is positional
- Python `mesh.reverse(dimTags)` → CL `(mesh:reverse :dim-tags dimTags)` — `dim-tags` is keyword
- Python `addPointOnGeometry(g, x, y, z, tag=1)` → CL `(geo:point-on-geometry g x y :z z :tag 1)` — `z` is keyword

Always verify against the `defun` in `src/generated/*-functions.lisp` before
writing API calls. The `&key` boundary is the source of truth.

## dim-tags Format

Always cons pairs: `'((3 . 1) (2 . 5))`, NOT lists. The `with-pairs-array`
macro uses `(car pair)` and `(cdr pair)`.

## SBCL Float Traps

Gmsh C code triggers IEEE 754 exceptions. Wrap with:
```lisp
(sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero) ...)
```

## Tutorial/Example Style

Flat scripts, no `defun`/`in-package`/`initialize`/`finalize`. Package-qualified
calls (`geo:point`, `occ:box`, `mesh:generate`). Loaded via:
```lisp
(gmsh:with-gmsh () (load "examples/foo.lisp"))
```

## Test Infrastructure

- fiveam test framework, `(finishes ...)` checks forms complete without error
- `with-gmsh-test` macro handles init/finalize/float-traps
- `./run-tests.sh` runs all 179 tests; `./run-tests.sh :gmsh-cl/examples` for subset
