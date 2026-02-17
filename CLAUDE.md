# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
# Run all tests (~179 tests)
./run-tests.sh

# Run a specific test suite
./run-tests.sh :gmsh-cl/core
./run-tests.sh :gmsh-cl/geo
./run-tests.sh :gmsh-cl/occ
./run-tests.sh :gmsh-cl/mesh
./run-tests.sh :gmsh-cl/view
./run-tests.sh :gmsh-cl/options
./run-tests.sh :gmsh-cl/tutorials
./run-tests.sh :gmsh-cl/examples

# Test tutorial files standalone
./test-all-tutorials.sh         # all tutorials
./test-all-tutorials.sh "t1"    # single tutorial

# Regenerate bindings (only when updating gmsh submodule)
python3 generate.py
```

## Architecture

Three-layer design over Gmsh's C API via CFFI:

1. **`src/core/`** — Infrastructure: library loading (`library.lisp`), error condition (`conditions.lisp`), CFFI marshalling macros (`util.lisp`). The `gmsh/internal` package exports helpers used by generated code.

2. **`src/generated/`** — Auto-generated from `generate.py` reading `_reference/gmsh/api/gen.py`. Do NOT hand-edit these files.
   - `packages.lisp` — All package definitions with nicknames (geo, occ, mesh, view, opt, fltk, onelab, logger, parser, plugin, algorithm)
   - `bindings.lisp` — Raw `cffi:defcfun` declarations
   - `*-functions.lisp` — Wrapped CL functions with type coercion, error checking, and memory management

3. **`src/api/`** — Hand-written convenience layer:
   - `gmsh.lisp` — `with-gmsh`, `with-model`, `start-gui` macros
   - `geo.lisp` / `occ.lisp` — Batch helpers (`points`, `lines`, `line-loop`)
   - `recording.lisp` — API call recording and `.geo` file translation

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

Gmsh C code triggers IEEE 754 exceptions. `with-gmsh` and `with-gmsh-test`
handle this automatically. For manual use:
```lisp
(sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero) ...)
```

## Tutorial/Example Style

Flat scripts, no `defun`/`in-package`/`initialize`/`finalize`. Package-qualified
calls (`geo:point`, `occ:box`, `mesh:generate`). `tutorials/` has gmsh tutorial
ports (t1-t21, x1-x7); `examples/` has API examples (~68 files). Both loaded via:
```lisp
(gmsh:with-gmsh () (load "tutorials/t1.lisp"))
(gmsh:with-gmsh () (load "examples/boolean.lisp"))
```

## Test Infrastructure

- fiveam test framework; `(finishes ...)` checks forms complete without error
- `with-gmsh-test` macro handles init/finalize/float-traps per test
- Tests defined in `tests/test-*.lisp`, suites in `tests/suite.lisp`
- CFFI callbacks: `mesh:set-size-callback` needs `(cffi:callback name)` from `cffi:defcallback`, not a CL lambda
