# gmsh-cl

Common Lisp interface for [Gmsh](https://gmsh.info). Interactive REPL-driven geometry creation, meshing, and visualization.

## Prerequisites

- SBCL (or another Common Lisp with CFFI support)
- [Quicklisp](https://www.quicklisp.org/)
- CMake, make, g++ (C++17)
- Python 3 (only needed to regenerate bindings)

### System libraries

```bash
# Debian 13 (trixie)
sudo apt install libfltk1.3-dev \
  libocct-modeling-algorithms-dev libocct-modeling-data-dev \
  libocct-data-exchange-dev libocct-foundation-dev \
  libgl-dev libglu1-mesa-dev
```

## Building libgmsh

The gmsh source is included as a git submodule. Build it as a shared library:

```bash
git submodule update --init
cd _reference/gmsh
mkdir build && cd build
cmake .. -DENABLE_BUILD_SHARED=ON -DCMAKE_BUILD_TYPE=Release
make -j$(nproc)
```

This produces `_reference/gmsh/build/libgmsh.so`.
To make it findable after building,
from inside the `build` directory:

```bash
export LD_LIBRARY_PATH=$(pwd):$LD_LIBRARY_PATH
```

## Loading

From the project directory, in SBCL:

```lisp
(pushnew (truename ".") asdf:*central-registry*)
(ql:quickload :gmsh-cl)
```

## Quick start

```lisp
;; Simple rectangle mesh
(gmsh:with-gmsh ()
  (gmsh:with-model ("example")
    (occ:rectangle 0 0 0 1 1)
    (occ:synchronize)
    (mesh:generate :dim 2)
    (gmsh:write "example.msh")))

;; Using the built-in CAD kernel with batch helpers
(gmsh:with-gmsh ()
  (gmsh:with-model ("t1")
    (let* ((pts (geo:points '((0 0 0) (1 0 0) (1 1 0) (0 1 0)) :size 0.1))
           (cl  (geo:line-loop pts))
           (s   (geo:plane-surface (list cl))))
      (geo:synchronize))
    (mesh:generate :dim 2)
    (gmsh:write "t1.msh")))
```

## Interactive REPL with GUI

```lisp
(gmsh:with-gmsh ()
  (gmsh:start-gui)     ; opens the gmsh window in a background thread
  ;; Now at the REPL:
  (geo:point 0 0 0 :mesh-size 0.1)
  (geo:point 1 0 0 :mesh-size 0.1)
  (geo:line 1 2)
  (geo:synchronize)
  (fltk:update)         ; GUI shows the geometry
  (mesh:generate :dim 1)
  (fltk:update))        ; GUI shows the mesh
```

## Package structure

| Package | Nickname | Contents |
|---|---|---|
| `gmsh` | — | Top-level: initialize, finalize, write, model ops |
| `gmsh/geo` | `geo` | Built-in CAD: point, line, curve-loop, plane-surface, extrude |
| `gmsh/occ` | `occ` | OpenCASCADE: box, sphere, rectangle, cut, fuse, fillet |
| `gmsh/mesh` | `mesh` | Meshing: generate, refine, get-nodes, get-elements |
| `gmsh/view` | `view` | Post-processing views |
| `gmsh/option` | `opt` | Option get/set |
| `gmsh/fltk` | `fltk` | GUI: initialize, wait, update, run |
| `gmsh/onelab` | `onelab` | Solver coupling |
| `gmsh/logger` | `logger` | Logging |

## Conventions

- **Scalars** are positional args: `(geo:point 0 0 0)`
- **Collections** are lists: `(geo:curve-loop '(1 2 3 4))`
- **Optional params** are keywords with defaults: `(geo:point 0 0 0 :mesh-size 0.1 :tag 5)`
- **Dim-tag pairs** are cons cells: `'((2 . 1) (2 . 2))` for two surfaces
- **Constructors** drop the "add" prefix: `addPoint` → `point`, `addBox` → `box`

## Regenerating bindings

Only needed if updating the gmsh submodule to a newer version:

```bash
python3 generate.py
```

This reads `_reference/gmsh/api/gen.py` and overwrites everything in `src/generated/`.
