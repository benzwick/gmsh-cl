# gmsh-cl

Common Lisp interface for [Gmsh](https://gmsh.info). Interactive REPL-driven geometry creation, meshing, and visualization.

## Prerequisites

- SBCL (or another Common Lisp with CFFI support)
- [ocicl](https://github.com/ocicl/ocicl) (CL dependency manager)
- CMake, make, g++ (C++17)
- Python 3 (only needed to regenerate bindings)

### System libraries

```bash
# Debian 13 (trixie)
sudo apt install sbcl ocicl \
  libfltk1.3-dev \
  libocct-modeling-algorithms-dev libocct-modeling-data-dev \
  libocct-data-exchange-dev libocct-foundation-dev \
  libgl-dev libglu1-mesa-dev
```

## Setup

```bash
git clone --recurse-submodules https://github.com/benzwick/gmsh-cl.git
cd gmsh-cl

# Install CL dependencies
ocicl install

# Build libgmsh
cd _reference/gmsh
mkdir build && cd build
cmake .. -DENABLE_BUILD_SHARED=ON -DCMAKE_BUILD_TYPE=Release
make -j$(nproc)
```

## Loading

From the project directory:

```bash
export LD_LIBRARY_PATH=_reference/gmsh/build:$LD_LIBRARY_PATH
sbcl
```

```lisp
(pushnew (truename ".") asdf:*central-registry*)
(asdf:load-system :gmsh-cl)
```

## Quick start

```lisp
;; Simple rectangle mesh
(gmsh:with-gmsh ()
  (gmsh:with-model ("example")
    (occ:rectangle 0 0 0 1 1)
    (occ:synchronize)
    (mesh:generate :dim 2)
    (gmsh:write "/tmp/example.msh")))

;; Polygon helper — one line for points + lines + curve-loop + surface
(gmsh:with-gmsh ()
  (gmsh:with-model ("polygon")
    (geo:polygon '((0 0 0) (1 0 0) (1 1 0) (0 1 0)) :size 0.1)
    (geo:synchronize)
    (mesh:generate :dim 2)
    (gmsh:write "/tmp/polygon.msh")))

;; OpenCASCADE boolean operations
(gmsh:with-gmsh ()
  (gmsh:with-model ("boolean")
    (occ:box 0 0 0 1 1 1 :tag 1)
    (occ:sphere 0.5 0.5 0.5 0.5 :tag 2)
    (occ:cut (gmsh:volume-tags '(1)) (gmsh:volume-tags '(2)))
    (occ:synchronize)
    (mesh:generate :dim 3)
    (gmsh:write "/tmp/boolean.msh")))
```

## GUI

### Using the `gmsh-gui` script

The easiest way to start the GUI with CL scripting enabled:

```bash
./gmsh-gui                    # empty GUI
./gmsh-gui untitled.lisp      # load a .lisp file, then open GUI
./gmsh-gui tutorials/t1.lisp  # view a tutorial in the GUI
./gmsh-gui --help             # show usage
```

GUI actions generate CL code to stdout and a companion `.lisp` file
next to the open `.geo`/`.msh` file.

### From the REPL

FLTK requires the event loop on the main thread, so use `gmsh:initialize`
directly (not `with-gmsh`, which finalizes on return).

**Blocking** (default) — opens the GUI, blocks until the window is closed:

```lisp
(pushnew (truename ".") asdf:*central-registry*)
(asdf:load-system :gmsh-cl)
(sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero)
  (gmsh:initialize)
  (opt:set-string "General.ScriptingLanguages" "geo,lisp")
  (gmsh:start-gui))
```

**Non-blocking** — GUI on main thread, REPL in a background thread:

```lisp
(pushnew (truename ".") asdf:*central-registry*)
(asdf:load-system :gmsh-cl)
(sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero)
  (gmsh:initialize)
  (opt:set-string "General.ScriptingLanguages" "geo,lisp")
  (gmsh:start-gui :block nil))
;; Gives you a gmsh> prompt to type commands while GUI is open:
;;   gmsh> (occ:box 0 0 0 1 1 1)
;;   gmsh> (occ:synchronize)
;;   gmsh> (fltk:update)
;;   gmsh> (mesh:generate :dim 3)
;;   gmsh> (gmsh:stop-gui)
```

### GUI scripting

The bundled Gmsh fork adds Common Lisp as a native scripting language.
Setting `"geo,lisp"` means "geo" executes the geometry and "lisp" records
the corresponding CL code. GUI actions will:

- Print CL code to stdout (e.g. `lisp: (geo:point 0.5 0.3 0 :mesh-size 1.0)`)
- Append CL code to a `.lisp` companion file next to the open `.geo`/`.msh` file

The generated `.lisp` files can be loaded directly:

```lisp
(gmsh:with-gmsh ()
  (load "recorded-session.lisp")
  (geo:synchronize))
```

Or viewed in the GUI:

```bash
./gmsh-gui recorded-session.lisp
```

## Convenience API

### Dim-tag helpers

Gmsh identifies entities as dimension-tag pairs. gmsh-cl provides helpers
to construct and destructure them:

```lisp
(gmsh:volume-tag 1)         ; => (3 . 1)
(gmsh:surface-tags '(1 2))  ; => ((2 . 1) (2 . 2))
(gmsh:tag (first result))   ; => extract tag from a dim-tag
(gmsh:tags-of result)       ; => list of tags from dim-tag list
(gmsh:volumes-of entities)  ; => filter to 3D entities only
```

### Mesh field helper

Set up mesh size fields with a single call instead of repeated `field-set-number`:

```lisp
(mesh:field "Distance" :tag 1
  :number-lists '(("PointsList" (5)) ("CurvesList" (2)))
  :numbers '(("Sampling" 100)))

(mesh:field "Threshold" :tag 2
  :numbers `(("InField" 1) ("SizeMin" 0.01) ("SizeMax" 0.1)
             ("DistMin" 0.15) ("DistMax" 0.5))
  :as-background t)
```

### Mesh size callback

```lisp
(mesh:define-size-callback my-size (dim tag x y z lc)
  (declare (ignore dim tag y z))
  (min lc (+ (* 0.02d0 x) 0.01d0)))
```

### Batch geometry helpers

```lisp
;; Create points from coordinate list
(geo:points '((0 0 0) (1 0 0) (1 1 0)) :size 0.1)

;; Create a closed polygon (points + lines + curve-loop + plane-surface)
(geo:polygon '((0 0 0) (1 0 0) (1 1 0) (0 1 0)) :size 0.1)

;; Create lines connecting a list of points
(geo:lines point-tags)
(geo:line-loop point-tags)
```

## Package structure

| Package | Nickname | Contents |
|---|---|---|
| `gmsh` | — | Top-level: initialize, finalize, write, model ops, dim-tag helpers |
| `gmsh/geo` | `geo` | Built-in CAD: point, line, curve-loop, plane-surface, extrude, polygon |
| `gmsh/occ` | `occ` | OpenCASCADE: box, sphere, rectangle, cut, fuse, fillet |
| `gmsh/mesh` | `mesh` | Meshing: generate, refine, get-nodes, get-elements, field helper |
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

## Tests

```bash
# Run all tests (~194 tests)
./run-tests.sh

# Run a specific test suite
./run-tests.sh :gmsh-cl/core
./run-tests.sh :gmsh-cl/tutorials
./run-tests.sh :gmsh-cl/examples

# Test tutorials standalone
./test-all-tutorials.sh
./test-all-tutorials.sh "t1"
```

## Tutorials and examples

The `tutorials/` directory contains CL ports of the
[Gmsh tutorials](https://gmsh.info/doc/texinfo/gmsh.html#Tutorial) (t1–t21, x1–x7).
The `examples/` directory contains ~85 examples covering geometry, meshing,
post-processing, and more.

```lisp
;; Run a tutorial
(gmsh:with-gmsh () (load "tutorials/t1.lisp"))

;; Run an example
(gmsh:with-gmsh () (load "examples/boolean.lisp"))
```

## Regenerating bindings

Only needed if updating the Gmsh submodule to a newer version:

```bash
python3 generate.py
```

This reads `_reference/gmsh/api/gen.py` and overwrites everything in `src/generated/`.

## License

GPL-2.0+
