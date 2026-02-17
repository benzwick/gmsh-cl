#!/bin/bash
# Debug a tutorial - shows full error output
# Usage: ./debug-tutorial.sh t10.lisp
set -e
cd "$(dirname "$0")"
T="${1:?usage: $0 <tutorial-file>}"
LD_LIBRARY_PATH=_reference/gmsh/build:$LD_LIBRARY_PATH exec sbcl --non-interactive \
  --eval '(pushnew (truename ".") asdf:*central-registry*)' \
  --eval '(asdf:load-system :gmsh-cl)' \
  --eval "(handler-case (gmsh:with-gmsh () (load \"tutorials/$T\")) (error (e) (format t \"~%ERROR: ~A~%\" e)))"
