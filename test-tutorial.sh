#!/bin/bash
# Test a single tutorial
# Usage: ./test-tutorial.sh t1.lisp
set -e
cd "$(dirname "$0")"
T="${1:?usage: $0 <tutorial-file>}"
LD_LIBRARY_PATH=_reference/gmsh/build:$LD_LIBRARY_PATH exec sbcl --non-interactive \
  --eval '(pushnew (truename ".") asdf:*central-registry*)' \
  --eval '(asdf:load-system :gmsh-cl)' \
  --eval "(gmsh:with-gmsh () (load \"tutorials/$T\"))" \
  --eval "(format t \"~%~A OK~%\" \"$T\")"
