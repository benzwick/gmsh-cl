#!/bin/bash
# Run gmsh-cl tests
# Usage: ./run-tests.sh [suite-name]
# Examples:
#   ./run-tests.sh              # run all tests
#   ./run-tests.sh :gmsh-cl/core  # run core tests only

set -e
cd "$(dirname "$0")"

SUITE="${1:-:gmsh-cl}"

LD_LIBRARY_PATH=_reference/gmsh/build:$LD_LIBRARY_PATH exec sbcl --non-interactive \
  --eval '(pushnew (truename ".") asdf:*central-registry*)' \
  --eval '(asdf:load-system :gmsh-cl/tests)' \
  --eval "(let ((results (fiveam:run $SUITE)))
            (fiveam:explain! results)
            (unless (fiveam:results-status results)
              (uiop:quit 1)))"
