#!/bin/bash
# Test all tutorial files that exist
# Usage: ./test-all-tutorials.sh [pattern]
# Examples:
#   ./test-all-tutorials.sh        # test all
#   ./test-all-tutorials.sh "t1"   # test only t1

set -e
cd "$(dirname "$0")"

PATTERN="${1:-*}"
PASS=0
FAIL=0
FAILED_LIST=""

for f in tutorials/${PATTERN}.lisp; do
  name=$(basename "$f" .lisp)
  result=$(LD_LIBRARY_PATH=_reference/gmsh/build:$LD_LIBRARY_PATH sbcl --non-interactive \
    --eval '(pushnew (truename ".") asdf:*central-registry*)' \
    --eval '(asdf:load-system :gmsh-cl)' \
    --eval "(handler-case (progn (gmsh:with-gmsh () (load \"$f\")) (format t \"OK\")) (error (e) (format t \"FAIL: ~A\" e)))" 2>&1 | grep -oE '^(OK|FAIL:.*)' | tail -1)

  if [ "$result" = "OK" ]; then
    echo "  PASS  $name"
    PASS=$((PASS + 1))
  else
    echo "  FAIL  $name  $result"
    FAIL=$((FAIL + 1))
    FAILED_LIST="$FAILED_LIST $name"
  fi
done

echo ""
echo "Results: $PASS passed, $FAIL failed"
if [ $FAIL -gt 0 ]; then
  echo "Failed:$FAILED_LIST"
  exit 1
fi
