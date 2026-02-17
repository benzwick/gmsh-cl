;;;; occ.lisp — Batch convenience helpers for the OpenCASCADE CAD kernel

(in-package :gmsh/occ)

(export '(points))

(defun points (coords &key (size 0))
  "Create multiple points. COORDS is a list of (x y z) triples.
   Returns list of tags."
  (mapcar (lambda (c)
            (point (first c) (second c) (third c) :mesh-size size))
          coords))
